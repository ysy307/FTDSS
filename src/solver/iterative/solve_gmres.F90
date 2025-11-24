submodule(solver_solve) solve_type_solver_gmres
    implicit none
contains

    !> GMRESソルバの初期化
    module subroutine initialize_type_solver_gmres(self, solver_settings, preconditioner_settings)
        implicit none
        class(type_solver_gmres), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        integer(int32) :: i, ierr

        self%id = solver_settings%id
        self%name = "GMRES"

        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%max_iterations = solver_settings%max_iterations
        self%m_restart = solver_settings%m_restart

        ! リスタート回数のデフォルト値チェック
        if (self%m_restart <= 0) self%m_restart = 30

        ! --- メモリ確保 ---

        ! 基底ベクトル V の確保 (配列の各要素を初期化)
        if (allocated(self%v)) deallocate (self%v)
        allocate (self%v(self%m_restart + 1))
        do i = 1, self%m_restart + 1
            call self%v(i)%initialize(self%num_nodes)
        end do

        ! 作業用ベクトルの初期化
        call self%r%initialize(self%num_nodes)
        call self%z%initialize(self%num_nodes)
        call self%x_update%initialize(self%num_nodes)
        call self%r%zero()
        call self%z%zero()
        call self%x_update%zero()

        ! ヘッセンベルグ行列等の確保 (m に依存する小さな配列)
        call allocate_array(self%h, self%m_restart + 1, self%m_restart)
        call allocate_array(self%g, self%m_restart + 1)
        call allocate_array(self%cs, self%m_restart)
        call allocate_array(self%sn, self%m_restart)
        call allocate_array(self%y, self%m_restart)

        ! 行列・配列のゼロクリア
        self%h = 0.0d0
        self%g = 0.0d0
        self%cs = 0.0d0
        self%sn = 0.0d0
        self%y = 0.0d0

        ! 履歴の初期化
        call self%residual_history%initialize(self%max_iterations)
        self%current_iteration = 0

        ! 前処理の作成
        call create_preconditioner(self%pc, preconditioner_settings, ierr)
        self%status = ierr

    end subroutine initialize_type_solver_gmres

!> GMRES(m) ソルバ本体 (lis実装ベースの右前処理)
    module subroutine solve_type_solver_gmres(self, A, b, x)
        implicit none
        class(type_solver_gmres), intent(inout) :: self
        class(abst_matrix), intent(in) :: A
        type(type_vector_dp), intent(in) :: b
        type(type_vector_dp), intent(inout) :: x

        real(real64) :: beta, w_norm, temp_val, resid
        integer(int32) :: i, j, k, ierr, iter_global
        logical :: converged

        call self%residual_history%zero()
        self%current_iteration = 0
        iter_global = 0
        converged = .false.

        ! 前処理セットアップ
        call self%pc%setup(A)

        ! ==========================================================
        ! Restart Loop
        ! ==========================================================
        restart_loop: do

            ! 1. 初期残差 r = b - Ax
            call self%r%zero()
            call matvec(A, x, self%r, ierr)
            call subtract(b, self%r, self%r)

            ! 2. beta = ||r||
            beta = vector_norm2(self%r)

            if (iter_global == 0) call self%residual_history%set(OP_INS, 1, beta)

            if (beta < self%tolerance) then
                self%status = SOLVER_STATUS_SUCCESS
                self%current_iteration = iter_global
                exit restart_loop
            end if

            ! 3. v1 = r / beta
            call self%v(1)%copy(self%r)
            call vector_scale(1.0d0 / beta, self%v(1))

            ! 4. g = [beta, 0, ...]^T
            self%g = 0.0d0
            self%g(1) = beta

            ! ==========================================================
            ! Arnoldi Loop (Inner Loop)
            ! ==========================================================
            arnoldi_loop: do j = 1, self%m_restart

                iter_global = iter_global + 1
                self%current_iteration = iter_global

                ! --- 右前処理 GMRES (Right Preconditioning) ---
                ! lis: z = M^-1 * v
                call self%pc%apply(self%v(j), self%z)

                ! lis: w = A * z
                call matvec(A, self%z, self%v(j + 1), ierr)

                ! --- 修正グラム・シュミット (MGS) ---
                do i = 1, j
                    self%h(i, j) = vector_dot(self%v(j + 1), self%v(i))
                    call vector_axpy(-self%h(i, j), self%v(i), self%v(j + 1))
                end do

                w_norm = vector_norm2(self%v(j + 1))
                self%h(j + 1, j) = w_norm

                ! Breakdown check
                if (w_norm < 1.0d-20) w_norm = 1.0d-20

                call vector_scale(1.0d0 / w_norm, self%v(j + 1))

                ! --- ギブンス回転 (Givens Rotation) ---
                ! 過去の回転を適用
                do i = 1, j - 1
                    temp_val = self%cs(i) * self%h(i, j) + self%sn(i) * self%h(i + 1, j)
                    self%h(i + 1, j) = -self%sn(i) * self%h(i, j) + self%cs(i) * self%h(i + 1, j)
                    self%h(i, j) = temp_val
                end do

                ! 新しい回転の生成と適用
                call generate_givens_rotation(self%h(j, j), self%h(j + 1, j), self%cs(j), self%sn(j))

                ! 対角成分の更新
                self%h(j, j) = self%cs(j) * self%h(j, j) + self%sn(j) * self%h(j + 1, j)
                self%h(j + 1, j) = 0.0d0

                ! 右辺ベクトル g の更新
                ! lis: s[i1] = -sn * s[ii]; s[ii] = cs * s[ii];
                self%g(j + 1) = -self%sn(j) * self%g(j)
                self%g(j) = self%cs(j) * self%g(j)

                ! --- 収束判定 ---
                resid = abs(self%g(j + 1))
                call self%residual_history%set(OP_INS, iter_global, resid)

                if (resid < self%tolerance) then
                    converged = .true.
                    exit arnoldi_loop
                end if

                if (iter_global >= self%max_iterations) then
                    self%status = SOLVER_STATUS_MAXITER
                    exit arnoldi_loop
                end if

                if (was_interrupted()) stop

            end do arnoldi_loop

            ! ==========================================================
            ! 解の更新 (Update Solution)
            ! ==========================================================
            k = j
            if (k > self%m_restart) k = self%m_restart

            ! 上三角行列の方程式 Hy = g を解く
            call backward_substitution(k, self%h, self%g, self%y)

            ! x = x + M^-1 * (V * y)
            ! 1. w = V * y (x_update に蓄積)
            call self%x_update%zero()
            do i = 1, k
                call vector_axpy(self%y(i), self%v(i), self%x_update)
            end do

            ! 2. z = M^-1 * w
            call self%pc%apply(self%x_update, self%z)

            ! 3. x = x + z
            call vector_axpy(1.0d0, self%z, x)

            if (converged .or. self%status == SOLVER_STATUS_MAXITER) exit restart_loop

        end do restart_loop

    end subroutine solve_type_solver_gmres
    !> メモリ解放
    module subroutine destroy_type_solver_gmres(self)
        implicit none
        class(type_solver_gmres), intent(inout) :: self
        integer(int32) :: i

        self%id = -1
        if (allocated(self%name)) deallocate (self%name)

        ! ベクトル配列の解放
        if (allocated(self%v)) then
            do i = 1, size(self%v)
                call self%v(i)%destroy()
            end do
            deallocate (self%v)
        end if

        call self%r%destroy()
        call self%z%destroy()
        call self%x_update%destroy()

        ! スカラー配列の解放
        call deallocate_array(self%h)
        call deallocate_array(self%g)
        call deallocate_array(self%cs)
        call deallocate_array(self%sn)
        call deallocate_array(self%y)
        if (allocated(self%pc)) then

            call self%pc%destroy()
            deallocate (self%pc)
        end if

        self%status = SOLVER_STATUS_SUCCESS
    end subroutine destroy_type_solver_gmres

    ! ------------------------------------------------------------------
    ! ヘルパーサブルーチン (内部利用)
    ! ------------------------------------------------------------------

!> ギブンス回転係数生成 (lis/BLAS drotg相当)
    pure subroutine generate_givens_rotation(dx, dy, c, s)
        implicit none
        real(real64), intent(in) :: dx, dy
        real(real64), intent(inout) :: c, s
        real(real64) :: temp

        if (dy == 0.0d0) then
            c = 1.0d0
            s = 0.0d0
        else if (abs(dy) > abs(dx)) then
            temp = dx / dy
            s = 1.0d0 / sqrt(1.0d0 + temp**2)
            c = temp * s
        else
            temp = dy / dx
            c = 1.0d0 / sqrt(1.0d0 + temp**2)
            s = temp * c
        end if
    end subroutine generate_givens_rotation

    !> 後退代入 (H y = g)
    pure subroutine backward_substitution(n, H, g, y)
        implicit none
        integer(int32), intent(in) :: n
        real(real64), intent(in) :: H(:, :)
        real(real64), intent(in) :: g(:)
        real(real64), intent(inout) :: y(:)

        integer(int32) :: i, j
        real(real64) :: sum_val

        y = 0.0d0
        do i = n, 1, -1
            sum_val = g(i)
            do j = i + 1, n
                sum_val = sum_val - H(i, j) * y(j)
            end do
            y(i) = sum_val / H(i, i)
        end do
    end subroutine backward_substitution

end submodule solve_type_solver_gmres

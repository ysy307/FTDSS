submodule(solver_preconditioner) solver_preconditioner_jacobi
    implicit none

contains

    !> Jacobi 前処理インスタンスを初期化する
    module subroutine initialize_preconditioner_jacobi(self, info)
        implicit none
        class(type_preconditioner_jacobi), intent(inout) :: self
        type(type_preconditioner_settings), intent(in) :: info

        self%name = "Jacobi"
        self%id = SOLVER_PRECONDITION_JACOBI

        self%num_nodes = info%num_nodes ! 初期値

        if (info%block_size > 1) then
            self%is_block = .true.
            self%block_size = info%block_size
        else
            self%is_block = .false.
            self%block_size = 1
        end if

        self%status = SOLVER_STATUS_SUCCESS
    end subroutine initialize_preconditioner_jacobi

    !> 行列 A に合わせて前処理をセットアップする
    module subroutine setup_preconditioner_jacobi(self, A)
        implicit none
        class(type_preconditioner_jacobi), intent(inout) :: self
        class(abst_matrix), intent(in) :: A

        type(type_matrix_info) :: info

        call A%get_info(info)

        ! 行列の実際のサイズ情報で更新
        if (self%num_nodes /= info%num_rows) then
            self%num_nodes = info%num_rows
        end if

        self%status = SOLVER_STATUS_NOT_IMPLEMENTED

        select type (A)
        type is (type_matrix_dense)
            self%is_block = .false.
            call self%setup_point(A)

        type is (type_matrix_csr)
            self%is_block = .false.
            call self%setup_point(A)

        type is (type_matrix_coo)
            self%is_block = .false.
            call self%setup_point(A)

        type is (type_matrix_bsr)
            if (self%is_block .and. self%block_size > 1) then
                call self%setup_block(A)
            else
                self%is_block = .false.
                call self%setup_point(A)
            end if

        class default
            write (*, *) "Error: Jacobi preconditioner does not support this matrix type."
            self%status = SOLVER_STATUS_NOT_IMPLEMENTED
        end select

    end subroutine setup_preconditioner_jacobi

    !> Point Jacobi のセットアップ
    !> 引数は abst_matrix のまま、対角成分の逆数を計算する
    module subroutine setup_preconditioner_jacobi_point(self, A)
        implicit none
        class(type_preconditioner_jacobi), intent(inout) :: self
        class(abst_matrix), intent(in) :: A

        ! メモリ確保：サイズは num_rows (総自由度)
        if (self%M_inv%get_size() /= self%num_nodes) then
            call self%M_inv%initialize(self%num_nodes)
        end if

        call self%M_inv%zero()

        ! 対角成分を取得 (abst_matrix のインターフェース経由)
        call A%get_diagonal(self%M_inv)

        ! 逆数を計算 ( 1 / A_ii )
        call vector_reciprocal(self%M_inv)

        self%status = SOLVER_STATUS_SUCCESS
    end subroutine setup_preconditioner_jacobi_point

    !> Block Jacobi のセットアップ
    !> 引数は abst_matrix のまま、内部で BSR にキャストして処理する
    module subroutine setup_preconditioner_jacobi_block(self, A)
        implicit none
        class(type_preconditioner_jacobi), intent(inout) :: self
        class(abst_matrix), intent(in) :: A

        integer(int32) :: i, ierr, bs, num_blocks

        bs = self%block_size
        num_blocks = self%num_nodes / bs

        select type (A)
        type is (type_matrix_bsr)
            ! メモリ再確保
            if (allocated(self%M_inv_blocks)) deallocate (self%M_inv_blocks)
            if (allocated(self%ipiv_blocks)) deallocate (self%ipiv_blocks)

            allocate (self%M_inv_blocks(bs, bs, num_blocks))
            allocate (self%ipiv_blocks(bs, num_blocks))

            !$omp parallel do private(i, ierr)
            do i = 1, num_blocks
                ! A は type_matrix_bsr なので get_diagonal_block を呼べる
                call A%get_diagonal_block(i, self%M_inv_blocks(:, :, i))

                ! LU分解 (dgetrf)
                call dgetrf(bs, bs, self%M_inv_blocks(:, :, i), bs, &
                            self%ipiv_blocks(:, i), ierr)

                if (ierr /= 0) then
                    ! エラー処理用のマーカー（必要に応じて実装）
                    self%M_inv_blocks(1, 1, i) = 0.0d0
                end if
            end do
            !$omp end parallel do

            self%status = SOLVER_STATUS_SUCCESS

        class default
            ! BSR以外がここに来ることは論理上ないが、安全のため
            self%status = -1
        end select

    end subroutine setup_preconditioner_jacobi_block

    !> 前処理の適用: z = M^-1 * r
    module subroutine apply_preconditioner_jacobi(self, r, z)
        implicit none
        class(type_preconditioner_jacobi), intent(inout) :: self
        type(type_vector_dp), intent(in) :: r
        type(type_vector_dp), intent(inout) :: z

        real(real64), dimension(:), pointer :: z_ptr
        integer(int32) :: i, idx_s, idx_e, ierr, bs, num_blocks

        if (self%status /= SOLVER_STATUS_SUCCESS) then
            call z%copy(r)
            return
        end if

        if (self%is_block) then
            ! ==========================================================
            ! Block Jacobi (LU solve)
            ! ==========================================================
            bs = self%block_size
            num_blocks = self%num_nodes / bs

            call z%copy(r)
            z_ptr => z%get_data()

            !$omp parallel do private(i, idx_s, idx_e, ierr)
            do i = 1, num_blocks
                idx_s = (i - 1) * bs + 1
                idx_e = i * bs

                call dgetrs('N', bs, 1, &
                            self%M_inv_blocks(:, :, i), bs, &
                            self%ipiv_blocks(:, i), &
                            z_ptr(idx_s:idx_e), bs, ierr)
            end do
            !$omp end parallel do

        else
            ! ==========================================================
            ! Point Jacobi (Scalar scaling)
            ! ==========================================================
            ! type_vector_dp 内の multiply 手続きを使用
            call multiply(self%M_inv, r, z)

        end if

    end subroutine apply_preconditioner_jacobi

    !> メモリの解放
    module subroutine destroy_preconditioner_jacobi(self)
        implicit none
        class(type_preconditioner_jacobi), intent(inout) :: self

        self%id = -1
        if (allocated(self%name)) deallocate (self%name)

        if (allocated(self%M_inv_blocks)) deallocate (self%M_inv_blocks)
        if (allocated(self%ipiv_blocks)) deallocate (self%ipiv_blocks)

        call self%M_inv%destroy()

        self%num_nodes = -1
        self%block_size = -1
        self%is_block = .false.
        self%status = SOLVER_STATUS_SUCCESS

    end subroutine destroy_preconditioner_jacobi

end submodule solver_preconditioner_jacobi

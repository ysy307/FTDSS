submodule(solver_preconditioner) solver_preconditioner_jacobi
    implicit none

contains

    !> Jacobi 前処理インスタンスを初期化する
    module subroutine initialize_preconditioner_jacobi(self, info)
        implicit none
        class(type_preconditioner_jacobi), intent(inout) :: self
        type(type_preconditioner_settings), intent(in) :: info

        self%name = "Jacobi"
        self%id = PRECONDITIONER_TYPES%JACOBI%id
        self%status = SOLVER_STATUS%SUCCESS%id

        ! 初期化時に設定されたノード数（自由度数）を正とする
        ! setupルーチンではこの値を変更しない
        self%num_nodes = info%num_nodes

        if (info%block_size > 1) then
            self%is_block = .true.
            self%block_size = info%block_size
        else
            self%is_block = .false.
            self%block_size = 1
        end if

    end subroutine initialize_preconditioner_jacobi

    !> 行列 A に合わせて前処理をセットアップする
    module subroutine setup_preconditioner_jacobi(self, A)
        implicit none
        class(type_preconditioner_jacobi), intent(inout) :: self
        class(abst_matrix), intent(in) :: A

        ! サイズ変更ロジックを削除
        ! Initializeで設定された self%num_nodes を信頼する

        self%status = SOLVER_STATUS%NOT_IMPLEMENTED%id

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
            self%status = SOLVER_STATUS%NOT_IMPLEMENTED%id
        end select

    end subroutine setup_preconditioner_jacobi

    !> Point Jacobi のセットアップ
    module subroutine setup_preconditioner_jacobi_point(self, A)
        implicit none
        class(type_preconditioner_jacobi), intent(inout) :: self
        class(abst_matrix), intent(in) :: A

        ! 初期化時のサイズでメモリを確保
        if (self%M_inv%get_size() /= self%num_nodes) then
            call self%M_inv%initialize(self%num_nodes)
        end if

        call self%M_inv%zero()

        ! 対角成分を取得
        call A%get_diagonal(self%M_inv)

        ! 逆数を計算 (linalg側でゼロ除算回避処理が行われる)
        call vector_reciprocal(self%M_inv)

        self%status = SOLVER_STATUS%SUCCESS%id
    end subroutine setup_preconditioner_jacobi_point

    !> Block Jacobi のセットアップ
    module subroutine setup_preconditioner_jacobi_block(self, A)
        implicit none
        class(type_preconditioner_jacobi), intent(inout) :: self
        class(abst_matrix), intent(in) :: A

        integer(int32) :: i, ierr, bs, num_blocks

        bs = self%block_size
        ! ブロック数は num_rows (全自由度) / block_size で計算
        num_blocks = self%num_nodes / bs

        select type (A)
        type is (type_matrix_bsr)
            if (allocated(self%M_inv_blocks)) deallocate (self%M_inv_blocks)
            if (allocated(self%ipiv_blocks)) deallocate (self%ipiv_blocks)

            allocate (self%M_inv_blocks(bs, bs, num_blocks))
            allocate (self%ipiv_blocks(bs, num_blocks))

            !$omp parallel do private(i, ierr)
            do i = 1, num_blocks
                call A%get_diagonal_block(i, self%M_inv_blocks(:, :, i))
                call dgetrf(bs, bs, self%M_inv_blocks(:, :, i), bs, &
                            self%ipiv_blocks(:, i), ierr)
            end do
            !$omp end parallel do

            self%status = SOLVER_STATUS%SUCCESS%id

        class default
            self%status = SOLVER_STATUS%NOT_IMPLEMENTED%id
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

        if (self%status /= SOLVER_STATUS%SUCCESS%id) then
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
        self%status = SOLVER_STATUS%SUCCESS%id

    end subroutine destroy_preconditioner_jacobi

end submodule solver_preconditioner_jacobi

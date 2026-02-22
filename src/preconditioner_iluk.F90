submodule(solver_preconditioner) solver_preconditioner_iluk
    use :: core_constants
    use :: core_types_matrix
    use :: module_type_vector, only:type_vector_dp
    implicit none

contains

    !> ILU(k) 前処理インスタンスを初期化する
    module subroutine initialize_preconditioner_iluk(self, info)
        implicit none
        class(type_preconditioner_iluk), intent(inout) :: self
        type(type_preconditioner_settings), intent(in) :: info

        self%id = SOLVER_PRECONDITION_ILU
        self%status = SOLVER_STATUS_SUCCESS

        if (allocated(self%name)) deallocate (self%name)
        self%name = "ILU(0)"

        ! 初期値としてノード数を設定するが、setup時に行列の実サイズで上書きされる
        self%num_rows = info%num_nodes

        if (info%block_size > 1) then
            self%is_block = .true.
            self%block_size = info%block_size
        else
            self%is_block = .false.
            self%block_size = 1
        end if

        self%fill_level = 0

    end subroutine initialize_preconditioner_iluk

    !> 行列 A に対する ILU(0) 分解のセットアップを行う
    module subroutine setup_preconditioner_iluk(self, A)
        implicit none
        class(type_preconditioner_iluk), intent(inout) :: self
        class(abst_matrix), intent(in) :: A

        type(type_matrix_info) :: info

        call A%get_info(info)

        ! 重要: 前処理のサイズを行列の実サイズに合わせる
        if (self%num_rows /= info%num_rows) then
            self%num_rows = info%num_rows
        end if

        self%status = SOLVER_STATUS_NOT_IMPLEMENTED

        select type (A)
        type is (type_matrix_csr)
            call self%setup_csr_ilu0(A)

        type is (type_matrix_bsr)
            if (self%is_block) then
                call self%setup_bsr_ilu0(A)
            else
                self%status = SOLVER_STATUS_NOT_IMPLEMENTED
            end if

        class default
            write (*, *) "Error: ILU(0) preconditioner does not support this matrix type."
            self%status = SOLVER_STATUS_NOT_IMPLEMENTED
        end select

    end subroutine setup_preconditioner_iluk

    !> CSR行列に対するスカラ ILU(0) 分解を実行する
    module subroutine setup_csr_ilu0(self, A)
        implicit none
        class(type_preconditioner_iluk), intent(inout) :: self
        class(type_matrix_csr), intent(in) :: A

        integer(int32), dimension(:), pointer :: mat_ptr, mat_ind
        real(real64), dimension(:), pointer :: mat_val
        integer(int32) :: i, k, j, jj, j_stop, nnz, col
        real(real64) :: multiplier
        real(real64), allocatable :: work_row(:)
        integer(int32), allocatable :: work_pos(:)

        mat_ptr => A%get_ptr()
        mat_ind => A%get_ind()
        mat_val => A%get_val()

        if (.not. associated(mat_ptr)) then
            self%status = -1
            return
        end if

        nnz = mat_ptr(self%num_rows + 1) - 1

        if (allocated(self%val)) deallocate (self%val)
        if (allocated(self%ptr)) deallocate (self%ptr)
        if (allocated(self%ind)) deallocate (self%ind)
        if (allocated(self%diag_ptr)) deallocate (self%diag_ptr)

        allocate (self%val(nnz))
        allocate (self%ptr(self%num_rows + 1))
        allocate (self%ind(nnz))
        allocate (self%diag_ptr(self%num_rows))

        self%ptr = mat_ptr
        self%ind = mat_ind
        self%val = mat_val

        allocate (work_row(self%num_rows))
        allocate (work_pos(self%num_rows))
        work_row = 0.0d0
        work_pos = 0

        do i = 1, self%num_rows
            self%diag_ptr(i) = -1
            do k = self%ptr(i), self%ptr(i + 1) - 1
                if (self%ind(k) == i) then
                    self%diag_ptr(i) = k
                    exit
                end if
            end do
            if (self%diag_ptr(i) == -1) then
                self%status = SOLVER_STATUS_DECOMPOSITION_FAILURE
                return
            end if
        end do

        do i = 1, self%num_rows
            ! 1. Scatter
            do k = self%ptr(i), self%ptr(i + 1) - 1
                col = self%ind(k)
                work_row(col) = self%val(k)
                work_pos(col) = k
            end do

            ! 2. Eliminate
            j_stop = self%diag_ptr(i) - 1
            do k = self%ptr(i), j_stop
                col = self%ind(k)
                multiplier = work_row(col) / self%val(self%diag_ptr(col))
                work_row(col) = multiplier
                self%val(k) = multiplier

                do jj = self%diag_ptr(col) + 1, self%ptr(col + 1) - 1
                    j = self%ind(jj)
                    if (work_pos(j) /= 0) then
                        work_row(j) = work_row(j) - multiplier * self%val(jj)
                    end if
                end do
            end do

            ! 3. Gather
            do k = self%ptr(i), self%ptr(i + 1) - 1
                col = self%ind(k)
                self%val(k) = work_row(col)
                work_row(col) = 0.0d0
                work_pos(col) = 0
            end do
        end do

        self%status = SOLVER_STATUS_SUCCESS
    end subroutine setup_csr_ilu0

    !> BSR行列に対するブロック ILU(0) 分解を実行する
    module subroutine setup_bsr_ilu0(self, A)
        implicit none
        class(type_preconditioner_iluk), intent(inout) :: self
        class(type_matrix_bsr), intent(in) :: A

        integer(int32), dimension(:), pointer :: mat_ptr, mat_ind
        real(real64), dimension(:, :, :), pointer :: mat_val
        integer(int32) :: i, k, jj, j, col, nnz, target_idx
        integer(int32) :: bs
        integer(int32) :: info_lapack
        integer(int32), allocatable :: work_pos(:)

        bs = self%block_size
        mat_ptr => A%get_ptr()
        mat_ind => A%get_ind()
        mat_val => A%get_val()

        if (.not. associated(mat_val)) then
            self%status = -1
            return
        end if
        nnz = size(mat_val, 3)

        if (allocated(self%val_blocks)) deallocate (self%val_blocks)
        if (allocated(self%ptr)) deallocate (self%ptr)
        if (allocated(self%ind)) deallocate (self%ind)
        if (allocated(self%diag_ptr)) deallocate (self%diag_ptr)
        if (allocated(self%diag_pivots)) deallocate (self%diag_pivots)

        allocate (self%val_blocks(bs, bs, nnz))
        allocate (self%ptr(self%num_rows + 1))
        allocate (self%ind(nnz))
        allocate (self%diag_ptr(self%num_rows))
        allocate (self%diag_pivots(bs, self%num_rows))

        allocate (work_pos(self%num_rows))
        work_pos = 0

        self%ptr = mat_ptr
        self%ind = mat_ind
        self%val_blocks = mat_val

        do i = 1, self%num_rows
            self%diag_ptr(i) = -1
            do k = self%ptr(i), self%ptr(i + 1) - 1
                if (self%ind(k) == i) then
                    self%diag_ptr(i) = k
                    exit
                end if
            end do
            if (self%diag_ptr(i) == -1) then
                self%status = SOLVER_STATUS_DECOMPOSITION_FAILURE
                return
            end if
        end do

        do i = 1, self%num_rows
            do k = self%ptr(i), self%ptr(i + 1) - 1
                work_pos(self%ind(k)) = k
            end do

            do k = self%ptr(i), self%diag_ptr(i) - 1
                col = self%ind(k)
                call dtrsm('R', 'U', 'N', 'N', bs, bs, 1.0d0, &
                           self%val_blocks(:, :, self%diag_ptr(col)), bs, &
                           self%val_blocks(:, :, k), bs)

                do jj = self%diag_ptr(col) + 1, self%ptr(col + 1) - 1
                    j = self%ind(jj)
                    target_idx = work_pos(j)
                    if (target_idx > 0) then
                        call dgemm('N', 'N', bs, bs, bs, -1.0d0, &
                                   self%val_blocks(:, :, k), bs, &
                                   self%val_blocks(:, :, jj), bs, 1.0d0, &
                                   self%val_blocks(:, :, target_idx), bs)
                    end if
                end do
            end do

            call dgetrf(bs, bs, self%val_blocks(:, :, self%diag_ptr(i)), bs, &
                        self%diag_pivots(:, i), info_lapack)

            if (info_lapack /= 0) then
                self%status = SOLVER_STATUS_DECOMPOSITION_FAILURE
                return
            end if

            do k = self%ptr(i), self%ptr(i + 1) - 1
                work_pos(self%ind(k)) = 0
            end do
        end do

        self%status = SOLVER_STATUS_SUCCESS
    end subroutine setup_bsr_ilu0

    !> 前処理の適用
    module subroutine apply_preconditioner_iluk(self, r, z)
        implicit none
        class(type_preconditioner_iluk), intent(inout) :: self
        type(type_vector_dp), intent(in) :: r
        type(type_vector_dp), intent(inout) :: z

        ! 安全装置：セットアップ未完了時は恒等写像を返す
        if (.not. allocated(self%ptr)) then
            write (*, *) "Warning: ILU preconditioner not initialized/setup. Applying Identity."
            call z%copy(r)
            return
        end if

        if (self%is_block) then
            call self%apply_bsr_ilu0(r, z)
        else
            call self%apply_csr_ilu0(r, z)
        end if
    end subroutine apply_preconditioner_iluk

    !> スカラ ILU(0) 適用 (CSR)
    module subroutine apply_csr_ilu0(self, r, z)
        implicit none
        class(type_preconditioner_iluk), intent(inout) :: self
        type(type_vector_dp), intent(in) :: r
        type(type_vector_dp), intent(inout) :: z

        real(real64), dimension(:), pointer :: x
        integer(int32) :: i, k, col

        call z%copy(r)
        x => z%get_data()

        ! --- サイズチェックの追加 ---
        if (size(x) /= self%num_rows) then
            write (*, '(A, I0, A, I0)') "Error in apply_csr_ilu0: Vector size (", size(x), &
                ") does not match Preconditioner Matrix size (", self%num_rows, ")"
            ! ここで処理を中断しないと、次のループでメモリ破壊や変な数値計算エラーが起きる
            self%status = -1
            return
        end if
        ! -------------------------

        ! 前進代入 Lz = r
        do i = 1, self%num_rows
            do k = self%ptr(i), self%diag_ptr(i) - 1
                col = self%ind(k)
                x(i) = x(i) - self%val(k) * x(col)
            end do
        end do

        ! 後退代入 Uz = z
        do i = self%num_rows, 1, -1
            do k = self%diag_ptr(i) + 1, self%ptr(i + 1) - 1
                col = self%ind(k)
                x(i) = x(i) - self%val(k) * x(col)
            end do
            x(i) = x(i) / self%val(self%diag_ptr(i))
        end do
    end subroutine apply_csr_ilu0

    !> ブロック ILU(0) 適用 (BSR)
    module subroutine apply_bsr_ilu0(self, r, z)
        implicit none
        class(type_preconditioner_iluk), intent(inout) :: self
        type(type_vector_dp), intent(in) :: r
        type(type_vector_dp), intent(inout) :: z

        real(real64), dimension(:), pointer :: x
        integer(int32) :: i, k, col, bs, idx_i, idx_c, ierr

        bs = self%block_size
        call z%copy(r)
        x => z%get_data()

        ! --- サイズチェックの追加 ---
        ! ブロック行列の場合、ベクトルサイズは num_rows * block_size のはず
        if (size(x) /= self%num_rows * bs) then
            write (*, '(A, I0, A, I0)') "Error in apply_bsr_ilu0: Vector size (", size(x), &
                ") does not match Preconditioner Matrix size (", self%num_rows * bs, ")"
            self%status = -1
            return
        end if
        ! -------------------------

        ! 前進代入
        do i = 1, self%num_rows
            idx_i = (i - 1) * bs + 1
            do k = self%ptr(i), self%diag_ptr(i) - 1
                col = self%ind(k)
                idx_c = (col - 1) * bs + 1
                call dgemv('N', bs, bs, -1.0d0, &
                           self%val_blocks(:, :, k), bs, &
                           x(idx_c), 1, 1.0d0, &
                           x(idx_i), 1)
            end do
        end do

        ! 後退代入
        do i = self%num_rows, 1, -1
            idx_i = (i - 1) * bs + 1
            do k = self%diag_ptr(i) + 1, self%ptr(i + 1) - 1
                col = self%ind(k)
                idx_c = (col - 1) * bs + 1
                call dgemv('N', bs, bs, -1.0d0, &
                           self%val_blocks(:, :, k), bs, &
                           x(idx_c), 1, 1.0d0, &
                           x(idx_i), 1)
            end do
            call dgetrs('N', bs, 1, &
                        self%val_blocks(:, :, self%diag_ptr(i)), bs, &
                        self%diag_pivots(:, i), &
                        x(idx_i), bs, ierr)
        end do
    end subroutine apply_bsr_ilu0

    !> デストラクタ
    module subroutine destroy_preconditioner_iluk(self)
        implicit none
        class(type_preconditioner_iluk), intent(inout) :: self

        if (allocated(self%val)) deallocate (self%val)
        if (allocated(self%val_blocks)) deallocate (self%val_blocks)
        if (allocated(self%ptr)) deallocate (self%ptr)
        if (allocated(self%ind)) deallocate (self%ind)
        if (allocated(self%diag_ptr)) deallocate (self%diag_ptr)
        if (allocated(self%diag_pivots)) deallocate (self%diag_pivots)
        if (allocated(self%name)) deallocate (self%name)

        self%id = -1
        self%num_rows = -1
        self%is_block = .false.

    end subroutine destroy_preconditioner_iluk

end submodule solver_preconditioner_iluk

!>
!> Implements the procedures for a Degree-of-Freedom (DOF) based Compressed
!> Row Storage (bsr) sparse matrix.
!>
submodule(core_types_algebra_matrix) algebra_matrix_bsr
#ifdef _MKL
    use :: mkl_spblas
#endif
    implicit none

contains

    !>
    !> Initializes the DOF-level bsr matrix structure.
    !>
    module subroutine initialize_bsr(self, num_nodes, row, col, row_blocks, col_blocks)
        implicit none
        class(type_matrix_bsr), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in), optional :: row(:)
        integer(int32), intent(in), optional :: col(:)
        integer(int32), intent(in), optional :: row_blocks
        integer(int32), intent(in), optional :: col_blocks

        ! Argument validation
        if (.not. present(row) .or. .not. present(col)) then
            print *, "Error: row (node_ptr) and col (node_ind) must be provided."
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        if (.not. present(row_blocks) .or. .not. present(col_blocks)) then
            print *, "Error: row_blocks and col_blocks must be provided for bsr matrix."
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        if (size(row) /= num_nodes + 1) then
            print *, "Error: The size of row (node_ptr) array must be num_nodes + 1."
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        self%num_nodes = num_nodes
        self%num_rows = num_nodes
        self%num_ptrs = num_nodes + 1
        self%num_blocks = size(col)
        self%num_block_rows = row_blocks
        self%num_block_cols = col_blocks
        self%nnz = self%num_block_rows * self%num_block_cols * self%num_blocks

        ! Allocate arrays
        call allocate_array(self%ptr, source=row)
        call allocate_array(self%ind, source=col)
        ! Allocate val as (rows, cols, blocks) to improve memory access patterns in block operations
        call allocate_array(self%val, self%num_block_rows, self%num_block_cols, self%num_blocks)

        call self%zero()

        self%is_initialized_matrix = .true.
        self%status = MATRIX_STATUS%SUCCESS
    end subroutine initialize_bsr

    !>
    !> Deallocates all internal arrays.
    !>
    module subroutine destroy_bsr(self)
        implicit none
        class(type_matrix_bsr), intent(inout) :: self
#ifdef _MKL
        integer(int32) :: info
        if (self%is_mkl_committed) then
            info = mkl_sparse_destroy(self%mkl_handle)
            self%is_mkl_committed = .false.
        end if
#endif
        call deallocate_array(self%ptr)
        call deallocate_array(self%ind)
        call deallocate_array(self%val)

        self%num_nodes = 0
        self%num_rows = 0
        self%num_ptrs = 0
        self%nnz = 0

        self%is_initialized_matrix = .false.
        self%status = MATRIX_STATUS%SUCCESS
    end subroutine destroy_bsr

    !>
    !> Returns matrix info.
    !>
    module subroutine get_info_bsr(self, info)
        implicit none
        class(type_matrix_bsr), intent(in) :: self
        type(type_matrix_info), intent(inout) :: info

        info%num_nodes = self%num_nodes
        info%num_rows = self%num_rows
        info%num_ptrs = self%num_ptrs
        info%num_blocks = self%num_blocks
        info%num_block_rows = self%num_block_rows
        info%num_block_cols = self%num_block_cols
        info%nnz = self%nnz
    end subroutine get_info_bsr

    module subroutine get_diagonal_bsr(self, diagonal)
        implicit none
        class(type_matrix_bsr), intent(in) :: self
        type(type_vector_dp), intent(inout) :: diagonal

        integer(int32) :: i, row_start, row_end, j, k, m

        !$omp parallel do private(i, row_start, row_end, j, k, m)
        do i = 1, self%num_ptrs - 1
            row_start = self%ptr(i)
            row_end = self%ptr(i + 1) - 1
            do j = row_start, row_end
                if (self%ind(j) == i) then
                    do k = 1, self%num_block_rows
                        do m = 1, self%num_block_cols
                            if (k == m) then
                                ! Access val(row_in_block, col_in_block, block_index)
                                call diagonal%set(VECTOR_OPS%INS, (i - 1) * self%num_block_rows + k, self%val(k, m, j))
                            end if
                        end do
                    end do
                    exit
                end if
            end do
        end do
        !$omp end parallel do

    end subroutine get_diagonal_bsr

    module subroutine get_diagonal_block_bsr(self, target_block, diagonal_block)
        implicit none
        class(type_matrix_bsr), intent(in) :: self
        integer(int32), intent(in) :: target_block
        real(real64), intent(inout) :: diagonal_block(:, :)

        integer(int32) :: row_start, row_end, j, k, m
        ! Initialize diagonal_block to zero
        diagonal_block = 0.0d0
        row_start = self%ptr(target_block)
        row_end = self%ptr(target_block + 1) - 1
        do j = row_start, row_end
            if (self%ind(j) == target_block) then
                do k = 1, self%num_block_rows
                    do m = 1, self%num_block_cols
                        diagonal_block(k, m) = self%val(k, m, j)
                    end do
                end do
                exit
            end if
        end do
    end subroutine get_diagonal_block_bsr

    !> Getters for internal arrays
    module function get_ptr_bsr(self) result(ptr)
        implicit none
        class(type_matrix_bsr), intent(in), target :: self
        integer(int32), dimension(:), pointer :: ptr
        ptr => self%ptr
    end function get_ptr_bsr

    module function get_ind_bsr(self) result(ind)
        implicit none
        class(type_matrix_bsr), intent(in), target :: self
        integer(int32), dimension(:), pointer :: ind
        ind => self%ind
    end function get_ind_bsr

    module function get_val_bsr(self) result(val)
        implicit none
        class(type_matrix_bsr), intent(in), target :: self
        real(real64), dimension(:, :, :), pointer :: val
        val => self%val
    end function get_val_bsr

    module subroutine set_value_bsr(self, op, row, col, value)
        implicit none
        class(type_matrix_bsr), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: row, col
        real(real64), intent(in) :: value
        error stop "Error: set_value is only permitted for dense matrix."
    end subroutine set_value_bsr

    !> Sets a stored entry by its flat (column-major) position in val(:,:,:) (no search).
    module subroutine set_value_at_bsr(self, op, idx, value)
        implicit none
        class(type_matrix_bsr), intent(inout), target :: self
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: idx
        real(real64), intent(in) :: value

        real(real64), pointer, contiguous :: flat(:)
        integer(int32) :: n

        if (.not. MATRIX_OPS%is_valid(op)) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        n = size(self%val)
        if (.not. value_in_range(idx, 1, n)) then
            self%status = MATRIX_STATUS%OUT_OF_MEMORY
            return
        end if

        flat(1:n) => self%val
        select case (op%ID)
        case (MATRIX_OPS%INS%ID)
            flat(idx) = value
        case (MATRIX_OPS%ADD%ID)
            flat(idx) = flat(idx) + value
        case default
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end select
    end subroutine set_value_at_bsr

    !>
    !> Sets all non-zero entries in a specific row to a single scalar value.
    !>
    module subroutine set_row_bsr(self, op, row, value, row_block)
        implicit none
        class(type_matrix_bsr), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: row
        real(real64), intent(in) :: value
        integer(int32), intent(in), optional :: row_block

        integer(int32) :: is, ie, k
        integer(int32) :: r_start, r_end, r

        if (.not. MATRIX_OPS%is_valid(op)) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        ! 範囲チェック
        if (.not. value_in_range(row, 1, self%num_rows)) then
            self%status = MATRIX_STATUS%OUT_OF_MEMORY
            return
        end if

        is = self%ptr(row)
        ie = self%ptr(row + 1) - 1

        if (present(row_block)) then
            if (row_block < 1 .or. row_block > self%num_block_rows) then
                self%status = MATRIX_STATUS%OUT_OF_MEMORY
                return
            end if
            r_start = row_block
            r_end = row_block
        else
            r_start = 1
            r_end = self%num_block_rows
        end if

        select case (op%ID)
        case (MATRIX_OPS%INS%ID)
            do k = is, ie
                self%val(r_start:r_end, :, k) = value
            end do
        case (MATRIX_OPS%ADD%ID)
            do k = is, ie
                if (self%ind(k) == row) then
                    do r = r_start, r_end
                        self%val(r, :, k) = self%val(r, :, k) + value
                    end do
                    exit
                end if
            end do
        case default
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end select

        self%is_mkl_committed = .false.
        self%status = MATRIX_STATUS%SUCCESS
    end subroutine set_row_bsr

    module subroutine scale_bsr(self, op, alpha)
        implicit none
        class(type_matrix_bsr), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        type(type_vector_dp), intent(in) :: alpha

        real(real64), dimension(:), pointer :: alpha_data

        integer(int32) :: i, j, rb, cb, col
        integer(int32) :: row_start, row_end
        integer(int32) :: row_dof, col_dof
        integer(int32) :: nrequired
        integer(int32) :: bnr, bnc ! ブロックサイズを保持する一時変数

        if (.not. self%is_initialized()) then
            self%status = MATRIX_STATUS%NOT_INITIALIZED
            return
        end if

        alpha_data => alpha%get_data()

        bnr = self%num_block_rows
        bnc = self%num_block_cols

        ! SCALE_SYMM_DIAG requires square blocks
        if (op == MATRIX_OPS%SCALE_SYMM_DIAG .and. bnr /= bnc) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        nrequired = bnr * self%num_nodes
        if (size(alpha_data) /= nrequired) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        select case (op%ID)
        case (MATRIX_OPS%SCALE_SYMM_DIAG%ID)
            ! A <- D^{-1/2} A D^{-1/2}
            !$omp parallel do default(shared) private(i,j,rb,cb,col,row_start,row_end,row_dof,col_dof)
            do i = 1, self%num_ptrs - 1
                row_start = self%ptr(i)
                row_end = self%ptr(i + 1) - 1
                do j = row_start, row_end
                    col = self%ind(j)
                    do cb = 1, bnc
                        col_dof = (col - 1) * bnr + cb
                        do rb = 1, bnr
                            row_dof = (i - 1) * bnr + rb
                            self%val(rb, cb, j) = self%val(rb, cb, j) * &
                                                  (alpha_data(row_dof) * alpha_data(col_dof))
                        end do
                    end do
                end do
            end do
            !$omp end parallel do
            self%status = MATRIX_STATUS%SUCCESS
        case (MATRIX_OPS%SCALE_JACOBI%ID)
            ! A <- D^{-1} A
            !$omp parallel do default(shared) private(i,j,rb,cb,row_start,row_end,row_dof)
            do i = 1, self%num_ptrs - 1
                row_start = self%ptr(i)
                row_end = self%ptr(i + 1) - 1
                do j = row_start, row_end
                    do cb = 1, bnc
                        do rb = 1, bnr
                            row_dof = (i - 1) * bnr + rb
                            self%val(rb, cb, j) = self%val(rb, cb, j) * alpha_data(row_dof)
                        end do
                    end do
                end do
            end do
            !$omp end parallel do
            self%status = MATRIX_STATUS%SUCCESS
        case (MATRIX_OPS%SCALE_COL%ID)
            ! A(:,j) <- A(:,j) * alpha(j)
            !$omp parallel do default(shared) private(i,j,cb,col,row_start,row_end,col_dof)
            do i = 1, self%num_ptrs - 1
                row_start = self%ptr(i)
                row_end = self%ptr(i + 1) - 1
                do j = row_start, row_end
                    col = self%ind(j)
                    do cb = 1, bnc
                        col_dof = (col - 1) * bnr + cb
                        self%val(:, cb, j) = self%val(:, cb, j) * alpha_data(col_dof)
                    end do
                end do
            end do
            !$omp end parallel do
            self%status = MATRIX_STATUS%SUCCESS
        case default
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end select

        self%is_mkl_committed = .false.

    end subroutine scale_bsr
    !>
    !> Zero out the matrix.
    !>
    module subroutine zero_all_bsr(self)
        implicit none
        class(type_matrix_bsr), intent(inout) :: self

        self%val(:, :, :) = 0.0d0
        self%is_mkl_committed = .false.
        self%status = MATRIX_STATUS%SUCCESS
    end subroutine zero_all_bsr

    !> Sets all values in a specified row of the bsr matrix to zero.
    module subroutine zero_row_bsr(self, row, row_block)
        implicit none
        class(type_matrix_bsr), intent(inout) :: self
        integer(int32), intent(in) :: row
        integer(int32), intent(in), optional :: row_block

        integer(int32) :: is, ie, k, r_start, r_end, r

        if (.not. value_in_range(row, 1, self%num_rows)) then
            self%status = MATRIX_STATUS%OUT_OF_MEMORY
            return
        end if

        is = self%ptr(row)
        ie = self%ptr(row + 1) - 1

        if (present(row_block)) then
            if (row_block < 1 .or. row_block > self%num_block_rows) then
                self%status = MATRIX_STATUS%OUT_OF_MEMORY
                return
            end if
            r_start = row_block
            r_end = row_block
        else
            r_start = 1
            r_end = self%num_block_rows
        end if

        do k = is, ie
            do r = r_start, r_end
                self%val(r, :, k) = 0.0d0
            end do
        end do

        self%is_mkl_committed = .false.
        self%status = MATRIX_STATUS%SUCCESS
    end subroutine zero_row_bsr

    !>
    !> Display.
    !>
    module subroutine display_bsr(self, unit_in)
        implicit none
        class(type_matrix_bsr), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in
        integer(int32) :: i, r, row_start, row_end, rb, cb
        integer(int32) :: unit

        unit = optval(unit_in, output_unit)

        if (.not. self%is_initialized()) then
            write (unit, '(a)') "Matrix is not initialized."
            return
        end if

        write (unit, '(a,i0,2x,a,i0,a)') "bsr Matrix (Nodes= ", self%num_nodes, ", nnz= ", self%nnz, ")"
        do r = 1, self%num_nodes
            row_start = self%ptr(r)
            row_end = self%ptr(r + 1) - 1
            do i = row_start, row_end
                do rb = 1, self%num_block_rows
                    do cb = 1, self%num_block_cols
                        ! Access val(row_in_block, col_in_block, block_index)
                        write (unit, '(a,i0,a,i0,a, i0,a,i0,a, es16.8e3)') &
                            "Node(", r, ",", self%ind(i), ") Block(", rb, ",", cb, "): ", &
                            self%val(rb, cb, i)
                    end do
                end do
            end do
        end do
    end subroutine display_bsr

    !>
    !> Scatter a local element matrix into BSR global matrix using pre-computed block indices.
    !> O(1) direct address scatter — no binary search.
    !> row_sys/col_sys: 1-based DOF offsets within the BSR block for sub-block scattering.
    !> op = MATRIX_OPS%INS overwrites; op = MATRIX_OPS%ADD accumulates.
    !>
    module subroutine set_local_matrix_bsr(self, op, n_local, indices, row_sys, col_sys, local_data)
        implicit none
        class(type_matrix_bsr), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: n_local
        integer(int32), intent(in) :: indices(:, :)
        integer(int32), intent(in) :: row_sys, col_sys
        real(real64), dimension(:, :), intent(in) :: local_data

        integer(int32) :: i_node, j_node, rb, cb, idx, local_row, local_col
        integer(int32) :: n_dof_row, n_dof_col

        n_dof_row = size(local_data, 1) / n_local
        n_dof_col = size(local_data, 2) / n_local

        select case (op%ID)
        case (MATRIX_OPS%INS%ID)
            do j_node = 1, n_local
                do i_node = 1, n_local
                    idx = indices(i_node, j_node)
                    if (idx > 0) then
                        do cb = 1, n_dof_col
                            do rb = 1, n_dof_row
                                local_row = (i_node - 1) * n_dof_row + rb
                                local_col = (j_node - 1) * n_dof_col + cb
                                self%val(row_sys + rb - 1, col_sys + cb - 1, idx) = &
                                    local_data(local_row, local_col)
                            end do
                        end do
                    end if
                end do
            end do
        case (MATRIX_OPS%ADD%ID)
            do j_node = 1, n_local
                do i_node = 1, n_local
                    idx = indices(i_node, j_node)
                    if (idx > 0) then
                        do cb = 1, n_dof_col
                            do rb = 1, n_dof_row
                                local_row = (i_node - 1) * n_dof_row + rb
                                local_col = (j_node - 1) * n_dof_col + cb
                                self%val(row_sys + rb - 1, col_sys + cb - 1, idx) = &
                                    self%val(row_sys + rb - 1, col_sys + cb - 1, idx) + &
                                    local_data(local_row, local_col)
                            end do
                        end do
                    end if
                end do
            end do
        case default
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end select

        self%is_mkl_committed = .false.
    end subroutine set_local_matrix_bsr

    !>
    !> Register val/ptr/ind with MKL Sparse BLAS (Inspector phase) and run mkl_sparse_optimize.
    !> Must be called after assembly is complete and before any SpMV via gemv_matrix_bsr.
    !>
    module subroutine commit_to_mkl_bsr(self, ierr)
        implicit none
        class(type_matrix_bsr), intent(inout) :: self
        integer(int32), intent(out), optional :: ierr
#ifdef _MKL
        integer(int32) :: info

        if (.not. self%is_initialized_matrix) then
            if (present(ierr)) ierr = MATRIX_STATUS%ILL_OPERATIONS%ID
            return
        end if
        if (self%num_block_rows /= self%num_block_cols) then
            if (present(ierr)) ierr = MATRIX_STATUS%ILL_OPERATIONS%ID
            return
        end if

        if (self%is_mkl_committed) then
            info = mkl_sparse_destroy(self%mkl_handle)
            self%is_mkl_committed = .false.
        end if

        info = mkl_sparse_d_create_bsr( &
            self%mkl_handle, &
            SPARSE_INDEX_BASE_ONE, &
            SPARSE_LAYOUT_COLUMN_MAJOR, &
            self%num_nodes, self%num_nodes, self%num_block_rows, &
            self%ptr(1), self%ptr(2), &
            self%ind(1), self%val(1, 1, 1))

        if (info == SPARSE_STATUS_SUCCESS) then
            info = mkl_sparse_optimize(self%mkl_handle)
            self%is_mkl_committed = .true.
            if (present(ierr)) then
                if (info == SPARSE_STATUS_SUCCESS) then
                    ierr = MATRIX_STATUS%SUCCESS%ID
                else
                    ierr = MATRIX_STATUS%ILL_OPERATIONS%ID
                end if
            end if
        else
            if (present(ierr)) ierr = MATRIX_STATUS%ILL_OPERATIONS%ID
        end if
#else
        if (present(ierr)) ierr = MATRIX_STATUS%NOT_IMPLEMENTED%ID
#endif
    end subroutine commit_to_mkl_bsr

    !> Returns .true. if MKL handle has been committed and is ready for SpMV.
    pure module function is_mkl_handle_ready_bsr(self) result(ready)
        implicit none
        class(type_matrix_bsr), intent(in) :: self
        logical :: ready
        ready = self%is_mkl_committed
    end function is_mkl_handle_ready_bsr

#ifdef _MKL
    !> Return the committed MKL sparse handle.
    module function get_mkl_handle_bsr(self) result(handle)
        implicit none
        class(type_matrix_bsr), intent(in) :: self
        type(sparse_matrix_t) :: handle
        handle = self%mkl_handle
    end function get_mkl_handle_bsr
#endif

end submodule algebra_matrix_bsr

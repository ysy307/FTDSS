!>
!> Implements the procedures for a Degree-of-Freedom (DOF) based Compressed
!> Row Storage (csr) sparse matrix.
!>
submodule(core_types_algebra_matrix) algebra_matrix_csr
    implicit none

contains

    !>
    !> Initializes the DOF-level csr matrix structure from a node-level adjacency pattern.
    !> It assumes the input node-level column indices (`col`) are sorted for each row segment.
    !> This routine expands the node-level graph into a full DOF-level matrix sparsity pattern.
    !>
    module subroutine initialize_type_matrix_csr(self, num_nodes, row, col, row_blocks, col_blocks)
        implicit none
        class(type_matrix_csr), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in), optional :: row(:)
        integer(int32), intent(in), optional :: col(:)
        integer(int32), intent(in), optional :: row_blocks
        integer(int32), intent(in), optional :: col_blocks

        ! Argument validation
        if (.not. present(row) .or. .not. present(col)) then
            print *, "Error: row (node_ptr) and col (node_ind) must be provided."
            stop
        end if

        if (size(row) /= num_nodes + 1) then
            print *, "Error: The size of row (node_ptr) array must be num_nodes + 1."
            stop
        end if

        self%num_nodes = num_nodes
        self%num_rows = num_nodes
        self%num_ptrs = num_nodes + 1
        self%nnz = size(col)

        ! Allocate arrays
        call allocate_array(self%ptr, source=row)
        call allocate_array(self%ind, source=col)
        call allocate_array(self%val, self%nnz)
        ! Initialize value array to zero
        call self%zero()

        self%is_initialized_matrix = .true.
        self%status = MATRIX_STATUS%SUCCESS
    end subroutine initialize_type_matrix_csr

    !>
    !> Deallocates all internal arrays of the csr matrix object.
    !>
    module subroutine destroy_csr(self)
        implicit none
        class(type_matrix_csr), intent(inout) :: self

        call deallocate_array(self%ptr)
        call deallocate_array(self%ind)
        call deallocate_array(self%val)

        self%num_nodes = 0
        self%num_rows = 0
        self%num_ptrs = 0
        self%nnz = 0

        self%is_initialized_matrix = .false.
        self%status = MATRIX_STATUS%SUCCESS
    end subroutine destroy_csr

    !>
    !> Returns the number of rows in the matrix.
    !>
    module subroutine get_info_csr(self, info)
        implicit none
        !> The matrix object.
        class(type_matrix_csr), intent(in) :: self
        !> The matrix information structure to populate.
        type(type_matrix_info), intent(inout) :: info

        info%num_nodes = self%num_nodes
        info%num_rows = self%num_rows
        info%num_ptrs = self%num_ptrs
        info%nnz = self%nnz
    end subroutine get_info_csr

    module subroutine get_diagonal_csr(self, diagonal)
        implicit none
        class(type_matrix_csr), intent(in) :: self
        type(type_vector_dp), intent(inout) :: diagonal

        integer(int32) :: i, row_start, row_end, j

        ! Extract diagonal elements into internal storage first
        do i = 1, self%num_ptrs - 1
            row_start = self%ptr(i)
            row_end = self%ptr(i + 1) - 1
            do j = row_start, row_end
                if (self%ind(j) == i) then
                    call diagonal%set(VECTOR_OPS%INS, i, self%val(j))
                    exit
                end if
            end do
        end do

    end subroutine get_diagonal_csr

    !>
    !> Returns a pointer to the internal CSR pointer array (`ptr`).
    !>
    module function get_ptr_csr(self) result(ptr)
        implicit none
        !> The csr matrix object.
        class(type_matrix_csr), intent(in), target :: self
        !> A pointer to the CSR `ptr` array.
        integer(int32), dimension(:), pointer :: ptr

        ptr => self%ptr
    end function get_ptr_csr

    !>
    !> Returns a pointer to the internal CSR column index array (`ind`).
    !>
    module function get_ind_csr(self) result(ind)
        implicit none
        !> The csr matrix object.
        class(type_matrix_csr), intent(in), target :: self
        !> A pointer to the CSR `ind` array.
        integer(int32), dimension(:), pointer :: ind

        ind => self%ind
    end function get_ind_csr

    !>
    !> Returns a pointer to the internal CSR value array (`val`).
    !>
    module function get_val_csr(self) result(val)
        implicit none
        !> The csr matrix object.
        class(type_matrix_csr), intent(in), target :: self
        !> A pointer to the CSR `val` array.
        real(real64), dimension(:), pointer :: val

        val => self%val
    end function get_val_csr

    !>
    !> Sets the value of a specific entry in the sparse matrix.
    !>
    module subroutine set_value_csr(self, op, row, col, value)
        implicit none
        !> The csr matrix object.
        class(type_matrix_csr), intent(inout) :: self
        !> The operation to perform.
        type(type_constant_id), intent(in) :: op
        !> The 1-based node index for the row.
        integer(int32), intent(in) :: row
        !> The 1-based node index for the column.
        integer(int32), intent(in) :: col
        !> The value to set at the specified entry.
        real(real64), intent(in) :: value

        integer(int32) :: index

        if (.not. MATRIX_OPS%is_valid(op)) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        index = self%find(row, col)
#ifdef USE_DEBUG
        if (index > 0) then
#endif
            if (op == MATRIX_OPS%INS) then
                self%val(index) = value
            else if (op == MATRIX_OPS%ADD) then
                self%val(index) = self%val(index) + value
            else
                self%status = MATRIX_STATUS%ILL_OPERATIONS
            end if

#ifdef USE_DEBUG
        else
            self%status = MATRIX_STATUS%OUT_OF_MEMORY
        end if
#endif
    end subroutine set_value_csr

    !>
    !> Sets the value of a specific entry in the sparse matrix.
    !>
    module subroutine set_value_block_csr(self, op, row, col, row_block, col_block, value)
        implicit none
        !> The csr matrix object.
        class(type_matrix_csr), intent(inout) :: self
        !> The operation to perform.
        type(type_constant_id), intent(in) :: op
        !> The 1-based node index for the row.
        integer(int32), intent(in) :: row
        !> The 1-based node index for the column.
        integer(int32), intent(in) :: col
        !> The block row index within the block.
        integer(int32), intent(in) :: row_block
        !> The block column index within the block.
        integer(int32), intent(in) :: col_block
        !> The value to set at the specified entry.
        real(real64), intent(in) :: value

        self%status = MATRIX_STATUS%NOT_IMPLEMENTED
    end subroutine set_value_block_csr

    !>
    !> Sets all non-zero entries in a specific row to a single scalar value.
    !>
    module subroutine set_row_csr(self, op, row, value, row_block)
        implicit none
        !> The csr matrix object.
        class(type_matrix_csr), intent(inout) :: self
        !> The operation to perform.
        type(type_constant_id), intent(in) :: op
        !> The 1-based node index for the row.
        integer(int32), intent(in) :: row
        !> The scalar value to assign.
        real(real64), intent(in) :: value
        !> The block row index.
        integer(int32), intent(in), optional :: row_block

        integer(int32) :: is, ie

        if (.not. MATRIX_OPS%is_valid(op)) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        is = self%ptr(row)
        ie = self%ptr(row + 1) - 1

        if (op == MATRIX_OPS%INS) then
            self%val(is:ie) = value
        else if (op == MATRIX_OPS%ADD) then
            self%val(is:ie) = self%val(is:ie) + value
        else
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end if

    end subroutine set_row_csr

    !>
    !> Sets all stored non-zero values in the matrix to a single scalar value.
    !>
    module subroutine set_all_csr(self, op, value)
        implicit none
        !> The csr matrix object.
        class(type_matrix_csr), intent(inout) :: self
        !> The operation to perform.
        type(type_constant_id), intent(in) :: op
        !> The scalar value to assign to all non-zero entries.
        real(real64), intent(in) :: value

        if (.not. MATRIX_OPS%is_valid(op)) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        if (op == MATRIX_OPS%INS) then
            self%val = value
        else if (op == MATRIX_OPS%ADD) then
            self%val = self%val + value
        else
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end if

    end subroutine set_all_csr

    !>
    !> Scales all stored values in the CSR matrix.
    module subroutine scale_csr(self, op, alpha)
        implicit none
        !> The CSR matrix object.
        class(type_matrix_csr), intent(inout) :: self
        !> The operation to perform
        type(type_constant_id), intent(in) :: op
        !> The scaling vector.
        type(type_vector_dp), intent(in) :: alpha

        integer(int32) :: i, k, col
        real(real64), dimension(:), pointer :: alpha_data

        if (.not. MATRIX_OPS%is_valid(op)) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        alpha_data => alpha%get_data()

        ! alphaのサイズチェック
        if (size(alpha_data) /= self%num_nodes) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        !-------------------------------------------------
        ! Symmetric Scaling: A_ij <- A_ij * alpha(i) * alpha(j)
        ! (alpha には 1/sqrt(|D|) が入っている)
        !-------------------------------------------------
        if (op == MATRIX_OPS%SCALE_SYMM_DIAG) then
            !$omp parallel do default(shared) private(i, k, col) schedule(static)
            do i = 1, self%num_nodes
                ! i行目の非ゼロ要素を走査
                do k = self%ptr(i), self%ptr(i + 1) - 1
                    col = self%ind(k) ! 列番号を取得

                    ! A_ij = A_ij * D_i * D_j
                    self%val(k) = self%val(k) * alpha_data(i) * alpha_data(col)
                end do
            end do
            !$omp end parallel do

            self%status = MATRIX_STATUS%SUCCESS

            !-------------------------------------------------
            ! Jacobi Scaling: A_ij <- A_ij * alpha(i)
            ! (alpha には 1/D が入っている)
            !-------------------------------------------------
        else if (op == MATRIX_OPS%SCALE_JACOBI) then

            !$omp parallel do default(shared) private(i, k) schedule(static)
            do i = 1, self%num_nodes
                ! i行目の全要素に対して、一律に alpha(i) を掛ける
                ! 行単位でアクセスするため、CSR形式では非常にキャッシュ効率が良い
                do k = self%ptr(i), self%ptr(i + 1) - 1
                    self%val(k) = self%val(k) * alpha_data(i)
                end do
            end do
            !$omp end parallel do

            self%status = MATRIX_STATUS%SUCCESS

        else
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end if

    end subroutine scale_csr

    !>
    !> Sets all stored values in the matrix to zero.
    !>
    module subroutine zero_all_csr(self)
        implicit none
        !> The csr matrix object.
        class(type_matrix_csr), intent(inout) :: self

        self%val = 0.0d0
    end subroutine zero_all_csr

    !> Sets all stored values in a specific row to zero.
    module subroutine zero_row_csr(self, row, row_block)
        implicit none
        !> The csr matrix object.
        class(type_matrix_csr), intent(inout) :: self
        !> The 1-based node index for the row.
        integer(int32), intent(in) :: row
        !> The block row index.
        integer(int32), intent(in), optional :: row_block

        integer(int32) :: is, ie
        is = self%ptr(row)
        ie = self%ptr(row + 1) - 1

        self%val(is:ie) = 0.0d0
    end subroutine zero_row_csr

    !>
    !> Finds the 1-based index in the `val` and `ind` arrays corresponding to a specific matrix entry.
    !>
    pure module function find_csr(self, row, col) result(index)
        implicit none
        !> The csr matrix object.
        class(type_matrix_csr), intent(in) :: self
        !> The 1-based node index for the row.
        integer(int32), intent(in) :: row
        !> The 1-based node index for the column.
        integer(int32), intent(in) :: col
        !> The 1-based index in the `val`/`ind` arrays, or 0 if not found.

        integer(int32) :: index
        integer(int32) :: ptr_start, ptr_end

        index = 0

#ifdef USE_DEBUG
        if (row < 1 .or. row > self%num_nodes) return
        if (col < 1 .or. col > self%num_nodes) return
#endif

        ptr_start = self%ptr(row)
        ptr_end = self%ptr(row + 1) - 1

        ! Perform a binary search within the relevant segment of the index array.
        index = binary_find(col, self%ind, ptr_start, ptr_end)

    end function find_csr

    !>
    !> Prints the non-zero contents of the sparse matrix to standard output.
    !>
    module subroutine display_csr(self, unit_in)
        implicit none
        !> The csr matrix object to display.
        class(type_matrix_csr), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: i, r, row_start, row_end
        integer(int32) :: unit

        unit = optval(unit_in, output_unit)

        write (unit, '(a,i0,2x,a,i0,a)') "csr Matrix (dims= ", self%num_rows, ", nnz= ", self%nnz, ")"
        do r = 1, self%num_rows
            row_start = self%ptr(r)
            row_end = self%ptr(r + 1) - 1
            do i = row_start, row_end
                write (unit, '(2(i0, ", "), es16.8e3)') r, self%ind(i), self%val(i)
            end do
        end do
    end subroutine display_csr

end submodule algebra_matrix_csr

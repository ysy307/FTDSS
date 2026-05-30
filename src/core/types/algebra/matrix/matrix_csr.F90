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
    module subroutine initialize_csr(self, num_nodes, row, col, row_blocks, col_blocks)
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
    end subroutine initialize_csr

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
        class(type_matrix_csr), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: row, col
        real(real64), intent(in) :: value
        error stop "Error: set_value is only permitted for dense matrix."
    end subroutine set_value_csr

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

        select case (op%ID)
        case (MATRIX_OPS%INS%ID)
            self%val(is:ie) = value
        case (MATRIX_OPS%ADD%ID)
            self%val(is:ie) = self%val(is:ie) + value
        case default
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end select
    end subroutine set_row_csr

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

        if (size(alpha_data) /= self%num_nodes) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        select case (op%ID)
        case (MATRIX_OPS%SCALE_SYMM_DIAG%ID)
            !$omp parallel do default(shared) private(i, k, col) schedule(static)
            do i = 1, self%num_nodes
                do k = self%ptr(i), self%ptr(i + 1) - 1
                    col = self%ind(k)
                    self%val(k) = self%val(k) * alpha_data(i) * alpha_data(col)
                end do
            end do
            !$omp end parallel do
            self%status = MATRIX_STATUS%SUCCESS
        case (MATRIX_OPS%SCALE_JACOBI%ID)
            !$omp parallel do default(shared) private(i, k) schedule(static)
            do i = 1, self%num_nodes
                do k = self%ptr(i), self%ptr(i + 1) - 1
                    self%val(k) = self%val(k) * alpha_data(i)
                end do
            end do
            !$omp end parallel do
            self%status = MATRIX_STATUS%SUCCESS
        case default
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end select

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

    module subroutine set_local_matrix_csr(self, op, n_local, indices, row_sys, col_sys, local_data)
        implicit none
        class(type_matrix_csr), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: n_local
        integer(int32), intent(in) :: indices(:, :)
        integer(int32), intent(in) :: row_sys, col_sys
        real(real64), dimension(:, :), intent(in) :: local_data
        integer(int32) :: i, j, idx

        select case (op%ID)
        case (MATRIX_OPS%INS%ID)
            do j = 1, n_local
                do i = 1, n_local
                    idx = indices(i, j)
                    if (idx > 0) self%val(idx) = local_data(i, j)
                end do
            end do
        case (MATRIX_OPS%ADD%ID)
            do j = 1, n_local
                do i = 1, n_local
                    idx = indices(i, j)
                    if (idx > 0) self%val(idx) = self%val(idx) + local_data(i, j)
                end do
            end do
        case default
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end select
    end subroutine set_local_matrix_csr

end submodule algebra_matrix_csr

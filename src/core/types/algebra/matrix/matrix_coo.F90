!>
!> Implements the procedures for the Coordinate (COO) sparse matrix type.
!>
submodule(core_types_algebra_matrix) algebra_matrix_coo
    implicit none

contains

    !>
    !> Initializes the DOF-level COO matrix structure from a node-level sparsity pattern.
    !> It assumes the input node-level `row` and `col` arrays are sorted appropriately
    !> to produce a sorted DOF-level COO matrix without requiring an explicit sort.
    !>
    module subroutine initialize_coo(self, num_nodes, row, col, row_blocks, col_blocks)
        implicit none
        !> The COO matrix object to initialize.
        class(type_matrix_coo), intent(inout) :: self
        !> The total number of nodes in the mesh.
        integer(int32), intent(in) :: num_nodes
        !> The node-level row indices defining the sparsity pattern.
        integer(int32), intent(in), optional :: row(:)
        !> The node-level column indices defining the sparsity pattern.
        integer(int32), intent(in), optional :: col(:)
        !> The number of row blocks.
        integer(int32), intent(in), optional :: row_blocks
        !> The number of column blocks.
        integer(int32), intent(in), optional :: col_blocks

        if (.not. present(row) .or. .not. present(col)) then
            print *, "Error: row and col must be provided for COO initialization."
            stop
        end if

        if (size(row) /= size(col)) then
            print *, "Error: row and col arrays must be the same length."
            stop
        end if

        ! Calculate final matrix dimensions
        self%num_nodes = num_nodes
        self%num_rows = size(row)
        self%num_cols = size(col)
        self%nnz = size(col)

        ! Allocate memory for COO arrays
        call allocate_array(self%row, source=row)
        call allocate_array(self%col, source=col)
        call allocate_array(self%val, self%nnz)

        ! Initialize values to zero
        call self%zero()

        self%is_initialized_matrix = .true.
        self%status = MATRIX_STATUS%SUCCESS
    end subroutine initialize_coo

    !>
    !> Deallocates all internal arrays of the COO matrix object.
    !>
    module subroutine destroy_coo(self)
        implicit none
        !> The COO matrix object to destroy.
        class(type_matrix_coo), intent(inout) :: self

        call deallocate_array(self%row)
        call deallocate_array(self%col)
        call deallocate_array(self%val)
        self%num_nodes = 0
        self%num_rows = 0
        self%num_cols = 0
        self%nnz = 0

        self%is_initialized_matrix = .false.
        self%status = MATRIX_STATUS%SUCCESS
    end subroutine destroy_coo

    !>
    !> Returns the number of non-zero entries in the matrix.
    !>
    module subroutine get_info_coo(self, info)
        implicit none
        !> The matrix object.
        class(type_matrix_coo), intent(in) :: self
        !> The matrix information structure to populate.
        type(type_matrix_info), intent(inout) :: info

        info%num_nodes = self%num_nodes
        info%num_rows = self%num_rows
        info%nnz = self%nnz
    end subroutine get_info_coo

    module subroutine get_diagonal_coo(self, diagonal)
        implicit none
        class(type_matrix_coo), intent(in) :: self
        type(type_vector_dp), intent(inout) :: diagonal

        integer(int32) :: i

        ! Compute the diagonal entries
        do i = 1, self%nnz
            if (self%row(i) == self%col(i)) then
                call diagonal%set(VECTOR_OPS%INS, self%row(i), self%val(i))
            end if
        end do

    end subroutine get_diagonal_coo

    !>
    !> Returns a pointer to the internal `row` index array.
    !>
    module function get_row_coo(self) result(row)
        implicit none
        !> The COO matrix object.
        class(type_matrix_coo), intent(in), target :: self
        !> A pointer to the `row` index array.
        integer(int32), dimension(:), pointer :: row

        row => self%row
    end function get_row_coo

    !>
    !> Returns a pointer to the internal `col` index array.
    !>
    module function get_col_coo(self) result(col)
        implicit none
        !> The COO matrix object.
        class(type_matrix_coo), intent(in), target :: self
        !> A pointer to the `col` index array.
        integer(int32), dimension(:), pointer :: col

        col => self%col
    end function get_col_coo

    !>
    !> Returns a pointer to the internal `val` array.
    !>
    module function get_val_coo(self) result(val)
        implicit none
        !> The COO matrix object.
        class(type_matrix_coo), intent(in), target :: self
        !> A pointer to the `val` array.
        real(real64), dimension(:), pointer :: val

        val => self%val
    end function get_val_coo

    !>
    !> Sets the value of a single entry in the matrix.
    !>
    module subroutine set_value_coo(self, op, row, col, value)
        implicit none
        class(type_matrix_coo), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: row, col
        real(real64), intent(in) :: value
        error stop "Error: set_value is only permitted for dense matrix."
    end subroutine set_value_coo

    !>
    !> Sets all non-zero entries in a specified row to a single scalar value.
    !> Note: This is an inefficient O(nnz) operation for the COO format.
    !>
    module subroutine set_row_coo(self, op, row, value, row_block)
        implicit none
        !> The COO matrix object.
        class(type_matrix_coo), intent(inout) :: self
        !> The operation to perform
        type(type_constant_id), intent(in) :: op
        !> The 1-based node index for the row.
        integer(int32), intent(in) :: row
        !> The scalar value to assign.
        real(real64), intent(in) :: value
        !> The block row index.
        integer(int32), intent(in), optional :: row_block

        integer(int32) :: i

        if (.not. MATRIX_OPS%is_valid(op)) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        select case (op%ID)
        case (MATRIX_OPS%INS%ID)
            do i = 1, self%nnz
                if (self%row(i) == row) self%val(i) = value
            end do
        case (MATRIX_OPS%ADD%ID)
            do i = 1, self%nnz
                if (self%row(i) == row) self%val(i) = self%val(i) + value
            end do
        case default
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end select
    end subroutine set_row_coo

    !>
    !> Scales all stored values in the matrix by a scalar factor vector.
    module subroutine scale_coo(self, op, alpha)
        implicit none
        !> The COO matrix object.
        class(type_matrix_coo), intent(inout) :: self
        !> The operation to perform
        type(type_constant_id), intent(in) :: op
        !> The scaling vector (derived from diagonal).
        type(type_vector_dp), intent(in) :: alpha

        integer(int32) :: i, r, c

        real(real64), dimension(:), pointer :: alpha_data
        alpha_data => alpha%get_data()

        if (size(alpha_data) /= self%num_nodes) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        select case (op%ID)
        case (MATRIX_OPS%SCALE_SYMM_DIAG%ID)
            !$omp parallel do default(shared) private(i, r, c)
            do i = 1, self%nnz
                r = self%row(i)
                c = self%col(i)
                self%val(i) = self%val(i) * alpha_data(r) * alpha_data(c)
            end do
            !$omp end parallel do
            self%status = MATRIX_STATUS%SUCCESS
        case (MATRIX_OPS%SCALE_JACOBI%ID)
            !$omp parallel do default(shared) private(i, r)
            do i = 1, self%nnz
                r = self%row(i)
                self%val(i) = self%val(i) * alpha_data(r)
            end do
            !$omp end parallel do
            self%status = MATRIX_STATUS%SUCCESS
        case default
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end select
    end subroutine scale_coo

    !>
    !> Sets all stored values in the matrix to zero.
    !>
    module subroutine zero_all_coo(self)
        implicit none
        !> The COO matrix object.
        class(type_matrix_coo), intent(inout) :: self

        self%val = 0.0d0
    end subroutine zero_all_coo

    !>
    !> Returns the 1-based storage index for a specific matrix entry.
    module subroutine zero_row_coo(self, row, row_block)
        implicit none
        !> The COO matrix object.
        class(type_matrix_coo), intent(inout) :: self
        !> The 1-based node index for the row.
        integer(int32), intent(in) :: row
        !> The block row index.
        integer(int32), intent(in), optional :: row_block

        integer(int32) :: i
        do i = 1, self%nnz
            if (self%row(i) == row) then
                self%val(i) = 0.0d0
            end if
        end do

    end subroutine zero_row_coo

    !>
    !> Prints the non-zero entries of the COO matrix to standard output.
    !>
    module subroutine display_coo(self, unit_in)
        implicit none
        !> The COO matrix object to display.
        class(type_matrix_coo), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: i
        integer(int32) :: unit

        unit = optval(unit_in, output_unit)

        write (unit, '(a,i0,a,i0,a,i0,a)') "COO Matrix (max_dims=", self%num_rows, "x", self%num_cols, ", nnz=", self%nnz, ")"
        do i = 1, self%nnz
            write (unit, '(2(i8, ", "), es16.8e3)') self%row(i), self%col(i), self%val(i)
        end do
    end subroutine display_coo

    module subroutine set_local_matrix_coo(self, op, n_local, indices, row_sys, col_sys, local_data)
        implicit none
        class(type_matrix_coo), intent(inout) :: self
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
    end subroutine set_local_matrix_coo

end submodule algebra_matrix_coo

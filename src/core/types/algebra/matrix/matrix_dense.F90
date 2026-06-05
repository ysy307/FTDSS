!>
!> Implements the procedures for the dense matrix type.
!>
submodule(core_types_algebra_matrix) algebra_matrix_dense
    implicit none

contains

    !>
    !> Initializes and allocates a dense matrix.
    !>
    module subroutine initialize_dense(self, num_nodes, row, col, row_blocks, col_blocks)
        implicit none
        class(type_matrix_dense), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in), optional :: row(:)
        integer(int32), intent(in), optional :: col(:)
        integer(int32), intent(in), optional :: row_blocks
        integer(int32), intent(in), optional :: col_blocks

        call deallocate_array(self%val)

        self%num_nodes = num_nodes
        self%num_rows = num_nodes
        self%num_cols = num_nodes

        call allocate_array(self%val, self%num_rows, self%num_cols)
        call self%zero()

        self%is_initialized_matrix = .true.
        self%status = MATRIX_STATUS%SUCCESS
    end subroutine initialize_dense

    !>
    !> Deallocates the dense matrix internal value array.
    !>
    module subroutine destroy_dense(self)
        implicit none
        class(type_matrix_dense), intent(inout) :: self

        call deallocate_array(self%val)
        self%num_nodes = 0
        self%num_rows = 0
        self%num_cols = 0

        self%is_initialized_matrix = .false.
        self%status = MATRIX_STATUS%SUCCESS
    end subroutine destroy_dense

    !>
    !> Returns the number of rows in the matrix.
    !>
    module subroutine get_info_dense(self, info)
        implicit none
        class(type_matrix_dense), intent(in) :: self
        type(type_matrix_info), intent(inout) :: info

        info%num_nodes = self%num_nodes
        info%num_rows = self%num_rows
        info%num_cols = self%num_cols
    end subroutine get_info_dense

    module subroutine get_diagonal_dense(self, diagonal)
        implicit none
        class(type_matrix_dense), intent(in) :: self
        type(type_vector_dp), intent(inout) :: diagonal
        integer(int32) :: i

        if (allocated(self%val)) then
            do i = 1, self%num_rows
                call diagonal%set(VECTOR_OPS%INS, i, self%val(i, i))
            end do
        end if

    end subroutine get_diagonal_dense

    !>
    !> Returns a pointer to the internal 2D array holding the matrix values.
    !>
    module function get_val_dense(self) result(val)
        implicit none
        class(type_matrix_dense), intent(in), target :: self
        real(real64), pointer, dimension(:, :) :: val

        val => self%val
    end function get_val_dense

    !>
    !> Sets the value of a single entry in the matrix.
    !>
    module subroutine set_value_dense(self, op, row, col, value)
        implicit none
        class(type_matrix_dense), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: row, col
        real(real64), intent(in) :: value

        if (.not. MATRIX_OPS%is_valid(op)) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        if (.not. value_in_range(row, 1, self%num_rows) .or. &
            .not. value_in_range(col, 1, self%num_cols)) then
            self%status = MATRIX_STATUS%OUT_OF_MEMORY
            return
        end if

        select case (op%ID)
        case (MATRIX_OPS%INS%ID)
            self%val(row, col) = value
        case (MATRIX_OPS%ADD%ID)
            self%val(row, col) = self%val(row, col) + value
        case default
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end select
    end subroutine set_value_dense

    !> Sets a stored entry by its flat (column-major) position in val(:,:) (no search).
    module subroutine set_value_at_dense(self, op, idx, value)
        implicit none
        class(type_matrix_dense), intent(inout), target :: self
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
    end subroutine set_value_at_dense

    !>
    !> Sets all entries in a specified row to a single scalar value.
    !>
    module subroutine set_row_dense(self, op, row, value, row_block)
        implicit none
        class(type_matrix_dense), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: row
        real(real64), intent(in) :: value
        integer(int32), intent(in), optional :: row_block

        if (.not. MATRIX_OPS%is_valid(op)) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        if (.not. value_in_range(row, 1, self%num_rows)) then
            self%status = MATRIX_STATUS%OUT_OF_MEMORY
            return
        end if

        select case (op%ID)
        case (MATRIX_OPS%INS%ID)
            self%val(row, :) = value
        case (MATRIX_OPS%ADD%ID)
            self%val(row, :) = self%val(row, :) + value
        case default
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end select
    end subroutine set_row_dense

!>
    !> Scales all stored values in the Dense matrix (2D array).
    !>
    module subroutine scale_dense(self, op, alpha)
        implicit none
        class(type_matrix_dense), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        type(type_vector_dp), intent(in) :: alpha

        real(real64), dimension(:), pointer :: alpha_data
        integer(int32) :: i, j
        integer(int32) :: nrows, ncols

        if (.not. MATRIX_OPS%is_valid(op)) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        nrows = self%num_rows
        ncols = self%num_cols

        alpha_data => alpha%get_data()

        if (size(alpha_data) < nrows) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        select case (op%ID)
        case (MATRIX_OPS%SCALE_SYMM_DIAG%ID)
            ! Column-major: outer loop on j gives contiguous inner loop on i
            !$omp parallel do default(shared) private(i, j)
            do j = 1, ncols
                do i = 1, nrows
                    self%val(i, j) = self%val(i, j) * alpha_data(i) * alpha_data(j)
                end do
            end do
            !$omp end parallel do
            self%status = MATRIX_STATUS%SUCCESS
        case (MATRIX_OPS%SCALE_JACOBI%ID)
            !$omp parallel do default(shared) private(i, j)
            do j = 1, ncols
                do i = 1, nrows
                    self%val(i, j) = self%val(i, j) * alpha_data(i)
                end do
            end do
            !$omp end parallel do
            self%status = MATRIX_STATUS%SUCCESS
        case default
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end select
    end subroutine scale_dense

    !>
    !> Sets all entries in the matrix to zero.
    !>
    module subroutine zero_all_dense(self)
        implicit none
        class(type_matrix_dense), intent(inout) :: self
        if (allocated(self%val)) self%val = 0.0d0
        self%status = MATRIX_STATUS%SUCCESS
    end subroutine zero_all_dense

    !> Sets all values in a specified row of the dense matrix to zero.
    !>
    module subroutine zero_row_dense(self, row, row_block)
        implicit none
        class(type_matrix_dense), intent(inout) :: self
        integer(int32), intent(in) :: row
        integer(int32), intent(in), optional :: row_block

        if (.not. value_in_range(row, 1, self%num_rows)) then
            self%status = MATRIX_STATUS%OUT_OF_MEMORY
            return
        end if

        self%val(row, :) = 0.0d0
        self%status = MATRIX_STATUS%SUCCESS
    end subroutine zero_row_dense

    !>
    !> Prints the contents of the dense matrix to standard output.
    !>
    module subroutine display_dense(self, unit_in)
        implicit none
        class(type_matrix_dense), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in
        integer(int32) :: i, j

        integer(int32) :: unit

        unit = optval(unit_in, output_unit)

        if (.not. self%is_initialized()) then
            write (unit, '(a)') "Matrix is not initialized."
            return
        end if

        write (unit, '("Matrix (", i0, "x", i0, "):")') self%num_rows, self%num_cols
        do i = 1, self%num_rows
            write (unit, '(10(es16.8e3))') (self%val(i, j), j=1, self%num_cols)
        end do
    end subroutine display_dense

    module subroutine set_local_matrix_dense(self, op, n_local, indices, row_sys, col_sys, local_data)
        implicit none
        class(type_matrix_dense), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: n_local
        integer(int32), intent(in) :: indices(:, :)
        integer(int32), intent(in) :: row_sys, col_sys
        real(real64), dimension(:, :), intent(in) :: local_data
        self%status = MATRIX_STATUS%NOT_IMPLEMENTED
    end subroutine set_local_matrix_dense

end submodule algebra_matrix_dense

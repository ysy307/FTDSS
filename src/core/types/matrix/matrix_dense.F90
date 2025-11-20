!>
!> Implements the procedures for the dense matrix type.
!>
submodule(core_types_matrix) core_types_matrix_dense
    implicit none

contains

    !>
    !> Initializes and allocates a dense matrix.
    !>
    module subroutine initialize_dense(self, num_nodes, row, col, row_blocks, col_blocks)
        implicit none
        class(type_dense), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in), optional :: row(:)
        integer(int32), intent(in), optional :: col(:)
        integer(int32), intent(in), optional :: row_blocks
        integer(int32), intent(in), optional :: col_blocks

        ! 既存割り当ての解除
        if (allocated(self%val)) call deallocate_array(self%val)
        if (allocated(self%diagonal)) call deallocate_array(self%diagonal)

        self%num_nodes = num_nodes
        self%num_rows = num_nodes
        self%num_cols = num_nodes

        call allocate_array(self%val, self%num_rows, self%num_cols)
        call allocate_array(self%diagonal, self%num_rows)
        call self%zero()

        self%is_initialized_matrix = .true.
        self%status = MATRIX_STATUS_SUCCESS
    end subroutine initialize_dense

    !>
    !> Deallocates the dense matrix's internal value array.
    !>
    module subroutine destroy_dense(self)
        implicit none
        class(type_dense), intent(inout) :: self

        call deallocate_array(self%val)
        call deallocate_array(self%diagonal)
        self%num_nodes = 0
        self%num_rows = 0
        self%num_cols = 0

        self%is_initialized_matrix = .false.
        self%status = MATRIX_STATUS_SUCCESS
    end subroutine destroy_dense

    !>
    !> Returns the number of rows in the matrix.
    !>
    module subroutine get_info_dense(self, info)
        implicit none
        class(type_dense), intent(in) :: self
        type(type_matrix_info), intent(inout) :: info

        info%num_nodes = self%num_nodes
        info%num_rows = self%num_rows
        info%num_cols = self%num_cols
    end subroutine get_info_dense

    module function get_diagonal_dense(self) result(diagonal)
        implicit none
        class(type_dense), intent(inout), target :: self
        real(real64), dimension(:), pointer :: diagonal
        integer(int32) :: i

        if (allocated(self%val)) then
            do i = 1, self%num_rows
                self%diagonal(i) = self%val(i, i)
            end do
        end if
        diagonal => self%diagonal
    end function get_diagonal_dense

    !>
    !> Returns a pointer to the internal 2D array holding the matrix values.
    !>
    module function get_val_dense(self) result(val)
        implicit none
        class(type_dense), intent(in), target :: self
        real(real64), dimension(:, :), pointer :: val
        val => self%val
    end function get_val_dense

    !>
    !> Sets the value of a single entry in the matrix.
    !>
    module subroutine set_value_dense(self, op, row, col, value)
        implicit none
        class(type_dense), intent(inout) :: self
        integer(int32), intent(in) :: op
        integer(int32), intent(in) :: row, col
        real(real64), intent(in) :: value

        ! 範囲チェック
        if (.not. value_in_range(row, 1, self%num_rows) .or. &
            .not. value_in_range(col, 1, self%num_cols)) then
            self%status = MATRIX_STATUS_OUT_OF_MEMORY
            return
        end if

        select case (op)
        case (OP_INS)
            self%val(row, col) = value
        case (OP_ADD)
            self%val(row, col) = self%val(row, col) + value
        case default
            self%status = MATRIX_STATUS_ILL_OPERATIONS
        end select
    end subroutine set_value_dense

    !>
    !> Sets the value of a block (Not implemented for dense yet, usually for BSR).
    !>
    module subroutine set_value_block_dense(self, op, row, col, row_block, col_block, value)
        implicit none
        class(type_dense), intent(inout) :: self
        integer(int32), intent(in) :: op
        integer(int32), intent(in) :: row, col
        integer(int32), intent(in) :: row_block, col_block
        real(real64), intent(in) :: value

        self%status = MATRIX_STATUS_NOT_IMPLEMENTED
    end subroutine set_value_block_dense

    !>
    !> Sets all entries in a specified row to a single scalar value.
    !>
    module subroutine set_row_dense(self, op, row, value, row_block)
        implicit none
        class(type_dense), intent(inout) :: self
        integer(int32), intent(in) :: op
        integer(int32), intent(in) :: row
        real(real64), intent(in) :: value
        integer(int32), intent(in), optional :: row_block

        if (.not. value_in_range(row, 1, self%num_rows)) then
            self%status = MATRIX_STATUS_OUT_OF_MEMORY
            return
        end if

        select case (op)
        case (OP_INS)
            self%val(row, :) = value
        case (OP_ADD)
            self%val(row, :) = self%val(row, :) + value
        case default
            self%status = MATRIX_STATUS_ILL_OPERATIONS
        end select
    end subroutine set_row_dense

    !>
    !> Sets all entries in the matrix to a single scalar value.
    !>
    module subroutine set_all_dense(self, op, value)
        implicit none
        class(type_dense), intent(inout) :: self
        integer(int32), intent(in) :: op
        real(real64), intent(in) :: value

        if (.not. allocated(self%val)) return

        select case (op)
        case (OP_INS)
            self%val = value
        case (OP_ADD)
            self%val = self%val + value
        case default
            self%status = MATRIX_STATUS_ILL_OPERATIONS
        end select
    end subroutine set_all_dense

    !>
    !> Sets all entries in the matrix to zero.
    !>
    module subroutine zero_dense(self)
        implicit none
        class(type_dense), intent(inout) :: self
        if (allocated(self%val)) self%val = 0.0d0
        self%status = MATRIX_STATUS_SUCCESS
    end subroutine zero_dense

    !>
    !> Finds the storage index of a matrix entry.
    !>
    module pure function find_dense(self, row, col) result(index)
        implicit none
        class(type_dense), intent(in) :: self
        integer(int32), intent(in) :: row, col
        integer(int32) :: index

        if (.not. value_in_range(row, 1, self%num_rows) .or. &
            .not. value_in_range(col, 1, self%num_cols)) then
            index = -1 ! Not found / Invalid

        else
            ! Column-major index for 2D array
            index = (col - 1) * self%num_rows + row
        end if
    end function find_dense

    !>
    !> Prints the contents of the dense matrix to standard output.
    !>
    module subroutine display_dense(self)
        implicit none
        class(type_dense), intent(in) :: self
        integer(int32) :: i, j

        if (.not. self%is_initialized()) then
            print *, "Matrix is not initialized."
            return
        end if

        write (*, '("Matrix (", i0, "x", i0, "):")') self%num_rows, self%num_cols
        do i = 1, self%num_rows
            write (*, '(10(es12.4e2))') (self%val(i, j), j=1, self%num_cols)
        end do
    end subroutine display_dense

end submodule core_types_matrix_dense

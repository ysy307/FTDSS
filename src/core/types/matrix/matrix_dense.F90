!>
!> Implements the procedures for the dense matrix type.
!>
submodule(core_types_matrix) core_types_matrix_dense
    implicit none

contains

    !>
    !> Initializes and allocates a dense matrix.
    !> The dimensions are determined by the total number of degrees of freedom
    !> (num_nodes * num_blocks). The sparsity pattern arguments are ignored.
    !>
    module subroutine initialize_dense(self, num_nodes, row, col, row_blocks, col_blocks)
        implicit none
        !> The dense matrix object to initialize.
        class(type_dense), intent(inout) :: self
        !> The number of nodes.
        integer(int32), intent(in) :: num_nodes
        !> Ignored for dense matrices, present for API compatibility.
        integer(int32), intent(in), optional :: row(:)
        !> Ignored for dense matrices, present for API compatibility.
        integer(int32), intent(in), optional :: col(:)
        !> Ignored for dense matrices, present for API compatibility.
        integer(int32), intent(in), optional :: row_blocks
        !> Ignored for dense matrices, present for API compatibility.
        integer(int32), intent(in), optional :: col_blocks

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
        !> The dense matrix object to destroy.
        class(type_dense), intent(inout) :: self

        call deallocate_array(self%val)
        call deallocate_array(self%diagonal)
        self%num_nodes = 0
        self%num_rows = 0
        self%num_cols = 0

        self%is_initialized_matrix = .false.
    end subroutine destroy_dense

    !>
    !> Returns the number of rows in the matrix.
    !>
    module subroutine get_info_dense(self, info)
        implicit none
        !> The matrix object.
        class(type_dense), intent(in) :: self
        !> The matrix information structure to populate.
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

        self%diagonal(:) = 0.0d0
        do i = 1, self%num_rows
            self%diagonal(i) = self%val(i, i)
        end do

        diagonal => self%diagonal

    end function get_diagonal_dense

    !>
    !> Returns a pointer to the internal 2D array holding the matrix values.
    !>
    module function get_val_dense(self) result(val)
        implicit none
        !> The dense matrix object.
        class(type_dense), intent(in), target :: self
        !> A pointer to the matrix's value array.
        real(real64), dimension(:, :), pointer :: val

        val => self%val
    end function get_val_dense

    !>
    !> Sets the value of a single entry in the matrix.
    module subroutine set_value_dense(self, op, row, col, value)
        implicit none
        !> The dense matrix object.
        class(type_dense), intent(inout) :: self
        !> The operation to perform
        integer(int32), intent(in) :: op
        !> The 1-based node index for the row.
        integer(int32), intent(in) :: row
        !> The 1-based node index for the column.
        integer(int32), intent(in) :: col
        !> The value to set at the specified entry.
        real(real64), intent(in) :: value

        select case (op)
        case (MATRIX_OP_INS)
            self%val(row, col) = value
        case (MATRIX_OP_ADD)
            self%val(row, col) = self%val(row, col) + value
        case default
            self%status = MATRIX_STATUS_ILL_OPERATIONS
        end select
    end subroutine set_value_dense

    !>
    !> Sets the value of a single entry in the matrix.
    module subroutine set_value_block_dense(self, op, row, col, row_block, col_block, value)
        implicit none
        !> The dense matrix object.
        class(type_dense), intent(inout) :: self
        !> The operation to perform
        integer(int32), intent(in) :: op
        !> The 1-based node index for the row.
        integer(int32), intent(in) :: row
        !> The 1-based node index for the column.
        integer(int32), intent(in) :: col
        !> The 1-based block row index.
        integer(int32), intent(in) :: row_block
        !> The 1-based block column index.
        integer(int32), intent(in) :: col_block
        !> The value to set at the specified entry.
        real(real64), intent(in) :: value

        self%status = MATRIX_STATUS_NOT_IMPLEMENTED
    end subroutine set_value_block_dense

    !>
    !> Sets all entries in a specified row to a single scalar value.
    !>
    module subroutine set_row_dense(self, op, row, value, row_block)
        implicit none
        !> The dense matrix object.
        class(type_dense), intent(inout) :: self
        !> The operation to perform
        integer(int32), intent(in) :: op
        !> The 1-based node index for the row.
        integer(int32), intent(in) :: row
        !> The scalar value to assign.
        real(real64), intent(in) :: value
        !> The block row index.
        integer(int32), intent(in), optional :: row_block

        select case (op)
        case (MATRIX_OP_INS)
            self%val(row, :) = value
        case (MATRIX_OP_ADD)
            self%val(row, :) = self%val(row, :) + value
        case default
            self%status = MATRIX_STATUS_ILL_OPERATIONS
        end select

    end subroutine set_row_dense

    !>
    !> Sets all entries in the matrix to a single scalar value.
    !>
    module subroutine set_all_dense(self, op, value)
        !> The dense matrix object.
        class(type_dense), intent(inout) :: self
        !> The operation to perform
        integer(int32), intent(in) :: op
        !> The scalar value to assign.
        real(real64), intent(in) :: value

        select case (op)
        case (MATRIX_OP_INS)
            self%val = value
        case (MATRIX_OP_ADD)
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
        !> The dense matrix object.
        class(type_dense), intent(inout) :: self

        self%val = 0.0d0
    end subroutine zero_dense

    !> Finds the storage index of a matrix entry.
    module pure function find_dense(self, row, col) result(index)
        implicit none
        class(type_dense), intent(in) :: self
        integer(int32), intent(in) :: row, col
        integer(int32) :: index

        index = (col - 1) * self%num_rows + row
    end function find_dense

!     !>
!     !> Adds a value to a single entry in the matrix.
!     !>
!     module subroutine add_value_dense(self, row, col, value)
!         implicit none
!         !> The dense matrix object.
!         class(type_dense), intent(inout) :: self
!         !> The 1-based node index for the row.
!         integer(int32), intent(in) :: row
!         !> The 1-based node index for the column.
!         integer(int32), intent(in) :: col
!         !> The value to add to the specified entry.
!         real(real64), intent(in) :: value

!         self%val(row, col) = self%val(row, col) + value
!     end subroutine add_value_dense

!     module subroutine add_values_dense(self, indices, values)
!         implicit none
!         class(type_dense), intent(inout) :: self
!         integer(int32), intent(in) :: indices(:)
!         class(abst_matrix), intent(in) :: values

!         integer(int32) :: i, j, n

!         select type (matrix => values)
!         type is (type_dense)
!             n = size(indices)
!             do i = 1, n
!                 do j = 1, n
!                     call self%add(indices(i), indices(j), matrix%val(i, j))
!                 end do
!             end do
!         end select
!     end subroutine add_values_dense

!     !>
!     !> Performs the matrix operation C = alpha*A + B, where A is `self`.
!     !>
!     module subroutine add_matrix_dense(self, alpha, B, C)
!         implicit none
!         !> The dense matrix object (A).
!         class(type_dense), intent(in) :: self
!         !> The scalar multiplier alpha.
!         real(real64), intent(in) :: alpha
!         !> The abstract matrix B (must be of type_dense).
!         class(abst_matrix), intent(in) :: B
!         !> The abstract matrix C to store the result (must be of type_dense).
!         class(abst_matrix), intent(inout) :: C

!         select type (B_dense => B)
!         type is (type_dense)
!             select type (C_dense => C)
!             type is (type_dense)
! #ifdef USE_DEBUG
!                 ! Check for matching dimensions
!                 if (any([self%num_row, self%num_col] /= [B_dense%num_row, B_dense%num_col]) .or. &
!                     any([self%num_row, self%num_col] /= [C_dense%num_row, C_dense%num_col])) then
!                     print *, "ERROR(add_matrix_dense): Matrix dimensions do not match."
!                     stop
!                 end if
! #endif
!                 C_dense%val = alpha * self%val + B_dense%val
!             end select
!         end select
!     end subroutine add_matrix_dense

!     !>
!     !> Performs a general matrix-vector multiplication: y = alpha*A*x + beta*y.
!     !> This implementation may use MKL's dgemv if available.
!     !>
!     module subroutine gemv_dense(self, alpha, x, beta, y)
!         implicit none
!         !> The dense matrix object (A).
!         class(type_dense), intent(in) :: self
!         !> The scalar multiplier alpha.
!         real(real64), intent(in) :: alpha
!         !> The input vector x.
!         real(real64), intent(in) :: x(:)
!         !> The scalar multiplier beta.
!         real(real64), intent(in) :: beta
!         !> The input/output vector y.
!         real(real64), intent(inout) :: y(:)

! #ifdef _MKL
!         interface
!             subroutine dgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
!                 use, intrinsic :: iso_fortran_env
!                 implicit none
!                 character(len=1), intent(in) :: trans
!                 integer, intent(in) :: m, n, lda, incx, incy
!                 real(real64), intent(in) :: alpha, beta
!                 real(real64), intent(in) :: a(lda, *), x(*), y(*)
!             end subroutine dgemv
!         end interface

!         call dgemv('N', self%num_row, self%num_col, alpha, self%val, self%num_row, x, 1, beta, y, 1)
! #else
!         integer(int32) :: i

!         !$omp parallel do private(i)
!         do i = 1, self%num_row
!             y(i) = alpha * dot_product(self%val(i, :), x) + beta * y(i)
!         end do
!         !$omp end parallel do
! #endif

!     end subroutine gemv_dense

    !>
    !> Prints the contents of the dense matrix to standard output.
    !>
    module subroutine display_dense(self)
        implicit none
        !> The dense matrix object to display.
        class(type_dense), intent(in) :: self
        integer(int32) :: i

        if (.not. self%is_initialized()) then
            print *, "Matrix is not initialized."
            return
        end if

        write (*, '("Matrix (", i0, "x", i0, "):")') self%num_rows, self%num_cols
        do i = 1, self%num_rows
            write (*, '(10(es12.4e2))') self%val(i, :)
        end do
    end subroutine display_dense

end submodule core_types_matrix_dense

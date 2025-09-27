!>
!> Implements the procedures for the dense matrix type.
!>
submodule(core_types_matrix) core_types_matrix_dense
    implicit none

contains

    !>
    !> Initializes and allocates a dense matrix.
    !> The dimensions are determined by the total number of degrees of freedom
    !> (num_nodes * num_dofs). The sparsity pattern arguments are ignored.
    !>
    module subroutine initialize_dense(self, num_nodes, num_dofs, row, col)
        implicit none
        !> The dense matrix object to initialize.
        class(type_dense), intent(inout) :: self
        !> The number of nodes.
        integer(int32), intent(in) :: num_nodes
        !> The number of DOFs per node.
        integer(int32), intent(in) :: num_dofs
        !> Ignored for dense matrices, present for API compatibility.
        integer(int32), intent(in), optional :: row(:)
        !> Ignored for dense matrices, present for API compatibility.
        integer(int32), intent(in), optional :: col(:)

        self%num_nodes = num_nodes
        self%num_dofs = num_dofs

        self%num_row = num_nodes * num_dofs
        self%num_col = num_nodes * num_dofs
        call allocate_array(self%val, self%num_row, self%num_col)
        self%val = 0.0d0
    end subroutine initialize_dense

    !>
    !> Deallocates the dense matrix's internal value array.
    !>
    module subroutine destroy_dense(self)
        implicit none
        !> The dense matrix object to destroy.
        class(type_dense), intent(inout) :: self

        call deallocate_array(self%val)
        self%num_row = 0
        self%num_col = 0
    end subroutine destroy_dense

    !>
    !> Returns the number of rows in the matrix.
    !>
    module pure function get_num_row_dense(self) result(num_row)
        implicit none
        !> The dense matrix object.
        class(type_dense), intent(in) :: self
        !> The number of rows.
        integer(int32) :: num_row

        num_row = self%num_row
    end function get_num_row_dense

    !>
    !> Returns the number of columns in the matrix.
    !>
    module pure function get_num_col_dense(self) result(num_col)
        implicit none
        !> The dense matrix object.
        class(type_dense), intent(in) :: self
        !> The number of columns.
        integer(int32) :: num_col

        num_col = self%num_col
    end function get_num_col_dense

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
    !>
    module subroutine set_value_dense(self, row_dof, col_dof, row, col, value)
        implicit none
        !> The dense matrix object.
        class(type_dense), intent(inout) :: self
        !> The 1-based DOF index within the row/column node.
        integer(int32), intent(in) :: row_dof, col_dof
        !> The 1-based node index for the row/column.
        integer(int32), intent(in) :: row, col
        !> The value to set at the specified entry.
        real(real64), intent(in) :: value

        integer(int32) :: actual_row, actual_col

        actual_row = (row - 1) * self%num_dofs + row_dof
        actual_col = (col - 1) * self%num_dofs + col_dof

        self%val(actual_row, actual_col) = value
    end subroutine set_value_dense

    !>
    !> Sets all entries in a specified row to a single scalar value.
    !>
    module subroutine set_row_dense(self, row_dof, row, value)
        implicit none
        !> The dense matrix object.
        class(type_dense), intent(inout) :: self
        !> The 1-based DOF index within the row node.
        integer(int32), intent(in) :: row_dof
        !> The 1-based node index for the row.
        integer(int32), intent(in) :: row
        !> The scalar value to assign.
        real(real64), intent(in) :: value

        integer(int32) :: actual_row

        actual_row = (row_dof - 1) * self%num_nodes + row

        self%val(actual_row, :) = value

    end subroutine set_row_dense

    !>
    !> Sets all entries in the matrix to a single scalar value.
    !>
    module subroutine set_all_dense(self, value)
        !> The dense matrix object.
        class(type_dense), intent(inout) :: self
        !> The scalar value to assign.
        real(real64), intent(in) :: value

        self%val = value
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

    !>
    !> Adds a value to a single entry in the matrix.
    !>
    module subroutine add_value_dense(self, row_dof, col_dof, row, col, value)
        implicit none
        !> The dense matrix object.
        class(type_dense), intent(inout) :: self
        !> The 1-based DOF index within the row/column node.
        integer(int32), intent(in) :: row_dof, col_dof
        !> The 1-based node index for the row/column.
        integer(int32), intent(in) :: row, col
        !> The value to add to the specified entry.
        real(real64), intent(in) :: value

        integer(int32) :: actual_row, actual_col

        actual_row = (row_dof - 1) * self%num_nodes + row
        actual_col = (col_dof - 1) * self%num_nodes + col

        self%val(actual_row, actual_col) = self%val(actual_row, actual_col) + value
    end subroutine add_value_dense

    !>
    !> Performs the matrix operation C = alpha*A + B, where A is `self`.
    !>
    module subroutine add_matrix_dense(self, alpha, B, C)
        implicit none
        !> The dense matrix object (A).
        class(type_dense), intent(in) :: self
        !> The scalar multiplier alpha.
        real(real64), intent(in) :: alpha
        !> The abstract matrix B (must be of type_dense).
        class(abst_matrix), intent(in) :: B
        !> The abstract matrix C to store the result (must be of type_dense).
        class(abst_matrix), intent(inout) :: C

        select type (B_dense => B)
        type is (type_dense)
            select type (C_dense => C)
            type is (type_dense)
#ifdef USE_DEBUG
                ! Check for matching dimensions
                if (any([self%num_row, self%num_col] /= [B_dense%num_row, B_dense%num_col]) .or. &
                    any([self%num_row, self%num_col] /= [C_dense%num_row, C_dense%num_col])) then
                    print *, "ERROR(add_matrix_dense): Matrix dimensions do not match."
                    stop
                end if
#endif
                C_dense%val = alpha * self%val + B_dense%val
            end select
        end select
    end subroutine add_matrix_dense

    !>
    !> Performs a general matrix-vector multiplication: y = alpha*A*x + beta*y.
    !> This implementation may use MKL's dgemv if available.
    !>
    module subroutine gemv_dense(self, alpha, x, beta, y)
        implicit none
        !> The dense matrix object (A).
        class(type_dense), intent(in) :: self
        !> The scalar multiplier alpha.
        real(real64), intent(in) :: alpha
        !> The input vector x.
        real(real64), intent(in) :: x(:)
        !> The scalar multiplier beta.
        real(real64), intent(in) :: beta
        !> The input/output vector y.
        real(real64), intent(inout) :: y(:)

#ifdef _MKL
        interface
            subroutine dgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
                use, intrinsic :: iso_fortran_env
                implicit none
                character(len=1), intent(in) :: trans
                integer, intent(in) :: m, n, lda, incx, incy
                real(real64), intent(in) :: alpha, beta
                real(real64), intent(in) :: a(lda, *), x(*), y(*)
            end subroutine dgemv
        end interface

        call dgemv('N', self%num_row, self%num_col, alpha, self%val, self%num_row, x, 1, beta, y, 1)
#else
        integer(int32) :: i

        !$omp parallel do private(i)
        do i = 1, self%num_row
            y(i) = alpha * dot_product(self%val(i, :), x) + beta * y(i)
        end do
        !$omp end parallel do
#endif

    end subroutine gemv_dense

    !>
    !> Prints the contents of the dense matrix to standard output.
    !>
    module subroutine display_dense(self)
        implicit none
        !> The dense matrix object to display.
        class(type_dense), intent(in) :: self
        integer :: i
        if (.not. allocated(self%val)) then
            print *, "Matrix is not allocated."
            return
        end if
        print '("Matrix (", i0, "x", i0, "):")', self%num_row, self%num_col
        do i = 1, self%num_row
            write (*, '(10(es12.4e2))') self%val(i, :)
        end do
    end subroutine display_dense

end submodule core_types_matrix_dense

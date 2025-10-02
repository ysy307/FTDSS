!>
!> Implements the procedures for the Coordinate (COO) sparse matrix type.
!>
submodule(core_types_matrix) core_types_matrix_coo
    implicit none

contains

    !>
    !> Initializes the DOF-level COO matrix structure from a node-level sparsity pattern.
    !> It assumes the input node-level `row` and `col` arrays are sorted appropriately
    !> to produce a sorted DOF-level COO matrix without requiring an explicit sort.
    !>
    module subroutine initialize_type_coo(self, num_nodes, row, col)
        implicit none
        !> The COO matrix object to initialize.
        class(type_coo), intent(inout) :: self
        !> The total number of nodes in the mesh.
        integer(int32), intent(in) :: num_nodes
        !> The node-level row indices defining the sparsity pattern.
        integer(int32), intent(in), optional :: row(:)
        !> The node-level column indices defining the sparsity pattern.
        integer(int32), intent(in), optional :: col(:)

        integer(int32) :: nloc

        if (.not. present(row) .or. .not. present(col)) then
            print *, "Error: row and col must be provided for COO initialization."
            stop
        end if

        if (size(row) /= size(col)) then
            print *, "Error: row and col arrays must be the same length."
            stop
        end if
        nloc = size(row)

        ! Calculate final matrix dimensions
        self%num_nodes = num_nodes
        self%num_row = num_nodes
        self%num_col = num_nodes
        self%nnz = nloc

        ! Allocate memory for COO arrays
        call allocate_array(self%row, source=row)
        call allocate_array(self%col, source=col)
        call allocate_array(self%val, self%nnz)

        ! Initialize values to zero
        self%val = 0.0d0

    end subroutine initialize_type_coo

    !>
    !> Deallocates all internal arrays of the COO matrix object.
    !>
    module subroutine destroy_coo(self)
        implicit none
        !> The COO matrix object to destroy.
        class(type_coo), intent(inout) :: self

        call deallocate_array(self%row)
        call deallocate_array(self%col)
        call deallocate_array(self%val)
        self%num_nodes = 0
        self%num_row = 0
        self%num_col = 0
        self%nnz = 0
    end subroutine destroy_coo

    !>
    !> Returns the number of non-zero entries in the matrix.
    !>
    module pure function get_nnz_coo(self) result(nnz)
        implicit none
        !> The COO matrix object.
        class(type_coo), intent(in) :: self
        !> The number of non-zero entries.
        integer(int32) :: nnz
        nnz = self%nnz
    end function get_nnz_coo

    !>
    !> Returns the number of rows in the matrix.
    !>
    module pure function get_num_row_coo(self) result(num_row)
        implicit none
        !> The COO matrix object.
        class(type_coo), intent(in) :: self
        !> The number of rows.
        integer(int32) :: num_row
        num_row = self%num_row
    end function get_num_row_coo

    !>
    !> Returns the number of columns in the matrix.
    !>
    module pure function get_num_col_coo(self) result(num_col)
        implicit none
        !> The COO matrix object.
        class(type_coo), intent(in) :: self
        !> The number of columns.
        integer(int32) :: num_col
        num_col = self%num_col
    end function get_num_col_coo

    !>
    !> Returns a pointer to the internal `row` index array.
    !>
    module function get_row_coo(self) result(row)
        implicit none
        !> The COO matrix object.
        class(type_coo), intent(in), target :: self
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
        class(type_coo), intent(in), target :: self
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
        class(type_coo), intent(in), target :: self
        !> A pointer to the `val` array.
        real(real64), dimension(:), pointer :: val

        val => self%val
    end function get_val_coo

    !>
    !> Sets the value of a single entry in the matrix.
    !>
    module subroutine set_value_coo(self, row, col, value)
        implicit none
        !> The COO matrix object.
        class(type_coo), intent(inout) :: self
        !> The 1-based node index for the row.
        integer(int32), intent(in) :: row
        !> The 1-based node index for the column.
        integer(int32), intent(in) :: col
        !> The value to set at the specified entry.
        real(real64), intent(in) :: value

        integer(int32) :: index

        index = self%find(row, col)
#ifdef USE_DEBUG
        if (index > 0) then
#endif
            self%val(index) = value
#ifdef USE_DEBUG
        else
            print *, "Warning(set_value_coo): Element not in sparsity pattern.", row, col
        end if
#endif
    end subroutine set_value_coo

    !>
    !> Sets all non-zero entries in a specified row to a single scalar value.
    !> Note: This is an inefficient O(nnz) operation for the COO format.
    !>
    module subroutine set_row_coo(self, row, value)
        implicit none
        !> The COO matrix object.
        class(type_coo), intent(inout) :: self
        !> The 1-based node index for the row.
        integer(int32), intent(in) :: row
        !> The scalar value to assign.
        real(real64), intent(in) :: value

        integer(int32) :: i

        do i = 1, self%nnz
            if (self%row(i) == row) then
                self%val(i) = value
            end if
        end do
    end subroutine set_row_coo

    !>
    !> Sets all stored non-zero values in the matrix to a single scalar value.
    !>
    module subroutine set_all_coo(self, value)
        implicit none
        !> The COO matrix object.
        class(type_coo), intent(inout) :: self
        !> The scalar value to assign to all non-zero entries.
        real(real64), intent(in) :: value

        self%val = value
    end subroutine set_all_coo

    !>
    !> Sets all stored values in the matrix to zero.
    !>
    module subroutine zero_coo(self)
        implicit none
        !> The COO matrix object.
        class(type_coo), intent(inout) :: self

        self%val = 0.0d0
    end subroutine zero_coo

    !>
    !> Adds a value to a single entry in the matrix.
    !>
    module subroutine add_value_coo(self, row, col, value)
        implicit none
        !> The COO matrix object.
        class(type_coo), intent(inout) :: self
        !> The 1-based node index for the row.
        integer(int32), intent(in) :: row
        !> The 1-based node index for the column.
        integer(int32), intent(in) :: col
        !> The value to add to the specified entry.
        real(real64), intent(in) :: value

        integer(int32) :: index

        index = self%find(row, col)

#ifdef USE_DEBUG
        if (index > 0) then
#endif
            self%val(index) = self%val(index) + value
#ifdef USE_DEBUG
        else
            print *, "Warning(add_value_coo): Element not in sparsity pattern.", row, col
        end if
#endif
    end subroutine add_value_coo

    module subroutine add_values_coo(self, indices, values)
        implicit none
        class(type_coo), intent(inout) :: self
        integer(int32), intent(in) :: indices(:)
        class(abst_matrix), intent(in) :: values

        integer(int32) :: n, i, j
        class(type_dense), pointer :: matrix

        n = size(indices)
        select type (matrix => values)
        type is (type_dense)
            do i = 1, n
                do j = 1, n
                    call self%add(indices(i), indices(j), matrix%val(i, j))
                end do
            end do
        end select
    end subroutine add_values_coo

    !>
    !> Performs the matrix operation C = alpha*A + B, where A is `self`.
    !> This simplified version requires all matrices to have identical sparsity patterns.
    !>
    module subroutine add_matrix_coo(self, alpha, B, C)
        implicit none
        !> The COO matrix object (A).
        class(type_coo), intent(in) :: self
        !> The scalar multiplier alpha.
        real(real64), intent(in) :: alpha
        !> The abstract matrix B (must be of type_coo).
        class(abst_matrix), intent(in) :: B
        !> The abstract matrix C to store the result (must be of type_coo).
        class(abst_matrix), intent(inout) :: C

        select type (B_coo => B)
        type is (type_coo)
            select type (C_coo => C)
            type is (type_coo)
                if (self%nnz /= B_coo%nnz .or. self%nnz /= C_coo%nnz) then
                    print *, "ERROR(add_matrix_coo): In this simplified version, NNZ must be identical."
                    stop
                end if
                C_coo%val = alpha * self%val + B_coo%val
            end select
        end select
    end subroutine add_matrix_coo

    !>
    !> Performs a general matrix-vector multiplication: y = alpha*A*x + beta*y.
    !> This implementation is parallelized with OpenMP atomics to handle potential
    !> race conditions when multiple threads write to the same element of `y`.
    !>
    module subroutine gemv_coo(self, alpha, x, beta, y)
        implicit none
        !> The COO matrix object (A).
        class(type_coo), intent(in) :: self
        !> The scalar multiplier alpha.
        real(real64), intent(in) :: alpha
        !> The input vector x.
        real(real64), intent(in) :: x(:)
        !> The scalar multiplier beta.
        real(real64), intent(in) :: beta
        !> The input/output vector y.
        real(real64), intent(inout) :: y(:)
        integer(int32) :: i

        ! First, scale the entire y vector by beta to avoid repeated multiplications inside the loop.
        if (beta == 0.0d0) then
            y = 0.0d0
        else
            y = beta * y
        end if

        ! Add the contribution of each non-zero element.
        ! Atomic updates are required as multiple non-zero entries may share the same row.
        !$omp parallel do
        do i = 1, self%nnz
            !$omp atomic update
            y(self%row(i)) = y(self%row(i)) + alpha * self%val(i) * x(self%col(i))
        end do
        !$omp end parallel do

    end subroutine gemv_coo

    !>
    !> Finds the 1-based storage index for a specific matrix entry using a linear search.
    !> Note: This is an O(nnz) operation and can be a significant bottleneck for
    !> frequent access on large matrices. The CRS format is recommended for such cases.
    !>
    module pure function find_coo(self, row, col) result(index)
        implicit none
        !> The COO matrix object.
        class(type_coo), intent(in) :: self
        !> The 1-based node index for the row.
        integer(int32), intent(in) :: row
        !> The 1-based node index for the column.
        integer(int32), intent(in) :: col
        !> The 1-based index in the `val`/`ind`/`col` arrays, or 0 if not found.
        integer(int32) :: index

        integer(int32) :: i

        index = 0
        ! Linear search through all non-zero entries.
        do i = 1, self%nnz
            if (self%row(i) == row .and. self%col(i) == col) then
                index = i
                return
            end if
        end do
    end function find_coo

    !>
    !> Prints the non-zero entries of the COO matrix to standard output.
    !>
    module subroutine display_coo(self)
        implicit none
        !> The COO matrix object to display.
        class(type_coo), intent(in) :: self

        integer(int32) :: i

        print *, "COO Matrix (max_dims=", self%num_row, "x", self%num_col, ", nnz=", self%nnz, ")"
        do i = 1, self%nnz
            write (*, '(2(i8, ", "), es16.8)') self%row(i), self%col(i), self%val(i)
        end do
    end subroutine display_coo

end submodule core_types_matrix_coo

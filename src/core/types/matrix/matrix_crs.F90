!>
!> Implements the procedures for a Degree-of-Freedom (DOF) based Compressed
!> Row Storage (CRS) sparse matrix.
!>
submodule(core_types_matrix) core_types_matrix_crs
    implicit none

contains

    !>
    !> Initializes the DOF-level CRS matrix structure from a node-level adjacency pattern.
    !> It assumes the input node-level column indices (`col`) are sorted for each row segment.
    !> This routine expands the node-level graph into a full DOF-level matrix sparsity pattern.
    !>
    module subroutine initialize_type_crs(self, num_nodes, row, col)
        implicit none
        !> The CRS matrix object to initialize.
        class(type_crs), intent(inout) :: self
        !> The total number of nodes in the mesh.
        integer(int32), intent(in) :: num_nodes
        !> The node-level CSR pointer array (`ptr`), of size num_nodes + 1.
        integer(int32), intent(in), optional :: row(:)
        !> The node-level CSR column index array (`ind`).
        integer(int32), intent(in), optional :: col(:)

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
        self%num_row = num_nodes
        self%num_ptr = num_nodes + 1
        self%nnz = size(col)

        ! Allocate arrays
        call allocate_array(self%ptr, source=row)
        call allocate_array(self%ind, source=col)
        call allocate_array(self%val, self%nnz)

        ! Initialize value array to zero
        self%val = 0.0d0
    end subroutine initialize_type_crs

    !>
    !> Deallocates all internal arrays of the CRS matrix object.
    !>
    module subroutine destroy_crs(self)
        implicit none
        !> The CRS matrix object to destroy.
        class(type_crs), intent(inout) :: self

        call deallocate_array(self%ptr)
        call deallocate_array(self%ind)
        call deallocate_array(self%val)
        self%nnz = 0
        self%num_row = 0
    end subroutine destroy_crs

    !>
    !> Returns the number of non-zero entries in the matrix.
    !>
    module pure function get_nnz_crs(self) result(nnz)
        implicit none
        !> The CRS matrix object.
        class(type_crs), intent(in) :: self
        !> The number of non-zero entries.
        integer(int32) :: nnz

        nnz = self%nnz
    end function get_nnz_crs

    !>
    !> Returns the size of the pointer array (`num_row + 1`).
    !>
    module pure function get_num_ptr_crs(self) result(num_ptr)
        implicit none
        !> The CRS matrix object.
        class(type_crs), intent(in) :: self
        !> The size of the pointer array.
        integer(int32) :: num_ptr

        num_ptr = self%num_ptr
    end function get_num_ptr_crs

    !>
    !> Returns the number of rows in the matrix.
    !>
    module pure function get_num_row_crs(self) result(num_row)
        implicit none
        !> The CRS matrix object.
        class(type_crs), intent(in) :: self
        !> The number of rows.
        integer(int32) :: num_row

        num_row = self%num_row
    end function get_num_row_crs

    !>
    !> Returns a pointer to the internal CSR pointer array (`ptr`).
    !>
    module function get_ptr_crs(self) result(ptr)
        implicit none
        !> The CRS matrix object.
        class(type_crs), intent(in), target :: self
        !> A pointer to the CSR `ptr` array.
        integer(int32), dimension(:), pointer :: ptr

        ptr => self%ptr
    end function get_ptr_crs

    !>
    !> Returns a pointer to the internal CSR column index array (`ind`).
    !>
    module function get_ind_crs(self) result(ind)
        implicit none
        !> The CRS matrix object.
        class(type_crs), intent(in), target :: self
        !> A pointer to the CSR `ind` array.
        integer(int32), dimension(:), pointer :: ind

        ind => self%ind
    end function get_ind_crs

    !>
    !> Returns a pointer to the internal CSR value array (`val`).
    !>
    module function get_val_crs(self) result(val)
        implicit none
        !> The CRS matrix object.
        class(type_crs), intent(in), target :: self
        !> A pointer to the CSR `val` array.
        real(real64), dimension(:), pointer :: val

        val => self%val
    end function get_val_crs

    !>
    !> Sets the value of a specific entry in the sparse matrix.
    !>
    module subroutine set_value_crs(self, row, col, value)
        implicit none
        !> The CRS matrix object.
        class(type_crs), intent(inout) :: self
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
            print *, "Warning(set_value_crs): Element not in sparsity pattern.", row, col
        end if
#endif
    end subroutine set_value_crs

    !>
    !> Sets all non-zero entries in a specific row to a single scalar value.
    !>
    module subroutine set_row_crs(self, row, value)
        implicit none
        !> The CRS matrix object.
        class(type_crs), intent(inout) :: self
        !> The 1-based node index for the row.
        integer(int32), intent(in) :: row
        !> The scalar value to assign.
        real(real64), intent(in) :: value

        integer(int32) :: is, ie

        is = self%ptr(row)
        ie = self%ptr(row + 1) - 1

        self%val(is:ie) = value
    end subroutine set_row_crs

    !>
    !> Sets all stored non-zero values in the matrix to a single scalar value.
    !>
    module subroutine set_all_crs(self, value)
        implicit none
        !> The CRS matrix object.
        class(type_crs), intent(inout) :: self
        !> The scalar value to assign to all non-zero entries.
        real(real64), intent(in) :: value

        self%val = value
    end subroutine set_all_crs

    !>
    !> Sets all stored values in the matrix to zero.
    !>
    module subroutine zero_crs(self)
        implicit none
        !> The CRS matrix object.
        class(type_crs), intent(inout) :: self

        self%val = 0.0d0
    end subroutine zero_crs

    !>
    !> Adds a value to a specific entry in the sparse matrix.
    !>
    module subroutine add_value_crs(self, row, col, value)
        implicit none
        !> The CRS matrix object.
        class(type_crs), intent(inout) :: self
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
            print *, "Warning(add_value_crs): Element not in sparsity pattern.", row, col
        end if
#endif
    end subroutine add_value_crs

    !>
    !> Adds the values from another CRS matrix to this matrix.
    !> This simplified version requires both matrices to have identical sparsity patterns.
    module subroutine add_values_crs(self, indices, values)
        implicit none
        !> The CRS matrix object to modify (self).
        class(type_crs), intent(inout) :: self
        !> The 1-based node indices specifying which rows and columns to update.
        integer(int32), intent(in) :: indices(:)
        !> The abstract matrix containing values to add (must be of type_crs).
        class(abst_matrix), intent(in) :: values

        integer(int32) :: i, j, n

        select type (matrix => values)
        type is (type_dense)
            n = size(indices)
            do i = 1, n
                do j = 1, n
                    call self%add(indices(i), indices(j), matrix%val(i, j))
                end do
            end do
        end select
    end subroutine add_values_crs
    !>
    !> Performs the matrix operation \( C = \alpha*A + B \), where A is self.
    !> This simplified version requires all matrices to have identical sparsity patterns.
    !>
    module subroutine add_matrix_crs(self, alpha, B, C)
        implicit none
        !> The CRS matrix object (A).
        class(type_crs), intent(in) :: self
        !> The scalar multiplier alpha.
        real(real64), intent(in) :: alpha
        !> The abstract matrix B (must be of type_crs).
        class(abst_matrix), intent(in) :: B
        !> The abstract matrix C to store the result (must be of type_crs).
        class(abst_matrix), intent(inout) :: C

        select type (B_crs => B)
        type is (type_crs)
            select type (C_crs => C)
            type is (type_crs)
                if (self%nnz /= B_crs%nnz .or. self%nnz /= C_crs%nnz) then
                    print *, "ERROR(add_matrix_crs): In this simplified version, NNZ must be identical."
                    stop
                end if
                C_crs%val = alpha * self%val + B_crs%val
            end select
        end select
    end subroutine add_matrix_crs

    !>
    !> Performs a sparse matrix-vector multiplication (GEMV): y = alpha*A*x + beta*y.
    !>
    module subroutine gemv_crs(self, alpha, x, beta, y)
        implicit none
        !> The CRS matrix object (A).
        class(type_crs), intent(in) :: self
        !> The scalar multiplier alpha.
        real(real64), intent(in) :: alpha
        !> The input vector x.
        real(real64), intent(in) :: x(:)
        !> The scalar multiplier beta.
        real(real64), intent(in) :: beta
        !> The input/output vector y.
        real(real64), intent(inout) :: y(:)

        integer(int32) :: i, j, is, ie
        real(real64) :: sum

        !$omp parallel do private(i, j, is, ie, sum)
        do i = 1, self%num_row
            sum = 0.0d0
            is = self%ptr(i)
            ie = self%ptr(i + 1) - 1
            do j = is, ie
                sum = sum + self%val(j) * x(self%ind(j))
            end do
            y(i) = alpha * sum + beta * y(i)
        end do
        !$omp end parallel do
    end subroutine gemv_crs

    !>
    !> Finds the 1-based index in the `val` and `ind` arrays corresponding to a specific matrix entry.
    !>
    pure module function find_crs(self, row, col) result(index)
        implicit none
        !> The CRS matrix object.
        class(type_crs), intent(in) :: self
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

    end function find_crs

    !>
    !> Prints the non-zero contents of the sparse matrix to standard output.
    !>
    module subroutine display_crs(self)
        implicit none
        !> The CRS matrix object to display.
        class(type_crs), intent(in) :: self
        integer(int32) :: i, r, row_start, row_end

        write (*, '(a,i0,2x,a,i0,a)') "CRS Matrix (dims= ", self%num_row, ", nnz= ", self%nnz, ")"
        do r = 1, self%num_row
            row_start = self%ptr(r)
            row_end = self%ptr(r + 1) - 1
            do i = row_start, row_end
                write (*, '(2(i0, ", "), es16.8)') r, self%ind(i), self%val(i)
            end do
        end do
    end subroutine display_crs

end submodule core_types_matrix_crs

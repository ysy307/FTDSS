!>
!> Implements the procedures for a Degree-of-Freedom (DOF) based Compressed
!> Row Storage (csr) sparse matrix.
!>
submodule(core_types_matrix) core_types_matrix_csr
    implicit none

contains

    !>
    !> Initializes the DOF-level csr matrix structure from a node-level adjacency pattern.
    !> It assumes the input node-level column indices (`col`) are sorted for each row segment.
    !> This routine expands the node-level graph into a full DOF-level matrix sparsity pattern.
    !>
    module subroutine initialize_type_csr(self, num_nodes, row, col, row_blocks, col_blocks)
        implicit none
        class(type_csr), intent(inout) :: self
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
        call allocate_array(self%diagonal, self%num_nodes)
        ! Initialize value array to zero
        call self%zero()

        self%is_initialized_matrix = .true.
        self%status = MATRIX_STATUS_SUCCESS
    end subroutine initialize_type_csr

    !>
    !> Deallocates all internal arrays of the csr matrix object.
    !>
    module subroutine destroy_csr(self)
        implicit none
        class(type_csr), intent(inout) :: self

        call deallocate_array(self%ptr)
        call deallocate_array(self%ind)
        call deallocate_array(self%val)
        call deallocate_array(self%diagonal)

        self%num_nodes = 0
        self%num_rows = 0
        self%num_ptrs = 0
        self%nnz = 0

        self%is_initialized_matrix = .false.
    end subroutine destroy_csr

    !>
    !> Returns the number of rows in the matrix.
    !>
    module subroutine get_info_csr(self, info)
        implicit none
        !> The matrix object.
        class(type_csr), intent(in) :: self
        !> The matrix information structure to populate.
        type(type_matrix_info), intent(inout) :: info

        info%num_nodes = self%num_nodes
        info%num_rows = self%num_rows
        info%num_ptrs = self%num_ptrs
        info%nnz = self%nnz
    end subroutine get_info_csr

    module function get_diagonal_csr(self) result(diagonal)
        implicit none
        class(type_csr), intent(inout), target :: self
        real(real64), dimension(:), pointer :: diagonal

        integer(int32) :: i, row_start, row_end, j

        ! Extract diagonal elements
        self%diagonal(:) = 0.0d0
        do i = 1, self%num_ptrs - 1
            row_start = self%ptr(i)
            row_end = self%ptr(i + 1) - 1
            do j = row_start, row_end
                if (self%ind(j) == i) then
                    diagonal(i) = self%val(j)
                    exit
                end if
            end do
        end do

        diagonal => self%diagonal
    end function get_diagonal_csr

    !>
    !> Returns a pointer to the internal CSR pointer array (`ptr`).
    !>
    module function get_ptr_csr(self) result(ptr)
        implicit none
        !> The csr matrix object.
        class(type_csr), intent(in), target :: self
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
        class(type_csr), intent(in), target :: self
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
        class(type_csr), intent(in), target :: self
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
        class(type_csr), intent(inout) :: self
        !> The operation to perform.
        integer(int32), intent(in) :: op
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
            select case (op)
            case (MATRIX_OP_INS)
                self%val(index) = value
            case (MATRIX_OP_ADD)
                self%val(index) = self%val(index) + value
            case default
                self%status = MATRIX_STATUS_ILL_OPERATIONS
            end select
#ifdef USE_DEBUG
        else
            print *, "Warning(set_value_csr): Element not in sparsity pattern.", row, col
        end if
#endif
    end subroutine set_value_csr

    !>
    !> Sets the value of a specific entry in the sparse matrix.
    !>
    module subroutine set_value_block_csr(self, op, row, col, row_block, col_block, value)
        implicit none
        !> The csr matrix object.
        class(type_csr), intent(inout) :: self
        !> The operation to perform.
        integer(int32), intent(in) :: op
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

        self%status = MATRIX_STATUS_NOT_IMPLEMENTED
    end subroutine set_value_block_csr

    !>
    !> Sets all non-zero entries in a specific row to a single scalar value.
    !>
    module subroutine set_row_csr(self, op, row, value)
        implicit none
        !> The csr matrix object.
        class(type_csr), intent(inout) :: self
        !> The operation to perform.
        integer(int32), intent(in) :: op
        !> The 1-based node index for the row.
        integer(int32), intent(in) :: row
        !> The scalar value to assign.
        real(real64), intent(in) :: value

        integer(int32) :: is, ie

        is = self%ptr(row)
        ie = self%ptr(row + 1) - 1

        select case (op)
        case (MATRIX_OP_INS)
            self%val(is:ie) = value
        case (MATRIX_OP_ADD)
            self%val(is:ie) = self%val(is:ie) + value
        case default
            self%status = MATRIX_STATUS_ILL_OPERATIONS
        end select
    end subroutine set_row_csr

    !>
    !> Sets all stored non-zero values in the matrix to a single scalar value.
    !>
    module subroutine set_all_csr(self, op, value)
        implicit none
        !> The csr matrix object.
        class(type_csr), intent(inout) :: self
        !> The operation to perform.
        integer(int32), intent(in) :: op
        !> The scalar value to assign to all non-zero entries.
        real(real64), intent(in) :: value

        select case (op)
        case (MATRIX_OP_INS)
            self%val = value
        case (MATRIX_OP_ADD)
            self%val = self%val + value
        case default
            self%status = MATRIX_STATUS_ILL_OPERATIONS
        end select
    end subroutine set_all_csr

    !>
    !> Sets all stored values in the matrix to zero.
    !>
    module subroutine zero_csr(self)
        implicit none
        !> The csr matrix object.
        class(type_csr), intent(inout) :: self

        self%val = 0.0d0
    end subroutine zero_csr

!     !>
!     !> Adds a value to a specific entry in the sparse matrix.
!     !>
!     module subroutine add_value_csr(self, row, col, value)
!         implicit none
!         !> The csr matrix object.
!         class(type_csr), intent(inout) :: self
!         !> The 1-based node index for the row.
!         integer(int32), intent(in) :: row
!         !> The 1-based node index for the column.
!         integer(int32), intent(in) :: col
!         !> The value to add to the specified entry.
!         real(real64), intent(in) :: value

!         integer(int32) :: index

!         index = self%find(row, col)
! #ifdef USE_DEBUG
!         if (index > 0) then
! #endif
!             self%val(index) = self%val(index) + value
! #ifdef USE_DEBUG
!         else
!             print *, "Warning(add_value_csr): Element not in sparsity pattern.", row, col
!         end if
! #endif
!     end subroutine add_value_csr

!     !>
!     !> Adds the values from another csr matrix to this matrix.
!     !> This simplified version requires both matrices to have identical sparsity patterns.
!     module subroutine add_values_csr(self, indices, values)
!         implicit none
!         !> The csr matrix object to modify (self).
!         class(type_csr), intent(inout) :: self
!         !> The 1-based node indices specifying which rows and columns to update.
!         integer(int32), intent(in) :: indices(:)
!         !> The abstract matrix containing values to add (must be of type_csr).
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
!     end subroutine add_values_csr
!     !>
!     !> Performs the matrix operation \( C = \alpha*A + B \), where A is self.
!     !> This simplified version requires all matrices to have identical sparsity patterns.
!     !>
!     module subroutine add_matrix_csr(self, alpha, B, C)
!         implicit none
!         !> The csr matrix object (A).
!         class(type_csr), intent(in) :: self
!         !> The scalar multiplier alpha.
!         real(real64), intent(in) :: alpha
!         !> The abstract matrix B (must be of type_csr).
!         class(abst_matrix), intent(in) :: B
!         !> The abstract matrix C to store the result (must be of type_csr).
!         class(abst_matrix), intent(inout) :: C

!         select type (B_csr => B)
!         type is (type_csr)
!             select type (C_csr => C)
!             type is (type_csr)
!                 if (self%nnz /= B_csr%nnz .or. self%nnz /= C_csr%nnz) then
!                     print *, "ERROR(add_matrix_csr): In this simplified version, NNZ must be identical."
!                     stop
!                 end if
!                 C_csr%val = alpha * self%val + B_csr%val
!             end select
!         end select
!     end subroutine add_matrix_csr

!     !>
!     !> Performs a sparse matrix-vector multiplication (GEMV): y = alpha*A*x + beta*y.
!     !>
!     module subroutine gemv_csr(self, alpha, x, beta, y)
!         implicit none
!         !> The csr matrix object (A).
!         class(type_csr), intent(in) :: self
!         !> The scalar multiplier alpha.
!         real(real64), intent(in) :: alpha
!         !> The input vector x.
!         real(real64), intent(in) :: x(:)
!         !> The scalar multiplier beta.
!         real(real64), intent(in) :: beta
!         !> The input/output vector y.
!         real(real64), intent(inout) :: y(:)

!         integer(int32) :: i, j, is, ie
!         real(real64) :: sum

!         !$omp parallel do private(i, j, is, ie, sum)
!         do i = 1, self%num_row
!             sum = 0.0d0
!             is = self%ptr(i)
!             ie = self%ptr(i + 1) - 1
!             do j = is, ie
!                 sum = sum + self%val(j) * x(self%ind(j))
!             end do
!             y(i) = alpha * sum + beta * y(i)
!         end do
!         !$omp end parallel do
!     end subroutine gemv_csr

    !>
    !> Finds the 1-based index in the `val` and `ind` arrays corresponding to a specific matrix entry.
    !>
    pure module function find_csr(self, row, col) result(index)
        implicit none
        !> The csr matrix object.
        class(type_csr), intent(in) :: self
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
    module subroutine display_csr(self)
        implicit none
        !> The csr matrix object to display.
        class(type_csr), intent(in) :: self
        integer(int32) :: i, r, row_start, row_end

        write (*, '(a,i0,2x,a,i0,a)') "csr Matrix (dims= ", self%num_rows, ", nnz= ", self%nnz, ")"
        do r = 1, self%num_rows
            row_start = self%ptr(r)
            row_end = self%ptr(r + 1) - 1
            do i = row_start, row_end
                write (*, '(2(i0, ", "), es16.8)') r, self%ind(i), self%val(i)
            end do
        end do
    end subroutine display_csr

end submodule core_types_matrix_csr

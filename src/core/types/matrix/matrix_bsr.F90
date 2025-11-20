!>
!> Implements the procedures for a Degree-of-Freedom (DOF) based Compressed
!> Row Storage (bsr) sparse matrix.
!>
submodule(core_types_matrix) core_types_matrix_bsr
    implicit none

contains

    !>
    !> Initializes the DOF-level bsr matrix structure from a node-level adjacency pattern.
    !> It assumes the input node-level column indices (`col`) are sorted for each row segment.
    !> This routine expands the node-level graph into a full DOF-level matrix sparsity pattern.
    !>
    module subroutine initialize_type_bsr(self, num_nodes, row, col, row_blocks, col_blocks)
        implicit none
        class(type_bsr), intent(inout) :: self
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

        if (.not. present(row_blocks) .or. .not. present(col_blocks)) then
            print *, "Error: row_blocks and col_blocks must be provided for bsr matrix."
            stop
        end if

        if (size(row) /= num_nodes + 1) then
            print *, "Error: The size of row (node_ptr) array must be num_nodes + 1."
            stop
        end if

        self%num_nodes = num_nodes
        self%num_rows = num_nodes
        self%num_ptrs = num_nodes + 1
        self%num_blocks = size(col)
        self%num_block_rows = row_blocks
        self%num_block_cols = col_blocks
        self%nnz = self%num_block_rows * self%num_block_cols * self%num_blocks

        ! Allocate arrays
        call allocate_array(self%ptr, source=row)
        call allocate_array(self%ind, source=col)
        call allocate_array(self%val, row_blocks, col_blocks, self%nnz)
        call allocate_array(self%diagonal, self%num_block_rows * self%num_nodes)
        ! Initialize value array to zero
        call self%zero()

        self%is_initialized_matrix = .true.
        self%status = MATRIX_STATUS_SUCCESS
    end subroutine initialize_type_bsr

    !>
    !> Deallocates all internal arrays of the bsr matrix object.
    !>
    module subroutine destroy_bsr(self)
        implicit none
        class(type_bsr), intent(inout) :: self

        call deallocate_array(self%ptr)
        call deallocate_array(self%ind)
        call deallocate_array(self%val)
        call deallocate_array(self%diagonal)

        self%num_nodes = 0
        self%num_rows = 0
        self%num_ptrs = 0
        self%nnz = 0

        self%is_initialized_matrix = .false.
    end subroutine destroy_bsr

    !>
    !> Returns the number of rows in the matrix.
    !>
    module subroutine get_info_bsr(self, info)
        implicit none
        !> The matrix object.
        class(type_bsr), intent(in) :: self
        !> The matrix information structure to populate.
        type(type_matrix_info), intent(inout) :: info

        info%num_nodes = self%num_nodes
        info%num_rows = self%num_rows
        info%num_ptrs = self%num_ptrs
        info%num_blocks = self%num_blocks
        info%num_block_rows = self%num_block_rows
        info%num_block_cols = self%num_block_cols
        info%nnz = self%nnz
    end subroutine get_info_bsr

    module function get_diagonal_bsr(self) result(diagonal)
        implicit none
        class(type_bsr), intent(inout), target :: self
        real(real64), dimension(:), pointer :: diagonal

        integer(int32) :: i, row_start, row_end, j, k, m

        ! Extract diagonal elements
        self%diagonal(:) = 0.0d0
        do i = 1, self%num_ptrs - 1
            row_start = self%ptr(i)
            row_end = self%ptr(i + 1) - 1
            do j = row_start, row_end
                if (self%ind(j) == i) then
                    do k = 1, self%num_block_rows
                        do m = 1, self%num_block_cols
                            if (k == m) then
                                diagonal((i - 1) * self%num_block_rows + k) = self%val(j, k, m)
                            end if
                        end do
                        exit
                    end do
                end if
            end do
        end do

        diagonal => self%diagonal
    end function get_diagonal_bsr

    !>
    !> Returns a pointer to the internal bsr pointer array (`ptr`).
    !>
    module function get_ptr_bsr(self) result(ptr)
        implicit none
        !> The bsr matrix object.
        class(type_bsr), intent(in), target :: self
        !> A pointer to the bsr `ptr` array.
        integer(int32), dimension(:), pointer :: ptr

        ptr => self%ptr
    end function get_ptr_bsr

    !>
    !> Returns a pointer to the internal bsr column index array (`ind`).
    !>
    module function get_ind_bsr(self) result(ind)
        implicit none
        !> The bsr matrix object.
        class(type_bsr), intent(in), target :: self
        !> A pointer to the bsr `ind` array.
        integer(int32), dimension(:), pointer :: ind

        ind => self%ind
    end function get_ind_bsr

    !>
    !> Returns a pointer to the internal bsr value array (`val`).
    !>
    module function get_val_bsr(self) result(val)
        implicit none
        !> The bsr matrix object.
        class(type_bsr), intent(in), target :: self
        !> A pointer to the bsr `val` array.
        real(real64), dimension(:), pointer :: val

        val => self%val
    end function get_val_bsr

    !>
    !> Sets the value of a specific entry in the sparse matrix.
    !>
    module subroutine set_value_bsr(self, op, row, col, value)
        implicit none
        !> The bsr matrix object.
        class(type_bsr), intent(inout) :: self
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
            print *, "Warning(set_value_bsr): Element not in sparsity pattern.", row, col
        end if
#endif
    end subroutine set_value_bsr

    !>
    !> Sets the value of a specific entry in the sparse matrix.
    !>
    module subroutine set_value_block_bsr(self, op, row, col, row_block, col_block, value)
        implicit none
        !> The bsr matrix object.
        class(type_bsr), intent(inout) :: self
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
    end subroutine set_value_block_bsr

    !>
    !> Sets all non-zero entries in a specific row to a single scalar value.
    !>
    module subroutine set_row_bsr(self, op, row, value)
        implicit none
        !> The bsr matrix object.
        class(type_bsr), intent(inout) :: self
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
    end subroutine set_row_bsr

    !>
    !> Sets all stored non-zero values in the matrix to a single scalar value.
    !>
    module subroutine set_all_bsr(self, op, value)
        implicit none
        !> The bsr matrix object.
        class(type_bsr), intent(inout) :: self
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
    end subroutine set_all_bsr

    !>
    !> Sets all stored values in the matrix to zero.
    !>
    module subroutine zero_bsr(self)
        implicit none
        !> The bsr matrix object.
        class(type_bsr), intent(inout) :: self

        self%val = 0.0d0
    end subroutine zero_bsr

!     !>
!     !> Adds a value to a specific entry in the sparse matrix.
!     !>
!     module subroutine add_value_bsr(self, row, col, value)
!         implicit none
!         !> The bsr matrix object.
!         class(type_bsr), intent(inout) :: self
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
!             print *, "Warning(add_value_bsr): Element not in sparsity pattern.", row, col
!         end if
! #endif
!     end subroutine add_value_bsr

!     !>
!     !> Adds the values from another bsr matrix to this matrix.
!     !> This simplified version requires both matrices to have identical sparsity patterns.
!     module subroutine add_values_bsr(self, indices, values)
!         implicit none
!         !> The bsr matrix object to modify (self).
!         class(type_bsr), intent(inout) :: self
!         !> The 1-based node indices specifying which rows and columns to update.
!         integer(int32), intent(in) :: indices(:)
!         !> The abstract matrix containing values to add (must be of type_bsr).
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
!     end subroutine add_values_bsr
!     !>
!     !> Performs the matrix operation \( C = \alpha*A + B \), where A is self.
!     !> This simplified version requires all matrices to have identical sparsity patterns.
!     !>
!     module subroutine add_matrix_bsr(self, alpha, B, C)
!         implicit none
!         !> The bsr matrix object (A).
!         class(type_bsr), intent(in) :: self
!         !> The scalar multiplier alpha.
!         real(real64), intent(in) :: alpha
!         !> The abstract matrix B (must be of type_bsr).
!         class(abst_matrix), intent(in) :: B
!         !> The abstract matrix C to store the result (must be of type_bsr).
!         class(abst_matrix), intent(inout) :: C

!         select type (B_bsr => B)
!         type is (type_bsr)
!             select type (C_bsr => C)
!             type is (type_bsr)
!                 if (self%nnz /= B_bsr%nnz .or. self%nnz /= C_bsr%nnz) then
!                     print *, "ERROR(add_matrix_bsr): In this simplified version, NNZ must be identical."
!                     stop
!                 end if
!                 C_bsr%val = alpha * self%val + B_bsr%val
!             end select
!         end select
!     end subroutine add_matrix_bsr

!     !>
!     !> Performs a sparse matrix-vector multiplication (GEMV): y = alpha*A*x + beta*y.
!     !>
!     module subroutine gemv_bsr(self, alpha, x, beta, y)
!         implicit none
!         !> The bsr matrix object (A).
!         class(type_bsr), intent(in) :: self
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
!     end subroutine gemv_bsr

    !>
    !> Finds the 1-based index in the `val` and `ind` arrays corresponding to a specific matrix entry.
    !>
    pure module function find_bsr(self, row, col) result(index)
        implicit none
        !> The bsr matrix object.
        class(type_bsr), intent(in) :: self
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

    end function find_bsr

    !>
    !> Prints the non-zero contents of the sparse matrix to standard output.
    !>
    module subroutine display_bsr(self)
        implicit none
        !> The bsr matrix object to display.
        class(type_bsr), intent(in) :: self
        integer(int32) :: i, r, row_start, row_end

        write (*, '(a,i0,2x,a,i0,a)') "bsr Matrix (dims= ", self%num_rows, ", nnz= ", self%nnz, ")"
        do r = 1, self%num_rows
            row_start = self%ptr(r)
            row_end = self%ptr(r + 1) - 1
            do i = row_start, row_end
                write (*, '(2(i0, ", "), es16.8)') r, self%ind(i), self%val(i)
            end do
        end do
    end subroutine display_bsr

end submodule core_types_matrix_bsr

!>
!> Implements the procedures for a Diagonal (DIA) storage sparse matrix.
!>
submodule(core_types_matrix) core_types_matrix_dia
    implicit none

contains

    !>
    !> Initializes the DIA matrix structure from a node-level sparsity pattern.
    !> Detects unique diagonals automatically from the provided row/col indices.
    !>
    module subroutine initialize_type_matrix_dia(self, num_nodes, row, col, row_blocks, col_blocks)
        implicit none
        class(type_matrix_dia), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in), optional :: row(:)
        integer(int32), intent(in), optional :: col(:)
        integer(int32), intent(in), optional :: row_blocks
        integer(int32), intent(in), optional :: col_blocks

        integer(int32), allocatable :: temp_offsets(:)
        integer(int32) :: i, k, current_offset, n_entries, n_unique
        logical :: found

        ! Argument validation
        if (.not. present(row) .or. .not. present(col)) then
            ! Default to main diagonal only if no pattern provided
            self%num_nodes = num_nodes
            self%num_rows = num_nodes
            self%num_cols = num_nodes
            self%num_diags = 1
            call allocate_array(self%offsets, 1)
            self%offsets(1) = 0
            call allocate_array(self%val, self%num_rows, self%num_diags)
            call self%zero()
            self%is_initialized_matrix = .true.
            self%status = MATRIX_STATUS%SUCCESS
            return
        end if

        self%num_nodes = num_nodes
        self%num_rows = num_nodes
        self%num_cols = num_nodes

        n_entries = size(row)
        allocate (temp_offsets(n_entries))
        n_unique = 0

        ! Identify unique diagonals (offsets = col - row)
        do i = 1, n_entries
            current_offset = col(i) - row(i)
            found = .false.
            do k = 1, n_unique
                if (temp_offsets(k) == current_offset) then
                    found = .true.
                    exit
                end if
            end do
            if (.not. found) then
                n_unique = n_unique + 1
                temp_offsets(n_unique) = current_offset
            end if
        end do

        self%num_diags = n_unique
        call allocate_array(self%offsets, n_unique)
        self%offsets(:) = temp_offsets(1:n_unique)
        deallocate (temp_offsets)

        ! Sort offsets (Bubble sort is sufficient for small num_diags)
        do i = 1, n_unique - 1
            do k = i + 1, n_unique
                if (self%offsets(i) > self%offsets(k)) then
                    current_offset = self%offsets(i)
                    self%offsets(i) = self%offsets(k)
                    self%offsets(k) = current_offset
                end if
            end do
        end do

        ! Allocate values (Rows x Diags)
        call allocate_array(self%val, self%num_rows, self%num_diags)

        ! Initialize value array to zero
        call self%zero()

        ! Approximate nnz (actual stored elements including padding)
        self%nnz = self%num_rows * self%num_diags

        self%is_initialized_matrix = .true.
        self%status = MATRIX_STATUS%SUCCESS
    end subroutine initialize_type_matrix_dia

    !>
    !> Deallocates all internal arrays of the DIA matrix object.
    !>
    module subroutine destroy_dia(self)
        implicit none
        class(type_matrix_dia), intent(inout) :: self

        call deallocate_array(self%offsets)
        call deallocate_array(self%val)

        self%num_diags = 0
        self%nnz = 0
        self%is_initialized_matrix = .false.
    end subroutine destroy_dia

    module subroutine get_info_dia(self, info)
        implicit none
        class(type_matrix_dia), intent(in) :: self
        type(type_matrix_info), intent(inout) :: info

        info%num_nodes = self%num_nodes
        info%num_rows = self%num_rows
        info%num_cols = self%num_cols
        info%nnz = self%nnz
        ! Reusing num_block_cols to store num_diags if needed, or add specific field
        info%num_block_cols = self%num_diags
    end subroutine get_info_dia

    !>
    !> Extracts the diagonal elements of the matrix.
    !>
    module subroutine get_diagonal_dia(self, diagonal)
        implicit none
        class(type_matrix_dia), intent(in) :: self
        type(type_vector_dp), intent(inout) :: diagonal

        integer(int32) :: i, main_diag_idx

        ! Find index of the main diagonal (offset 0)
        main_diag_idx = -1
        do i = 1, self%num_diags
            if (self%offsets(i) == 0) then
                main_diag_idx = i
                exit
            end if
        end do

        if (main_diag_idx /= -1) then
            ! Use type_vector_dp's set method instead of direct access
            do i = 1, self%num_rows
                call diagonal%set(MATRIX_OPS%INS, i, self%val(i, main_diag_idx))
            end do
        else
            ! No main diagonal stored, set to zero
            call diagonal%set(MATRIX_OPS%INS, 0.0_real64)
        end if
    end subroutine get_diagonal_dia

    module function get_val_dia(self) result(val)
        implicit none
        class(type_matrix_dia), intent(in), target :: self
        real(real64), dimension(:, :), pointer :: val
        val => self%val
    end function get_val_dia

    module function get_offsets_dia(self) result(offsets)
        implicit none
        class(type_matrix_dia), intent(in), target :: self
        integer(int32), dimension(:), pointer :: offsets
        offsets => self%offsets
    end function get_offsets_dia

    !>
    !> Finds the diagonal index for a given row and col.
    !> Returns -1 if the diagonal is not stored.
    !>
    pure module function find_dia(self, row, col) result(index)
        implicit none
        class(type_matrix_dia), intent(in) :: self
        integer(int32), intent(in) :: row, col
        integer(int32) :: index
        integer(int32) :: target_offset, i

        index = -1
        target_offset = col - row

        ! Linear search for the diagonal index
        ! Since num_diags is small (e.g., 3 for tridiagonal), this is fast.
        do i = 1, self%num_diags
            if (self%offsets(i) == target_offset) then
                index = i
                return
            end if
        end do
    end function find_dia

    !>
    !> Sets a single value in the DIA matrix.
    !>
    module subroutine set_value_dia(self, op, row, col, value)
        implicit none
        class(type_matrix_dia), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: row, col
        real(real64), intent(in) :: value
        integer(int32) :: diag_idx

        if (.not. MATRIX_OPS%is_valid(op)) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        diag_idx = self%find(row, col)

#ifdef USE_DEBUG
        if (diag_idx > 0) then
#endif
            if (op == MATRIX_OPS%INS) then
                self%val(row, diag_idx) = value
            else if (op == MATRIX_OPS%ADD) then
                self%val(row, diag_idx) = self%val(row, diag_idx) + value
            else
                self%status = MATRIX_STATUS%ILL_OPERATIONS
            end if
#ifdef USE_DEBUG
        else
            ! Attempted to set value in a non-allocated diagonal
            self%status = MATRIX_STATUS%OUT_OF_MEMORY
        end if
#endif
    end subroutine set_value_dia

    module subroutine set_value_block_dia(self, op, row, col, row_block, col_block, value)
        implicit none
        class(type_matrix_dia), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: row, col
        integer(int32), intent(in) :: row_block, col_block
        real(real64), intent(in) :: value

        ! DIA doesn't support blocks, treat as scalar
        call self%set_value(op, row, col, value)
    end subroutine set_value_block_dia

    !>
    !> Sets an entire row to a value (only for allocated diagonals).
    !>
    module subroutine set_row_dia(self, op, row, value, row_block)
        implicit none
        class(type_matrix_dia), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: row
        real(real64), intent(in) :: value
        integer(int32), intent(in), optional :: row_block
        integer(int32) :: i

        if (.not. MATRIX_OPS%is_valid(op)) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        if (op == MATRIX_OPS%INS) then
            do i = 1, self%num_diags
                self%val(row, i) = value
            end do
        else if (op == MATRIX_OPS%ADD) then
            do i = 1, self%num_diags
                self%val(row, i) = self%val(row, i) + value
            end do
        else
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end if
    end subroutine set_row_dia

    module subroutine set_all_dia(self, op, value)
        implicit none
        class(type_matrix_dia), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        real(real64), intent(in) :: value

        if (.not. MATRIX_OPS%is_valid(op)) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        if (op == MATRIX_OPS%INS) then
            self%val(:, :) = value
        else if (op == MATRIX_OPS%ADD) then
            self%val(:, :) = self%val(:, :) + value
        else
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end if

    end subroutine set_all_dia

    !>
    !> Scales the matrix.
    !>
    module subroutine scale_dia(self, op, alpha)
        implicit none
        class(type_matrix_dia), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        type(type_vector_dp), intent(in) :: alpha

        integer(int32) :: i, j, col_idx
        real(real64), dimension(:), pointer :: alpha_data

        if (.not. MATRIX_OPS%is_valid(op)) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        ! Get direct access to vector data
        alpha_data => alpha%get_data()

        ! Check size
        if (size(alpha_data) /= self%num_nodes) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
            return
        end if

        !-------------------------------------------------
        ! Symmetric Scaling: A_ij <- A_ij * alpha(i) * alpha(j)
        !-------------------------------------------------
        if (op == MATRIX_OPS%SCALE_SYMM_DIAG) then
            !$omp parallel do default(shared) private(i, j, col_idx) schedule(static)
            do j = 1, self%num_diags
                do i = 1, self%num_rows
                    col_idx = i + self%offsets(j)
                    ! Check bounds for valid columns
                    if (col_idx >= 1 .and. col_idx <= self%num_cols) then
                        self%val(i, j) = self%val(i, j) * alpha_data(i) * alpha_data(col_idx)
                    end if
                end do
            end do
            !$omp end parallel do
            self%status = MATRIX_STATUS%SUCCESS

            !-------------------------------------------------
            ! Jacobi Scaling: A_ij <- A_ij * alpha(i)
            !-------------------------------------------------
        else if (op == MATRIX_OPS%SCALE_JACOBI) then
            !$omp parallel do default(shared) private(i, j) schedule(static)
            do j = 1, self%num_diags
                do i = 1, self%num_rows
                    self%val(i, j) = self%val(i, j) * alpha_data(i)
                end do
            end do
            !$omp end parallel do
            self%status = MATRIX_STATUS%SUCCESS
        else
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end if
    end subroutine scale_dia

    module subroutine zero_all_dia(self)
        implicit none
        class(type_matrix_dia), intent(inout) :: self
        self%val(:, :) = 0.0_real64
    end subroutine zero_all_dia

    module subroutine zero_row_dia(self, row, row_block)
        implicit none
        class(type_matrix_dia), intent(inout) :: self
        integer(int32), intent(in) :: row
        integer(int32), intent(in), optional :: row_block

        self%val(row, :) = 0.0_real64
    end subroutine zero_row_dia

    module subroutine display_dia(self, unit_in)
        implicit none
        class(type_matrix_dia), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in
        integer(int32) :: u, i, j, col_idx

        u = optval(unit_in, output_unit)
        write (u, '(a,i0,2x,a,i0,a)') "DIA Matrix (dims= ", self%num_rows, ", diags= ", self%num_diags, ")"

        do i = 1, self%num_rows
            do j = 1, self%num_diags
                col_idx = i + self%offsets(j)
                if (col_idx >= 1 .and. col_idx <= self%num_cols) then
                    if (abs(self%val(i, j)) > 1.0e-16_real64) then
                        write (u, '(2(i0, ", "), es16.8e3)') i, col_idx, self%val(i, j)
                    end if
                end if
            end do
        end do
    end subroutine display_dia

end submodule core_types_matrix_dia

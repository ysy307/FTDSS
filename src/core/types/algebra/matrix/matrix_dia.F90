!>
!> Implements the procedures for a Diagonal (DIA) storage sparse matrix.
!>
submodule(core_types_algebra_matrix) algebra_matrix_dia
    implicit none

contains

    !>
    !> Initializes the DIA matrix structure from a node-level sparsity pattern.
    !> Detects unique diagonals automatically from the provided row/col indices.
    !>
    module subroutine initialize_dia(self, num_nodes, row, col, row_blocks, col_blocks)
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
    end subroutine initialize_dia

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
            ! Use type_vector_dp set method instead of direct access
            do i = 1, self%num_rows
                call diagonal%set(VECTOR_OPS%INS, i, self%val(i, main_diag_idx))
            end do
        else
            ! No main diagonal stored, set to zero
            call diagonal%set(VECTOR_OPS%INS, 0.0d0)
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
    !> Sets a single value in the DIA matrix.
    !>
    module subroutine set_value_dia(self, op, row, col, value)
        implicit none
        class(type_matrix_dia), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: row, col
        real(real64), intent(in) :: value
        error stop "Error: set_value is only permitted for dense matrix."
    end subroutine set_value_dia

    !> Sets a stored entry by its flat (column-major) position in val(:,:) (no search).
    module subroutine set_value_at_dia(self, op, idx, value)
        implicit none
        class(type_matrix_dia), intent(inout), target :: self
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
    end subroutine set_value_at_dia

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

        if (.not. MATRIX_OPS%is_valid(op)) then
            self%status = MATRIX_STATUS%ILL_OPERATIONS
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
    end subroutine set_row_dia

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

        select case (op%ID)
        case (MATRIX_OPS%SCALE_SYMM_DIAG%ID)
            ! A_ij <- A_ij * alpha(i) * alpha(j)
            !$omp parallel do default(shared) private(i, j, col_idx) schedule(static)
            do j = 1, self%num_diags
                do i = 1, self%num_rows
                    col_idx = i + self%offsets(j)
                    if (col_idx >= 1 .and. col_idx <= self%num_cols) then
                        self%val(i, j) = self%val(i, j) * alpha_data(i) * alpha_data(col_idx)
                    end if
                end do
            end do
            !$omp end parallel do
            self%status = MATRIX_STATUS%SUCCESS
        case (MATRIX_OPS%SCALE_JACOBI%ID)
            ! A_ij <- A_ij * alpha(i)
            !$omp parallel do default(shared) private(i, j) schedule(static)
            do j = 1, self%num_diags
                do i = 1, self%num_rows
                    self%val(i, j) = self%val(i, j) * alpha_data(i)
                end do
            end do
            !$omp end parallel do
            self%status = MATRIX_STATUS%SUCCESS
        case default
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end select
    end subroutine scale_dia

    module subroutine zero_all_dia(self)
        implicit none
        class(type_matrix_dia), intent(inout) :: self
        self%val(:, :) = 0.0d0
    end subroutine zero_all_dia

    module subroutine zero_row_dia(self, row, row_block)
        implicit none
        class(type_matrix_dia), intent(inout) :: self
        integer(int32), intent(in) :: row
        integer(int32), intent(in), optional :: row_block

        self%val(row, :) = 0.0d0
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
                    if (abs(self%val(i, j)) > 1.0d-16) then
                        write (u, '(2(i0, ", "), es16.8e3)') i, col_idx, self%val(i, j)
                    end if
                end if
            end do
        end do
    end subroutine display_dia

    module subroutine set_local_matrix_dia(self, op, n_local, indices, row_sys, col_sys, local_data)
        implicit none
        class(type_matrix_dia), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: n_local
        integer(int32), intent(in) :: indices(:, :)
        integer(int32), intent(in) :: row_sys, col_sys
        real(real64), dimension(:, :), intent(in) :: local_data
        integer(int32) :: i, j, idx, r_idx, d_idx

        select case (op%ID)
        case (MATRIX_OPS%INS%ID)
            do j = 1, n_local
                do i = 1, n_local
                    idx = indices(i, j)
                    if (idx > 0) then
                        r_idx = mod(idx - 1, self%num_rows) + 1
                        d_idx = (idx - 1) / self%num_rows + 1
                        self%val(r_idx, d_idx) = local_data(i, j)
                    end if
                end do
            end do
        case (MATRIX_OPS%ADD%ID)
            do j = 1, n_local
                do i = 1, n_local
                    idx = indices(i, j)
                    if (idx > 0) then
                        r_idx = mod(idx - 1, self%num_rows) + 1
                        d_idx = (idx - 1) / self%num_rows + 1
                        self%val(r_idx, d_idx) = self%val(r_idx, d_idx) + local_data(i, j)
                    end if
                end do
            end do
        case default
            self%status = MATRIX_STATUS%ILL_OPERATIONS
        end select
    end subroutine set_local_matrix_dia

end submodule algebra_matrix_dia

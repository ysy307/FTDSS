submodule(types_algebra_vector) algebra_vector_dp
    implicit none

contains

    !>
    !> Initializes the vector.
    !>
    module subroutine initialize_vector_dp(self, num_nodes, num_blocks)
        implicit none
        class(type_vector_dp), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in), optional :: num_blocks

        ! 既存の配列があれば解放
        if (allocated(self%val)) call deallocate_array(self%val)

        self%num_nodes = num_nodes
        if (present(num_blocks)) then
            self%num_blocks = num_blocks
        else
            self%num_blocks = 1
        end if

        call allocate_array(self%val, self%num_blocks * self%num_nodes)
        self%val = 0.0d0 ! 配列演算で一括初期化

        self%status = VECTOR_STATUS%SUCCESS
        self%is_initialized_vector = .true.
    end subroutine initialize_vector_dp

    !>
    !> Destroys the vector.
    !>
    module subroutine destroy_vector_dp(self)
        implicit none
        class(type_vector_dp), intent(inout) :: self

        if (allocated(self%val)) call deallocate_array(self%val)
        self%num_nodes = 0
        self%num_blocks = 0
        self%status = VECTOR_STATUS%SUCCESS
        self%is_initialized_vector = .false.
    end subroutine destroy_vector_dp

    !>
    !> Checks initialization status.
    !>
    module pure function is_initialized_vector_dp(self) result(initialized)
        implicit none
        class(type_vector_dp), intent(in) :: self
        logical :: initialized
        initialized = self%is_initialized_vector
    end function is_initialized_vector_dp

    !>
    !> Returns size.
    !>
    module pure function get_size_vector_dp(self) result(vector_size)
        implicit none
        class(type_vector_dp), intent(in) :: self
        integer(int32) :: vector_size
        vector_size = self%num_nodes
    end function get_size_vector_dp

    !>
    !> Returns data pointer.
    !>
    module function get_data_vector_dp(self) result(data)
        implicit none
        class(type_vector_dp), intent(in), target :: self
        real(real64), pointer, contiguous, dimension(:) :: data

        data => self%val
    end function get_data_vector_dp

    module pure function get_status_vector_dp(self) result(status)
        implicit none
        class(type_vector_dp), intent(in) :: self
        type(type_constant_id) :: status

        status = self%status
    end function get_status_vector_dp

    !>
    !> Sets scalar value using array syntax.
    !>
    module subroutine set_scalar_vector_dp(self, op, scalar_value, row_block)
        implicit none
        class(type_vector_dp), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        real(real64), intent(in) :: scalar_value
        integer(int32), intent(in), optional :: row_block

        integer(int32) :: nb

        if (.not. VECTOR_OPS%is_valid(op)) then
            self%status = VECTOR_STATUS%ILL_OPERATIONS
            return
        end if
        nb = optval(row_block, 1)

#ifdef USE_DEBUG
        if (.not. value_in_range(nb, 1_int32, self%num_blocks)) then
            self%status = VECTOR_STATUS%OUT_OF_MEMORY
            return
        end if
#endif

        associate (val => self%val, n_blks => self%num_blocks)
            if (op == VECTOR_OPS%INS) then
                val(nb :: n_blks) = scalar_value
            else if (op == VECTOR_OPS%ADD) then
                val(nb :: n_blks) = val(nb :: n_blks) + scalar_value
            else
                self%status = VECTOR_STATUS%ILL_OPERATIONS
            end if
        end associate
    end subroutine set_scalar_vector_dp

    !>
    !> Sets array values using array syntax.
    !>
    module subroutine set_array_vector_dp(self, op, array_value, row_block)
        implicit none
        class(type_vector_dp), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        real(real64), intent(in) :: array_value(:)
        integer(int32), intent(in), optional :: row_block

        integer(int32) :: nb

        if (.not. VECTOR_OPS%is_valid(op)) then
            self%status = VECTOR_STATUS%ILL_OPERATIONS
            return
        end if
        nb = optval(row_block, 1)

        if (.not. value_in_range(nb, 1_int32, self%num_blocks)) then
            self%status = VECTOR_STATUS%OUT_OF_MEMORY
            return
        end if

        ! 入力配列サイズのチェック（本来はここで行うべき）
        if (size(array_value) /= self%num_nodes) then
            call check_match_length(array_value, self%val, "set_array_vector_dp") ! 既存のエラー処理へ委譲
            return
        end if

        associate (val => self%val, n_blks => self%num_blocks)
            if (op == VECTOR_OPS%INS) then
                val(nb :: n_blks) = array_value
            else if (op == VECTOR_OPS%ADD) then
                val(nb :: n_blks) = val(nb :: n_blks) + array_value
            else
                self%status = VECTOR_STATUS%ILL_OPERATIONS
            end if
        end associate
    end subroutine set_array_vector_dp

    !>
    !> Sets value at specific index.
    !>
    module subroutine set_value_at_index_vector_dp(self, op, global_index, value, row_block)
        implicit none
        class(type_vector_dp), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: global_index
        real(real64), intent(in) :: value
        integer(int32), intent(in), optional :: row_block

        integer(int32) :: nb, idx

        if (.not. VECTOR_OPS%is_valid(op)) then
            self%status = VECTOR_STATUS%ILL_OPERATIONS
            return
        end if
        nb = optval(row_block, 1)

        ! 範囲チェック
#ifdef USE_DEBUG
        if (.not. value_in_range(nb, 1_int32, self%num_blocks) .or. &
            .not. value_in_range(global_index, 1_int32, self%num_nodes)) then
            self%status = VECTOR_STATUS%OUT_OF_MEMORY
            return
        end if
#endif

        ! インデックス計算
        idx = (global_index - 1) * self%num_blocks + nb

        if (op == VECTOR_OPS%INS) then
            self%val(idx) = value
        else if (op == VECTOR_OPS%ADD) then
            self%val(idx) = self%val(idx) + value
        else
            self%status = VECTOR_STATUS%ILL_OPERATIONS
        end if
    end subroutine set_value_at_index_vector_dp

    !>
    !> Sets values using vector subscripts (Scatter).
    !>
    module subroutine set_values_at_indices_vector_dp(self, op, global_indices, new_values, row_block)
        implicit none
        class(type_vector_dp), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        integer(int32), intent(in) :: global_indices(:)
        real(real64), intent(in) :: new_values(:)
        integer(int32), intent(in), optional :: row_block

        integer(int32) :: nb
        integer(int32), allocatable :: target_indices(:)
        integer(int32) :: i
        logical :: error_found

        if (.not. VECTOR_OPS%is_valid(op)) then
            self%status = VECTOR_STATUS%ILL_OPERATIONS
            return
        end if
        nb = optval(row_block, 1)

#ifdef USE_DEBUG
        if (.not. value_in_range(nb, 1_int32, self%num_blocks)) then
            self%status = VECTOR_STATUS%OUT_OF_MEMORY
            return
        end if
#endif
        call allocate_array(target_indices, size(global_indices))

        target_indices = (global_indices - 1) * self%num_blocks + nb

#ifdef USE_DEBUG
        if (any(target_indices < 1 .or. target_indices > size(self%val))) then
            self%status = VECTOR_STATUS%OUT_OF_MEMORY
            call deallocate_array(target_indices)
            return
        end if
#endif

        if (op == VECTOR_OPS%INS) then
            self%val(target_indices) = new_values
        else if (op == VECTOR_OPS%ADD) then
            self%val(target_indices) = self%val(target_indices) + new_values
        else
            self%status = VECTOR_STATUS%ILL_OPERATIONS
        end if

        call deallocate_array(target_indices)

    end subroutine set_values_at_indices_vector_dp

    module subroutine scale_vector_dp(self, op, alpha)
        implicit none
        class(type_vector_dp), intent(inout) :: self
        type(type_constant_id), intent(in) :: op
        class(type_vector_dp), intent(in) :: alpha

        integer(int32) :: i
        real(real64), dimension(:), pointer :: alpha_data

        if (.not. VECTOR_OPS%is_valid(op)) then
            self%status = VECTOR_STATUS%ILL_OPERATIONS
            return
        end if

        alpha_data => alpha%get_data()

        if (op == VECTOR_OPS%SCALE_SYMM_DIAG .or. &
            op == VECTOR_OPS%SCALE_JACOBI) then
            self%val = self%val * alpha_data
        else
            self%status = VECTOR_STATUS%ILL_OPERATIONS
        end if

    end subroutine scale_vector_dp

    !>
    !> Optimized Copy.
    !>
    module subroutine copy_vector_dp(self, source_vector)
        implicit none
        class(type_vector_dp), intent(inout) :: self
        class(type_vector_dp), intent(in) :: source_vector

        if (self%is_initialized_vector .and. &
            self%num_nodes == source_vector%num_nodes .and. &
            self%num_blocks == source_vector%num_blocks) then

            self%val = source_vector%val
        else
            if (self%is_initialized_vector) call self%destroy()
            call self%initialize(source_vector%num_nodes, source_vector%num_blocks)
            self%val = source_vector%val
        end if

        self%status = VECTOR_STATUS%SUCCESS
    end subroutine copy_vector_dp

    !>
    !> Zero vector.
    !>
    module subroutine zero_vector_dp(self)
        implicit none
        class(type_vector_dp), intent(inout) :: self

        self%val = 0.0d0
    end subroutine zero_vector_dp

    !>
    !> Display with block awareness.
    !>
    module subroutine display_vector_dp(self, unit_in)
        implicit none
        class(type_vector_dp), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: i, b, idx
        integer(int32) :: unit

        unit = optval(unit_in, output_unit)

        if (.not. self%is_initialized_vector) then
            write (unit, '(a)') "Vector not initialized."
            return
        end if

        write (unit, '(a,i0,a,i0)') "Vector Size:", self%num_nodes, " Blocks:", self%num_blocks
        do i = 1, self%num_nodes
            do b = 1, self%num_blocks
                idx = (i - 1) * self%num_blocks + b
                write (unit, '(A,I0,A,I0,A,es16.8e3)') &
                    "Node ", i, " Block ", b, ": ", self%val(idx)
            end do
        end do
    end subroutine display_vector_dp

    module subroutine check_vector_dp(self)
        implicit none
        class(type_vector_dp), intent(in) :: self

        if (.not. self%is_initialized_vector) then
            write (*, *) "Error: Vector not initialized."
            return
        end if
        if (self%status == VECTOR_STATUS%SUCCESS) return
        if (self%status == VECTOR_STATUS%OUT_OF_MEMORY) then
            write (*, *) "Error: Vector operation failed due to out of memory."
        else if (self%status == VECTOR_STATUS%ILL_OPERATIONS) then
            write (*, *) "Error: Vector operation failed due to illegal operation."
        else if (self%status == VECTOR_STATUS%NOT_IMPLEMENTED) then
            write (*, *) "Error: Vector operation not implemented."
        else
            write (*, *) "Error: Vector operation failed due to unknown error."
        end if
    end subroutine check_vector_dp

end submodule algebra_vector_dp

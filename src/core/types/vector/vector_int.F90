submodule(core_types_vector) core_types_vector_int
    implicit none

contains

    !>
    !> Initializes the integer vector.
    !>
    module subroutine initialize_vector_int(self, num_nodes, num_blocks)
        implicit none
        class(type_vector_int), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        integer(int32), intent(in), optional :: num_blocks

        if (allocated(self%val)) call deallocate_array(self%val)

        self%num_nodes = num_nodes
        if (present(num_blocks)) then
            self%num_blocks = num_blocks
        else
            self%num_blocks = 1
        end if

        call allocate_array(self%val, self%num_blocks * self%num_nodes)
        self%val = 0 ! 一括初期化

        self%status = VECTOR_STATUS_SUCCESS
        self%is_initialized_vector = .true.
    end subroutine initialize_vector_int

    !>
    !> Destroys the vector.
    !>
    module subroutine destroy_vector_int(self)
        implicit none
        class(type_vector_int), intent(inout) :: self

        if (allocated(self%val)) call deallocate_array(self%val)
        self%num_nodes = 0
        self%num_blocks = 0
        self%status = VECTOR_STATUS_SUCCESS
        self%is_initialized_vector = .false.
    end subroutine destroy_vector_int

    !>
    !> Checks initialization status.
    !>
    module pure function is_initialized_vector_int(self) result(initialized)
        implicit none
        class(type_vector_int), intent(in) :: self
        logical :: initialized
        initialized = self%is_initialized_vector
    end function is_initialized_vector_int

    !>
    !> Returns size.
    !>
    module pure function get_size_vector_int(self) result(vector_size)
        implicit none
        class(type_vector_int), intent(in) :: self
        integer(int32) :: vector_size
        vector_size = self%num_nodes
    end function get_size_vector_int

    !>
    !> Returns data pointer.
    !>
    module function get_data_vector_int(self) result(data)
        implicit none
        class(type_vector_int), intent(in), target :: self
        integer(int32), pointer, dimension(:) :: data

        data => self%val
    end function get_data_vector_int

    module pure function get_status_vector_int(self) result(status)
        implicit none
        class(type_vector_int), intent(in) :: self
        integer(int32) :: status

        status = self%status
    end function get_status_vector_int

    !>
    !> Sets array values using array syntax.
    !>
    module subroutine set_array_vector_int(self, op, array_value, row_block)
        implicit none
        class(type_vector_int), intent(inout) :: self
        integer(int32), intent(in) :: op
        integer(int32), intent(in) :: array_value(:)
        integer(int32), intent(in), optional :: row_block

        integer(int32) :: nb
        nb = 1
        if (present(row_block)) nb = row_block

        if (.not. value_in_range(nb, 1_int32, self%num_blocks)) then
            self%status = VECTOR_STATUS_OUT_OF_MEMORY
            return
        end if

        if (size(array_value) /= self%num_nodes) then
            ! ここではチェックのみ行い、既存のルーチンを呼ぶ想定
            call check_match_length(real(array_value, real64), real(self%val, real64), "set_array_vector_int")
            return
        end if

        associate (val => self%val, n_blks => self%num_blocks)
            select case (op)
            case (OP_INS)
                val(nb :: n_blks) = array_value
            case (OP_ADD)
                val(nb :: n_blks) = val(nb :: n_blks) + array_value
            case default
                self%status = VECTOR_STATUS_ILL_OPERATIONS
            end select
        end associate
    end subroutine set_array_vector_int

    !>
    !> Sets value at specific index.
    !>
    module subroutine set_value_at_index_vector_int(self, op, global_index, value, row_block)
        implicit none
        class(type_vector_int), intent(inout) :: self
        integer(int32), intent(in) :: op
        integer(int32), intent(in) :: global_index
        integer(int32), intent(in) :: value
        integer(int32), intent(in), optional :: row_block

        integer(int32) :: nb, idx
        nb = 1
        if (present(row_block)) nb = row_block

#ifdef USE_DEBUG
        if (.not. value_in_range(nb, 1_int32, self%num_blocks) .or. &
            .not. value_in_range(global_index, 1_int32, self%num_nodes)) then
            self%status = VECTOR_STATUS_OUT_OF_MEMORY
            return
        end if
#endif

        idx = (global_index - 1) * self%num_blocks + nb

        select case (op)
        case (OP_INS)
            self%val(idx) = value
        case (OP_ADD)
            self%val(idx) = self%val(idx) + value
        case default
            self%status = VECTOR_STATUS_ILL_OPERATIONS
        end select
    end subroutine set_value_at_index_vector_int

    !>
    !> Sets values using vector subscripts (Scatter).
    !>
    module subroutine set_values_at_indices_vector_int(self, op, global_indices, new_values, row_block)
        implicit none
        class(type_vector_int), intent(inout) :: self
        integer(int32), intent(in) :: op
        integer(int32), intent(in) :: global_indices(:)
        integer(int32), intent(in) :: new_values(:)
        integer(int32), intent(in), optional :: row_block

        integer(int32) :: nb
        integer(int32), allocatable :: target_indices(:)

        nb = 1
        if (present(row_block)) nb = row_block

#ifdef USE_DEBUG
        if (.not. value_in_range(nb, 1_int32, self%num_blocks)) then
            self%status = VECTOR_STATUS_OUT_OF_MEMORY
            return
        end if
#endif

        call allocate_array(target_indices, size(global_indices))
        target_indices = (global_indices - 1) * self%num_blocks + nb

#ifdef USE_DEBUG
        if (any(target_indices < 1 .or. target_indices > size(self%val))) then
            self%status = VECTOR_STATUS_OUT_OF_MEMORY
            call deallocate_array(target_indices)
            return
        end if
#endif

        select case (op)
        case (OP_INS)
            self%val(target_indices) = new_values
        case (OP_ADD)
            self%val(target_indices) = self%val(target_indices) + new_values
        case default
            self%status = VECTOR_STATUS_ILL_OPERATIONS
        end select

        call deallocate_array(target_indices)
    end subroutine set_values_at_indices_vector_int

    !>
    !> Scales the vector by a scalar alpha.
    !>
    module subroutine scale_vector_int(self, op, alpha)
        implicit none
        class(type_vector_int), intent(inout) :: self
        integer(int32), intent(in) :: op
        class(type_vector_int), intent(in) :: alpha

        integer(int32) :: i
        integer(int32), dimension(:), pointer :: alpha_data

        alpha_data => alpha%get_data()

        select case (op)

        case (OP_SCALE_SYMM_DIAG, OP_SCALE_JACOBI)
            self%val = self%val * alpha_data
        case default
            self%status = VECTOR_STATUS_ILL_OPERATIONS
        end select

    end subroutine scale_vector_int

    !>
    !> Optimized Copy.
    !>
    module subroutine copy_vector_int(self, source_vector)
        implicit none
        class(type_vector_int), intent(inout) :: self
        class(type_vector_int), intent(in) :: source_vector

        if (self%is_initialized_vector .and. &
            self%num_nodes == source_vector%num_nodes .and. &
            self%num_blocks == source_vector%num_blocks) then

            self%val = source_vector%val
        else
            if (self%is_initialized_vector) call self%destroy()
            call self%initialize(source_vector%num_nodes, source_vector%num_blocks)
            self%val = source_vector%val
        end if

        self%status = VECTOR_STATUS_SUCCESS
    end subroutine copy_vector_int

    !>
    !> Zero vector.
    !>
    module subroutine zero_vector_int(self)
        implicit none
        class(type_vector_int), intent(inout) :: self

        self%val = 0
    end subroutine zero_vector_int

    !>
    !> Display with block awareness.
    !>
    module subroutine display_vector_int(self, unit_in)
        implicit none
        class(type_vector_int), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: i, b, idx
        integer(int32) :: unit

        unit = optval(unit_in, output_unit)

        if (.not. self%is_initialized_vector) then
            write (unit, '(a)') "Vector not initialized."
            return
        end if

        write (unit, '(a,i0,a,i0)') "Vector(Int) Size:", self%num_nodes, " Blocks:", self%num_blocks
        do i = 1, self%num_nodes
            do b = 1, self%num_blocks
                idx = (i - 1) * self%num_blocks + b
                write (unit, '(A,I0,A,I0,A,I12)') &
                    "Node ", i, " Block ", b, ": ", self%val(idx)
            end do
        end do
    end subroutine display_vector_int

    module subroutine check_vector_int(self)
        implicit none
        class(type_vector_int), intent(in) :: self

        select case (self%status)
        case (VECTOR_STATUS_SUCCESS)
            ! 正常
        case (VECTOR_STATUS_OUT_OF_MEMORY)
            write (*, *) "Error: Vector operation failed due to out of memory."
        case (VECTOR_STATUS_ILL_OPERATIONS)
            write (*, *) "Error: Vector operation failed due to illegal operation."
        case (VECTOR_STATUS_NOT_IMPLEMENTED)
            write (*, *) "Error: Vector operation not implemented."
        case default
            write (*, *) "Error: Vector operation failed due to unknown error."
        end select
    end subroutine check_vector_int

end submodule core_types_vector_int

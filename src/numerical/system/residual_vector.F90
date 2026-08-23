!> High-level container for the residual vector.
!> Holds a single or multiple blocked vectors (type_vector_dp) internally
!> and maps operations according to the coupling mode.
module numerical_system_residual_vector
    use, intrinsic :: iso_fortran_env
    use :: stdlib_optval, only:optval
    use :: module_core
    use :: module_linalg
    implicit none
    private

    public :: type_residual_vector

    type :: type_residual_vector
        private
        integer(int32) :: num_nodes = 0
        integer(int32) :: num_dofs_per_node = 0
        integer(int32) :: num_dofs_of_physics(PHYSICS_TYPES%NUM_ID) = 0
        integer(int32) :: size = 0

        type(type_constant_id) :: coupling_mode

        type(type_vector_dp), allocatable :: data(:)

        integer(int32) :: num_system = 0

        !> physics i -> system index (0 if inactive)
        integer(int32) :: physics_to_system(PHYSICS_TYPES%NUM_ID) = 0

        !> physics i -> block coordinate inside a monolithic node block (0 if inactive)
        integer(int32) :: physics_to_block(PHYSICS_TYPES%NUM_ID) = 0

        !> per system total dofs
        integer(int32), allocatable :: system_size(:)
    contains
        procedure, pass(self), public :: initialize => initialize_residual_vector
        procedure, pass(self), public :: destroy => destroy_residual_vector

        procedure, pass(self), public :: get_size => get_size_residual_vector
        procedure, pass(self), public :: get_num_dofs_per_node => get_num_dofs_per_node_residual_vector

        procedure, public, pass(self) :: get_vector => get_underlying_vector
        procedure, public, pass(self) :: get_data => get_data_vector

        procedure, pass(self), private :: resolve_target_vector
        procedure, pass(self), private :: set_scalar_residual_vector
        procedure, pass(self), private :: set_array_residual_vector
        procedure, pass(self), private :: set_value_at_index_residual_vector
        procedure, pass(self), private :: set_values_at_indices_residual_vector
        generic, public :: set => set_scalar_residual_vector, set_array_residual_vector, &
            set_value_at_index_residual_vector, set_values_at_indices_residual_vector

        procedure, pass(self), private :: add_value_residual_vector
        procedure, pass(self), private :: add_array_residual_vector
        procedure, pass(self), private :: add_value_at_index_residual_vector
        procedure, pass(self), private :: add_values_at_indices_residual_vector
        procedure, pass(self), private :: add_values_from_vector_residual_vector
        generic, public :: add => add_value_residual_vector, add_array_residual_vector, &
            add_value_at_index_residual_vector, add_values_at_indices_residual_vector, &
            add_values_from_vector_residual_vector

        procedure, pass(self), public :: zero => zero_residual_vector
        procedure, public, pass(self) :: copy => copy_residual_vector
        procedure, public, pass(self) :: scale => scale_residual_vector
        procedure, public, pass(self) :: display => display_residual_vector
    end type type_residual_vector

contains

    subroutine initialize_residual_vector(self, topology, coupling_mode)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        type(type_system_topology), intent(in) :: topology
        type(type_constant_id), intent(in) :: coupling_mode

        integer(int32) :: i, j

        call self%destroy()

        self%coupling_mode = coupling_mode
        self%physics_to_system(:) = 0
        self%physics_to_block(:) = 0
        self%num_dofs_of_physics(:) = 0

        call topology%get_total_dofs(self%size)
        call topology%get_num_nodes(self%num_nodes)

        do i = 1, PHYSICS_TYPES%NUM_ID
            call topology%get_start_dof_index(PHYSICS_TYPES%to_object(i), self%physics_to_block(i))
        end do

        select case (coupling_mode%ID)
        case (COUPLING_MODES%MONOLITHIC%ID)
            call topology%get_num_dof_per_node(self%num_dofs_per_node)
            self%num_system = 1

            allocate (self%system_size(1))
            self%system_size(1) = self%size

        case (COUPLING_MODES%STAGGERED%ID)
            do i = 1, PHYSICS_TYPES%NUM_ID
                call topology%get_target_dof(PHYSICS_TYPES%to_object(i), self%num_dofs_of_physics(i))
            end do

            self%num_system = count(self%num_dofs_of_physics > 0)
            allocate (self%system_size(self%num_system))
            self%num_dofs_per_node = 0
        end select

        allocate (self%data(self%num_system))

        select case (coupling_mode%ID)
        case (COUPLING_MODES%MONOLITHIC%ID)
            call self%data(1)%initialize(self%num_nodes, num_blocks=self%num_dofs_per_node)
        case (COUPLING_MODES%STAGGERED%ID)
            j = 0
            do i = 1, PHYSICS_TYPES%NUM_ID
                if (self%num_dofs_of_physics(i) == 0) cycle
                j = j + 1
                self%physics_to_system(i) = j
                self%system_size(j) = self%num_nodes * self%num_dofs_of_physics(i)
                call self%data(j)%initialize(self%num_nodes, num_blocks=self%num_dofs_of_physics(i))
            end do
        end select

    end subroutine initialize_residual_vector

    pure function get_size_residual_vector(self) result(size)
        implicit none
        class(type_residual_vector), intent(in) :: self
        integer(int32) :: size
        size = self%size
    end function get_size_residual_vector

    pure function get_num_dofs_per_node_residual_vector(self) result(num_dofs_per_node)
        implicit none
        class(type_residual_vector), intent(in) :: self
        integer(int32) :: num_dofs_per_node
        num_dofs_per_node = self%num_dofs_per_node
    end function get_num_dofs_per_node_residual_vector

    function get_underlying_vector(self, physics_id_in) result(vec)
        implicit none
        class(type_residual_vector), intent(in), target :: self
        integer(int32), intent(in), optional :: physics_id_in
        class(type_vector_dp), pointer :: vec
        integer(int32) :: sys_id, physics_id

        select case (self%coupling_mode%ID)
        case (COUPLING_MODES%MONOLITHIC%ID)
            physics_id = optval(physics_id_in, 1)
            if (physics_id < 1 .or. physics_id > PHYSICS_TYPES%NUM_ID) then
                nullify (vec)
                return
            end if
            vec => self%data(1)
        case (COUPLING_MODES%STAGGERED%ID)
            if (.not. present(physics_id_in)) then
                if (allocated(self%data) .and. size(self%data) > 0) then
                    vec => self%data(1)
                else
                    nullify (vec)
                end if
                return
            end if

            physics_id = physics_id_in
            if (physics_id < 1 .or. physics_id > PHYSICS_TYPES%NUM_ID) then
                nullify (vec)
                return
            end if
            sys_id = self%physics_to_system(physics_id)
            if (sys_id <= 0 .or. sys_id > size(self%data)) then
                nullify (vec)
                return
            end if
            vec => self%data(sys_id)
        end select

    end function get_underlying_vector

    function get_data_vector(self, physics_id_in) result(data_ptr)
        implicit none
        class(type_residual_vector), intent(in), target :: self
        integer(int32), intent(in), optional :: physics_id_in
        real(real64), pointer, dimension(:) :: data_ptr
        class(type_vector_dp), pointer :: vec

        vec => self%get_vector(physics_id_in)
        if (associated(vec)) then
            data_ptr => vec%get_data()
        else
            nullify (data_ptr)
        end if
    end function get_data_vector

    !> Resolve the target vector pointer and the effective block index for a
    !> given physics id, encapsulating the coupling-mode logic in one place.
    !> Returns a nullified pointer when the physics is out of range or inactive.
    subroutine resolve_target_vector(self, physics_id, vec, row_block)
        implicit none
        class(type_residual_vector), intent(in), target :: self
        integer(int32), intent(in) :: physics_id
        class(type_vector_dp), pointer, intent(inout) :: vec
        integer(int32), intent(inout) :: row_block

        vec => self%get_vector(physics_id)

        select case (self%coupling_mode%ID)
        case (COUPLING_MODES%MONOLITHIC%ID)
            if (physics_id < 1 .or. physics_id > PHYSICS_TYPES%NUM_ID) then
                nullify (vec)
                return
            end if
            row_block = self%physics_to_block(physics_id)
            if (row_block <= 0) nullify (vec)
        case (COUPLING_MODES%STAGGERED%ID)
            row_block = 1
        end select
    end subroutine resolve_target_vector

    subroutine set_scalar_residual_vector(self, physics_id, value)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: physics_id
        real(real64), intent(in) :: value
        class(type_vector_dp), pointer :: vec
        integer(int32) :: row_block

        call self%resolve_target_vector(physics_id, vec, row_block)
        if (.not. associated(vec)) return
        call vec%set(VECTOR_OPS%INS, value, row_block=row_block)
    end subroutine set_scalar_residual_vector

    subroutine set_array_residual_vector(self, physics_id, values)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: physics_id
        real(real64), intent(in) :: values(:)
        class(type_vector_dp), pointer :: vec
        integer(int32) :: row_block

        call self%resolve_target_vector(physics_id, vec, row_block)
        if (.not. associated(vec)) return
        call vec%set(VECTOR_OPS%INS, values, row_block=row_block)
    end subroutine set_array_residual_vector

    subroutine set_value_at_index_residual_vector(self, physics_id, global_index, value)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: physics_id
        integer(int32), intent(in) :: global_index
        real(real64), intent(in) :: value
        class(type_vector_dp), pointer :: vec
        integer(int32) :: row_block

        call self%resolve_target_vector(physics_id, vec, row_block)
        if (.not. associated(vec)) return
        call vec%set(VECTOR_OPS%INS, global_index, value, row_block=row_block)
    end subroutine set_value_at_index_residual_vector

    subroutine set_values_at_indices_residual_vector(self, physics_id, global_indices, values)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: physics_id
        integer(int32), intent(in) :: global_indices(:)
        real(real64), intent(in) :: values(:)
        class(type_vector_dp), pointer :: vec
        integer(int32) :: row_block

        call self%resolve_target_vector(physics_id, vec, row_block)
        if (.not. associated(vec)) return
        call vec%set(VECTOR_OPS%INS, global_indices, values, row_block=row_block)
    end subroutine set_values_at_indices_residual_vector

    subroutine add_value_residual_vector(self, physics_id, value)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: physics_id
        real(real64), intent(in) :: value
        class(type_vector_dp), pointer :: vec
        integer(int32) :: row_block

        call self%resolve_target_vector(physics_id, vec, row_block)
        if (.not. associated(vec)) return
        call vec%set(VECTOR_OPS%ADD, value, row_block=row_block)
    end subroutine add_value_residual_vector

    subroutine add_array_residual_vector(self, physics_id, values)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: physics_id
        real(real64), intent(in) :: values(:)
        class(type_vector_dp), pointer :: vec
        integer(int32) :: row_block

        call self%resolve_target_vector(physics_id, vec, row_block)
        if (.not. associated(vec)) return
        call vec%set(VECTOR_OPS%ADD, values, row_block=row_block)
    end subroutine add_array_residual_vector

    subroutine add_value_at_index_residual_vector(self, physics_id, global_index, value)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: physics_id
        integer(int32), intent(in) :: global_index
        real(real64), intent(in) :: value
        class(type_vector_dp), pointer :: vec
        integer(int32) :: row_block

        call self%resolve_target_vector(physics_id, vec, row_block)
        if (.not. associated(vec)) return
        call vec%set(VECTOR_OPS%ADD, global_index, value, row_block=row_block)
    end subroutine add_value_at_index_residual_vector

    subroutine add_values_at_indices_residual_vector(self, physics_id, global_indices, values)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: physics_id
        integer(int32), intent(in) :: global_indices(:)
        real(real64), intent(in) :: values(:)
        class(type_vector_dp), pointer :: vec
        integer(int32) :: row_block

        call self%resolve_target_vector(physics_id, vec, row_block)
        if (.not. associated(vec)) return
        call vec%set(VECTOR_OPS%ADD, global_indices, values, row_block=row_block)
    end subroutine add_values_at_indices_residual_vector

    subroutine add_values_from_vector_residual_vector(self, physics_id, global_indices, values)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32), intent(in) :: physics_id
        integer(int32), intent(in) :: global_indices(:)
        type(type_vector_dp), intent(in) :: values
        real(real64), pointer, dimension(:) :: vals_data
        class(type_vector_dp), pointer :: vec
        integer(int32) :: row_block

        vals_data => values%get_data()
        if (.not. associated(vals_data)) return

        call self%resolve_target_vector(physics_id, vec, row_block)
        if (.not. associated(vec)) return
        call vec%set(VECTOR_OPS%ADD, global_indices, vals_data, row_block=row_block)
    end subroutine add_values_from_vector_residual_vector

    subroutine scale_residual_vector(self, alpha)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        real(real64), intent(in) :: alpha
        real(real64), pointer :: raw_data(:)
        integer(int32) :: i

        if (.not. allocated(self%data)) return

        do i = 1, size(self%data)
            if (.not. self%data(i)%is_initialized()) cycle
            raw_data => self%data(i)%get_data()
            if (associated(raw_data)) raw_data = raw_data * alpha
        end do
    end subroutine scale_residual_vector

    subroutine copy_residual_vector(self, source_vector)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        class(type_residual_vector), intent(in) :: source_vector
        integer(int32) :: i

        if (.not. allocated(self%data) .or. .not. allocated(source_vector%data)) return

        do i = 1, min(size(self%data), size(source_vector%data))
            if (.not. source_vector%data(i)%is_initialized()) cycle
            call self%data(i)%copy(source_vector%data(i))
        end do
    end subroutine copy_residual_vector

    subroutine zero_residual_vector(self)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32) :: i

        if (allocated(self%data)) then
            do i = 1, size(self%data)
                if (self%data(i)%is_initialized()) call self%data(i)%zero()
            end do
        end if
    end subroutine zero_residual_vector

    subroutine destroy_residual_vector(self)
        implicit none
        class(type_residual_vector), intent(inout) :: self
        integer(int32) :: i

        if (allocated(self%data)) then
            do i = 1, size(self%data)
                call self%data(i)%destroy()
            end do
            deallocate (self%data)
        end if

        call deallocate_array(self%system_size)

        self%size = 0
        self%num_nodes = 0
        self%num_dofs_per_node = 0
        self%num_system = 0
        self%physics_to_system(:) = 0
        self%num_dofs_of_physics(:) = 0
    end subroutine destroy_residual_vector

    subroutine display_residual_vector(self, unit_in)
        implicit none
        class(type_residual_vector), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in
        integer(int32) :: unit, i

        unit = optval(unit_in, output_unit)

        write (unit, '(A)') '--- Residual Vector (Blocked) ---'
        write (unit, '(A, I0)') 'Size (total DOFs): ', self%size
        write (unit, '(A, I0)') 'Number of DOFs per Node: ', self%num_dofs_per_node

        if (allocated(self%data)) then
            do i = 1, size(self%data)
                write (unit, '(A, I0)') 'System ID: ', i
                if (self%data(i)%is_initialized()) then
                    call self%data(i)%display(unit)
                else
                    write (unit, '(A)') '  [Not initialized]'
                end if
            end do
        else
            write (unit, '(A)') '  [Matrix not allocated]'
        end if
        write (unit, '(A)') '---------------------------------'
    end subroutine display_residual_vector

end module numerical_system_residual_vector

module core_types_discretization_dof_map
    use, intrinsic :: iso_fortran_env
    use :: stdlib_optval, only:optval
    use :: core_constants, only:type_constant_id, PHYSICS_TYPES
    implicit none
    private

    public :: type_dof_map

    !>
    !> Stores the mapping and layout of degrees of freedom (DOF) per node.
    !>
    type :: type_dof_map
        !> Total number of degrees of freedom per node for the active physics.
        integer(int32), private :: num_dofs_per_node = 0
        !> Number of DOFs for each individual physics type.
        integer(int32), private :: num_dofs_of_physics(PHYSICS_TYPES%NUM_ID) = 0
        !> The starting index for each physics DOFs within the block of DOFs for a single node.
        integer(int32), private :: start_dof_index(PHYSICS_TYPES%NUM_ID) = 0
    contains
        ! ---- Lifecycle ----
        procedure, public, pass(self) :: initialize => initialize_type_dof_map
        procedure, public, pass(self) :: destroy => destroy_type_dof_map
        ! ---- Getter ----
        procedure, public, pass(self) :: get_num_dofs_per_node => get_num_dofs_per_node_dof_map
        procedure, public, pass(self) :: get_num_dofs_of_physics => get_num_dofs_of_physics_dof_map
        procedure, public, pass(self) :: get_start_dof_index => get_start_dof_index_dof_map

        ! ---- Meta / Utility ----
        procedure, public, pass(self) :: display => display_dof_map
    end type type_dof_map

contains
    subroutine initialize_type_dof_map(self, active_dofs)
        implicit none
        class(type_dof_map), intent(inout) :: self
        logical, intent(in) :: active_dofs(:)

        integer(int32) :: current_dof_index, slot, physics_id

        ! Block layout order, deliberately not physics-id order: the coupled
        ! THM unknowns stay contiguous even when mechanics is active.
        integer(int32), parameter :: LAYOUT_ORDER(4) = [ &
                                     PHYSICS_TYPES%THERMAL%ID, &
                                     PHYSICS_TYPES%HYDRAULIC%ID, &
                                     PHYSICS_TYPES%PNEUMATIC%ID, &
                                     PHYSICS_TYPES%MECHANICAL%ID]
        integer(int32), parameter :: LAYOUT_WIDTH(4) = [1, 1, 1, 3]

        self%start_dof_index(:) = 0
        self%num_dofs_of_physics(:) = 0

        current_dof_index = 1
        do slot = 1, size(LAYOUT_ORDER)
            physics_id = LAYOUT_ORDER(slot)
            if (physics_id > size(active_dofs)) cycle
            if (.not. active_dofs(physics_id)) cycle
            self%num_dofs_of_physics(physics_id) = LAYOUT_WIDTH(slot)
            self%start_dof_index(physics_id) = current_dof_index
            current_dof_index = current_dof_index + LAYOUT_WIDTH(slot)
        end do
        self%num_dofs_per_node = current_dof_index - 1
    end subroutine initialize_type_dof_map

    subroutine destroy_type_dof_map(self)
        implicit none
        class(type_dof_map), intent(inout) :: self

        self%num_dofs_per_node = 0
        self%num_dofs_of_physics = 0
        self%start_dof_index = 0
    end subroutine destroy_type_dof_map

    subroutine get_num_dofs_per_node_dof_map(self, num_dofs_per_node)
        implicit none
        class(type_dof_map), intent(in) :: self
        integer(int32), intent(out) :: num_dofs_per_node

        num_dofs_per_node = self%num_dofs_per_node
    end subroutine get_num_dofs_per_node_dof_map

    subroutine get_num_dofs_of_physics_dof_map(self, physics_id, num_dof)
        implicit none
        class(type_dof_map), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_id
        integer(int32), intent(out) :: num_dof

        if (.not. PHYSICS_TYPES%is_valid(physics_id)) then
            num_dof = 0
            return
        end if

        num_dof = self%num_dofs_of_physics(physics_id%ID)

    end subroutine get_num_dofs_of_physics_dof_map

    subroutine get_start_dof_index_dof_map(self, physics_id, start_dof_index)
        implicit none
        class(type_dof_map), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_id
        integer(int32), intent(out) :: start_dof_index

        if (.not. PHYSICS_TYPES%is_valid(physics_id)) then
            start_dof_index = 0
            return
        end if

        start_dof_index = self%start_dof_index(physics_id%ID)

    end subroutine get_start_dof_index_dof_map

    subroutine display_dof_map(self, unit_in)
        implicit none
        class(type_dof_map), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: unit

        unit = optval(unit_in, output_unit)

        write (unit, '(A)') '### DOF Map'
        write (unit, '(A)')
        write (unit, '(A, I0)') '  - **Total DOFs per Node**: ', self%num_dofs_per_node
        write (unit, '(A)')
    end subroutine display_dof_map

end module core_types_discretization_dof_map

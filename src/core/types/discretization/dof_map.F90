module types_discretization_dof_map
    use, intrinsic :: iso_fortran_env
    use :: stdlib_optval, only:optval
    use :: core_constants, only:PHYSICS_TYPES
    implicit none
    private

    public :: type_dof_map

    !>
    !> Stores the mapping and layout of degrees of freedom (DOF) per node.
    !>
    type :: type_dof_map
        !> Total number of degrees of freedom per node for the active physics.
        integer(int32) :: num_dof_per_node = 0
        !> Number of DOFs for each individual physics type.
        integer(int32) :: num_dof_of_physics(PHYSICS_TYPES%NUM_ID) = 0
        !> The starting index for each physics' DOFs within the block of DOFs for a single node.
        integer(int32) :: start_dof_index(PHYSICS_TYPES%NUM_ID) = 0
    contains
        procedure, public, pass(self) :: initialize => initialize_type_dof_map
        procedure, public, pass(self) :: display => display_dof_map
    end type type_dof_map

contains
    subroutine initialize_type_dof_map(self, active_dofs)
        implicit none
        class(type_dof_map), intent(inout) :: self
        logical, intent(in) :: active_dofs(:)

        integer(int32) :: current_dof_index

        self%num_dof_of_physics(PHYSICS_TYPES%THERMAL%ID) = 1
        self%num_dof_of_physics(PHYSICS_TYPES%HYDRAULIC%ID) = 1
        self%num_dof_of_physics(PHYSICS_TYPES%MECHANICAL%ID) = 3

        current_dof_index = 1
        if (active_dofs(PHYSICS_TYPES%THERMAL%ID)) then
            self%start_dof_index(PHYSICS_TYPES%THERMAL%ID) = current_dof_index
            current_dof_index = current_dof_index + self%num_dof_of_physics(PHYSICS_TYPES%THERMAL%ID)
        end if
        if (active_dofs(PHYSICS_TYPES%HYDRAULIC%ID)) then
            self%start_dof_index(PHYSICS_TYPES%HYDRAULIC%ID) = current_dof_index
            current_dof_index = current_dof_index + self%num_dof_of_physics(PHYSICS_TYPES%HYDRAULIC%ID)
        end if
        if (active_dofs(PHYSICS_TYPES%MECHANICAL%ID)) then
            self%start_dof_index(PHYSICS_TYPES%MECHANICAL%ID) = current_dof_index
            current_dof_index = current_dof_index + self%num_dof_of_physics(PHYSICS_TYPES%MECHANICAL%ID)
        end if
        self%num_dof_per_node = current_dof_index - 1
    end subroutine initialize_type_dof_map

    subroutine display_dof_map(self, unit_in)
        implicit none
        class(type_dof_map), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: unit

        unit = optval(unit_in, output_unit)

        write (unit, '(A)') '### DOF Map'
        write (unit, '(A)')
        write (unit, '(A, I0)') '  - **Total DOFs per Node**: ', self%num_dof_per_node
        write (unit, '(A)')
    end subroutine display_dof_map

end module types_discretization_dof_map

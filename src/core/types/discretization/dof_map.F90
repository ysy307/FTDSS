module types_discretization_dof_map
    use, intrinsic :: iso_fortran_env
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
        procedure, public, pass(self) :: display => display_dof_map
    end type type_dof_map

contains

    subroutine display_dof_map(self)
        implicit none
        class(type_dof_map), intent(in) :: self
        write (*, '(A)') '### DOF Map'
        write (*, '(A)')
        write (*, '(A, I0)') '  - **Total DOFs per Node**: ', self%num_dof_per_node
        write (*, '(A)')
    end subroutine display_dof_map

end module types_discretization_dof_map

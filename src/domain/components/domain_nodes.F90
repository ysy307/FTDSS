module domain_components_nodes
    use, intrinsic :: iso_fortran_env
    use :: stdlib_optval, only:optval
    use :: module_core
    implicit none
    private

    public :: type_nodes_manager

    !>
    !> Manages all data related to nodes (points) in the domain.
    !>
    type :: type_nodes_manager
        ! !> Pointer to the parent domain object.
        ! type(type_domain), pointer, private :: parent => null()
        !> Number of nodes in this subdomain.
        integer(int32), private :: num_nodes = 0
        !> Nodal coordinates. Size: (computation_dimension, num_nodes).
        real(real64), private, allocatable :: coordinates(:, :)
        !> Global ID for each node in this subdomain.
        integer(int32), private, allocatable :: global_ids(:)
    contains
        ! ---- Lifecycle ----
        ! initialize, destroy, reset, etc.
        procedure, public, pass(self) :: initialize => initialize_nodes_manager
        procedure, public, pass(self) :: destroy => destroy_nodes_manager

        ! ---- Mutator ----
        ! set_XXX, increment_XXX, update_XXX, etc.

        ! ---- Algorithm / Operation ----
        ! compute_XXX, check_XXX, solve_XXX, etc.

        ! ---- Inquiry ----
        ! is_XXX, has_XXX, should_XXX, etc.

        ! ---- Getter ----
        ! get_XXX, etc.
        procedure, public, pass(self) :: get_num_nodes => get_num_nodes_nodes_manager
        procedure, public, pass(self) :: get_dimension => get_dimension_nodes_manager
        generic, public :: get_coordinate => get_coordinate_nodes_manager, &
            get_coordinates_nodes_manager
        procedure, private, pass(self) :: get_coordinate_nodes_manager
        procedure, private, pass(self) :: get_coordinates_nodes_manager

        ! ---- Meta / Utility ----
        procedure, public, pass(self) :: display => display_nodes_manager

        ! ---- Operator ----
    end type type_nodes_manager

contains
    !> Initializes the node manager by reading data from the input object.
    subroutine initialize_nodes_manager(self, config)
        implicit none
        class(type_nodes_manager), intent(inout) :: self
        type(type_config_nodes), intent(in) :: config

        self%num_nodes = config%num_nodes

        call allocate_array(self%coordinates, source=config%points)
        call allocate_array(self%global_ids, source=config%global_node_ids)

    end subroutine initialize_nodes_manager

    !> Destroys the node manager by deallocating arrays and resetting values.
    subroutine destroy_nodes_manager(self)
        implicit none
        class(type_nodes_manager), intent(inout) :: self

        self%num_nodes = 0
        call deallocate_array(self%coordinates)
        call deallocate_array(self%global_ids)

    end subroutine destroy_nodes_manager

    subroutine get_num_nodes_nodes_manager(self, num_nodes)
        implicit none
        class(type_nodes_manager), intent(in) :: self
        integer(int32), intent(inout) :: num_nodes

        num_nodes = self%num_nodes
    end subroutine get_num_nodes_nodes_manager

    subroutine get_dimension_nodes_manager(self, dimension)
        implicit none
        class(type_nodes_manager), intent(in) :: self
        integer(int32), intent(inout) :: dimension

        if (allocated(self%coordinates)) then
            dimension = size(self%coordinates, 1)
        else
            dimension = 0
        end if
    end subroutine get_dimension_nodes_manager

    subroutine get_coordinate_nodes_manager(self, node_id, coords)
        implicit none
        class(type_nodes_manager), intent(in) :: self
        integer(int32), intent(in) :: node_id
        real(real64), intent(inout) :: coords(:)

        coords = self%coordinates(:, node_id)
    end subroutine get_coordinate_nodes_manager

    subroutine get_coordinates_nodes_manager(self, node_ids, coords)
        implicit none
        class(type_nodes_manager), intent(in) :: self
        integer(int32), intent(in) :: node_ids(:)
        real(real64), intent(inout) :: coords(:, :)

        coords = self%coordinates(:, node_ids)
    end subroutine get_coordinates_nodes_manager

    subroutine display_nodes_manager(self, unit_in)
        implicit none
        class(type_nodes_manager), intent(in) :: self
        integer(int32), intent(in) :: unit_in

        integer(int32) :: unit
        unit = optval(unit_in, output_unit)

        write (unit, '(A)') '### Nodes Manager'
        write (unit, '(A)')
        write (unit, '(A, I0)') '  - **Number of Nodes**: ', self%num_nodes
        write (unit, '(A)')
    end subroutine display_nodes_manager

end module domain_components_nodes

module components_domain_nodes
    use, intrinsic :: iso_fortran_env
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
        integer(int32) :: num_nodes = 0
        !> Nodal coordinates. Size: (computation_dimension, num_nodes).
        real(real64), allocatable :: coordinates(:, :)
        !> Global ID for each node in this subdomain.
        integer(int32), allocatable :: global_ids(:)
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

        ! ---- Meta / Utility ----
        ! display, to_string, etc.
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

    subroutine display_nodes_manager(self)
        implicit none
        class(type_nodes_manager), intent(in) :: self

        write (*, '(A)') '### Nodes Manager'
        write (*, '(A)')
        write (*, '(A, I0)') '  - **Number of Nodes**: ', self%num_nodes
        write (*, '(A)')
    end subroutine display_nodes_manager

end module components_domain_nodes

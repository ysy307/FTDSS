module components_domain_boundaries
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: domain_fe_manager, only:type_fe_manager
    implicit none
    private

    public :: type_boundaries_manager

    !>
    !> Represents a single, unique boundary condition applied to a set of geometric entities.
    !>
    type :: type_boundary_patch
        !> The integer ID representing the type of boundary condition (e.g., Dirichlet, Neumann).
        integer(int32) :: type_id = -1
        !> The number of elements (sides) this boundary condition applies to.
        integer(int32) :: num_fe = 0
        !> Array of finite element type IDs for each element in this BC set.
        integer(int32), allocatable :: fe_types(:)
        !> Manager for FE type-specific operations (shape functions, etc.).
        type(type_fe_manager) :: fe_manager
        !> Connectivity data for the elements in this BC set.
        type(type_csr_index) :: connectivity
    contains
        ! procedure, public, pass(self) :: display => display_boundary_patch
    end type type_boundary_patch

    !>
    !> Manages all boundary conditions for a single physics type.
    !>
    type :: type_physics_bc_manager
        !> The number of unique boundary conditions for this physics.
        integer(int32) :: num_bcs = 0
        !> Array of unique boundary condition sets.
        type(type_boundary_patch), allocatable :: bcs(:)
    contains
        ! procedure, public, pass(self) :: display => display_physics_bc_manager
    end type type_physics_bc_manager

    !>
    !> Top-level manager for all boundary conditions across all physics types.
    !>
    type :: type_boundaries_manager
        ! !> Pointer to the parent domain object.
        ! type(type_domain), pointer, private :: parent => null()
        !> Array of BC managers, one for each physics type.
        type(type_physics_bc_manager) :: physics(PHYSICS_TYPES%NUM_ID)
    contains
        ! procedure, public, pass(self) :: initialize => initialize_boundary_manager
        ! procedure, private, pass(self) :: process_single_physics_bcs
        ! procedure, private, pass(self) :: filter_active_bcs
        ! procedure, private, pass(self) :: create_entity_id_to_group_map
        ! procedure, private, pass(self) :: measure_and_allocate_bc_geometry
        ! procedure, private, pass(self) :: store_bc_geometry
        ! procedure, private, pass(self) :: create_bc_instances
        ! procedure, public, pass(self) :: display => display_boundary_manager
    end type type_boundaries_manager

end module components_domain_boundaries

!>
!> Manages the computational domain, including mesh, boundary conditions, and parallel data.
!>
module domain_manager
    use, intrinsic :: iso_fortran_env
    use :: mpi_f08
    use :: stdlib_logger
    use :: stdlib_strings, only:strip
    use :: module_core
    use :: module_input, only:type_input
    use :: module_control, only:type_controls
    use :: module_fe, only:type_fe_manager, abst_fe
    use :: module_boundary
    use :: domain_multicoloring, only:type_coloring
    use :: domain_adjacency, only:type_node_adjacency, type_map_node_to_element

    implicit none
    private

    public :: type_domain

    !>
    !> Stores element connectivity in Compressed Sparse Row (CSR) format.
    !>
    type :: type_fe_connectivity
        !> Index array for CSR format. Stores the starting position of each element's nodes in 'val'.
        !> Size is (num_elements + 1).
        integer(int32), allocatable :: ind(:)
        !> Value array for CSR format. Stores the concatenated node IDs for all elements.
        integer(int32), allocatable :: val(:)
    contains
        procedure, public, pass(self) :: display => display_connectivity
    end type type_fe_connectivity

    !>
    !> Represents a single, unique boundary condition applied to a set of geometric entities.
    !>
    type :: type_boundary_patch
        !> The integer ID representing the type of boundary condition (e.g., Dirichlet, Neumann).
        integer(int32) :: type_id = -1
        !> The number of elements (sides) this boundary condition applies to.
        integer(int32) :: num_elements = 0
        !> Array of finite element type IDs for each element in this BC set.
        integer(int32), allocatable :: element_types(:)
        !> Manager for FE type-specific operations (shape functions, etc.).
        type(type_fe_manager) :: fe_manager
        !> Connectivity data for the elements in this BC set.
        type(type_fe_connectivity) :: connectivity
        !> The polymorphic boundary condition logic.
        class(abst_bc), allocatable :: condition
    contains
        procedure, public, pass(self) :: display => display_boundary_patch
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
        procedure, public, pass(self) :: display => display_physics_bc_manager
    end type type_physics_bc_manager

    !>
    !> Top-level manager for all boundary conditions across all physics types.
    !>
    type :: type_boundary_manager
        !> Pointer to the parent domain object.
        type(type_domain), pointer, private :: parent => null()
        !> Array of BC managers, one for each physics type.
        type(type_physics_bc_manager) :: physics(NUM_PHYSICS_TYPES)
    contains
        procedure, public, pass(self) :: initialize => initialize_boundary_manager
        procedure, private, pass(self) :: process_single_physics_bcs
        procedure, private, pass(self) :: filter_active_bcs
        procedure, private, pass(self) :: create_entity_id_to_group_map
        procedure, private, pass(self) :: measure_and_allocate_bc_geometry
        procedure, private, pass(self) :: store_bc_geometry
        procedure, private, pass(self) :: create_bc_instances
        procedure, public, pass(self) :: display => display_boundary_manager
    end type type_boundary_manager

    !>
    !> Stores the mapping and layout of degrees of freedom (DOF) per node.
    !>
    type :: type_dof_map
        !> Total number of degrees of freedom per node for the active physics.
        integer(int32) :: num_dof_per_node = 0
        !> Number of DOFs for each individual physics type.
        integer(int32) :: num_dof_of_physics(NUM_PHYSICS_TYPES) = 0
        !> The starting index for each physics' DOFs within the block of DOFs for a single node.
        integer(int32) :: start_dof_index(NUM_PHYSICS_TYPES) = 0
    contains
        procedure, public, pass(self) :: display => display_dof_map
    end type type_dof_map

    !>
    !> Manages all data related to nodes (points) in the domain.
    !>
    type :: type_node_manager
        !> Pointer to the parent domain object.
        type(type_domain), pointer, private :: parent => null()
        !> Number of nodes in this subdomain.
        integer(int32) :: num_nodes = 0
        !> Nodal coordinates. Size: (computation_dimension, num_nodes).
        real(real64), allocatable :: coordinates(:, :)
        !> Global ID for each node in this subdomain.
        integer(int32), allocatable :: node_global_ids(:)
    contains
        procedure, public, pass(self) :: initialize => initialize_node_manager
        procedure, public, pass(self) :: display => display_node_manager
    end type type_node_manager

    !>
    !> Manages all data related to volume elements in the domain.
    !>
    type :: type_element_manager
        !> Pointer to the parent domain object.
        type(type_domain), pointer, private :: parent => null()
        !> Number of elements in this subdomain.
        integer(int32) :: num_elements = 0
        !> Finite element type ID for each element.
        integer(int32), allocatable :: fe_types(:)
        !> Material ID for each element.
        integer(int32), allocatable :: fe_material_ids(:)
        !> Manager for FE type-specific operations.
        type(type_fe_manager) :: fe_manager
        !> Connectivity data for all elements.
        type(type_fe_connectivity) :: connectivity
        !> Coloring information for parallel element processing.
        type(type_coloring) :: colors
    contains
        procedure, public, pass(self) :: initialize => initialize_element_manager
        procedure, public, pass(self) :: display => display_element_manager
    end type type_element_manager

    !>
    !> The main container for all simulation domain data.
    !>
    type :: type_domain
        !> MPI rank of the current process.
        integer(int32) :: my_rank = -1
        !> Total number of MPI processes.
        integer(int32) :: num_procs = -1
        !> The spatial dimension of the computation (e.g., 2 for 2D, 3 for 3D).
        integer(int32) :: computation_dimension
        !> The type of computation (e.g., 1 for XY-plane, 2 for XZ-plane, 3 for 3D).
        integer(int32), private :: computation_type
        !> The type of coupling (e.g., staggered or monolithic).
        integer(int32) :: coupling_mode
        !> Manages the degree of freedom layout.
        type(type_dof_map) :: dof_map
        !> Manages all nodal data.
        type(type_node_manager) :: nodes
        !> Manages all element data.
        type(type_element_manager) :: elements
        !> Node adjacency information for all nodes in the domain.
        type(type_node_adjacency) :: node_adjacency
        !> Element-to-node adjacency information.
        type(type_map_node_to_element) :: element_adjacency
        !> Manages all boundary condition data.
        type(type_boundary_manager) :: boundaries
        !> Indicates whether the domain is associated with a parent.
        logical, private :: is_associated = .false.
    contains
        procedure, public, pass(self) :: initialize => initialize_type_domain
        procedure, private, pass(self) :: associate_parent
        procedure, private, pass(self) :: set_basic_info_and_dof_map
        procedure, public, pass(self) :: get_num_nodes => get_num_nodes_domain
        procedure, public, pass(self) :: get_num_elements => get_num_elements_domain
        procedure, public, pass(self) :: get_num_dofs_per_node => get_num_dofs_per_node_domain
        procedure, public, pass(self) :: get_total_dofs => get_total_dofs_domain
        procedure, public, pass(self) :: get_computation_dimension => get_computation_dimension_domain
        procedure, public, pass(self) :: get_computation_type => get_computation_type_domain
        procedure, public, pass(self) :: get_coupling_mode => get_coupling_mode_domain
        procedure, public, pass(self) :: get_node_adjacency => get_node_adjacency_domain
        procedure, public, pass(self) :: get_element => get_element_domain
        procedure, public, pass(self) :: get_element_connectivity => get_element_connectivity_domain
        procedure, public, pass(self) :: get_element_coordinate => get_element_coordinate_domain
        procedure, public, pass(self) :: get_geometry => get_geometry_domain
        procedure, public, pass(self) :: get_target_dof => get_target_dof_domain
        procedure, public, pass(self) :: get_material_id => get_material_id_domain

        procedure, private, pass(self) :: lerp_1d_domain
        procedure, private, pass(self) :: lerp_2d_domain
        procedure, private, pass(self) :: lerp_3d_domain
        generic, public :: lerp => lerp_1d_domain, lerp_2d_domain, lerp_3d_domain
        procedure, public, pass(self) :: display => display_domain
    end type type_domain

contains

    !> Initializes the entire domain object and its components.
    subroutine initialize_type_domain(self, input, controls)
        implicit none
        class(type_domain), intent(inout) :: self
        type(type_input), intent(in) :: input
        type(type_controls), intent(in) :: controls

        if (.not. self%is_associated) call self%associate_parent(self%nodes, self%elements, self%boundaries)
        call self%set_basic_info_and_dof_map(input)

        call self%nodes%initialize(input)
        call self%elements%initialize(input)
        call self%node_adjacency%initialize(self%nodes%num_nodes, self%elements%connectivity%ind, self%elements%connectivity%val)
        call self%element_adjacency%initialize(self%nodes%num_nodes, self%elements%num_elements, &
                                               self%elements%connectivity%ind, self%elements%connectivity%val)
        call self%boundaries%initialize(input, controls)
    end subroutine initialize_type_domain

    !> Associates child manager components with this parent domain object.
    subroutine associate_parent(self, node, element, boundary)
        implicit none
        class(type_domain), intent(inout), target :: self
        class(type_node_manager), intent(inout) :: node
        class(type_element_manager), intent(inout) :: element
        class(type_boundary_manager), intent(inout) :: boundary

        node%parent => self
        element%parent => self
        boundary%parent => self
        self%is_associated = .true.
    end subroutine associate_parent

    !> Sets basic simulation info and configures the DOF map based on input settings.
    subroutine set_basic_info_and_dof_map(self, input)
        implicit none
        class(type_domain), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32) :: current_dof_index

        call MPI_Comm_rank(MPI_COMM_WORLD, self%my_rank)
        call MPI_Comm_size(MPI_COMM_WORLD, self%num_procs)
        self%computation_dimension = input%basic%simulation_settings%calculate_dimension
        self%computation_type = input%basic%simulation_settings%calculate_type

        self%dof_map%num_dof_of_physics(PHYSICS_TYPE_THERMAL) = 1
        self%dof_map%num_dof_of_physics(PHYSICS_TYPE_HYDRAULIC) = 1
        self%dof_map%num_dof_of_physics(PHYSICS_TYPE_MECHANICAL) = self%computation_dimension

        current_dof_index = 1
        if (input%basic%analysis_controls%is_active(PHYSICS_TYPE_THERMAL)) then
            self%dof_map%start_dof_index(PHYSICS_TYPE_THERMAL) = current_dof_index
            current_dof_index = current_dof_index + self%dof_map%num_dof_of_physics(PHYSICS_TYPE_THERMAL)
        end if
        if (input%basic%analysis_controls%is_active(PHYSICS_TYPE_HYDRAULIC)) then
            self%dof_map%start_dof_index(PHYSICS_TYPE_HYDRAULIC) = current_dof_index
            current_dof_index = current_dof_index + self%dof_map%num_dof_of_physics(PHYSICS_TYPE_HYDRAULIC)
        end if
        if (input%basic%analysis_controls%is_active(PHYSICS_TYPE_MECHANICAL)) then
            self%dof_map%start_dof_index(PHYSICS_TYPE_MECHANICAL) = current_dof_index
            current_dof_index = current_dof_index + self%dof_map%num_dof_of_physics(PHYSICS_TYPE_MECHANICAL)
        end if
        self%dof_map%num_dof_per_node = current_dof_index - 1

        select case (input%basic%analysis_controls%coupling_mode)
        case (COUPLING_MODE_STAGGERED)
            self%coupling_mode = COUPLING_MODE_STAGGERED
        case (COUPLING_MODE_MONOLITHIC)
            self%coupling_mode = COUPLING_MODE_MONOLITHIC
        end select
    end subroutine set_basic_info_and_dof_map

    !> Initializes the node manager by reading data from the input object.
    subroutine initialize_node_manager(self, input)
        implicit none
        class(type_node_manager), intent(inout) :: self
        type(type_input), intent(in) :: input

        self%num_nodes = input%geometry%vtk%num_points

        if (allocated(self%coordinates)) deallocate (self%coordinates)
        allocate (self%coordinates(self%parent%computation_dimension, self%num_nodes))

        select case (self%parent%computation_type)
        case (COMP_TYPE_2D_XY)
            self%coordinates(1, :) = input%geometry%vtk%points%x(1:self%num_nodes)
            self%coordinates(2, :) = input%geometry%vtk%points%y(1:self%num_nodes)
        case (COMP_TYPE_2D_XZ)
            self%coordinates(1, :) = input%geometry%vtk%points%x(1:self%num_nodes)
            self%coordinates(2, :) = input%geometry%vtk%points%z(1:self%num_nodes)
        case (COMP_TYPE_3D)
            self%coordinates(1, :) = input%geometry%vtk%points%x(1:self%num_nodes)
            self%coordinates(2, :) = input%geometry%vtk%points%y(1:self%num_nodes)
            self%coordinates(3, :) = input%geometry%vtk%points%z(1:self%num_nodes)
        end select

        if (allocated(self%node_global_ids)) deallocate (self%node_global_ids)
        call allocate_array(self%node_global_ids, self%num_nodes)
        self%node_global_ids(:) = input%geometry%vtk%global_node_ids(1:self%num_nodes)
    end subroutine initialize_node_manager

    !> Initializes the element manager by reading and organizing element data.
    subroutine initialize_element_manager(self, input)
        implicit none
        class(type_element_manager), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32) :: i, ind, num_total_cells, num_total_connectivity

        num_total_cells = input%geometry%vtk%num_total_cells

        ! Pass 1: Count elements and total connectivity size
        self%num_elements = 0
        num_total_connectivity = 0
        do i = 1, num_total_cells
            if (input%geometry%vtk%cells(i)%get_dimension() == self%parent%computation_dimension) then
                self%num_elements = self%num_elements + 1
                num_total_connectivity = num_total_connectivity + input%geometry%vtk%cells(i)%num_nodes_in_cell
            end if
        end do

        ! Allocation with safe cleanup
        if (allocated(self%fe_types)) deallocate (self%fe_types)
        if (allocated(self%fe_material_ids)) deallocate (self%fe_material_ids)
        if (allocated(self%connectivity%ind)) deallocate (self%connectivity%ind)
        if (allocated(self%connectivity%val)) deallocate (self%connectivity%val)

        if (self%num_elements > 0) then
            call allocate_array(self%fe_types, self%num_elements)
            call allocate_array(self%fe_material_ids, self%num_elements)
            call allocate_array(self%connectivity%ind, self%num_elements + 1)
            call allocate_array(self%connectivity%val, num_total_connectivity)
        end if

        ! Pass 2: Store element data
        if (self%num_elements > 0) then
            self%connectivity%ind(1) = 1
            ind = 0
            do i = 1, num_total_cells
                if (input%geometry%vtk%cells(i)%get_dimension() == self%parent%computation_dimension) then
                    ind = ind + 1
                    self%fe_types(ind) = input%geometry%vtk%cells(i)%cell_type
                    self%fe_material_ids(ind) = input%geometry%vtk%cells(i)%cell_entity_id
                    self%connectivity%ind(ind + 1) = self%connectivity%ind(ind) + input%geometry%vtk%cells(i)%num_nodes_in_cell
                    self%connectivity%val(self%connectivity%ind(ind):self%connectivity%ind(ind + 1) - 1) = &
                        input%geometry%vtk%cells(i)%connectivity(1:input%geometry%vtk%cells(i)%num_nodes_in_cell)
                end if
            end do
        end if

        call self%fe_manager%initialize(input, self%num_elements, self%fe_types)
        call self%colors%initialize(input)
    end subroutine initialize_element_manager

    !> Initializes the boundary manager by processing BCs for all active physics.
    subroutine initialize_boundary_manager(self, input, controls)
        class(type_boundary_manager), intent(inout) :: self
        type(type_input), intent(in) :: input
        type(type_controls), intent(in) :: controls

        if (input%basic%analysis_controls%is_active(PHYSICS_TYPE_THERMAL)) then
            call self%process_single_physics_bcs(PHYSICS_TYPE_THERMAL, input, controls)
        end if
        if (input%basic%analysis_controls%is_active(PHYSICS_TYPE_HYDRAULIC)) then
            call self%process_single_physics_bcs(PHYSICS_TYPE_HYDRAULIC, input, controls)
        end if
        if (input%basic%analysis_controls%is_active(PHYSICS_TYPE_MECHANICAL)) then
            call self%process_single_physics_bcs(PHYSICS_TYPE_MECHANICAL, input, controls)
        end if
    end subroutine initialize_boundary_manager

    ! --------------------------------------------------------------------------
    ! Refactored Boundary Condition Processing
    ! --------------------------------------------------------------------------

    !> Processes, sorts, and groups all boundary conditions for a single physics type.
    subroutine process_single_physics_bcs(self, physics_type_id, input, controls)
        implicit none
        class(type_boundary_manager), intent(inout) :: self
        integer(int32), intent(in) :: physics_type_id
        type(type_input), intent(in) :: input
        type(type_controls), intent(in) :: controls

        integer(int32) :: target_dimension, num_groups
        integer(int32), allocatable :: bc_idx_list(:), bc_key(:)
        integer(int32), allocatable :: entity_id_to_group_idx_map(:)
        integer(int32), allocatable :: group_to_cell_types(:)

        target_dimension = self%parent%computation_dimension - 1
        if (target_dimension < 1) return

        ! Step 1: Filter active BCs
        call self%filter_active_bcs(physics_type_id, input, target_dimension, bc_idx_list)
        if (.not. allocated(bc_idx_list)) return

        ! Step 2: Sort BCs
        call sort_by_key_wrapper(physics_type_id, input, bc_idx_list, bc_key)

        ! Step 3: Create map
        num_groups = size(bc_idx_list)
        self%physics(physics_type_id)%num_bcs = num_groups

        if (allocated(self%physics(physics_type_id)%bcs)) deallocate (self%physics(physics_type_id)%bcs)
        allocate (self%physics(physics_type_id)%bcs(num_groups))

        call self%create_entity_id_to_group_map(input, bc_idx_list, entity_id_to_group_idx_map)

        ! Step 4 & 5: Measure and Store
        call self%measure_and_allocate_bc_geometry(input, target_dimension, entity_id_to_group_idx_map, &
                                                   self%physics(physics_type_id)%bcs)
        call self%store_bc_geometry(input, target_dimension, entity_id_to_group_idx_map, &
                                    self%physics(physics_type_id)%bcs, group_to_cell_types)

        ! Step 6: Create instances
        call self%create_bc_instances(physics_type_id, input, controls, bc_idx_list, group_to_cell_types, &
                                      self%physics(physics_type_id)%bcs)

        ! Cleanup
        if (allocated(entity_id_to_group_idx_map)) call deallocate_array(entity_id_to_group_idx_map)
        if (allocated(bc_key)) call deallocate_array(bc_key)
        if (allocated(bc_idx_list)) call deallocate_array(bc_idx_list)
        if (allocated(group_to_cell_types)) call deallocate_array(group_to_cell_types)
    end subroutine process_single_physics_bcs

    !> Step 1: Filters boundary conditions.
    subroutine filter_active_bcs(self, physics_type_id, input, target_dimension, bc_idx_list)
        implicit none
        class(type_boundary_manager), intent(in) :: self
        integer(int32), intent(in) :: physics_type_id
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: target_dimension
        integer(int32), allocatable, intent(inout) :: bc_idx_list(:)

        integer(int32) :: i, num_active_bcs
        integer(int32), allocatable :: active_region_id(:)
        logical :: is_bc_active

        if (allocated(bc_idx_list)) deallocate (bc_idx_list)

        call input%geometry%vtk%get_active_region_info(active_region_id, target_dimension)
        if (.not. allocated(active_region_id)) return

        ! Pass 1: Count active BCs
        num_active_bcs = 0
        do i = 1, input%conditions%num_boundaries
            is_bc_active = is_boundary_condition_active(i, physics_type_id, input, active_region_id)
            if (is_bc_active) then
                num_active_bcs = num_active_bcs + 1
            end if
        end do

        ! Pass 2: Allocate and store indices
        if (num_active_bcs > 0) then
            allocate (bc_idx_list(num_active_bcs))
            num_active_bcs = 0
            do i = 1, input%conditions%num_boundaries
                is_bc_active = is_boundary_condition_active(i, physics_type_id, input, active_region_id)
                if (is_bc_active) then
                    num_active_bcs = num_active_bcs + 1
                    bc_idx_list(num_active_bcs) = i
                end if
            end do
        end if

        call deallocate_array(active_region_id)
    end subroutine filter_active_bcs

    !> Step 3: Creates a mapping.
    subroutine create_entity_id_to_group_map(self, input, bc_idx_list, entity_map)
        implicit none
        class(type_boundary_manager), intent(in) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: bc_idx_list(:)
        integer(int32), allocatable, intent(inout) :: entity_map(:)

        integer(int32) :: i, max_id, bc_id

        if (allocated(entity_map)) deallocate (entity_map)

        max_id = maxval(input%conditions%boundary_conditions(:)%id)
        call allocate_array(entity_map, max_id)
        entity_map = 0
        do i = 1, size(bc_idx_list)
            bc_id = input%conditions%boundary_conditions(bc_idx_list(i))%id
            entity_map(bc_id) = i
        end do
    end subroutine create_entity_id_to_group_map

    !> Step 4: Measures connectivity size.
    subroutine measure_and_allocate_bc_geometry(self, input, target_dimension, entity_map, bcs)
        implicit none
        class(type_boundary_manager), intent(in) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: target_dimension
        integer(int32), intent(in) :: entity_map(:)
        class(type_boundary_patch), intent(inout) :: bcs(:)

        integer(int32) :: i, cell_entity_id, group_idx, num_total_cells
        integer(int32), allocatable :: total_conn_per_group(:)

        call allocate_array(total_conn_per_group, size(bcs))
        total_conn_per_group = 0

        ! Pass 1: Measure
        num_total_cells = input%geometry%vtk%num_total_cells
        do i = 1, num_total_cells
            if (input%geometry%vtk%cells(i)%cell_dimension == target_dimension) then
                cell_entity_id = input%geometry%vtk%cells(i)%cell_entity_id
                if (cell_entity_id > size(entity_map)) cycle
                if (entity_map(cell_entity_id) == 0) cycle

                group_idx = entity_map(cell_entity_id)
                bcs(group_idx)%num_elements = bcs(group_idx)%num_elements + 1
                total_conn_per_group(group_idx) = total_conn_per_group(group_idx) &
                                                  + input%geometry%vtk%cells(i)%num_nodes_in_cell
            end if
        end do

        ! Pass 2: Allocate
        do i = 1, size(bcs)
            if (bcs(i)%num_elements > 0) then
                if (allocated(bcs(i)%element_types)) deallocate (bcs(i)%element_types)
                if (allocated(bcs(i)%connectivity%ind)) deallocate (bcs(i)%connectivity%ind)
                if (allocated(bcs(i)%connectivity%val)) deallocate (bcs(i)%connectivity%val)

                call allocate_array(bcs(i)%element_types, bcs(i)%num_elements)
                call allocate_array(bcs(i)%connectivity%ind, bcs(i)%num_elements + 1)
                call allocate_array(bcs(i)%connectivity%val, total_conn_per_group(i))
                bcs(i)%connectivity%ind(1) = 1
            end if
        end do

        call deallocate_array(total_conn_per_group)
    end subroutine measure_and_allocate_bc_geometry

    !> Step 5: Stores connectivity.
    subroutine store_bc_geometry(self, input, target_dimension, entity_map, bcs, group_cell_types)
        implicit none
        class(type_boundary_manager), intent(in) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: target_dimension
        integer(int32), intent(in) :: entity_map(:)
        class(type_boundary_patch), intent(inout) :: bcs(:)
        integer(int32), allocatable, intent(inout) :: group_cell_types(:)

        integer(int32) :: i, cell_entity_id, group_idx, num_nodes, num_total_cells
        integer(int32), allocatable :: current_elem_indices(:)

        if (allocated(group_cell_types)) deallocate (group_cell_types)

        allocate (current_elem_indices(size(bcs)), group_cell_types(size(bcs)))
        current_elem_indices = 0
        group_cell_types = -1

        num_total_cells = input%geometry%vtk%num_total_cells
        do i = 1, num_total_cells
            if (input%geometry%vtk%cells(i)%cell_dimension == target_dimension) then
                cell_entity_id = input%geometry%vtk%cells(i)%cell_entity_id
                if (cell_entity_id > size(entity_map)) cycle
                if (entity_map(cell_entity_id) == 0) cycle

                group_idx = entity_map(cell_entity_id)
                current_elem_indices(group_idx) = current_elem_indices(group_idx) + 1
                num_nodes = input%geometry%vtk%cells(i)%num_nodes_in_cell

                bcs(group_idx)%element_types(current_elem_indices(group_idx)) = input%geometry%vtk%cells(i)%cell_type
                bcs(group_idx)%connectivity%ind(current_elem_indices(group_idx) + 1) = &
                    bcs(group_idx)%connectivity%ind(current_elem_indices(group_idx)) + num_nodes
                bcs(group_idx)%connectivity%val( &
                    bcs(group_idx)%connectivity%ind(current_elem_indices(group_idx)): &
                    bcs(group_idx)%connectivity%ind(current_elem_indices(group_idx) + 1) - 1) = &
                    input%geometry%vtk%cells(i)%connectivity(1:num_nodes)

                if (group_cell_types(group_idx) < 0) group_cell_types(group_idx) = input%geometry%vtk%cells(i)%cell_type
            end if
        end do
        call deallocate_array(current_elem_indices)
    end subroutine store_bc_geometry

    !> Step 6: Creates instances of the polymorphic boundary condition objects.
    subroutine create_bc_instances(self, physics_type_id, input, controls, bc_idx_list, group_cell_types, bcs)
        implicit none
        class(type_boundary_manager), intent(in) :: self
        integer(int32), intent(in) :: physics_type_id
        type(type_input), intent(in) :: input
        type(type_controls), intent(in) :: controls
        integer(int32), intent(in) :: bc_idx_list(:), group_cell_types(:)
        class(type_boundary_patch), intent(inout) :: bcs(:)

        integer(int32) :: i, original_input_idx, bc_type, bc_id

        do i = 1, size(bcs)
            original_input_idx = bc_idx_list(i)

            ! [修正] インデックスから正しい境界条件IDを取得する
            bc_id = input%conditions%boundary_conditions(original_input_idx)%id

            select case (physics_type_id)
            case (PHYSICS_TYPE_THERMAL)
                bc_type = input%conditions%boundary_conditions(original_input_idx)%physics(PHYSICS_TYPE_THERMAL)%type
            case (PHYSICS_TYPE_HYDRAULIC)
                bc_type = input%conditions%boundary_conditions(original_input_idx)%physics(PHYSICS_TYPE_HYDRAULIC)%type
            case default
                bc_type = -1
            end select

            bcs(i)%type_id = bc_type

            ! [修正] original_input_idx ではなく bc_id を渡す
            bcs(i)%condition = create_boundary_conditions(bc_type, bc_id, input, controls)

            call bcs(i)%fe_manager%initialize(input, 1, group_cell_types)
        end do
    end subroutine create_bc_instances

    ! --------------------------------------------------------------------------
    ! Helper Functions for BC Processing
    ! --------------------------------------------------------------------------

    !> Checks if a given boundary condition is active for the current physics and region.
    pure function is_boundary_condition_active(idx, physics_type_id, input, active_region_id) result(is_active)
        integer(int32), intent(in) :: idx, physics_type_id
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: active_region_id(:)
        logical :: is_active

        is_active = .false.
        select case (physics_type_id)
        case (PHYSICS_TYPE_THERMAL)
            if (input%conditions%boundary_conditions(idx)%physics(PHYSICS_TYPE_THERMAL)%is_active .and. &
                any(active_region_id == input%conditions%boundary_conditions(idx)%id)) then
                is_active = .true.
            end if
        case (PHYSICS_TYPE_HYDRAULIC)
            if (input%conditions%boundary_conditions(idx)%physics(PHYSICS_TYPE_HYDRAULIC)%is_active .and. &
                any(active_region_id == input%conditions%boundary_conditions(idx)%id)) then
                is_active = .true.
            end if
        case (PHYSICS_TYPE_MECHANICAL)
            ! Not implemented
        end select
    end function is_boundary_condition_active

    !> Wrapper for sorting.
    subroutine sort_by_key_wrapper(physics_type_id, input, bc_idx_list, bc_key)
        implicit none
        integer(int32), intent(in) :: physics_type_id
        type(type_input), intent(in) :: input
        integer(int32), intent(inout) :: bc_idx_list(:)
        integer(int32), allocatable, intent(inout) :: bc_key(:)

        integer(int32) :: i
        integer(int32), allocatable :: bc_sequence(:)

        if (allocated(bc_key)) deallocate (bc_key)

        select case (physics_type_id)
        case (PHYSICS_TYPE_THERMAL)
            allocate (bc_sequence, source=THERMAL_BC_SEQUENCE)
        case (PHYSICS_TYPE_HYDRAULIC)
            allocate (bc_sequence, source=HYDRAULIC_BC_SEQUENCE)
        case (PHYSICS_TYPE_MECHANICAL)
            return
        end select

        call allocate_array(bc_key, size(bc_idx_list))
        do i = 1, size(bc_idx_list)
            select case (physics_type_id)
            case (PHYSICS_TYPE_THERMAL)
                bc_key(i) = get_bc_seq_pos( &
                            input%conditions%boundary_conditions(bc_idx_list(i))%physics(PHYSICS_TYPE_THERMAL)%type, &
                            bc_sequence)
            case (PHYSICS_TYPE_HYDRAULIC)
                bc_key(i) = get_bc_seq_pos( &
                            input%conditions%boundary_conditions(bc_idx_list(i))%physics(PHYSICS_TYPE_HYDRAULIC)%type, &
                            bc_sequence)
            end select
        end do

        call sort_by_key(bc_idx_list, bc_key)
        call deallocate_array(bc_sequence)
    end subroutine sort_by_key_wrapper

    !> Finds the position of a BC ID within a predefined sequence array.
    pure function get_bc_seq_pos(bc_id, bc_sequence) result(pos)
        implicit none
        integer(int32), intent(in) :: bc_id
        integer(int32), intent(in) :: bc_sequence(:)
        integer(int32) :: pos, k
        pos = size(bc_sequence) + 1
        do k = 1, size(bc_sequence)
            if (bc_sequence(k) == bc_id) then
                pos = k
                exit
            end if
        end do
    end function get_bc_seq_pos

    !> Sorts an index array based on a corresponding key array using insertion sort.
    subroutine sort_by_key(idx, key)
        implicit none
        integer(int32), intent(inout) :: idx(:)
        integer(int32), intent(inout) :: key(:)
        integer(int32) :: i, j, tmp_idx, tmp_key

        if (size(idx) < 2) return

        do i = 2, size(idx)
            j = i
            ! [FIX] 分割して評価することで、Short-circuit evaluation が効かない環境でも
            ! 配列外参照 (key(0)) を防ぐ
            do
                if (j <= 1) exit
                if (key(j) >= key(j - 1)) exit

                ! Swap
                tmp_idx = idx(j); idx(j) = idx(j - 1); idx(j - 1) = tmp_idx
                tmp_key = key(j); key(j) = key(j - 1); key(j - 1) = tmp_key

                j = j - 1
            end do
        end do
    end subroutine sort_by_key

    ! --------------------------------------------------------------------------
    ! Getter Functions
    ! --------------------------------------------------------------------------

    pure function get_num_nodes_domain(self) result(num_nodes)
        class(type_domain), intent(in) :: self
        integer(int32) :: num_nodes
        num_nodes = self%nodes%num_nodes
    end function get_num_nodes_domain

    pure function get_num_elements_domain(self) result(num_elements)
        class(type_domain), intent(in) :: self
        integer(int32) :: num_elements
        num_elements = self%elements%num_elements
    end function get_num_elements_domain

    pure function get_num_dofs_per_node_domain(self) result(num_dofs_per_node)
        class(type_domain), intent(in) :: self
        integer(int32) :: num_dofs_per_node
        num_dofs_per_node = self%dof_map%num_dof_per_node
    end function get_num_dofs_per_node_domain

    pure function get_total_dofs_domain(self) result(total_dofs)
        class(type_domain), intent(in) :: self
        integer(int32) :: total_dofs
        total_dofs = self%nodes%num_nodes * self%dof_map%num_dof_per_node
    end function get_total_dofs_domain

    pure function get_computation_dimension_domain(self) result(comp_dim)
        class(type_domain), intent(in) :: self
        integer(int32) :: comp_dim
        comp_dim = self%computation_dimension
    end function get_computation_dimension_domain

    pure function get_computation_type_domain(self) result(comp_type)
        class(type_domain), intent(in) :: self
        integer(int32) :: comp_type
        comp_type = self%computation_type
    end function get_computation_type_domain

    pure function get_coupling_mode_domain(self) result(coupling_mode)
        class(type_domain), intent(in) :: self
        integer(int32) :: coupling_mode
        coupling_mode = self%coupling_mode
    end function get_coupling_mode_domain

    subroutine get_node_adjacency_domain(self, matrix_type, row, col)
        implicit none
        class(type_domain), intent(in), target :: self
        integer(int32), intent(in) :: matrix_type
        integer(int32), allocatable, intent(inout) :: row(:), col(:)

        select case (matrix_type)
        case (MATRIX_COO)
            call self%node_adjacency%get_coo(row, col)
        case (MATRIX_CSR)
            call self%node_adjacency%get_csr(row, col)
        end select
    end subroutine get_node_adjacency_domain

    subroutine get_element_domain(self, elem_id, element)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32), intent(in) :: elem_id
        class(abst_fe), pointer, intent(inout) :: element

        integer(int32) :: type_id

        ! 1. Validate the element ID
        if (elem_id < 1 .or. elem_id > self%elements%num_elements) then
            ! Invalid ID: return null pointer
            element => null()
            return
        end if

        ! 2. Get the finite element type ID for this specific element
        !    (fe_types maps Element Index -> Type ID)
        type_id = self%elements%fe_types(elem_id)

        ! 3. Retrieve the FE object pointer from the FE manager
        !    (Delegate the lookup to fe_manager)
        element => self%elements%fe_manager%get_fe(type_id)

    end subroutine get_element_domain

    !>
    !> 指定された要素IDに対応するコネクティビティ（節点番号リスト）へのポインタを返す。
    !> データのコピーを行わず、内部配列を直接参照するため高速である。
    !>
    subroutine get_element_connectivity_domain(self, element_id, connectivity)
        implicit none
        class(type_domain), intent(in), target :: self
        integer(int32), intent(in) :: element_id
        integer(int32), intent(inout), pointer, contiguous, dimension(:) :: connectivity

        integer(int32) :: istart, iend

        ! 1. IDの範囲チェック
        if (element_id < 1 .or. element_id > self%elements%num_elements) then
            connectivity => null()
            return
        end if

        ! 2. CSR形式から開始位置と終了位置を取得
        !    ind(i) が開始インデックス、ind(i+1)-1 が終了インデックス
        istart = self%elements%connectivity%ind(element_id)
        iend = self%elements%connectivity%ind(element_id + 1) - 1

        ! 3. 内部配列へのポインタ結合 (データのコピーは発生しない)
        connectivity => self%elements%connectivity%val(istart:iend)

    end subroutine get_element_connectivity_domain

    subroutine get_element_coordinate_domain(self, element_id, coordinates)
        implicit none
        class(type_domain), intent(in), target :: self
        integer(int32), intent(in) :: element_id
        ! ルール通り intent(inout) を使用
        real(real64), intent(inout), allocatable :: coordinates(:, :)

        integer(int32), pointer, contiguous :: connectivity(:)
        integer(int32) :: num_nodes, n_dim
        logical :: need_reallocate

        ! 1. コネクティビティを取得
        call self%get_element_connectivity(element_id, connectivity)

        if (.not. associated(connectivity)) then
            ! エラー処理: 必要であれば deallocate するか、そのまま戻る
            return
        end if

        ! 2. 必要なサイズを計算
        num_nodes = size(connectivity)
        n_dim = size(self%nodes%coordinates, 1) ! 空間次元数(2 or 3)

        ! 3. メモリ再利用の判定 (Smart Allocation)
        need_reallocate = .true.

        if (allocated(coordinates)) then
            ! サイズがぴったり同じなら再利用する (再確保コストをカット)
            if (size(coordinates, 1) == n_dim .and. size(coordinates, 2) == num_nodes) then
                need_reallocate = .false.
            end if
        end if

        if (need_reallocate) then
            if (allocated(coordinates)) deallocate (coordinates)
            allocate (coordinates(n_dim, num_nodes))
        end if

        ! 4. データのコピー (ベクトル添字使用)
        coordinates = self%nodes%coordinates(:, connectivity)

    end subroutine get_element_coordinate_domain

    subroutine get_target_dof_domain(self, physics_type_id, target_dof)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32), intent(in) :: physics_type_id
        integer(int32), intent(inout) :: target_dof

        ! select case (physics_type_id)
        ! case (PHYSICS_TYPE_THERMAL)
        target_dof = self%dof_map%num_dof_of_physics(physics_type_id)

    end subroutine get_target_dof_domain

    subroutine get_material_id_domain(self, element_id, material_id)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32), intent(in) :: element_id
        integer(int32), intent(inout) :: material_id

        material_id = self%elements%fe_material_ids(element_id)
    end subroutine get_material_id_domain

    subroutine get_geometry_domain(self, element_id, geometry)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32), intent(in) :: element_id
        real(real64), intent(inout) :: geometry

        class(abst_fe), pointer :: fe
        real(real64), allocatable :: coordinates(:, :)

        call self%get_element(element_id, fe)
        call self%get_element_coordinate(element_id, coordinates)

        call fe%get_geometry(coordinates, geometry)

    end subroutine get_geometry_domain

    subroutine lerp_1d_domain(self, element_id, r, value, lerped_value)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32), intent(in) :: element_id
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: value(:)
        real(real64), intent(inout) :: lerped_value

        class(abst_fe), pointer :: fe

        call self%get_element(element_id, fe)

        call fe%lerp(r, value, lerped_value)

    end subroutine lerp_1d_domain

    subroutine lerp_2d_domain(self, element_id, r, value, lerped_value)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32), intent(in) :: element_id
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: value(:, :)
        real(real64), intent(inout) :: lerped_value(:)

        class(abst_fe), pointer :: fe

        call self%get_element(element_id, fe)

        call fe%lerp(r, value, lerped_value)

    end subroutine lerp_2d_domain

    subroutine lerp_3d_domain(self, element_id, r, value, lerped_value)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32), intent(in) :: element_id
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: value(:, :, :)
        real(real64), intent(inout) :: lerped_value(:, :)

        class(abst_fe), pointer :: fe

        call self%get_element(element_id, fe)

        call fe%lerp(r, value, lerped_value)

    end subroutine lerp_3d_domain

    ! --------------------------------------------------------------------------
    ! Display Procedures for Debugging
    ! --------------------------------------------------------------------------

    subroutine display_domain(self)
        implicit none
        class(type_domain), intent(in) :: self

        write (*, '(A)') '##  Domain Information'
        write (*, '(A)') '| Property                | Value |'
        write (*, '(A)') '|:------------------------|:------|'
        write (*, '(A, I0, 2A)') '| MPI Rank / Procs        | ', self%my_rank, ' / '//achar(48 + self%num_procs), '|'
        write (*, '(A, I0, 2A)') '| Computation Dimension   | ', self%computation_dimension, 'D', '|'
        write (*, '(A, I0, A)') '| Coupling Mode           | ', self%coupling_mode, '|'
        write (*, '(A)')

        call self%dof_map%display()
        call self%nodes%display()
        call self%elements%display()
        call self%boundaries%display()
    end subroutine display_domain

    subroutine display_dof_map(self)
        implicit none
        class(type_dof_map), intent(in) :: self
        write (*, '(A)') '### DOF Map'
        write (*, '(A)')
        write (*, '(A, I0)') '  - **Total DOFs per Node**: ', self%num_dof_per_node
        write (*, '(A)')
    end subroutine display_dof_map

    subroutine display_node_manager(self)
        implicit none
        class(type_node_manager), intent(in) :: self
        write (*, '(A)') '### Node Manager'
        write (*, '(A)')
        write (*, '(A, I0)') '  - **Number of Nodes**: ', self%num_nodes
        write (*, '(A)')
    end subroutine display_node_manager

    subroutine display_element_manager(self)
        implicit none
        class(type_element_manager), intent(in) :: self
        write (*, '(A)') '### Element Manager'
        write (*, '(A)')
        write (*, '(A, I0)') '  - **Number of Elements**: ', self%num_elements
        call self%connectivity%display('Volume Elements')
        write (*, '(A)')
    end subroutine display_element_manager

    subroutine display_boundary_manager(self)
        implicit none
        class(type_boundary_manager), intent(in) :: self
        integer(int32) :: i
        character(len=20) :: physics_name

        write (*, '(A)') '### Boundary Manager'
        do i = 1, NUM_PHYSICS_TYPES
            if (self%physics(i)%num_bcs > 0) then
                select case (i)
                case (PHYSICS_TYPE_THERMAL); physics_name = 'Thermal'
                case (PHYSICS_TYPE_HYDRAULIC); physics_name = 'Hydraulic'
                case (PHYSICS_TYPE_MECHANICAL); physics_name = 'Mechanical'
                end select
                write (*, '(A, A, A)') '  - **Physics**: ', strip(physics_name)
                call self%physics(i)%display()
            end if
        end do
        write (*, '(A)')
    end subroutine display_boundary_manager

    subroutine display_physics_bc_manager(self)
        implicit none
        class(type_physics_bc_manager), intent(in) :: self
        integer(int32) :: i
        write (*, '(A, I0, A)') '    - **Number of BCs**: ', self%num_bcs
        do i = 1, self%num_bcs
            write (*, '(A, I0, A)') '    - **BC Group**: ', i
            call self%bcs(i)%display()
        end do
    end subroutine display_physics_bc_manager

    subroutine display_boundary_patch(self)
        implicit none
        class(type_boundary_patch), intent(in) :: self
        write (*, '(A, I0)') '        - **Type ID**: ', self%type_id
        write (*, '(A, I0)') '        - **Num Elements**: ', self%num_elements
        call self%connectivity%display('BC Elements')
    end subroutine display_boundary_patch

    subroutine display_connectivity(self, title)
        class(type_fe_connectivity), intent(in) :: self
        character(len=*), intent(in) :: title
        write (*, '(A,A,A)') '        - **Connectivity (', strip(title), ')**:'
        if (allocated(self%ind)) then
            write (*, '(A, I0, A, I0, A)') '          - Ind Size: ', size(self%ind), ', Val Size: ', size(self%val)
        else
            write (*, '(A)') '          - Not allocated'
        end if
    end subroutine display_connectivity

end module domain_manager

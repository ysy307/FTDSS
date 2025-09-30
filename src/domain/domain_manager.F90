!>
!> Manages the computational domain, including mesh, boundary conditions, and parallel data.
!>s
module domain_manager
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: mpi_f08
    use :: stdlib_logger
    use :: stdlib_strings, only:strip
    use :: module_core
    use :: module_input, only:type_input
    use :: module_control, only:type_controls
    use :: module_fe, only:type_fe_manager
    use :: domain_multicoloring, only:type_coloring
    use :: domain_adjacency, only:type_node_adjacency, type_map_node_to_element
    use :: module_boundary
    ! use :: conditions_boundary, only:abst_bc, construct_type_bc_thermal_dirichlet, &
    !     construct_type_bc_thermal_adiabatic ! Other construct_* would be USEd similarly

    implicit none
    private

    public :: type_domain

    !>
    !>  Stores element connectivity in Compressed Sparse Row (CSR) format.
    !>
    type :: type_fe_connectivity
        !>
        !> Index array for CSR format. Stores the starting position
        !>        of each element's nodes in 'val'. Size is (num_elements + 1).
        !>
        integer(int32), allocatable :: ind(:)
        !>
        !> Value array for CSR format. Stores the concatenated node IDs for all elements.
        !>
        integer(int32), allocatable :: val(:)
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
        !>
        class(abst_bc), allocatable :: condition
    end type type_boundary_patch

    !>
    !> Manages all boundary conditions for a single physics type (e.g., thermal).
    !>
    type :: type_physics_bc_manager
        !>
        !> The number of unique boundary conditions for this physics.
        !>
        integer(int32) :: num_bcs = 0
        !>
        !> Array of unique boundary condition sets.
        !>
        type(type_boundary_patch), allocatable :: bcs(:)
        !>
        !>
        !>
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
    end type type_boundary_manager

    !>
    !> Stores the mapping and layout of degrees of freedom (DOF) per node.
    !>
    type :: type_dof_map
        !>
        !> Total number of degrees of freedom per node for the active physics.
        !>
        integer(int32) :: num_dof_per_node = 0
        !>
        !> Number of DOFs for each individual physics type (e.g., thermal=1, mechanical=3).
        !>
        integer(int32) :: num_dof_of_physics(NUM_PHYSICS_TYPES) = 0
        !>
        !> The starting index for each physics' DOFs within the block of DOFs for a single node.
        !>
        integer(int32) :: start_dof_index(NUM_PHYSICS_TYPES) = 0
    end type type_dof_map

    !>
    !> Manages all data related to nodes (points) in the domain.
    !>
    type :: type_node_manager
        !>
        !> Pointer to the parent domain object.
        !>
        type(type_domain), pointer, private :: parent => null()
        !>
        !> Number of nodes in this subdomain.
        !>
        integer(int32) :: num_nodes = 0
        !>
        !> Nodal coordinates. Size: (computation_dimension, num_nodes).
        !>
        real(real64), allocatable :: coordinates(:, :)
        !>
        !> Global ID for each node in this subdomain.
        !>
        integer(int32), allocatable :: node_global_ids(:)
    contains
        procedure, public, pass(self) :: initialize => initialize_node_manager
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
        !> Manager for FE type-specific operations (shape functions, etc.).
        type(type_fe_manager) :: fe_manager
        !> Connectivity data for all elements.
        type(type_fe_connectivity) :: connectivity
        !> Coloring information for parallel element processing.
        type(type_coloring) :: colors
    contains
        procedure, public, pass(self) :: initialize => initialize_element_manager
    end type type_element_manager

    ! ==========================================================
    ! Top-level Domain Container Type
    ! ==========================================================
    !>
    !> The main container for all simulation domain data.
    !>
    !>  This type acts as the top-level object that holds and manages the mesh (nodes, elements),
    !>          boundary conditions, DOF mappings, and parallel processing information.
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
    end type type_domain

contains

    !> Initializes the entire domain object and its components.
    !>
    !>  This is the main entry point for setting up the domain. It orchestrates the
    !>          initialization of basic info, nodes, elements, and boundaries.
    subroutine initialize_type_domain(self, input, controls)
        implicit none
        !> The domain object to be initialized.
        class(type_domain), intent(inout) :: self
        !> The parsed input data from a file.
        type(type_input), intent(in) :: input
        !> The control parameters for the simulation.
        type(type_controls), intent(in) :: controls

        if (.not. self%is_associated) call self%associate_parent(self%nodes, self%elements, self%boundaries)
        call self%set_basic_info_and_dof_map(input)

        call self%nodes%initialize(input)
        call self%elements%initialize(input)
        ! Initialize the node adjacency information
        call self%node_adjacency%initialize(self%nodes%num_nodes, self%elements%connectivity%ind, self%elements%connectivity%val)
        ! Initialize the element-to-node adjacency information
        call self%element_adjacency%initialize(self%nodes%num_nodes, self%elements%num_elements, &
                                               self%elements%connectivity%ind, self%elements%connectivity%val)
        call self%boundaries%initialize(input, controls)
    end subroutine initialize_type_domain

    !> Associates child manager components with this parent domain object.
    subroutine associate_parent(self, node, element, boundary)
        implicit none
        !> The parent domain object.
        class(type_domain), intent(inout), target :: self
        !> The node manager component.
        class(type_node_manager), intent(inout) :: node
        !> The boundary manager component.
        class(type_element_manager), intent(inout) :: element
        !> The element manager component.
        class(type_boundary_manager), intent(inout) :: boundary

        node%parent => self
        element%parent => self
        boundary%parent => self

        self%is_associated = .true.
    end subroutine associate_parent

    !> Sets basic simulation info and configures the DOF map based on input settings.
    subroutine set_basic_info_and_dof_map(self, input)
        implicit none
        !> The domain object.
        class(type_domain), intent(inout) :: self
        !> The parsed input data.
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
        if (input%basic%analysis_controls%calculate_thermal) then
            self%dof_map%start_dof_index(PHYSICS_TYPE_THERMAL) = current_dof_index
            current_dof_index = current_dof_index + 1
        end if
        if (input%basic%analysis_controls%calculate_hydraulic) then
            self%dof_map%start_dof_index(PHYSICS_TYPE_HYDRAULIC) = current_dof_index
            current_dof_index = current_dof_index + 1
        end if
        if (input%basic%analysis_controls%calculate_mechanical) then
            self%dof_map%start_dof_index(PHYSICS_TYPE_MECHANICAL) = current_dof_index
            current_dof_index = current_dof_index + self%computation_dimension
        end if
        self%dof_map%num_dof_per_node = current_dof_index - 1

        select case (strip(input%basic%analysis_controls%coupling_mode))
        case ("weak")
            self%coupling_mode = COUPLING_MODE_STAGGERED
        case ("strong")
            self%coupling_mode = COUPLING_MODE_MONOLITHIC
        end select
    end subroutine set_basic_info_and_dof_map

    !> Initializes the node manager by reading data from the input object.
    subroutine initialize_node_manager(self, input)
        implicit none
        !> The node manager object.
        class(type_node_manager), intent(inout) :: self
        !> The parsed input data.
        type(type_input), intent(in) :: input

        self%num_nodes = input%geometry%vtk%num_points

        allocate (self%coordinates(self%parent%computation_dimension, self%num_nodes))
        select case (self%parent%computation_type)
        case (1) ! 2D (XY-plane)
            self%coordinates(1, :) = input%geometry%vtk%points%x(1:self%num_nodes)
            self%coordinates(2, :) = input%geometry%vtk%points%y(1:self%num_nodes)
        case (2) ! 2D (XZ-plane)
            self%coordinates(1, :) = input%geometry%vtk%points%x(1:self%num_nodes)
            self%coordinates(2, :) = input%geometry%vtk%points%z(1:self%num_nodes)
        case (3) ! 3D
            self%coordinates(1, :) = input%geometry%vtk%points%x(1:self%num_nodes)
            self%coordinates(2, :) = input%geometry%vtk%points%y(1:self%num_nodes)
            self%coordinates(3, :) = input%geometry%vtk%points%z(1:self%num_nodes)
        end select

        call allocate_array(self%node_global_ids, self%num_nodes)
        self%node_global_ids(:) = input%geometry%vtk%global_node_ids(1:self%num_nodes)

    end subroutine initialize_node_manager

    !> Initializes the element manager by reading and organizing element data.
    !>
    !>  This routine extracts volume elements that match the computation dimension from the
    !>          input data, stores their properties and connectivity, and initializes the
    !>          FE and coloring sub-managers.
    subroutine initialize_element_manager(self, input)
        implicit none
        !> The element manager object.
        class(type_element_manager), intent(inout) :: self
        !> The parsed input data.
        type(type_input), intent(in) :: input

        integer(int32) :: i, ind, cell_dimension, num_total_cells, num_total_connectivity
        integer(int32), allocatable :: unique_fe_types(:)

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

        ! Initialize the FE manager with the unique element types found
        call unique(self%fe_types, unique_fe_types)
        call self%fe_manager%initialize(input, self%num_elements, self%fe_types)

        ! Initialize coloring information
        call self%colors%initialize(input)
    end subroutine initialize_element_manager

    !> Initializes the boundary manager by processing BCs for all active physics.
    subroutine initialize_boundary_manager(self, input, controls)
        !> The boundary manager object.
        class(type_boundary_manager), intent(inout) :: self
        !> The parsed input data.
        type(type_input), intent(in) :: input
        !> The control parameters for the simulation.
        type(type_controls), intent(in) :: controls

        integer(int32) :: phys, ibc, ie

        if (input%basic%analysis_controls%calculate_thermal) then
            call self%process_single_physics_bcs(PHYSICS_TYPE_THERMAL, input, controls)
        end if
        if (input%basic%analysis_controls%calculate_hydraulic) then
            call self%process_single_physics_bcs(PHYSICS_TYPE_HYDRAULIC, input, controls)
        end if
        if (input%basic%analysis_controls%calculate_mechanical) then
            call self%process_single_physics_bcs(PHYSICS_TYPE_MECHANICAL, input, controls)
        end if

    end subroutine initialize_boundary_manager

    !> Processes, sorts, and groups all boundary conditions for a single physics type.
    !>
    !> This routine identifies active boundary entities, groups them by identical condition
    !>          (type and values), and stores the geometric information in CSR format for each group.
    subroutine process_single_physics_bcs(self, physics_type_id, input, controls)
        implicit none
        !> The boundary manager object.
        class(type_boundary_manager), intent(inout) :: self
        !> The integer ID of the physics to process.
        integer(int32), intent(in) :: physics_type_id
        !> The parsed input data.
        type(type_input), intent(in) :: input
        !> The control parameters for the simulation.
        type(type_controls), intent(in) :: controls

        ! --- Local variables ---
        integer(int32) :: i, bc_type, num_groups
        integer(int32) :: max_id, bc_id, group_idx
        integer(int32) :: target_dimension
        integer(int32), allocatable :: bc_sequence(:), bc_idx_list(:), bc_key(:), active_region_id(:), group_to_cell_id(:)
        integer(int32), allocatable :: input_idx_to_group_idx_map(:)
        integer(int32), allocatable :: entity_id_to_group_idx_map(:)
        integer(int32), allocatable :: total_conn_per_group(:), current_elem_indices(:)
        integer(int32) :: num_total_cells, cell_id, current_group_idx, num_nodes

        ! Check target dimension
        target_dimension = self%parent%computation_dimension - 1
        if (target_dimension < 1) return

        ! --- Select BC_SEQUENCE for the given physics type
        select case (physics_type_id)
        case (PHYSICS_TYPE_THERMAL)
            allocate (bc_sequence, source=THERMAL_BC_SEQUENCE)
        case (PHYSICS_TYPE_HYDRAULIC)
            allocate (bc_sequence, source=HYDRAULIC_BC_SEQUENCE)
        case (PHYSICS_TYPE_MECHANICAL)
            return ! Not implemented
        end select

        ! --- Collect indices of active boundary conditions ---
        call input%geometry%vtk%get_active_region_info(active_region_id, target_dimension)

        ! Collect relevant boundary condition indices into a temporary array
        allocate (bc_idx_list(0))
        do i = 1, input%conditions%num_boundaries
            select case (physics_type_id)
            case (PHYSICS_TYPE_THERMAL)
                if (input%conditions%boundary_conditions(i)%calculate_thermal .and. &
                    any(active_region_id == input%conditions%boundary_conditions(i)%id)) then
                    bc_idx_list = [bc_idx_list, i]
                end if
            case (PHYSICS_TYPE_HYDRAULIC)
                if (input%conditions%boundary_conditions(i)%calculate_hydraulic .and. &
                    any(active_region_id == input%conditions%boundary_conditions(i)%id)) then
                    bc_idx_list = [bc_idx_list, i]
                end if
            end select
        end do

        ! --- Create a key array for sorting ---
        call allocate_array(bc_key, size(bc_idx_list))
        do i = 1, size(bc_idx_list)
            select case (physics_type_id)
            case (PHYSICS_TYPE_THERMAL)
                bc_type = get_bc_type_from_string( &
                          input%conditions%boundary_conditions(bc_idx_list(i))%thermal%type, &
                          physics_type_id)
            case (PHYSICS_TYPE_HYDRAULIC)
                bc_type = get_bc_type_from_string( &
                          input%conditions%boundary_conditions(bc_idx_list(i))%hydraulic%type, &
                          physics_type_id)
            end select
            bc_key(i) = get_bc_seq_pos(bc_type, bc_sequence)
        end do

        ! --- Sort using the key array ---
        call sort_by_key(bc_idx_list, bc_key)

        if (size(bc_idx_list) == 0) then
            self%physics(physics_type_id)%num_bcs = 0
            return
        end if

        ! --- Step A: Grouping and determining the final number of BCs ---
        ! Scan the sorted list and compare adjacent elements to count groups
        call allocate_array(input_idx_to_group_idx_map, size(bc_idx_list))
        num_groups = 1
        input_idx_to_group_idx_map(1) = num_groups

        do i = 2, size(bc_idx_list)
            if (.not. are_bcs_identical(bc_idx_list(i), bc_idx_list(i - 1), input, physics_type_id)) then
                num_groups = num_groups + 1
            end if
            input_idx_to_group_idx_map(i) = num_groups
        end do

        self%physics(physics_type_id)%num_bcs = num_groups
        allocate (self%physics(physics_type_id)%bcs(num_groups))

        ! --- Step B: Create a map from entity ID to group index ---
        max_id = maxval(input%conditions%boundary_conditions(:)%id)
        call allocate_array(entity_id_to_group_idx_map, max_id)
        entity_id_to_group_idx_map = 0

        do i = 1, size(bc_idx_list)
            bc_id = input%conditions%boundary_conditions(bc_idx_list(i))%id
            group_idx = input_idx_to_group_idx_map(i)
            entity_id_to_group_idx_map(bc_id) = group_idx
        end do
        call deallocate_array(input_idx_to_group_idx_map)
        call deallocate_array(bc_idx_list)

        ! --- Step C: Store geometric information (2-pass process) ---
        call allocate_array(total_conn_per_group, num_groups)
        total_conn_per_group = 0

        ! Pass 1: Measure
        num_total_cells = input%geometry%vtk%num_total_cells
        do i = 1, num_total_cells
            if (input%geometry%vtk%cells(i)%cell_dimension == target_dimension) then
                cell_id = input%geometry%vtk%cells(i)%cell_entity_id
                if (cell_id > size(entity_id_to_group_idx_map) .or. entity_id_to_group_idx_map(cell_id) == 0) cycle

                current_group_idx = entity_id_to_group_idx_map(cell_id)
                self%physics(physics_type_id)%bcs(current_group_idx)%num_elements = &
                    self%physics(physics_type_id)%bcs(current_group_idx)%num_elements + 1
                total_conn_per_group(current_group_idx) = total_conn_per_group(current_group_idx) &
                                                          + input%geometry%vtk%cells(i)%num_nodes_in_cell
            end if
        end do

        ! Pass 2: Allocate and Store
        do i = 1, num_groups
            if (self%physics(physics_type_id)%bcs(i)%num_elements > 0) then
                call allocate_array(self%physics(physics_type_id)%bcs(i)%element_types, &
                                    self%physics(physics_type_id)%bcs(i)%num_elements)
                call allocate_array(self%physics(physics_type_id)%bcs(i)%connectivity%ind, &
                                    self%physics(physics_type_id)%bcs(i)%num_elements + 1)
                call allocate_array(self%physics(physics_type_id)%bcs(i)%connectivity%val, &
                                    total_conn_per_group(i))
                self%physics(physics_type_id)%bcs(i)%connectivity%ind(1) = 1
            end if
        end do
        call deallocate_array(total_conn_per_group)

        call allocate_array(current_elem_indices, num_groups)
        current_elem_indices = 0

        call allocate_array(group_to_cell_id, num_groups)
        group_to_cell_id = -1

        do i = 1, num_total_cells
            if (input%geometry%vtk%cells(i)%cell_dimension == target_dimension) then
                cell_id = input%geometry%vtk%cells(i)%cell_entity_id
                if (cell_id > size(entity_id_to_group_idx_map) .or. entity_id_to_group_idx_map(cell_id) == 0) cycle

                current_group_idx = entity_id_to_group_idx_map(cell_id)
                current_elem_indices(current_group_idx) = current_elem_indices(current_group_idx) + 1

                num_nodes = input%geometry%vtk%cells(i)%num_nodes_in_cell
                self%physics(physics_type_id)%bcs(current_group_idx)%element_types(current_elem_indices(current_group_idx)) &
                    = input%geometry%vtk%cells(i)%cell_type

                self%physics(physics_type_id)%bcs(current_group_idx)%connectivity%ind(current_elem_indices(current_group_idx) + 1) = &
                    self%physics(physics_type_id)%bcs(current_group_idx)%connectivity%ind(current_elem_indices(current_group_idx)) + num_nodes

                self%physics(physics_type_id)%bcs(current_group_idx)%connectivity%val( &
                    self%physics(physics_type_id)%bcs(current_group_idx)%connectivity%ind(current_elem_indices(current_group_idx)): &
                    self%physics(physics_type_id)%bcs(current_group_idx)%connectivity%ind(current_elem_indices(current_group_idx) + 1) - 1) = &
                    input%geometry%vtk%cells(i)%connectivity(1:num_nodes)

                if (group_to_cell_id(current_group_idx) < 0) group_to_cell_id(current_group_idx) = cell_id
            end if
        end do

        block
            integer(int32) :: my_rank
            call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
            if (my_rank == 0) then
                write (*, *) group_to_cell_id
            end if
        end block

        ! do current_group_idx = 1, num_groups
        !     if (group_to_cell_id(current_group_idx) > 0) then
        !         call create_boundary_conditions( &
        !             cell_id=group_to_cell_id(current_group_idx), &
        !             input=input, &
        !             controls=controls)
        !     end if
        ! end do

        call deallocate_array(current_elem_indices)
        call deallocate_array(entity_id_to_group_idx_map)
        call deallocate_array(bc_key)
        call deallocate_array(bc_sequence)
        call deallocate_array(active_region_id)
        call deallocate_array(group_to_cell_id)
    end subroutine process_single_physics_bcs

    !> Finds the position of a BC ID within a predefined sequence array.
    pure function get_bc_seq_pos(bc_id, bc_sequence) result(pos)
        implicit none
        !> The boundary condition ID to find.
        integer(int32), intent(in) :: bc_id
        !> The array defining the order of BC types.
        integer(int32), intent(in) :: bc_sequence(:)
        !> The 1-based index of the BC ID in the sequence. Returns size+1 if not found.
        integer(int32) :: pos

        integer(int32) :: k

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
        !> The index array to be sorted.
        integer(int32), intent(inout) :: idx(:)
        !> The key array to sort by. Both arrays are modified in place.
        integer(int32), intent(inout) :: key(:)

        integer(int32) :: i, j, tmp_idx, tmp_key

        do i = 2, size(idx)
            j = i
            do while (j > 1 .and. key(j) < key(j - 1))
                tmp_idx = idx(j)
                idx(j) = idx(j - 1)
                idx(j - 1) = tmp_idx
                tmp_key = key(j)
                key(j) = key(j - 1)
                key(j - 1) = tmp_key
                j = j - 1
            end do
        end do
    end subroutine sort_by_key

    !> Checks if two boundary conditions from the input are functionally identical.
    pure function are_bcs_identical(idx1, idx2, input, physics_type_id) result(is_identical)
        implicit none
        !> Index of the first BC in the input array.
        integer(int32), intent(in) :: idx1
        !> Index of the second BC in the input array.
        integer(int32), intent(in) :: idx2
        !> The parsed input data.
        type(type_input), intent(in) :: input
        !> physics_type_id The integer ID of the physics to compare.
        integer(int32), intent(in) :: physics_type_id
        !> If the BCs are identical, `.true.` (same type and values), `.false.` otherwise.
        logical :: is_identical

        integer(int32) :: bc_type1, bc_type2
        logical :: alloc1, alloc2

        is_identical = .false.

        select case (physics_type_id)
        case (PHYSICS_TYPE_THERMAL)
            ! --- Convert type from string to integer ID using a helper function ---
            bc_type1 = get_bc_type_from_string(input%conditions%boundary_conditions(idx1)%thermal%type, physics_type_id)
            bc_type2 = get_bc_type_from_string(input%conditions%boundary_conditions(idx2)%thermal%type, physics_type_id)

            ! --- Compare by integer ID ---
            if (bc_type1 /= bc_type2) then
                return
            end if

            ! --- Safe value comparison using allocated() ---
            alloc1 = allocated(input%conditions%boundary_conditions(idx1)%thermal%values)
            alloc2 = allocated(input%conditions%boundary_conditions(idx2)%thermal%values)

            if (alloc1 .and. alloc2) then
                if (size(input%conditions%boundary_conditions(idx1)%thermal%values) == &
                    size(input%conditions%boundary_conditions(idx2)%thermal%values)) then

                    if (all(input%conditions%boundary_conditions(idx1)%thermal%values == &
                            input%conditions%boundary_conditions(idx2)%thermal%values)) then
                        is_identical = .true.
                    end if
                end if
            else if (.not. alloc1 .and. .not. alloc2) then
                is_identical = .true.
            end if

        case (PHYSICS_TYPE_HYDRAULIC)
            ! --- Convert type from string to integer ID using a helper function ---
            bc_type1 = get_bc_type_from_string(input%conditions%boundary_conditions(idx1)%hydraulic%type, physics_type_id)
            bc_type2 = get_bc_type_from_string(input%conditions%boundary_conditions(idx2)%hydraulic%type, physics_type_id)

            ! --- Compare by integer ID ---
            if (bc_type1 /= bc_type2) then
                return
            end if

            ! --- Safe value comparison using allocated() ---
            alloc1 = allocated(input%conditions%boundary_conditions(idx1)%hydraulic%values)
            alloc2 = allocated(input%conditions%boundary_conditions(idx2)%hydraulic%values)

            if (alloc1 .and. alloc2) then
                if (size(input%conditions%boundary_conditions(idx1)%hydraulic%values) == &
                    size(input%conditions%boundary_conditions(idx2)%hydraulic%values)) then

                    if (all(input%conditions%boundary_conditions(idx1)%hydraulic%values == &
                            input%conditions%boundary_conditions(idx2)%hydraulic%values)) then
                        is_identical = .true.
                    end if
                end if
            else
                is_identical = .true.
            end if
        end select

    end function are_bcs_identical

    pure function get_num_nodes_domain(self) result(num_nodes)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: num_nodes

        num_nodes = self%nodes%num_nodes
    end function get_num_nodes_domain

    pure function get_num_elements_domain(self) result(num_elements)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: num_elements

        num_elements = self%elements%num_elements
    end function get_num_elements_domain

    pure function get_num_dofs_per_node_domain(self) result(num_dofs_per_node)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: num_dofs_per_node

        num_dofs_per_node = self%dof_map%num_dof_per_node
    end function get_num_dofs_per_node_domain

    pure function get_total_dofs_domain(self) result(total_dofs)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: total_dofs

        total_dofs = self%nodes%num_nodes * self%dof_map%num_dof_per_node
    end function get_total_dofs_domain

    pure function get_computation_dimension_domain(self) result(comp_dim)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: comp_dim

        comp_dim = self%computation_dimension
    end function get_computation_dimension_domain

    pure function get_computation_type_domain(self) result(comp_type)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: comp_type

        comp_type = self%computation_type
    end function get_computation_type_domain

    pure function get_coupling_mode_domain(self) result(coupling_mode)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32) :: coupling_mode

        coupling_mode = self%coupling_mode
    end function get_coupling_mode_domain

    subroutine get_node_adjacency_domain(self, matrix_type, row, col)
        implicit none
        class(type_domain), intent(in), target :: self
        integer(int32), intent(in) :: matrix_type
        integer(int32), dimension(:), pointer, intent(inout) :: row, col

        ! Get the node adjacency information based on the matrix type
        select case (matrix_type)
        case (MATRIX_COO)
            call self%node_adjacency%get_coo_ptr(row, col)
        case (MATRIX_CRS)
            call self%node_adjacency%get_csr_ptr(row, col)
        end select
    end subroutine get_node_adjacency_domain

end module domain_manager

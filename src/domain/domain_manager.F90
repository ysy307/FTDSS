!>
!> Manages the computational domain, including mesh, boundary conditions, and parallel data.
!>
module domain_manager
    use, intrinsic :: iso_fortran_env
    use :: mpi_f08
    use :: stdlib_logger
    use :: stdlib_strings, only:strip
    use :: stdlib_optval, only:optval
    use :: module_core

    use :: module_fe, only:type_fe_manager, abst_fe
    use :: domain_multicoloring, only:type_coloring
    use :: domain_adjacency, only:type_node_adjacency, type_map_node_to_element
    use :: components_domain_nodes, only:type_nodes_manager
    use :: components_domain_elements, only:type_elements_manager
    use :: components_domain_boundaries, only:type_boundaries_manager, type_boundary_patch

    implicit none
    private

    public :: type_domain

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
        type(type_constant_id) :: computation_type
        !> The type of coupling (e.g., staggered or monolithic).
        type(type_constant_id) :: coupling_mode
        !> Manages the degree of freedom layout.
        type(type_dof_map) :: dof_map
        !> Manages all nodal data.
        type(type_nodes_manager) :: nodes
        !> Manages all element data.
        type(type_elements_manager) :: elements
        !> Manages all boundary condition data.
        type(type_boundaries_manager) :: boundaries
        !> Node adjacency information for all nodes in the domain.
        type(type_node_adjacency) :: node_adjacency
        !> Element-to-node adjacency information.
        type(type_map_node_to_element) :: element_adjacency
        !> Indicates whether the domain is associated with a parent.
        logical, private :: is_associated = .false.
    contains
        ! ---- Lifecycle ----
        ! initialize, destroy, reset, etc.
        procedure, public, pass(self) :: initialize => initialize_type_domain

        ! ---- Mutator ----
        ! set_XXX, increment_XXX, update_XXX, etc.

        ! ---- Algorithm / Operation ----
        ! compute_XXX, check_XXX, solve_XXX, etc.
        procedure, public, pass(self) :: calc_measure => calc_measure_domain
        generic, public :: lerp => lerp_1d_domain, &
            lerp_2d_domain, &
            lerp_3d_domain
        procedure, private, pass(self) :: lerp_1d_domain
        procedure, private, pass(self) :: lerp_2d_domain
        procedure, private, pass(self) :: lerp_3d_domain
        procedure, public, pass(self) :: dlerp => dlerp_domain
        procedure, public, pass(self) :: find_element => find_element_domain

        ! ---- Inquiry ----
        ! is_XXX, has_XXX, should_XXX, etc.

        ! ---- Getter ----
        procedure, public, pass(self) :: get_computation_dimension => get_computation_dimension_domain
        procedure, public, pass(self) :: get_computation_type => get_computation_type_domain
        procedure, public, pass(self) :: get_coupling_mode => get_coupling_mode_domain
        procedure, public, pass(self) :: get_num_nodes => get_num_nodes_domain
        procedure, public, pass(self) :: get_num_fe => get_num_fe_domain
        procedure, public, pass(self) :: get_fe => get_fe_domain
        procedure, public, pass(self) :: get_fe_connectivity => get_fe_connectivity_domain
        procedure, public, pass(self) :: get_fe_coordinate => get_fe_coordinate_domain
        procedure, public, pass(self) :: get_num_dof_per_node => get_num_dof_per_node_domain
        procedure, public, pass(self) :: get_start_dof_index => get_start_dof_index_domain
        procedure, public, pass(self) :: get_node_adjacency => get_node_adjacency_domain
        procedure, public, pass(self) :: get_target_dof => get_target_dof_domain
        procedure, public, pass(self) :: get_material_id => get_material_id_domain
        procedure, public, pass(self) :: get_num_colors => get_num_colors_domain
        procedure, public, pass(self) :: get_colored_elements => get_colored_elements_domain
        procedure, public, pass(self) :: get_total_dofs => get_total_dofs_domain
        procedure, public, pass(self) :: get_bc_patch => get_bc_patch_domain
        procedure, public, pass(self) :: get_config => get_config_domain

        ! ---- Meta / Utility ----
        ! display, to_string, etc.
        procedure, public, pass(self) :: display => display_domain

        ! ---- Operator ----

    end type type_domain

contains

    !> Initializes the entire domain object and its components.
    subroutine initialize_type_domain(self, config_nodes, config_elements, config_multicoloring, config_boundary_elements)
        implicit none
        class(type_domain), intent(inout) :: self
        type(type_config_nodes), intent(in) :: config_nodes
        type(type_config_elements), intent(in) :: config_elements
        type(type_config_multicoloring), intent(in) :: config_multicoloring
        type(type_config_elements), intent(in) :: config_boundary_elements(:)

        integer(int32) :: num_nodes

        call self%nodes%initialize(config_nodes)
        call self%nodes%get_num_nodes(num_nodes)

        call self%elements%initialize(config_elements, config_multicoloring)
        call self%node_adjacency%initialize(num_nodes, self%elements%connectivity%row_ptr, &
                                            self%elements%connectivity%col_ind)
        call self%element_adjacency%initialize(num_nodes, self%elements%num_fe, &
                                               self%elements%connectivity%row_ptr, self%elements%connectivity%col_ind)
        call self%boundaries%initialize(config_boundary_elements)
    end subroutine initialize_type_domain

    ! --------------------------------------------------------------------------
    ! Getter Functions
    ! --------------------------------------------------------------------------
    subroutine get_computation_dimension_domain(self, computation_dimension)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32), intent(inout) :: computation_dimension

        computation_dimension = self%computation_dimension
    end subroutine get_computation_dimension_domain

    subroutine get_computation_type_domain(self, computation_type)
        implicit none
        class(type_domain), intent(in), target :: self
        type(type_constant_id), intent(inout), pointer :: computation_type

        computation_type => self%computation_type
    end subroutine get_computation_type_domain

    subroutine get_coupling_mode_domain(self, coupling_mode)
        implicit none
        class(type_domain), intent(in), target :: self
        type(type_constant_id), intent(inout), pointer :: coupling_mode

        coupling_mode => self%coupling_mode
    end subroutine get_coupling_mode_domain

    subroutine get_num_nodes_domain(self, num_nodes)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32), intent(inout) :: num_nodes

        call self%nodes%get_num_nodes(num_nodes)

    end subroutine get_num_nodes_domain

    subroutine get_num_fe_domain(self, num_fe)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32), intent(inout) :: num_fe

        call self%elements%get_num_fe(num_fe)

    end subroutine get_num_fe_domain

    subroutine get_fe_domain(self, elem_id, fe)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32), intent(in) :: elem_id
        class(abst_fe), pointer, intent(inout) :: fe

        call self%elements%get_fe(elem_id, fe)
    end subroutine get_fe_domain

    subroutine get_fe_connectivity_domain(self, element_id, connectivity)
        implicit none
        class(type_domain), intent(in), target :: self
        integer(int32), intent(in) :: element_id
        integer(int32), intent(inout), pointer, contiguous, dimension(:) :: connectivity

        call self%elements%get_fe_connectivity(element_id, connectivity)

    end subroutine get_fe_connectivity_domain

    subroutine get_num_dof_per_node_domain(self, num_dofs_per_node)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32), intent(inout) :: num_dofs_per_node

        call self%dof_map%get_num_dofs_per_node(num_dofs_per_node)
    end subroutine get_num_dof_per_node_domain

    subroutine get_total_dofs_domain(self, total_dofs)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32), intent(inout) :: total_dofs

        integer(int32) :: num_nodes
        integer(int32) :: num_dofs_per_node

        call self%nodes%get_num_nodes(num_nodes)
        call self%dof_map%get_num_dofs_per_node(num_dofs_per_node)

        total_dofs = num_nodes * num_dofs_per_node
    end subroutine get_total_dofs_domain

    subroutine get_node_adjacency_domain(self, matrix_type, row, col)
        implicit none
        class(type_domain), intent(in), target :: self
        type(type_constant_id), intent(in) :: matrix_type
        integer(int32), allocatable, intent(inout) :: row(:)
        integer(int32), allocatable, intent(inout) :: col(:)

        if (.not. MATRIX_TYPES%is_valid(matrix_type)) then
            return
        end if

        select case (matrix_type%ID)
        case (MATRIX_TYPES%COO%ID)
            call self%node_adjacency%get_coo(row, col)
        case (MATRIX_TYPES%CSR%ID)
            call self%node_adjacency%get_csr(row, col)
        end select
    end subroutine get_node_adjacency_domain

    subroutine get_fe_coordinate_domain(self, element_id, coordinates)
        implicit none
        class(type_domain), intent(in), target :: self
        integer(int32), intent(in) :: element_id
        real(real64), intent(inout), allocatable :: coordinates(:, :)

        integer(int32), pointer, contiguous :: connectivity(:)
        integer(int32) :: num_nodes, n_dim
        logical :: need_reallocate

        call self%get_fe_connectivity(element_id, connectivity)

        if (.not. associated(connectivity)) then
            return
        end if

        num_nodes = size(connectivity)
        call self%nodes%get_dimension(n_dim)

        need_reallocate = .true.

        if (allocated(coordinates)) then
            if (size(coordinates, 1) == n_dim .and. size(coordinates, 2) == num_nodes) then
                need_reallocate = .false.
            end if
        end if

        if (need_reallocate) then
            if (allocated(coordinates)) deallocate (coordinates)
            allocate (coordinates(n_dim, num_nodes))
        end if

        call self%nodes%get_coordinate(connectivity, coordinates)

    end subroutine get_fe_coordinate_domain

    subroutine get_target_dof_domain(self, physics_type, target_dof)
        implicit none
        class(type_domain), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        integer(int32), intent(inout) :: target_dof

        call self%dof_map%get_num_dofs_of_physics(physics_type, target_dof)

    end subroutine get_target_dof_domain

    subroutine get_material_id_domain(self, element_id, material_id)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32), intent(in) :: element_id
        integer(int32), intent(inout) :: material_id

        material_id = self%elements%fe_material_ids(element_id)
    end subroutine get_material_id_domain

    subroutine get_num_colors_domain(self, num_colors)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32), intent(inout) :: num_colors

        call self%elements%colors%get_num_colors(num_colors)
    end subroutine get_num_colors_domain

    subroutine get_colored_elements_domain(self, color_id, num_fe, elements)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32), intent(in) :: color_id
        integer(int32), intent(inout) :: num_fe
        integer(int32), pointer, contiguous, dimension(:), intent(inout) :: elements

        call self%elements%colors%get_colored_elements(color_id, num_fe, elements)
    end subroutine get_colored_elements_domain

    subroutine get_start_dof_index_domain(self, physics_id, start_dof_index)
        implicit none
        class(type_domain), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_id
        integer(int32), intent(inout) :: start_dof_index

        call self%dof_map%get_start_dof_index(physics_id, start_dof_index)

    end subroutine get_start_dof_index_domain

    subroutine get_bc_patch_domain(self, patch_id, boundary_patch)
        implicit none
        class(type_domain), intent(in), target :: self
        integer(int32), intent(in) :: patch_id
        type(type_boundary_patch), intent(inout), pointer :: boundary_patch

        call self%boundaries%get_bc_patch(patch_id, boundary_patch)
    end subroutine get_bc_patch_domain

    subroutine calc_measure_domain(self, element_id, measure)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32), intent(in) :: element_id
        real(real64), intent(inout) :: measure

        class(abst_fe), pointer :: fe
        real(real64), allocatable :: coordinates(:, :)

        call self%get_fe(element_id, fe)
        call self%get_fe_coordinate(element_id, coordinates)

        call fe%calc_measure(coordinates, measure)

    end subroutine calc_measure_domain

    subroutine lerp_1d_domain(self, element_id, r, value, lerped_value)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32), intent(in) :: element_id
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: value(:)
        real(real64), intent(inout) :: lerped_value

        class(abst_fe), pointer :: fe

        call self%get_fe(element_id, fe)

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

        call self%get_fe(element_id, fe)

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

        call self%get_fe(element_id, fe)

        call fe%lerp(r, value, lerped_value)

    end subroutine lerp_3d_domain

    subroutine dlerp_domain(self, element_id, r, values, dlerped_value)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32), intent(in) :: element_id
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: values(:)
        type(type_coordinate_dp) :: dlerped_value

        class(abst_fe), pointer :: fe
        real(real64), allocatable :: coordinates(:, :)

        call self%get_fe(element_id, fe)
        call self%get_fe_coordinate(element_id, coordinates)

        call fe%dlerp(r, values, coordinates, self%computation_type%ID, dlerped_value)

    end subroutine dlerp_domain

    subroutine find_element_domain(self, coordinate, coordinate_normalized, found_fe_id)
        implicit none
        class(type_domain), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: coordinate
        type(type_coordinate_dp), intent(inout) :: coordinate_normalized
        integer(int32), intent(inout) :: found_fe_id

        integer(int32) :: i
        integer(int32) :: num_fe
        type(type_coordinate_dp) :: cartesian, normalized
        class(abst_fe), pointer :: fe
        integer(int32), pointer, contiguous, dimension(:) :: conn => null()
        real(real64), allocatable :: element_coordinate(:, :)
        logical :: inside

        found_fe_id = -1

        ! 観測点座標セット (探索点)
        select case (self%computation_type%ID)
        case (COMP_TYPES%XY_2D%ID)
            call cartesian%set(coordinate%x, coordinate%y, 0.0d0)
        case (COMP_TYPES%XZ_2D%ID)
            call cartesian%set(coordinate%x, coordinate%z, 0.0d0)
        case (COMP_TYPES%XYZ_3D%ID)
            call cartesian%set(coordinate%x, coordinate%y, coordinate%z)
        end select

        call self%elements%get_num_fe(num_fe)

        do i = 1, num_fe
            call self%get_fe(i, fe)
            if (.not. associated(fe)) cycle
            call self%get_fe_connectivity(i, conn)
            if (.not. associated(conn)) cycle
            call self%get_fe_coordinate(i, element_coordinate)
            if (.not. allocated(element_coordinate)) cycle
            !                 ! 包含判定
            call fe%is_inside(cartesian, normalized, element_coordinate, inside)

            if (inside) then
                coordinate_normalized = normalized
                found_fe_id = i
                exit
            end if
        end do

    end subroutine find_element_domain

    subroutine get_config_domain(self, config)
        implicit none
        class(type_domain), intent(in) :: self
        type(type_config_observation_geometry), intent(inout) :: config

        integer(int32), pointer, contiguous, dimension(:) :: p_conn => null()
        class(abst_fe), pointer :: fe => null()

        call self%find_element(config%coordinate, config%coordinate_normalized, config%fe_id)
        call self%get_fe(config%fe_id, fe)
        config%fe => fe
        call self%get_fe_connectivity(config%fe_id, p_conn)
        call allocate_array(config%connectivity, p_conn)

    end subroutine get_config_domain

    ! --------------------------------------------------------------------------
    ! Display Procedures for Debugging
    ! --------------------------------------------------------------------------

    subroutine display_domain(self, unit_in)
        implicit none
        class(type_domain), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: unit

        unit = optval(unit_in, output_unit)

        write (unit, '(A)') '##  Domain Information'
        write (unit, '(A)') '| Property                | Value |'
        write (unit, '(A)') '|:------------------------|:------|'
#ifdef USE_MPI
        write (unit, '(A, I0, 2A)') '| MPI Rank / Procs        | ', self%my_rank, ' / '//achar(48 + self%num_procs), '|'
#else
        write (unit, '(A, A)') '| MPI Rank / Procs        | N/A |'
#endif
        write (unit, '(A, I0, 2A)') '| Computation Dimension   | ', self%computation_dimension, 'D', '|'
        write (unit, '(A, I0, A)') '| Coupling Mode           | ', self%coupling_mode, '|'
        write (unit, '(A)')

        call self%dof_map%display(unit)
        call self%nodes%display(unit)
        call self%elements%display(unit)
        call self%boundaries%display(unit)
    end subroutine display_domain

end module domain_manager

submodule(io_input_translator) translator_geometry
    implicit none
contains
    module subroutine execute_geometry_domain_nodes(self, input, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        type(type_config_nodes), intent(inout) :: config

        config%num_nodes = input%geometry%vtk%num_points

        if (allocated(config%points)) deallocate (config%points)

        allocate (config%points(3, config%num_nodes))

        config%points(1, :) = input%geometry%vtk%points%x(1:config%num_nodes)
        config%points(2, :) = input%geometry%vtk%points%y(1:config%num_nodes)
        config%points(3, :) = input%geometry%vtk%points%z(1:config%num_nodes)

        call allocate_array(config%global_node_ids, source=input%geometry%vtk%global_node_ids(1:config%num_nodes))

    end subroutine execute_geometry_domain_nodes

    !> Helper subroutine to build a config_elements object from a specific set of VTK cell indices
    subroutine build_config_elements(vtk_cells, target_indices, integration_order, config)
        implicit none
        class(type_vtk_cell), intent(in) :: vtk_cells(:)
        integer(int32), intent(in) :: target_indices(:)
        integer(int32), intent(in) :: integration_order
        type(type_config_elements), intent(inout) :: config

        integer(int32) :: i, idx, num_conn

        ! Count total connectivity size
        num_conn = 0
        do i = 1, size(target_indices)
            num_conn = num_conn + vtk_cells(target_indices(i))%num_nodes_in_cell
        end do

        ! Initialize and allocate
        config%num_elements = size(target_indices)
        config%integration_order = integration_order
        call allocate_array(config%fe_types, config%num_elements)
        call allocate_array(config%fe_material_ids, config%num_elements)
        call config%connectivity%initialize(config%num_elements + 1, num_conn)

        ! Store data
        config%connectivity%row_ptr(1) = 1
        do i = 1, config%num_elements
            idx = target_indices(i)
            config%fe_types(i) = vtk_cells(idx)%cell_type
            config%fe_material_ids(i) = vtk_cells(idx)%cell_entity_id

            config%connectivity%row_ptr(i + 1) = config%connectivity%row_ptr(i) + vtk_cells(idx)%num_nodes_in_cell
            config%connectivity%col_ind(config%connectivity%row_ptr(i):config%connectivity%row_ptr(i + 1) - 1) = &
                vtk_cells(idx)%connectivity(1:vtk_cells(idx)%num_nodes_in_cell)
        end do
    end subroutine build_config_elements

    module subroutine execute_geometry_domain_elements(self, input, config_elements, config_multicoloring)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        type(type_config_elements), intent(inout) :: config_elements
        type(type_config_multicoloring), intent(inout) :: config_multicoloring

        integer(int32) :: i, ind, cell_color
        integer(int32) :: num_total_cells, comp_dim, num_target_cells
        integer(int32), allocatable :: target_indices(:)
        integer(int32), allocatable :: counts_per_color(:), current_color_idx(:)

        comp_dim = input%basic%simulation_settings%calculate_dimension
        num_total_cells = input%geometry%vtk%num_total_cells

        ! ======================================================================
        ! Pass 1: Count target elements and max colors
        ! ======================================================================
        num_target_cells = 0
        config_multicoloring%num_colors = 0

        do i = 1, num_total_cells
            if (input%geometry%vtk%cells(i)%get_dimension() == comp_dim) then
                num_target_cells = num_target_cells + 1
                config_multicoloring%num_colors = max(config_multicoloring%num_colors, input%geometry%vtk%cells(i)%color)
            end if
        end do

        if (num_target_cells == 0) return

        ! ======================================================================
        ! Pass 2: Collect target indices and build config_elements
        ! ======================================================================
        call allocate_array(target_indices, num_target_cells)
        ind = 0
        do i = 1, num_total_cells
            if (input%geometry%vtk%cells(i)%get_dimension() == comp_dim) then
                ind = ind + 1
                target_indices(ind) = i
            end if
        end do

        call build_config_elements(input%geometry%vtk%cells, target_indices, &
                                   input%basic%geometry_settings%integration_order, config_elements)

        ! ======================================================================
        ! Pass 3: Multicoloring setup
        ! ======================================================================
        if (config_multicoloring%num_colors > 0) then
            call allocate_array(counts_per_color, config_multicoloring%num_colors)
            counts_per_color = 0

            ! Count elements per color
            do i = 1, num_target_cells
                cell_color = input%geometry%vtk%cells(target_indices(i))%color
                if (cell_color > 0) then
                    counts_per_color(cell_color) = counts_per_color(cell_color) + 1
                end if
            end do

            ! Allocate multicoloring arrays
            if (allocated(config_multicoloring%colored)) deallocate (config_multicoloring%colored)
            allocate (config_multicoloring%colored(config_multicoloring%num_colors))

            do i = 1, config_multicoloring%num_colors
                config_multicoloring%colored(i)%num_elements = counts_per_color(i)
                if (counts_per_color(i) > 0) then
                    allocate (config_multicoloring%colored(i)%elements(counts_per_color(i)))
                end if
            end do

            ! Store elements per color
            call allocate_array(current_color_idx, config_multicoloring%num_colors)
            current_color_idx = 0

            do i = 1, num_target_cells
                cell_color = input%geometry%vtk%cells(target_indices(i))%color
                if (cell_color > 0) then
                    current_color_idx(cell_color) = current_color_idx(cell_color) + 1
                    ! Store the local index (1 to num_target_cells), not the global VTK index
                    config_multicoloring%colored(cell_color)%elements(current_color_idx(cell_color)) = i
                end if
            end do

            call deallocate_array(counts_per_color)
            call deallocate_array(current_color_idx)
        end if

        call deallocate_array(target_indices)

    end subroutine execute_geometry_domain_elements

    module subroutine execute_geometry_domain_boundaries(self, input, config_elements)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        type(type_config_elements), intent(inout), allocatable :: config_elements(:)

        integer(int32) :: i, j, ind, num_total_cells, bnd_dim
        integer(int32), allocatable :: temp_bnd_ids(:), unique_bnd_ids(:)
        integer(int32), allocatable :: elem_counts(:), target_indices(:)

        num_total_cells = input%geometry%vtk%num_total_cells
        bnd_dim = input%basic%simulation_settings%calculate_dimension - 1

        ! ======================================================================
        ! Pass 1: Collect unique entity IDs for boundary cells
        ! ======================================================================
        call allocate_array(temp_bnd_ids, num_total_cells)
        ind = 0
        do i = 1, num_total_cells
            if (input%geometry%vtk%cells(i)%get_dimension() == bnd_dim) then
                ind = ind + 1
                temp_bnd_ids(ind) = input%geometry%vtk%cells(i)%cell_entity_id
            end if
        end do

        if (ind == 0) then
            if (allocated(config_elements)) deallocate (config_elements)
            return
        end if

        call unique(temp_bnd_ids(1:ind), unique_bnd_ids)
        call deallocate_array(temp_bnd_ids)

        ! ======================================================================
        ! Pass 2: Count elements per unique boundary ID
        ! ======================================================================
        if (allocated(config_elements)) deallocate (config_elements)
        allocate (config_elements(size(unique_bnd_ids)))

        call allocate_array(elem_counts, size(unique_bnd_ids))
        elem_counts = 0

        do i = 1, num_total_cells
            if (input%geometry%vtk%cells(i)%get_dimension() == bnd_dim) then
                do j = 1, size(unique_bnd_ids)
                    if (unique_bnd_ids(j) == input%geometry%vtk%cells(i)%cell_entity_id) then
                        elem_counts(j) = elem_counts(j) + 1
                        exit
                    end if
                end do
            end if
        end do

        ! ======================================================================
        ! Pass 3: Build config_elements for each boundary
        ! ======================================================================
        do j = 1, size(unique_bnd_ids)
            call allocate_array(target_indices, elem_counts(j))
            ind = 0
            do i = 1, num_total_cells
                if (input%geometry%vtk%cells(i)%get_dimension() == bnd_dim .and. &
                    input%geometry%vtk%cells(i)%cell_entity_id == unique_bnd_ids(j)) then
                    ind = ind + 1
                    target_indices(ind) = i
                end if
            end do

            call build_config_elements(input%geometry%vtk%cells, target_indices, &
                                       input%basic%geometry_settings%integration_order, config_elements(j))
            config_elements(j)%entity_id = unique_bnd_ids(j)

            call deallocate_array(target_indices)
        end do

        call deallocate_array(elem_counts)
        call deallocate_array(unique_bnd_ids)

    end subroutine execute_geometry_domain_boundaries

end submodule translator_geometry

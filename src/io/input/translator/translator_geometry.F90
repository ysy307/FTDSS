submodule(io_input_translator) translator_geometry
    use :: domain_mesh_plex, only:type_mesh_plex
    implicit none
contains
    module subroutine execute_geometry_domain_nodes(self, input, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        type(type_config_nodes), intent(inout) :: config

        associate (mesh => input%geometry%mesh)
            config%num_nodes = mesh%num_nodes

            if (allocated(config%points)) deallocate (config%points)
            allocate (config%points(3, config%num_nodes))
            call mesh%get_node_coordinates(config%points)

            call allocate_array(config%global_node_ids, source=mesh%node_global_id(1:config%num_nodes))
        end associate

    end subroutine execute_geometry_domain_nodes

    !> Build a config_elements object from a set of mesh cell indices.
    subroutine build_config_elements(mesh, target_indices, integration_order, config)
        implicit none
        class(type_mesh_plex), intent(in) :: mesh
        integer(int32), intent(in) :: target_indices(:)
        integer(int32), intent(in) :: integration_order
        type(type_config_elements), intent(inout) :: config

        integer(int32), allocatable :: connectivity(:)
        integer(int32) :: i, idx, num_conn, count

        num_conn = 0
        do i = 1, size(target_indices)
            num_conn = num_conn + mesh%cell_num_nodes(target_indices(i))
        end do

        config%num_elements = size(target_indices)
        config%integration_order = integration_order
        call allocate_array(config%fe_types, config%num_elements)
        call allocate_array(config%fe_material_ids, config%num_elements)
        call config%connectivity%initialize(config%num_elements + 1, num_conn)

        config%connectivity%row_ptr(1) = 1
        do i = 1, config%num_elements
            idx = target_indices(i)
            config%fe_types(i) = mesh%cell_fe_type(idx)
            config%fe_material_ids(i) = mesh%cell_entity_id(idx)

            call mesh%get_cell_connectivity(idx, connectivity, count)

            config%connectivity%row_ptr(i + 1) = config%connectivity%row_ptr(i) + count
            config%connectivity%col_ind(config%connectivity%row_ptr(i):config%connectivity%row_ptr(i + 1) - 1) = &
                connectivity(1:count)
        end do

        if (allocated(connectivity)) deallocate (connectivity)
    end subroutine build_config_elements

    module subroutine execute_geometry_domain_elements(self, input, config_elements, config_multicoloring)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        type(type_config_elements), intent(inout) :: config_elements
        type(type_config_multicoloring), intent(inout) :: config_multicoloring

        integer(int32) :: i, ind, cell_color
        integer(int32) :: comp_dim, num_target_cells
        integer(int32), allocatable :: target_indices(:)
        integer(int32), allocatable :: counts_per_color(:), current_color_idx(:)

        comp_dim = input%basic%simulation_settings%calculate_dimension

        associate (mesh => input%geometry%mesh)
            num_target_cells = 0
            config_multicoloring%num_colors = 0

            do i = 1, mesh%num_cells
                if (mesh%cell_dimension(i) /= comp_dim) cycle
                num_target_cells = num_target_cells + 1
                config_multicoloring%num_colors = max(config_multicoloring%num_colors, mesh%cell_color(i))
            end do

            if (num_target_cells == 0) return

            call allocate_array(target_indices, num_target_cells)
            ind = 0
            do i = 1, mesh%num_cells
                if (mesh%cell_dimension(i) /= comp_dim) cycle
                ind = ind + 1
                target_indices(ind) = i
            end do

            call build_config_elements(mesh, target_indices, &
                                       input%basic%geometry_settings%integration_order, config_elements)

            if (config_multicoloring%num_colors > 0) then
                call allocate_array(counts_per_color, config_multicoloring%num_colors)
                counts_per_color = 0

                do i = 1, num_target_cells
                    cell_color = mesh%cell_color(target_indices(i))
                    if (cell_color > 0) counts_per_color(cell_color) = counts_per_color(cell_color) + 1
                end do

                if (allocated(config_multicoloring%colored)) deallocate (config_multicoloring%colored)
                allocate (config_multicoloring%colored(config_multicoloring%num_colors))

                do i = 1, config_multicoloring%num_colors
                    config_multicoloring%colored(i)%num_elements = counts_per_color(i)
                    if (counts_per_color(i) > 0) then
                        allocate (config_multicoloring%colored(i)%elements(counts_per_color(i)))
                    end if
                end do

                call allocate_array(current_color_idx, config_multicoloring%num_colors)
                current_color_idx = 0

                do i = 1, num_target_cells
                    cell_color = mesh%cell_color(target_indices(i))
                    if (cell_color <= 0) cycle
                    current_color_idx(cell_color) = current_color_idx(cell_color) + 1
                    ! The element index within this config, not the mesh cell index.
                    config_multicoloring%colored(cell_color)%elements(current_color_idx(cell_color)) = i
                end do

                call deallocate_array(counts_per_color)
                call deallocate_array(current_color_idx)
            end if

            call deallocate_array(target_indices)
        end associate

    end subroutine execute_geometry_domain_elements

    module subroutine execute_geometry_domain_boundaries(self, input, config_elements)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        type(type_config_elements), intent(inout), allocatable :: config_elements(:)

        integer(int32) :: i, j, ind, bnd_dim
        integer(int32), allocatable :: temp_bnd_ids(:), unique_bnd_ids(:)
        integer(int32), allocatable :: elem_counts(:), target_indices(:)

        bnd_dim = input%basic%simulation_settings%calculate_dimension - 1

        associate (mesh => input%geometry%mesh)
            call allocate_array(temp_bnd_ids, mesh%num_cells)
            ind = 0
            do i = 1, mesh%num_cells
                if (mesh%cell_dimension(i) /= bnd_dim) cycle
                ind = ind + 1
                temp_bnd_ids(ind) = mesh%cell_entity_id(i)
            end do

            if (ind == 0) then
                if (allocated(config_elements)) deallocate (config_elements)
                return
            end if

            call unique(temp_bnd_ids(1:ind), unique_bnd_ids)
            call deallocate_array(temp_bnd_ids)

            if (allocated(config_elements)) deallocate (config_elements)
            allocate (config_elements(size(unique_bnd_ids)))

            call allocate_array(elem_counts, size(unique_bnd_ids))
            elem_counts = 0

            do i = 1, mesh%num_cells
                if (mesh%cell_dimension(i) /= bnd_dim) cycle
                do j = 1, size(unique_bnd_ids)
                    if (unique_bnd_ids(j) == mesh%cell_entity_id(i)) then
                        elem_counts(j) = elem_counts(j) + 1
                        exit
                    end if
                end do
            end do

            do j = 1, size(unique_bnd_ids)
                call allocate_array(target_indices, elem_counts(j))
                ind = 0
                do i = 1, mesh%num_cells
                    if (mesh%cell_dimension(i) /= bnd_dim) cycle
                    if (mesh%cell_entity_id(i) /= unique_bnd_ids(j)) cycle
                    ind = ind + 1
                    target_indices(ind) = i
                end do

                call build_config_elements(mesh, target_indices, &
                                           input%basic%geometry_settings%integration_order, config_elements(j))
                config_elements(j)%entity_id = unique_bnd_ids(j)

                call deallocate_array(target_indices)
            end do

            call deallocate_array(elem_counts)
            call deallocate_array(unique_bnd_ids)
        end associate

    end subroutine execute_geometry_domain_boundaries

end submodule translator_geometry

submodule(io_input_translator) translator_geometry
    implicit none
contains

    module subroutine execute_geometry_domain_elements(self, input, config_elements, config_multicoloring)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        class(type_config_elements), intent(inout) :: config_elements
        class(type_config_multicoloring), intent(inout) :: config_multicoloring

        integer(int32) :: i, ind, cell_color
        integer(int32) :: num_total_cells, num_total_connectivity
        integer(int32) :: comp_dim
        integer(int32), allocatable :: counts_per_color(:)
        integer(int32), allocatable :: current_color_idx(:)

        comp_dim = input%basic%simulation_settings%calculate_dimension
        num_total_cells = input%geometry%vtk%num_total_cells
        config_elements%integration_order = input%basic%geometry_settings%integration_order

        ! ======================================================================
        ! Pass 1: Count elements, connectivity size, and max colors
        ! ======================================================================
        config_elements%num_elements = 0
        num_total_connectivity = 0
        config_multicoloring%num_colors = 0

        do i = 1, num_total_cells
            if (input%geometry%vtk%cells(i)%get_dimension() == comp_dim) then
                config_elements%num_elements = config_elements%num_elements + 1
                num_total_connectivity = num_total_connectivity + input%geometry%vtk%cells(i)%num_nodes_in_cell
                config_multicoloring%num_colors = max(config_multicoloring%num_colors, input%geometry%vtk%cells(i)%color)
            end if
        end do

        if (config_elements%num_elements == 0) return

        ! ======================================================================
        ! Pass 2: Count elements per color
        ! ======================================================================
        call allocate_array(counts_per_color, config_multicoloring%num_colors)
        counts_per_color = 0

        do i = 1, num_total_cells
            if (input%geometry%vtk%cells(i)%get_dimension() == comp_dim) then
                cell_color = input%geometry%vtk%cells(i)%color
                if (cell_color > 0) then
                    counts_per_color(cell_color) = counts_per_color(cell_color) + 1
                end if
            end if
        end do

        ! ======================================================================
        ! Pass 3: Allocate memory
        ! ======================================================================
        call allocate_array(config_elements%fe_types, config_elements%num_elements)
        call allocate_array(config_elements%fe_material_ids, config_elements%num_elements)

        call config_elements%connectivity%initialize(config_elements%num_elements + 1, num_total_connectivity)

        if (allocated(config_multicoloring%colored)) deallocate (config_multicoloring%colored)
        allocate (config_multicoloring%colored(config_multicoloring%num_colors))

        do i = 1, config_multicoloring%num_colors
            config_multicoloring%colored(i)%num_elements = counts_per_color(i)
            if (config_multicoloring%colored(i)%num_elements > 0) then
                allocate (config_multicoloring%colored(i)%elements(config_multicoloring%colored(i)%num_elements))
            end if
        end do
        call deallocate_array(counts_per_color)

        ! ======================================================================
        ! Pass 4: Store element data and multicoloring indices
        ! ======================================================================
        call allocate_array(current_color_idx, config_multicoloring%num_colors)
        current_color_idx = 0

        config_elements%connectivity%row_ptr(1) = 1
        ind = 0

        do i = 1, num_total_cells
            if (input%geometry%vtk%cells(i)%get_dimension() == comp_dim) then
                ind = ind + 1

                config_elements%fe_types(ind) = input%geometry%vtk%cells(i)%cell_type
                config_elements%fe_material_ids(ind) = input%geometry%vtk%cells(i)%cell_entity_id

                config_elements%connectivity%row_ptr(ind + 1) = config_elements%connectivity%row_ptr(ind) + input%geometry%vtk%cells(i)%num_nodes_in_cell
                config_elements%connectivity%col_ind(config_elements%connectivity%row_ptr(ind):config_elements%connectivity%row_ptr(ind + 1) - 1) = &
                    input%geometry%vtk%cells(i)%connectivity(1:input%geometry%vtk%cells(i)%num_nodes_in_cell)

                cell_color = input%geometry%vtk%cells(i)%color
                if (cell_color > 0) then
                    current_color_idx(cell_color) = current_color_idx(cell_color) + 1
                    config_multicoloring%colored(cell_color)%elements(current_color_idx(cell_color)) = ind
                end if
            end if
        end do

        call deallocate_array(current_color_idx)

    end subroutine execute_geometry_domain_elements

end submodule translator_geometry

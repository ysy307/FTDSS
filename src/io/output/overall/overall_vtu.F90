submodule(io_output_overall) output_overall_vtu
    implicit none

contains
    module subroutine initialize_output_overall_vtu(self, input, domain)
        implicit none
        class(type_output_overall), intent(inout) :: self
        type(type_input), intent(in) :: input
        type(type_domain), intent(inout) :: domain

        integer(int32) :: i, j
        integer(int32) :: total_connectivity_size, current_offset, start_index

        self%vtk%num_points = input%geometry%vtk%num_points
        self%vtk%num_cells = input%geometry%vtk%num_total_cells
        call self%vtk%coordinate%initialize(self%vtk%num_points)
        self%vtk%coordinate = input%geometry%vtk%POINTS

        call allocate_array(self%vtk%offsets, self%vtk%num_cells)
        call allocate_array(self%vtk%cell_types, self%vtk%num_cells)

        current_offset = 0
        do i = 1, self%vtk%num_cells
            self%vtk%cell_types(i) = input%geometry%vtk%CELLS(i)%cell_type
            current_offset = current_offset + input%geometry%vtk%CELLS(i)%num_nodes_in_cell
            self%vtk%offsets(i) = current_offset
        end do

        if (self%vtk%num_cells > 0) then
            total_connectivity_size = self%vtk%offsets(self%vtk%num_cells)
        else
            total_connectivity_size = 0
        end if
        call allocate_array(self%VTK%connectivities, total_connectivity_size)

        do i = 1, self%vtk%num_cells
            if (i == 1) then
                start_index = 0
            else
                start_index = self%vtk%offsets(i - 1)
            end if

            do j = 1, input%geometry%vtk%CELLS(i)%num_nodes_in_cell
                self%VTK%connectivities(start_index + j) = input%geometry%vtk%CELLS(i)%connectivity(j) - 1
            end do
        end do

        if (associated(self%write_fields)) nullify (self%write_fields)
        self%write_fields => output_overall_vtu_fields

        if (associated(self%write_cell)) nullify (self%write_cell)
        self%write_cell => output_overall_vtu_cell

    end subroutine initialize_output_overall_vtu

    subroutine output_overall_vtu_fields(self, file_counts, domain, porosity, temperature, si, pressure, water_flux)
        implicit none
        class(type_output_overall), intent(inout) :: self
        integer(int32), intent(in) :: file_counts
        type(type_domain), intent(in) :: domain
        real(real64), intent(in), optional :: porosity(:)
        real(real64), intent(in), optional :: temperature(:)
        real(real64), intent(in), optional :: si(:)
        real(real64), intent(in), optional :: pressure(:)
        type(type_coordinate_array_dp), intent(in), optional :: water_flux

        type(vtk_file) :: vtu
        integer(int32) :: status
        integer(int32) :: unit_num
        integer(int32) :: i
        character(256) :: output_name

        write (output_name, self%format_output) trim(self%dir_output_field), "Out_", file_counts, self%file_extension

        status = vtu%initialize(format='binary', filename=trim(output_name), mesh_topology='UnstructuredGrid')

        status = vtu%xml_writer%write_piece(np=self%vtk%num_points, nc=self%vtk%num_cells)
        status = vtu%xml_writer%write_geo(np=self%vtk%num_points, &
                                          nc=self%vtk%num_cells, &
                                          x=self%vtk%coordinate%x, &
                                          y=self%vtk%coordinate%y, &
                                          z=self%vtk%coordinate%z)
        status = vtu%xml_writer%write_connectivity(nc=self%vtk%num_cells, &
                                                   connectivity=self%VTK%connectivities, &
                                                   offset=self%vtk%offsets, &
                                                   cell_type=self%vtk%cell_types)

        do i = 1, size(self%variable_names)
            if (i == 1) status = vtu%xml_writer%write_dataarray(location='node', action='open')
            select case (self%variable_names(i))
            case ("temperature")
                if (present(temperature)) status = vtu%xml_writer%write_dataarray(data_name='Temperature', x=temperature)
            case ("ice_saturation")
                if (present(si)) status = vtu%xml_writer%write_dataarray(data_name='Si', x=si)
            case ("thermal_conductivity")
                print *, "Warning: 'thermal_conductivity' is not implemented in VTK output."
            case ("volumetric_heat_capacity")
                print *, "Warning: 'volumetric_heat_capacity' is not implemented in VTK output."
            case ("pressure")
                if (present(pressure)) status = vtu%xml_writer%write_dataarray(data_name='Pressure', x=pressure)
            case ("water_flux")
                if (present(water_flux)) status = vtu%xml_writer%write_dataarray(data_name='waterFlux', &
                                                                                 x=water_flux%x, y=water_flux%y, z=water_flux%z)
            case ("hydraulic_conductivity")
                print *, "Warning: 'hydraulic_conductivity' is not implemented in VTK output."
            end select
        end do
        status = vtu%xml_writer%write_dataarray(location='node', action='close')
        status = vtu%xml_writer%write_piece()

        status = vtu%finalize()

    end subroutine output_overall_vtu_fields

    subroutine output_overall_vtu_cell(self, file_name, variable_name, variable)
        implicit none
        class(type_output_overall), intent(inout) :: self
        character(*), intent(in) :: file_name
        character(*), intent(in) :: variable_name
        integer(int32), intent(in) :: variable(:)

        type(vtk_file) :: vtu
        integer(int32) :: status

        status = vtu%initialize(format='binary', filename=trim(self%dir_output_field)//trim(file_name)//trim(self%file_extension), &
                                mesh_topology='UnstructuredGrid')

        status = vtu%xml_writer%write_piece(np=self%vtk%num_points, nc=self%vtk%num_cells)
        status = vtu%xml_writer%write_geo(np=self%vtk%num_points, &
                                          nc=self%vtk%num_cells, &
                                          x=self%vtk%coordinate%x, &
                                          y=self%vtk%coordinate%y, &
                                          z=self%vtk%coordinate%z)
        status = vtu%xml_writer%write_connectivity(nc=self%vtk%num_cells, &
                                                   connectivity=self%VTK%connectivities, &
                                                   offset=self%vtk%offsets, &
                                                   cell_type=self%vtk%cell_types)

        status = vtu%xml_writer%write_dataarray(location='cell', action='open')
        status = vtu%xml_writer%write_dataarray(data_name=variable_name, x=variable)
        status = vtu%xml_writer%write_dataarray(location='cell', action='close')
        status = vtu%xml_writer%write_piece()

        status = vtu%finalize()

    end subroutine output_overall_vtu_cell

end submodule output_overall_vtu

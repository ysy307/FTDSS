submodule(io_output_overall) output_overall_vtu
    implicit none

contains
    module subroutine initialize_type_output_overall_vtu(self, dir_output, config)
        implicit none
        class(type_output_overall_vtu), intent(inout) :: self
        character(*), intent(in) :: dir_output
        type(type_config_overall), intent(in) :: config

        self%dir_output_field = dir_output
        self%file_format = config%file_format
        self%format_output_file = config%format_output_file

        if (allocated(self%variables)) deallocate (self%variables)
        if (allocated(config%output_variables)) then
            allocate (self%variables(size(config%output_variables)))
            self%variables = config%output_variables
        end if

        self%num_points = config%num_points
        self%num_cells = config%num_cells
        self%coordinate = config%coordinate
        call allocate_array(self%connectivities, source=config%connectivities)
        call allocate_array(self%offsets, source=config%offsets)
        call allocate_array(self%cell_types, source=config%cell_types)

    end subroutine initialize_type_output_overall_vtu

    module subroutine write_fields_vtu(self, file_counts, temperature, water_content, &
                                       ice_content, vapor_content, pressure, water_flux)
        implicit none
        class(type_output_overall_vtu), intent(inout) :: self
        integer(int32), intent(in) :: file_counts
        real(real64), intent(in), optional :: temperature(:)
        real(real64), intent(in), optional :: water_content(:)
        real(real64), intent(in), optional :: ice_content(:)
        real(real64), intent(in), optional :: vapor_content(:)
        real(real64), intent(in), optional :: pressure(:)
        type(type_coordinate_array_dp), intent(in), optional :: water_flux

        type(vtk_file) :: vtu
        integer(int32) :: status
        integer(int32) :: unit_num
        integer(int32) :: i
        character(256) :: output_name

        write (output_name, self%format_output_file) &
            strip(self%dir_output_field), "Out_", file_counts, ".", to_lower(strip(self%file_format%NAME))

        status = vtu%initialize(format='binary', filename=strip(output_name), mesh_topology='UnstructuredGrid')

        status = vtu%xml_writer%write_piece(np=self%num_points, nc=self%num_cells)
        status = vtu%xml_writer%write_geo(np=self%num_points, &
                                          nc=self%num_cells, &
                                          x=self%coordinate%x, &
                                          y=self%coordinate%y, &
                                          z=self%coordinate%z)
        status = vtu%xml_writer%write_connectivity(nc=self%num_cells, &
                                                   connectivity=self%connectivities, &
                                                   offset=self%offsets, &
                                                   cell_type=self%cell_types)

        status = vtu%xml_writer%write_dataarray(location='node', action='open')
        do i = 1, size(self%variables)
            select case (self%variables(i)%ID)
            case (OUTPUT_VARIABLE_TYPES%TEMPERATURE%ID)
                if (present(temperature)) status = vtu%xml_writer%write_dataarray(data_name='Temperature', x=temperature)
            case (OUTPUT_VARIABLE_TYPES%WATER_CONTENT%ID)
                if (present(water_content)) status = vtu%xml_writer%write_dataarray(data_name='WaterContent', x=water_content)
            case (OUTPUT_VARIABLE_TYPES%ICE_CONTENT%ID)
                if (present(ice_content)) status = vtu%xml_writer%write_dataarray(data_name='IceContent', x=ice_content)
            case (OUTPUT_VARIABLE_TYPES%VAPOR_CONTENT%ID)
                if (present(vapor_content)) status = vtu%xml_writer%write_dataarray(data_name='VaporContent', x=vapor_content)
            case (OUTPUT_VARIABLE_TYPES%THERMAL_CONDUCTIVITY%ID)
                print *, "Warning: 'thermal_conductivity' is not implemented in VTK output."
            case (OUTPUT_VARIABLE_TYPES%VOLUMETRIC_HEAT_CAPACITY%ID)
                print *, "Warning: 'volumetric_heat_capacity' is not implemented in VTK output."
            case (OUTPUT_VARIABLE_TYPES%PRESSURE%ID)
                if (present(pressure)) status = vtu%xml_writer%write_dataarray(data_name='Pressure', x=pressure)
            case (OUTPUT_VARIABLE_TYPES%WATER_FLUX%ID)
                if (present(water_flux)) status = vtu%xml_writer%write_dataarray(data_name='waterFlux', &
                                                                                 x=water_flux%x, y=water_flux%y, z=water_flux%z)
            case (OUTPUT_VARIABLE_TYPES%HYDRAULIC_CONDUCTIVITY%ID)
                print *, "Warning: 'hydraulic_conductivity' is not implemented in VTK output."
            end select
        end do
        status = vtu%xml_writer%write_dataarray(location='node', action='close')
        status = vtu%xml_writer%write_piece()

        status = vtu%finalize()

    end subroutine write_fields_vtu

    module subroutine write_cell_vtu(self, file_name, variable_name, variable)
        implicit none
        class(type_output_overall_vtu), intent(inout) :: self
        character(*), intent(in) :: file_name
        character(*), intent(in) :: variable_name
        integer(int32), intent(in) :: variable(:)

        type(vtk_file) :: vtu
        integer(int32) :: status

        character(256) :: output_name

        write (output_name, self%format_output_file) &
            strip(self%dir_output_field), trim(file_name), ".", to_lower(strip(self%file_format%NAME))

        status = vtu%initialize(format='binary', filename=strip(output_name), mesh_topology='UnstructuredGrid')

        status = vtu%xml_writer%write_piece(np=self%num_points, nc=self%num_cells)
        status = vtu%xml_writer%write_geo(np=self%num_points, &
                                          nc=self%num_cells, &
                                          x=self%coordinate%x, &
                                          y=self%coordinate%y, &
                                          z=self%coordinate%z)
        status = vtu%xml_writer%write_connectivity(nc=self%num_cells, &
                                                   connectivity=self%connectivities, &
                                                   offset=self%offsets, &
                                                   cell_type=self%cell_types)

        status = vtu%xml_writer%write_dataarray(location='cell', action='open')
        status = vtu%xml_writer%write_dataarray(data_name=variable_name, x=variable)
        status = vtu%xml_writer%write_dataarray(location='cell', action='close')
        status = vtu%xml_writer%write_piece()

        status = vtu%finalize()

    end subroutine write_cell_vtu

end submodule output_overall_vtu

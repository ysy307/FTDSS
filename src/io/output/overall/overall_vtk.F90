submodule(io_output_overall) output_overall_vtk
    implicit none

contains

    module subroutine initialize_type_output_overall_vtk(self, dir_output, config)
        implicit none
        class(type_output_overall_vtk), intent(inout) :: self
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

    end subroutine initialize_type_output_overall_vtk

    module subroutine write_fields_vtk(self, file_counts, temperature, water_content, ice_content, &
                                       vapor_content, pressure, water_flux)
        implicit none
        class(type_output_overall_vtk), intent(inout) :: self
        integer(int32), intent(in) :: file_counts
        real(real64), intent(in), optional :: temperature(:)
        real(real64), intent(in), optional :: water_content(:)
        real(real64), intent(in), optional :: ice_content(:)
        real(real64), intent(in), optional :: vapor_content(:)
        real(real64), intent(in), optional :: pressure(:)
        type(type_coordinate_array_dp), intent(in), optional :: water_flux

        integer(int32) :: iostat
        integer(int32) :: unit
        integer(int32) :: iN, iE, idx, i

        character(256) :: output_name

        write (output_name, self%format_output_file) &
            strip(self%dir_output_field), "Out_", file_counts, ".", to_lower(strip(self%file_format%NAME))
        unit = open (strip(output_name), "wt", iostat=iostat)
        if (iostat /= 0) call raise_error(ERROR_CODES%OPEN_FILE_FAILED, opt=output_name)

        write (unit, '(a)') "# vtk DataFile Version 2.0"
        write (unit, '(a)') "Analysis ASCII VTK file"
        write (unit, '(a)') "ASCII"
        write (unit, '(a)') "DATASET UNSTRUCTURED_GRID"
        write (unit, '(a,i0,a)') "POINTS ", self%num_points, " double"

        do iN = 1, self%num_points
            write (unit, '(3(es22.15,1x))') self%coordinate%x(iN), self%coordinate%y(iN), self%coordinate%z(iN)
        end do
        write (unit, '(a)') ""

        write (unit, '(a,i0,1x,i0,a)') "CELLS ", self%num_cells, sum(self%offsets(:)) + self%num_cells
        idx = 1
        do iE = 1, self%num_cells
            write (unit, '(i0,'//to_string(self%offsets(iE))//'(1x,i0))') self%offsets(iE), &
                self%connectivities(idx:idx + self%offsets(iE) - 1)
            idx = idx + self%offsets(iE)
        end do
        write (unit, '(a)') ""

        write (unit, '(a,i0)') "CELL_TYPES ", self%num_cells
        write (unit, '(i0)') self%cell_types(:)
        write (unit, '(a)') ""

        write (unit, '(a, i0)') "POINT_DATA ", self%num_points
        do i = 1, size(self%variables)
            select case (self%variables(i)%ID)
            case (OUTPUT_VARIABLE_TYPES%TEMPERATURE%ID)
                if (present(temperature)) call self%output_field(unit, "Temperature", temperature)
            case (OUTPUT_VARIABLE_TYPES%WATER_CONTENT%ID)
                if (present(water_content)) call self%output_field(unit, "WaterContent", water_content)
            case (OUTPUT_VARIABLE_TYPES%ICE_CONTENT%ID)
                if (present(ice_content)) call self%output_field(unit, "IceContent", ice_content)
            case (OUTPUT_VARIABLE_TYPES%VAPOR_CONTENT%ID)
                if (present(vapor_content)) call self%output_field(unit, "VaporContent", vapor_content)
            case (OUTPUT_VARIABLE_TYPES%THERMAL_CONDUCTIVITY%ID)
                print *, "Warning: 'thermal_conductivity' is not implemented in VTK output."
            case (OUTPUT_VARIABLE_TYPES%VOLUMETRIC_HEAT_CAPACITY%ID)
                print *, "Warning: 'volumetric_heat_capacity' is not implemented in VTK output."
            case (OUTPUT_VARIABLE_TYPES%PRESSURE%ID)
                if (present(pressure)) call self%output_field(unit, "Pressure", pressure)
            case (OUTPUT_VARIABLE_TYPES%WATER_FLUX%ID)
                if (present(water_flux)) call self%output_field(unit, "waterFlux", water_flux%x, water_flux%y, water_flux%z)
            case (OUTPUT_VARIABLE_TYPES%HYDRAULIC_CONDUCTIVITY%ID)
                print *, "Warning: 'hydraulic_conductivity' is not implemented in VTK output."
            end select
        end do

        close (unit)

    end subroutine write_fields_vtk

    module subroutine write_cell_vtk(self, file_name, variable_name, variable)
        implicit none
        class(type_output_overall_vtk), intent(inout) :: self
        character(*), intent(in) :: file_name
        character(*), intent(in) :: variable_name
        integer(int32), intent(in) :: variable(:)

        integer(int32) :: iostat
        integer(int32) :: unit
        integer(int32) :: iN, iE, idx, i

        character(256) :: output_name

        write (output_name, self%format_output_file) &
            strip(self%dir_output_field), trim(file_name), ".", to_lower(strip(self%file_format%NAME))

        unit = open (strip(output_name), "wt", iostat=iostat)
        if (iostat /= 0) call raise_error(ERROR_CODES%OPEN_FILE_FAILED, opt=output_name)

        write (unit, '(a)') "# vtk DataFile Version 2.0"
        write (unit, '(a)') "Analysis ASCII VTK file"
        write (unit, '(a)') "ASCII"
        write (unit, '(a)') "DATASET UNSTRUCTURED_GRID"
        write (unit, '(a,i0,a)') "POINTS ", self%num_points, " double"

        do iN = 1, self%num_points
            write (unit, '(3(es22.15,1x))') self%coordinate%x(iN), self%coordinate%y(iN), self%coordinate%z(iN)
        end do
        write (unit, '(a)') ""

        write (unit, '(a,i0,1x,i0,a)') "CELLS ", self%num_cells, sum(self%offsets(:)) + self%num_cells
        idx = 1
        do iE = 1, self%num_cells
            write (unit, '(i0,'//to_string(self%offsets(iE))//'(1x,i0))') self%offsets(iE), &
                self%connectivities(idx:idx + self%offsets(iE) - 1)
            idx = idx + self%offsets(iE)
        end do
        write (unit, '(a)') ""

        write (unit, '(a,i0)') "CELL_TYPES ", self%num_cells
        write (unit, '(i0)') self%cell_types(:)
        write (unit, '(a)') ""

        write (unit, '(a,i0)') "CELL_DATA ", self%num_cells
        call self%output_cell(unit, variable_name, variable)

        close (unit)

    end subroutine write_cell_vtk

    module subroutine output_points_scalar_real64_vtk(self, unit, data_name, x)
        implicit none
        class(type_output_overall_vtk), intent(in) :: self
        integer(int32), intent(in) :: unit
        character(*), intent(in) :: data_name
        real(real64), intent(in) :: x(:)

        write (unit, '(3a)') "SCALARS ", strip(data_name), " double 1"
        write (unit, '(a)') "LOOKUP_TABLE default"
        write (unit, '(es22.15)') x(:)
        write (unit, '(a)') ""

    end subroutine output_points_scalar_real64_vtk

    module subroutine output_points_scalar_int32_vtk(self, unit, data_name, x)
        implicit none
        class(type_output_overall_vtk), intent(in) :: self
        integer(int32), intent(in) :: unit
        character(*), intent(in) :: data_name
        integer(int32), intent(in) :: x(:)

        write (unit, '(3a)') "SCALARS ", strip(data_name), " int 1"
        write (unit, '(a)') "LOOKUP_TABLE default"
        write (unit, '(i0)') x(:)
        write (unit, '(a)') ""

    end subroutine output_points_scalar_int32_vtk

    module subroutine output_points_vector_real64_vtk(self, unit, data_name, x, y, z)
        implicit none
        class(type_output_overall_vtk), intent(in) :: self
        integer(int32), intent(in) :: unit
        character(*), intent(in) :: data_name
        real(real64), intent(in) :: x(:), y(:), z(:)

        integer(int32) :: i

        write (unit, '(3a)') "VECTORS ", strip(data_name), " double"
        do i = 1, size(x)
            write (unit, '(3(es22.15,1x))') x(i), y(i), z(i)
        end do
        write (unit, '(a)') ""

    end subroutine output_points_vector_real64_vtk

    module subroutine output_points_vector_int32_vtk(self, unit, data_name, x, y, z)
        implicit none
        class(type_output_overall_vtk), intent(in) :: self
        integer(int32), intent(in) :: unit
        character(*), intent(in) :: data_name
        integer(int32), intent(in) :: x(:), y(:), z(:)

        integer(int32) :: i

        write (unit, '(3a)') "VECTORS ", strip(data_name), " double"
        do i = 1, size(x)
            write (unit, '(3(i0,1x))') x(i), y(i), z(i)
        end do
        write (unit, '(a)') ""

    end subroutine output_points_vector_int32_vtk

    module subroutine output_cell_real64_vtk(self, unit, data_name, x)
        implicit none
        class(type_output_overall_vtk), intent(in) :: self
        integer(int32), intent(in) :: unit
        character(*), intent(in) :: data_name
        real(real64), intent(in) :: x(:)

        write (unit, '(3a)') "SCALARS ", strip(data_name), " double 1"
        write (unit, '(a)') "LOOKUP_TABLE default"
        write (unit, '(es22.15)') x(:)
        write (unit, '(a)') ""

    end subroutine output_cell_real64_vtk

    module subroutine output_cell_int32_vtk(self, unit, data_name, x)
        implicit none
        class(type_output_overall_vtk), intent(in) :: self
        integer(int32), intent(in) :: unit
        character(*), intent(in) :: data_name
        integer(int32), intent(in) :: x(:)

        write (unit, '(3a)') "SCALARS ", strip(data_name), " double 1"
        write (unit, '(a)') "LOOKUP_TABLE default"
        write (unit, '(i0)') x(:)
        write (unit, '(a)') ""

    end subroutine output_cell_int32_vtk

end submodule output_overall_vtk

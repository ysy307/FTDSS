submodule(io_output_overall) output_overall_vtu
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none

contains
    module subroutine initialize_type_output_overall_vtu(self, dir_output, config)
        implicit none
        class(type_output_overall_vtu), intent(inout) :: self
        character(*), intent(in) :: dir_output
        type(type_config_overall), intent(in) :: config

        integer(int32) :: i
        integer(int32) :: conn_size

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

        conn_size = size(self%connectivities)

        if (allocated(self%points_xyz)) deallocate (self%points_xyz)
        allocate (self%points_xyz(3, self%num_points))
        self%points_xyz(1, :) = self%coordinate%x
        self%points_xyz(2, :) = self%coordinate%y
        self%points_xyz(3, :) = self%coordinate%z

        if (allocated(self%connectivity_c)) deallocate (self%connectivity_c)
        allocate (self%connectivity_c(conn_size))
        self%connectivity_c = int(self%connectivities, kind=c_int)

        if (allocated(self%offsets_vtk)) deallocate (self%offsets_vtk)
        allocate (self%offsets_vtk(self%num_cells))
        self%offsets_vtk(1) = int(self%offsets(1), kind=c_int)
        do i = 2, self%num_cells
            self%offsets_vtk(i) = self%offsets_vtk(i - 1) + int(self%offsets(i), kind=c_int)
        end do

        if (allocated(self%cell_types_c)) deallocate (self%cell_types_c)
        allocate (self%cell_types_c(self%num_cells))
        self%cell_types_c = int(self%cell_types, kind=c_int)

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

        type(type_vtu_writer) :: writer
        real(c_double), allocatable :: water_flux_vec(:, :)
        integer(int32) :: i
        character(256) :: output_name

        write (output_name, self%format_output_file) &
            strip(self%dir_output_field), "Out_", file_counts, ".", to_lower(strip(self%file_format%NAME))

        call writer%initialize(strip(output_name))
        call writer%write_mesh( &
            int(self%num_points, kind=c_int), &
            self%points_xyz, &
            int(self%num_cells, kind=c_int), &
            int(size(self%connectivity_c), kind=c_int), &
            self%connectivity_c, &
            self%offsets_vtk, &
            self%cell_types_c)

        do i = 1, size(self%variables)
            select case (self%variables(i)%ID)
            case (OUTPUT_VARIABLE_TYPES%TEMPERATURE%ID)
                if (present(temperature)) then
                    call assert_finite_real64_array(temperature, 'Temperature')
                    call writer%write_scalar_point_data( &
                        'Temperature', int(self%num_points, kind=c_int), temperature)
                end if
            case (OUTPUT_VARIABLE_TYPES%WATER_CONTENT%ID)
                if (present(water_content)) then
                    call assert_finite_real64_array(water_content, 'WaterContent')
                    call writer%write_scalar_point_data( &
                        'WaterContent', int(self%num_points, kind=c_int), water_content)
                end if
            case (OUTPUT_VARIABLE_TYPES%ICE_CONTENT%ID)
                if (present(ice_content)) then
                    call assert_finite_real64_array(ice_content, 'IceContent')
                    call writer%write_scalar_point_data( &
                        'IceContent', int(self%num_points, kind=c_int), ice_content)
                end if
            case (OUTPUT_VARIABLE_TYPES%VAPOR_CONTENT%ID)
                if (present(vapor_content)) then
                    call assert_finite_real64_array(vapor_content, 'VaporContent')
                    call writer%write_scalar_point_data( &
                        'VaporContent', int(self%num_points, kind=c_int), vapor_content)
                end if
            case (OUTPUT_VARIABLE_TYPES%THERMAL_CONDUCTIVITY%ID)
                print *, "Warning: 'thermal_conductivity' is not implemented in VTK output."
            case (OUTPUT_VARIABLE_TYPES%VOLUMETRIC_HEAT_CAPACITY%ID)
                print *, "Warning: 'volumetric_heat_capacity' is not implemented in VTK output."
            case (OUTPUT_VARIABLE_TYPES%PRESSURE%ID)
                if (present(pressure)) then
                    call assert_finite_real64_array(pressure, 'Pressure')
                    call writer%write_scalar_point_data( &
                        'Pressure', int(self%num_points, kind=c_int), pressure)
                end if
            case (OUTPUT_VARIABLE_TYPES%WATER_FLUX%ID)
                if (present(water_flux)) then
                    allocate (water_flux_vec(3, self%num_points))
                    water_flux_vec(1, :) = water_flux%x
                    water_flux_vec(2, :) = water_flux%y
                    water_flux_vec(3, :) = water_flux%z
                    call assert_finite_real64_array(water_flux_vec(1, :), 'waterFlux_x')
                    call assert_finite_real64_array(water_flux_vec(2, :), 'waterFlux_y')
                    call assert_finite_real64_array(water_flux_vec(3, :), 'waterFlux_z')
                    call writer%write_vector_point_data( &
                        'waterFlux', int(self%num_points, kind=c_int), water_flux_vec)
                end if
            case (OUTPUT_VARIABLE_TYPES%HYDRAULIC_CONDUCTIVITY%ID)
                print *, "Warning: 'hydraulic_conductivity' is not implemented in VTK output."
            end select
        end do

        call writer%write()

        if (allocated(water_flux_vec)) deallocate (water_flux_vec)
        call writer%finalize()

    contains

        subroutine assert_finite_real64_array(values, label)
            real(real64), intent(in) :: values(:)
            character(*), intent(in) :: label

            integer(int32) :: k
            do k = 1, size(values)
                if (.not. ieee_is_finite(values(k))) then
                    write (*, '(a,1x,a,1x,a,1x,i0)') 'Error: non-finite value detected during VTU output.', &
                        'variable =', strip(label), k
                    error stop 1
                end if
            end do
        end subroutine assert_finite_real64_array

    end subroutine write_fields_vtu

    module subroutine write_cell_vtu(self, file_name, variable_name, variable)
        implicit none
        class(type_output_overall_vtu), intent(inout) :: self
        character(*), intent(in) :: file_name
        character(*), intent(in) :: variable_name
        integer(int32), intent(in) :: variable(:)

        type(type_vtu_writer) :: writer
        real(c_double), allocatable :: cell_scalar(:)

        character(256) :: output_name

        write (output_name, self%format_output_file) &
            strip(self%dir_output_field), strip(file_name), ".", to_lower(strip(self%file_format%NAME))

        call writer%initialize(strip(output_name))
        call writer%write_mesh( &
            int(self%num_points, kind=c_int), &
            self%points_xyz, &
            int(self%num_cells, kind=c_int), &
            int(size(self%connectivity_c), kind=c_int), &
            self%connectivity_c, &
            self%offsets_vtk, &
            self%cell_types_c)

        allocate (cell_scalar(self%num_cells))
        cell_scalar = real(variable, kind=c_double)
        call writer%write_scalar_cell_data( &
            strip(variable_name), int(self%num_cells, kind=c_int), cell_scalar)

        call writer%write()

        if (allocated(cell_scalar)) deallocate (cell_scalar)
        call writer%finalize()

    end subroutine write_cell_vtu

end submodule output_overall_vtu

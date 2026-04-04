submodule(io_input_translator) translator_output
    implicit none
contains

    module subroutine execute_output_field(self, input, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        type(type_config_output_manager), intent(inout) :: config

        associate (output => input%output_settings%field_output)
            config%interval_val = output%output_interval_step
            config%file_format = FILE_FORMATS%to_object(output%file_format)

            if (config%file_format == FILE_FORMATS%NONE) then
                config%interval_val = 0.0d0
                config%interval_unit = TIME_UNITS%to_object("second")
                config%output_unit = TIME_UNITS%to_object("second")
                return
            end if

            config%interval_unit = TIME_UNITS%to_object(output%output_interval_unit)
            config%output_unit = TIME_UNITS%to_object(output%output_time_unit)
        end associate

    end subroutine execute_output_field

    module subroutine execute_output_manager(self, input, output_type, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        type(type_constant_id), intent(in) :: output_type
        type(type_config_output_manager), intent(inout) :: config

        select case (output_type%ID)
        case (OUTPUT_TYPES%FIELD%ID)
            associate (output => input%output_settings%field_output)
                config%interval_val = output%output_interval_step
                config%file_format = FILE_FORMATS%to_object(output%file_format)

                if (config%file_format == FILE_FORMATS%NONE) then
                    config%interval_val = 0.0d0
                    config%interval_unit = TIME_UNITS%to_object("second")
                    config%output_unit = TIME_UNITS%to_object("second")
                    return
                end if

                config%interval_unit = TIME_UNITS%to_object(output%output_interval_unit)
                config%output_unit = TIME_UNITS%to_object(output%output_time_unit)
            end associate

        case (OUTPUT_TYPES%HISTORY%ID)
            associate (output => input%output_settings%history_output)
                config%interval_val = output%output_interval_step
                config%file_format = FILE_FORMATS%to_object(output%file_format)

                if (config%file_format == FILE_FORMATS%NONE) then
                    config%interval_val = 0.0d0
                    config%interval_unit = TIME_UNITS%to_object("second")
                    config%output_unit = TIME_UNITS%to_object("second")
                    return
                end if

                config%interval_unit = TIME_UNITS%to_object(output%output_interval_unit)
                config%output_unit = TIME_UNITS%to_object(output%output_time_unit)
            end associate

        case default
            ! Fall back to field output for backward compatibility.
            call self%execute_output_field(input, config)
        end select

    end subroutine execute_output_manager

    module subroutine execute_output_base(self, input, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        type(type_config_output), intent(inout) :: config

        call allocate_array(config%is_output_enabled, source=input%basic%analysis_controls%is_active)

    end subroutine execute_output_base

    module subroutine execute_output_observation(self, input, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        type(type_config_observation), intent(inout) :: config

        integer(int32) :: i

        associate (observation => input%output_settings%history_output)

            config%file_format = FILE_FORMATS%to_object(observation%file_format)

            if (config%file_format == FILE_FORMATS%NONE) then
                config%point_type = OUTPUT_OBSERVATION_TYPES%NONE
                config%num_observations = 0
                if (allocated(config%output_variables)) deallocate (config%output_variables)
                if (allocated(config%observation_geometries)) deallocate (config%observation_geometries)
                return
            end if

            config%point_type = OUTPUT_OBSERVATION_TYPES%to_object(observation%observation_type)
            config%num_observations = observation%num_observations
            call config%set(config%output_time_unit_name, observation%output_time_unit)

            ! Translate output variables
            if (allocated(observation%variable_names)) then
                if (allocated(config%output_variables)) deallocate (config%output_variables)
                allocate (config%output_variables(size(observation%variable_names)))
                do i = 1, size(observation%variable_names)
                    config%output_variables(i) = OUTPUT_VARIABLE_TYPES%to_object(observation%variable_names(i))
                end do
            end if

            ! Translate observation geometries
            if (allocated(config%observation_geometries)) deallocate (config%observation_geometries)

            select case (config%point_type%ID)
            case (OUTPUT_OBSERVATION_TYPES%NODE_IDS%ID)
                if (allocated(observation%node_ids)) then
                    allocate (config%observation_geometries(size(observation%node_ids)))
                    do i = 1, size(observation%node_ids)
                        config%observation_geometries(i)%node_id = observation%node_ids(i)
                    end do
                end if

            case (OUTPUT_OBSERVATION_TYPES%COORDINATES%ID)
                if (allocated(observation%coordinates)) then
                    allocate (config%observation_geometries(size(observation%coordinates)))
                    do i = 1, size(observation%coordinates)
                        config%observation_geometries(i)%coordinate = observation%coordinates(i)
                    end do
                end if
            end select

        end associate

    end subroutine execute_output_observation

    module subroutine execute_output_overall(self, input, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        type(type_config_overall), intent(inout) :: config

        integer(int32) :: i, j, idx, total
        real(real64) :: simulation_period_second
        real(real64) :: output_step_second
        integer(int32) :: max_file_counts_digit
        character(:), allocatable :: format_count
        type(type_constant_value) :: time_unit

        associate (field_output => input%output_settings%field_output)
            config%file_format = FILE_FORMATS%to_object(field_output%file_format)

            if (allocated(field_output%variable_names)) then
                if (allocated(config%output_variables)) deallocate (config%output_variables)
                allocate (config%output_variables(size(field_output%variable_names)))
                do i = 1, size(field_output%variable_names)
                    config%output_variables(i) = OUTPUT_VARIABLE_TYPES%to_object(field_output%variable_names(i))
                end do
            end if
        end associate

        associate (overall => input%geometry%vtk)
            config%num_points = overall%num_points
            config%num_cells = overall%num_total_cells
            config%coordinate = overall%POINTS

            call allocate_array(config%offsets, overall%num_total_cells)
            call allocate_array(config%cell_types, overall%num_total_cells)

            do i = 1, overall%num_total_cells
                config%offsets(i) = overall%CELLS(i)%num_nodes_in_cell
                config%cell_types(i) = int(overall%CELLS(i)%cell_type, int8)
            end do
            total = sum(config%offsets(:))

            call allocate_array(config%connectivities, total)
            idx = 0
            do i = 1, overall%num_total_cells
                do j = 1, overall%CELLS(i)%num_nodes_in_cell
                    idx = idx + 1
                    config%connectivities(idx) = overall%CELLS(i)%connectivity(j) - 1
                end do
            end do
        end associate

        ! --- Convert simulation period to seconds ---
        time_unit = TIME_UNITS%to_object(input%conditions%time_control%simulation_period%unit)

        simulation_period_second = (input%conditions%time_control%simulation_period%end &
                                    - input%conditions%time_control%simulation_period%start) * time_unit%value

        ! --- Convert output interval to seconds ---
        if (input%output_settings%field_output%output_interval_step > 0.0d0) then
            time_unit = TIME_UNITS%to_object(input%output_settings%field_output%output_interval_unit)

            output_step_second = input%output_settings%field_output%output_interval_step * time_unit%value

            if (output_step_second > 0.0d0) then
                max_file_counts_digit = int(log10(simulation_period_second / output_step_second), kind=int32) + 1
            else
                max_file_counts_digit = 1
            end if

            if (max_file_counts_digit < 1) max_file_counts_digit = 1

            format_count = "i"//strip(to_string(max_file_counts_digit))//"."//strip(to_string(max_file_counts_digit))
            config%format_output_file = "(2a,"//format_count//",2a)"
        end if

    end subroutine execute_output_overall

end submodule translator_output

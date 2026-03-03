submodule(io_input_translator) translator_output
    implicit none
contains

    module subroutine execute_output_field(self, input, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        class(type_config_output_manager), intent(inout) :: config

        select type (config)
        type is (type_config_output_manager)
            associate (output => input%output_settings%field_output)
                config%interval_val = output%output_interval_step
                config%interval_unit = TIME_UNITS%to_object(output%output_interval_unit)
                config%output_unit = TIME_UNITS%to_object(output%output_time_unit)
                config%file_format = FILE_FORMATS%to_object(output%file_format)
            end associate
        end select

    end subroutine execute_output_field

    module subroutine execute_output_observation(self, input, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        class(type_config_observation), intent(inout) :: config

        integer(int32) :: i

        select type (config)
        type is (type_config_observation)
            associate (observation => input%output_settings%history_output)

                config%file_format = FILE_FORMATS%to_object(observation%file_format)
                config%point_type = OUTPUT_OBSERVATION_TYPES%to_object(observation%observation_type)
                config%num_observations = observation%num_observations

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
        end select
    end subroutine execute_output_observation

end submodule translator_output

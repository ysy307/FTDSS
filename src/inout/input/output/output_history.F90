submodule(inout_input_output_conditions) inout_input_output_conditions_history
    implicit none
    !!------------------------------------------------------------------------------------------------------------------------------
    !! JSON key names for history output
    !!------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: history_output = "history_output"
    character(*), parameter :: valid_history_file_formats(3) = [character(len=8) :: "none", "dat", "csv"]
    character(*), parameter :: observation_type = "observation_type"
    character(*), parameter :: valid_observation_types(2) = [character(len=16) :: "node_ids", "coordinates"]
    character(*), parameter :: x = "x"
    character(*), parameter :: y = "y"
    character(*), parameter :: z = "z"

contains

    module subroutine read_output_settings_history(self, json)
        implicit none
        class(type_output_settings), intent(inout) :: self
        type(json_file), intent(inout) :: json

        character(256) :: buffer(3) = [character(256) :: history_output, "", ""]
        integer(int32) :: i
        logical :: found
        character(:), allocatable :: tmp_string

        buffer(2) = file_format
        call get_json_value(json, join(buffer), self%history_output%file_format, &
                            is_required=.true., valid_list=valid_history_file_formats)

        select case (self%history_output%file_format)
        case (valid_history_file_formats(1), valid_history_file_formats(2)) ! "dat", "csv"
            buffer(2) = observation_type
            call get_json_value(json, join(buffer), self%history_output%observation_type, &
                                is_required=.true., valid_list=valid_observation_types)

            buffer(2) = output_interval
            buffer(3) = unit
            call get_json_value(json, join(buffer), tmp_string, &
                                is_required=.true., valid_list=valid_time_units)
            self%field_output%output_interval_unit = get_time_unit(trim(tmp_string))
            buffer(3) = value
            call get_json_value(json, join(buffer), self%history_output%output_interval_step, is_required=.true., &
                                valid_range=[0.0d0, huge(0.0d0)])

            buffer(2) = variables
            buffer(3) = ""
            call get_json_value(json, join(buffer), self%history_output%variable_names, is_required=.true., &
                                valid_list=valid_variables_lists)

            select case (self%history_output%observation_type)
            case (valid_observation_types(1)) ! "node_ids"
                buffer(2) = valid_observation_types(1)
                buffer(3) = ""
                call get_json_value(json, join(buffer), self%history_output%node_ids, is_required=.true., &
                                    valid_range=[1, huge(0)])
                self%history_output%num_observations = size(self%history_output%node_ids)
            case (valid_observation_types(2)) ! "coordinates"
                buffer(2) = valid_observation_types(2)
                call json%info(join(buffer), found=found, n_children=self%history_output%num_observations)
                call json%print_error_message(output_unit)
                if (.not. found) then
                    call json%destroy()
                    call error_message(904, c_opt=join(buffer))
                else if (self%history_output%num_observations <= 0) then
                    call json%destroy()
                    call error_message(905, c_opt=join(buffer))
                end if
                allocate (self%history_output%coordinates(self%history_output%num_observations))

                do i = 1, self%history_output%num_observations
                    buffer(2) = valid_observation_types(2)//"("//trim(adjustl(to_string(i)))//")"
                    buffer(3) = x
                    call get_json_value(json, join(buffer), self%history_output%coordinates(i)%x, is_required=.true.)
                    buffer(3) = y
                    call get_json_value(json, join(buffer), self%history_output%coordinates(i)%y, is_required=.true.)
                    buffer(3) = z
                    call get_json_value(json, join(buffer), self%history_output%coordinates(i)%z, is_required=.true.)
                end do

            end select

        end select

    end subroutine read_output_settings_history

    module subroutine display_output_settings_history(self)
        implicit none
        class(types_history_output), intent(in) :: self
        integer(int32) :: i

        write (output_unit, '(A)') "  History Output Settings:"
        write (output_unit, '(A, A)') "    File Format: ", trim(self%file_format)
        select case (self%file_format)
        case (valid_history_file_formats(1), valid_history_file_formats(2)) ! "dat", "csv"
            write (output_unit, '(A, A, F8.3)') "    Output Interval: ", get_time_unit_string(self%output_interval_unit), &
                self%output_interval_step
            write (output_unit, '(A, A)') "    Observation Type: ", trim(self%observation_type)
            write (output_unit, '(A)') "    Variables:"
            do i = 1, size(self%variable_names)
                write (output_unit, '(A, A)') "      - ", trim(self%variable_names(i))
            end do
            write (output_unit, '(A, I8)') "    Number of Observations: ", self%num_observations
            select case (self%observation_type)
            case (valid_observation_types(1)) ! "node_ids"
                write (output_unit, '(A)') "    Node IDs:"
                do i = 1, self%num_observations
                    write (output_unit, '(A, I8)') "      - ", self%node_ids(i)
                end do
            case (valid_observation_types(2)) ! "coordinates"
                write (output_unit, '(A)') "    Coordinates:"
                do i = 1, self%num_observations
                    write (output_unit, '(A, 3F8.3)') "      - ", self%coordinates(i)%x, &
                        self%coordinates(i)%y, self%coordinates(i)%z
                end do
            end select
        end select

    end subroutine display_output_settings_history

end submodule inout_input_output_conditions_history

submodule(io_input_output_conditions) input_output_conditions_standard
    implicit none

    !!------------------------------------------------------------------------------------------------------------------------------
    !! JSON key names for standard output
    !!------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: standard_output = "standard_output"
    character(*), parameter :: print_progress = "print_progress"
    character(*), parameter :: print_interval = "print_interval"
    !!------------------------------------------------------------------------------------------------------------------------------
contains
    module subroutine read_output_settings_standard(self, json)
        implicit none
        class(type_output_settings), intent(inout) :: self
        type(json_file), intent(inout) :: json
        character(256) :: buffer(3) = [character(256) :: standard_output, "", ""]

        buffer(2) = print_progress
        call get_json_value(json, join(buffer), self%standard_output%print_progress, default_value=.true.)

        buffer(2) = print_interval
        buffer(3) = unit
        call get_json_value(json, join(buffer), self%standard_output%print_progress_unit, &
                            is_required=.true., valid_list=valid_time_units)
        buffer(3) = value
        call get_json_value(json, join(buffer), self%standard_output%print_progress_interval, is_required=.true., &
                            valid_range=[0.0d0, huge(0.0d0)])

    end subroutine read_output_settings_standard

    module subroutine display_output_settings_standard(self)
        implicit none
        class(type_standard_output), intent(in) :: self

        write (output_unit, '(A)') "  Standard Output Settings:"
        write (output_unit, '(A, L1)') "    Print Progress: ", self%print_progress
        write (output_unit, '(A, A, F8.3)') "    Print Interval: ", trim(self%print_progress_unit), &
            self%print_progress_interval

    end subroutine display_output_settings_standard

end submodule input_output_conditions_standard

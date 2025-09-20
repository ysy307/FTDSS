submodule(inout_input_output_conditions) inout_input_output_conditions_fields
    implicit none
    !!------------------------------------------------------------------------------------------------------------------------------
    !! JSON key names for field output
    !!------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: field_output = "field_output"
    character(*), parameter :: valid_field_file_formats(3) = [character(len=16) :: "none", "vtk", "vtu"]
    !!------------------------------------------------------------------------------------------------------------------------------
contains
    module subroutine read_output_settings_fields(self, json)
        implicit none
        class(type_output_settings), intent(inout) :: self
        type(json_file), intent(inout) :: json

        character(256) :: buffer(3) = [character(256) :: field_output, "", ""]

        buffer(2) = file_format
        call get_json_value(json, join(buffer), self%field_output%file_format, &
                            is_required=.true., valid_list=valid_field_file_formats)

        buffer(2) = output_interval
        buffer(3) = unit
        call get_json_value(json, join(buffer), self%field_output%output_interval_unit, &
                            is_required=.true., valid_list=valid_time_units)
        buffer(3) = value
        call get_json_value(json, join(buffer), self%field_output%output_interval_step, is_required=.true., &
                            valid_range=[0.0d0, huge(0.0d0)])

        buffer(2) = variables
        buffer(3) = ""
        call get_json_value(json, join(buffer), self%field_output%variable_names, is_required=.true., &
                            valid_list=valid_variables_lists)
    end subroutine read_output_settings_fields

    module subroutine display_output_settings_fields(self)
        implicit none
        class(type_field_output), intent(in) :: self
        integer(int32) :: i

        write (output_unit, '(A)') "  Field Output Settings:"
        write (output_unit, '(A, A)') "    File Format: ", trim(self%file_format)
        write (output_unit, '(A, A, F8.3)') "    Output Interval: ", trim(self%output_interval_unit), &
            self%output_interval_step
        write (output_unit, '(A)') "    Variables:"
        do i = 1, size(self%variable_names)
            write (output_unit, '(A, A)') "      - ", trim(self%variable_names(i))
        end do

    end subroutine display_output_settings_fields
end submodule inout_input_output_conditions_fields

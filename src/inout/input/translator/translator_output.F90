submodule(inout_input_translator) input_translator_output
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

end submodule input_translator_output

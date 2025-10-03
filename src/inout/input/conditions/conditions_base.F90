submodule(inout_input_conditions) inout_input_conditions_base
    implicit none
contains
    module subroutine initialize_type_conditions(self)
        implicit none
        class(type_conditions), intent(inout) :: self
        type(json_file) :: json

        call json%initialize()
        call json%load(filename=self%file_name)
        call json%print_error_message(output_unit)

        call self%read_time_controls(json)
        call self%read_boundary_conditions(json)
        call self%read_initial_conditions(json)

        call json%destroy()
        call json%print_error_message(output_unit)

    end subroutine initialize_type_conditions

    module subroutine display_conditions(self)
        implicit none
        class(type_conditions), intent(in) :: self
    end subroutine display_conditions

end submodule inout_input_conditions_base

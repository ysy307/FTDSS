submodule(inout_input_basic) inout_input_basic_base
    implicit none
contains

    module subroutine initialize_type_input_basic(self)
        !< Load the input parameters from the JSON file
        implicit none
        class(type_input_basic), intent(inout) :: self
        type(json_file) :: json

        call json%initialize()

        call json%load(filename=self%file_name)
        call json%print_error_message(output_unit)

        call read_parameters_simulation_settings(self, json)
        call read_parameters_analysis_controls(self, json)
        call read_parameters_geometry_settings(self, json)
        call read_parameters_materials(self, json)
        call read_parameters_solver_settings(self, json)

        call json%destroy()
        call json%print_error_message(output_unit)

    end subroutine initialize_type_input_basic

end submodule inout_input_basic_base

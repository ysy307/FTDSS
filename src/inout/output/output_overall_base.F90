submodule(input_output) input_output_overall_base
    implicit none

contains
    module subroutine initialize_input_type_output_overall(self, input, coordinate, domain, dir_output)
        implicit none
        class(type_output_overall), intent(inout) :: self
        type(Type_input), intent(in) :: input
        type(type_coordinate_array_dp), intent(in) :: coordinate
        type(type_domain), intent(inout) :: domain
        character(*), intent(in) :: dir_output

        real(real64) :: simulation_period_second
        real(real64) :: output_step_second
        integer(int32) :: max_file_counts_digit

        self%do_output = .false.
        select case (input%output_settings%field_output%file_format)
        case ("vtk")
            call self%initialize_vtk(input, coordinate, domain)
            self%do_output = .true.
        case ("vtu")
            call self%initialize_vtu(input, coordinate, domain)
            self%do_output = .true.
        end select

        if (allocated(self%variable_names)) deallocate (self%variable_names)
        allocate (self%variable_names, source=input%output_settings%field_output%variable_names)

        self%dir_output_field = trim(adjustl(dir_output))
        self%file_extension = "."//input%output_settings%field_output%file_format

        select case (input%conditions%time_control%simulation_period%unit)
        case ("second")
            simulation_period_second = (input%conditions%time_control%simulation_period%end &
                                        - input%conditions%time_control%simulation_period%start)
        case ("minute")
            simulation_period_second = (input%conditions%time_control%simulation_period%end &
                                        - input%conditions%time_control%simulation_period%start) * 60.0d0
        case ("hour")
            simulation_period_second = (input%conditions%time_control%simulation_period%end &
                                        - input%conditions%time_control%simulation_period%start) * 3600.0d0
        case ("day")
            simulation_period_second = (input%conditions%time_control%simulation_period%end &
                                        - input%conditions%time_control%simulation_period%start) * 86400.0d0
        case ("year")
            simulation_period_second = (input%conditions%time_control%simulation_period%end &
                                        - input%conditions%time_control%simulation_period%start) * 31536000.0d0
        end select

        if (allocated(input%output_settings%field_output%output_interval_unit)) then
            select case (input%output_settings%field_output%output_interval_unit)
            case ("second")
                output_step_second = input%output_settings%field_output%output_interval_step
            case ("minute")
                output_step_second = input%output_settings%field_output%output_interval_step * 60.0d0
            case ("hour")
                output_step_second = input%output_settings%field_output%output_interval_step * 3600.0d0
            case ("day")
                output_step_second = input%output_settings%field_output%output_interval_step * 86400.0d0
            case ("year")
                output_step_second = input%output_settings%field_output%output_interval_step * 31536000.0d0
            end select
            max_file_counts_digit = int(log10(simulation_period_second / output_step_second), kind=int32) + 1_int32
            if (max_file_counts_digit < 1) max_file_counts_digit = 1
            self%format_output = "(a,a,i"//to_string(max_file_counts_digit)//"."//to_string(max_file_counts_digit)//",a)"
        end if

    end subroutine initialize_input_type_output_overall

end submodule input_output_overall_base

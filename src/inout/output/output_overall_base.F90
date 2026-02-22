submodule(inout_output) inout_output_overall_base
    use :: iso_fortran_env, only:int32, real64
    use :: stdlib_strings, only:to_string
    use :: module_control, only:type_controls
    implicit none

contains

    module subroutine initialize_input_type_output_overall(self, input, control, domain, dir_output)
        implicit none
        class(type_output_overall), intent(inout) :: self
        type(type_input), intent(in) :: input
        type(type_controls), intent(in) :: control
        type(type_domain), intent(inout) :: domain
        character(*), intent(in) :: dir_output

        real(real64) :: simulation_period_second
        real(real64) :: output_step_second
        integer(int32) :: max_file_counts_digit

        type(type_constant_value) :: time_unit

        self%do_output = .false.

        select case (trim(input%output_settings%field_output%file_format))
        case ("vtk")
            ! coordinate 削除
            call self%initialize_vtk(input, domain)
            self%do_output = .true.
        case ("vtu")
            ! coordinate 削除
            call self%initialize_vtu(input, domain)
            self%do_output = .true.
        end select

        if (allocated(self%variable_names)) deallocate (self%variable_names)
        allocate (self%variable_names, source=input%output_settings%field_output%variable_names)

        self%dir_output_field = trim(adjustl(dir_output))
        self%file_extension = "."//trim(adjustl(input%output_settings%field_output%file_format))

        ! --- シミュレーション期間を秒単位に変換 ---
        time_unit = TIME_UNITS%to_object(input%conditions%time_control%simulation_period%unit)

        simulation_period_second = (input%conditions%time_control%simulation_period%end &
                                    - input%conditions%time_control%simulation_period%start) * time_unit%value

        ! --- 出力インターバルを秒単位に変換 ---
        if (input%output_settings%field_output%output_interval_step > 0.0d0) then
            time_unit = TIME_UNITS%to_object(input%output_settings%field_output%output_interval_unit)

            output_step_second = input%output_settings%field_output%output_interval_step * time_unit%value

            if (output_step_second > 0.0d0) then
                max_file_counts_digit = int(log10(simulation_period_second / output_step_second), kind=int32) + 1_int32
            else
                max_file_counts_digit = 1
            end if

            if (max_file_counts_digit < 1) max_file_counts_digit = 1

            self%format_output = "(a,a,i"//trim(to_string(max_file_counts_digit))//"."//trim(to_string(max_file_counts_digit))//",a)"
        end if

    end subroutine initialize_input_type_output_overall

end submodule inout_output_overall_base

submodule(inout_input_basic) inout_input_basic_simulation_setting
    implicit none
    !!------------------------------------------------------------------------------------------------------------------------------
    ! JSON key names for simulation settings
    !!------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: simulation_settins = "simulation_settings"
    character(*), parameter :: title = "title"
    character(*), parameter :: calculate_type = "calculate_type"
    integer(int32), parameter :: valid_calculation_types(2) = [1, 3]
contains
    module subroutine read_parameters_simulation_settings(self, json)
        implicit none
        class(type_input_basic), intent(inout) :: self
        type(json_file), intent(inout) :: json

        character(:), allocatable :: key
        character(256) :: buffer(2)
        logical :: found

        buffer(1) = simulation_settins
        buffer(2) = title
        call get_json_value(json, join(buffer), self%simulation_settings%title, default_value="FTDSS Simulation")

        buffer(2) = calculate_type
        call get_json_value(json, join(buffer), self%simulation_settings%calculate_type, &
                            is_required=.true., valid_range=valid_calculation_types)

        select case (self%simulation_settings%calculate_type)
        case (1:2)
            self%simulation_settings%calculate_dimension = 2
        case (3)
            self%simulation_settings%calculate_dimension = 3
        end select

    end subroutine read_parameters_simulation_settings

    module subroutine display_simulation_settings(self)
        implicit none
        class(type_simulation_settings) :: self

        write (*, '(a)') "Title: "//strip(self%title)
        write (*, '(a)') "Calculation Type: "//to_string(self%calculate_type)
        write (*, '(a)') "Calculation Dimension: "//to_string(self%calculate_dimension)

    end subroutine display_simulation_settings
end submodule inout_input_basic_simulation_setting

submodule(inout_input_conditions) inout_input_conditions_initial
    implicit none
    !------------------------------------------------------------------------------------------------------------------------------
    ! JSON key names for initial conditions
    !!------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: initial_conditions = "initial_conditions"
    character(*), parameter :: calculate_thermal = "calculate_thermal"
    character(*), parameter :: calculate_hydraulic = "calculate_hydraulic"
    character(*), parameter :: calculate_mechanical = "calculate_mechanical"
    character(*), parameter :: type = "type"
    character(*), parameter :: value = "value"
    character(*), parameter :: valid_initial_condition_types(3) = [character(8) :: "uniform", "laplace", "file"]
    character(*), parameter :: field_name = "field_name"
    !!------------------------------------------------------------------------------------------------------------------------------
contains
    module subroutine read_conditions_initial_conditions(self, json)
        implicit none
        class(type_conditions), intent(inout) :: self
        type(json_file), intent(inout) :: json

        character(256) :: buffer(3) = [character(256) :: initial_conditions, "", ""]
        logical :: has_thermal, has_hydraulic, has_mechanical

        buffer(2) = calculate_thermal
        call get_json_value(json, join(buffer), has_thermal, default_value=.false.)
        if (has_thermal) then
            buffer(2) = "thermal"
            buffer(3) = type
            call get_json_value(json, join(buffer), self%initial_conditions%thermal%type, &
                                is_required=.true., valid_list=valid_initial_condition_types)
            select case (self%initial_conditions%thermal%type)
            case ("uniform")
                buffer(3) = value
                call get_json_value(json, join(buffer), self%initial_conditions%thermal%value, &
                                    is_required=.true.)
            case ("laplace")
                ! No additional parameters needed for laplace
            case ("file")
                buffer(3) = field_name
                call get_json_value(json, join(buffer), self%initial_conditions%thermal%field_name, &
                                    is_required=.true.)
            end select
        end if
    end subroutine read_conditions_initial_conditions

    module subroutine display_initial_conditions(self)
        implicit none
        class(type_initial_conditions), intent(in) :: self

        write (output_unit, '(A)') "  Initial Conditions:"
        write (output_unit, '(A)') "    Thermal:"
        write (output_unit, '(A, A)') "      Type: ", trim(self%thermal%type)
        select case (self%thermal%type)
        case ("uniform")
            write (output_unit, '(A, F8.3)') "      Value: ", self%thermal%value
        case ("laplace")
            write (output_unit, '(A)') "      Value: Laplace equation will be solved"
        case ("file")
            write (output_unit, '(A, A)') "      Field Name: ", trim(self%thermal%field_name)
        end select
        write (output_unit, '(A)') "    Hydraulic:"
        write (output_unit, '(A, A)') "      Type: ", trim(self%hydraulic%type)
        select case (self%hydraulic%type)
        case ("uniform")
            write (output_unit, '(A, F8.3)') "      Value: ", self%hydraulic%value
        case ("laplace")
            write (output_unit, '(A)') "      Value: Laplace equation will be solved"
        case ("file")
            write (output_unit, '(A, A)') "      Field Name: ", trim(self%hydraulic%field_name)
        end select
        write (output_unit, '(A)') "    Porosity:"
        write (output_unit, '(A, A)') "      Type: ", trim(self%porosity%type)
        select case (self%porosity%type)
        case ("uniform")
            write (output_unit, '(A, F8.3)') "      Value: ", self%porosity%value
        case ("laplace")
            write (output_unit, '(A)') "      Value: Laplace equation will be solved"
        case ("file")
            write (output_unit, '(A, A)') "      Field Name: ", trim(self%porosity%field_name)
        end select

    end subroutine display_initial_conditions

end submodule inout_input_conditions_initial

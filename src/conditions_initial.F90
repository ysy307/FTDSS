submodule(inout_input_conditions) inout_input_conditions_initial
    use :: inout_input
    implicit none
    !------------------------------------------------------------------------------------------------------------------------------
    ! JSON key names for initial conditions
    !!------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: initial_conditions = "initial_conditions"
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
        integer(int32) :: i
        character(:), allocatable :: tmp_string
        character(256) :: buffer(3) = [character(256) :: initial_conditions, "", ""]

        select type (p => self%parent)
        type is (type_input)
            do i = 1, NUM_IC_TARGETS

                select case (i)
                case (IC_TARGET_THERMAL)
                    if (.not. p%basic%analysis_controls%is_active(i)) cycle
                    buffer(2) = thermal
                case (IC_TARGET_HYDRAULIC)
                    if (.not. p%basic%analysis_controls%is_active(i)) cycle
                    buffer(2) = hydraulic
                case (IC_TARGET_MECHANICAL)
                    if (.not. p%basic%analysis_controls%is_active(i)) cycle
                    buffer(2) = mechanical
                case (IC_TARGET_POROSITY)
                    buffer(2) = porosity
                end select
                buffer(3) = type

                call get_json_value(json, join(buffer), tmp_string, &
                                    is_required=.true., valid_list=valid_initial_condition_types)
                self%initial_conditions%physics(i)%type = get_initial_condition_type(tmp_string)

                select case (self%initial_conditions%physics(i)%type)
                case (IC_METHOD_UNIFORM)
                    buffer(3) = value
                    call get_json_value(json, join(buffer), self%initial_conditions%physics(i)%value, &
                                        is_required=.true.)
                case (IC_METHOD_LAPLACE)
                    ! No additional parameters needed for laplace
                case (IC_METHOD_FROM_FILE)
                    buffer(3) = field_name
                    call get_json_value(json, join(buffer), self%initial_conditions%physics(i)%field_name, &
                                        is_required=.true.)
                end select
            end do
        end select

    end subroutine read_conditions_initial_conditions

    module subroutine display_initial_conditions(self)
        implicit none
        class(type_initial_conditions), intent(in) :: self

        write (*, '(A)') "  Initial Conditions:"

        select type (p => self%parent%parent)
        type is (type_input)

            if (p%basic%analysis_controls%is_active(PHYSICS_TYPE_THERMAL)) then
                call display_initial_local(self%physics, "Thermal", IC_TARGET_THERMAL)
            end if
            if (p%basic%analysis_controls%is_active(PHYSICS_TYPE_HYDRAULIC)) then
                call display_initial_local(self%physics, "Hydraulic", IC_TARGET_HYDRAULIC)
            end if

            call display_initial_local(self%physics, "Porosity", IC_TARGET_POROSITY)

        end select

    end subroutine display_initial_conditions

    subroutine display_initial_local(fields, title, target_ic_id)
        implicit none
        type(type_initial_local), intent(in) :: fields(:)
        character(*), intent(in) :: title
        integer(int32), intent(in), optional :: target_ic_id

        write (*, '(A, A)') "    ", trim(title), ":"
        write (*, '(A, A)') "      Type: ", get_initial_condition_type_string(fields(target_ic_id)%type)

        select case (fields(target_ic_id)%type)
        case (IC_METHOD_UNIFORM)
            write (*, '(A, F8.3)') "      Value: ", fields(target_ic_id)%value
        case (IC_METHOD_LAPLACE)
            write (*, '(A)') "      Value: Laplace equation will be solved"
        case (IC_METHOD_FROM_FILE)
            write (*, '(A, A)') "      Field Name: ", trim(fields(target_ic_id)%field_name)
        case default
            write (*, '(A)') "      Unknown type"
        end select
    end subroutine display_initial_local

end submodule inout_input_conditions_initial

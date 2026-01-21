submodule(inout_input_basic) inout_input_basic_analysis_controls
    implicit none
    !!------------------------------------------------------------------------------------------------------------------------------
    ! JSON key names for analysis controls
    !!------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: analysis_controls = "analysis_controls"
    character(*), parameter :: calculate_thermal = "calculate_thermal"
    character(*), parameter :: calculate_hydraulic = "calculate_hydraulic"
    character(*), parameter :: calculate_mechanical = "calculate_mechanical"
    character(*), parameter :: coupling_mode = "coupling_mode"
    character(*), parameter :: coupling_modes_strings(2) = [character(len=16) :: "weak", "strong"]
    character(*), parameter :: partitioning = "partitioning"
    !!------------------------------------------------------------------------------------------------------------------------------
contains
    module subroutine read_parameters_analysis_controls(self, json)
        implicit none
        class(type_input_basic), intent(inout) :: self
        type(json_file), intent(inout) :: json
        integer(int32) :: target_id

        character(:), allocatable :: temp_string

        character(256) :: buffer(2) = [character(256) :: analysis_controls, ""]

        buffer(2) = calculate_thermal
        target_id = get_physics_type(thermal)
        call get_json_value(json, join(buffer), self%analysis_controls%is_active(target_id), &
                            is_required=.true., default_value=.false.)

        buffer(2) = calculate_hydraulic
        target_id = get_physics_type(hydraulic)
        call get_json_value(json, join(buffer), self%analysis_controls%is_active(target_id), &
                            is_required=.true., default_value=.false.)

        buffer(2) = calculate_mechanical
        target_id = get_physics_type(mechanical)
        call get_json_value(json, join(buffer), self%analysis_controls%is_active(target_id), &
                            is_required=.true., default_value=.false.)

        if (.not. any(self%analysis_controls%is_active(:))) then
            call raise_error(ERROR_CODES%VAR_INVALID, opt=analysis_controls)
        end if

        buffer(2) = coupling_mode
        call get_json_value(json, join(buffer), temp_string, &
                            is_required=.true., default_value="weak", valid_list=coupling_modes_strings)
        self%analysis_controls%coupling_mode = get_coupling_mode(temp_string)

        buffer(2) = partitioning
        call get_json_value(json, join(buffer), self%analysis_controls%partitioning, &
                            is_required=.true., default_value=.false.)

    end subroutine read_parameters_analysis_controls

    module subroutine display_analysis_controls(self)
        implicit none
        class(type_analysis_controls) :: self

        write (*, '(a)') "Calculate Thermal: "//to_string(self%is_active(get_physics_type(thermal)))
        write (*, '(a)') "Calculate Hydraulic: "//to_string(self%is_active(get_physics_type(hydraulic)))
        write (*, '(a)') "Calculate Mechanical: "//to_string(self%is_active(get_physics_type(mechanical)))
        write (*, '(a)') "Coupling Mode: "//to_string(self%coupling_mode)
        write (*, '(a)') "Partitioning: "//to_string(self%partitioning)

    end subroutine display_analysis_controls
end submodule inout_input_basic_analysis_controls

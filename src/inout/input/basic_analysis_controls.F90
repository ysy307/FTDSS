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
    character(*), parameter :: coupling_modes(3) = ["none", "weak", "strong"]
    character(*), parameter :: partitioning = "partitioning"
    !!------------------------------------------------------------------------------------------------------------------------------
contains
    module subroutine read_parameters_analysis_controls(self, json)
        implicit none
        class(type_input_basic), intent(inout) :: self
        type(json_file), intent(inout) :: json

        character(:), allocatable :: key
        character(256) :: buffer(2)
        logical :: found

        buffer(1) = analysis_controls
        buffer(2) = calculate_thermal
        call get_json_value(json, join(buffer), self%analysis_controls%calculate_thermal, &
                            is_required=.true., default_value=.false.)

        buffer(2) = calculate_hydraulic
        call get_json_value(json, join(buffer), self%analysis_controls%calculate_hydraulic, &
                            is_required=.true., default_value=.false.)

        buffer(2) = calculate_mechanical
        call get_json_value(json, join(buffer), self%analysis_controls%calculate_mechanical, &
                            is_required=.true., default_value=.false.)

        if (.not. self%analysis_controls%calculate_thermal .and. &
            .not. self%analysis_controls%calculate_hydraulic .and. &
            .not. self%analysis_controls%calculate_mechanical) then
            call error_message(905, c_opt=analysis_controls)
        end if

        buffer(2) = coupling_mode
        call get_json_value(json, join(buffer), self%analysis_controls%coupling_mode, &
                            is_required=.true., default_value="weak", valid_list=coupling_modes)

        buffer(2) = partitioning
        call get_json_value(json, join(buffer), self%analysis_controls%partitioning, &
                            is_required=.true., default_value=.false.)

    end subroutine read_parameters_analysis_controls

    module subroutine display_analysis_controls(self)
        implicit none
        class(type_analysis_controls) :: self

        write (*, '(a)') "Calculate Thermal: "//to_string(self%calculate_thermal)
        write (*, '(a)') "Calculate Hydraulic: "//to_string(self%calculate_hydraulic)
        write (*, '(a)') "Calculate Mechanical: "//to_string(self%calculate_mechanical)
        write (*, '(a)') "Coupling Mode: "//strip(self%coupling_mode)
        write (*, '(a)') "Partitioning: "//to_string(self%partitioning)

    end subroutine display_analysis_controls
end submodule inout_input_basic_analysis_controls

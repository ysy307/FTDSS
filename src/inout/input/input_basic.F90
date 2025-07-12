submodule(inout_input) inout_input_basic
    implicit none
    !-------------------------------------------------------------------------------
    character(*), parameter :: simulation_settins = "simulation_settings"
    character(*), parameter :: title = "title"
    character(*), parameter :: calculate_type = "calculate_type"
    integer(int32), parameter :: min_calculation_type = 1
    integer(int32), parameter :: max_calculation_type = 3
    !-------------------------------------------------------------------------------
    character(*), parameter :: analysis_controls = "analysis_controls"
    character(*), parameter :: calculate_thermal = "calculate_thermal"
    character(*), parameter :: calculate_hydraulic = "calculate_hydraulic"
    character(*), parameter :: calculate_mechanical = "calculate_mechanical"
    !-------------------------------------------------------------------------------

contains
    module subroutine inout_input_basic_parameters(self)
        !< Load the input parameters from the JSON file
        implicit none
        class(Type_Input), intent(inout) :: self
        type(json_file) :: json
        integer(int32) :: status, unit_num
        integer(int32) :: iRegion

        call json%initialize()

        call json%load(filename=self%basic_file_name)
        call json%print_error_message(output_unit)

        call read_parameters_simulation_settings(self, json)
        call read_parameters_analysis_controls(self, json)

        ! call inout_input_Parameters_JSON_Basic(self, json)
        ! if (.not. allocated(self%Regions)) allocate (self%Regions(self%Basic%numRegion))
        ! do iRegion = 1, self%Basic%numRegion
        !     call inout_input_Parameters_JSON_Reigion_Infomation(self, json, iRegion)
        !     if (self%Regions(iRegion)%Flag%isHeat) then
        !         call inout_input_Parameters_JSON_Thermal(self, json, iRegion)
        !     end if
        !     !     if (self%Regions(iRegion)%Flags%isWater) then
        !     !         call inout_input_Parameters_JSON_Hydraulic(self, json, iRegion)
        !     !     end if
        ! end do
        ! call inout_input_Parameters_JSON_Solver(self, json)

        call json%destroy()
        call json%print_error_message(output_unit)
    end subroutine inout_input_basic_parameters

    subroutine read_parameters_simulation_settings(self, json)
        !> Load the basic input parameters from the JSON file
        implicit none
        class(Type_Input) :: self
        type(json_file), intent(inout) :: json !! JSON parser

        character(:), allocatable :: key
        logical :: found

        key = join([simulation_settins, title])
        call json%get(key, self%Basic%simulation_settings%title, found)
        call json%print_error_message(output_unit)
        if (.not. found) self%Basic%simulation_settings%title = "FTDSS Simulation"

        key = join([simulation_settins, calculate_type])
        call json%get(key, self%Basic%simulation_settings%calculate_type, found)
        call json%print_error_message(output_unit)
        if (.not. found) call error_message(904, c_opt=key)
        if (.not. value_in_range(self%Basic%simulation_settings%calculate_type, min_calculation_type, max_calculation_type)) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

    end subroutine read_parameters_simulation_settings

    subroutine read_parameters_analysis_controls(self, json)
        !> Load the analysis control parameters from the JSON file
        implicit none
        class(Type_Input) :: self
        type(json_file), intent(inout) :: json !! JSON parser

        logical :: found
        character(:), allocatable :: key

        key = join([analysis_controls, calculate_thermal])
        call json%get(key, self%Basic%analysis_controls%calculate_thermal, found)
        call json%print_error_message(output_unit)
        if (.not. found) self%Basic%analysis_controls%calculate_thermal = .false.

        key = join([analysis_controls, calculate_hydraulic])
        call json%get(key, self%Basic%analysis_controls%calculate_hydraulic, found)
        call json%print_error_message(output_unit)
        if (.not. found) self%Basic%analysis_controls%calculate_hydraulic = .false.

        key = join([analysis_controls, calculate_mechanical])
        call json%get(key, self%Basic%analysis_controls%calculate_mechanical, found)
        call json%print_error_message(output_unit)
        if (.not. found) self%Basic%analysis_controls%calculate_mechanical = .false.

        if (.not. self%Basic%analysis_controls%calculate_thermal .and. &
            .not. self%Basic%analysis_controls%calculate_hydraulic .and. &
            .not. self%Basic%analysis_controls%calculate_mechanical) then
            call json%destroy()
            call error_message(905, c_opt=analysis_controls)
        end if

    end subroutine read_parameters_analysis_controls

end submodule inout_input_basic

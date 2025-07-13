submodule(inout_input) inout_input_basic
    implicit none
    !-------------------------------------------------------------------------------
    ! JSON key names for simulation settings
    !-------------------------------------------------------------------------------
    character(*), parameter :: simulation_settins = "simulation_settings"
    character(*), parameter :: title = "title"
    character(*), parameter :: calculate_type = "calculate_type"
    integer(int32), parameter :: min_calculation_type = 1
    integer(int32), parameter :: max_calculation_type = 3
    !-------------------------------------------------------------------------------
    ! JSON key names for analysis controls
    !-------------------------------------------------------------------------------
    character(*), parameter :: analysis_controls = "analysis_controls"
    character(*), parameter :: calculate_thermal = "calculate_thermal"
    character(*), parameter :: calculate_hydraulic = "calculate_hydraulic"
    character(*), parameter :: calculate_mechanical = "calculate_mechanical"
    character(*), parameter :: coupling_mode = "coupling_mode"
    character(*), parameter :: coppling_modes(3) = ["none", "weak", "strong"]
    !-------------------------------------------------------------------------------
    ! JSON key names for materials
    !-------------------------------------------------------------------------------
    character(*), parameter :: materials = "materials"
    character(*), parameter :: id = "id"
    character(*), parameter :: name = "name"
    character(*), parameter :: phase = "phase"
    character(*), parameter :: is_frozen = "is_frozen"
    character(*), parameter :: is_dispersed = "is_dispersed"
    character(*), parameter :: thermal = "thermal"
    character(*), parameter :: denstiy = "density"
    character(*), parameter :: specific_heat = "specific_heat"
    character(*), parameter :: thermal_conductivity = "thermal_conductivity"
    character(*), parameter :: dispersivity = "dispersivity"
    ! character(*), parameter :: phase_change = "phase_change"

    character(*), parameter :: hydraulic = "hydraulic"

contains
    module subroutine inout_input_basic_parameters(self)
        !< Load the input parameters from the JSON file
        implicit none
        class(type_input), intent(inout) :: self
        type(json_file) :: json
        integer(int32) :: status, unit_num
        integer(int32) :: iRegion

        call json%initialize()

        call json%load(filename=self%basic_file_name)
        call json%print_error_message(output_unit)

        call read_parameters_simulation_settings(self, json)
        call read_parameters_analysis_controls(self, json)
        call read_parameters_materials(self, json)

        ! call inout_input_Parameters_JSON_basic(self, json)
        ! if (.not. allocated(self%Regions)) allocate (self%Regions(self%basic%numRegion))
        ! do iRegion = 1, self%basic%numRegion
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
        class(type_input) :: self
        type(json_file), intent(inout) :: json !! JSON parser

        character(:), allocatable :: key
        logical :: found

        key = join([simulation_settins, title])
        call json%get(key, self%basic%simulation_settings%title, found)
        call json%print_error_message(output_unit)
        if (.not. found) self%basic%simulation_settings%title = "FTDSS Simulation"

        key = join([simulation_settins, calculate_type])
        call json%get(key, self%basic%simulation_settings%calculate_type, found)
        call json%print_error_message(output_unit)
        if (.not. found) call error_message(904, c_opt=key)
        if (.not. value_in_range(self%basic%simulation_settings%calculate_type, min_calculation_type, max_calculation_type)) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        select case (self%basic%simulation_settings%calculate_type)
        case (1:2)
            self%basic%simulation_settings%calculate_dimension = 2
        case (3)
            self%basic%simulation_settings%calculate_dimension = 3
        end select

    end subroutine read_parameters_simulation_settings

    subroutine read_parameters_analysis_controls(self, json)
        !> Load the analysis control parameters from the JSON file
        implicit none
        class(type_input) :: self
        type(json_file), intent(inout) :: json !! JSON parser

        logical :: found
        character(:), allocatable :: key

        key = join([analysis_controls, calculate_thermal])
        call json%get(key, self%basic%analysis_controls%calculate_thermal, found)
        call json%print_error_message(output_unit)
        if (.not. found) self%basic%analysis_controls%calculate_thermal = .false.

        key = join([analysis_controls, calculate_hydraulic])
        call json%get(key, self%basic%analysis_controls%calculate_hydraulic, found)
        call json%print_error_message(output_unit)
        if (.not. found) self%basic%analysis_controls%calculate_hydraulic = .false.

        key = join([analysis_controls, calculate_mechanical])
        call json%get(key, self%basic%analysis_controls%calculate_mechanical, found)
        call json%print_error_message(output_unit)
        if (.not. found) self%basic%analysis_controls%calculate_mechanical = .false.

        if (.not. self%basic%analysis_controls%calculate_thermal .and. &
            .not. self%basic%analysis_controls%calculate_hydraulic .and. &
            .not. self%basic%analysis_controls%calculate_mechanical) then
            call json%destroy()
            call error_message(905, c_opt=analysis_controls)
        end if

        key = join([analysis_controls, coupling_mode])
        call json%get(key, self%basic%analysis_controls%coupling_mode, found)
        call json%print_error_message(output_unit)
        if (.not. found) then
            self%basic%analysis_controls%coupling_mode = "weak"
        else
            if (.not. any(coppling_modes(:) == self%basic%analysis_controls%coupling_mode)) then
                call json%destroy()
                call error_message(905, c_opt=key)
            end if
        end if

    end subroutine read_parameters_analysis_controls

    subroutine read_parameters_materials(self, json)
        !> Load the material parameters from the JSON file
        implicit none
        class(type_input) :: self
        type(json_file), intent(inout) :: json !! JSON parser

        logical :: found
        character(:), allocatable :: key
        integer(int32) :: i

        call json%info(materials, found=found, n_children=self%basic%num_materials)
        call json%print_error_message(output_unit)
        if (.not. found .or. self%basic%num_materials <= 0) then
            call json%destroy()
            call error_message(904, c_opt=materials)
        end if

        if (allocated(self%basic%materials)) deallocate (self%basic%materials)
        allocate (self%basic%materials(self%basic%num_materials))

        do i = 1, self%basic%num_materials
            call read_parameters_materials_basic(self, json, i)
            call read_parameters_materials_thermal(self, json, i)
            print *, "Material ", i, ":", self%basic%materials(i)%name
            print *, "  Phase: ", self%basic%materials(i)%phase
            print *, "  Density: ", self%basic%materials(i)%thermal%density
            print *, "  Specific Heat: ", self%basic%materials(i)%thermal%specific_heat
            print *, "  Thermal Conductivity: ", self%basic%materials(i)%thermal%thermal_conductivity
            if (self%basic%materials(i)%is_dispersed) then
                print *, "  Thermal Conductivity Dispersivity: ", self%basic%materials(i)%thermal%thermal_conductivity_dispersity
            else
                print *, "  Thermal Conductivity Dispersivity: Not applicable"
            end if

        end do
        stop

    end subroutine read_parameters_materials

    subroutine read_parameters_materials_basic(self, json, i)
        !> Load the basic material parameters from the JSON file
        implicit none
        class(type_input) :: self
        type(json_file), intent(inout) :: json !! JSON parser
        integer(int32), intent(in) :: i !! Material index

        logical :: found
        character(:), allocatable :: key
        character(:), allocatable :: key_material

        key_material = join([materials//"("//to_string(i)//")"])

        key = join([key_material, id])
        call json%get(key, self%basic%materials(i)%id, found)
        call json%print_error_message(output_unit)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if

        key = join([key_material, name])
        call json%get(key, self%basic%materials(i)%name, found)
        call json%print_error_message(output_unit)
        if (.not. found) self%basic%materials(i)%name = "Material_"//to_string(i)
        self%basic%materials(i)%name = trim(adjustl(self%basic%materials(i)%name))

        key = join([key_material, phase])
        call json%get(key, self%basic%materials(i)%phase, found)
        call json%print_error_message(output_unit)
        if (.not. found) call error_message(904, c_opt=key)
        if (.not. value_in_range(self%basic%materials(i)%phase, 1, 4)) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        key = join([key_material, is_frozen])
        call json%get(key, self%basic%materials(i)%is_frozen, found)
        call json%print_error_message(output_unit)
        if (.not. found) self%basic%materials(i)%is_frozen = .false.

        key = join([key_material, is_dispersed])
        call json%get(key, self%basic%materials(i)%is_dispersed, found)
        call json%print_error_message(output_unit)
        if (.not. found) self%basic%materials(i)%is_dispersed = .false.

    end subroutine read_parameters_materials_basic

    subroutine read_parameters_materials_thermal(self, json, i)
        implicit none
        class(type_input) :: self
        type(json_file), intent(inout) :: json !! JSON parser
        integer(int32), intent(in) :: i !! Material index

        logical :: found
        character(:), allocatable :: key
        character(:), allocatable :: key_material

        key_material = join([materials//"("//to_string(i)//")", thermal])

        key = join([key_material, denstiy])
        call json%get(key, self%basic%materials(i)%thermal%density, found)
        call json%print_error_message(output_unit)
        if (.not. found) call error_message(904, c_opt=key)
        if (any(self%basic%materials(i)%thermal%density(:) <= 0.0d0) .and. &
            size(self%basic%materials(i)%thermal%density(:)) == self%basic%materials(i)%phase) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        key = join([key_material, specific_heat])
        call json%get(key, self%basic%materials(i)%thermal%specific_heat, found)
        call json%print_error_message(output_unit)
        if (.not. found) call error_message(904, c_opt=key)
        if (any(self%basic%materials(i)%thermal%specific_heat(:) <= 0.0d0) .and. &
            size(self%basic%materials(i)%thermal%specific_heat(:)) == self%basic%materials(i)%phase) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        key = join([key_material, thermal_conductivity])
        call json%get(key, self%basic%materials(i)%thermal%thermal_conductivity, found)
        call json%print_error_message(output_unit)
        if (.not. found) call error_message(904, c_opt=key)
        if (any(self%basic%materials(i)%thermal%thermal_conductivity(:) <= 0.0d0) .and. &
            size(self%basic%materials(i)%thermal%thermal_conductivity(:)) == self%basic%materials(i)%phase) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        if (self%basic%materials(i)%is_dispersed) then
            key = join([key_material, dispersivity])
            call json%get(key, self%basic%materials(i)%thermal%thermal_conductivity_dispersity, found)
            call json%print_error_message(output_unit)
            if (.not. found) call error_message(904, c_opt=key)
            if (any(self%basic%materials(i)%thermal%thermal_conductivity_dispersity(:) < 0.0d0) .and. &
                size(self%basic%materials(i)%thermal%thermal_conductivity_dispersity(:)) == 2) then
                call json%destroy()
                call error_message(905, c_opt=key)
            end if
        else
            call allocate_array(self%basic%materials(i)%thermal%thermal_conductivity_dispersity, 1)
            self%basic%materials(i)%thermal%thermal_conductivity_dispersity = 0.0d0
        end if

    end subroutine read_parameters_materials_thermal

end submodule inout_input_basic

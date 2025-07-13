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
    character(*), parameter :: phase_change = "phase_change"
    character(*), parameter :: latent_heat = "latent_heat"
    character(*), parameter :: freezeing_temperature = "freezing_temperature"
    character(*), parameter :: unfrozen_water_model = "unfrozen_water_model"
    character(*), parameter :: model_number = "model_number"
    character(*), parameter :: theta_s = "theta_s"
    character(*), parameter :: theta_r = "theta_r"
    character(*), parameter :: n1 = "n1"
    character(*), parameter :: n2 = "n2"
    character(*), parameter :: alpha1 = "alpha1"
    character(*), parameter :: alpha2 = "alpha2"
    character(*), parameter :: w1 = "w1"
    character(*), parameter :: h_crit = "h_crit"

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
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if
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
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if
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
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if
        if (any(self%basic%materials(i)%thermal%density(:) <= 0.0d0) .or. &
            size(self%basic%materials(i)%thermal%density(:)) /= self%basic%materials(i)%phase) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        key = join([key_material, specific_heat])
        call json%get(key, self%basic%materials(i)%thermal%specific_heat, found)
        call json%print_error_message(output_unit)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if
        if (any(self%basic%materials(i)%thermal%specific_heat(:) <= 0.0d0) .or. &
            size(self%basic%materials(i)%thermal%specific_heat(:)) /= self%basic%materials(i)%phase) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        key = join([key_material, thermal_conductivity])
        call json%get(key, self%basic%materials(i)%thermal%thermal_conductivity, found)
        call json%print_error_message(output_unit)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if
        if (any(self%basic%materials(i)%thermal%thermal_conductivity(:) <= 0.0d0) .or. &
            size(self%basic%materials(i)%thermal%thermal_conductivity(:)) /= self%basic%materials(i)%phase) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        if (self%basic%materials(i)%is_dispersed) then
            key = join([key_material, dispersivity])
            call json%get(key, self%basic%materials(i)%thermal%thermal_conductivity_dispersity, found)
            call json%print_error_message(output_unit)
            if (.not. found) then
                call json%destroy()
                call error_message(904, c_opt=key)
            end if
            if (any(self%basic%materials(i)%thermal%thermal_conductivity_dispersity(:) < 0.0d0) .or. &
                size(self%basic%materials(i)%thermal%thermal_conductivity_dispersity(:)) /= 2) then
                call json%destroy()
                call error_message(905, c_opt=key)
            end if
        else
            call allocate_array(self%basic%materials(i)%thermal%thermal_conductivity_dispersity, 1)
            self%basic%materials(i)%thermal%thermal_conductivity_dispersity = 0.0d0
        end if

        if (self%basic%materials(i)%is_frozen) then
            key = join([key_material, phase_change, latent_heat])
            call json%get(key, self%basic%materials(i)%thermal%phase_change%latent_heat, found)
            call json%print_error_message(output_unit)
            if (.not. found) then
                call json%destroy()
                call error_message(904, c_opt=key)
            end if
            if (self%basic%materials(i)%thermal%phase_change%latent_heat <= 0.0d0) then
                call json%destroy()
                call error_message(905, c_opt=key)
            end if

            key = join([key_material, phase_change, freezeing_temperature])
            call json%get(key, self%basic%materials(i)%thermal%phase_change%freezing_temperature, found)
            call json%print_error_message(output_unit)
            if (.not. found) then
                call json%destroy()
                call error_message(904, c_opt=key)
            end if
            if (self%basic%materials(i)%thermal%phase_change%freezing_temperature > 0.0d0) then
                call json%destroy()
                call error_message(905, c_opt=key)
            end if

            key = join([key_material, phase_change, unfrozen_water_model])
            call read_parameters_materials_wrf(self%basic%materials(i)%thermal%phase_change%wrf, json, key)
        end if

    end subroutine read_parameters_materials_thermal

    subroutine read_parameters_materials_wrf(wrf, json, key_base)
        type(type_materials_wrf), intent(inout) :: wrf
        type(json_file), intent(inout) :: json !! JSON parser
        character(*), intent(in) :: key_base !! Base key for WRF material parameters

        logical :: found
        character(:), allocatable :: key

        key = join([key_base, model_number])
        call json%get(key, wrf%model_number, found)
        call json%print_error_message(output_unit)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if
        if (.not. value_in_range(wrf%model_number, 1, 6)) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if
        key = join([key_base, theta_s])
        call json%get(key, wrf%theta_s, found)
        call json%print_error_message(output_unit)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if

        key = join([key_base, theta_r])
        call json%get(key, wrf%theta_r, found)
        call json%print_error_message(output_unit)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if
        if (wrf%theta_s <= wrf%theta_r .or. &
            wrf%theta_s <= 0.0d0 .or. &
            wrf%theta_s < 0.0d0) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        key = join([key_base, alpha1])
        call json%get(key, wrf%alpha1, found)
        call json%print_error_message(output_unit)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if

        key = join([key_base, n1])
        call json%get(key, wrf%n1, found)
        call json%print_error_message(output_unit)
        if (.not. found) then
            call json%destroy()
            call error_message(904, c_opt=key)
        end if
        if (wrf%n1 <= 0.0d0) then
            call json%destroy()
            call error_message(905, c_opt=key)
        end if

        wrf%m1 = 1.0d0 - 1.0d0 / wrf%n1

        select case (wrf%model_number)
        case (4)
            key = join([key_base, h_crit])
            call json%get(key, wrf%h_crit, found)
            call json%print_error_message(output_unit)
            if (.not. found) then
                call json%destroy()
                call error_message(904, c_opt=key)
            end if
            if (wrf%h_crit > 0.0d0) then
                call json%destroy()
                call error_message(905, c_opt=key)
            end if
        case (5)
            key = join([key_base, alpha2])
            call json%get(key, wrf%alpha2, found)
            call json%print_error_message(output_unit)
            if (.not. found) then
                call json%destroy()
                call error_message(904, c_opt=key)
            end if
            if (wrf%alpha2 <= 0.0d0) then
                call json%destroy()
                call error_message(905, c_opt=key)
            end if

            key = join([key_base, n2])
            call json%get(key, wrf%n2, found)
            call json%print_error_message(output_unit)
            if (.not. found) then
                call json%destroy()
                call error_message(904, c_opt=key)
            end if
            if (wrf%n2 <= 0) then
                call json%destroy()
                call error_message(905, c_opt=key)
            end if

            wrf%m2 = 1.0d0 - 1.0d0 / wrf%n2

            key = join([key_base, w1])
            call json%get(key, wrf%w1, found)
            call json%print_error_message(output_unit)
            if (.not. found) then
                call json%destroy()
                call error_message(904, c_opt=key)
            end if
            if (wrf%w1 < 0.0d0 .or. wrf%w1 > 1.0d0) then
                call json%destroy()
                call error_message(905, c_opt=key)
            end if

            wrf%w2 = 1.0d0 - wrf%w1
        case (6)
            key = join([key_base, n2])
            call json%get(key, wrf%n2, found)
            call json%print_error_message(output_unit)
            if (.not. found) then
                call json%destroy()
                call error_message(904, c_opt=key)
            end if
            if (wrf%n2 <= 1.0d0) then
                call json%destroy()
                call error_message(905, c_opt=key)
            end if

            wrf%m1 = 1.0d0 - 1.0d0 / wrf%n2

            key = join([key_base, w1])
            call json%get(key, wrf%w1, found)
            call json%print_error_message(output_unit)
            if (.not. found) then
                call json%destroy()
                call error_message(904, c_opt=key)
            end if
            if (wrf%w1 < 0.0d0 .or. wrf%w1 > 1.0d0) then
                call json%destroy()
                call error_message(905, c_opt=key)
            end if

            wrf%w2 = 1.0d0 - wrf%w1
        end select

    end subroutine read_parameters_materials_wrf

end submodule inout_input_basic

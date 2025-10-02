submodule(inout_input_basic) inout_input_basic_materials
    implicit none
    !!------------------------------------------------------------------------------------------------------------------------------
    ! JSON key names for materials
    !!------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: materials = "materials"
    character(*), parameter :: id = "id"
    character(*), parameter :: name = "name"
    character(*), parameter :: calculate_thermal = "calculate_thermal"
    character(*), parameter :: calculate_hydraulic = "calculate_hydraulic"
    character(*), parameter :: calculate_mechanical = "calculate_mechanical"
    character(*), parameter :: phase = "phase"
    character(*), parameter :: is_frozen = "is_frozen"
    character(*), parameter :: is_dispersed = "is_dispersed"

    character(*), parameter :: density = "density"
    character(*), parameter :: specific_heat = "specific_heat"
    character(*), parameter :: thermal_conductivity = "thermal_conductivity"
    character(*), parameter :: dispersivity = "dispersivity"
    character(*), parameter :: phase_change = "phase_change"
    character(*), parameter :: latent_heat = "latent_heat"
    character(*), parameter :: fusion = "fusion"
    character(*), parameter :: freezing_temperature = "freezing_temperature"
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
    character(*), parameter :: equilibrium_model = "equilibrium_model"
    character(*), parameter :: segregation = "segregation"
    character(*), parameter :: unit = "unit"
    character(*), parameter :: valid_gcc_units(2) = [character(len=4) :: "m", "pa"]
    character(*), parameter :: hydraulic_conductivity_model = "hydraulic_conductivity_model"
    character(*), parameter :: saturated_conductivity = "saturated_conductivity"
    character(*), parameter :: l = "l"
    character(*), parameter :: impedance_factor = "impedance_factor"
    character(*), parameter :: water_viscosity_model = "water_viscosity_model"
    character(*), parameter :: water_retention_model = "water_retention_model"
contains
    module subroutine read_parameters_materials(self, json)
        implicit none
        class(type_input_basic), intent(inout) :: self
        type(json_file), intent(inout) :: json

        logical :: found
        integer(int32) :: i

        call json%info(materials, found=found, n_children=self%num_materials)
        call json%print_error_message(output_unit)
        if (.not. found .or. self%num_materials <= 0) then
            call error_message(904, c_opt=materials)
        end if

        if (allocated(self%materials)) deallocate (self%materials)
        allocate (self%materials(self%num_materials))

        do i = 1, self%num_materials
            call read_parameters_materials_basic(self, json, i)
            if (self%analysis_controls%calculate_thermal) then
                call read_parameters_materials_thermal(self, json, i)
            end if
            if (self%analysis_controls%calculate_hydraulic) then
                call read_parameters_materials_hydraulic(self, json, i)
            end if
            if (self%analysis_controls%calculate_mechanical) then
                ! Mechanical parameters can be added here in the future
            end if
        end do

    end subroutine read_parameters_materials

    subroutine read_parameters_materials_basic(self, json, i_material)
        !> Load the basic material parameters from the JSON file
        implicit none
        class(type_input_basic), intent(inout) :: self
        type(json_file), intent(inout) :: json !! JSON parser
        integer(int32), intent(in) :: i_material !! Material index

        character(256) :: buffer(2)

        buffer(1) = join([materials//"("//to_string(i_material)//")"])
        buffer(2) = id
        call get_json_value(json, join(buffer), self%materials(i_material)%id, &
                            is_required=.true.)

        buffer(2) = name
        call get_json_value(json, join(buffer), self%materials(i_material)%name, &
                            is_required=.false., default_value="Material_"//to_string(i_material))

        if (self%analysis_controls%calculate_thermal) then
            buffer(2) = calculate_thermal
            call get_json_value(json, join(buffer), self%materials(i_material)%calculate_thermal, &
                                is_required=.false., default_value=.false.)
        end if

        if (self%analysis_controls%calculate_hydraulic) then
            buffer(2) = calculate_hydraulic
            call get_json_value(json, join(buffer), self%materials(i_material)%calculate_hydraulic, &
                                is_required=.false., default_value=.false.)
        end if

        if (self%analysis_controls%calculate_mechanical) then
            buffer(2) = calculate_mechanical
            call get_json_value(json, join(buffer), self%materials(i_material)%calculate_mechanical, &
                                is_required=.false., default_value=.false.)
        end if

        buffer(2) = phase
        call get_json_value(json, join(buffer), self%materials(i_material)%phase, &
                            is_required=.true., valid_range=[1, 4])

        buffer(2) = is_frozen
        call get_json_value(json, join(buffer), self%materials(i_material)%is_frozen, &
                            is_required=.false., default_value=.false.)

        buffer(2) = is_dispersed
        call get_json_value(json, join(buffer), self%materials(i_material)%is_dispersed, &
                            is_required=.false., default_value=.false.)

    end subroutine read_parameters_materials_basic

    subroutine read_parameters_materials_thermal(self, json, i_material)
        implicit none
        class(type_input_basic), intent(inout) :: self
        type(json_file), intent(inout) :: json !! JSON parser
        integer(int32), intent(in) :: i_material !! Material index

        character(256) :: buffer(5) = [character(256) :: "", "", "", "", ""]

        buffer(1) = materials//"("//to_string(i_material)//")"
        buffer(2) = thermal

        buffer(3) = density
        call get_json_value(json, join(buffer), self%materials(i_material)%thermal%density, &
                            is_required=.true., valid_range=[0.0d0, huge(0.0d0)], array_size=self%materials(i_material)%phase)

        buffer(3) = specific_heat
        call get_json_value(json, join(buffer), self%materials(i_material)%thermal%specific_heat, &
                            is_required=.true., valid_range=[0.0d0, huge(0.0d0)], array_size=self%materials(i_material)%phase)

        buffer(3) = thermal_conductivity
        call get_json_value(json, join(buffer), self%materials(i_material)%thermal%thermal_conductivity, &
                            is_required=.true., valid_range=[0.0d0, huge(0.0d0)], array_size=self%materials(i_material)%phase)

        if (self%materials(i_material)%is_dispersed) then
            buffer(3) = dispersivity
            call get_json_value(json, join(buffer), self%materials(i_material)%thermal%thermal_conductivity_dispersity, &
                                is_required=.true., valid_range=[0.0d0, huge(0.0d0)], array_size=2)
        else
            call allocate_array(self%materials(i_material)%thermal%thermal_conductivity_dispersity, 1)
            self%materials(i_material)%thermal%thermal_conductivity_dispersity = 0.0d0
        end if

        if (self%materials(i_material)%is_frozen) then
            buffer(3) = phase_change
            buffer(4) = latent_heat
            buffer(5) = fusion
            call get_json_value(json, join(buffer), self%materials(i_material)%thermal%phase_change%latent_heat_fusion, &
                                is_required=.true., valid_range=[0.0d0, huge(0.0d0)])

            buffer(4) = freezing_temperature
            buffer(5) = ""
            call get_json_value(json, join(buffer), self%materials(i_material)%thermal%phase_change%freezing_temperature, &
                                is_required=.true., default_value=0.0d0, valid_range=[-huge(0.0d0), 0.0d0])

            buffer(4) = unfrozen_water_model
            call read_parameters_materials_wrf(self%materials(i_material)%thermal%phase_change%wrf, json, buffer, 4)

            buffer(4) = equilibrium_model
            buffer(5) = segregation
            call get_json_value(json, join(buffer), self%materials(i_material)%thermal%phase_change%gcc%is_segregation, &
                                is_required=.true., default_value=.false.)
            buffer(5) = unit
            call get_json_value(json, join(buffer), self%materials(i_material)%thermal%phase_change%gcc%unit, &
                                is_required=.true., default_value="m", valid_list=valid_gcc_units)

        end if

    end subroutine read_parameters_materials_thermal

    subroutine read_parameters_materials_hydraulic(self, json, i_material)
        !> Load the hydraulic parameters from the JSON file
        implicit none
        class(type_input_basic), intent(inout) :: self
        type(json_file), intent(inout) :: json !! JSON parser
        integer(int32), intent(in) :: i_material !! Material index

        character(256) :: buffer(5) = [character(256) :: "", "", "", "", ""]

        buffer(1) = materials//"("//to_string(i_material)//")"
        buffer(2) = hydraulic

        buffer(3) = hydraulic_conductivity_model
        buffer(4) = model_number
        call get_json_value(json, join(buffer), self%materials(i_material)%hydraulic%model_number, &
                            is_required=.true., valid_range=[1, 7])

        buffer(4) = saturated_conductivity
        call get_json_value(json, join(buffer), self%materials(i_material)%hydraulic%hydraulic_conductivity, &
                            is_required=.true., valid_range=[0.0d0, huge(0.0d0)])

        select case (self%materials(i_material)%hydraulic%model_number)
        case (1)
            buffer(3) = impedance_factor
            buffer(4) = ""
            call get_json_value(json, join(buffer), self%materials(i_material)%hydraulic%impedance_factor, &
                                is_required=.true., valid_range=[0.0d0, huge(0.0d0)])

        case (2)
            buffer(3) = water_retention_model
            buffer(4) = ""
            call read_parameters_materials_wrf(self%materials(i_material)%hydraulic%hcf, json, buffer, 3)
        case (3)
            buffer(3) = water_viscosity_model
            call get_json_value(json, join(buffer), self%materials(i_material)%hydraulic%water_viscosity_model, &
                                is_required=.true., valid_range=[0, 2])
        case (4)
            buffer(3) = impedance_factor
            buffer(4) = ""
            call get_json_value(json, join(buffer), self%materials(i_material)%hydraulic%impedance_factor, &
                                is_required=.true., valid_range=[0.0d0, huge(0.0d0)])

            buffer(3) = water_retention_model
            buffer(4) = ""
            call read_parameters_materials_wrf(self%materials(i_material)%hydraulic%hcf, json, buffer, 3)
        case (5)
            buffer(3) = impedance_factor
            buffer(4) = ""
            call get_json_value(json, join(buffer), self%materials(i_material)%hydraulic%impedance_factor, &
                                is_required=.true., valid_range=[0.0d0, huge(0.0d0)])

            buffer(3) = water_viscosity_model
            call get_json_value(json, join(buffer), self%materials(i_material)%hydraulic%water_viscosity_model, &
                                is_required=.true., valid_range=[0, 2])
        case (6)
            buffer(3) = water_retention_model
            buffer(4) = ""
            call read_parameters_materials_wrf(self%materials(i_material)%hydraulic%hcf, json, buffer, 3)

            buffer(3) = water_viscosity_model
            call get_json_value(json, join(buffer), self%materials(i_material)%hydraulic%water_viscosity_model, &
                                is_required=.true., valid_range=[0, 2])
        case (7)
            buffer(3) = impedance_factor
            buffer(4) = ""
            call get_json_value(json, join(buffer), self%materials(i_material)%hydraulic%impedance_factor, &
                                is_required=.true., valid_range=[0.0d0, huge(0.0d0)])

            buffer(3) = water_retention_model
            call read_parameters_materials_wrf(self%materials(i_material)%hydraulic%hcf, json, buffer, 3)

            buffer(3) = water_viscosity_model
            call get_json_value(json, join(buffer), self%materials(i_material)%hydraulic%water_viscosity_model, &
                                is_required=.true., valid_range=[0, 2])
        end select

    end subroutine read_parameters_materials_hydraulic

    subroutine read_parameters_materials_wrf(wrf, json, buffer, end_index)
        implicit none
        class(type_materials_wrf), intent(inout) :: wrf
        type(json_file), intent(inout) :: json
        character(*), intent(in) :: buffer(:)
        integer(int32), intent(in) :: end_index

        character(len=256), allocatable :: local_buffer(:)

        if (size(buffer) > end_index) then
            allocate (local_buffer(size(buffer)))
        else
            allocate (local_buffer(size(buffer) + 1))
        end if
        local_buffer(1:end_index) = buffer(1:end_index)

        local_buffer(end_index + 1) = model_number
        call get_json_value(json, join(local_buffer), wrf%model_number, is_required=.true., valid_range=[1, 6])

        local_buffer(end_index + 1) = theta_s
        call get_json_value(json, join(local_buffer), wrf%theta_s, is_required=.true., valid_range=[0.0d0, 1.0d0])

        local_buffer(end_index + 1) = theta_r
        call get_json_value(json, join(local_buffer), wrf%theta_r, is_required=.true., valid_range=[0.0d0, 1.0d0])

        local_buffer(end_index + 1) = alpha1
        call get_json_value(json, join(local_buffer), wrf%alpha1, is_required=.true.)

        local_buffer(end_index + 1) = n1
        call get_json_value(json, join(local_buffer), wrf%n1, is_required=.true.)

        wrf%m1 = 1.0d0 - 1.0d0 / wrf%n1

        select case (wrf%model_number)
        case (4)
            local_buffer(end_index + 1) = h_crit
            call get_json_value(json, join(local_buffer), wrf%h_crit, is_required=.true., valid_range=[-huge(0.0d0), 0.0d0])
        case (5)
            local_buffer(end_index + 1) = alpha2
            call get_json_value(json, join(local_buffer), wrf%alpha2, is_required=.true.)

            local_buffer(end_index + 1) = n2
            call get_json_value(json, join(local_buffer), wrf%n2, is_required=.true.)
            wrf%m2 = 1.0d0 - 1.0d0 / wrf%n2

            local_buffer(end_index + 1) = w1
            call get_json_value(json, join(local_buffer), wrf%w1, is_required=.true., valid_range=[0.0d0, 1.0d0])
            wrf%w2 = 1.0d0 - wrf%w1
        case (6)
            local_buffer(end_index + 1) = alpha2
            call get_json_value(json, join(local_buffer), wrf%alpha2, is_required=.true.)

            local_buffer(end_index + 1) = n2
            call get_json_value(json, join(local_buffer), wrf%n2, is_required=.true.)
            wrf%m2 = 1.0d0 - 1.0d0 / wrf%n2

            local_buffer(end_index + 1) = w1
            call get_json_value(json, join(local_buffer), wrf%w1, is_required=.true., valid_range=[0.0d0, 1.0d0])
            wrf%w2 = 1.0d0 - wrf%w1
        end select

        ! ----------------------------------------------------------------
        ! 2. 型に固有のパラメータを読み込む
        ! ----------------------------------------------------------------
        select type (wrf)
        type is (type_materials_hcf)
            local_buffer(end_index + 1) = l
            call get_json_value(json, join(local_buffer), wrf%l, is_required=.true., valid_range=[0.0d0, huge(0.0d0)])

        class default
            ! do nothing
        end select

    end subroutine read_parameters_materials_wrf

    module subroutine display_material_settings(self)
        implicit none
        class(type_material_settings), intent(in) :: self

        ! --- 1. Basic Information ---
        call display_material_basic(self)

        ! --- 2. Thermal Properties ---
        if (self%calculate_thermal) then
            call display_material_thermal(self)
        end if

        ! --- 3. Hydraulic Properties ---
        if (self%calculate_hydraulic) then
            call display_material_hydraulic(self)
        end if

        ! --- 4. Mechanical Properties (for future implementation) ---
        if (self%calculate_mechanical) then
            write (*, '(a)') "  Mechanical Properties: (Not implemented)"
        end if

    end subroutine display_material_settings

    subroutine display_material_basic(material)
        implicit none
        class(type_material_settings), intent(in) :: material

        write (*, '(a, i0)') "  ID                  : ", material%id
        write (*, '(a, a)') "  Name                : ", trim(material%name)
        write (*, '(a, i0)') "  Phase Count         : ", material%phase
        write (*, '(a, g0)') "  Is Frozen           : ", material%is_frozen
        write (*, '(a, g0)') "  Is Dispersed        : ", material%is_dispersed
        write (*, '(a, g0)') "  Calculate Thermal   : ", material%calculate_thermal
        write (*, '(a, g0)') "  Calculate Hydraulic : ", material%calculate_hydraulic
        write (*, '(a, g0)') "  Calculate Mechanical: ", material%calculate_mechanical
    end subroutine display_material_basic

    subroutine display_material_thermal(material)
        implicit none
        class(type_material_settings), intent(in) :: material
        character(:), allocatable :: fmt
        integer :: j

        write (*, '(a)') "  --- Thermal Properties ---"
        fmt = '(a, '//to_string(size(material%thermal%density))//'(es12.4e2, 2x))'
        write (*, fmt) "    Density             : ", (material%thermal%density(j), j=1, size(material%thermal%density))
        fmt = '(a, '//to_string(size(material%thermal%specific_heat))//'(es12.4e2, 2x))'
        write (*, fmt) "    Specific Heat       : ", (material%thermal%specific_heat(j), j=1, size(material%thermal%specific_heat))
        fmt = '(a, '//to_string(size(material%thermal%thermal_conductivity))//'(es12.4e2, 2x))'
        write (*, fmt) "    Thermal Conductivity: ", (material%thermal%thermal_conductivity(j), &
                                                      j=1, size(material%thermal%thermal_conductivity))

        if (material%is_dispersed) then
            write (*, '(a, es12.4e2, " / ", es12.4e2)') "    Dispersivity (L/T)  : ", &
                material%thermal%thermal_conductivity_dispersity(1), material%thermal%thermal_conductivity_dispersity(2)
        end if

        if (material%is_frozen) then
            write (*, '(a)') "    --- Phase Change ---"
            write (*, '(a, es12.4e2)') "      Latent Heat Fusion  : ", material%thermal%phase_change%latent_heat_fusion
            write (*, '(a, es12.4e2)') "      Freezing Temp       : ", material%thermal%phase_change%freezing_temperature
            call display_materials_wrf(material%thermal%phase_change%wrf, "      Unfrozen Water Model")

            write (*, '(a)') "      --- Equilibrium Model ---"
            if (material%thermal%phase_change%gcc%is_segregation) then
                write (*, '(a)') "        Type                : Segregation"
            else
                write (*, '(a)') "        Type                : Equilibrium"
            end if
            write (*, '(a, a)') "        Unit                : ", trim(material%thermal%phase_change%gcc%unit)
        end if
    end subroutine display_material_thermal

    subroutine display_material_hydraulic(material)
        implicit none
        class(type_material_settings), intent(in) :: material

        write (*, '(a)') "  --- Hydraulic Properties ---"
        write (*, '(a, i0)') "    Conductivity Model #: ", material%hydraulic%model_number
        write (*, '(a, es12.4e2)') "    Saturated K         : ", material%hydraulic%hydraulic_conductivity
        write (*, '(a, i0)') "    Water Viscosity Mod #: ", material%hydraulic%water_viscosity_model
        write (*, '(a, es12.4e2)') "    Impedance Factor    : ", material%hydraulic%impedance_factor
        call display_materials_wrf(material%hydraulic%hcf, "    Water Retention Model")
    end subroutine display_material_hydraulic

    subroutine display_materials_wrf(wrf, title)
        implicit none
        class(type_materials_wrf), intent(in) :: wrf
        character(*), intent(in) :: title

        write (*, '(a, a, i0)') trim(title), ": #", wrf%model_number
        write (*, '(a, es12.4e2)') "        theta_s           : ", wrf%theta_s
        write (*, '(a, es12.4e2)') "        theta_r           : ", wrf%theta_r
        write (*, '(a, es12.4e2)') "        alpha1            : ", wrf%alpha1
        write (*, '(a, es12.4e2)') "        n1                : ", wrf%n1

        select case (wrf%model_number)
        case (4)
            write (*, '(a, es12.4e2)') "        h_crit            : ", wrf%h_crit
        case (5, 6)
            write (*, '(a, es12.4e2)') "        alpha2            : ", wrf%alpha2
            write (*, '(a, es12.4e2)') "        n2                : ", wrf%n2
            write (*, '(a, es12.4e2)') "        w1                : ", wrf%w1
        end select

        ! This checks if the object is the extended HCF type and prints its specific parameter.
        select type (wrf)
        type is (type_materials_hcf)
            write (*, '(a, es12.4e2)') "        l (HCF specific)  : ", wrf%l
        end select
    end subroutine display_materials_wrf

end submodule inout_input_basic_materials

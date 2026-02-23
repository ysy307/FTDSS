submodule(inout_input_translator) input_translator_basic
    implicit none
contains

    module subroutine execute_basic_swcc(self, input, material_id, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id
        class(type_config_wrf), intent(inout) :: config

        type(type_constant_id) :: L_unit
        real(real64) :: scale_pressure

        if (material_id < 1 .or. material_id > self%basic%num_materials) then
            error stop "Input Error: material_id is out of range."
        end if

        select type (config)

            !==================================================
            ! WRF 共通処理（HCFもここを通る）
            !==================================================
        class is (type_config_wrf)
            associate (material => input%basic%materials(material_id)%water_characteristic_curve)

                call config%reset()

                config%material_id = material_id
                config%model = SWCC_MODELS%to_object(material%model_number)
                L_unit = PHYSICS_UNITS%to_object(material%unit)

                config%theta_s = material%theta_s
                config%theta_r = material%theta_r
                config%alpha1 = material%alpha1

                ! ---- モデル別基本設定 ----
                select case (config%model%ID)

                case (SWCC_MODELS%WRF_VG%ID)
                    config%n1 = material%n1
                    config%m1 = 1.0d0 - 1.0d0 / material%n1

                case (SWCC_MODELS%WRF_MVG%ID)
                    config%n1 = material%n1
                    config%m1 = 1.0d0 - 1.0d0 / material%n1
                    config%h_crit = material%h_crit

                case (SWCC_MODELS%WRF_DURNER%ID)
                    config%n1 = material%n1
                    config%m1 = 1.0d0 - 1.0d0 / material%n1
                    config%alpha2 = material%alpha2
                    config%n2 = material%n2
                    config%m2 = 1.0d0 - 1.0d0 / material%n2
                    config%w1 = material%w1
                    config%w2 = 1.0d0 - material%w1

                case (SWCC_MODELS%WRF_DVGCH%ID)
                    config%n1 = material%n1
                    config%m1 = 1.0d0 - 1.0d0 / material%n1
                    config%n2 = material%n2
                    config%m2 = 1.0d0 - 1.0d0 / material%n2
                    config%w1 = material%w1
                    config%w2 = 1.0d0 - material%w1

                end select

                ! ---- 単位変換係数 ----
                select case (L_unit%ID)
                case (PHYSICS_UNIT_M)
                    scale_pressure = 1000.0d0 * 9.80655d0
                case (PHYSICS_UNIT_CM)
                    scale_pressure = 1000.0d0 * 9.80655d0 * 1.0d-2
                case (PHYSICS_UNIT_PA)
                    scale_pressure = 1.0d0
                case default
                    scale_pressure = 1.0d0
                end select

                ! ---- モデル別スケーリング ----
                select case (config%model%ID)

                case (SWCC_MODELS%WRF_BC%ID, SWCC_MODELS%WRF_KO%ID)
                    config%alpha1 = config%alpha1 * scale_pressure
                    config%h_crit = config%h_crit * scale_pressure
                    config%alpha2 = config%alpha2 * scale_pressure

                case (SWCC_MODELS%WRF_VG%ID, SWCC_MODELS%WRF_DVGCH%ID)
                    config%alpha1 = config%alpha1 / scale_pressure

                case (SWCC_MODELS%WRF_MVG%ID)
                    config%alpha1 = config%alpha1 / scale_pressure
                    config%h_crit = config%h_crit * scale_pressure

                case (SWCC_MODELS%WRF_DURNER%ID)
                    config%alpha1 = config%alpha1 / scale_pressure
                    config%alpha2 = config%alpha2 / scale_pressure

                end select

            end associate

            !==================================================
            ! HCF 追加処理（WRF処理の後に上乗せ）
            !==================================================
        type is (type_config_hcf)

            associate (material => input%basic%materials(material_id)%water_characteristic_curve)

                config%hcf_model = HCF_MODELS%to_object(material%hcf_model_number)
                config%water_viscosity_model = VISCOSITY_MODELS%to_object(material%viscosity_model)

                config%k_sat = material%k_sat
                config%l = material%l
                config%omega = material%omega
                config%gain_factor = material%gain_factor

            end associate

        class default
            error stop "execute_basic_swcc: unsupported config type."

        end select

    end subroutine execute_basic_swcc

    module subroutine execute_basic_gcc(self, input, material_id, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id
        class(type_config_gcc), intent(inout) :: config

        if (material_id < 1 .or. material_id > self%basic%num_materials) then
            error stop "Input Error: material_id is out of range."
        end if

        select type (config)

            !==================================================
            ! GCC モデルの処理
            !==================================================
        class is (type_config_gcc)
            associate (material => input%basic%materials(material_id)%phase%gcc)

                call config%reset()

                config%material_id = material_id
                if (material%is_segregation) then
                    config%model = GCC_TYPES%SEGRGATION
                else
                    config%model = GCC_TYPES%NON_SEGREGATION
                end if

            end associate

        class default
            error stop "execute_basic_gcc: unsupported config type."
        end select

    end subroutine execute_basic_gcc

end submodule input_translator_basic

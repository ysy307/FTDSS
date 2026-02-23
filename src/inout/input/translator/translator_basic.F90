submodule(inout_input_translator) input_translator_basic
    implicit none
contains

    module subroutine execute_basic_wrf(self, input, material_id, config)
        implicit none
        class(type_input_translator), intent(in) :: self
        class(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id
        type(type_config_wrf), intent(inout) :: config

        type(type_constant_id) :: L_unit

        real(real64) :: scale_pressure

        if (material_id < 1 .or. material_id > self%basic%num_materials) then
            error stop "Input Error: material_id is out of range in get_wrf_info."
        end if

        associate (material => input%basic%materials(material_id)%water_characteristic_curve)
            call config%reset()

            config%model = SWCC_MODELS%to_object(material%model_number)
            L_unit = PHYSICS_UNITS%to_object(material%unit)
            config%theta_s = material%theta_s
            config%theta_r = material%theta_r
            config%alpha1 = material%alpha1
            if (config%model == SWCC_MODELS%WRF_VG) then
                config%n1 = material%n1
                config%m1 = 1.0d0 - 1.0d0 / material%n1
            else if (config%model == SWCC_MODELS%WRF_MVG) then
                config%n1 = material%n1
                config%m1 = 1.0d0 - 1.0d0 / material%n1
                config%h_crit = material%h_crit
            else if (config%model == SWCC_MODELS%WRF_DURNER) then
                config%n1 = material%n1
                config%m1 = 1.0d0 - 1.0d0 / material%n1
                config%alpha2 = material%alpha2
                config%n2 = material%n2
                config%m2 = 1.0d0 - 1.0d0 / material%n2
                config%w1 = material%w1
                config%w2 = 1.0d0 - material%w1
            else if (config%model == SWCC_MODELS%WRF_DVGCH) then
                config%n1 = material%n1
                config%m1 = 1.0d0 - 1.0d0 / material%n1
                config%n2 = material%n2
                config%m2 = 1.0d0 - 1.0d0 / material%n2
                config%w1 = material%w1
                config%w2 = 1.0d0 - material%w1
            end if

            ! --- 変換係数の決定 ---
            if (L_unit%id == PHYSICS_UNIT_M) then
                ! m -> Pa (圧力の単位変換: 1 mH2O = 1000 kg/m3 * 9.80655 m/s2 = 9806.55 Pa)
                scale_pressure = 1000.0d0 * 9.80655d0
            else if (L_unit%id == PHYSICS_UNIT_CM) then
                ! cm -> m -> Pa (圧力の単位変換: 1 cmH2O = 0.01 mH2O = 98.0655 Pa)
                scale_pressure = 1000.0d0 * 9.80655d0 * 1.0d-2
            else if (L_unit%id == PHYSICS_UNIT_PA) then
                ! Pa -> Pa (圧力の単位変換: 1 Pa = 1 Pa)
                scale_pressure = 1.0d0
            else
                scale_pressure = 1.0d0
            end if

            ! --- モデルごとのパラメータ変換 ---
            if (config%model == SWCC_MODELS%WRF_BC .or. &
                config%model == SWCC_MODELS%WRF_KO) then
                config%alpha1 = config%alpha1 * scale_pressure
                config%h_crit = config%h_crit * scale_pressure
                config%alpha2 = config%alpha2 * scale_pressure

            else if (config%model == SWCC_MODELS%WRF_VG .or. &
                     config%model == SWCC_MODELS%WRF_DVGCH) then
                config%alpha1 = config%alpha1 / scale_pressure
            else if (config%model == SWCC_MODELS%WRF_VG .or. &
                     config%model == SWCC_MODELS%WRF_MVG) then
                config%alpha1 = config%alpha1 / scale_pressure
                config%h_crit = config%h_crit * scale_pressure
            else if (config%model == SWCC_MODELS%WRF_DURNER) then
                config%alpha1 = config%alpha1 / scale_pressure
                config%alpha2 = config%alpha2 / scale_pressure
            end if

        end associate

    end subroutine execute_basic_wrf

end submodule input_translator_basic

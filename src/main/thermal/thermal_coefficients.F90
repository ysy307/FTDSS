submodule(main_thermal) thermal_coefficients
    implicit none
contains

    !>
    !> @brief 単位体積あたりのエンタルピー(内部エネルギー)密度 U [J/m3] を計算する
    !>
    module subroutine calc_enthalpy_density_thermal(self, material_id, state, U)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: U

        ! ローカル変数
        real(real64) :: temperature
        real(real64) :: porosity, Qw, Qi, Qv
        real(real64) :: rho_s, rho_w, rho_i
        real(real64) :: c_s, c_w, c_i, c_v
        real(real64) :: Lf, Lv

        ! 1. 状態量の取得
        call state%temperature%get(temperature)
        call state%porosity%get(porosity)
        call state%water_content%get(Qw)
        call state%ice_content%get(Qi)
        call state%vapor_content%get(Qv)

        ! 2. 物性値の取得・計算
        call self%physics%get_density_solid(material_id, rho_s)
        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_density_ice(state, rho_i)

        call self%physics%get_specific_heat_solid(material_id, c_s)
        call self%physics%calc_specific_heat_water(state, c_w)
        call self%physics%calc_specific_heat_ice(state, c_i)
        call self%physics%calc_specific_heat_vapor(state, c_v)

        call self%physics%calc_latent_heat_fusion(material_id, state, Lf)
        call self%physics%calc_latent_heat_vaporization(material_id, state, Lv)

        ! 3. エンタルピー密度の計算 (提示された式)
        !    U = (顕熱項) + (潜熱項)
        U = c_s * rho_s * (1.0d0 - porosity) * temperature &
            + c_w * rho_w * Qw * temperature &
            + c_i * rho_i * Qi * temperature &
            + c_v * rho_w * Qv * temperature &
            - rho_i * Lf * Qi &
            + rho_w * Lv * Qv

    end subroutine calc_enthalpy_density_thermal

    module pure elemental subroutine calc_density_water_thermal(self, state, rho_water)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: rho_water

        call self%physics%calc_density_water(state, rho_water)

    end subroutine calc_density_water_thermal

    module pure elemental subroutine calc_density_ice_thermal(self, state, rho_ice)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: rho_ice

        call self%physics%calc_density_ice(state, rho_ice)

    end subroutine calc_density_ice_thermal

    module pure elemental subroutine calc_density_vapor_saturation_thermal(self, state, rho_vapor_sat)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: rho_vapor_sat

        call self%physics%calc_density_vapor_saturation(state, rho_vapor_sat)

    end subroutine calc_density_vapor_saturation_thermal

    module pure elemental subroutine update_water_phases_thermal(self, material_id, state)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state

        call self%physics%update_water_phases(material_id, state)

    end subroutine update_water_phases_thermal

    module subroutine compute_transient_term_thermal(self, material_id, state, bdf_coeffs, dU_dt)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(in) :: bdf_coeffs(:)
        real(real64), intent(inout) :: dU_dt

        type(type_state) :: local_state
        real(real64), allocatable :: temperature_history(:)
        real(real64), allocatable :: pressure_history(:)
        real(real64), allocatable :: porosity_history(:)

        real(real64) :: porosity, Qw, Qi, Qv
        real(real64) :: rho_s, rho_w, rho_i
        real(real64) :: c_s, c_w, c_i, c_v
        real(real64) :: Lf, Lv
        real(real64) :: T, Uj
        integer(int32) :: j, n

        call state%get(temperature_history=temperature_history, &
                       pressure_history=pressure_history, &
                       porosity_history=porosity_history)

        dU_dt = 0.0d0
        do j = 1, size(bdf_coeffs)
            call local_state%reset()
            call local_state%set(temperature=temperature_history(j), &
                                 pressure=pressure_history(j), &
                                 porosity=porosity_history(j))
            call self%update_water_phases(material_id, local_state)
            call self%calc_enthalpy_density(material_id, local_state, Uj)
            dU_dt = dU_dt + bdf_coeffs(j) * Uj
        end do

    end subroutine compute_transient_term_thermal

    module subroutine compute_mass_term_thermal(self, target_material_id, state, C_TT)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: target_material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: C_TT

        real(real64) :: temperature
        real(real64) :: porosity, Qw, Qi, Qv
        real(real64) :: rho_s, rho_w, rho_i
        real(real64) :: c_s, c_w, c_i, c_v
        real(real64) :: drho_w_dT, drho_ice_dT
        real(real64) :: drho_w_dP, drho_ice_dP
        real(real64) :: dQw_dT, dQi_dT, dQv_dT
        real(real64) :: dQw_dP, dQi_dP, dQv_dP
        real(real64) :: Lf, Lv

        ! Get state variables
        call state%get(temperature=temperature, &
                       porosity=porosity, &
                       water_content=Qw, &
                       ice_content=Qi, &
                       vapor_content=Qv)

        C_TT = 0.0d0

        if (porosity > 0.0d0) then
            call self%physics%get_density_solid(target_material_id, rho_s)
            call self%physics%get_specific_heat_solid(target_material_id, c_s)
            C_TT = C_TT + rho_s * c_s * (1.0d0 - porosity)
        end if

        if (Qw > 0.0d0) then
            call self%physics%calc_density_water(state, rho_w)
            call self%physics%calc_specific_heat_water(state, c_w)
            call state%dQw_dT%get(dQw_dT)
            call state%dQw_dP%get(dQw_dP)
            C_TT = C_TT + rho_w * c_w * Qw
        end if

        if (Qi > 0.0d0) then
            call self%physics%calc_density_ice(state, rho_i)
            call self%physics%calc_specific_heat_ice(state, c_i)
            call self%physics%calc_latent_heat_fusion(target_material_id, state, Lf)
            call state%dQi_dT%get(dQi_dT)
            call state%dQi_dP%get(dQi_dP)
            C_TT = C_TT + rho_i * c_i * Qi - Lf * rho_i * dQi_dT
        end if

        if (Qv > 0.0d0) then
            call self%physics%calc_density_water(state, rho_w)
            call self%physics%calc_specific_heat_vapor(state, c_v)
            call state%dQv_dT%get(dQv_dT)
            call state%dQv_dP%get(dQv_dP)
            C_TT = C_TT + rho_w * c_v * Qv + Lv * rho_w * dQv_dT
        end if
        ! call self%physics%calc_density_water(state, rho_w)
        ! call self%physics%calc_density_ice(state, rho_i)
        ! call self%physics%calc_density_water_derivatives(target_material_id, state, drho_w_dT, drho_w_dP)
        ! call self%physics%calc_density_ice_derivatives(target_material_id, state, drho_ice_dT, drho_ice_dP)
        ! call self%physics%calc_specific_heat_water(state, c_w)
        ! call self%physics%calc_specific_heat_ice(state, c_i)
        ! call self%physics%calc_specific_heat_vapor(state, c_v)
        ! call self%physics%calc_latent_heat_fusion(target_material_id, state, Lf)
        ! call self%physics%calc_latent_heat_vaporization(target_material_id, state, Lv)

        ! Heat Capacity Calculation
        ! C_TT = c_s * rho_s * (1.0d0 - porosity) &
        !        + c_w * rho_w * Qw &
        !        + c_i * rho_i * Qi &
        !        + c_v * rho_w * Qv &
        !        - Lf * rho_i * dQi_dT &
        !        + Lv * rho_w * dQv_dT
        ! Additional Terms from Density and Content Derivatives
        ! C_TT = c_s * rho_s * (1.0d0 - porosity) &
        !        + c_w * rho_w * Qw + c_w * Qw * temperature * drho_w_dT + c_w * rho_w * temperature * dQw_dT &
        !        + c_i * rho_i * Qi + c_i * Qi * temperature * drho_ice_dT + c_i * rho_i * temperature * dQi_dT &
        !        + c_v * rho_w * Qv + c_v * Qv * temperature * drho_w_dT + c_v * rho_w * temperature * dQv_dT &
        !        - Lf * Qi * drho_ice_dT - Lf * rho_i * dQi_dT &
        !        + Lv * Qv * drho_w_dT + Lv * rho_w * dQv_dT

    end subroutine compute_mass_term_thermal

    module subroutine compute_diffusion_term_thermal(self, target_material_id, state, D_TT)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: target_material_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: D_TT(:, :)

        type(type_thc_dispersivity) :: lambda

        call self%physics%calc_thermal_conductivity(target_material_id, state, lambda)

        D_TT(:, :) = 0.0d0
        select case (self%computation_type)
        case (COMP_TYPE_2D_XY)
            D_TT(1, 1) = lambda%lambda_xx
            D_TT(1, 2) = lambda%lambda_xy
            D_TT(2, 1) = lambda%lambda_xy
            D_TT(2, 2) = lambda%lambda_yy
        case (COMP_TYPE_2D_XZ)
            D_TT(1, 1) = lambda%lambda_xx
            D_TT(1, 2) = lambda%lambda_zx
            D_TT(2, 1) = lambda%lambda_zx
            D_TT(2, 2) = lambda%lambda_zz
        case (COMP_TYPE_3D)
            D_TT(1, 1) = lambda%lambda_xx
            D_TT(1, 2) = lambda%lambda_xy
            D_TT(1, 3) = lambda%lambda_zx
            D_TT(2, 1) = lambda%lambda_xy
            D_TT(2, 2) = lambda%lambda_yy
            D_TT(2, 3) = lambda%lambda_yz
            D_TT(3, 1) = lambda%lambda_zx
            D_TT(3, 2) = lambda%lambda_yz
            D_TT(3, 3) = lambda%lambda_zz
        end select

    end subroutine compute_diffusion_term_thermal

    module subroutine compute_advective_term_thermal(self, target_material_id, state, V_TT)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: target_material_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: V_TT(:)

        type(type_coordinate_dp) :: water_flux, vapor_flux
        real(real64) :: rho_w, c_w, c_v

        ! Get fluxes
        call state%get(water_flux=water_flux, &
                       vapor_flux=vapor_flux)

        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_specific_heat_water(state, c_w)
        call self%physics%calc_specific_heat_vapor(state, c_v)

        V_TT(:) = 0.0d0
        select case (self%computation_type)
        case (COMP_TYPE_2D_XY)
            V_TT(1) = c_w * rho_w * water_flux%x + c_v * rho_w * vapor_flux%x
            V_TT(2) = c_w * rho_w * water_flux%y + c_v * rho_w * vapor_flux%y
        case (COMP_TYPE_2D_XZ)
            V_TT(1) = c_w * rho_w * water_flux%x + c_v * rho_w * vapor_flux%x
            V_TT(2) = c_w * rho_w * water_flux%z + c_v * rho_w * vapor_flux%z
        case (COMP_TYPE_3D)
            V_TT(1) = c_w * rho_w * water_flux%x + c_v * rho_w * vapor_flux%x
            V_TT(2) = c_w * rho_w * water_flux%y + c_v * rho_w * vapor_flux%y
            V_TT(3) = c_w * rho_w * water_flux%z + c_v * rho_w * vapor_flux%z
        end select

    end subroutine compute_advective_term_thermal

    module subroutine compute_latent_term_thermal(self, target_material_id, state, L_TT)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: target_material_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: L_TT

        real(real64) :: rho_w, L_v, K_vT

        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_latent_heat_vaporization(target_material_id, state, L_v)
        call self%physics%calc_KvT(target_material_id, state, K_vT)

        L_TT = 0.0d0
        L_TT = rho_w * L_v * K_vT

    end subroutine compute_latent_term_thermal

    !>
    !> 積分点における履歴項（History Term）を計算する
    !>
    !> 定義
    !>   H_term = C_TT(T^m) * sum_{k=1}^{order} ( alpha_k * T_{n+1-k} )
    !>
    !>   bdf_coeffs(1) は alpha_0 (現在の項) なのでループから除外する。
    !>   temperature_history(1) も current (現在の項) なのでループから除外する。
    !>   よって、インデックス j=2 から開始することで整合する。
    !>
    module subroutine compute_history_term_thermal(self, material_id, state, bdf_coeffs, history_term)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(in) :: bdf_coeffs(:)
        real(real64), intent(inout) :: history_term

        real(real64), allocatable :: temperature_history(:)
        real(real64) :: C_TT
        real(real64) :: T_hist_sum
        integer(int32) :: j, n_steps

        ! 1. 積分点における温度履歴配列を取得
        call state%get(temperature_history=temperature_history)

        ! 2. 積分点における熱容量係数 C_TT を計算
        !    (現在の反復温度 T^m に基づいて評価)
        call self%compute_mass_term(material_id, state, C_TT)

        ! 3. 履歴項の積和計算
        !    History = alpha_1 * T_{n} + alpha_2 * T_{n-1} + ...
        !    配列のインデックス j=2 が alpha_1 および T_{n} に対応する
        T_hist_sum = 0.0d0
        n_steps = size(bdf_coeffs)

        do j = 2, n_steps
            ! 安全のため配列サイズ確認（通常は一致するはず）
            if (j > size(temperature_history)) exit

            T_hist_sum = T_hist_sum + bdf_coeffs(j) * temperature_history(j)
        end do

        ! 4. 熱容量を乗じて履歴項とする
        history_term = C_TT * T_hist_sum

    end subroutine compute_history_term_thermal
end submodule thermal_coefficients

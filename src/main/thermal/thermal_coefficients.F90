submodule(main_thermal) thermal_coefficients
    implicit none

contains

    !>
    !> @brief 単位体積あたりのエンタルピー(内部エネルギー)密度 U [J/m3] を計算する
    !> @note  これは状態方程式 U = f(T, phi, ...) です。
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
        logical :: has_rho_w

        call state%temperature%get(temperature)
        call state%porosity%get(porosity)
        call state%water_content%get(Qw)
        call state%ice_content%get(Qi)
        call state%vapor_content%get(Qv)

        U = 0.0d0
        has_rho_w = .false.

        ! Solid phase
        if (porosity > 0.0d0) then
            call self%physics%get_density_solid(material_id, rho_s)
            call self%physics%get_specific_heat_solid(material_id, c_s)
            U = U + rho_s * c_s * (1.0d0 - porosity) * temperature
        end if

        ! Water phase
        if (Qw > 0.0d0) then
            call self%physics%calc_density_water(state, rho_w)
            has_rho_w = .true.
            call self%physics%calc_specific_heat_water(state, c_w)
            U = U + rho_w * c_w * Qw * temperature
        end if

        ! Ice phase (Include Latent Heat of Fusion)
        if (Qi > 0.0d0) then
            call self%physics%calc_density_ice(state, rho_i)
            call self%physics%calc_specific_heat_ice(state, c_i)
            call self%physics%calc_latent_heat_fusion(material_id, state, Lf)
            ! Reference state logic implies Lf is removed at T < 0
            U = U + rho_i * c_i * Qi * temperature - Lf * rho_i * Qi
        end if

        ! Vapor phase (Include Latent Heat of Vaporization)
        if (Qv > 0.0d0) then
            if (.not. has_rho_w) then
                call self%physics%calc_density_water(state, rho_w) ! 通常、蒸気の基準密度は液相
                has_rho_w = .true.
            end if
            call self%physics%calc_specific_heat_vapor(state, c_v)
            call self%physics%calc_latent_heat_vaporization(material_id, state, Lv)
            U = U + rho_w * c_v * Qv * temperature + Lv * rho_w * Qv
        end if

    end subroutine calc_enthalpy_density_thermal

    ! --------------------------------------------------------------------------
    ! Helper Wrappers ()
    ! --------------------------------------------------------------------------
    module subroutine calc_density_water_thermal(self, state, rho_water)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: rho_water
        call self%physics%calc_density_water(state, rho_water)
    end subroutine calc_density_water_thermal

    module subroutine calc_density_ice_thermal(self, state, rho_ice)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: rho_ice
        call self%physics%calc_density_ice(state, rho_ice)
    end subroutine calc_density_ice_thermal

    module subroutine calc_density_vapor_saturation_thermal(self, state, rho_vapor_sat)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: rho_vapor_sat
        call self%physics%calc_density_vapor_saturation(state, rho_vapor_sat)
    end subroutine calc_density_vapor_saturation_thermal

    module subroutine update_water_phases_thermal(self, material_id, state)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state
        call self%physics%update_water_phases(material_id, state)
    end subroutine update_water_phases_thermal

    ! ==========================================================================
    ! Transient / Mass Terms (Refactored for NR & Picard)
    ! ==========================================================================

    !>
    !> @brief 時間微分項(Residul)を計算する: dU/dt
    !> @details
    !>   NR法の残差計算に使用します。
    !>   Res_transient = dU_dt = sum(bdf_coeffs(j) * U(t_{n+1-j}))
    !>
    module subroutine compute_transient_term_thermal(self, material_id, state, bdf_coeffs, dU_dt)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(in) :: bdf_coeffs(:)
        real(real64), intent(inout) :: dU_dt

        type(type_state) :: local_state
        real(real64), pointer, contiguous, dimension(:) :: temperature_history
        real(real64), pointer, contiguous, dimension(:) :: pressure_history
        real(real64), pointer, contiguous, dimension(:) :: porosity_history
        real(real64) :: Uj
        integer(int32) :: j, n_hist

        call state%get(temperature_history=temperature_history, &
                       pressure_history=pressure_history, &
                       porosity_history=porosity_history)

        dU_dt = 0.0d0
        if (.not. associated(temperature_history)) return
        if (.not. associated(pressure_history)) return
        if (.not. associated(porosity_history)) return
        n_hist = min(size(bdf_coeffs), size(temperature_history), size(pressure_history), size(porosity_history))
        do j = 1, n_hist
            ! 履歴データから状態を復元してエンタルピーを再計算
            ! Note: 計算コスト削減のため、本来はUの履歴をstateに持たせることが望ましい
            call local_state%temperature%set(temperature_history(j))
            call local_state%pressure%set(pressure_history(j))
            call local_state%porosity%set(porosity_history(j))
            call self%update_water_phases(material_id, local_state)
            call self%calc_enthalpy_density(material_id, local_state, Uj)

            dU_dt = dU_dt + bdf_coeffs(j) * Uj
        end do

    end subroutine compute_transient_term_thermal

    !>
    !> @brief 熱容量係数 C_TT = dU/dT (または Delta U / Delta T) を計算する
    !> @details
    !>   NR法のヤコビアン、またはPicard法の質量行列に使用します。
    !>   dTが小さい、または scheme_opt=TANGENT 指定時は接線熱容量。
    !>   dTが大きい、または scheme_opt=SECANT 指定時は割線熱容量(有効熱容量)。
    !>
    module subroutine compute_mass_term_thermal(self, material_id, state, C_TT, scheme_opt)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: C_TT
        integer(int32), intent(in), optional :: scheme_opt

        real(real64) :: temperature
        real(real64), pointer, contiguous, dimension(:) :: temperature_history
        real(real64), pointer, contiguous, dimension(:) :: pressure_history
        real(real64), pointer, contiguous, dimension(:) :: porosity_history

        ! Tangent用
        real(real64) :: porosity, Qw, Qi, Qv
        real(real64) :: rho_s, rho_w, rho_i
        real(real64) :: c_s, c_w, c_i, c_v
        real(real64) :: dQi_dT, dQv_dT, Lf, Lv
        logical :: has_rho_w

        ! Secant用
        real(real64) :: C_TT_current, C_TT_old, dT
        type(type_state) :: temp_state
        integer(int32) :: use_scheme

        ! デフォルトの挙動決定（指定がなければ自動判定）
        call state%get(temperature=temperature, temperature_history=temperature_history, &
                       pressure_history=pressure_history, porosity_history=porosity_history)

        if (.not. associated(temperature_history)) then
            C_TT = 0.0d0
            return
        end if
        if (.not. associated(pressure_history)) then
            C_TT = 0.0d0
            return
        end if
        if (.not. associated(porosity_history)) then
            C_TT = 0.0d0
            return
        end if

        if (size(temperature_history) >= 2) then
            dT = temperature - temperature_history(2)
        else
            dT = 0.0d0
        end if

        if (present(scheme_opt)) then
            use_scheme = scheme_opt
        else
            ! 従来ロジック: 温度変化が大きければSecantで安定化、小さければTangent
            if (abs(dT) > 1.0d-6) then
                use_scheme = SCHEME_SECANT
            else
                use_scheme = SCHEME_TANGENT
            end if
        end if

        C_TT = 0.0d0
        has_rho_w = .false.

        if (use_scheme == SCHEME_SECANT) then
            ! --- Secant Method (Average/Effective Heat Capacity) ---
            ! C_eff = (U(T) - U(T_old)) / (T - T_old)

            if (size(temperature_history) < 2 .or. size(pressure_history) < 2 .or. size(porosity_history) < 2) then
                use_scheme = SCHEME_TANGENT
            end if

        end if

        if (use_scheme == SCHEME_SECANT) then

            ! Current U
            call self%calc_enthalpy_density(material_id, state, C_TT_current)

            ! Old U (from previous time step)
            call temp_state%temperature%set(temperature_history(2))
            call temp_state%pressure%set(pressure_history(2))
            call temp_state%porosity%set(porosity_history(2))
            call self%update_water_phases(material_id, temp_state)
            call self%calc_enthalpy_density(material_id, temp_state, C_TT_old)

            ! ゼロ除算防止
            dT = sign(max(abs(dT), 1.0d-8), dT)
            C_TT = (C_TT_current - C_TT_old) / dT

        else
            ! --- Tangent Method (Apparent Heat Capacity) ---
            ! C_app = dU/dT

            call state%get(porosity=porosity, water_content=Qw, &
                           ice_content=Qi, vapor_content=Qv)

            ! Solid
            if (porosity > 0.0d0) then
                call self%physics%get_density_solid(material_id, rho_s)
                call self%physics%get_specific_heat_solid(material_id, c_s)
                C_TT = C_TT + rho_s * c_s * (1.0d0 - porosity)
            end if

            ! Water
            if (Qw > 0.0d0) then
                call self%physics%calc_density_water(state, rho_w)
                has_rho_w = .true.
                call self%physics%calc_specific_heat_water(state, c_w)
                C_TT = C_TT + rho_w * c_w * Qw
            end if

            ! Ice (including latent heat derivative)
            if (Qi > 0.0d0) then
                call self%physics%calc_density_ice(state, rho_i)
                call self%physics%calc_specific_heat_ice(state, c_i)
                call self%physics%calc_latent_heat_fusion(material_id, state, Lf)
                call state%dQi_dT%get(dQi_dT)
                C_TT = C_TT + rho_i * c_i * Qi - Lf * rho_i * dQi_dT
            end if

            ! Vapor (including latent heat derivative)
            if (Qv > 0.0d0) then
                if (.not. has_rho_w) then
                    call self%physics%calc_density_water(state, rho_w)
                    has_rho_w = .true.
                end if
                call self%physics%calc_specific_heat_vapor(state, c_v)
                call self%physics%calc_latent_heat_vaporization(material_id, state, Lv)
                call state%dQv_dT%get(dQv_dT)
                C_TT = C_TT + rho_w * c_v * Qv + Lv * rho_w * dQv_dT
            end if
        end if

        ! 物理的に負の熱容量はありえないためクリップ
        C_TT = max(C_TT, 0.0d0)

    end subroutine compute_mass_term_thermal

    ! ==========================================================================
    ! Flux Terms (Diffusion / Advection / Latent Source)
    ! ==========================================================================

    module subroutine compute_diffusion_term_thermal(self, material_id, state, D_TT)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: D_TT(:, :)
        type(type_thc_dispersivity) :: lambda

        call self%physics%calc_thermal_conductivity(material_id, state, lambda)

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

    module subroutine compute_advective_term_thermal(self, material_id, state, V_TT)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: V_TT(:)

        type(type_coordinate_dp), pointer :: water_flux, vapor_flux
        real(real64) :: rho_w, c_w, c_v
        real(real64) :: flux_norm1

        call state%get(water_flux=water_flux, vapor_flux=vapor_flux)

        V_TT(:) = 0.0d0
        flux_norm1 = abs(water_flux%x) + abs(water_flux%y) + abs(water_flux%z) + &
                 abs(vapor_flux%x) + abs(vapor_flux%y) + abs(vapor_flux%z)
        if (flux_norm1 <= epsilon(1.0d0)) return

        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_specific_heat_water(state, c_w)
        call self%physics%calc_specific_heat_vapor(state, c_v)

        select case (self%computation_type)
        case (COMP_TYPE_2D_XY)
            V_TT(1) = rho_w * (c_w * water_flux%x + c_v * vapor_flux%x)
            V_TT(2) = rho_w * (c_w * water_flux%y + c_v * vapor_flux%y)
        case (COMP_TYPE_2D_XZ)
            V_TT(1) = rho_w * (c_w * water_flux%x + c_v * vapor_flux%x)
            V_TT(2) = rho_w * (c_w * water_flux%z + c_v * vapor_flux%z)
        case (COMP_TYPE_3D)
            V_TT(1) = rho_w * (c_w * water_flux%x + c_v * vapor_flux%x)
            V_TT(2) = rho_w * (c_w * water_flux%y + c_v * vapor_flux%y)
            V_TT(3) = rho_w * (c_w * water_flux%z + c_v * vapor_flux%z)
        end select
    end subroutine compute_advective_term_thermal

    module subroutine compute_latent_term_thermal(self, material_id, state, L_TT)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: L_TT

        real(real64) :: rho_w, L_v, K_vT
        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_latent_heat_vaporization(material_id, state, L_v)
        call self%physics%calc_KvT(material_id, state, K_vT)
        L_TT = rho_w * L_v * K_vT
    end subroutine compute_latent_term_thermal

    !>
    !> @brief 積分点における履歴項（History Term）を計算する
    !> @details
    !>   Picard法などで方程式を C(T) * dT/dt + ... = 0 と線形化する場合に使用します。
    !>   History = C_TT * sum_{k=2}^{order} ( alpha_k * T_{n+1-k} )
    !>
    !>   @warning NR法で「エンタルピー法」として残差を構成する場合（compute_transient_termを使用する場合）、
    !>            この項は使用しないでください。重複してカウントすることになります。
    !>
    module subroutine compute_history_term_thermal(self, material_id, state, bdf_coeffs, history_term)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(in) :: bdf_coeffs(:)
        real(real64), intent(inout) :: history_term

        real(real64), pointer, contiguous, dimension(:) :: temperature_history
        real(real64) :: C_TT
        real(real64) :: T_hist_sum
        integer(int32) :: j, n_steps

        call state%get(temperature_history=temperature_history)

        ! 熱容量の計算（Picard法では一般に現在の反復温度でのSecant等を用いる）
        call self%compute_mass_term(material_id, state, C_TT)

        T_hist_sum = 0.0d0
        n_steps = size(bdf_coeffs)

        ! alpha_0 (current) は除外して、過去の項のみを合計
        do j = 2, n_steps
            if (j > size(temperature_history)) exit
            T_hist_sum = T_hist_sum + bdf_coeffs(j) * temperature_history(j)
        end do

        history_term = C_TT * T_hist_sum

    end subroutine compute_history_term_thermal

end submodule thermal_coefficients

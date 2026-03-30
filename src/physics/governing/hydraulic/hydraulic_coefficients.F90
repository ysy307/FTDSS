submodule(physics_governing_hydraulic) hydraulic_coefficients
    implicit none
contains

    !> @brief Calculate Mass Term C_HH = d(rho_eff)/dP
    module subroutine compute_mass_term_hydraulic(self, material_id, state, C_HH)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: C_HH

        real(real64) :: Qw, Qi, Qv
        real(real64) :: rho_w, rho_i
        real(real64) :: drho_w_dP, drho_ice_dP
        real(real64) :: dP_ice_dP_water
        real(real64) :: dQw_dP, dQi_dP, dQv_dP

        dQw_dP = 0.0d0
        dQi_dP = 0.0d0
        dQv_dP = 0.0d0
        drho_w_dP = 0.0d0
        drho_ice_dP = 0.0d0
        dP_ice_dP_water = 0.0d0

        call state%water_content%get(Qw)
        call state%ice_content%get(Qi)
        call state%vapor_content%get(Qv)

        call state%dQw_dP%get(dQw_dP)
        call state%dQi_dP%get(dQi_dP)
        call state%dQv_dP%get(dQv_dP)

        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_density_ice(state, rho_i)

        ! C_HH = d(rho_eff)/dP >= 0 (thermodynamic constraint)
        ! rho_eff = rho_w*Qw + rho_i*Qi + rho_w*Qv
        ! Clamp: near freezing front rho_i*dQi_dP can exceed rho_w*dQw_dP numerically.
        C_HH = max(0.0d0, rho_w * dQw_dP + Qw * drho_w_dP &
               + rho_i * dQi_dP + Qi * drho_ice_dP * dP_ice_dP_water &
               + rho_w * dQv_dP + Qv * drho_w_dP)

    end subroutine compute_mass_term_hydraulic

    !> @brief Calculate Diffusion Term D_HH (Hydraulic Conductivity Tensor)
    !> @details
    !>   J_m_diff = - D_HH * grad P
    !>   From Darcy: q = -K/rho_w g * grad P ...
    !>   J_m = rho_w * q = - K/g * grad P
    !>   D_HH = K_eff / g
    module subroutine compute_diffusion_term_hydraulic(self, material_id, state, D_HH)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: D_HH(:, :)

        real(real64) :: K_flh, K_vP
        real(real64) :: coeff_D
        integer(int32) :: i

        ! K_flh: Liquid Hydraulic Conductivity [m/s]
        ! K_vP: Vapor Hydraulic Conductivity (equivalent) [m/s]
        call self%physics%calc_Kflh(material_id, state, K_flh)
        call self%physics%calc_Kvh(material_id, state, K_vP)

        ! D_HH = (K_liquid + K_vapor) / g
        ! Unit: [m/s] / [m/s^2] = [s]
        ! Flux J = - D * grad P [s * Pa/m] = [s * N/m^3] = [s * kg m/s^2 / m^3] = [kg/m^2 s] (Mass Flux)
        coeff_D = (K_flh + K_vP) / g

        D_HH(:, :) = 0.0d0
        do i = 1, self%computation_dimension
            D_HH(i, i) = coeff_D
        end do

    end subroutine compute_diffusion_term_hydraulic

    !> @brief Calculate Advective (Gravity) Term V_H
    !> @details
    !>   J_m_grav = V_H
    !>   J_m_grav = rho_w * (-K * grad z)
    !>   V_H = - rho_w * K * grad z
    module subroutine compute_advective_term_hydraulic(self, material_id, state, V_H)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: V_H(:)

        real(real64) :: rho_w, K_flh
        real(real64) :: grav_flux_mag

        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_Kflh(material_id, state, K_flh)

        ! Gravity Mass Flux Magnitude = rho_w * K
        grav_flux_mag = rho_w * K_flh

        V_H(:) = 0.0d0

        ! Assuming z is the vertical coordinate acting against gravity
        select case (self%computation_type)
        case (COMP_TYPES%XZ_2D%ID)
            V_H(2) = -grav_flux_mag ! z-direction
        case (COMP_TYPES%XYZ_3D%ID)
            V_H(3) = -grav_flux_mag ! z-direction
        case (COMP_TYPES%XY_2D%ID)
            ! Usually gravity is perpendicular to XY plane or handled differently
            ! Assuming XY implies horizontal plane, gravity term might be zero or handled externally
            V_H(:) = 0.0d0
        end select

    end subroutine compute_advective_term_hydraulic

    !> @brief Calculate Transient Term (drho_eff/dt)
    module subroutine compute_transient_term_hydraulic(self, material_id, state, bdf_coeffs, drho_dt)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(in) :: bdf_coeffs(:)
        real(real64), intent(inout) :: drho_dt

        ! Reuse existing implementation
        call self%calc_effective_density(material_id, state, bdf_coeffs, drho_dt)

    end subroutine compute_transient_term_hydraulic

    ! ==========================================================================
    ! Coupling Coefficients
    ! ==========================================================================

    !> @brief Calculate Coupling Mass Term C_HT = d(rho_eff)/dT
    module subroutine compute_coupling_mass_term_hydraulic(self, material_id, state, C_HT)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: C_HT

        real(real64) :: rho_w, rho_i
        real(real64) :: dQw_dT, dQi_dT, dQv_dT

        dQw_dT = 0.0d0
        dQi_dT = 0.0d0
        dQv_dT = 0.0d0

        call state%dQw_dT%get(dQw_dT)
        call state%dQi_dT%get(dQi_dT)
        call state%dQv_dT%get(dQv_dT)

        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_density_ice(state, rho_i)

        ! C_HT = d(rho_eff)/dT
        ! rho_eff = rho_w*Qw + rho_i*Qi + rho_w*Qv
        C_HT = rho_w * dQw_dT + rho_i * dQi_dT + rho_w * dQv_dT

    end subroutine compute_coupling_mass_term_hydraulic

    !> @brief Calculate Coupling Diffusion Term D_HT
    !> @details
    !>   Total moisture flux driven by temperature gradient:
    !>   \( J^{(T)} = -(K_{vT} + K_{flh}/g \cdot d\psi_{cryo}/dT) \nabla T \)
    !>   The cryo-suction liquid flux term \( K_{flh}/g \cdot d\psi_{cryo}/dT \)
    !>   is the dominant mechanism for water transport toward the freezing front.
    module subroutine compute_coupling_diffusion_term_hydraulic(self, material_id, state, D_HT)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: D_HT(:, :)

        real(real64) :: K_vT, K_flh, dpsi_cryo_dT
        real(real64) :: coeff_D
        integer(int32) :: i

        call self%calc_K_vT(material_id, state, K_vT)
        call self%physics%calc_Kflh(material_id, state, K_flh)
        call self%physics%calc_cryo_suction_deriv_T(material_id, state, dpsi_cryo_dT)

        ! D_HT = K_vT - K_flh/g * dpsi_cryo/dT
        ! Derived from total potential: Psi = P_w - psi_cryo + rho_w*g*z
        ! Flux J = -(K/g)*grad(P) + (K/g)*grad(psi_cryo) - rho_w*K*grad(z)
        ! Fitting to -D_HT*grad(T): D_HT = K_vT - (K_flh/g)*dpsi_cryo/dT
        coeff_D = K_vT - K_flh / g * dpsi_cryo_dT

        D_HT(:, :) = 0.0d0
        do i = 1, self%computation_dimension
            D_HT(i, i) = coeff_D
        end do

    end subroutine compute_coupling_diffusion_term_hydraulic

    ! ==========================================================================
    ! Helper Wrappers (Existing)
    ! ==========================================================================
    module subroutine calc_K_wT_hydraulic(self, target_id, state, K_wT)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: K_wT
        call self%physics%calc_KlT(target_id, state, K_wT)
    end subroutine calc_K_wT_hydraulic

    module subroutine calc_K_wP_hydraulic(self, target_id, state, K_wP)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: K_wP
        call self%physics%calc_Kflh(target_id, state, K_wP)
    end subroutine calc_K_wP_hydraulic

    module subroutine calc_K_vT_hydraulic(self, target_id, state, K_vT)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: K_vT
        call self%physics%calc_KvT(target_id, state, K_vT)
    end subroutine calc_K_vT_hydraulic

    module subroutine calc_K_vP_hydraulic(self, target_id, state, K_vP)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: K_vP
        call self%physics%calc_Kvh(target_id, state, K_vP)
    end subroutine calc_K_vP_hydraulic

    module subroutine update_water_phases_hydraulic(self, material_id, state)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state
        call self%physics%update_water_phases(material_id, state)
    end subroutine update_water_phases_hydraulic

    module subroutine calc_effective_density_hydraulic(self, material_id, state, bdf_coeffs, drho_dt)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(in) :: bdf_coeffs(:)
        real(real64), intent(inout) :: drho_dt

        type(type_state) :: local_state
        real(real64), pointer, dimension(:), contiguous :: temperature_history
        real(real64), pointer, dimension(:), contiguous :: pressure_history

        real(real64) :: Qw, Qi, Qv
        real(real64) :: rho_w, rho_i
        real(real64) :: Uj
        integer(int32) :: j, n

        nullify (temperature_history)
        nullify (pressure_history)

        call state%temperature_history%get(temperature_history)
        call state%pressure_history%get(pressure_history)

        drho_dt = 0.0d0
        if (.not. associated(temperature_history)) return
        if (.not. associated(pressure_history)) return

        n = min(size(bdf_coeffs), size(temperature_history), size(pressure_history))
        call local_state%copy(state)

        do j = 1, n
            call local_state%temperature%set(temperature_history(j))
            call local_state%pressure%set(pressure_history(j))

            call self%update_water_phases(material_id, local_state)

            call local_state%water_content%get(Qw)
            call local_state%ice_content%get(Qi)
            call local_state%vapor_content%get(Qv)

            call self%physics%calc_density_water(local_state, rho_w)
            call self%physics%calc_density_ice(local_state, rho_i)

            ! Effective Density (Mass per unit volume)
            Uj = rho_w * Qw &
                 + rho_i * Qi &
                 + rho_w * Qv

            drho_dt = drho_dt + bdf_coeffs(j) * Uj
        end do

    end subroutine calc_effective_density_hydraulic

    !> @brief Compute equivalent specific moisture capacity C_eq = dTheta/dP.
    module subroutine compute_C_eq_hydraulic(self, material_id, state, C_eq)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: C_eq

        real(real64) :: rho_w, rho_i
        real(real64) :: dQw_dP, dQi_dP, dQv_dP

        dQw_dP = 0.0d0
        dQi_dP = 0.0d0
        dQv_dP = 0.0d0

        call state%dQw_dP%get(dQw_dP)
        call state%dQi_dP%get(dQi_dP)
        call state%dQv_dP%get(dQv_dP)

        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_density_ice(state, rho_i)

        ! C_eq = dTheta/dP = dQw_dP + (rho_i/rho_w)*dQi_dP + dQv_dP
        ! (rho_v ~ rho_w approximation consistent with calc_effective_density)
        ! Clamp to non-negative: thermodynamically dTheta/dP >= 0 always holds,
        ! but phase_systems compensation (dQw_dP = -dQi_dP) can cause numerical
        ! sign reversal near the freezing front.
        C_eq = max(0.0d0, dQw_dP + (rho_i / rho_w) * dQi_dP + dQv_dP)

    end subroutine compute_C_eq_hydraulic

    !> @brief Compute BDF approximation of dTheta/dt for Mixed formulation.
    module subroutine compute_transient_term_mixed_hydraulic(self, material_id, state, bdf_coeffs, dTheta_dt)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(in) :: bdf_coeffs(:)
        real(real64), intent(inout) :: dTheta_dt

        type(type_state) :: local_state
        real(real64), pointer, dimension(:), contiguous :: temperature_history
        real(real64), pointer, dimension(:), contiguous :: pressure_history

        real(real64) :: Qw, Qi, Qv
        real(real64) :: rho_w, rho_i
        real(real64) :: Theta_j
        integer(int32) :: j, n

        nullify (temperature_history)
        nullify (pressure_history)

        call state%temperature_history%get(temperature_history)
        call state%pressure_history%get(pressure_history)

        dTheta_dt = 0.0d0
        if (.not. associated(temperature_history)) return
        if (.not. associated(pressure_history)) return

        n = min(size(bdf_coeffs), size(temperature_history), size(pressure_history))
        call local_state%copy(state)

        do j = 1, n
            call local_state%temperature%set(temperature_history(j))
            call local_state%pressure%set(pressure_history(j))

            call self%update_water_phases(material_id, local_state)

            call local_state%water_content%get(Qw)
            call local_state%ice_content%get(Qi)
            call local_state%vapor_content%get(Qv)

            call self%physics%calc_density_water(local_state, rho_w)
            call self%physics%calc_density_ice(local_state, rho_i)

            ! Theta = Qw + (rho_i/rho_w)*Qi + Qv  (rho_v ~ rho_w)
            Theta_j = Qw + (rho_i / rho_w) * Qi + Qv

            dTheta_dt = dTheta_dt + bdf_coeffs(j) * Theta_j
        end do

    end subroutine compute_transient_term_mixed_hydraulic

end submodule hydraulic_coefficients

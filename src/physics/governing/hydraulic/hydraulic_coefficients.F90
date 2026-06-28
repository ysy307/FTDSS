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
        real(real64) :: phi, theta_tot
        real(real64) :: cap_ref
        real(real64), parameter :: S_s = 1.0d-8
        ! L-scheme capacity floor for the freezing-front runaway (Pop/Radu L-scheme).
        ! As cryosuction migration drives p_w down, the capillary suction P_aw = -p_w
        ! climbs the dry tail of the water-retention curve where the moisture capacity
        ! d(theta)/dp collapses (C_HH falls from ~1e-3 to ~3e-5). The pressure update
        ! dp = R / C_HH then amplifies as C_HH shrinks: a positive feedback that
        ! overshoots the cryosuction equilibrium and diverges (du_p ~ 1e11 Pa). The
        ! L-scheme replaces the strongly varying capacity on the LHS by a constant
        ! L >= sup C_HH (here the near-saturation peak capacity), so the iteration
        ! matrix diagonal cannot collapse and the feedback is removed. It enters ONLY
        ! the LHS capacity (the residual recomputes the true Theta), so the converged
        ! solution is unchanged: p_w still drops to the cryosuction equilibrium and
        ! water migrates. L is the soil's own peak moisture capacity (a material
        ! property), evaluated at zero capillary suction, not a tuned constant.

        dQw_dP = 0.0d0
        dQi_dP = 0.0d0
        dQv_dP = 0.0d0
        drho_w_dP = 0.0d0
        drho_ice_dP = 0.0d0
        dP_ice_dP_water = 1.0d0
        phi = 0.0d0

        call state%water_content%get(Qw)
        call state%ice_content%get(Qi)
        call state%vapor_content%get(Qv)

        call state%dQw_dP%get(dQw_dP)
        call state%dQi_dP%get(dQi_dP)
        call state%dQv_dP%get(dQv_dP)

        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_density_ice(state, rho_i)
         call self%physics%calc_density_water_derivatives(material_id, state, dden_dP=drho_w_dP)
         call self%physics%calc_density_ice_derivatives(material_id, state, dden_dP=drho_ice_dP)
         call self%physics%calc_pressure_ice_water_derivative(material_id, state, dP_ice_dP_water)

        call state%porosity%get(phi)

         ! C_HH = d(rho_eff)/dP
        ! rho_eff = rho_w*Qw + rho_i*Qi + rho_w*Qv
         C_HH = rho_w * dQw_dP + Qw * drho_w_dP &
               + rho_i * dQi_dP + Qi * drho_ice_dP * dP_ice_dP_water &
             + rho_w * dQv_dP + Qv * drho_w_dP

        ! Physical specific storage (full-saturation / general non-zero diagonal).
        theta_tot = Qw + (rho_i / rho_w) * Qi + Qv
        C_HH = C_HH + rho_w * S_s * theta_tot / max(phi, 1.0d-12)

        ! L-scheme capacity floor (see declarations). The reference capacity is
        ! computed from the material WRF, converted to dtheta/dP, and applied only
        ! to the Picard LHS. The residual still recomputes the true storage, so the
        ! converged water migration is preserved while the dry-tail diagonal collapse
        ! at the freezing front is removed.
        cap_ref = 0.0d0
        call self%physics%calc_lscheme_capacity(material_id, cap_ref)
        C_HH = max(C_HH, rho_w * cap_ref)

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
        call self%calc_K_vP(material_id, state, K_vP)

        ! D_HH = (K_liquid + K_vapor) / g. The liquid Darcy term retains the full
        ! grad(p_w) Laplacian; the frozen-zone reduction of liquid flow is carried
        ! by the impedance factor inside K_flh (K_s * 10^(-Omega*theta_ice)), which
        ! decreases smoothly with the ice content. A capillary weight w_cap =
        ! d(p_c*)/d(P_aw) was previously applied here, but the cryogenic suction P_iw
        ! rises near-vertically just below 0 C (generalized Clausius-Clapeyron), so
        ! w_cap collapses to 0 within ~0.1 C and removes the pressure Laplacian at the
        ! front, leaving only the tiny specific-storage diagonal. The pressure block
        ! then becomes catastrophically ill-conditioned (du_p ~ 1e9 Pa from the linear
        ! solve) and no relaxation/dt can recover it. The cryosuction-driven migration
        ! is provided by the grad T coupling D_HT; p_w drops toward the front as a
        ! computed result of the mass balance, not by weighting this Laplacian away.
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
            ! No gravity contribution in XY_2D.
            ! Keep the thermo-osmotic term accumulated above.
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
        real(real64), pointer, contiguous, dimension(:) :: temperature_history
        real(real64) :: temperature, dT, rho_eff_cur, rho_eff_old
        type(type_state) :: temp_state
        real(real64), parameter :: DT_SECANT_FLOOR = 1.0d-3

        dQw_dT = 0.0d0
        dQi_dT = 0.0d0
        dQv_dT = 0.0d0

        call state%dQw_dT%get(dQw_dT)
        call state%dQi_dT%get(dQi_dT)
        if (self%enable_vapor_transport) then
            call state%dQv_dT%get(dQv_dT)
        end if

        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_density_ice(state, rho_i)

        ! C_HT = d(rho_eff)/dT  (analytical tangent)
        ! rho_eff = rho_w*Qw + rho_i*Qi + rho_w*Qv (vapor term only when enabled)
        C_HT = rho_w * dQw_dT + rho_i * dQi_dT + rho_w * dQv_dT

        ! Phase-change stabilization (mirrors the thermal C_TT chord): across the
        ! freezing front d(rho_eff)/dT spikes (ice forms, liquid water vanishes), so
        ! the instantaneous tangent overshoots the water-mass response to a
        ! temperature change and the coupled iteration chatters. Replace it by the
        ! chord C_HT = (rho_eff(T^m,p^m) - rho_eff(T^n,p^m)) / (T^m - T^n), which
        ! averages the actual storage change over the step (consistent with the
        ! conservative residual). Evaluated only for |dT| above a floor so it stays
        ! bounded; below that the tangent is the correct slope.
        call state%get(temperature=temperature, temperature_history=temperature_history)
        if (associated(temperature_history)) then
            if (size(temperature_history) >= 2) then
                dT = temperature - temperature_history(2)
                if (abs(dT) > DT_SECANT_FLOOR) then
                    call self%calc_effective_density_value(state, rho_eff_cur)
                    call temp_state%copy(state)
                    call temp_state%temperature%set(temperature_history(2))
                    call self%update_water_phases(material_id, temp_state)
                    call self%calc_effective_density_value(temp_state, rho_eff_old)
                    C_HT = (rho_eff_cur - rho_eff_old) / dT
                end if
            end if
        end if

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
        real(real64) :: coeff_D, temperature_local
        integer(int32) :: i

        call self%calc_K_vT(material_id, state, K_vT)
        call state%temperature%get(temperature_local)

        ! Cryosuction-driven moisture migration: the grad T contribution to the
        ! liquid Darcy flux, D_HT = K_flh/g * |d(psi_cryo)/dT| (active for T < T_f0).
        ! K_flh carries the impedance factor, so this self-limits smoothly as the
        ! soil freezes. This is the coupling that pulls water toward the freezing
        ! front; p_w then drops toward the front as a computed result of the water
        ! mass balance. (The minus sign is correct: d(psi_cryo)/dT < 0.)
        if (self%physics%has_cryo_transport(material_id) .and. temperature_local < 0.0d0) then
            call self%physics%calc_Kflh(material_id, state, K_flh)
            call self%physics%calc_cryo_suction_deriv_T(material_id, state, dpsi_cryo_dT)
            coeff_D = K_vT - K_flh / g * dpsi_cryo_dT
        else
            coeff_D = K_vT
        end if

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
        if (self%enable_vapor_transport) then
            call self%physics%calc_KvT(target_id, state, K_vT)
        else
            K_vT = 0.0d0
        end if
    end subroutine calc_K_vT_hydraulic

    module subroutine calc_K_vP_hydraulic(self, target_id, state, K_vP)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: K_vP
        if (self%enable_vapor_transport) then
            call self%physics%calc_Kvh(target_id, state, K_vP)
        else
            K_vP = 0.0d0
        end if
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

    !> Evaluate the pore-water effective density rho_eff at the supplied state.
    !>
    !> Mathematical definition:
    !> \( \rho_{eff} = \rho_w \theta_w + \rho_{ice} \theta_{ice} + \rho_w \theta_v^{\star} \) [kg/m3]
    !>
    !> This is the conserved storage quantity of the water-mass balance and the plain
    !> counterpart of calc_effective_density (which returns its BDF time-derivative).
    !> Assumptions: the phase contents Qw, Qi, Qv stored in state are already
    !> consistent with (T, p_w) (call update_water_phases beforehand). Used by the
    !> conserved-quantity convergence norm (PDF 6.2.4). Cost: O(1).
    module subroutine calc_effective_density_value_hydraulic(self, state, rho_eff)
        implicit none
        class(type_hydraulic), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: rho_eff

        real(real64) :: Qw, Qi, Qv
        real(real64) :: rho_w, rho_i

        call state%water_content%get(Qw)
        call state%ice_content%get(Qi)
        call state%vapor_content%get(Qv)

        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_density_ice(state, rho_i)

        rho_eff = rho_w * Qw + rho_i * Qi + rho_w * Qv
    end subroutine calc_effective_density_value_hydraulic

    !> @brief Compute equivalent specific moisture capacity C_eq = dTheta/dP.
    module subroutine compute_C_eq_hydraulic(self, material_id, state, C_eq)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: C_eq

        real(real64) :: rho_w, rho_i
        real(real64) :: dQw_dP, dQi_dP, dQv_dP
        real(real64) :: cap_ref

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
        cap_ref = 0.0d0
        call self%physics%calc_lscheme_capacity(material_id, cap_ref)
        C_eq = max(C_eq, cap_ref)

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

    !> @brief Compute segregation sink from temperature gradient magnitude.
    !>
    !> Returns an **effective** S_seg (volumetric liquid-water removal rate
    !> [1/s]) that is consistent with the forward-Euler Qi_seg update clamps.
    !> The PDE sink and the Qi_seg accumulator must see the same rate to
    !> preserve total water mass; any clamping of ice growth (available
    !> water, remaining pore space) is folded back into S_seg here.
    module subroutine calc_segregation_sink_hydraulic(self, material_id, state, dt, S_seg)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(in) :: dt
        real(real64), intent(inout) :: S_seg

        type(type_coordinate_dp), pointer :: grad_T
        real(real64) :: grad_T_mag, S_seg_raw
        real(real64) :: Qw, Qi_pore, Qi_seg, porosity_val
        real(real64) :: delta_seg_raw, delta_seg_clamped, pore_space_left
        real(real64), parameter :: rho_water = 999.8d0
        real(real64), parameter :: rho_ice = 917.0d0
        real(real64), parameter :: density_ratio = rho_water / rho_ice

        S_seg = 0.0d0
        nullify (grad_T)
        call state%grad_T%get(grad_T)
        if (.not. associated(grad_T)) return

        grad_T_mag = sqrt(grad_T%x**2 + grad_T%y**2 + grad_T%z**2)
        if (grad_T_mag <= 0.0d0) return

        S_seg_raw = 0.0d0
        call self%physics%calc_segregation_sink(material_id, state, grad_T_mag, S_seg_raw)
        if (S_seg_raw <= 0.0d0 .or. dt <= 0.0d0) return

        call state%water_content%get(Qw)
        call state%ice_content%get(Qi_pore)
        call state%ice_content_seg%get(Qi_seg)
        call state%porosity%get(porosity_val)

        ! Apply the same forward-Euler clamps used by update_segregation_ice:
        !   (a) cannot consume more liquid water than is locally available
        !   (b) cannot push Qi_pore + Qi_seg + Qw beyond porosity
        delta_seg_raw = density_ratio * S_seg_raw * dt
        delta_seg_clamped = min(delta_seg_raw, density_ratio * max(Qw, 0.0d0))
        pore_space_left = max(porosity_val - Qi_pore - Qi_seg - max(Qw, 0.0d0), 0.0d0)
        delta_seg_clamped = min(delta_seg_clamped, pore_space_left)
        delta_seg_clamped = max(delta_seg_clamped, 0.0d0)

        S_seg = delta_seg_clamped / (density_ratio * dt)

    end subroutine calc_segregation_sink_hydraulic

end submodule hydraulic_coefficients

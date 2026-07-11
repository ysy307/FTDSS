submodule(physics_governing_hydraulic) hydraulic_coefficients
    implicit none
contains

    !> @brief Calculate Diffusion Term D_HH (Hydraulic Conductivity Tensor)
    !> @details
    !>   Mixed water-content form uses volumetric Darcy flux:
    !>   q = -K/(rho_w g) * grad P ...
    !>   D_HH = K_eff / (rho_w g)
    module subroutine compute_diffusion_term_hydraulic(self, material_id, state, D_HH)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: D_HH(:, :)

        real(real64) :: K_flh, K_vP, rho_w
        real(real64) :: w_cap, w_cryo
        real(real64) :: coeff_D

        integer(int32) :: i

        call self%physics%calc_Kflh(material_id, state, K_flh)
        call self%calc_K_vP(material_id, state, K_vP)
        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_suction_weights(material_id, state, w_cap, w_cryo)

        ! D_HH = K_flh * w_cap / (rho_w g): the effective-suction weight w_cap
        ! ensures that in the frozen fringe the capillary and cryosuction driving
        ! forces are not double-counted.  The impedance factor inside K_flh
        ! independently reduces conductivity in the fully-frozen zone.
        coeff_D = (K_flh * w_cap + K_vP) / (rho_w * g)

        D_HH(:, :) = 0.0d0
        do i = 1, self%computation_dimension
            D_HH(i, i) = coeff_D
        end do

    end subroutine compute_diffusion_term_hydraulic

    !> @brief Calculate Advective (Gravity) Term V_H
    !> @details
    !>   q_grav = V_H
    !>   q_grav = -K * grad z
    !>   V_H = -K * grad z
    module subroutine compute_advective_term_hydraulic(self, material_id, state, V_H)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: V_H(:)

        real(real64) :: K_flh
        real(real64) :: grav_flux_mag

        call self%physics%calc_Kflh(material_id, state, K_flh)

        ! Gravity volumetric flux magnitude in the mixed water-content equation.
        grav_flux_mag = K_flh

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

    ! ==========================================================================
    ! Coupling Coefficients
    ! ==========================================================================

    !> @brief Calculate Coupling Mass Term C_HT = dTheta/dT
    module subroutine compute_coupling_mass_term_hydraulic(self, material_id, state, C_HT)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: C_HT

        real(real64) :: rho_w, rho_i
        real(real64) :: dQw_dT, dQi_dT, dQv_dT
        real(real64), pointer, contiguous, dimension(:) :: temperature_history
        real(real64) :: temperature, dT, theta_cur, theta_old
        real(real64) :: Qw, Qi, Qv
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

        ! C_HT = dTheta/dT in the mixed water-content formulation.
        ! Theta = Qw + (rho_i/rho_w)*Qi + Qv. For pore-water freezing, the
        ! Qw/Qi terms cancel when mass is conserved, preventing an artificial
        ! temperature-storage spike in the hydraulic equation.
        C_HT = dQw_dT + (rho_i / rho_w) * dQi_dT + dQv_dT

        ! Phase-change stabilization (mirrors the thermal C_TT chord): use the
        ! actual mixed storage change crossed over the step. Evaluated only for
        ! |dT| above a floor so it stays bounded; below that the tangent is the
        ! correct slope.
        call state%get(temperature=temperature, temperature_history=temperature_history)
        if (associated(temperature_history)) then
            if (size(temperature_history) >= 2) then
                dT = temperature - temperature_history(2)
                if (abs(dT) > DT_SECANT_FLOOR) then
                    call state%water_content%get(Qw)
                    call state%ice_content%get(Qi)
                    call state%vapor_content%get(Qv)
                    theta_cur = Qw + (rho_i / rho_w) * Qi + Qv

                    call temp_state%copy(state)
                    call temp_state%temperature%set(temperature_history(2))
                    call self%update_water_phases(material_id, temp_state)
                    call temp_state%water_content%get(Qw)
                    call temp_state%ice_content%get(Qi)
                    call temp_state%vapor_content%get(Qv)
                    call self%physics%calc_density_water(temp_state, rho_w)
                    call self%physics%calc_density_ice(temp_state, rho_i)
                    theta_old = Qw + (rho_i / rho_w) * Qi + Qv

                    C_HT = (theta_cur - theta_old) / dT
                end if
            end if
        end if

    end subroutine compute_coupling_mass_term_hydraulic

    !> @brief Calculate Coupling Diffusion Term D_HT
    !> @details
    !>   Total moisture flux driven by temperature gradient:
    !>   \( q^{(T)} = -(K_{lT} + K_{vT} + K_{flh}/(\rho_w g) \cdot d\psi_{cryo}/dT) \nabla T \)
    !>   The cryo-suction liquid flux term \( K_{flh}/(\rho_w g) \cdot d\psi_{cryo}/dT \)
    !>   is the dominant mechanism for water transport toward the freezing front.
    module subroutine compute_coupling_diffusion_term_hydraulic(self, material_id, state, D_HT)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: D_HT(:, :)

        real(real64) :: K_lT, K_vT, K_flh, dpsi_cryo_dT, rho_w
        real(real64) :: w_cap, w_cryo
        real(real64) :: coeff_D, temperature_local
        integer(int32) :: i

        call self%physics%calc_KlT(material_id, state, K_lT)
        call self%calc_K_vT(material_id, state, K_vT)
        call state%temperature%get(temperature_local)

        if (self%physics%has_cryo_transport(material_id) .and. temperature_local < 0.0d0) then
            call self%physics%calc_Kflh(material_id, state, K_flh)
            call self%physics%calc_density_water(state, rho_w)
            call self%physics%calc_suction_weights(material_id, state, w_cap, w_cryo)
            call self%physics%calc_cryo_suction_deriv_T(material_id, state, dpsi_cryo_dT)
            if (rho_w > tiny(1.0d0)) then
                coeff_D = K_lT + K_vT - K_flh * w_cryo / (rho_w * g) * dpsi_cryo_dT
            else
                coeff_D = K_lT + K_vT
            end if
        else
            coeff_D = K_lT + K_vT
        end if

        D_HT(:, :) = 0.0d0
        do i = 1, self%computation_dimension
            D_HT(i, i) = coeff_D
        end do

    end subroutine compute_coupling_diffusion_term_hydraulic

    !> Implementation: 5-point Gauss-Legendre quadrature on [T_min, T_max], reusing
    !> the same reference-interval abscissas/weights as domain_fe_integration's line
    !> rule (order 5). Only T is resampled on a local copy of state_ref; every other
    !> field (P, porosity, ...) is carried through unchanged, so the water-phase
    !> re-equilibration at each sample isolates the T-dependence of D_HH/D_HT that
    !> the caller wants path-averaged.
    module subroutine compute_diffusion_term_K_averaged_hydraulic(self, material_id, state_ref, T_min, T_max, &
                                                                   need_D_HT, D_HH_avg, D_HT_avg)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state_ref
        real(real64), intent(in) :: T_min
        real(real64), intent(in) :: T_max
        logical, intent(in) :: need_D_HT
        real(real64), intent(inout) :: D_HH_avg
        real(real64), intent(inout) :: D_HT_avg

        ! 5-point Gauss-Legendre rule on the reference interval [-1, 1]
        ! (same constants as domain_fe_integration's get_legendre_point(5, ...)).
        ! Not declared PARAMETER: Intel's submodule mangled-symbol name for a
        ! module-array constant here (module@submodule + this long procedure
        ! name + variable name) exceeds the linker's global-name length limit;
        ! a plain local array assigned once per call avoids that without any
        ! behavioral difference (still computed once, negligible O(1) cost).
        integer(int32), parameter :: N_GP = 5
        real(real64) :: gp_abscissa(N_GP), gp_weight(N_GP)

        type(type_state) :: local_state
        real(real64) :: T_mid, T_half, T_sample
        real(real64) :: D_mat(self%computation_dimension, self%computation_dimension)
        integer(int32) :: q

        gp_abscissa = [-0.906179845938664d0, -0.538469310105683d0, 0.0d0, 0.538469310105683d0, 0.906179845938664d0]
        gp_weight = [0.236926885056189d0, 0.478628670499366d0, 0.568888888888889d0, 0.478628670499366d0, &
                     0.236926885056189d0]

        T_mid = 0.5d0 * (T_max + T_min)
        T_half = 0.5d0 * (T_max - T_min)

        call local_state%copy(state_ref)

        D_HH_avg = 0.0d0
        D_HT_avg = 0.0d0

        do q = 1, N_GP
            T_sample = T_mid + T_half * gp_abscissa(q)
            call local_state%temperature%set(T_sample)
            call self%update_water_phases(material_id, local_state)

            D_mat(:, :) = 0.0d0
            call self%compute_diffusion_term(material_id, local_state, D_mat)
            ! Average = (1/(T_max-T_min)) * integral = 0.5 * sum(w_i * f(T_i)),
            ! independent of T_half (the reference-to-physical Jacobian cancels
            ! against the 1/(T_max-T_min) normalization of the average).
            D_HH_avg = D_HH_avg + 0.5d0 * gp_weight(q) * D_mat(1, 1)

            if (need_D_HT) then
                D_mat(:, :) = 0.0d0
                call self%compute_coupling_diffusion_term(material_id, local_state, D_mat)
                D_HT_avg = D_HT_avg + 0.5d0 * gp_weight(q) * D_mat(1, 1)
            end if
        end do

    end subroutine compute_diffusion_term_K_averaged_hydraulic

    ! ==========================================================================
    ! Helper Wrappers (Existing)
    ! ==========================================================================
    module subroutine calc_K_wT_hydraulic(self, target_id, state, K_wT)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: K_wT
        real(real64) :: K_flh, dpsi_cryo_dT, rho_w, temperature_local
        real(real64) :: w_cap, w_cryo

        call self%physics%calc_KlT(target_id, state, K_wT)

        ! Keep diagnostic/output liquid flux consistent with the hydraulic
        ! equation: a temperature gradient contributes not only through surface
        ! tension (KlT), but also through generalized Clapeyron cryosuction.
        call state%temperature%get(temperature_local)
        if (self%physics%has_cryo_transport(target_id) .and. temperature_local < 0.0d0) then
            call self%physics%calc_Kflh(target_id, state, K_flh)
            call self%physics%calc_density_water(state, rho_w)
            if (rho_w > tiny(1.0d0)) then
                call self%physics%calc_suction_weights(target_id, state, w_cap, w_cryo)
                call self%physics%calc_cryo_suction_deriv_T(target_id, state, dpsi_cryo_dT)
                K_wT = K_wT - K_flh * w_cryo / (rho_w * g) * dpsi_cryo_dT
            end if
        end if
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

    !> Evaluate the mixed water-equivalent content Theta = Qw + (rho_i/rho_w)*Qi + Qv
    !> at the supplied state. See interface for the mathematical definition.
    module subroutine calc_theta_value_hydraulic(self, state, theta)
        implicit none
        class(type_hydraulic), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: theta

        real(real64) :: Qw, Qi, Qv
        real(real64) :: rho_w, rho_i

        call state%water_content%get(Qw)
        call state%ice_content%get(Qi)
        call state%vapor_content%get(Qv)

        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_density_ice(state, rho_i)

        theta = Qw + (rho_i / rho_w) * Qi + Qv
    end subroutine calc_theta_value_hydraulic

    !> See interface for the mathematical definition.
    module subroutine calc_cryo_suction_hydraulic(self, material_id, state, psi_cryo)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: psi_cryo

        call self%physics%calc_cryo_suction(material_id, state, psi_cryo)
    end subroutine calc_cryo_suction_hydraulic

    !> @brief Compute equivalent specific moisture capacity C_eq = dTheta/dP.
    module subroutine compute_C_eq_hydraulic(self, state, C_eq)
        implicit none
        class(type_hydraulic), intent(in) :: self
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

    !> Compute the nonlinear-iteration capacity from the physical tangent and WRF bound.
    module subroutine compute_iteration_capacity_hydraulic(self, material_id, state, capacity)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: capacity

        real(real64) :: physical_capacity
        real(real64) :: pressure_capacity_bound

        physical_capacity = 0.0d0
        pressure_capacity_bound = 0.0d0
        call self%compute_C_eq(state, physical_capacity)
        call self%physics%calc_lscheme_capacity(material_id, pressure_capacity_bound)
        capacity = max(physical_capacity, pressure_capacity_bound)
    end subroutine compute_iteration_capacity_hydraulic

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
    !> [1/s]) with available-water and open-pore capacity folded into the rate.
    module subroutine calc_segregation_sink_hydraulic(self, material_id, state, dt, S_seg)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        real(real64), intent(in) :: dt
        real(real64), intent(inout) :: S_seg

        type(type_coordinate_dp), pointer :: grad_T
        real(real64) :: grad_T_mag

        S_seg = 0.0d0
        nullify (grad_T)
        call state%grad_T%get(grad_T)
        if (.not. associated(grad_T)) return

        grad_T_mag = sqrt(grad_T%x**2 + grad_T%y**2 + grad_T%z**2)
        if (grad_T_mag <= 0.0d0) return

        call self%physics%calc_effective_segregation_sink(material_id, state, grad_T_mag, dt, S_seg)

    end subroutine calc_segregation_sink_hydraulic

end submodule hydraulic_coefficients

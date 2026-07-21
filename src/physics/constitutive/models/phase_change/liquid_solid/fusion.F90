module models_phase_change_fusion
    use, intrinsic :: iso_fortran_env
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core, only:type_state
    use :: constitutive_constants, only: &
        Tf0 => water_freezing_point_at_standard_atmospheric_pressure, &
        g => gravity_acceleration, rho_std => reference_water_density, &
        SUCTION_BLEND_EPS => suction_blend_epsilon, &
        Lf0 => latent_heat_fusion_water_0C, &
        TtoK => celsius_to_kelvin
    use :: physics_constitutive_base, only:abst_constitutive
    use :: models_wrf, only:abst_wrf
    use :: models_phase_change_gcc, only:abst_gcc
    implicit none
    private

    public :: type_fusion

    !>
    !> @brief Model for fusion (melting/freezing) physics.
    !>
    type, extends(abst_constitutive) :: type_fusion
        private
        class(abst_wrf), pointer :: wrf
        class(abst_gcc), pointer :: gcc
    contains
        procedure, pass(self), public :: initialize => initialize_type_fusion
        procedure, pass(self), public :: calc_water_content
        procedure, pass(self), public :: calc_water_content_derivatives
        procedure, pass(self), public :: calc_effective_suction
        procedure, pass(self), public :: project_ice_content
        procedure, pass(self), public :: calc_saturation_pressure
        procedure, pass(self), public :: deriv_pressure_ice_water

    end type type_fusion

contains

    pure subroutine compute_effective_suction(psi_cap, psi_cryo, psi_eff, dpsi_eff_dpsi_cap, dpsi_eff_dpsi_cryo)
        implicit none
        real(real64), intent(in) :: psi_cap, psi_cryo
        real(real64), intent(inout) :: psi_eff
        real(real64), intent(inout), optional :: dpsi_eff_dpsi_cap, dpsi_eff_dpsi_cryo

        ! Generalized suction p_c* = max(P_aw, P_iw): the water-retention function
        ! responds to whichever interface (air-water capillary P_aw or ice-water
        ! cryogenic P_iw) holds the liquid more strongly. The liquid saturation is
        ! the single relation S_w = F_WRF(p_c*) for both unfrozen and frozen states.
        ! Thermodynamically p_c* is the chemical-potential lowering of soil water,
        ! mu_w = mu_w^sat - v_w * p_c*. A smooth max keeps the derivatives well
        ! defined. The weights are used only for derivatives of retention and
        ! phase storage. Darcy transport is driven by the actual pore-water
        ! pressure, not by the generalized suction.
        real(real64) :: delta_psi, blend_denom

        delta_psi = psi_cap - psi_cryo
        blend_denom = sqrt(delta_psi*delta_psi + SUCTION_BLEND_EPS*SUCTION_BLEND_EPS)

        psi_eff = 0.5d0*(psi_cap + psi_cryo + blend_denom)

        if (present(dpsi_eff_dpsi_cap)) dpsi_eff_dpsi_cap = 0.5d0*(1.0d0 + delta_psi/blend_denom)
        if (present(dpsi_eff_dpsi_cryo)) dpsi_eff_dpsi_cryo = 0.5d0*(1.0d0 - delta_psi/blend_denom)
    end subroutine compute_effective_suction

    !>
    !> @brief Initialize fusion model.
    !>
    subroutine initialize_type_fusion(self, wrf, gcc, water, ice)
        implicit none
        class(type_fusion), intent(inout) :: self
        class(abst_wrf), intent(in), target :: wrf
        class(abst_gcc), intent(in), target :: gcc
        type(type_iapws97), intent(in), target :: water
        type(type_iapws06), intent(in), target :: ice

        self%wrf => wrf
        self%gcc => gcc
        self%water => water
        self%ice => ice
    end subroutine initialize_type_fusion

    !---------------------------------------------------------------------------
    ! Ice Calculations
    !---------------------------------------------------------------------------

    !>
    !> @brief Calculate ice content based on thermodynamic state.
    !>
    !> \[ \theta_{tot} = \theta_{WRF}(-\psi_{cap}) + \frac{\rho_i}{\rho_l}\theta_i \]
    !> \[ \theta_{l,new} = \theta_{WRF}(-\psi_{eff}) \]
    !> \[ \theta_i = \left(\theta_{tot} - \theta_{l,new}\right)\frac{\rho_l}{\rho_i} \]
    !> \(\theta_{WRF}(-\psi_{cap})\) approximates the post-hydraulic-solve liquid content
    !> at current pressure without cryogenic suction, avoiding dependence on the stale
    !> nodal water content field (updated only after convergence).
    !> Assumptions: \(T < 0\) for phase change; \(\theta_i\) from state is the previous-iteration value.
    !> Numerical guarantee: No theoretical error bound available.
    !> Computational complexity: \(O(1)\) arithmetic and memory.
    !> Failure behavior: returns zero ice content when \(T \ge 0\) or \(\theta_{l,new} \ge \theta_{tot}\).
    subroutine calc_ice_content(self, state, ice_content)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: ice_content

        real(real64) :: pressure, temperature
        real(real64) :: psi_cap, psi_cryo, psi_eff
        real(real64) :: theta_l_cap, theta_l_new
        real(real64) :: rho_w, rho_i
        real(real64) :: theta_tot, phi

        call state%temperature%get(temperature)
        call state%pressure%get(pressure)

        if (temperature < 0.0d0) then
            call self%calc_rho_water(state, rho_w)
            call self%calc_rho_ice(state, rho_i)

            if (rho_w <= tiny(1.0d0) .or. rho_i <= tiny(1.0d0)) then
                ice_content = 0.0d0
                return
            end if

            psi_cap = max(0.0d0, -pressure)
            call self%wrf%calc(-psi_cap / (rho_std * g), theta_l_cap)

            call self%gcc%calc(state, psi_cryo)
            call compute_effective_suction(psi_cap, psi_cryo, psi_eff)
            call self%wrf%calc(-psi_eff / (rho_std * g), theta_l_new)

            ! Pore-volume bound on the water-equivalent total theta_tot.
            !
            ! The physical constraint is the instantaneous phase-volume balance
            !   theta_l + theta_i <= phi,
            ! which, with theta_i = (theta_tot - theta_l) * rho_w/rho_i, is
            !   theta_tot <= phi * (rho_i/rho_w) + theta_l * (1 - rho_i/rho_w).
            ! The bound therefore relaxes continuously from phi*(rho_i/rho_w)
            ! (pore entirely ice, theta_l = 0) to phi (pore entirely liquid).
            ! The previous form capped theta_tot at phi*(rho_i/rho_w) uncondition-
            ! ally, i.e. it demanded that the water present could still fit after
            ! freezing ALL of it - far too strict whenever liquid remains. Because
            ! that cap made theta_tot a constant, it also drove the storage
            ! capacity dTheta/dP to zero, so any inflow into a capped node turned
            ! into unbounded pressure growth instead of storage (the +MPa pockets).
            theta_tot = theta_l_cap
            call state%porosity%get(phi)
            if (phi > 0.0d0) then
                theta_tot = min(theta_tot, phi * (rho_i / rho_w) + theta_l_new * (1.0d0 - rho_i / rho_w))
            end if

            if (theta_l_new < theta_tot) then
                ice_content = (theta_tot - theta_l_new) * (rho_w / rho_i)
            else
                ice_content = 0.0d0
            end if
        else
            ice_content = 0.0d0
        end if

    end subroutine calc_ice_content

    !>
    !> @brief Calculate derivatives of ice content w.r.t pressure and temperature.
    !>
    !> \[ \frac{\partial \theta_i}{\partial P} = \frac{\rho_l}{\rho_i}
    !>    \left(\frac{\partial \theta_{l,cap}}{\partial P}
    !>          - \frac{\partial \theta_{l,new}}{\partial P}\right) \]
    !> \[ \frac{\partial \theta_i}{\partial T} = -\frac{\rho_l}{\rho_i}
    !>    \frac{\partial \theta_{l,new}}{\partial T} \]
    !> \(\theta_i\) from state is treated as fixed (previous-iteration value).
    !> Numerical guarantee: No theoretical error bound available.
    !> Computational complexity: \(O(1)\) arithmetic and memory.
    !> Failure behavior: returns zero derivatives when \(T \ge 0\) or \(\theta_{l,new} \ge \theta_{tot}\).
    subroutine calc_ice_content_derivatives(self, state, dice_dP, dice_dT)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: dice_dP
        real(real64), intent(inout) :: dice_dT

        real(real64) :: pressure, temperature
        real(real64) :: psi_cap, psi_cryo, psi_eff
        real(real64) :: d_psi_eff_dpsi_cap, d_psi_eff_dpsi_cryo
        real(real64) :: d_psi_cap_dP, d_psi_cryo_dP, d_psi_cryo_dT
        real(real64) :: d_psi_eff_dP, d_psi_eff_dT
        real(real64) :: theta_l_cap, theta_l_new
        real(real64) :: dtheta_dPin_cap, dtheta_dPin_eff
        real(real64) :: d_theta_cap_dP, d_theta_eff_dP, d_theta_eff_dT
        real(real64) :: rho_w, rho_i
        real(real64) :: theta_tot, phi, d_theta_tot_dP, d_theta_tot_dT, theta_bound

        call state%temperature%get(temperature)
        call state%pressure%get(pressure)

        if (temperature < 0.0d0) then
            call self%calc_rho_water(state, rho_w)
            call self%calc_rho_ice(state, rho_i)

            if (rho_w <= tiny(1.0d0) .or. rho_i <= tiny(1.0d0)) then
                dice_dP = 0.0d0
                dice_dT = 0.0d0
                return
            end if

            if (pressure < 0.0d0) then
                psi_cap = -pressure
                d_psi_cap_dP = -1.0d0
            else
                psi_cap = 0.0d0
                d_psi_cap_dP = 0.0d0
            end if

            call self%wrf%calc(-psi_cap / (rho_std * g), theta_l_cap)
            call self%wrf%deriv(-psi_cap / (rho_std * g), dtheta_dPin_cap)
            d_theta_cap_dP = dtheta_dPin_cap * (-d_psi_cap_dP) / (rho_std * g)

            call self%gcc%calc(state, psi_cryo)
            call self%gcc%deriv_pressure(state, d_psi_cryo_dP)
            call self%gcc%deriv_temperature(state, d_psi_cryo_dT)

            call compute_effective_suction(psi_cap, psi_cryo, psi_eff, d_psi_eff_dpsi_cap, d_psi_eff_dpsi_cryo)
            d_psi_eff_dP = d_psi_eff_dpsi_cap * d_psi_cap_dP + d_psi_eff_dpsi_cryo * d_psi_cryo_dP
            d_psi_eff_dT = d_psi_eff_dpsi_cryo * d_psi_cryo_dT

            call self%wrf%calc(-psi_eff / (rho_std * g), theta_l_new)
            call self%wrf%deriv(-psi_eff / (rho_std * g), dtheta_dPin_eff)

            d_theta_eff_dP = dtheta_dPin_eff * (-d_psi_eff_dP) / (rho_std * g)
            d_theta_eff_dT = dtheta_dPin_eff * (-d_psi_eff_dT) / (rho_std * g)

            ! Pore-volume bound, differentiated consistently with calc_ice_content:
            !   theta_tot <= phi*(rho_i/rho_w) + theta_l*(1 - rho_i/rho_w).
            ! On the bound the total follows the liquid, so
            !   d(theta_tot) = (1 - rho_i/rho_w) * d(theta_l),
            ! and the ice derivatives collapse to dQi = -d(theta_l): further ice
            ! can only replace liquid one-for-one in volume. Off the bound the
            ! total is theta(psi_cap), which has no direct T dependence.
            theta_tot = theta_l_cap
            call state%porosity%get(phi)
            d_theta_tot_dP = d_theta_cap_dP
            d_theta_tot_dT = 0.0d0
            if (phi > 0.0d0) then
                theta_bound = phi * (rho_i / rho_w) + theta_l_new * (1.0d0 - rho_i / rho_w)
                if (theta_l_cap >= theta_bound) then
                    theta_tot = theta_bound
                    d_theta_tot_dP = (1.0d0 - rho_i / rho_w) * d_theta_eff_dP
                    d_theta_tot_dT = (1.0d0 - rho_i / rho_w) * d_theta_eff_dT
                end if
            end if

            if (theta_l_new < theta_tot) then
                dice_dP = (d_theta_tot_dP - d_theta_eff_dP) * (rho_w / rho_i)
                dice_dT = (d_theta_tot_dT - d_theta_eff_dT) * (rho_w / rho_i)
            else
                dice_dP = 0.0d0
                dice_dT = 0.0d0
            end if
        else
            dice_dP = 0.0d0
            dice_dT = 0.0d0
        end if

    end subroutine calc_ice_content_derivatives

    !---------------------------------------------------------------------------
    ! Liquid Water Calculations
    !---------------------------------------------------------------------------

    !>
    !> @brief Calculate liquid water content based on thermodynamic state.
    !>
    subroutine calc_water_content(self, state, water_content)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: water_content

        real(real64) :: pressure
        real(real64) :: psi_cap

        call state%pressure%get(pressure)

        if (pressure < 0.0d0) then
            psi_cap = -pressure
        else
            psi_cap = 0.0d0
        end if

        ! The primary pressure is the actual pore-water pressure. Retention is
        ! therefore evaluated from its capillary head; freezing is represented
        ! by the independent outer ice state and the resulting pressure change.
        call self%wrf%calc(-psi_cap / (rho_std * g), water_content)

    end subroutine calc_water_content

    !>
    !> @brief Calculate derivatives of liquid water content w.r.t pressure and temperature.
    !>
    subroutine calc_water_content_derivatives(self, state, dwater_dP, dwater_dT)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: dwater_dP !> d(theta_l)/dP
        real(real64), intent(inout) :: dwater_dT !> d(theta_l)/dT

        real(real64) :: pressure
        real(real64) :: psi_cap
        real(real64) :: d_psi_cap_dP
        real(real64) :: d_theta_liquid_dPress ! renamed variable

        call state%pressure%get(pressure)

        ! Capillary suction
        if (pressure < 0.0d0) then
            psi_cap = -pressure
            d_psi_cap_dP = -1.0d0
        else
            psi_cap = 0.0d0
            d_psi_cap_dP = 0.0d0
        end if

        call self%wrf%deriv(-psi_cap / (rho_std * g), d_theta_liquid_dPress)

        ! 4. Assemble liquid water content derivatives (chain rule):
        !    dTheta/dP = (dTheta/dh) * dh/dP.
        dwater_dP = d_theta_liquid_dPress * (-d_psi_cap_dP) / (rho_std * g)
        dwater_dT = 0.0d0

    end subroutine calc_water_content_derivatives

    !>
    !> @brief Capillary suction \(\max(0,-p_w)\) [Pa] of actual pore pressure.
    subroutine calc_effective_suction(self, state, psi_eff)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: psi_eff

        real(real64) :: pressure

        call state%pressure%get(pressure)
        psi_eff = max(0.0d0, -pressure)
    end subroutine calc_effective_suction

    !> Project the outer ice state onto local ice-water equilibrium.
    !>
    !> The liquid content is evaluated from the actual pore pressure, while
    !> the equilibrium unfrozen content is evaluated from the Clapeyron
    !> suction. The proposed phase increment is the liquid-equivalent phase
    !> transfer used by the outer water-conserving solve:
    !> \(\Delta\theta_i=(\theta_l(p_w)-\theta_l^{eq}(T))\rho_w/\rho_i\).
    !> Bounds enforce non-negative ice and fixed pore volume. The returned
    !> increment is zero on the admissible side of either active bound.
    subroutine project_ice_content(self, state, projected_ice, ice_increment, equilibrium_error)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: projected_ice
        real(real64), intent(inout) :: ice_increment
        real(real64), intent(inout) :: equilibrium_error

        real(real64) :: current_ice, liquid_pressure, liquid_equilibrium
        real(real64) :: pressure, psi_cap, psi_cryo, rho_w, rho_i, porosity, upper_bound
        ! Active-set tolerance for the ice-free/full-ice complementarity
        ! bounds. It matches the outer phase-content discretization tolerance
        ! so a node is not classified simultaneously as bound-converged and
        ! as an interior point requiring pressure equality.
        real(real64), parameter :: BOUND_TOLERANCE = 1.0d-3
        logical :: ice_is_set

        current_ice = 0.0d0
        ice_is_set = .false.
        call state%ice_content%get(current_ice, ice_is_set)
        if (.not. ice_is_set) current_ice = 0.0d0
        call state%porosity%get(porosity)
        call self%calc_water_content(state, liquid_pressure)
        call self%gcc%calc(state, psi_cryo)
        call self%wrf%calc(-max(0.0d0, psi_cryo) / (rho_std * g), liquid_equilibrium)
        call self%calc_rho_water(state, rho_w)
        call self%calc_rho_ice(state, rho_i)

        if (rho_w <= tiny(1.0d0) .or. rho_i <= tiny(1.0d0)) then
            projected_ice = max(0.0d0, current_ice)
            ice_increment = 0.0d0
            equilibrium_error = 0.0d0
            return
        end if

        projected_ice = current_ice + (liquid_pressure - liquid_equilibrium) * rho_w / rho_i
        upper_bound = max(0.0d0, porosity - liquid_equilibrium)
        projected_ice = min(max(projected_ice, 0.0d0), upper_bound)
        ice_increment = projected_ice - current_ice

        call state%pressure%get(pressure)
        psi_cap = max(0.0d0, -pressure)
        equilibrium_error = 0.0d0
        if (abs(ice_increment) > BOUND_TOLERANCE .or. &
            (current_ice > BOUND_TOLERANCE .and. current_ice < upper_bound - BOUND_TOLERANCE)) then
            equilibrium_error = abs(max(0.0d0, psi_cryo) - psi_cap)
        end if
    end subroutine project_ice_content

    !> Calculate the pore-water pressure at which the gas-filled pore volume vanishes.
    !>
    !> \[ \theta_l(T,p_b)+\theta_i(T,p_b)=\phi \]
    !> Assumptions: local phase equilibrium, monotone WRF, and fixed porosity.
    !> Numerical guarantee: returns a bracketed bisection solution when a transition exists.
    !> Computational complexity: \(O(N_b)\) arithmetic and \(O(1)\) memory.
    !> Failure behavior: reports an unsaturated state when no finite transition is bracketed.
    subroutine calc_saturation_pressure(self, state, saturation_pressure, is_saturated)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: saturation_pressure
        logical, intent(inout) :: is_saturated

        integer(int32), parameter :: max_bisection_iterations = 64
        real(real64), parameter :: pressure_floor = -1.0d12
        type(type_state) :: local_state
        real(real64) :: pressure, porosity, ice_content
        real(real64) :: pressure_low, pressure_high, pressure_mid
        real(real64) :: volume_current, volume_low, volume_high, volume_mid
        real(real64) :: volume_tolerance
        integer(int32) :: i

        saturation_pressure = 0.0d0
        is_saturated = .false.
        call state%pressure%get(pressure)
        call state%porosity%get(porosity)
        call state%ice_content%get(ice_content)
        if (porosity <= 0.0d0) return

        call local_state%copy(state)
        call evaluate_unconstrained_phase_volume(local_state, pressure, volume_current)
        volume_tolerance = 64.0d0 * epsilon(1.0d0) * max(1.0d0, porosity)
        if (volume_current < porosity - volume_tolerance) return

        pressure_high = 0.0d0
        call evaluate_unconstrained_phase_volume(local_state, pressure_high, volume_high)
        if (volume_high < porosity - volume_tolerance) return

        pressure_low = -rho_std * g
        call evaluate_unconstrained_phase_volume(local_state, pressure_low, volume_low)
        do while (volume_low >= porosity .and. pressure_low > pressure_floor)
            pressure_low = max(10.0d0 * pressure_low, pressure_floor)
            call evaluate_unconstrained_phase_volume(local_state, pressure_low, volume_low)
        end do
        if (volume_low >= porosity) return

        do i = 1, max_bisection_iterations
            pressure_mid = 0.5d0 * (pressure_low + pressure_high)
            call evaluate_unconstrained_phase_volume(local_state, pressure_mid, volume_mid)
            if (volume_mid >= porosity) then
                pressure_high = pressure_mid
            else
                pressure_low = pressure_mid
            end if
        end do

        saturation_pressure = 0.5d0 * (pressure_low + pressure_high)
        is_saturated = pressure >= saturation_pressure - &
                       max(1.0d-8 * max(abs(saturation_pressure), 1.0d0), epsilon(1.0d0))

    contains
        subroutine evaluate_unconstrained_phase_volume(candidate_state, candidate_pressure, phase_volume)
            implicit none
            type(type_state), intent(inout) :: candidate_state
            real(real64), intent(in) :: candidate_pressure
            real(real64), intent(inout) :: phase_volume

            real(real64) :: theta_liquid

            call candidate_state%pressure%set(candidate_pressure)
            call self%calc_water_content(candidate_state, theta_liquid)
            phase_volume = theta_liquid + ice_content
        end subroutine evaluate_unconstrained_phase_volume
    end subroutine calc_saturation_pressure

    !>
    !> @brief Calculate derivative of ice pressure w.r.t water pressure.
    !>
    subroutine deriv_pressure_ice_water(self, state, deriv)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv

        call self%gcc%deriv_pressure_ice_water(state, deriv)

    end subroutine deriv_pressure_ice_water

end module models_phase_change_fusion

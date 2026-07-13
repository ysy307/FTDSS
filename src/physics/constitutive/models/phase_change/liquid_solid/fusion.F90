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
        procedure, pass(self), public :: calc_ice_content
        procedure, pass(self), public :: calc_ice_content_derivatives
        procedure, pass(self), public :: calc_water_content
        procedure, pass(self), public :: calc_water_content_derivatives
        procedure, pass(self), public :: calc_effective_suction
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
        ! defined; dpsi_eff_dpsi_cap and dpsi_eff_dpsi_cryo are the weights with which
        ! the capillary (grad p_w) and cryogenic (grad T) contributions enter the
        ! Darcy flux driven by grad(p_c*).
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
        real(real64) :: psi_cap, psi_cryo, psi_eff

        call state%pressure%get(pressure)

        if (pressure < 0.0d0) then
            psi_cap = -pressure
        else
            psi_cap = 0.0d0
        end if

        call self%gcc%calc(state, psi_cryo)
        call compute_effective_suction(psi_cap, psi_cryo, psi_eff)

        ! Pass negative pressure head [m] to WRF
        call self%wrf%calc(-psi_eff / (rho_std * g), water_content)

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
        real(real64) :: psi_cap, psi_cryo
        real(real64) :: d_psi_cap_dP
        real(real64) :: d_psi_cryo_dP, d_psi_cryo_dT
        real(real64) :: psi_eff, d_psi_eff_dpsi_cap, d_psi_eff_dpsi_cryo
        real(real64) :: d_psi_eff_dP, d_psi_eff_dT
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

        ! Cryogenic suction
        call self%gcc%calc(state, psi_cryo)

        call self%gcc%deriv_pressure(state, d_psi_cryo_dP)
        call self%gcc%deriv_temperature(state, d_psi_cryo_dT)

        ! Select effective suction and determine derivatives with smooth blending.
        call compute_effective_suction(psi_cap, psi_cryo, psi_eff, d_psi_eff_dpsi_cap, d_psi_eff_dpsi_cryo)
        d_psi_eff_dP = d_psi_eff_dpsi_cap*d_psi_cap_dP + d_psi_eff_dpsi_cryo*d_psi_cryo_dP
        d_psi_eff_dT = d_psi_eff_dpsi_cryo*d_psi_cryo_dT

        ! 3. Compute moisture capacity (dTheta/dh) where h is in meters
        call self%wrf%deriv(-psi_eff / (rho_std * g), d_theta_liquid_dPress)

        ! 4. Assemble liquid water content derivatives (chain rule):
        !    dTheta/dP = (dTheta/dh) * dh/dP = (dTheta/dh) * (-dpsi_eff/dP) / (rho_std*g)
        dwater_dP = d_theta_liquid_dPress * (-d_psi_eff_dP) / (rho_std * g)
        dwater_dT = d_theta_liquid_dPress * (-d_psi_eff_dT) / (rho_std * g)

    end subroutine calc_water_content_derivatives

    !>
    !> @brief Generalized suction \(p_c^* = \max(\psi_{cap}, \psi_{cryo})\) [Pa].
    !>
    !> The liquid-phase potential is \(-p_c^*\); water retention AND relative
    !> permeability must be evaluated at this suction for thermodynamic
    !> consistency of the single-potential freezing model.
    subroutine calc_effective_suction(self, state, psi_eff)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: psi_eff

        real(real64) :: pressure, psi_cap, psi_cryo

        call state%pressure%get(pressure)
        psi_cap = max(0.0d0, -pressure)

        psi_cryo = 0.0d0
        call self%gcc%calc(state, psi_cryo)
        call compute_effective_suction(psi_cap, psi_cryo, psi_eff)
    end subroutine calc_effective_suction

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

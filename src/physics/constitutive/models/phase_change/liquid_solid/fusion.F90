module models_phase_change_fusion
    use, intrinsic :: iso_fortran_env
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core, only:type_state
    use :: constitutive_constants, only: &
        Tf0 => water_freezing_point_at_standard_atmospheric_pressure
    use :: physics_constitutive_base, only:abst_constitutive
    use :: models_wrf, only:abst_wrf
    use :: models_phase_change_gcc, only:abst_gcc
    implicit none
    private

    public :: type_fusion

    ! Smooth blending scale [Pa] for effective suction max(psi_cap, psi_cryo).
    ! A small positive epsilon avoids non-differentiable switching at psi_cap=psi_cryo.
    real(real64), parameter :: SUCTION_BLEND_EPS = 1.0d4
    real(real64), parameter :: FREEZING_SMOOTH_BAND = 0.25d0
    !> Pore-space cap: Qi <= (porosity - theta_l_eff - PORE_MARGIN).
    !> A small margin keeps phase_systems's hard cap out of the picture.
    real(real64), parameter :: PORE_MARGIN = 1.0d-4
    !> Smoothing scale for pore cap, relative to porosity. A wider scale keeps
    !> the Jacobian contribution from Qi_unbounded alive deeper into the capped
    !> regime, which is critical for deep-subfreezing thermal convergence.
    real(real64), parameter :: PORE_CAP_SMOOTH_REL = 2.0d-2

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
        procedure, pass(self), public :: deriv_pressure_ice_water

    end type type_fusion

contains

    pure subroutine compute_effective_suction(psi_cap, psi_cryo, psi_eff, dpsi_eff_dpsi_cap, dpsi_eff_dpsi_cryo)
        implicit none
        real(real64), intent(in) :: psi_cap, psi_cryo
        real(real64), intent(inout) :: psi_eff
        real(real64), intent(inout), optional :: dpsi_eff_dpsi_cap, dpsi_eff_dpsi_cryo

        real(real64) :: delta_psi, blend_denom

        delta_psi = psi_cap - psi_cryo
        blend_denom = sqrt(delta_psi*delta_psi + SUCTION_BLEND_EPS*SUCTION_BLEND_EPS)

        psi_eff = 0.5d0*(psi_cap + psi_cryo + blend_denom)

        if (present(dpsi_eff_dpsi_cap)) then
            dpsi_eff_dpsi_cap = 0.5d0*(1.0d0 + delta_psi/blend_denom)
        end if
        if (present(dpsi_eff_dpsi_cryo)) then
            dpsi_eff_dpsi_cryo = 0.5d0*(1.0d0 - delta_psi/blend_denom)
        end if
    end subroutine compute_effective_suction

    pure subroutine compute_freezing_activation(temperature, activation, dactivation_dT)
        implicit none
        real(real64), intent(in) :: temperature
        real(real64), intent(inout) :: activation
        real(real64), intent(inout), optional :: dactivation_dT

        real(real64) :: arg, sech2

        arg = (temperature - Tf0) / FREEZING_SMOOTH_BAND
        activation = 0.5d0*(1.0d0 - tanh(arg))

        if (present(dactivation_dT)) then
            sech2 = 1.0d0 - tanh(arg)*tanh(arg)
            dactivation_dT = -0.5d0*sech2/FREEZING_SMOOTH_BAND
        end if
    end subroutine compute_freezing_activation

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
    !> In-situ (local equilibrium) equivalent-saturation formulation with pore-space cap:
    !> \[ Q_{i,unb} = f_{freeze}(T) \cdot (\rho_w/\rho_i) \cdot \max(0, \theta_l(-\psi_{cap}) - \theta_l(-\psi_{eff})) \]
    !> \[ Q_{i,cap} = \max(0, \phi - \theta_l(-\psi_{eff}) - \epsilon) \]
    !> \[ Q_i = \tfrac{1}{2}\left(Q_{i,unb} + Q_{i,cap}
    !>         - \sqrt{(Q_{i,unb} - Q_{i,cap})^2 + s^2}\right) \]
    !> The first WRF evaluation gives the unfrozen-reference liquid content (capillary
    !> only); the second gives the current liquid content with cryogenic suction
    !> included. Mass conservation between the two is scaled by the ice expansion
    !> ratio \(\rho_w/\rho_i\); a C1-smooth minimum with the available pore space
    !> enforces geometric feasibility (Qi + theta_l <= phi) without discontinuous
    !> Jacobian drops. Excess mass that cannot fit as ice is physically carried out
    !> by Darcy flow in the hydraulic PDE.
    subroutine calc_ice_content(self, state, ice_content)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: ice_content

        real(real64) :: pressure, temperature
        real(real64) :: psi_cap, psi_cryo, psi_eff
        real(real64) :: theta_l_cap, theta_l_eff, delta_theta
        real(real64) :: freezing_activation
        real(real64) :: rho_water, rho_ice, density_ratio
        real(real64) :: porosity_val, Qi_seg, Qi_unbounded, Qi_cap
        real(real64) :: smooth_scale, diff

        call state%pressure%get(pressure)
        call state%temperature%get(temperature)

        psi_cap = max(0.0d0, -pressure)
        call self%gcc%calc(state, psi_cryo)
        call compute_effective_suction(psi_cap, psi_cryo, psi_eff)

        call self%wrf%calc(-psi_cap, theta_l_cap)
        call self%wrf%calc(-psi_eff, theta_l_eff)

        call self%calc_rho_water(state, rho_water)
        call self%calc_rho_ice(state, rho_ice)

        if (rho_ice > 1.0d-6) then
            density_ratio = rho_water / rho_ice
        else
            density_ratio = 1.0d0
        end if

        call compute_freezing_activation(temperature, freezing_activation)

        delta_theta = max(0.0d0, theta_l_cap - theta_l_eff)
        Qi_unbounded = freezing_activation * density_ratio * delta_theta

        call state%porosity%get(porosity_val)
        call state%ice_content_seg%get(Qi_seg)
        Qi_cap = max(0.0d0, porosity_val - theta_l_eff - max(Qi_seg, 0.0d0) - PORE_MARGIN)

        smooth_scale = PORE_CAP_SMOOTH_REL * max(porosity_val, 1.0d-6)
        diff = Qi_unbounded - Qi_cap
        ice_content = 0.5d0 * (Qi_unbounded + Qi_cap - sqrt(diff*diff + smooth_scale*smooth_scale))
        ice_content = max(0.0d0, ice_content)

    end subroutine calc_ice_content

    !>
    !> @brief Calculate derivatives of ice content w.r.t pressure and temperature.
    !>
    !> Chain rule through the pore-cap smooth minimum:
    !> \[ \partial Q_i/\partial x = w_{unb}\,\partial Q_{i,unb}/\partial x
    !>    + w_{cap}\,\partial Q_{i,cap}/\partial x \]
    !> with weights \(w_{unb} = \tfrac{1}{2}(1-(Q_{unb}-Q_{cap})/r)\),
    !> \(w_{cap} = \tfrac{1}{2}(1+(Q_{unb}-Q_{cap})/r)\),
    !> \(r = \sqrt{(Q_{unb}-Q_{cap})^2 + s^2}\).
    subroutine calc_ice_content_derivatives(self, state, dice_dP, dice_dT)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: dice_dP
        real(real64), intent(inout) :: dice_dT

        real(real64) :: pressure, temperature
        real(real64) :: psi_cap, psi_cryo, psi_eff
        real(real64) :: d_psi_cap_dP
        real(real64) :: d_psi_cryo_dP, d_psi_cryo_dT
        real(real64) :: d_psi_eff_dpsi_cap, d_psi_eff_dpsi_cryo
        real(real64) :: d_psi_eff_dP, d_psi_eff_dT
        real(real64) :: theta_l_cap, theta_l_eff, delta_theta
        real(real64) :: dtheta_dPin_cap, dtheta_dPin_eff
        real(real64) :: d_theta_cap_dP, d_theta_eff_dP, d_theta_eff_dT
        real(real64) :: freezing_activation, d_freezing_activation_dT
        real(real64) :: rho_w, rho_i, density_ratio
        real(real64) :: porosity_val, Qi_unbounded, Qi_cap
        real(real64) :: dQi_unb_dP, dQi_unb_dT, dQi_cap_dP, dQi_cap_dT
        real(real64) :: smooth_scale, diff, denom, w_unb, w_cap

        call state%pressure%get(pressure)
        call state%temperature%get(temperature)
        call self%calc_rho_water(state, rho_w)
        call self%calc_rho_ice(state, rho_i)

        if (rho_i > 1.0d-6) then
            density_ratio = rho_w / rho_i
        else
            density_ratio = 1.0d0
        end if

        if (pressure < 0.0d0) then
            psi_cap = -pressure
            d_psi_cap_dP = -1.0d0
        else
            psi_cap = 0.0d0
            d_psi_cap_dP = 0.0d0
        end if

        call self%gcc%calc(state, psi_cryo)
        call self%gcc%deriv_pressure(state, d_psi_cryo_dP)
        call self%gcc%deriv_temperature(state, d_psi_cryo_dT)

        call compute_effective_suction(psi_cap, psi_cryo, psi_eff, d_psi_eff_dpsi_cap, d_psi_eff_dpsi_cryo)
        d_psi_eff_dP = d_psi_eff_dpsi_cap * d_psi_cap_dP + d_psi_eff_dpsi_cryo * d_psi_cryo_dP
        d_psi_eff_dT = d_psi_eff_dpsi_cryo * d_psi_cryo_dT

        call self%wrf%calc(-psi_cap, theta_l_cap)
        call self%wrf%calc(-psi_eff, theta_l_eff)
        call self%wrf%deriv(-psi_cap, dtheta_dPin_cap)
        call self%wrf%deriv(-psi_eff, dtheta_dPin_eff)

        call compute_freezing_activation(temperature, freezing_activation, d_freezing_activation_dT)

        ! Liquid-content derivatives (chain rule: WRF input is -psi).
        d_theta_cap_dP = dtheta_dPin_cap * (-d_psi_cap_dP)
        d_theta_eff_dP = dtheta_dPin_eff * (-d_psi_eff_dP)
        d_theta_eff_dT = dtheta_dPin_eff * (-d_psi_eff_dT)

        ! Unbounded ice content and its derivatives.
        delta_theta = theta_l_cap - theta_l_eff
        if (delta_theta > 0.0d0) then
            Qi_unbounded = freezing_activation * density_ratio * delta_theta
            dQi_unb_dP = freezing_activation * density_ratio * (d_theta_cap_dP - d_theta_eff_dP)
            dQi_unb_dT = freezing_activation * density_ratio * (-d_theta_eff_dT) + &
                         d_freezing_activation_dT * density_ratio * delta_theta
        else
            Qi_unbounded = 0.0d0
            dQi_unb_dP = 0.0d0
            dQi_unb_dT = 0.0d0
        end if

        ! Pore-space cap: Qi_cap = max(0, porosity - theta_l_eff - Qi_seg - PORE_MARGIN).
        ! Qi_seg is treated as an explicit (frozen-in-Newton) state here, so it
        ! contributes no Jacobian terms.
        call state%porosity%get(porosity_val)
        block
            real(real64) :: qi_seg_val
            call state%ice_content_seg%get(qi_seg_val)
            qi_seg_val = max(qi_seg_val, 0.0d0)
            if (porosity_val - theta_l_eff - qi_seg_val - PORE_MARGIN > 0.0d0) then
                Qi_cap = porosity_val - theta_l_eff - qi_seg_val - PORE_MARGIN
                dQi_cap_dP = -d_theta_eff_dP
                dQi_cap_dT = -d_theta_eff_dT
            else
                Qi_cap = 0.0d0
                dQi_cap_dP = 0.0d0
                dQi_cap_dT = 0.0d0
            end if
        end block

        ! C1-smooth minimum of Qi_unbounded and Qi_cap.
        smooth_scale = PORE_CAP_SMOOTH_REL * max(porosity_val, 1.0d-6)
        diff = Qi_unbounded - Qi_cap
        denom = sqrt(diff*diff + smooth_scale*smooth_scale)
        w_unb = 0.5d0 * (1.0d0 - diff / denom)
        w_cap = 0.5d0 * (1.0d0 + diff / denom)

        dice_dP = w_unb * dQi_unb_dP + w_cap * dQi_cap_dP
        dice_dT = w_unb * dQi_unb_dT + w_cap * dQi_cap_dT

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

        ! Pass negative pressure to WRF
        call self%wrf%calc(-psi_eff, water_content)

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
        real(real64) :: psi_eff
        real(real64) :: d_psi_eff_dP, d_psi_eff_dT
        real(real64) :: d_psi_eff_dpsi_cap, d_psi_eff_dpsi_cryo
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

        ! 3. Compute moisture capacity (dTheta/dPress)
        ! Pass negative pressure to WRF
        call self%wrf%deriv(-psi_eff, d_theta_liquid_dPress)

        ! 4. Assemble liquid water content derivatives (chain rule)
        ! Input is -psi_eff, so chain rule multiplies by -d(psi)/dX

        ! d(Theta_l)/dP = (dTheta/dP_in) * d(-Psi_eff)/dP
        dwater_dP = d_theta_liquid_dPress * (-d_psi_eff_dP)

        ! d(Theta_l)/dT = (dTheta/dP_in) * d(-Psi_eff)/dT
        dwater_dT = d_theta_liquid_dPress * (-d_psi_eff_dT)

    end subroutine calc_water_content_derivatives

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

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
    !

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
        real(real64) :: theta_tot

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
            call self%wrf%calc(-psi_cap, theta_l_cap)

            theta_tot = theta_l_cap

            call self%gcc%calc(state, psi_cryo)
            call compute_effective_suction(psi_cap, psi_cryo, psi_eff)
            call self%wrf%calc(-psi_eff, theta_l_new)

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
        real(real64) :: theta_tot

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

            call self%wrf%calc(-psi_cap, theta_l_cap)
            call self%wrf%deriv(-psi_cap, dtheta_dPin_cap)
            d_theta_cap_dP = dtheta_dPin_cap * (-d_psi_cap_dP)

            theta_tot = theta_l_cap

            call self%gcc%calc(state, psi_cryo)
            call self%gcc%deriv_pressure(state, d_psi_cryo_dP)
            call self%gcc%deriv_temperature(state, d_psi_cryo_dT)

            call compute_effective_suction(psi_cap, psi_cryo, psi_eff, d_psi_eff_dpsi_cap, d_psi_eff_dpsi_cryo)
            d_psi_eff_dP = d_psi_eff_dpsi_cap * d_psi_cap_dP + d_psi_eff_dpsi_cryo * d_psi_cryo_dP
            d_psi_eff_dT = d_psi_eff_dpsi_cryo * d_psi_cryo_dT

            call self%wrf%calc(-psi_eff, theta_l_new)
            call self%wrf%deriv(-psi_eff, dtheta_dPin_eff)

            d_theta_eff_dP = dtheta_dPin_eff * (-d_psi_eff_dP)
            d_theta_eff_dT = dtheta_dPin_eff * (-d_psi_eff_dT)

            if (theta_l_new < theta_tot) then
                dice_dP = (d_theta_cap_dP - d_theta_eff_dP) * (rho_w / rho_i)
                dice_dT = -d_theta_eff_dT * (rho_w / rho_i)
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

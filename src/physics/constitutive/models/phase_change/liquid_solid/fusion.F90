module models_phase_change_fusion
    use, intrinsic :: iso_fortran_env
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core, only:type_state
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
        procedure, pass(self), public :: deriv_pressure_ice_water

    end type type_fusion

contains

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
    subroutine calc_ice_content(self, state, ice_content)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: ice_content

        real(real64) :: pressure
        real(real64) :: psi_cap, psi_cryo, psi_eff
        real(real64) :: theta_target_unfrozen, theta_liquid
        real(real64) :: rho_water, rho_ice, density_ratio

        ! Compute suction as positive magnitude for comparison
        call state%pressure%get(pressure)

        psi_cap = max(0.0d0, -pressure)
        call self%gcc%calc(state, psi_cryo)
        psi_eff = max(psi_cap, psi_cryo)

        ! WRF input requires negative pressure head, so negate psi
        call self%wrf%calc(-psi_cap, theta_target_unfrozen)
        call self%wrf%calc(-psi_eff, theta_liquid)

        call self%calc_rho_water(state, rho_water)
        call self%calc_rho_ice(state, rho_ice)

        if (rho_ice > 1.0d-6) then
            density_ratio = rho_water / rho_ice
        else
            density_ratio = 1.0d0
        end if

        ice_content = max(0.0d0, (theta_target_unfrozen - theta_liquid) * density_ratio)

    end subroutine calc_ice_content

    !>
    !> @brief Calculate derivatives of ice content w.r.t pressure and temperature.
    !>
    subroutine calc_ice_content_derivatives(self, state, dice_dP, dice_dT)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: dice_dP
        real(real64), intent(inout) :: dice_dT

        real(real64) :: pressure
        real(real64) :: psi_cap, psi_cryo
        real(real64) :: d_psi_cap_dP
        real(real64) :: d_psi_cryo_dP, d_psi_cryo_dT
        real(real64) :: d_psi_eff_dP, d_psi_eff_dT
        real(real64) :: d_theta_target_dPress, d_theta_liquid_dPress ! renamed from dPsi to dPress
        real(real64) :: rho_w, rho_i, density_ratio
        real(real64) :: theta_target, theta_liquid

        call state%pressure%get(pressure)
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
        call self%gcc%deriv_temperature(state, d_psi_cryo_dT)
        call self%gcc%deriv_pressure(state, d_psi_cryo_dP)

        if (psi_cap >= psi_cryo) then
            d_psi_eff_dP = d_psi_cap_dP
            d_psi_eff_dT = 0.0d0
        else
            d_psi_eff_dP = d_psi_cryo_dP
            d_psi_eff_dT = d_psi_cryo_dT
        end if

        ! WRF evaluation
        ! Derivative computation: input is negative pressure (-psi).
        ! Return value d_theta_..._dPress is d(theta)/d(Pressure) (typically positive)
        call self%wrf%calc(-psi_cap, theta_target)
        call self%wrf%calc(-max(psi_cap, psi_cryo), theta_liquid)
        call self%wrf%deriv(-psi_cap, d_theta_target_dPress)
        call self%wrf%deriv(-max(psi_cap, psi_cryo), d_theta_liquid_dPress)

        ! Chain rule application
        ! Input variable P_in = -psi
        ! d(P_in)/dX = - d(psi)/dX
        ! d(theta)/dX = d(theta)/d(P_in) * d(P_in)/dX
        !             = d_theta_..._dPress * (- d_psi_..._dX)

        if (theta_target > theta_liquid) then
            ! d(Ice)/dP = ratio * [ d(Theta_target)/dP - d(Theta_liquid)/dP ]
            !           = ratio * [ (dThetaT/dP_in * -dPsiCap/dP) - (dThetaL/dP_in * -dPsiEff/dP) ]
            !           = ratio * [ - dThetaT * dPsiCap + dThetaL * dPsiEff ]

            dice_dP = density_ratio * ( &
                      d_theta_target_dPress * (-d_psi_cap_dP) - &
                      d_theta_liquid_dPress * (-d_psi_eff_dP) &
                      )

            dice_dT = density_ratio * ( &
                      -d_theta_liquid_dPress * (-d_psi_eff_dT) &
                      )
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
        psi_eff = max(psi_cap, psi_cryo)

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

        ! Select effective suction and determine derivatives
        if (psi_cap >= psi_cryo) then
            d_psi_eff_dP = d_psi_cap_dP
            d_psi_eff_dT = 0.0d0
        else
            call self%gcc%deriv_pressure(state, d_psi_cryo_dP)
            call self%gcc%deriv_temperature(state, d_psi_cryo_dT)
            d_psi_eff_dP = d_psi_cryo_dP
            d_psi_eff_dT = d_psi_cryo_dT
        end if

        ! 3. Compute moisture capacity (dTheta/dPress)
        ! Pass negative pressure to WRF
        call self%wrf%deriv(-max(psi_cap, psi_cryo), d_theta_liquid_dPress)

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

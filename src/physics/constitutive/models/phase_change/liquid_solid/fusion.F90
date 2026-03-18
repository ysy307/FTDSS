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
        class(abst_wrf), private, pointer :: wrf
        class(abst_gcc), private, pointer :: gcc
    contains
        procedure, pass(self), public :: initialize => initialize_type_fusion
        procedure, pass(self), public :: calc_derivatives
    end type type_fusion

contains

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

    subroutine calc_derivatives(self, state)
        implicit none
        class(type_fusion), intent(in) :: self
        type(type_state), intent(inout) :: state

        real(real64) :: pressure, temperature, porosity
        real(real64) :: theta_w, theta_i
        real(real64) :: psi_cryo, dpsi_cryo_dp, dpsi_cryo_dt
        real(real64) :: dQw_dpsi
        real(real64) :: dQw_dP, dQw_dT, dQi_dP, dQi_dT
        real(real64) :: rho_w, rho_i

        call state%get(pressure=pressure, &
                       temperature=temperature, &
                       porosity=porosity, &
                       ice_content=theta_i)

        call self%calc_rho_water(state, rho_w)
        call self%calc_rho_ice(state, rho_i)

        call self%gcc%calc(state, psi_cryo)
        call self%gcc%deriv_pressure(state, dpsi_cryo_dp)
        call self%gcc%deriv_temperature(state, dpsi_cryo_dt)

        call self%wrf%calc(psi_cryo, theta_w)
        call self%wrf%deriv(psi_cryo, dQw_dpsi)

        call state%water_content%set(theta_w)

        dQw_dP = dQw_dpsi * dpsi_cryo_dp
        dQw_dT = dQw_dpsi * dpsi_cryo_dt

        if (theta_w + theta_i >= porosity) then
            dQi_dP = -dQw_dP
            dQi_dT = -dQw_dT
        else
            dQi_dP = -(rho_w / rho_i) * dQw_dP
            dQi_dT = -(rho_w / rho_i) * dQw_dT
        end if

        call state%dQw_dP%set(dQw_dP)
        call state%dQw_dT%set(dQw_dT)
        call state%dQi_dP%set(dQi_dP)
        call state%dQi_dT%set(dQi_dT)

    end subroutine calc_derivatives

end module models_phase_change_fusion

!> This implementation based on:
!> Saito, H., Šimůnek, J. and Mohanty, B.P. (2006)
!> Numerical analysis of coupled water, vapor, and heat transport in the vadose zone, Vadose zone journal: VZJ, 5(2), pp. 784–800.
submodule(physics_models_hcf) hcf_vapor
    implicit none
contains

    !> Da is the diffusivity of water vapor in air (m2 s2 1) at tem- perature T (K) in Eq. (16)
    !> \(D_a = 2.12 \times 10^{-5} \left(\frac{T}{273.15}\right)^2 \)
    module pure elemental subroutine calc_diffusivity_vapor_in_air(self, temperature, Da)
        implicit none
        !> HCF object
        class(type_hcf_vapor), intent(in) :: self
        !> Temperature [degrees C]
        real(real64), intent(in) :: temperature
        !> Diffusivity of water vapor in air [m^2/s]
        real(real64), intent(inout) :: Da

        real(real64) :: relative_temp

        relative_temp = (temperature + TtoK) / 273.15d0

        Da = 2.12d-5 * relative_temp * relative_temp

    end subroutine calc_diffusivity_vapor_in_air

    !> Reference: Eq. (15)
    module pure elemental subroutine calc_tortuosity_factor_vapor(self, Qa, Qs, tau)
        implicit none
        class(type_hcf_vapor), intent(in) :: self
        !> Air-filled porosity [-]
        real(real64), intent(in) :: Qa
        !> Saturated water content [-]
        real(real64), intent(in) :: Qs
        !> Tortuosity factor [-]
        real(real64), intent(inout) :: tau

        tau = Qa**(10.0d0 / 3.0d0) / Qs**2.0d0

    end subroutine calc_tortuosity_factor_vapor

    !> Reference: Eq. (19)
    module pure elemental subroutine calc_enhancement_factor_vapor(self, Qw, Qs, fc, eta)
        implicit none
        !> HCF object
        class(type_hcf_vapor), intent(in) :: self
        !> Water content [-]
        real(real64), intent(in) :: Qw
        !> Saturated water content [-]
        real(real64), intent(in) :: Qs
        !> Mass fraction of clay [-]
        real(real64), intent(in) :: fc
        !> Enhancement factor [-]
        real(real64), intent(inout) :: eta

        real(real64) :: Qw_ratio

        Qw_ratio = Qw / Qs

        eta = 9.5d0 + 3.0d0 * Qw_ratio - 8.5d0 * exp(-((1.0d0 + 2.6d0 / sqrt(fc)) * Qw_ratio)**4)

    end subroutine calc_enhancement_factor_vapor

    module pure elemental subroutine calc_Kvh_vapor(self, state, Kvh)
        implicit none
        class(type_hcf_vapor), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: Kvh

        real(real64) :: Dv, Da, tau
        real(real64) :: rho_water, rho_vapor_sat

        call self%calc_diffusivity(state%temperature, Da)
        call self%calc_tortuosity_factor(state%air_content, state%porosity, tau)
        call self%parent%water%calc_rho(state%temperature + TtoK, state%pressure, rho_water)
        call self%parent%water%calc_saturation_density(state%temperature + TtoK, rho_vapor_sat)

        Dv = Da * tau
        Kvh = Dv * rho_vapor_sat * Mw * g * state%relative_humidity / (Rg * (TtoK + state%temperature) * rho_water)

    end subroutine calc_Kvh_vapor

    module pure elemental subroutine calc_KvT_vapor(self, state, KvT)
        implicit none
        class(type_hcf_vapor), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: KvT

        real(real64) :: Dv, Da, tau, eta
        real(real64) :: rho_water, drho_vapor_sat_dT

        call self%calc_diffusivity(state%temperature, Da)
        call self%calc_tortuosity_factor(state%air_content, state%porosity, tau)
        call self%calc_enhancement_factor(state%water_content, state%porosity, &
                                          state%mass_fraction_clay, eta)
        call self%parent%water%calc_rho(state%temperature + TtoK, state%pressure, rho_water)
        call self%parent%water%calc_saturation_drho_dT(state%temperature + TtoK, drho_vapor_sat_dT)

        Dv = Da * tau * eta
        KvT = Dv * state%relative_humidity * drho_vapor_sat_dT / rho_water

    end subroutine calc_KvT_vapor

end submodule hcf_vapor

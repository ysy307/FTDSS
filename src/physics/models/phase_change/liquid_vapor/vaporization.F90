!>
!> @brief Physics models for vaporization.
!>
module physics_models_phase_change_liquid_vapor_vaporization
    use, intrinsic :: iso_fortran_env
    use :: iapws, only:type_iapws97
    use :: module_core, only:type_state
    use :: physics_constants, only: &
        g => gravity_acceleration, &
        T_to_K => celsius_to_kelvin, &
        Rg => universal_gas_constant, &
        Mw => molar_mass_water, &
        rho_std => reference_water_density, &
        P_atm => standard_atmospheric_pressure
    use :: physics_types, only:abst_physics

    implicit none
    private

    public :: type_evaporation

    type, extends(abst_physics) :: type_evaporation
    contains
        procedure, pass(self), public :: initialize => initialize_evaporation_model
        procedure, pass(self), public :: calc_latent_heat_vaporization
        procedure, pass(self), public :: calc_relative_humidity => calc_relative_humidity_evaporation
        procedure, pass(self), public :: deriv_relative_humidity_temperature => deriv_relative_humidity_temperature_evaporation
        procedure, pass(self), public :: deriv_relative_humidity_pressure => deriv_relative_humidity_pressure_evaporation
        procedure, pass(self), public :: calc_vapor_content => calc_vapor_content_vaporization
        procedure, pass(self), public :: deriv_vapor_content_temperature => deriv_vapor_content_temperature_vaporization
        procedure, pass(self), public :: deriv_vapor_content_pressure => deriv_vapor_content_pressure_vaporization
    end type type_evaporation

contains

    subroutine initialize_evaporation_model(self, water)
        implicit none
        class(type_evaporation), intent(inout) :: self
        type(type_iapws97), intent(in), target, optional :: water

        if (present(water)) self%water => water
    end subroutine initialize_evaporation_model

    pure subroutine calc_latent_heat_vaporization(self, temperature, latent_heat)
        implicit none
        class(type_evaporation), intent(in) :: self
        real(real64), intent(in) :: temperature
        real(real64), intent(inout) :: latent_heat

        real(real64) :: temperature_K

        if (associated(self%water)) then
            call self%shift_temperature_absolute(temperature, temperature_K)
            call self%water%calc_latent_heat(latent_heat, temperature_K)
        else
            latent_heat = 2.501d6 - 2369.2d0 * temperature
        end if
    end subroutine calc_latent_heat_vaporization

    pure elemental subroutine calc_relative_humidity_evaporation(self, state, relative_humidity)
        implicit none
        class(type_evaporation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: relative_humidity

        real(real64) :: temperature_K

        if (state%pressure >= 0.0d0) then
            relative_humidity = 1.0d0
            return
        end if

        call self%shift_temperature_absolute(state%temperature, temperature_K)
        relative_humidity = exp((state%pressure * Mw) / (rho_std * Rg * temperature_K))

    end subroutine calc_relative_humidity_evaporation

    pure elemental subroutine deriv_relative_humidity_temperature_evaporation(self, state, deriv_rh_temp)
        implicit none
        class(type_evaporation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv_rh_temp
        real(real64) :: rh, temperature_K

        if (state%pressure >= 0.0d0) then
            deriv_rh_temp = 0.0d0
            return
        end if

        call self%shift_temperature_absolute(state%temperature, temperature_K)
        call self%calc_relative_humidity(state, rh)

        deriv_rh_temp = rh * (-state%pressure * Mw) / (rho_std * Rg * temperature_K**2)
    end subroutine deriv_relative_humidity_temperature_evaporation

    pure elemental subroutine deriv_relative_humidity_pressure_evaporation(self, state, deriv_rh_pressure)
        implicit none
        class(type_evaporation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv_rh_pressure
        real(real64) :: rh, temperature_K

        if (state%pressure >= 0.0d0) then
            deriv_rh_pressure = 0.0d0
            return
        end if

        call self%shift_temperature_absolute(state%temperature, temperature_K)
        call self%calc_relative_humidity(state, rh)
        deriv_rh_pressure = rh * Mw / (rho_std * Rg * temperature_K)

    end subroutine deriv_relative_humidity_pressure_evaporation

    !> @brief Calculate vapor content.
    pure elemental subroutine calc_vapor_content_vaporization(self, state, vapor_content)
        implicit none
        class(type_evaporation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: vapor_content

        real(real64) :: relative_humidity
        real(real64) :: saturation_density, water_density

        call self%calc_relative_humidity(state, relative_humidity)
        call self%calc_rho_water(state, water_density)
        call self%calc_rho_vapor_saturation(state, saturation_density)

        vapor_content = saturation_density * relative_humidity * state%air_content / water_density
    end subroutine calc_vapor_content_vaporization

    !> @brief Derivative of vapor content with respect to temperature.
    pure elemental subroutine deriv_vapor_content_temperature_vaporization(self, state, deriv_vapor_temp)
        implicit none
        class(type_evaporation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv_vapor_temp

        real(real64) :: relative_humidity, deriv_rh_temp
        real(real64) :: saturation_density, water_density
        real(real64) :: temperature_K
        real(real64) :: drho_sat_dT, drho_w_dT
        real(real64) :: pressure_absolute

        call self%shift_temperature_absolute(state%temperature, temperature_K)
        call self%shift_pressure_absolute(state%pressure, pressure_absolute)

        call self%calc_relative_humidity(state, relative_humidity)
        call self%deriv_relative_humidity_temperature(state, deriv_rh_temp)

        call self%water%calc_rho(temperature_K, pressure_absolute, water_density)
        call self%water%calc_drho_dT(temperature_K, pressure_absolute, drho_w_dT)

        call self%water%calc_saturation_density(temperature_K, saturation_density)
        call self%water%calc_saturation_drho_dT(temperature_K, drho_sat_dT)

        deriv_vapor_temp = state%air_content * ( &
                           deriv_rh_temp * saturation_density / water_density + &
                           relative_humidity * drho_sat_dT / water_density - &
                           relative_humidity * saturation_density * drho_w_dT / (water_density**2))
    end subroutine deriv_vapor_content_temperature_vaporization

    !> @brief Derivative of vapor content with respect to pressure.
    pure elemental subroutine deriv_vapor_content_pressure_vaporization(self, state, deriv_vapor_pressure)
        implicit none
        class(type_evaporation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv_vapor_pressure

        real(real64) :: relative_humidity, deriv_rh_pressure
        real(real64) :: saturation_density, water_density
        real(real64) :: temperature_K
        real(real64) :: drho_w_dP
        real(real64) :: pressure_absolute

        call self%shift_temperature_absolute(state%temperature, temperature_K)
        call self%shift_pressure_absolute(state%pressure, pressure_absolute)

        call self%calc_relative_humidity(state, relative_humidity)
        call self%deriv_relative_humidity_pressure(state, deriv_rh_pressure)

        if (state%pressure < 0.0d0) then
            call self%water%calc_rho(temperature_K, pressure_absolute, water_density)
            drho_w_dP = 0.0d0
        else
            call self%water%calc_rho(temperature_K, pressure_absolute, water_density)
            call self%water%calc_drho_dP(temperature_K, pressure_absolute, drho_w_dP)
        end if

        call self%water%calc_saturation_density(temperature_K, saturation_density)

        deriv_vapor_pressure = state%air_content * ( &
                               deriv_rh_pressure * saturation_density / water_density - &
                               relative_humidity * saturation_density * drho_w_dP / (water_density**2))
    end subroutine deriv_vapor_content_pressure_vaporization

end module physics_models_phase_change_liquid_vapor_vaporization

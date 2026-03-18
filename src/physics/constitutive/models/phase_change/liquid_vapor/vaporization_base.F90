!> Implementation of evaporation models.
!>
!> Algorithm overview:
!> - Calculates relative humidity and associated derivatives utilizing algebraic evaluation of the Kelvin equation.
!> - Assembles the equivalent vapor content and its analytical derivatives via the chain rule.
submodule(models_phase_change_vaporization) vaporization_base
    implicit none

contains

    module subroutine initialize_evaporation_model(self, water)
        implicit none
        class(type_evaporation), intent(inout) :: self
        type(type_iapws97), intent(in), target :: water

        self%water => water
        self%initialized = .true.
    end subroutine initialize_evaporation_model

    !> Computes latent heat dynamically from IAPWS-97 or falls back to an empirical linear relation.
    module subroutine calc_latent_heat_vaporization_evaporation(self, temperature, latent_heat)
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
    end subroutine calc_latent_heat_vaporization_evaporation

    !> Evaluates the Kelvin equation, applying a threshold to guarantee RH=1 when pressure is non-negative.
    module subroutine calc_relative_humidity_evaporation(self, state, relative_humidity)
        implicit none
        class(type_evaporation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: relative_humidity

        real(real64) :: temperature, temperature_K
        real(real64) :: pressure

        call state%temperature%get(temperature)
        call state%pressure%get(pressure)

        if (pressure >= 0.0d0) then
            relative_humidity = 1.0d0
            return
        end if

        call self%shift_temperature_absolute(temperature, temperature_K)
        relative_humidity = exp((pressure * Mw) / (rho_std * Rg * temperature_K))
    end subroutine calc_relative_humidity_evaporation

    !> Evaluates the analytical derivative of the Kelvin equation w.r.t temperature.
    module subroutine deriv_relative_humidity_temperature_evaporation(self, state, deriv_rh_temp)
        implicit none
        class(type_evaporation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv_rh_temp

        real(real64) :: rh, temperature, temperature_K
        real(real64) :: pressure

        call state%temperature%get(temperature)
        call state%pressure%get(pressure)

        if (pressure >= 0.0d0) then
            deriv_rh_temp = 0.0d0
            return
        end if

        call self%shift_temperature_absolute(temperature, temperature_K)
        call self%calc_relative_humidity(state, rh)

        deriv_rh_temp = rh * (-pressure * Mw) / (rho_std * Rg * temperature_K**2)
    end subroutine deriv_relative_humidity_temperature_evaporation

    !> Evaluates the analytical derivative of the Kelvin equation w.r.t pressure.
    module subroutine deriv_relative_humidity_pressure_evaporation(self, state, deriv_rh_pressure)
        implicit none
        class(type_evaporation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: deriv_rh_pressure

        real(real64) :: rh, temperature, temperature_K
        real(real64) :: pressure

        call state%temperature%get(temperature)
        call state%pressure%get(pressure)

        if (pressure >= 0.0d0) then
            deriv_rh_pressure = 0.0d0
            return
        end if

        call self%shift_temperature_absolute(temperature, temperature_K)
        call self%calc_relative_humidity(state, rh)

        deriv_rh_pressure = rh * Mw / (rho_std * Rg * temperature_K)
    end subroutine deriv_relative_humidity_pressure_evaporation

    !> Assembles equivalent liquid volume of vapor from relative humidity and phase properties.
    module subroutine calc_vapor_content_vaporization(self, state, vapor_content)
        implicit none
        class(type_evaporation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: vapor_content

        real(real64) :: relative_humidity
        real(real64) :: saturation_density, water_density
        real(real64) :: air_content

        call self%calc_relative_humidity(state, relative_humidity)
        call self%calc_rho_vapor_saturation(state, saturation_density)

        call self%calc_rho_water(state, water_density)
        call state%air_content%get(air_content)

        vapor_content = saturation_density * relative_humidity * air_content / water_density
    end subroutine calc_vapor_content_vaporization

    !> Computes derivatives of vapor content iteratively using the chain rule over all contributing terms.
    module subroutine calc_vapor_content_derivatives_vaporization(self, state, dvapor_dP, dvapor_dT)
        implicit none
        class(type_evaporation), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: dvapor_dP, dvapor_dT

        ! Local variables for state
        real(real64) :: temperature, pressure, air_content
        real(real64) :: d_air_content_dP, d_air_content_dT
        real(real64) :: temperature_K

        ! Local variables for intermediate derivatives
        real(real64) :: rh, drh_dP, drh_dT
        real(real64) :: rho_sat, drho_sat_dT
        real(real64) :: rho_w, drho_w_dP, drho_w_dT
        real(real64) :: ratio, dratio_dP, dratio_dT

        call state%temperature%get(temperature)
        call state%pressure%get(pressure)
        call state%air_content%get(air_content)
        call state%dQa_dP%get(d_air_content_dP)
        call state%dQa_dT%get(d_air_content_dT)
        call self%shift_temperature_absolute(temperature, temperature_K)

        call self%calc_relative_humidity(state, rh)
        call self%deriv_relative_humidity_pressure(state, drh_dP)
        call self%deriv_relative_humidity_temperature(state, drh_dT)

        call self%calc_rho_vapor_saturation(state, rho_sat)
        call self%calc_drho_vapor_saturation_dT(state, drho_sat_dT)

        call self%calc_rho_water(state, rho_w)
        call self%calc_drho_water_dT(state, drho_w_dT)
        call self%calc_drho_water_dP(state, drho_w_dP)

        ! Construct the ratio component (rho_sat * RH) / rho_w
        ratio = (rho_sat * rh) / rho_w

        dratio_dP = (rho_sat / rho_w) * drh_dP - &
                    (rho_sat * rh / (rho_w**2)) * drho_w_dP

        dratio_dT = (1.0d0 / rho_w) * (drho_sat_dT * rh + rho_sat * drh_dT) - &
                    (ratio / rho_w) * drho_w_dT

        dvapor_dP = d_air_content_dP * ratio + air_content * dratio_dP
        dvapor_dT = d_air_content_dT * ratio + air_content * dratio_dT
    end subroutine calc_vapor_content_derivatives_vaporization

end submodule vaporization_base

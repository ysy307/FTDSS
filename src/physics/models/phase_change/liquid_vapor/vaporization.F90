module physics_models_vaporization
    use, intrinsic :: iso_fortran_env
    use :: module_core, only: &
        g => gravity_acceleration, &
        TtoK => celsius_to_kelvin, &
        R => universal_gas_constant, &
        Mw => molar_mass_water
    implicit none

    type :: type_evaporation_model
    end type type_evaporation_model

contains

    !>
    !> Laten heat of vaporization model
    !>
    pure function latent_heat_vaporization(temperature) result(latent_heat)
        implicit none
        !> Temperature at which vaporization occurs (K)
        real(real64), intent(in) :: temperature
        !> Latent heat of vaporization of liquid water (J/kg)
        real(real64) :: latent_heat

        latent_heat = 2.501d6 - 2369.2 * temperature

    end function latent_heat_vaporization

    !>
    !> Relative humidity calculation based on temperature and pressure
    !>
    pure function relative_humidity(temperature, pressure) result(rh)
        implicit none
        !> Temperature (degree Celsius)
        real(real64), intent(in) :: temperature
        !> Pressure - matirx potential (m)
        real(real64), intent(in) :: pressure
        !> Relative humidity (0 to 1)
        real(real64) :: rh

        rh = exp(pressure * Mw * g / (R * (TtoK + temperature)))
    end function relative_humidity

end module physics_models_vaporization

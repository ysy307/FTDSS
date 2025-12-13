module physics_models_vaporization
    use, intrinsic :: iso_fortran_env
    use :: iapws, only:type_iapws97
    use :: physics_constants, only: &
        g => gravity_acceleration, &
        TtoK => celsius_to_kelvin, &
        Rg => universal_gas_constant, &
        Mw => molar_mass_water
    implicit none
    private

    type :: type_evaporation_model
        private
        type(type_iapws97), pointer :: water => null()
    contains
        procedure, pass(self), public :: initialize => initialize_evaporation_model
        procedure, pass(self), public :: calc_latent_heat_vaporization
        procedure, pass(self), public :: calc_relative_humidity
    end type type_evaporation_model

contains
    !>
    !> initialize evaporation model
    subroutine initialize_evaporation_model(self, water)
        implicit none
        class(type_evaporation_model), intent(inout) :: self
        type(type_iapws97), intent(in), target, optional :: water

        if (present(water)) then
            self%water => water
        end if
    end subroutine initialize_evaporation_model

    !>
    !> Laten heat of watre vaporization calculation
    !>
    pure subroutine calc_latent_heat_vaporization(self, temperature, latent_heat)
        implicit none
        class(type_evaporation_model), intent(in) :: self
        !> Temperature at which vaporization occurs (K)
        real(real64), intent(in) :: temperature
        !> Latent heat of vaporization of liquid water (J/kg)
        real(real64), intent(inout) :: latent_heat

        if (associated(self%water)) then
            call self%water%calc_latent_heat(latent_heat, temperature)
        else
            latent_heat = 2.501d6 - 2369.2 * temperature
        end if

    end subroutine calc_latent_heat_vaporization

    !>
    !> Relative humidity calculation based on temperature and pressure
    !>
    pure elemental subroutine calc_relative_humidity(self, temperature, pressure, relative_humidity)
        implicit none
        class(type_evaporation_model), intent(in) :: self
        !> Temperature [C]
        real(real64), intent(in) :: temperature
        !> Pressure - matirx potential [m]
        real(real64), intent(in) :: pressure
        !> Relative humidity (0 to 1)
        real(real64), intent(inout) :: relative_humidity

        relative_humidity = exp(pressure * Mw * g / (Rg * (TtoK + temperature)))
    end subroutine calc_relative_humidity

end module physics_models_vaporization

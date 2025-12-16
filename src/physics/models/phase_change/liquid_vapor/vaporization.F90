!>
!> @brief Physics models for vaporization.
!>
!> This module handles the calculation of latent heat of vaporization and
!> relative humidity based on thermodynamic state.
!>
module phase_change_liquid_vapor_vaporization
    use, intrinsic :: iso_fortran_env
    use :: iapws, only:type_iapws97
    use :: physics_constants, only: &
        g => gravity_acceleration, &
        T_to_K => celsius_to_kelvin, &
        Rg => universal_gas_constant, &
        Mw => molar_mass_water, &
        rho_std => reference_water_density

    implicit none
    private

    public :: type_evaporation_model

    !>
    !> @brief Model for evaporation physics.
    !>
    type :: type_evaporation_model
        private
        !> Pointer to water property object (IAPWS97)
        type(type_iapws97), pointer :: water => null()
    contains
        procedure, pass(self), public :: initialize => initialize_evaporation_model
        procedure, pass(self), public :: calc_latent_heat_vaporization
        procedure, pass(self), public :: calc_relative_humidity
    end type type_evaporation_model

contains

    !>
    !> @brief Initialize evaporation model.
    !>
    !> Associates an optional water property object if provided.
    !>
    subroutine initialize_evaporation_model(self, water)
        implicit none
        !> Evaporation model object
        class(type_evaporation_model), intent(inout) :: self
        !> (Optional) Water property object
        type(type_iapws97), intent(in), target, optional :: water

        if (present(water)) then
            self%water => water
        end if
    end subroutine initialize_evaporation_model

    !>
    !> @brief Calculate latent heat of water vaporization.
    !>
    !> Calculates the energy required to vaporize water. Uses IAPWS97 if available,
    !> otherwise uses a linear approximation.
    !>
    pure subroutine calc_latent_heat_vaporization(self, temperature, latent_heat)
        implicit none
        !> Evaporation model object
        class(type_evaporation_model), intent(in) :: self
        !> Temperature at which vaporization occurs [C]
        real(real64), intent(in) :: temperature
        !> Latent heat of vaporization of liquid water [J/kg]
        real(real64), intent(inout) :: latent_heat

        if (associated(self%water)) then
            call self%water%calc_latent_heat(latent_heat, temperature + T_to_K)
        else
            ! Linear approximation
            latent_heat = 2.501d6 - 2369.2d0 * temperature
        end if
    end subroutine calc_latent_heat_vaporization

    !>
    !> @brief Calculate relative humidity based on temperature and pressure (matric potential).
    !>
    !> Computes relative humidity using the Kelvin equation.
    !> Accepts pressure in [Pa] (negative for suction/unsaturated).
    !>
    !> Formula: \(RH = exp( (P * Mw) / (rho_w * R * T))\)
    !>
    pure elemental subroutine calc_relative_humidity(self, temperature, pressure, relative_humidity)
        implicit none
        !> Evaporation model object
        class(type_evaporation_model), intent(in) :: self
        !> Temperature [C]
        real(real64), intent(in) :: temperature
        !> Pressure / Matric potential [Pa] (Note: Changed from Head [m] to Pressure [Pa])
        real(real64), intent(in) :: pressure
        !> Relative humidity (0 to 1)
        real(real64), intent(inout) :: relative_humidity

        ! Kelvin equation adapted for Pressure input [Pa]:
        ! Potential energy per mole = V_m * P = (Mw / rho) * P
        ! Argument for exp is (Mw * P) / (rho * R * T)
        ! Note: P is typically negative for unsaturated soil (suction).
        relative_humidity = exp((pressure * Mw) / (rho_std * Rg * (T_to_K + temperature)))
    end subroutine calc_relative_humidity

end module phase_change_liquid_vapor_vaporization

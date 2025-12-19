!> Defines abstract physics types and interfaces for property calculations.
!> Connects state variables to specific IAPWS material models (water, ice).
module physics_types
    use, intrinsic :: iso_fortran_env
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core, only:type_state
    use :: physics_constants, only:TtoK => celsius_to_kelvin, P_atm => standard_atmospheric_pressure, min_vapor_density
    implicit none
    private

    public :: abst_physics

    !> Abstract base class for physics calculations.
    !> Handles thermodynamic property lookups for water, ice, and vapor phases.
    type, abstract :: abst_physics
        !> Pointer to IAPWS-97 water property calculator
        type(type_iapws97), pointer :: water => null()
        !> Pointer to IAPWS-06 ice property calculator
        type(type_iapws06), pointer :: ice => null()
    contains
        !> Convert temperature from Celsius to Kelvin.
        procedure, pass(self), public :: shift_temperature_absolute => shift_temperature_absolute_abst_physics
        !> Convert gauge pressure to absolute pressure.
        procedure, pass(self), public :: shift_pressure_absolute => shift_pressure_absolute_abst_physics
        !> Calculate liquid water density.
        procedure, pass(self), public :: calc_rho_water => calc_rho_water_abst_physics
        !> Calculate ice density.
        procedure, pass(self), public :: calc_rho_ice => calc_rho_ice_abst_physics
        !> Calculate vapor density based on relative humidity.
        procedure, pass(self), public :: calc_rho_vapor => calc_rho_vapor_abst_physics
        !> Calculate saturated vapor density.
        procedure, pass(self), public :: calc_rho_vapor_saturation => calc_rho_vapor_saturation_abst_physics
        !> Calculate specific heat capacity of liquid water.
        procedure, pass(self), public :: calc_cp_water => calc_cp_water_abst_physics
        !> Calculate specific heat capacity of ice.
        procedure, pass(self), public :: calc_cp_ice => calc_cp_ice_abst_physics
        !> Calculate specific heat capacity of vapor.
        procedure, pass(self), public :: calc_cp_vapor => calc_cp_vapor_abst_physics
    end type abst_physics

contains

    !> Helper to shift temperature to absolute scale (Kelvin).
    pure elemental subroutine shift_temperature_absolute_abst_physics(self, temperature_degree, temperature_K)
        implicit none
        !> Physics instance
        class(abst_physics), intent(in) :: self
        !> Temperature in degrees Celsius
        real(real64), intent(in) :: temperature_degree
        !> Temperature in Kelvin
        real(real64), intent(inout) :: temperature_K

        temperature_K = temperature_degree + TtoK
    end subroutine shift_temperature_absolute_abst_physics

    !> Helper to shift gauge pressure to absolute pressure.
    !> If gauge pressure is negative, assumes atmospheric pressure.
    pure elemental subroutine shift_pressure_absolute_abst_physics(self, pressure_gauge, pressure_absolute)
        implicit none
        !> Physics instance
        class(abst_physics), intent(in) :: self
        !> Gauge pressure [Pa]
        real(real64), intent(in) :: pressure_gauge
        !> Absolute pressure [Pa]
        real(real64), intent(inout) :: pressure_absolute

        if (pressure_gauge < 0.0d0) then
            pressure_absolute = P_atm
        else
            pressure_absolute = P_atm + pressure_gauge
        end if
    end subroutine shift_pressure_absolute_abst_physics

    !> Calculate liquid water density using IAPWS-97.
    pure elemental subroutine calc_rho_water_abst_physics(self, state, density)
        implicit none
        !> Physics instance
        class(abst_physics), intent(in) :: self
        !> State variables.
        !> Required: `temperature` [C], `pressure` [Pa] (gauge)
        type(type_state), intent(in) :: state
        !> Calculated density [kg/m^3]
        real(real64), intent(inout) :: density

        real(real64) :: temperature_K
        real(real64) :: pressure_absolute

        call self%shift_temperature_absolute(state%temperature, temperature_K)
        call self%shift_pressure_absolute(state%pressure, pressure_absolute)

        call self%water%calc_rho(temperature_K, pressure_absolute, density)

    end subroutine calc_rho_water_abst_physics

    !> Calculate ice density using IAPWS-06.
    pure elemental subroutine calc_rho_ice_abst_physics(self, state, density)
        implicit none
        !> Physics instance
        class(abst_physics), intent(in) :: self
        !> State variables.
        !> Required: `temperature` [C], `pressure` [Pa] (gauge)
        type(type_state), intent(in) :: state
        !> Calculated density [kg/m^3]
        real(real64), intent(inout) :: density

        real(real64) :: temperature_K
        real(real64) :: pressure_absolute

        call self%shift_temperature_absolute(state%temperature, temperature_K)
        call self%shift_pressure_absolute(state%pressure, pressure_absolute)

        call self%ice%calc_rho(temperature_K, pressure_absolute, density)

    end subroutine calc_rho_ice_abst_physics

    !> Calculate vapor density based on saturation density and relative humidity.
    !> Enforces a minimum vapor density to avoid numerical issues.
    pure elemental subroutine calc_rho_vapor_abst_physics(self, state, density)
        implicit none
        !> Physics instance
        class(abst_physics), intent(in) :: self
        !> State variables.
        !> Required: `temperature` [C], `relative_humidity` [-]
        type(type_state), intent(in) :: state
        !> Calculated density [kg/m^3]
        real(real64), intent(inout) :: density

        real(real64) :: temperature_K

        call self%shift_temperature_absolute(state%temperature, temperature_K)

        call self%water%calc_saturation_density(temperature_K, density)
        density = max(density * state%relative_humidity, min_vapor_density)

    end subroutine calc_rho_vapor_abst_physics

    !> Calculate saturated vapor density using IAPWS-97.
    pure elemental subroutine calc_rho_vapor_saturation_abst_physics(self, state, density)
        implicit none
        !> Physics instance
        class(abst_physics), intent(in) :: self
        !> State variables.
        !> Required: `temperature` [C]
        type(type_state), intent(in) :: state
        !> Calculated saturated vapor density [kg/m^3]
        real(real64), intent(inout) :: density

        real(real64) :: temperature_K

        call self%shift_temperature_absolute(state%temperature, temperature_K)
        call self%water%calc_saturation_density(temperature_K, density)
    end subroutine calc_rho_vapor_saturation_abst_physics

    !> Calculate specific isobaric heat capacity of liquid water (Cp) using IAPWS-97.
    pure elemental subroutine calc_cp_water_abst_physics(self, state, cp)
        implicit none
        !> Physics instance
        class(abst_physics), intent(in) :: self
        !> State variables.
        !> Required: `temperature` [C], `pressure` [Pa] (gauge)
        type(type_state), intent(in) :: state
        !> Calculated specific heat [J/(kg K)]
        real(real64), intent(inout) :: cp

        real(real64) :: temperature_K
        real(real64) :: pressure_absolute

        call self%shift_temperature_absolute(state%temperature, temperature_K)
        call self%shift_pressure_absolute(state%pressure, pressure_absolute)

        call self%water%calc_cp(temperature_K, pressure_absolute, cp)

    end subroutine calc_cp_water_abst_physics

    !> Calculate specific isobaric heat capacity of ice (Cp) using IAPWS-06.
    pure elemental subroutine calc_cp_ice_abst_physics(self, state, cp)
        implicit none
        !> Physics instance
        class(abst_physics), intent(in) :: self
        !> State variables.
        !> Required: `temperature` [C], `pressure` [Pa] (gauge)
        type(type_state), intent(in) :: state
        !> Calculated specific heat [J/(kg K)]
        real(real64), intent(inout) :: cp

        real(real64) :: temperature_K
        real(real64) :: pressure_absolute

        call self%shift_temperature_absolute(state%temperature, temperature_K)
        call self%shift_pressure_absolute(state%pressure, pressure_absolute)

        call self%ice%calc_cp(temperature_K, pressure_absolute, cp)

    end subroutine calc_cp_ice_abst_physics

    !> Calculate specific isobaric heat capacity of vapor (Cp) at saturation.
    pure elemental subroutine calc_cp_vapor_abst_physics(self, state, cp)
        implicit none
        !> Physics instance
        class(abst_physics), intent(in) :: self
        !> State variables.
        !> Required: `temperature` [C]
        type(type_state), intent(in) :: state
        !> Calculated specific heat [J/(kg K)]
        real(real64), intent(inout) :: cp

        real(real64) :: temperature_K

        call self%shift_temperature_absolute(state%temperature, temperature_K)

        call self%water%calc_saturation_cp(temperature_K, cp)

    end subroutine calc_cp_vapor_abst_physics

end module physics_types

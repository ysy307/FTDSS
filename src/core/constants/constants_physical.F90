module core_constants_physical
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    !> Mathematical constant pi
    real(real64), parameter, public :: pi = 3.141592653589793d0

    !> Conversion from Celsius to Kelvin [K]
    real(real64), parameter, public :: celsius_to_kelvin = 273.15d0

    !> Gravitational acceleration [m/s^2]
    real(real64), parameter, public :: gravity_acceleration = 9.80665d0
    !> Standard atmospheric pressure [Pa]
    real(real64), parameter, public :: standard_atmospheric_pressure = 101325.0d0

    !> Universal gas constant [J/K/mol]
    real(real64), parameter, public :: universal_gas_constant = 8.31446261815324d0

    !------------------------------------------------------------------------------------------
    ! For water properties
    !------------------------------------------------------------------------------------------
    !> The specific gas constant of ordinary water [kJ/kg/K]
    real(real64), parameter, public :: specific_gas_constant_water = 0.461526d0
    !> Water triple point temperature [K]
    real(real64), parameter, public :: water_triple_point_temperature = 273.16d0
    !> Water triple point pressure [Pa]
    real(real64), parameter, public :: water_triple_point_pressure = 611.657d0
    !> Water triple point density [kg/m^3]
    real(real64), parameter, public :: water_triple_point_density = 999.793d0
    !> Water critical point temperature [K]
    real(real64), parameter, public :: water_critical_point_temperature = 647.096d0
    !> Water critical point pressure [Pa]
    real(real64), parameter, public :: water_critical_point_pressure = 22.06464d6
    !> Water critical point density [kg/m^3]
    real(real64), parameter, public :: water_critical_point_density = 322.0d0

    !> Molar mass of water [kg/mol]
    real(real64), parameter, public :: molar_mass_water = 0.01801528d0

    !> Stefan-Boltzman constant [W/m^2/K^4]
    real(real64), parameter, public :: stefan_boltzmann_constant = 5.670374419d-8
end module core_constants_physical

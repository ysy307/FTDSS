!> Physics models for vaporization and evaporation in porous media.
!>
!> This module provides the constitutive relations for calculating
!> vaporization effects, including relative humidity via the Kelvin equation,
!> latent heat of vaporization, and equivalent liquid vapor content.
module models_phase_change_vaporization
    use, intrinsic :: iso_fortran_env
    use :: iapws, only:type_iapws97
    use :: module_core, only:type_state
    use :: constitutive_constants, only: &
        g => gravity_acceleration, &
        T_to_K => celsius_to_kelvin, &
        Rg => universal_gas_constant, &
        Mw => molar_mass_water, &
        rho_std => reference_water_density, &
        P_atm => standard_atmospheric_pressure
    use :: physics_constitutive_base, only:abst_constitutive

    implicit none
    private

    public :: type_evaporation

    !> Constitutive model for evaporation and vapor phase thermodynamics.
    type, extends(abst_constitutive) :: type_evaporation
    contains
        procedure, pass(self), public :: initialize => initialize_evaporation_model
        procedure, pass(self), public :: calc_latent_heat_vaporization => calc_latent_heat_vaporization_evaporation
        procedure, pass(self), public :: calc_relative_humidity => calc_relative_humidity_evaporation
        procedure, pass(self), public :: deriv_relative_humidity_temperature => deriv_relative_humidity_temperature_evaporation
        procedure, pass(self), public :: deriv_relative_humidity_pressure => deriv_relative_humidity_pressure_evaporation
        procedure, pass(self), public :: calc_vapor_content => calc_vapor_content_vaporization
        procedure, pass(self), public :: calc_vapor_content_derivatives => calc_vapor_content_derivatives_vaporization
    end type type_evaporation

    interface
        !> Initialize the evaporation model.
        !>
        !> Functional specification:
        !> - Connects the IAPWS-97 water model for accurate thermodynamic calculations.
        module subroutine initialize_evaporation_model(self, water)
            implicit none
            !> Evaporation model instance
            class(type_evaporation), intent(inout) :: self
            !> IAPWS-97 water property model
            type(type_iapws97), intent(in), target :: water
        end subroutine initialize_evaporation_model

        !> Calculate the latent heat of vaporization.
        !>
        !> Mathematical definition:
        !> - Evaluates \( L_v = f(T) \) utilizing the IAPWS-97 formulation if available.
        !> - Fallback formulation: \( L_v = 2.501 \times 10^6 - 2369.2 T \)
        module subroutine calc_latent_heat_vaporization_evaporation(self, temperature, latent_heat)
            implicit none
            !> Evaporation model instance
            class(type_evaporation), intent(in) :: self
            !> Temperature in Celsius
            real(real64), intent(in) :: temperature
            !> Latent heat of vaporization [J/kg]
            real(real64), intent(inout) :: latent_heat
        end subroutine calc_latent_heat_vaporization_evaporation

        !> Calculate relative humidity using the Kelvin equation.
        !>
        !> Mathematical definition:
        !> \[
        !> RH = \begin{cases}
        !> \exp\left(\frac{P M_w}{\rho_{std} R_g T}\right) & \text{if } P < 0 \\
        !> 1.0 & \text{if } P \ge 0
        !> \end{cases}
        !> \]
        !>
        !> Assumptions:
        !> - Liquid water and vapor are in local thermodynamic equilibrium.
        !> - Vapor behaves as an ideal gas.
        module subroutine calc_relative_humidity_evaporation(self, state, relative_humidity)
            implicit none
            !> Evaporation model instance
            class(type_evaporation), intent(in) :: self
            !> Thermodynamic state variables
            type(type_state), intent(in) :: state
            !> Relative humidity [-]
            real(real64), intent(inout) :: relative_humidity
        end subroutine calc_relative_humidity_evaporation

        !> Calculate the temperature derivative of relative humidity.
        !>
        !> Mathematical definition:
        !> \[
        !> \frac{\partial RH}{\partial T} = RH \left( \frac{-P M_w}{\rho_{std} R_g T^2} \right)
        !> \]
        module subroutine deriv_relative_humidity_temperature_evaporation(self, state, deriv_rh_temp)
            implicit none
            !> Evaporation model instance
            class(type_evaporation), intent(in) :: self
            !> Thermodynamic state variables
            type(type_state), intent(in) :: state
            !> Derivative of relative humidity with respect to temperature [1/K]
            real(real64), intent(inout) :: deriv_rh_temp
        end subroutine deriv_relative_humidity_temperature_evaporation

        !> Calculate the pressure derivative of relative humidity.
        !>
        !> Mathematical definition:
        !> \[
        !> \frac{\partial RH}{\partial P} = RH \left( \frac{M_w}{\rho_{std} R_g T} \right)
        !> \]
        module subroutine deriv_relative_humidity_pressure_evaporation(self, state, deriv_rh_pressure)
            implicit none
            !> Evaporation model instance
            class(type_evaporation), intent(in) :: self
            !> Thermodynamic state variables
            type(type_state), intent(in) :: state
            !> Derivative of relative humidity with respect to pressure [1/Pa]
            real(real64), intent(inout) :: deriv_rh_pressure
        end subroutine deriv_relative_humidity_pressure_evaporation

        !> Calculate vapor content as an equivalent liquid volume fraction.
        !>
        !> Mathematical definition:
        !> \[
        !> \theta_v = \frac{\rho_{sat} RH \theta_a}{\rho_w}
        !> \]
        !>
        !> Assumptions:
        !> - Vapor mass is converted to an equivalent liquid volume to maintain consistent units across mass balance equations.
        module subroutine calc_vapor_content_vaporization(self, state, vapor_content)
            implicit none
            !> Evaporation model instance
            class(type_evaporation), intent(in) :: self
            !> Thermodynamic state variables
            type(type_state), intent(in) :: state
            !> Vapor content (liquid equivalent) [-]
            real(real64), intent(inout) :: vapor_content
        end subroutine calc_vapor_content_vaporization

        !> Compute pressure and temperature derivatives of vapor content.
        !>
        !> Mathematical definition:
        !> - Applies the chain rule to the vapor content equation to find \( \frac{\partial \theta_v}{\partial P} \) 
        !> and \( \frac{\partial \theta_v}{\partial T} \).
        module subroutine calc_vapor_content_derivatives_vaporization(self, state, dvapor_dP, dvapor_dT)
            implicit none
            !> Evaporation model instance
            class(type_evaporation), intent(in) :: self
            !> Thermodynamic state variables
            type(type_state), intent(in) :: state
            !> Derivative of vapor content with respect to pressure [1/Pa]
            real(real64), intent(inout) :: dvapor_dP
            !> Derivative of vapor content with respect to temperature [1/K]
            real(real64), intent(inout) :: dvapor_dT
        end subroutine calc_vapor_content_derivatives_vaporization
    end interface

end module models_phase_change_vaporization

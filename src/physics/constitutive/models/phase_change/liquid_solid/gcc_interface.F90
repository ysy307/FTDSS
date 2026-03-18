!> Interface definition for Generalized Clausius-Clapeyron (GCC) models.
!>
!> This module defines the abstract base classes and interfaces for calculating
!> suction and its derivatives based on the Generalized Clausius-Clapeyron equation.
!> It serves as the foundation for phase change models involving liquid and solid water.
module models_phase_change_gcc
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core
    use :: constitutive_constants, only: &
        T_to_K => celsius_to_kelvin, &
        P_atm => standard_atmospheric_pressure, &
        lf => latent_heat_fusion_water_0c, &
        g => gravity_acceleration, &
        Tf0 => water_freezing_point_at_standard_atmospheric_pressure, &
        Tf0_K => water_freezing_point_at_standard_atmospheric_pressure_k

    use :: physics_constitutive_base, only:abst_constitutive
    implicit none
    private

    public :: holder_gccs
    public :: abst_gcc
    public :: type_gcc_non_segregation
    public :: type_gcc_segregation

    !> Polymorphic holder for GCC objects.
    !> Safely manages dynamic dispatch for different phase change models.
    type :: holder_gccs
        !> Polymorphic pointer to the specific GCC implementation
        class(abst_gcc), allocatable :: p
    contains
        procedure, pass(self) :: initialize => initialize_holder_gccs
        procedure, pass(self) :: calc => calc_holder
        procedure, pass(self) :: deriv_temperature => deriv_temperature_holder
        procedure, pass(self) :: deriv_pressure => deriv_pressure_holder
    end type holder_gccs

    interface
        !> Initialize the polymorphic GCC holder.
        !>
        !> Allocates the underlying specific GCC model based on the configuration.
        !>
        !> Assumptions:
        !> - Configuration must contain a valid GCC model ID.
        module subroutine initialize_holder_gccs(self, material_id, config, water, ice)
            implicit none
            class(holder_gccs), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            class(type_config_gcc), intent(in) :: config
            type(type_iapws97), target, intent(in) :: water
            type(type_iapws06), target, intent(in) :: ice
        end subroutine initialize_holder_gccs
    end interface

    !> Abstract base class defining the contract for GCC models.
    !>
    !> Defines procedures to calculate cryogenic suction \( \psi \) and its derivatives.
    type, extends(abst_constitutive), abstract :: abst_gcc
        !> Target material ID
        integer(int32) :: material_id = -1
    contains
        procedure, pass(self), public :: initialize => initialize_abst_gcc
        procedure(abst_calc_gcc), pass(self), public, deferred :: calc
        procedure(abst_deriv_temp_gcc), pass(self), public, deferred :: deriv_temperature
        procedure(abst_deriv_pres_gcc), pass(self), public, deferred :: deriv_pressure
        procedure(abst_deriv2_temp_gcc), pass(self), public, deferred :: deriv_temperature_2nd
        procedure(abst_deriv_pressure_ice_water), pass(self), public, deferred :: deriv_pressure_ice_water
    end type abst_gcc

    abstract interface
        !> Calculate cryogenic suction.
        !>
        !> Mathematical definition:
        !> - Computes suction \( \psi \) representing the energy potential difference.
        !>
        !> Assumptions:
        !> - Local thermodynamic equilibrium between ice and liquid water.
        !>
        !> Valid ranges and side effects:
        !> - Suction is strictly \(\ge 0\). It is zero for temperatures above \( T_{f0} \).
        subroutine abst_calc_gcc(self, state, suction)
            import :: abst_gcc, type_state, real64
            implicit none
            class(abst_gcc), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction
        end subroutine abst_calc_gcc

        !> Calculate the first derivative of suction with respect to temperature.
        !>
        !> Mathematical definition:
        !> - Computes \( \frac{\partial \psi}{\partial T} \).
        !>
        !> Valid ranges and side effects:
        !> - Evaluates to zero for \( T > T_{f0} \).
        subroutine abst_deriv_temp_gcc(self, state, suction_derivative)
            import :: abst_gcc, type_state, real64
            implicit none
            class(abst_gcc), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction_derivative
        end subroutine abst_deriv_temp_gcc

        !> Calculate the first derivative of suction with respect to pressure.
        !>
        !> Mathematical definition:
        !> - Computes \( \frac{\partial \psi}{\partial P} \).
        !>
        !> Valid ranges and side effects:
        !> - Evaluates to zero for \( T > T_{f0} \) or in non-segregation models.
        subroutine abst_deriv_pres_gcc(self, state, suction_derivative)
            import :: abst_gcc, type_state, real64
            implicit none
            class(abst_gcc), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction_derivative
        end subroutine abst_deriv_pres_gcc

        !> Calculate the second derivative of suction with respect to temperature.
        !>
        !> Mathematical definition:
        !> - Computes \( \frac{\partial^2 \psi}{\partial T^2} \).
        !>
        !> Valid ranges and side effects:
        !> - Evaluates to zero for \( T > T_{f0} \).
        subroutine abst_deriv2_temp_gcc(self, state, suction_derivative)
            import :: abst_gcc, type_state, real64
            implicit none
            class(abst_gcc), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction_derivative
        end subroutine abst_deriv2_temp_gcc

        !> Calculate the derivative of ice pressure with respect to water pressure.
        !>
        !> Mathematical definition:
        !> - Computes \( \frac{\partial P_i}{\partial P_w} \).
        !>
        !> Valid ranges and side effects:
        !> - Evaluates to 0.0 for \( T > T_{f0} \).
        subroutine abst_deriv_pressure_ice_water(self, state, deriv)
            import :: abst_gcc, type_state, real64
            implicit none
            class(abst_gcc), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: deriv
        end subroutine abst_deriv_pressure_ice_water
    end interface

    interface
        !> Initialize the abstract GCC component.
        module subroutine initialize_abst_gcc(self, material_id, water, ice)
            implicit none
            class(abst_gcc), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_iapws97), target, intent(in) :: water
            type(type_iapws06), target, intent(in) :: ice
        end subroutine initialize_abst_gcc
    end interface

    !> GCC model assuming no ice segregation (in-situ freezing only).
    !>
    !> Mathematical definition:
    !> \[
    !>  \psi = -L_f \rho_w \ln\left(\frac{T}{T_{f0}}\right)
    !> \]
    !> Suction strictly depends on temperature and assumes \( P_i \) equals \( P_w \) plus suction.
    type, extends(abst_gcc) :: type_gcc_non_segregation
    contains
        procedure, pass(self) :: calc => calc_gcc_nonseg
        procedure, pass(self) :: deriv_temperature => deriv_temp_gcc_nonseg
        procedure, pass(self) :: deriv_pressure => deriv_pres_gcc_nonseg
        procedure, pass(self) :: deriv_temperature_2nd => deriv2_temp_gcc_nonseg
        procedure, pass(self) :: deriv_pressure_ice_water => deriv_pressure_ice_water_nonseg
    end type type_gcc_non_segregation

    !> GCC model incorporating ice segregation mechanics.
    !>
    !> Mathematical definition:
    !> \[
    !>  \psi = \left(\frac{\rho_i}{\rho_w} - 1\right) P - L_f \rho_i \ln\left(\frac{T}{T_{f0}}\right)
    !> \]
    !> Allows macroscopic pressure dependence indicating heave potential.
    type, extends(abst_gcc) :: type_gcc_segregation
    contains
        procedure, pass(self) :: calc => calc_gcc_seg
        procedure, pass(self) :: deriv_temperature => deriv_temp_gcc_seg
        procedure, pass(self) :: deriv_pressure => deriv_pres_gcc_seg
        procedure, pass(self) :: deriv_temperature_2nd => deriv2_temp_gcc_seg
        procedure, pass(self) :: deriv_pressure_ice_water => deriv_pressure_ice_water_seg
    end type type_gcc_segregation

    interface
        module subroutine calc_holder(self, state, suction)
            implicit none
            class(holder_gccs), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction
        end subroutine calc_holder

        module subroutine deriv_temperature_holder(self, state, deriv)
            implicit none
            class(holder_gccs), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: deriv
        end subroutine deriv_temperature_holder

        module subroutine deriv_pressure_holder(self, state, deriv)
            implicit none
            class(holder_gccs), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: deriv
        end subroutine deriv_pressure_holder

        module subroutine calc_gcc_nonseg(self, state, suction)
            implicit none
            class(type_gcc_non_segregation), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction
        end subroutine calc_gcc_nonseg

        module subroutine deriv_temp_gcc_nonseg(self, state, suction_derivative)
            implicit none
            class(type_gcc_non_segregation), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction_derivative
        end subroutine deriv_temp_gcc_nonseg

        module subroutine deriv_pres_gcc_nonseg(self, state, suction_derivative)
            implicit none
            class(type_gcc_non_segregation), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction_derivative
        end subroutine deriv_pres_gcc_nonseg

        module subroutine deriv2_temp_gcc_nonseg(self, state, suction_derivative)
            implicit none
            class(type_gcc_non_segregation), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction_derivative
        end subroutine deriv2_temp_gcc_nonseg

        module subroutine deriv_pressure_ice_water_nonseg(self, state, deriv)
            implicit none
            class(type_gcc_non_segregation), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: deriv
        end subroutine deriv_pressure_ice_water_nonseg

        module subroutine calc_gcc_seg(self, state, suction)
            implicit none
            class(type_gcc_segregation), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction
        end subroutine calc_gcc_seg

        module subroutine deriv_temp_gcc_seg(self, state, suction_derivative)
            implicit none
            class(type_gcc_segregation), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction_derivative
        end subroutine deriv_temp_gcc_seg

        module subroutine deriv_pres_gcc_seg(self, state, suction_derivative)
            implicit none
            class(type_gcc_segregation), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction_derivative
        end subroutine deriv_pres_gcc_seg

        module subroutine deriv2_temp_gcc_seg(self, state, suction_derivative)
            implicit none
            class(type_gcc_segregation), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction_derivative
        end subroutine deriv2_temp_gcc_seg

        module subroutine deriv_pressure_ice_water_seg(self, state, deriv)
            implicit none
            class(type_gcc_segregation), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: deriv
        end subroutine deriv_pressure_ice_water_seg
    end interface

end module models_phase_change_gcc

!>
!> @brief Interface definition for Generalized Clausius-Clapeyron (GCC) models.
!>
!> This module defines the abstract base classes and interfaces for calculating
!> suction and its derivatives based on the Generalized Clausius-Clapeyron equation.
!> It serves as the foundation for phase change models involving liquid and solid water.
!>
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

    !>
    !> @brief Holder for GCC objects to handle polymorphism.
    !>
    type :: holder_gccs
        !> Polymorphic pointer to the specific GCC implementation
        class(abst_gcc), allocatable :: p
    contains
        procedure, pass(self) :: initialize => initialize_holder_gccs
        !> Wrapper methods for the holder
        procedure, pass(self) :: calc => calc_holder
        procedure, pass(self) :: deriv_temperature => deriv_temperature_holder
        procedure, pass(self) :: deriv_pressure => deriv_pressure_holder
    end type holder_gccs

!>
!> @brief Interface for initializing the holder.
!>
    interface
        module subroutine initialize_holder_gccs(self, material_id, config, water, ice)
            implicit none
            class(holder_gccs), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            class(type_config_gcc), intent(in) :: config
            type(type_iapws97), target, intent(in) :: water
            type(type_iapws06), target, intent(in) :: ice
        end subroutine initialize_holder_gccs
    end interface

!>
!> @brief Abstract base class for GCC models.
!>
    type, extends(abst_constitutive), abstract :: abst_gcc
        !> Material ID
        integer(int32) :: material_id = -1
    contains
        procedure, pass(self), public :: initialize => initialize_abst_gcc

        !> Calculate Suction [Pa]
        procedure(abst_calc_gcc), pass(self), public, deferred :: calc

        !> Calculate d(Suction)/dT [Pa/K] (Temperature derivative)
        procedure(abst_deriv_temp_gcc), pass(self), public, deferred :: deriv_temperature

        !> Calculate d(Suction)/dP [-] (Pressure derivative)
        procedure(abst_deriv_pres_gcc), pass(self), public, deferred :: deriv_pressure

        !> Calculate d^2(Suction)/dT^2 [Pa/K^2] (2nd Temperature derivative)
        procedure(abst_deriv2_temp_gcc), pass(self), public, deferred :: deriv_temperature_2nd

        !> Calculate dP_ice/dP_w
        procedure(abst_deriv_pressure_ice_water), pass(self), public, deferred :: deriv_pressure_ice_water
    end type abst_gcc

    !---------------------------------------------------------------------------
    ! Abstract Interfaces
    !---------------------------------------------------------------------------
    abstract interface
        !> @brief Calculate suction [Pa].
        subroutine abst_calc_gcc(self, state, suction)
            import :: abst_gcc, type_state, real64
            implicit none
            class(abst_gcc), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction
        end subroutine abst_calc_gcc

        !> @brief Calculate first derivative of suction w.r.t Temperature [Pa/K].
        subroutine abst_deriv_temp_gcc(self, state, suction_derivative)
            import :: abst_gcc, type_state, real64
            implicit none
            class(abst_gcc), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction_derivative
        end subroutine abst_deriv_temp_gcc

        !> @brief Calculate first derivative of suction w.r.t Pressure [-].
        !> Note: d(Pa)/d(Pa) is dimensionless.
        subroutine abst_deriv_pres_gcc(self, state, suction_derivative)
            import :: abst_gcc, type_state, real64
            implicit none
            class(abst_gcc), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction_derivative
        end subroutine abst_deriv_pres_gcc

        !> @brief Calculate second derivative of suction w.r.t Temperature [Pa/K^2].
        subroutine abst_deriv2_temp_gcc(self, state, suction_derivative)
            import :: abst_gcc, type_state, real64
            implicit none
            class(abst_gcc), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction_derivative
        end subroutine abst_deriv2_temp_gcc

        !> @brief Calculate derivative of ice pressure w.r.t water pressure [-].
        subroutine abst_deriv_pressure_ice_water(self, state, deriv)
            import :: abst_gcc, type_state, real64
            implicit none
            class(abst_gcc), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: deriv
        end subroutine abst_deriv_pressure_ice_water
    end interface

    interface
        module subroutine initialize_abst_gcc(self, material_id, water, ice)
            implicit none
            class(abst_gcc), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_iapws97), target, intent(in) :: water
            type(type_iapws06), target, intent(in) :: ice
        end subroutine initialize_abst_gcc
    end interface

    !---------------------------------------------------------------------------
    ! Concrete Types
    !---------------------------------------------------------------------------

!>
!> @brief GCC model without ice segregation (Non-segregation).
!>
    type, extends(abst_gcc) :: type_gcc_non_segregation
    contains
        procedure, pass(self) :: calc => calc_gcc_nonseg
        procedure, pass(self) :: deriv_temperature => deriv_temp_gcc_nonseg
        procedure, pass(self) :: deriv_pressure => deriv_pres_gcc_nonseg
        procedure, pass(self) :: deriv_temperature_2nd => deriv2_temp_gcc_nonseg
        procedure, pass(self) :: deriv_pressure_ice_water => deriv_pressure_ice_water_nonseg
    end type type_gcc_non_segregation

!>
!> @brief GCC model with ice segregation.
!>
    type, extends(abst_gcc) :: type_gcc_segregation
    contains
        procedure, pass(self) :: calc => calc_gcc_seg
        procedure, pass(self) :: deriv_temperature => deriv_temp_gcc_seg
        procedure, pass(self) :: deriv_pressure => deriv_pres_gcc_seg
        procedure, pass(self) :: deriv_temperature_2nd => deriv2_temp_gcc_seg
        procedure, pass(self) :: deriv_pressure_ice_water => deriv_pressure_ice_water_seg
    end type type_gcc_segregation

    !---------------------------------------------------------------------------
    ! Module Procedure Interfaces
    !---------------------------------------------------------------------------
    interface
        ! --- Holder methods ---
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

        ! --- Non-segregation methods ---
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

        ! --- Segregation methods ---
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

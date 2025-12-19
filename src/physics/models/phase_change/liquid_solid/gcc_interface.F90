!>
!> @brief Interface definition for Generalized Clausius-Clapeyron (GCC) models.
!>
!> This module defines the abstract base classes and interfaces for calculating
!> suction and its derivatives based on the Generalized Clausius-Clapeyron equation.
!> It serves as the foundation for phase change models involving liquid and solid water.
!>
module physics_models_phase_change_liquid_solid_gcc
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: iapws, only:type_iapws97, type_iapws06
    use :: module_core
    use :: physics_constants, only: &
        T_to_K => celsius_to_kelvin, &
        lf => latent_heat_fusion_water_0c, &
        g => gravity_acceleration, &
        Tf0 => water_freezing_point_at_standard_atmospheric_pressure, &
        Tf0_K => water_freezing_point_at_standard_atmospheric_pressure_k

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
    end type holder_gccs

!>
!> @brief Interface for initializing the holder.
!>
    interface
        module subroutine initialize_holder_gccs(self, material_id, gcc_id, water, ice)
            implicit none
            !> The holder object to be initialized
            class(holder_gccs), intent(inout) :: self
            !> Material ID associated with this model
            integer(int32), intent(in) :: material_id
            !> GCC model ID (e.g. Segregation or Non-segregation)
            integer(int32), intent(in) :: gcc_id
            !> Water property object (IAPWS97)
            type(type_iapws97), target, intent(in) :: water
            !> Ice property object (IAPWS06)
            type(type_iapws06), target, intent(in) :: ice
        end subroutine initialize_holder_gccs
    end interface

!>
!> @brief Abstract base class for GCC models.
!>
    type, abstract :: abst_gcc
        !> Material ID
        integer(int32) :: material_id = -1
        !> Pointer to water property object
        type(type_iapws97), pointer :: water => null()
        !> Pointer to ice property object
        type(type_iapws06), pointer :: ice => null()
    contains
        procedure, pass(self), public :: initialize => initialize_abst_gcc
        procedure(abst_calc_gcc), pass(self), public, deferred :: calc
        procedure(abst_deriv_gcc), pass(self), public, deferred :: deriv
        procedure(abst_deriv2_gcc), pass(self), public, deferred :: deriv2
    end type abst_gcc

!>
!> @brief Abstract interface for calculating suction.
!>
    abstract interface
        !>
        !> @brief Calculate suction [Pa].
        !>
        pure elemental subroutine abst_calc_gcc(self, state, suction)
            import :: abst_gcc, type_state, real64
            implicit none
            !> GCC object
            class(abst_gcc), intent(in) :: self
            !> Thermodynamic state
            type(type_state), intent(in) :: state
            !> Suction pressure [Pa]
            real(real64), intent(inout) :: suction
        end subroutine abst_calc_gcc

        !>
        !> @brief Calculate first derivative of suction [Pa/K].
        !>
        pure elemental subroutine abst_deriv_gcc(self, state, suction_derivative)
            import :: abst_gcc, type_state, real64
            implicit none
            !> GCC object
            class(abst_gcc), intent(in) :: self
            !> Thermodynamic state
            type(type_state), intent(in) :: state
            !> d(Suction)/dT [Pa/K]
            real(real64), intent(inout) :: suction_derivative
        end subroutine abst_deriv_gcc

        !>
        !> @brief Calculate second derivative of suction [Pa/K^2].
        !>
        pure elemental subroutine abst_deriv2_gcc(self, state, suction_derivative)
            import :: abst_gcc, type_state, real64
            implicit none
            !> GCC object
            class(abst_gcc), intent(in) :: self
            !> Thermodynamic state
            type(type_state), intent(in) :: state
            !> d^2(Suction)/dT^2 [Pa/K^2]
            real(real64), intent(inout) :: suction_derivative
        end subroutine abst_deriv2_gcc
    end interface

    interface
        module subroutine initialize_abst_gcc(self, material_id, water, ice)
            implicit none
            !> Abstract GCC object
            class(abst_gcc), intent(inout) :: self
            !> Material ID
            integer(int32), intent(in) :: material_id
            !> Water property object
            type(type_iapws97), target, intent(in) :: water
            !> Ice property object
            type(type_iapws06), target, intent(in) :: ice
        end subroutine initialize_abst_gcc
    end interface

!>
!> @brief GCC model without ice segregation (Non-segregation).
!>
    type, extends(abst_gcc) :: type_gcc_non_segregation
    contains
        procedure, pass(self) :: calc => calc_gcc_nonseg
        procedure, pass(self) :: deriv => deriv_gcc_nonseg
        procedure, pass(self) :: deriv2 => deriv_2nd_gcc_nonseg
    end type type_gcc_non_segregation

!>
!> @brief GCC model with ice segregation.
!>
    type, extends(abst_gcc) :: type_gcc_segregation
    contains
        procedure, pass(self) :: calc => calc_gcc_seg
        procedure, pass(self) :: deriv => deriv_gcc_seg
        procedure, pass(self) :: deriv2 => deriv_2nd_gcc_seg
    end type type_gcc_segregation

!>
!> @brief Interfaces for concrete implementations of GCC methods.
!>
    interface
        module pure elemental subroutine calc_gcc_nonseg(self, state, suction)
            implicit none
            class(type_gcc_non_segregation), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction
        end subroutine calc_gcc_nonseg

        module pure elemental subroutine deriv_gcc_nonseg(self, state, suction_derivative)
            implicit none
            class(type_gcc_non_segregation), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction_derivative
        end subroutine deriv_gcc_nonseg

        module pure elemental subroutine deriv_2nd_gcc_nonseg(self, state, suction_derivative)
            implicit none
            class(type_gcc_non_segregation), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction_derivative
        end subroutine deriv_2nd_gcc_nonseg
    end interface

    interface
        module pure elemental subroutine calc_gcc_seg(self, state, suction)
            implicit none
            class(type_gcc_segregation), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction
        end subroutine calc_gcc_seg

        module pure elemental subroutine deriv_gcc_seg(self, state, suction_derivative)
            implicit none
            class(type_gcc_segregation), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction_derivative
        end subroutine deriv_gcc_seg

        module pure elemental subroutine deriv_2nd_gcc_seg(self, state, suction_derivative)
            implicit none
            class(type_gcc_segregation), intent(in) :: self
            type(type_state), intent(in) :: state
            real(real64), intent(inout) :: suction_derivative
        end subroutine deriv_2nd_gcc_seg
    end interface

end module physics_models_phase_change_liquid_solid_gcc

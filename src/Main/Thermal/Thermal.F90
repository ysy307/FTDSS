module Main_Thermal
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Core_BaseTypes
    use :: Core_Element
    use :: Inout_Input
    use :: Calculate_Ice, only:Abstract_Ice, Type_Ice_TRM, Type_Ice_GCC, Type_Ice_EXP
    use :: Calculate_VolumetricHeatCapacity
    use :: Calculate_ThermalConductivity
    use :: Matrix_Assemble
    use :: Matrix_CRS
    use :: Condition_Fix_Boundary_Conditions, only:Type_BC_Thermal
    use :: Solver_Solve
    implicit none

    type, abstract :: Abstract_Thermal
        type(Variables) :: T

        type(Type_CRS) :: KT_star_0
        type(Type_CRS) :: KT_l
        type(Type_CRS) :: KT_old
        type(Type_CRS) :: CT_l
        type(Type_CRS), allocatable :: CT_old(:)

        real(real64), allocatable :: FT(:)
        real(real64), allocatable :: FT_old(:)
        real(real64), allocatable :: PHIT(:)
        real(real64), allocatable :: PHIT_old(:)

        ! type(DP3d), pointer :: Coordinate
        integer(int32) :: nsize
        integer(int32) :: nElement
        type(ElementHolder), allocatable :: Elements(:)
        type(Type_BC_Thermal) :: BC
        !! Thermal properties
        class(Abstract_Ice), allocatable :: Ice
        ! class(Abstarct_VolumetricHeatCapacity), allocatable :: C
        class(Abstract_ThermalConductivity), allocatable :: lambda

        !! Solver
        class(Abstract_Solver_CRS), allocatable :: Solver
        integer(int32) :: Order
    contains
        procedure(Abstract_Assemble), pass(self), deferred :: Assemble
    end type Abstract_Thermal

    type, extends(Abstract_Thermal) :: Type_Thermal_3Phase_2D
    contains
        procedure, pass(self) :: Assemble => Type_Thermal_3Phase_2D_Assemble
        ! procedure :: Update => Type_Thermal_3Phase_2D_Update
    end type Type_Thermal_3Phase_2D

    abstract interface
        subroutine Abstract_Assemble(self, dt, step, iter)
            import :: Abstract_Thermal, int32, real64
            implicit none
            class(Abstract_Thermal), intent(inout) :: self
            real(real64), intent(in) :: dt
            integer(int32), intent(in) :: step
            integer(int32), intent(in) :: iter

        end subroutine Abstract_Assemble
    end interface

    interface
        module function Type_Thermal_3Phase_2D_Construct(Input, Coordinate) result(Structure)
            implicit none
            class(Abstract_Thermal), allocatable :: Structure
            type(Type_Input), intent(in) :: Input
            type(DP3d), intent(inout), pointer :: Coordinate

        end function Type_Thermal_3Phase_2D_Construct

        module subroutine Type_Thermal_3Phase_2D_Assemble(self, dt, step, iter)
            implicit none
            class(Type_Thermal_3Phase_2D), intent(inout) :: self
            real(real64), intent(in) :: dt
            integer(int32), intent(in) :: step
            integer(int32), intent(in) :: iter

        end subroutine Type_Thermal_3Phase_2D_Assemble

    end interface

    interface Type_Thermal_3Phase_2D
        module procedure Type_Thermal_3Phase_2D_Construct
    end interface

contains

    ! subroutine Type_Thermal_3Phase_2D_Update(self, phi_soil, rho_ice, iiter)
    !     implicit none
    !     class(Type_Thermal_3Phase_2D), intent(inout) :: self
    !     real(real64), intent(in) :: phi_soil
    !     real(real64), intent(in) :: rho_ice
    !     integer(int32), intent(in) :: iiter

    !     select type (Ice => self%Ice)
    !     type is (Type_Ice_GCC)
    !         call Ice%Update_Ice(self%T%pre(:))
    !     end select
    !     call self%lambda%Update(phi_soil, self%Ice%Qw%pre, self%Ice%Qice%pre)
    !     call self%C%Update(phi_soil, self%Ice%Qw%pre, self%Ice%Qice%pre)
    !     ! if (iiter == 1) then
    !     !     call self%C%Update_Ca_Revise(structure_Ice=self%Ice, rho_ice=ThermalInput%rho(3), arr_Temperature=self%T%pre(:), arr_Temperature_old=self%T%old(:))
    !     ! else
    !     call self%C%Update_Ca(structure_Ice=self%Ice, rho_ice=rho_ice, arr_Temperature=self%T%pre(:))
    !     ! end if

    ! end subroutine Type_Thermal_3Phase_2D_Update

end module Main_Thermal

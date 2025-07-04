module Calculate_ThermalConductivity
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Core_BaseTypes, only:GaussPointState_t
    use :: Inout_Input
    implicit none
    private

    public :: THCHolder, Abst_THC, Type_THC_3Phase

    type :: THCHolder
        class(Abst_THC), allocatable :: l
    contains
        procedure, pass(self) :: initialize => THCHolder_initialize
    end type THCHolder

    type, abstract :: Abst_THC
        integer(int32) :: region_id
        real(real64) :: Material1 !! like a soil or a rock, a concrete
        real(real64) :: Material2 !! like a water
        real(real64) :: Material3 !! like a ice
        real(real64) :: Material4 !! like a gas
    contains
        procedure(Abst_Calc_THC_GaussPoint), pass(self), deferred :: Calc_GaussPoint !&
    end type Abst_THC

    !--------------------------------------------------------------------------------
    type, extends(Abst_THC) :: Type_THC_3Phase
    contains
        procedure, pass(self) :: Calc_GaussPoint => Calc_THC_GaussPoint_3Phase !&
    end type Type_THC_3Phase

    abstract interface
        function Abst_Calc_THC_GaussPoint(self, state) result(lambda)
            import :: Abst_THC, GaussPointState_t, real64
            implicit none
            class(Abst_THC), intent(in) :: self
            type(GaussPointState_t), intent(in) :: state
            real(real64) :: lambda

        end function Abst_Calc_THC_GaussPoint
    end interface

    interface
        module subroutine THCHolder_initialize(self, iRegion, Input)
            implicit none
            class(THCHolder), intent(inout) :: self
            integer(int32), intent(in) :: iRegion
            type(Type_Input), intent(in) :: Input

        end subroutine THCHolder_initialize

        module function Calc_THC_GaussPoint_3Phase(self, state) result(lambda)
            implicit none
            class(Type_THC_3Phase), intent(in) :: self
            type(GaussPointState_t), intent(in) :: state
            real(real64) :: lambda

        end function Calc_THC_GaussPoint_3Phase

        module function THC_3_Construct(iRegion, Input) result(Structure)
            implicit none
            class(Abst_THC), allocatable :: Structure
            integer(int32), intent(in) :: iRegion
            type(Type_Input), intent(in) :: Input

        end function THC_3_Construct
    end interface

    interface
        module function Calc_THC_3(lambda_soil, phi_soil, &
                                   lambda_water, phi_water, &
                                   lambda_ice, phi_ice) result(lambda)
            implicit none
            real(real64), intent(in) :: lambda_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: lambda_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: lambda_ice
            real(real64), intent(in) :: phi_ice
            real(real64) :: lambda

        end function Calc_THC_3
    end interface

    interface Type_THC_3Phase
        module procedure THC_3_Construct
    end interface
end module Calculate_ThermalConductivity

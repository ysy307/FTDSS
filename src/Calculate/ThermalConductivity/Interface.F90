module Calculate_ThermalConductivity
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Core_BaseTypes, only:GaussPointState_t
    use :: Core_Allocate, only:Allocate_Array
    use :: Inout_Input
    implicit none
    private

    public :: THCHolder, Abst_ThermalConductivity, Type_THC_3Phase

    type :: THCHolder
        class(Abst_ThermalConductivity), allocatable :: l
    contains
        procedure, pass(self) :: allocate => THCHolder_allocate
    end type THCHolder

    type, abstract :: Abst_ThermalConductivity
        integer(int32) :: region_id
        real(real64) :: Material1 !! like a soil or a rock, a concrete
        real(real64) :: Material2 !! like a water
        real(real64) :: Material3 !! like a ice
        real(real64) :: Material4 !! like a gas
    contains
        procedure(Abst_Calc_ThermalConductivity_GaussPoint), pass(self), deferred :: Calc_GaussPoint !&
    end type Abst_ThermalConductivity

    !--------------------------------------------------------------------------------
    type, extends(Abst_ThermalConductivity) :: Type_THC_3Phase
    contains
        procedure, pass(self) :: Calc_GaussPoint => Calc_GaussPoint_3Phase !&
    end type Type_THC_3Phase

    abstract interface
        function Abst_Calc_ThermalConductivity_GaussPoint(self, state) result(lambda)
            import :: Abst_ThermalConductivity, GaussPointState_t, real64
            implicit none
            class(Abst_ThermalConductivity), intent(in) :: self
            type(GaussPointState_t), intent(in) :: state
            real(real64) :: lambda

        end function Abst_Calc_ThermalConductivity_GaussPoint
    end interface

    interface
        module subroutine THCHolder_allocate(self, region_id, lambda1, lambda2, lambda3, lambda4)
            implicit none
            class(THCHolder), intent(inout) :: self
            integer(int32), intent(in) :: region_id
            real(real64), intent(in), optional :: lambda1
            real(real64), intent(in), optional :: lambda2
            real(real64), intent(in), optional :: lambda3
            real(real64), intent(in), optional :: lambda4

        end subroutine THCHolder_allocate

        module function Calc_GaussPoint_3Phase(self, state) result(lambda)
            implicit none
            class(Type_THC_3Phase), intent(in) :: self
            type(GaussPointState_t), intent(in) :: state
            real(real64) :: lambda

        end function Calc_GaussPoint_3Phase

        module function THC_3_Construct(region_id, lambda1, lambda2, lambda3, lambda4) result(structure)
            implicit none
            integer(int32), intent(in) :: region_id
            real(real64), intent(in), optional :: lambda1
            real(real64), intent(in), optional :: lambda2
            real(real64), intent(in), optional :: lambda3
            real(real64), intent(in), optional :: lambda4
            class(Abst_ThermalConductivity), allocatable :: structure

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

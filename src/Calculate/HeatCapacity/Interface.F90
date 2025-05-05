module Calculate_HeatCapacity
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Core_BaseTypes
    use :: Core_Allocate, only:Allocate_Array
    use :: Inout_Input
    use :: Calculate_Ice
    use :: Calculate_GCC
    use :: Calculate_Density
    implicit none
    !----------------------------------------------------------------------------------------------------
    ! Access settings
    !----------------------------------------------------------------------------------------------------
    private
    !----------------------------------------------------------------------------------------------------
    ! Public abstract types
    !----------------------------------------------------------------------------------------------------
    public :: Abstract_HeatCapacity
    !----------------------------------------------------------------------------------------------------
    ! Public types
    !----------------------------------------------------------------------------------------------------
    public :: Type_HeatCapacity_3Phase_Apparent
    !----------------------------------------------------------------------------------------------------

    type, abstract :: Abstract_HeatCapacity
        integer(int32) :: nsize
        integer(int32) :: nRegion
        real(real64), allocatable :: value(:, :)
    contains
        procedure(Abstract_Calculate_HeatCapacity), pass(self), deferred :: Calculate
        procedure(Abstract_Update_HeatCapacity), pass(self), deferred :: Update
    end type Abstract_HeatCapacity

    type, extends(Abstract_HeatCapacity) :: Type_HeatCapacity_3Phase_Apparent
        real(real64), allocatable :: soil(:)
        real(real64), allocatable :: water(:)
        real(real64), allocatable :: ice(:)
    contains
        procedure, pass(self) :: Calculate => Calc_HTC_3A_Wrap
        procedure, pass(self) :: Update => Update_HTC_3A
    end type Type_HeatCapacity_3Phase_Apparent
    !----------------------------------------------------------------------------------------------------

    abstract interface
        function Abstract_Calculate_HeatCapacity(self, NodeBelonging, phi1, phi2, phi3, phi4, &
                                                 Ice, Temperature, Density, Pw) result(HeatCapacity)
            import :: Abstract_HeatCapacity, Abstract_Ice, Belonging, real64, Abstract_Density
            implicit none
            class(Abstract_HeatCapacity), intent(in) :: self
            type(Belonging), intent(inout) :: NodeBelonging
            real(real64), intent(in), optional :: phi1
            real(real64), intent(in), optional :: phi2
            real(real64), intent(in), optional :: phi3
            real(real64), intent(in), optional :: phi4
            class(Abstract_Ice), intent(inout), optional :: Ice
            real(real64), intent(in), optional :: Temperature
            class(Abstract_Density), intent(inout), optional :: Density
            real(real64), intent(in), optional :: Pw
            real(real64) :: HeatCapacity

        end function Abstract_Calculate_HeatCapacity

        subroutine Abstract_Update_HeatCapacity(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4, &
                                                Ice, Temperature, Density, arr_Pw)
            import :: Abstract_HeatCapacity, Abstract_Ice, Belonging, real64, Abstract_Density
            implicit none
            class(Abstract_HeatCapacity), intent(inout) :: self
            type(Belonging), intent(inout) :: NodeBelonging(:)
            real(real64), intent(in), optional :: arr_phi1(:)
            real(real64), intent(in), optional :: arr_phi2(:)
            real(real64), intent(in), optional :: arr_phi3(:)
            real(real64), intent(in), optional :: arr_phi4(:)
            class(Abstract_Ice), intent(inout), optional :: Ice
            real(real64), intent(in), optional :: Temperature(:)
            class(Abstract_Density), intent(inout), optional :: Density
            real(real64), intent(in), optional :: arr_Pw(:)

        end subroutine Abstract_Update_HeatCapacity

    end interface

    interface
        module function Calc_HTC_3(NodeBelonging, HeatCapacity_soil, phi_soil, &
                                   HeatCapacity_water, phi_water, HeatCapacity_ice, phi_ice) result(HeatCapacity)
            implicit none
            type(Belonging), intent(inout) :: NodeBelonging
            real(real64), intent(in) :: HeatCapacity_soil(:)
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: HeatCapacity_water(:)
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: HeatCapacity_ice(:)
            real(real64), intent(in) :: phi_ice
            real(real64) :: HeatCapacity

        end function Calc_HTC_3

        module function Calc_HTC_3A(NodeBelonging, Cp, Ice, Temperature, Density, Pw) result(HeatCapacity)
            implicit none
            type(Belonging), intent(inout) :: NodeBelonging
            real(real64), intent(in) :: Cp
            class(Abstract_Ice), intent(inout), optional :: Ice
            real(real64), intent(in), optional :: Temperature
            class(Abstract_Density), intent(inout), optional :: Density
            real(real64), intent(in), optional :: Pw
            real(real64) :: HeatCapacity

        end function Calc_HTC_3A
    end interface

    interface
        module function HTC_3A_Construct(Input) result(Structure)
            implicit none
            class(Abstract_HeatCapacity), allocatable :: Structure
            type(Type_Input), intent(in) :: Input

        end function HTC_3A_Construct

        module function Calc_HTC_3A_Wrap(self, NodeBelonging, phi1, phi2, phi3, phi4, &
                                         Ice, Temperature, Density, Pw) result(HeatCapacity)
            implicit none
            class(Type_HeatCapacity_3Phase_Apparent), intent(in) :: self
            type(Belonging), intent(inout) :: NodeBelonging
            real(real64), intent(in), optional :: phi1
            real(real64), intent(in), optional :: phi2
            real(real64), intent(in), optional :: phi3
            real(real64), intent(in), optional :: phi4
            class(Abstract_Ice), intent(inout), optional :: Ice
            real(real64), intent(in), optional :: Temperature
            class(Abstract_Density), intent(inout), optional :: Density
            real(real64), intent(in), optional :: Pw
            real(real64) :: HeatCapacity

        end function Calc_HTC_3A_Wrap

        module subroutine Update_HTC_3A(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4, &
                                        Ice, Temperature, Density, arr_Pw)
            implicit none
            class(Type_HeatCapacity_3Phase_Apparent), intent(inout) :: self
            type(Belonging), intent(inout) :: NodeBelonging(:)
            real(real64), intent(in), optional :: arr_phi1(:)
            real(real64), intent(in), optional :: arr_phi2(:)
            real(real64), intent(in), optional :: arr_phi3(:)
            real(real64), intent(in), optional :: arr_phi4(:)
            class(Abstract_Ice), intent(inout), optional :: Ice
            real(real64), intent(in), optional :: Temperature(:)
            class(Abstract_Density), intent(inout), optional :: Density
            real(real64), intent(in), optional :: arr_Pw(:)

        end subroutine Update_HTC_3A
    end interface

    interface Type_HeatCapacity_3Phase_Apparent
        module procedure :: HTC_3A_Construct
    end interface

end module Calculate_HeatCapacity

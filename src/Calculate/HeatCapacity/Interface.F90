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
    public :: Abst_HeatCapacity
    public :: HTCHolder
    !----------------------------------------------------------------------------------------------------
    ! Public types
    !----------------------------------------------------------------------------------------------------
    public :: Type_HeatCapacity_3Phase_Apparent
    !----------------------------------------------------------------------------------------------------

    type :: HTCHolder
        class(Abst_HeatCapacity), allocatable :: c
    contains
        procedure, pass(self) :: allocate => HTCHolder_allocate
    end type HTCHolder

    type, abstract :: Abst_HeatCapacity
        real(real64) :: Material1 !! like a soil or a rock, a concrete
        real(real64) :: Material2 !! like a water
        real(real64) :: Material3 !! like a ice
        real(real64) :: Material4 !! like a gas
        ! integer(int32) :: nsize
        ! integer(int32) :: nRegion
        ! real(real64), allocatable :: value(:, :)
    contains
        procedure(Abstract_Calculate_HeatCapacity), pass(self), deferred :: Calc
        ! procedure(Abstract_Update_HeatCapacity), pass(self), deferred :: Update
    end type Abst_HeatCapacity

    type, extends(Abst_HeatCapacity) :: Type_HeatCapacity_3Phase_Apparent
        real(real64) :: soil
        real(real64) :: water
        real(real64) :: ice
    contains
        procedure, pass(self) :: Calc => Calc_HTC_3A_Wrap
        ! procedure, pass(self) :: Update => Update_HTC_3A
    end type Type_HeatCapacity_3Phase_Apparent
    !----------------------------------------------------------------------------------------------------

    abstract interface
        function Abstract_Calculate_HeatCapacity(self, phi, Temperature, Pw, Ice, Density) result(HeatCapacity)
            import :: Abst_HeatCapacity, Abstract_Ice, real64, Abstract_Density
            implicit none
            class(Abst_HeatCapacity), intent(in) :: self
            real(real64), intent(in) :: phi
            real(real64), intent(in) :: Temperature
            real(real64), intent(in), optional :: Pw
            class(Abstract_Ice), intent(inout), optional :: Ice
            class(Abstract_Density), intent(inout), optional :: Density
            real(real64) :: HeatCapacity

        end function Abstract_Calculate_HeatCapacity

        ! subroutine Abstract_Update_HeatCapacity(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4, &
        !                                         Ice, Temperature, Density, arr_Pw)
        !     import :: Abst_HeatCapacity, Abstract_Ice, Belonging, real64, Abstract_Density
        !     implicit none
        !     class(Abst_HeatCapacity), intent(inout) :: self
        !     type(Belonging), intent(inout) :: NodeBelonging(:)
        !     real(real64), intent(in), optional :: arr_phi1(:)
        !     real(real64), intent(in), optional :: arr_phi2(:)
        !     real(real64), intent(in), optional :: arr_phi3(:)
        !     real(real64), intent(in), optional :: arr_phi4(:)
        !     class(Abstract_Ice), intent(inout), optional :: Ice
        !     real(real64), intent(in), optional :: Temperature(:)
        !     class(Abstract_Density), intent(inout), optional :: Density
        !     real(real64), intent(in), optional :: arr_Pw(:)

        ! end subroutine Abstract_Update_HeatCapacity

    end interface

    interface
        module subroutine HTCHolder_allocate(self, iRegion, Input)
            implicit none
            class(HTCHolder), intent(inout) :: self
            integer(int32), intent(in) :: iRegion
            type(Type_Input), intent(in) :: Input

        end subroutine HTCHolder_allocate
    end interface

    interface
        module function Calc_HTC_3(HeatCapacity_soil, phi_soil, HeatCapacity_water, phi_water, HeatCapacity_ice, phi_ice) result(HeatCapacity)
            implicit none
            real(real64), intent(in) :: HeatCapacity_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: HeatCapacity_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: HeatCapacity_ice
            real(real64), intent(in) :: phi_ice
            real(real64) :: HeatCapacity

        end function Calc_HTC_3

        module function Calc_HTC_3A(Cp, Ice, Temperature, rho_ice, rho_water, Pw, phi) result(HeatCapacity)
            implicit none
            real(real64), intent(in) :: Cp
            class(Abstract_Ice), intent(inout), optional :: Ice
            real(real64), intent(in), optional :: Temperature
            real(real64), intent(in) :: rho_ice, rho_water
            real(real64), intent(in), optional :: Pw
            real(real64), intent(in), optional :: phi
            real(real64) :: HeatCapacity

        end function Calc_HTC_3A
    end interface

    interface
        module function HTC_3A_Construct(iRegion, Input) result(Structure)
            implicit none
            class(Abst_HeatCapacity), allocatable :: Structure
            integer(int32), intent(in) :: iRegion
            type(Type_Input), intent(in) :: Input

        end function HTC_3A_Construct

        module function Calc_HTC_3A_Wrap(self, phi, Temperature, Pw, Ice, Density) result(HeatCapacity)
            implicit none
            class(Type_HeatCapacity_3Phase_Apparent), intent(in) :: self
            real(real64), intent(in) :: phi
            real(real64), intent(in) :: Temperature
            real(real64), intent(in), optional :: Pw
            class(Abstract_Ice), intent(inout), optional :: Ice
            class(Abstract_Density), intent(inout), optional :: Density
            real(real64) :: HeatCapacity

        end function Calc_HTC_3A_Wrap

        ! module subroutine Update_HTC_3A(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4, &
        !                                 Ice, Temperature, Density, arr_Pw)
        !     implicit none
        !     class(Type_HeatCapacity_3Phase_Apparent), intent(inout) :: self
        !     type(Belonging), intent(inout) :: NodeBelonging(:)
        !     real(real64), intent(in), optional :: arr_phi1(:)
        !     real(real64), intent(in), optional :: arr_phi2(:)
        !     real(real64), intent(in), optional :: arr_phi3(:)
        !     real(real64), intent(in), optional :: arr_phi4(:)
        !     class(Abstract_Ice), intent(inout), optional :: Ice
        !     real(real64), intent(in), optional :: Temperature(:)
        !     class(Abstract_Density), intent(inout), optional :: Density
        !     real(real64), intent(in), optional :: arr_Pw(:)

        ! end subroutine Update_HTC_3A
    end interface

    interface Type_HeatCapacity_3Phase_Apparent
        module procedure :: HTC_3A_Construct
    end interface

end module Calculate_HeatCapacity

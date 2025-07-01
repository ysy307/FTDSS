module Calculate_Density
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Core_BaseTypes
    use :: Core_Allocate, only:Allocate_Array
    use :: Inout_Input
    use :: Calculate_Ice
    implicit none
    private

    public :: Abstract_Density
    public :: Type_Density_3Phase
    public :: DENHolder

    type :: DENHolder
        class(Abstract_Density), allocatable :: d
    contains
        procedure, pass(self) :: allocate => DENHolder_allocate
    end type DENHolder

    type, abstract :: Abstract_Density
        real(real64) :: Material1 !! like a soil or a rock, a concrete
        real(real64) :: Material2 !! like a water
        real(real64) :: Material3 !! like a ice
        real(real64) :: Material4 !! like a gas
    contains
        procedure(Abstract_Calculate_DEN), pass(self), deferred :: Calc
        ! procedure(Abstract_Update_Denstiy), pass(self), deferred :: Update
    end type Abstract_Density

    type, extends(Abstract_Density) :: Type_Density_3Phase
    contains
        procedure :: Calc => Calc_DEN_3_Wrap
        ! procedure :: Update => Update_DEN_3
    end type Type_Density_3Phase

    abstract interface
        function Abstract_Calculate_DEN(self, NodeBelonging, phi1, phi2, phi3, phi4) result(Denstiy)
            import :: Abstract_Density, Belonging, real64
            implicit none
            class(Abstract_Density), intent(in) :: self
            type(Belonging), intent(inout) :: NodeBelonging
            real(real64), intent(in), optional :: phi1
            real(real64), intent(in), optional :: phi2
            real(real64), intent(in), optional :: phi3
            real(real64), intent(in), optional :: phi4
            real(real64) :: Denstiy

        end function Abstract_Calculate_DEN

        subroutine Abstract_Update_Denstiy(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4)
            import :: Abstract_Density, Belonging, DP3d, real64
            implicit none
            class(Abstract_Density), intent(inout) :: self
            type(Belonging), intent(inout) :: NodeBelonging(:)
            real(real64), intent(in), optional :: arr_phi1(:)
            real(real64), intent(in), optional :: arr_phi2(:)
            real(real64), intent(in), optional :: arr_phi3(:)
            real(real64), intent(in), optional :: arr_phi4(:)

        end subroutine Abstract_Update_Denstiy
    end interface

    interface
        module subroutine DENHolder_allocate(self, iRegion, Input)
            implicit none
            class(DENHolder), intent(inout) :: self
            integer(int32), intent(in) :: iRegion
            type(Type_Input), intent(in) :: Input

        end subroutine DENHolder_allocate
    end interface

    interface
        module function Calc_DEN_3(density_soil, phi_soil, &
                                   density_water, phi_water, density_ice, phi_ice) result(density)
            implicit none
            real(real64), intent(in) :: density_soil
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: density_water
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: density_ice
            real(real64), intent(in) :: phi_ice
            real(real64) :: density

        end function Calc_DEN_3
    end interface

    !--------------------------------------------------------------------------------
    ! 3-phase density calculation interface
    !--------------------------------------------------------------------------------
    interface
        module function DEN_3_Construct(iRegion, Input) result(Structure)
            implicit none
            class(Abstract_Density), allocatable :: Structure
            integer(int32), intent(in) :: iRegion
            type(Type_Input), intent(in) :: Input

        end function DEN_3_Construct

        module function Calc_DEN_3_Wrap(self, phi, Temperature, Pw, Ice) result(density)
            implicit none
            class(Type_Density_3Phase), intent(in) :: self
            real(real64), intent(in) :: phi
            real(real64), intent(in) :: Temperature
            real(real64), intent(in), optional :: Pw
            class(Abstract_Ice), intent(inout), optional :: Ice
            real(real64) :: density

        end function Calc_DEN_3_Wrap

        ! module subroutine Update_DEN_3(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4)
        !     implicit none
        !     class(Type_Density_3Phase), intent(inout) :: self
        !     type(Belonging), intent(inout) :: NodeBelonging(:)
        !     real(real64), intent(in), optional :: arr_phi1(:)
        !     real(real64), intent(in), optional :: arr_phi2(:)
        !     real(real64), intent(in), optional :: arr_phi3(:)
        !     real(real64), intent(in), optional :: arr_phi4(:)

        ! end subroutine Update_DEN_3

    end interface

    interface Type_Density_3Phase
        module procedure :: DEN_3_Construct
    end interface

end module Calculate_Density

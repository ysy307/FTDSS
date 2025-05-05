module Calculate_SpecificHeat
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Core_BaseTypes
    use :: Core_Allocate, only:Allocate_Array
    use :: Inout_Input
    implicit none
    private

    public :: Abstract_SpecificHeat
    public :: Type_SpecificHeat_3Phase

    type, abstract :: Abstract_SpecificHeat
        integer(int32) :: nsize
        integer(int32) :: nRegion
        real(real64), allocatable :: value(:, :)
    contains
        procedure(Abstract_Calculate_SpecificHeat), pass(self), deferred :: Calculate
        procedure(Abstract_Update_SpecificHeat), pass(self), deferred :: Update
    end type Abstract_SpecificHeat

    type, extends(Abstract_SpecificHeat) :: Type_SpecificHeat_3Phase
        real(real64), allocatable :: soil(:)
        real(real64), allocatable :: water(:)
        real(real64), allocatable :: ice(:)
    contains
        procedure :: Calculate => Calc_SPH_3_Wrap
        procedure :: Update => Update_SPH_3
    end type Type_SpecificHeat_3Phase

    abstract interface
        function Abstract_Calculate_SpecificHeat(self, NodeBelonging, phi1, phi2, phi3, phi4) result(SpecificHeat)
            import :: Abstract_SpecificHeat, Belonging, real64
            implicit none
            class(Abstract_SpecificHeat), intent(in) :: self
            type(Belonging), intent(inout) :: NodeBelonging
            real(real64), intent(in), optional :: phi1
            real(real64), intent(in), optional :: phi2
            real(real64), intent(in), optional :: phi3
            real(real64), intent(in), optional :: phi4
            real(real64) :: SpecificHeat

        end function Abstract_Calculate_SpecificHeat

        subroutine Abstract_Update_SpecificHeat(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4)
            import :: Abstract_SpecificHeat, Belonging, DP3d, real64
            implicit none
            class(Abstract_SpecificHeat), intent(inout) :: self
            type(Belonging), intent(inout) :: NodeBelonging(:)
            real(real64), intent(in), optional :: arr_phi1(:)
            real(real64), intent(in), optional :: arr_phi2(:)
            real(real64), intent(in), optional :: arr_phi3(:)
            real(real64), intent(in), optional :: arr_phi4(:)

        end subroutine Abstract_Update_SpecificHeat

    end interface

    interface
        module function Calc_SPH_3(NodeBelonging, SpecificHeat_soil, phi_soil, &
                                   SpecificHeat_water, phi_water, SpecificHeat_ice, phi_ice) result(SpecificHeat)
            implicit none
            type(Belonging), intent(inout) :: NodeBelonging
            real(real64), intent(in) :: SpecificHeat_soil(:)
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: SpecificHeat_water(:)
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: SpecificHeat_ice(:)
            real(real64), intent(in) :: phi_ice
            real(real64) :: SpecificHeat

        end function Calc_SPH_3
    end interface

    !--------------------------------------------------------------------------------
    ! 3-phase SpecificHeat calculation interface
    !--------------------------------------------------------------------------------
    interface
        module function SPH_3_Construct(Input) result(Structure)
            implicit none
            type(Type_Input), intent(in) :: Input
            class(Abstract_SpecificHeat), allocatable :: Structure

        end function SPH_3_Construct

        module function Calc_SPH_3_Wrap(self, NodeBelonging, phi1, phi2, phi3, phi4) result(SpecificHeat)
            implicit none
            class(Type_SpecificHeat_3Phase), intent(in) :: self
            type(Belonging), intent(inout) :: NodeBelonging
            real(real64), intent(in), optional :: phi1
            real(real64), intent(in), optional :: phi2
            real(real64), intent(in), optional :: phi3
            real(real64), intent(in), optional :: phi4
            real(real64) :: SpecificHeat

        end function Calc_SPH_3_Wrap

        module subroutine Update_SPH_3(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4)
            implicit none
            class(Type_SpecificHeat_3Phase), intent(inout) :: self
            type(Belonging), intent(inout) :: NodeBelonging(:)
            real(real64), intent(in), optional :: arr_phi1(:)
            real(real64), intent(in), optional :: arr_phi2(:)
            real(real64), intent(in), optional :: arr_phi3(:)
            real(real64), intent(in), optional :: arr_phi4(:)

        end subroutine Update_SPH_3

    end interface

    interface Type_SpecificHeat_3Phase
        module procedure :: SPH_3_Construct
    end interface

end module Calculate_SpecificHeat

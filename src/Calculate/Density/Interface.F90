module Calculate_Density
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Core_BaseTypes
    use :: Core_Allocate, only:Allocate_Array
    use :: Inout_Input
    implicit none
    private

    public :: Abstract_Density
    public :: Type_Density_3Phase

    type, abstract :: Abstract_Density
        integer(int32) :: nsize
        integer(int32) :: numRegion
        real(real64), allocatable :: value(:, :)
    contains
        procedure(Abstract_Calculate_DEN), pass(self), deferred :: Calculate
        procedure(Abstract_Update_Denstiy), pass(self), deferred :: Update
    end type Abstract_Density

    type, extends(Abstract_Density) :: Type_Density_3Phase
        real(real64), allocatable :: soil(:)
        real(real64), allocatable :: water(:)
        real(real64), allocatable :: ice(:)
    contains
        procedure :: Calculate => Calc_DEN_3_Wrap
        procedure :: Update => Update_DEN_3
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
        module function Calc_DEN_3(NodeBelonging, density_soil, phi_soil, &
                                   density_water, phi_water, density_ice, phi_ice) result(density)
            implicit none
            type(Belonging), intent(inout) :: NodeBelonging
            real(real64), intent(in) :: density_soil(:)
            real(real64), intent(in) :: phi_soil
            real(real64), intent(in) :: density_water(:)
            real(real64), intent(in) :: phi_water
            real(real64), intent(in) :: density_ice(:)
            real(real64), intent(in) :: phi_ice
            real(real64) :: density

        end function Calc_DEN_3
    end interface

    !--------------------------------------------------------------------------------
    ! 3-phase density calculation interface
    !--------------------------------------------------------------------------------
    interface
        module function DEN_3_Construct(Input) result(Structure)
            implicit none
            type(Type_Input), intent(in) :: Input
            class(Abstract_Density), allocatable :: Structure

        end function DEN_3_Construct

        module function Calc_DEN_3_Wrap(self, NodeBelonging, phi1, phi2, phi3, phi4) result(Denstiy)
            implicit none
            class(Type_Density_3Phase), intent(in) :: self
            type(Belonging), intent(inout) :: NodeBelonging
            real(real64), intent(in), optional :: phi1
            real(real64), intent(in), optional :: phi2
            real(real64), intent(in), optional :: phi3
            real(real64), intent(in), optional :: phi4
            real(real64) :: Denstiy

        end function Calc_DEN_3_Wrap

        module subroutine Update_DEN_3(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4)
            implicit none
            class(Type_Density_3Phase), intent(inout) :: self
            type(Belonging), intent(inout) :: NodeBelonging(:)
            real(real64), intent(in), optional :: arr_phi1(:)
            real(real64), intent(in), optional :: arr_phi2(:)
            real(real64), intent(in), optional :: arr_phi3(:)
            real(real64), intent(in), optional :: arr_phi4(:)

        end subroutine Update_DEN_3

    end interface

    interface Type_Density_3Phase
        module procedure :: DEN_3_Construct
    end interface

end module Calculate_Density

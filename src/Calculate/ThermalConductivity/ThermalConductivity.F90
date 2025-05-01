module Calculate_ThermalConductivity
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Core_BaseTypes
    use :: Core_Allocate, only:Allocate_Array
    use :: Inout_Input
    use :: Calculate_Ice
    implicit none
    private

    public :: Abstract_ThermalConductivity
    public :: Type_ThermalConductivity_3Phase

    type, abstract :: Abstract_ThermalConductivity
        integer(int32) :: nsize
        integer(int32) :: numRegion
        real(real64), allocatable :: value(:, :)
    contains
        procedure(Abstract_Calculate_ThermalConductivity), deferred :: Calculate
        procedure(Abstract_Update_ThermalConductivity), deferred :: Update
    end type Abstract_ThermalConductivity

    ! type, extends(Abstract_ThermalConductivity) :: Type_ThermalConductivity_1Phase
    !     real(real64) :: lambda1
    ! end type Type_ThermalConductivity_1Phase

    ! type, extends(Abstract_ThermalConductivity) :: Type_ThermalConductivity_2Phase
    !     real(real64) :: lambda1
    !     real(real64) :: lambda2
    ! end type Type_ThermalConductivity_2Phase

    type, extends(Abstract_ThermalConductivity) :: Type_ThermalConductivity_3Phase
        real(real64), allocatable :: soil(:)
        real(real64), allocatable :: water(:)
        real(real64), allocatable :: ice(:)
    contains
        procedure :: Calculate => Calculate_ThermalConductivity_3Phase_Wrap
        procedure :: Update => Update_ThermalConductivity_3Phase
    end type Type_ThermalConductivity_3Phase

    ! type, extends(Abstract_ThermalConductivity) :: Type_ThermalConductivity_3Phase_Dispersed_2D
    !     real(real64) :: soil
    !     real(real64) :: water
    !     real(real64) :: ice
    !     real(real64) :: Longitude
    !     real(real64) :: Transverse

    ! end type Type_ThermalConductivity_3Phase_Dispersed_2D

    ! type, extends(Abstract_ThermalConductivity) :: Type_ThermalConductivity_3Phase_Dispersed_3D
    !     real(real64) :: soil
    !     real(real64) :: water
    !     real(real64) :: ice
    !     real(real64) :: Longitude
    !     real(real64) :: Transverse
    ! end type Type_ThermalConductivity_3Phase_Dispersed_3D

    ! type, extends(Abstract_ThermalConductivity) :: Type_ThermalConductivity_4Phase
    !     real(real64) :: soil
    !     real(real64) :: water
    !     real(real64) :: ice
    !     real(real64) :: air
    ! end type Type_ThermalConductivity_4Phase

    ! type, extends(Abstract_ThermalConductivity) :: Type_ThermalConductivity_4Phase_Dispersed_2D
    !     real(real64) :: soil
    !     real(real64) :: water
    !     real(real64) :: ice
    !     real(real64) :: air
    !     real(real64) :: Longitude
    !     real(real64) :: Transverse
    ! end type Type_ThermalConductivity_4Phase_Dispersed_2D

    ! type, extends(Abstract_ThermalConductivity) :: Type_ThermalConductivity_4Phase_Dispersed_3D
    !     real(real64) :: soil
    !     real(real64) :: water
    !     real(real64) :: ice
    !     real(real64) :: air
    !     real(real64) :: Longitude
    !     real(real64) :: Transverse
    ! end type Type_ThermalConductivity_4Phase_Dispersed_3D

    abstract interface
        function Abstract_Calculate_ThermalConductivity(self, NodeBelonging, phi1, phi2, phi3, phi4, waterFlux) result(lambda)
            import :: Abstract_ThermalConductivity, Belonging, real64
            implicit none
            class(Abstract_ThermalConductivity), intent(in) :: self
            type(Belonging), intent(in) :: NodeBelonging
            real(real64), intent(in), optional :: phi1
            real(real64), intent(in), optional :: phi2
            real(real64), intent(in), optional :: phi3
            real(real64), intent(in), optional :: phi4
            real(real64), intent(in), optional :: waterFlux(:)
            real(real64) :: lambda

        end function Abstract_Calculate_ThermalConductivity

        subroutine Abstract_Update_ThermalConductivity(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4, waterFlux)
            import :: Abstract_ThermalConductivity, Belonging, DP3d, real64
            implicit none
            class(Abstract_ThermalConductivity), intent(inout) :: self
            type(Belonging), intent(in) :: NodeBelonging(:)
            real(real64), intent(in), optional :: arr_phi1(:)
            real(real64), intent(in), optional :: arr_phi2(:)
            real(real64), intent(in), optional :: arr_phi3(:)
            real(real64), intent(in), optional :: arr_phi4(:)
            type(DP3d), intent(in), optional :: waterFlux

        end subroutine Abstract_Update_ThermalConductivity

    end interface

    !--------------------------------------------------------------------------------
    ! 3-phase thermal conductivity calculation interface
    !--------------------------------------------------------------------------------
    interface
        module function ThermalConductivity_3Phase_Construct(Input) result(Structure)
            implicit none
            type(Type_Input), intent(in) :: Input
            class(Abstract_ThermalConductivity), allocatable :: Structure

        end function ThermalConductivity_3Phase_Construct

        module function Calculate_ThermalConductivity_3Phase_Wrap(self, NodeBelonging, phi1, phi2, phi3, phi4, waterFlux) result(lambda)
            implicit none
            class(Type_ThermalConductivity_3Phase), intent(in) :: self
            type(Belonging), intent(in) :: NodeBelonging
            real(real64), intent(in), optional :: phi1
            real(real64), intent(in), optional :: phi2
            real(real64), intent(in), optional :: phi3
            real(real64), intent(in), optional :: phi4
            real(real64), intent(in), optional :: waterFlux(:)
            real(real64) :: lambda

        end function Calculate_ThermalConductivity_3Phase_Wrap

        module subroutine Update_ThermalConductivity_3Phase(self, NodeBelonging, arr_phi1, arr_phi2, arr_phi3, arr_phi4, waterFlux)
            implicit none
            class(Type_ThermalConductivity_3Phase), intent(inout) :: self
            type(Belonging), intent(in) :: NodeBelonging(:)
            real(real64), intent(in), optional :: arr_phi1(:)
            real(real64), intent(in), optional :: arr_phi2(:)
            real(real64), intent(in), optional :: arr_phi3(:)
            real(real64), intent(in), optional :: arr_phi4(:)
            type(DP3d), intent(in), optional :: waterFlux

        end subroutine Update_ThermalConductivity_3Phase

    end interface

    interface Type_ThermalConductivity_3Phase
        module procedure :: ThermalConductivity_3Phase_Construct
    end interface

    ! interface Type_ThermalConductivity_3Phase
    !     procedure :: ThermalConductivity_3Phase_Construct
    ! end interface Type_ThermalConductivity_3Phase

contains

    ! function Calculate_ThermalConductivity_3Phase(lambda_soil, phi_soil, lambda_water, phi_water, lambda_ice, phi_ice) result(lambda)
    !     implicit none
    !     real(real64), intent(in) :: lambda_soil !! Volumetric heat capacity of soil
    !     real(real64), intent(in) :: phi_soil !! the ratio of soil
    !     real(real64), intent(in) :: lambda_water !! Volumetric heat capacity of water
    !     real(real64), intent(in) :: phi_water !! the ratio of water
    !     real(real64), intent(in) :: lambda_ice !! Volumetric heat capacity of ice
    !     real(real64), intent(in) :: phi_ice !! the ratio of ice
    !     real(real64) :: lambda

    !     lambda = phi_soil * lambda_soil + phi_water * lambda_water + phi_Ice * lambda_ice
    ! end function Calculate_ThermalConductivity_3Phase

    ! function Calculate_ThermalConductivity_3Phase_Wrap(self, phi1, phi2, phi3, phi4) result(lambda)
    !     implicit none
    !     class(Type_ThermalConductivity_3Phase), intent(in) :: self
    !     real(real64), intent(in), optional :: phi1 !! the ratio of material 1
    !     real(real64), intent(in), optional :: phi2 !! the ratio of material 2
    !     real(real64), intent(in), optional :: phi3 !! the ratio of material 3
    !     real(real64), intent(in), optional :: phi4 !! the ratio of material 4
    !     real(real64) :: lambda

    !     lambda = Calculate_ThermalConductivity_3Phase(self%soil, phi1, self%water, phi2, self%ice, phi3)
    ! end function Calculate_ThermalConductivity_3Phase_Wrap

    ! subroutine Update_ThermalConductivity_3Phase_Scalar(self, phi_soil, arr_Qw, arr_Qice)
    !     implicit none
    !     class(Type_ThermalConductivity_3Phase), intent(inout) :: self
    !     real(real64), intent(in) :: phi_soil !! the ratio of soil
    !     real(real64), intent(in) :: arr_Qw(:) !! the ratio of water
    !     real(real64), intent(in) :: arr_Qice(:) !! the ratio of ice

    !     integer(int32) :: iN

    !     !$omp parallel do private(iN)
    !     do iN = 1, self%nsize
    !         self%value(iN) = self%soil**(1.0d0 - phi_soil) &
    !                          * self%water**arr_Qw(iN) &
    !                          * self%ice**arr_Qice(iN)
    !     end do
    !     !$omp end parallel do
    ! end subroutine Update_ThermalConductivity_3Phase_Scalar

    ! subroutine Update_ThermalConductivity_3Phase_Array(self, arr_phi_soil, arr_Qw, arr_Qice)
    !     implicit none
    !     class(Type_ThermalConductivity_3Phase), intent(inout) :: self
    !     real(real64), intent(in) :: arr_phi_soil(:) !! the ratio of soil
    !     real(real64), intent(in) :: arr_Qw(:) !! the ratio of water
    !     real(real64), intent(in) :: arr_Qice(:) !! the ratio of ice

    !     integer(int32) :: iN

    !     !$omp parallel do private(iN)
    !     do iN = 1, self%nsize
    !         self%value(iN) = self%soil**(1.0d0 - arr_phi_soil(iN)) &
    !                          * self%water**arr_Qw(iN) &
    !                          * self%ice**arr_Qice(iN)
    !     end do
    !     !$omp end parallel do
    ! end subroutine Update_ThermalConductivity_3Phase_Array

end module Calculate_ThermalConductivity

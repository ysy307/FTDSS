module Calculate_ThermalConductivity
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Core_Allocate, only:Allocate_Array
    use :: Calculate_Ice
    implicit none

    type, abstract :: Abstract_ThermalConductivity
        integer(int32) :: nsize
        real(real64), allocatable :: value(:)
    end type Abstract_ThermalConductivity

    type, extends(Abstract_ThermalConductivity) :: Type_ThermalConductivity_1Phase
        real(real64) :: lambda1
    end type Type_ThermalConductivity_1Phase

    type, extends(Abstract_ThermalConductivity) :: Type_ThermalConductivity_2Phase
        real(real64) :: lambda1
        real(real64) :: lambda2
    end type Type_ThermalConductivity_2Phase

    type, extends(Abstract_ThermalConductivity) :: Type_ThermalConductivity_3Phase
        real(real64) :: soil
        real(real64) :: water
        real(real64) :: ice
    contains
        procedure :: Calculate => Calculate_ThermalConductivity_3Phase_Wrap
        procedure :: Update_ThermalConductivity_3Phase_Scalar
        procedure :: Update_ThermalConductivity_3Phase_Array
        generic :: Update => Update_ThermalConductivity_3Phase_Scalar, & !&
                             Update_ThermalConductivity_3Phase_Array
    end type Type_ThermalConductivity_3Phase

    type, extends(Abstract_ThermalConductivity) :: Type_ThermalConductivity_3Phase_Dispersed_2D
        real(real64) :: soil
        real(real64) :: water
        real(real64) :: ice
        real(real64) :: Longitude
        real(real64) :: Transverse

    end type Type_ThermalConductivity_3Phase_Dispersed_2D

    type, extends(Abstract_ThermalConductivity) :: Type_ThermalConductivity_3Phase_Dispersed_3D
        real(real64) :: soil
        real(real64) :: water
        real(real64) :: ice
        real(real64) :: Longitude
        real(real64) :: Transverse
    end type Type_ThermalConductivity_3Phase_Dispersed_3D

    type, extends(Abstract_ThermalConductivity) :: Type_ThermalConductivity_4Phase
        real(real64) :: soil
        real(real64) :: water
        real(real64) :: ice
        real(real64) :: air
    end type Type_ThermalConductivity_4Phase

    type, extends(Abstract_ThermalConductivity) :: Type_ThermalConductivity_4Phase_Dispersed_2D
        real(real64) :: soil
        real(real64) :: water
        real(real64) :: ice
        real(real64) :: air
        real(real64) :: Longitude
        real(real64) :: Transverse
    end type Type_ThermalConductivity_4Phase_Dispersed_2D

    type, extends(Abstract_ThermalConductivity) :: Type_ThermalConductivity_4Phase_Dispersed_3D
        real(real64) :: soil
        real(real64) :: water
        real(real64) :: ice
        real(real64) :: air
        real(real64) :: Longitude
        real(real64) :: Transverse
    end type Type_ThermalConductivity_4Phase_Dispersed_3D

    interface Type_ThermalConductivity_3Phase
        module procedure Construct_ThermalConductivity_3Phase
    end interface Type_ThermalConductivity_3Phase

contains
    !----------------------------------------------------------------------------------------------------
    ! Constructe each type of heat conductivity
    !----------------------------------------------------------------------------------------------------
    function Construct_ThermalConductivity_3Phase(lambad_soil, lambad_water, lambad_ice, nsize) result(structure)
        use :: Allocate_Allocate, only:Allocate_Array
        implicit none
        real(real64), intent(in) :: lambad_soil
        real(real64), intent(in) :: lambad_water
        real(real64), intent(in) :: lambad_ice
        integer(int32), intent(in) :: nsize
        type(Type_ThermalConductivity_3Phase) :: structure

        structure%soil = lambad_soil
        structure%water = lambad_water
        structure%ice = lambad_ice

        structure%nsize = nsize

        call Allocate_Array(structure%value, nsize)
        structure%value(:) = 0.0d0

    end function Construct_ThermalConductivity_3Phase

    function Calculate_ThermalConductivity_3Phase(lambda_soil, phi_soil, lambda_water, phi_water, lambda_ice, phi_ice) result(lambda)
        implicit none
        real(real64), intent(in) :: lambda_soil !! Volumetric heat capacity of soil
        real(real64), intent(in) :: phi_soil !! the ratio of soil
        real(real64), intent(in) :: lambda_water !! Volumetric heat capacity of water
        real(real64), intent(in) :: phi_water !! the ratio of water
        real(real64), intent(in) :: lambda_ice !! Volumetric heat capacity of ice
        real(real64), intent(in) :: phi_ice !! the ratio of ice
        real(real64) :: lambda

        lambda = phi_soil * lambda_soil + phi_water * lambda_water + phi_Ice * lambda_ice
    end function Calculate_ThermalConductivity_3Phase

    function Calculate_ThermalConductivity_3Phase_Wrap(self, phi1, phi2, phi3, phi4) result(lambda)
        implicit none
        class(Type_ThermalConductivity_3Phase), intent(in) :: self
        real(real64), intent(in), optional :: phi1 !! the ratio of material 1
        real(real64), intent(in), optional :: phi2 !! the ratio of material 2
        real(real64), intent(in), optional :: phi3 !! the ratio of material 3
        real(real64), intent(in), optional :: phi4 !! the ratio of material 4
        real(real64) :: lambda

        lambda = Calculate_ThermalConductivity_3Phase(self%soil, phi1, self%water, phi2, self%ice, phi3)
    end function Calculate_ThermalConductivity_3Phase_Wrap

    subroutine Update_ThermalConductivity_3Phase_Scalar(self, phi_soil, arr_Qw, arr_Qice)
        implicit none
        class(Type_ThermalConductivity_3Phase), intent(inout) :: self
        real(real64), intent(in) :: phi_soil !! the ratio of soil
        real(real64), intent(in) :: arr_Qw(:) !! the ratio of water
        real(real64), intent(in) :: arr_Qice(:) !! the ratio of ice

        integer(int32) :: iN

        !$omp parallel do private(iN)
        do iN = 1, self%nsize
            self%value(iN) = self%soil**(1.0d0 - phi_soil) &
                             * self%water**arr_Qw(iN) &
                             * self%ice**arr_Qice(iN)
        end do
        !$omp end parallel do
    end subroutine Update_ThermalConductivity_3Phase_Scalar

    subroutine Update_ThermalConductivity_3Phase_Array(self, arr_phi_soil, arr_Qw, arr_Qice)
        implicit none
        class(Type_ThermalConductivity_3Phase), intent(inout) :: self
        real(real64), intent(in) :: arr_phi_soil(:) !! the ratio of soil
        real(real64), intent(in) :: arr_Qw(:) !! the ratio of water
        real(real64), intent(in) :: arr_Qice(:) !! the ratio of ice

        integer(int32) :: iN

        !$omp parallel do private(iN)
        do iN = 1, self%nsize
            self%value(iN) = self%soil**(1.0d0 - arr_phi_soil(iN)) &
                             * self%water**arr_Qw(iN) &
                             * self%ice**arr_Qice(iN)
        end do
        !$omp end parallel do
    end subroutine Update_ThermalConductivity_3Phase_Array

end module Calculate_ThermalConductivity

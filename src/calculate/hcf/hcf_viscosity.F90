submodule(Calculate_HCF) Calculate_HCF_Viscosity_Implementation
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Constructe the type
    !----------------------------------------------------------------------------------------------------
    module function Construct_Type_HCF_Viscosity(Ks, useViscosity, nsize) result(structure_HCF)
        implicit none
        real(real64), intent(in) :: Ks
        integer(int32), intent(in) :: useViscosity
        integer(int32), intent(in) :: nsize
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Viscosity :: structure_HCF)

        select type (this => structure_HCF)
        type is (Type_HCF_Viscosity)
            this%Ks = Ks
            this%nsize = nsize

            call this%Set_Calculate_Viscosity(useViscosity, this%Calculate_Viscosity)
            this%Kzero = this%Ks * this%Calculate_Viscosity(15.d0)

            call Allocate_Array(this%Kflh, nsize)
            this%Kflh(:) = 0.0d0
        end select

    end function Construct_Type_HCF_Viscosity

    module function Construct_Type_HCF_Viscosity_minimal() result(structure_HCF)
        implicit none
        class(Abstract_HCF), allocatable :: structure_HCF

        if (allocated(structure_HCF)) deallocate (structure_HCF)
        allocate (Type_HCF_Viscosity :: structure_HCF)

    end function Construct_Type_HCF_Viscosity_minimal

    !----------------------------------------------------------------------------------------------------
    ! Associate the function to calculate the water viscosity
    !----------------------------------------------------------------------------------------------------
    module subroutine Set_Calculate_Viscosity_Base(Calculate_Viscosity_Type, Calculate_Viscosity)
        implicit none
        integer(int32), intent(in) :: Calculate_Viscosity_Type
        procedure(Abstract_Calculate_Viscosity), pointer, intent(inout) :: Calculate_Viscosity

        select case (Calculate_Viscosity_Type)
        case (1)
            Calculate_Viscosity => Calculate_HCF_mu_Exponential
        case (2)
            Calculate_Viscosity => Calculate_HCF_mu_Exponential_Supercooled
        case default
            Calculate_Viscosity => null()
        end select

    end subroutine Set_Calculate_Viscosity_Base

    !----------------------------------------------------------------------------------------------------
    ! Calculate water viscosity depending on the temperature
    !----------------------------------------------------------------------------------------------------
    module function Calculate_HCF_mu_Exponential(Temperature) result(Viscosity)
        !$omp declare simd uniform(Temperature)
        implicit none
        real(real64), intent(in) :: Temperature
        real(real64) :: Viscosity

        Viscosity = 2.1d-6 * exp(1808.5d0 / (Temperature + 273.15d0))

    end function Calculate_HCF_mu_Exponential

    module function Calculate_HCF_mu_Exponential_Supercooled(Temperature) result(Viscosity)
        !$omp declare simd uniform(Temperature)
        implicit none
        real(real64), intent(in) :: Temperature
        real(real64) :: Viscosity

        Viscosity = 1.3788d-4 * ((273.15d0 + Temperature) / 225.66d0 - 1.0d0)**(-1.6438d0)

    end function Calculate_HCF_mu_Exponential_Supercooled

    !----------------------------------------------------------------------------------------------------
    ! Calculate Kflh using the water viscosity
    !----------------------------------------------------------------------------------------------------
    module function Calculate_Kflh_Viscosity(self, Temperature) result(Kflh)
        implicit none
        class(Type_HCF_Viscosity), intent(in) :: self
        real(real64), intent(in) :: Temperature
        real(real64) :: Kflh

        Kflh = self%kzero / self%Calculate_Viscosity(Temperature)

    end function Calculate_Kflh_Viscosity

    !----------------------------------------------------------------------------------------------------
    ! Update Kflh using the water viscosity
    !----------------------------------------------------------------------------------------------------
    module subroutine Update_Kflh_Viscosity(self, arr_Temperature)
        implicit none
        class(Type_HCF_Viscosity), intent(inout) :: self
        real(real64), intent(in) :: arr_Temperature(:)

        integer(int32) :: iN

        !$omp parallel do schedule(guided) private(iN)
        do iN = 1, self%nsize
            self%Kflh(iN) = self%Calculate_Kflh(arr_Temperature(iN))
        end do

    end subroutine Update_Kflh_Viscosity

end submodule Calculate_HCF_Viscosity_Implementation

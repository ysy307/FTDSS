submodule(Calculate_HCF) Calculate_HCF_Viscosity_Implementation
    implicit none
contains
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

end submodule Calculate_HCF_Viscosity_Implementation

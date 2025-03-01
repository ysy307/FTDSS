submodule(Calculate_HCF) Calculate_HCF_Impedance_Implementation
    implicit none
contains
    module function Calculate_Impedance_Base(Omega, thetaI) result(Impedance)
        !$omp declare simd uniform(Omega, thetaI)
        implicit none
        real(real64), intent(in) :: Omega
        real(real64), intent(in) :: thetaI
        real(real64) :: Impedance

        Impedance = 10.0d0**(-Omega * thetaI)

    end function Calculate_Impedance_Base

end submodule Calculate_HCF_Impedance_Implementation

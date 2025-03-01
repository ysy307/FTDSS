submodule(Calculate_HCF) Calculate_HCF_MVG_Implementation
    implicit none
contains
    module function Calculate_kr_MVG_Base(thetaS, thetaR, alpha1, n1, m1, l, hcrit, h) result(kr)
        !$omp declare simd uniform(thetaS, thetaR, alpha1, n1, m1, l, hcrit, h)
        implicit none
        real(real64), intent(in) :: thetaS
        real(real64), intent(in) :: thetaR
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: m1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: hcrit
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: Sw, thetaM

        thetaM = thetaR + (thetaS - thetaR) * (1.0d0 + (-alpha1 * hcrit)**n1)**(-m1)

        if (h < hcrit) then
            Sw = (thetaS - thetaR) / (thetaM - thetaR) * (1.0d0 + abs(alpha1 * h)**n1)**(-m1)
            kr = Sw**l * ((1.0d0 - (1.0d0 - Sw**(1.0d0 / m1))**m1) / (1.0d0 - (1.0d0 - 1.0d0**(1.0d0 / m1))**m1))**2.0d0
        else
            kr = 1.0d0
        end if

    end function Calculate_kr_MVG_Base

    module function Calculate_kr_Base_MVG(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_MVG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_MVG_Base(self%thetaS, self%thetaR, self%alpha1, self%n1, self%m1, self%l, self%hcrit, h)

    end function Calculate_kr_Base_MVG

    module function Calculate_kr_Base_Impedance_MVG(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Impedance_MVG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_MVG_Base(self%thetaS, self%thetaR, self%alpha1, self%n1, self%m1, self%l, self%hcrit, h)

    end function Calculate_kr_Base_Impedance_MVG

    module function Calculate_kr_Base_Viscosity_MVG(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Viscosity_MVG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_MVG_Base(self%thetaS, self%thetaR, self%alpha1, self%n1, self%m1, self%l, self%hcrit, h)

    end function Calculate_kr_Base_Viscosity_MVG

    module function Calculate_kr_Base_Impedance_Viscosity_MVG(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Impedance_Viscosity_MVG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_MVG_Base(self%thetaS, self%thetaR, self%alpha1, self%n1, self%m1, self%l, self%hcrit, h)

    end function Calculate_kr_Base_Impedance_Viscosity_MVG

end submodule Calculate_HCF_MVG_Implementation

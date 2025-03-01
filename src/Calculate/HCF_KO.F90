submodule(Calculate_HCF) Calculate_HCF_KO_Implementation
    implicit none
contains
    module function Calculate_kr_KO_Base(alpha1, n1, l, h) result(kr)
        !$omp declare simd uniform(alpha1, n1, l, h)
        implicit none
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: Sw

        if (h < 0.0d0) then
            Sw = 0.5d0 * erfc(log(h / alpha1) / (n1 * sqrt(2.0d0)))
            kr = Sw**0.5d0 * (0.5d0 * erfc(log(h / alpha1) / (n1 * sqrt(2.0d0)) + n1 / sqrt(2.0d0)))**2.0d0
        else
            kr = 1.0d0
        end if

    end function Calculate_kr_KO_Base

    module function Calculate_kr_Base_KO(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_KO), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_KO_Base(self%alpha1, self%n1, self%l, h)

    end function Calculate_kr_Base_KO

    module function Calculate_kr_Base_Impedance_KO(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Impedance_KO), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_KO_Base(self%alpha1, self%n1, self%l, h)

    end function Calculate_kr_Base_Impedance_KO

    module function Calculate_kr_Base_Viscosity_KO(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Viscosity_KO), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_KO_Base(self%alpha1, self%n1, self%l, h)

    end function Calculate_kr_Base_Viscosity_KO

    module function Calculate_kr_Base_Impedance_Viscosity_KO(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Impedance_Viscosity_KO), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_KO_Base(self%alpha1, self%n1, self%l, h)

    end function Calculate_kr_Base_Impedance_Viscosity_KO

end submodule Calculate_HCF_KO_Implementation

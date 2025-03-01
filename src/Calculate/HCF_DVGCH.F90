submodule(Calculate_HCF) Calculate_HCF_DVGCH_Implementation
    implicit none
contains
    module function Calculate_kr_DVGCH_Base(alpha1, n1, m1, w1, n2, m2, w2, l, h) result(kr)
        !$omp declare simd uniform(alpha1, n1, m1, w1, n2, m2, w2, l, h)
        implicit none
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1, n2
        real(real64), intent(in) :: m1, m2
        real(real64), intent(in) :: w1, w2
        real(real64), intent(in) :: l
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: Sw1, Sw2

        if (h < 0) then
            Sw1 = (1.0d0 + (-alpha1 * h)**n1)**(-m1)
            Sw2 = (1.0d0 + (-alpha1 * h)**n2)**(-m2)
            kr = (w1 * Sw1 + w2 * Sw2)**l &
                 * (w1 * alpha1 * (1.0d0 - (1.0d0 - Sw1**(1.0d0 / m1))**m1) &
                    + w2 * alpha1 * (1.0d0 - (1.0d0 - Sw2**(1.0d0 / m2))**m2))**2.0d0 &
                 / (w1 * alpha1 + w2 * alpha1)**2.0d0
        else
            kr = 1.0d0
        end if

    end function Calculate_kr_DVGCH_Base

    module function Calculate_kr_Base_DVGCH(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_DVGCH), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_DVGCH_Base(self%alpha1, self%n1, self%m1, self%w1, self%n2, self%m2, self%w2, self%l, h)

    end function Calculate_kr_Base_DVGCH

    module function Calculate_kr_Base_Impedance_DVGCH(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Impedance_DVGCH), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_DVGCH_Base(self%alpha1, self%n1, self%m1, self%w1, self%n2, self%m2, self%w2, self%l, h)

    end function Calculate_kr_Base_Impedance_DVGCH

    module function Calculate_kr_Base_Viscosity_DVGCH(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Viscosity_DVGCH), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_DVGCH_Base(self%alpha1, self%n1, self%m1, self%w1, self%n2, self%m2, self%w2, self%l, h)

    end function Calculate_kr_Base_Viscosity_DVGCH

    module function Calculate_kr_Base_Impedance_Viscosity_DVGCH(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Impedance_Viscosity_DVGCH), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_DVGCH_Base(self%alpha1, self%n1, self%m1, self%w1, self%n2, self%m2, self%w2, self%l, h)

    end function Calculate_kr_Base_Impedance_Viscosity_DVGCH

end submodule Calculate_HCF_DVGCH_Implementation

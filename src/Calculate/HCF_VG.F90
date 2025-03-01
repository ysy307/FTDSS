submodule(Calculate_HCF) Calculate_HCF_VG_Implementation
    implicit none
contains
    module function Calculate_kr_VG_Base(alpha1, n1, m1, l, h) result(kr)
        !$omp declare simd uniform(alpha1, n1, m1, l, h)
        implicit none
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: m1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: Sw

        if (h < 0.0d0) then
            Sw = (1.0d0 + (-alpha1 * h)**n1)**(-m1)
        else
            Sw = 1.0d0
        end if

        kr = Sw**l * (1.0d0 - (1.0d0 - Sw**(1.0d0 / m1))**m1)**2.0d0

    end function Calculate_kr_VG_Base

    module function Calculate_kr_Base_VG(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_VG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_VG_Base(self%alpha1, self%n1, self%m1, self%l, h)

    end function Calculate_kr_Base_VG

    module function Calculate_kr_Base_Impedance_VG(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Impedance_VG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_VG_Base(self%alpha1, self%n1, self%m1, self%l, h)

    end function Calculate_kr_Base_Impedance_VG

    module function Calculate_kr_Base_Viscosity_VG(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Viscosity_VG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_VG_Base(self%alpha1, self%n1, self%m1, self%l, h)

    end function Calculate_kr_Base_Viscosity_VG

    module function Calculate_kr_Base_Impedance_Viscosity_VG(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_Impedance_Viscosity_VG), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = Calculate_kr_VG_Base(self%alpha1, self%n1, self%m1, self%l, h)

    end function Calculate_kr_Base_Impedance_Viscosity_VG

end submodule Calculate_HCF_VG_Implementation

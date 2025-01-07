module Calculate_HCF
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    implicit none

contains

    function Calculate_HCF_BC(structure_HCF, h) result(Klh)
        type(HCF_Parameters), intent(in) :: structure_HCF
        real(real64), intent(in) :: h
        real(real64) :: Klh
        real(real64) :: Sw

        if (h < -1.0d0 / structure_HCF%alpha1) then
            Sw = abs(structure_HCF%alpha1 * h)**(-structure_HCF%n1)
        else
            Sw = 1.0d0
        end if

        Klh = structure_HCF%Ks * Sw**(2.0d0 / (structure_HCF%n1 + structure_HCF%l + 2.0d0))

    end function Calculate_HCF_BC

    function Calculate_HCF_VG(structure_HCF, h) result(Klh)
        type(HCF_Parameters), intent(in) :: structure_HCF
        real(real64), intent(in) :: h
        real(real64) :: Klh
        real(real64) :: Sw

        if (h < 0) then
            Sw = (1.0d0 + abs(structure_HCF%alpha1 * h)**structure_HCF%n1)**(-structure_HCF%m1)
        else
            Sw = 1.0d0
        end if

        Klh = structure_HCF%Ks * Sw**structure_HCF%l * (1.0d0 - (1.0d0 - Sw**(1.0d0 / structure_HCF%m1))**structure_HCF%m1)**2.0d0

    end function Calculate_HCF_VG

    function Calculate_HCF_KO(structure_HCF, h) result(Klh)
        type(HCF_Parameters), intent(in) :: structure_HCF
        real(real64), intent(in) :: h
        real(real64) :: Klh
        real(real64) :: Sw

        if (h < 0) then
            Sw = 0.5d0 * erfc(log(h / structure_HCF%alpha1) / (structure_HCF%n1 * sqrt(2.0d0)))
            Klh = structure_HCF%Ks * Sw**0.5d0 * (0.5d0 * erfc(log(h / structure_HCF%alpha1) / (structure_HCF%n1 * sqrt(2.0d0)) + structure_HCF%n1 / sqrt(2.0d0)))**2.0d0
        else
            Klh = structure_HCF%Ks
        end if

    end function Calculate_HCF_KO

    function Calculate_HCF_MVG(structure_HCF, h) result(Klh)
        type(HCF_Parameters), intent(in) :: structure_HCF
        real(real64), intent(in) :: h
        real(real64) :: Klh
        real(real64) :: Sw, thetaM

        thetaM = structure_HCF%thetaR + (structure_HCF%thetaS - structure_HCF%thetaR) * (1.0d0 + abs(structure_HCF%alpha1 * structure_HCF%hcrit)**structure_HCF%n1)**(-structure_HCF%m1)

        if (h < structure_HCF%hcrit) then
            Sw = (structure_HCF%thetaS - structure_HCF%thetaR) / (thetaM - structure_HCF%thetaR) * (1.0d0 + abs(structure_HCF%alpha1 * h)**structure_HCF%n1)**(-structure_HCF%m1)
            Klh = structure_HCF%Ks * Sw**structure_HCF%l * ((1.0d0 - (1.0d0 - Sw**(1.0d0 / structure_HCF%m1))**structure_HCF%m1) / (1.0d0 - (1.0d0 - 1.0d0**(1.0d0 / structure_HCF%m1))**structure_HCF%m1))**2.0d0
        else
            Klh = structure_HCF%Ks
        end if

    end function Calculate_HCF_MVG

    function Calculate_HCF_Durner(structure_HCF, h) result(Klh)
        type(HCF_Parameters), intent(in) :: structure_HCF
        real(real64), intent(in) :: h
        real(real64) :: Klh
        real(real64) :: Sw1, Sw2

        if (h < 0) then
            Sw1 = structure_HCF%w1 * (1.0d0 + abs(structure_HCF%alpha1 * h)**structure_HCF%n1)**(-structure_HCF%m1)
            Sw2 = structure_HCF%w2 * (1.0d0 + abs(structure_HCF%alpha2 * h)**structure_HCF%n2)**(-structure_HCF%m2)
            Klh = structure_HCF%Ks * (structure_HCF%w1 * Sw1 + structure_HCF%w2 * Sw2)**structure_HCF%l &
                  * (structure_HCF%w1 * structure_HCF%alpha1 * (1.0d0 - (1.0d0 - Sw1**(1.0d0 / structure_HCF%m1))**structure_HCF%m1) &
                     + structure_HCF%w2 * structure_HCF%alpha2 * (1.0d0 - (1.0d0 - Sw2**(1.0d0 / structure_HCF%m2))**structure_HCF%m2))**2.0d0 &
                  / (structure_HCF%w1 * structure_HCF%alpha1 + structure_HCF%w2 * structure_HCF%alpha2)**2.0d0
        else
            Klh = structure_HCF%Ks
        end if

    end function Calculate_HCF_Durner

    function Calculate_HCF_DVGCH(structure_HCF, h) result(Klh)
        type(HCF_Parameters), intent(in) :: structure_HCF
        real(real64), intent(in) :: h
        real(real64) :: Klh
        real(real64) :: Sw1, Sw2

        if (h < 0) then
            Sw1 = structure_HCF%w1 * (1.0d0 + abs(structure_HCF%alpha1 * h)**structure_HCF%n1)**(-structure_HCF%m1)
            Sw2 = structure_HCF%w2 * (1.0d0 + abs(structure_HCF%alpha1 * h)**structure_HCF%n2)**(-structure_HCF%m2)
            Klh = structure_HCF%Ks * (structure_HCF%w1 * Sw1 + structure_HCF%w2 * Sw2)**structure_HCF%l &
                  * (structure_HCF%w1 * structure_HCF%alpha1 * (1.0d0 - (1.0d0 - Sw1**(1.0d0 / structure_HCF%m1))**structure_HCF%m1) &
                     + structure_HCF%w2 * structure_HCF%alpha1 * (1.0d0 - (1.0d0 - Sw2**(1.0d0 / structure_HCF%m2))**structure_HCF%m2))**2.0d0 &
                  / (structure_HCF%w1 * structure_HCF%alpha1 + structure_HCF%w2 * structure_HCF%alpha2)**2.0d0
        else
            Klh = structure_HCF%Ks
        end if

    end function Calculate_HCF_DVGCH

end module Calculate_HCF

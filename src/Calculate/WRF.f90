module Calculate_WRF
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    implicit none

contains

    function Calculate_WRF_BC(structure_WRF, h) result(thetaW)
        type(WRF_Parameters), intent(in) :: structure_WRF
        real(real64), intent(in) :: h
        real(real64) :: thetaW

        if (h < -1.0d0 / structure_WRF%alpha1) then
            thetaW = structure_WRF%thetaR + (structure_WRF%thetaS - structure_WRF%thetaR) * abs(structure_WRF%alpha1 * h)**(-structure_WRF%n1)
        else
            thetaW = structure_WRF%thetaS
        end if

    end function Calculate_WRF_BC

    function Calculate_WRF_VG(structure_WRF, h) result(thetaW)
        type(WRF_Parameters), intent(in) :: structure_WRF
        real(real64), intent(in) :: h
        real(real64) :: thetaW

        if (h < 0) then
            thetaW = structure_WRF%thetaR + (structure_WRF%thetaS - structure_WRF%thetaR) * (1.0d0 + abs(structure_WRF%alpha1 * h)**structure_WRF%n1)**(-structure_WRF%m1)
        else
            thetaW = structure_WRF%thetaS
        end if

    end function Calculate_WRF_VG

    function Calculate_WRF_KO(structure_WRF, h) result(thetaW)
        type(WRF_Parameters), intent(in) :: structure_WRF
        real(real64), intent(in) :: h
        real(real64) :: thetaW

        if (h < 0) then
            thetaW = structure_WRF%thetaR + (structure_WRF%thetaS - structure_WRF%thetaR) * 0.5d0 * erfc(log(h / structure_WRF%alpha1) / (structure_WRF%n1 * sqrt(2.0d0)))
        else
            thetaW = structure_WRF%thetaS
        end if

    end function Calculate_WRF_KO

    function Calculate_WRF_MVG(structure_WRF, h) result(thetaW)
        type(WRF_Parameters), intent(in) :: structure_WRF
        real(real64), intent(in) :: h
        real(real64) :: thetaW

        if (h < structure_WRF%hcrit) then
            thetaW = structure_WRF%thetaR + (structure_WRF%thetaS - structure_WRF%thetaR) * (1.0d0 + abs(structure_WRF%alpha1 * h)**structure_WRF%n1)**(-structure_WRF%m1)
        else
            thetaW = structure_WRF%thetaS
        end if

    end function Calculate_WRF_MVG

    function Calculate_WRF_Durner(structure_WRF, h) result(thetaW)
        type(WRF_Parameters), intent(in) :: structure_WRF
        real(real64), intent(in) :: h
        real(real64) :: thetaW

        if (h < 0) then
            thetaW = structure_WRF%thetaR + (structure_WRF%thetaS - structure_WRF%thetaR) * &
                     (structure_WRF%w1 * (1.0d0 + abs(structure_WRF%alpha1 * h)**structure_WRF%n1)**(-structure_WRF%m1) &
                      + structure_WRF%w2 * (1.0d0 + abs(structure_WRF%alpha2 * h)**structure_WRF%n2)**(-structure_WRF%m2))
        else
            thetaW = structure_WRF%thetaS
        end if

    end function Calculate_WRF_Durner

    function Calculate_WRF_DVGCH(structure_WRF, h) result(thetaW)
        type(WRF_Parameters), intent(in) :: structure_WRF
        real(real64), intent(in) :: h
        real(real64) :: thetaW

        if (h < 0) then
            thetaW = structure_WRF%thetaR + (structure_WRF%thetaS - structure_WRF%thetaR) * &
                     (structure_WRF%w1 * (1.0d0 + abs(structure_WRF%alpha1 * h)**structure_WRF%n1)**(-structure_WRF%m1) &
                      + structure_WRF%w2 * (1.0d0 + abs(structure_WRF%alpha1 * h)**structure_WRF%n2)**(-structure_WRF%m2))
        else
            thetaW = structure_WRF%thetaS
        end if

    end function Calculate_WRF_DVGCH

end module Calculate_WRF

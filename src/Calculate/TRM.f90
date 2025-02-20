module Calculate_TRM
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use omp_lib
    use types
    implicit none
    private

    public :: TRMethod

contains

    subroutine TRMethod(Solver)
        implicit none
        type(SolverInfo), intent(inout) :: Solver
        real(real64), parameter :: vTf = 0.0d0
        real(real64) :: Fr_tmp
        real(real64) :: C, TRM_BTM, tmpSi
        integer(int32) :: iN, vector_size

        if (Solver%Flags%isSwitchOnceTRM) then
            TRM_BTM = 1.0d0 / Solver%Heat%Constants%Density%water * Solver%Heat%Constants%Porosity * Solver%Heat%Constants%LatentHeat
            ! $omp parallel do private(iN, C, tmpSi)
            do iN = 1, Solver%N%node
                C = Solver%Heat%Variables%rho%pre(iN) * Solver%Heat%Variables%Cs%pre(iN) * TRM_BTM
                tmpSi = Solver%Si%old(iN) + C * (vTf - Solver%T%new(iN))
                if (tmpSi <= 0.0d0 .and. Solver%Si%old(iN) == 0.0d0) then
                    Solver%Si%new(iN) = 0.0d0
                else if (tmpSi >= 1.0d0 .and. Solver%Si%old(iN) == 1.0d0) then
                    Solver%Si%new(iN) = 1.0d0
                else if (0.0d0 < tmpSi .and. tmpSi < 1.0d0 .and. Solver%Si%old(iN) <= 1.0d0) then
                    Solver%T%new(iN) = vTf
                    Solver%Si%new(iN) = tmpSi
                else if (0.0d0 < Solver%Si%old(iN) .and. Solver%Si%old(iN) < 1.0d0 .and. tmpSi >= 1.0d0) then
                    Solver%T%new(iN) = vTf + (1.0d0 - tmpSi) / C
                    Solver%Si%new(iN) = 1.0d0
                end if
            end do
            ! $omp end parallel do
        else
            ! $omp parallel do private(iN, C, tmpSi, TRM_BTM)
            ! print*,Solver%Heat%Variables%Cs%pre(:)
            ! stop
            do iN = 1, Solver%N%node
                TRM_BTM = Solver%Heat%Constants%Density%water * Solver%Heat%Constants%LatentHeat
                C = Solver%Heat%Variables%rho%pre(iN) * Solver%Heat%Variables%Cs%pre(iN) / (Solver%mWater%pre(iN) * TRM_BTM)
                tmpSi = Solver%Si%old(iN) + C * (vTf - Solver%T%new(iN))
                if (tmpSi <= 0.0d0 .and. Solver%Si%old(iN) == 0.0d0) then
                    Solver%Si%new(iN) = 0.0d0
                else if (tmpSi >= 1.0d0 .and. Solver%Si%old(iN) == 1.0d0) then
                    Solver%Si%new(iN) = 1.0d0
                else if (0.0d0 < tmpSi .and. tmpSi < 1.0d0 .and. Solver%Si%old(iN) <= 1.0d0) then
                    Solver%T%new(iN) = vTf
                    Solver%Si%new(iN) = tmpSi
                else if (0.0d0 < Solver%Si%old(iN) .and. Solver%Si%old(iN) < 1.0d0 .and. tmpSi >= 1.0d0) then
                    Solver%T%new(iN) = vTf + (1.0d0 - tmpSi) / C
                    Solver%Si%new(iN) = 1.0d0
                end if
            end do
            ! $omp end parallel do
        end if
        ! stop
        ! do iN = 250,260
        !         print*,Solver%Si%new(iN), Solver%T%new(iN)
        ! end do
    end subroutine TRMethod

    ! subroutine Calculate_TRM()
    !     implicit none

    !     TRM_BTM = Solver%Heat%Constants%Density%water * Solver%Heat%Constants%LatentHeat
    !     C = Solver%Heat%Variables%rho%pre(iN) * Solver%Heat%Variables%Cs%pre(iN) / (Solver%mWater%pre(iN) * TRM_BTM)
    !     tmpSi = Solver%Si%old(iN) + C * (vTf - Solver%T%new(iN))
    !     if (tmpSi <= 0.0d0 .and. Solver%Si%old(iN) == 0.0d0) then
    !         Solver%Si%new(iN) = 0.0d0
    !     else if (tmpSi >= 1.0d0 .and. Solver%Si%old(iN) == 1.0d0) then
    !         Solver%Si%new(iN) = 1.0d0
    !     else if (0.0d0 < tmpSi .and. tmpSi < 1.0d0 .and. Solver%Si%old(iN) <= 1.0d0) then
    !         Solver%T%new(iN) = vTf
    !         Solver%Si%new(iN) = tmpSi
    !     else if (0.0d0 < Solver%Si%old(iN) .and. Solver%Si%old(iN) < 1.0d0 .and. tmpSi >= 1.0d0) then
    !         Solver%T%new(iN) = vTf + (1.0d0 - tmpSi) / C
    !         Solver%Si%new(iN) = 1.0d0
    !     end if
    ! end subroutine Calculate_TRM
end module Calculate_TRM

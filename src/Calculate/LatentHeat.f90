module Calculate_LatentHeat
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use omp_lib
    use :: Types, g => GravityAcceleration
    implicit none
    private
    real(real64), parameter :: TtoK = 273.15d0

    public :: Calc_LatentHeatTerm
    public :: LatentHeatTreatment
    public :: Find_Ca_max

contains

    function Calc_LatentHeatTerm(T, Tnew, Latent) result(res)
        implicit none
        real(real64)                          :: res
        real(real64), intent(in) :: T, Tnew
        type(LatentHeatTreatment), intent(in) :: Latent

        ! 20: GCC, 30: Power
        if (Latent%useModel == 20) then
            res = Latent%Cp_unf * (T - Tnew) - Latent%Lf * Latent%rhoI * (Latent%GCC%thetaS - Latent%GCC%thetaR) &
                  * (1.0d0 - (1.0d0 + abs(Latent%GCC%alpha * Latent%Lf * log((TtoK + T) / TtoK) / g)**Latent%GCC%n)**(-Latent%GCC%m))
        else if (Latent%useModel == 30) then
            res = Latent%Cp_unf * (T - Tnew) / (Latent%Lf * Latent%rhoI * Latent%Power%phi) &
                  + (1.0d0 - T + Latent%Power%Tf)**Latent%Power%a - 1.0d0
        end if
    end function Calc_LatentHeatTerm

    function Calc_Ca_GCC(Heat, T) result(Ca)
        implicit none
        type(HeatFields), intent(in) :: Heat
        real(real64), intent(in) :: T
        real(real64)                 :: Ca
        real(real64)                 :: Cp, Si, A, B, C
        real(real64)                                 :: Qs, Qr, alpha, n, m, Lf, Dice, Tf

        Qs = Heat%Latent%GCC%thetaS
        Qr = Heat%Latent%GCC%thetaR
        alpha = Heat%Latent%GCC%alpha
        n = Heat%Latent%GCC%n
        m = Heat%Latent%GCC%m
        Lf = Heat%Latent%Lf
        Dice = Heat%Latent%rhoI
        Tf = Heat%Latent%GCC%Tf

        A = Qs - Qr
        B = alpha * Lf / g
        C = (T + TtoK) / (Tf + TtoK)

        if (T < Tf) then
            Si = (A * (1.0d0 - (1.0d0 + abs(B * log(C))**n)**(-m))) / Qs
        else
            Si = 0.0d0
        end if
        Cp = Heat%Constants%HeatCapacity%soil * (1.0d0 - Heat%Constants%Porosity) &
             + Heat%Constants%HeatCapacity%water * Heat%Constants%Porosity * (1.0d0 - Si) &
             + Heat%Constants%HeatCapacity%ice * Heat%Constants%Porosity * Si
        if (T < Tf) then
            Ca = Cp - Lf * Dice * (A * B**2 * n * m * log(C) * abs(B * log(C))**(n - 2.d0)) &
                 / ((T + TtoK) * (1.d0 + abs(B * log(C))**n)**(m + 1.d0))
        else
            Ca = Cp
        end if
    end function Calc_Ca_GCC

    subroutine Find_Ca_max(Heat)
        implicit none
        type(HeatFields), intent(inout) :: Heat

        real(real64)                    :: x0, x1, x2, x3, f1, f2, tau
        real(real64), parameter         :: epsilon = 1.0d-15

        if (Heat%Latent%useModel == 20) then
            tau = (sqrt(5.d0) - 1.d0) / 2.d0 ! 黄金比

            x0 = 0.0d0
            x3 = -1.0d0
            x1 = x0 + (1.d0 - tau) * (x3 - x0)
            x2 = x0 + tau * (x3 - x0)
            f1 = Calc_Ca_GCC(Heat, x1)
            f2 = Calc_Ca_GCC(Heat, x2)

            do while (abs(x3 - x0) > epsilon)
                if (f2 > f1) then
                    x0 = x1
                    x1 = x2
                    x2 = x0 + tau * (x3 - x0)
                    f1 = f2
                    f2 = Calc_Ca_GCC(Heat, x2)
                else
                    x3 = x2
                    x2 = x1
                    x1 = x0 + (1.d0 - tau) * (x3 - x0)
                    f2 = f1
                    f1 = Calc_Ca_GCC(Heat, x1)
                end if
            end do

            Heat%Latent%GCC%Ca_max = Calc_Ca_GCC(Heat, (x1 + x2) / 2.d0)
        else if (Heat%Latent%useModel == 30) then
            Heat%Latent%Power%Ca_max = Heat%Latent%Cp_unf - Heat%Latent%Lf * Heat%Latent%rhoI * Heat%Latent%Power%phi * Heat%Latent%Power%a
        end if
    end subroutine Find_Ca_max
end module Calculate_LatentHeat

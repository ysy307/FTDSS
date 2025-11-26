submodule(physics_material_iapws) iapws97_base_region5
    implicit none
    !------------------------------------------------------------------------------------------
    ! Region5: High temperature region (IAPWS-IF97)
    !------------------------------------------------------------------------------------------
    integer(int32), parameter :: N05_terms = 6
    real(real64), parameter :: J0_r5(N05_terms) = [0.0d0, 1.0d0, -3.0d0, -2.0d0, -1.0d0, 2.0d0]
    real(real64), parameter :: n0_r5(N05_terms) = [ &
                               -0.13179983674201d2, &
                               0.68540841634434d1, &
                               -0.24805148933466d-1, &
                               0.36901534980333d0, &
                               -0.31161318213925d1, &
                               -0.32961626538917d0]

    integer(int32), parameter :: Nr5_terms = 6
    real(real64), parameter :: Ir_r5(Nr5_terms) = [1.0d0, 1.0d0, 1.0d0, 2.0d0, 2.0d0, 3.0d0]
    real(real64), parameter :: Jr_r5(Nr5_terms) = [1.0d0, 2.0d0, 3.0d0, 3.0d0, 9.0d0, 7.0d0]
    real(real64), parameter :: nr_r5(Nr5_terms) = [ &
                               0.15736404855259d-2, &
                               0.90153761673944d-3, &
                               -0.50270077677648d-2, &
                               0.22440037409485d-5, &
                               -0.41163275453471d-5, &
                               0.37919454822955d-7]

contains
    module pure elemental function calc_gamma0_region5(pi, tau) result(gamma0)
        implicit none
        real(real64), intent(in) :: pi
        real(real64), intent(in) :: tau
        real(real64) :: gamma0

        integer(int32) :: i

        gamma0 = log(pi)
        do i = 1, N05_terms
            gamma0 = gamma0 + n0_r5(i) * (tau**J0_r5(i))
        end do
    end function calc_gamma0_region5

    module pure elemental function calc_gamma0_pi_region5(pi, tau) result(gamma0_p)
        implicit none
        real(real64), intent(in) :: pi
        real(real64), intent(in) :: tau
        real(real64) :: gamma0_p

        gamma0_p = 1.0d0 / pi
    end function calc_gamma0_pi_region5

    module pure elemental function calc_gamma0_pipi_region5(pi, tau) result(gamma0_pp)
        implicit none
        real(real64), intent(in) :: pi
        real(real64), intent(in) :: tau
        real(real64) :: gamma0_pp

        ! Table 40: Second derivative w.r.t pi is -1/pi^2
        gamma0_pp = -1.0d0 / (pi * pi)
    end function calc_gamma0_pipi_region5

    module pure elemental function calc_gamma0_tau_region5(pi, tau) result(gamma0_t)
        implicit none
        real(real64), intent(in) :: pi
        real(real64), intent(in) :: tau
        real(real64) :: gamma0_t

        integer(int32) :: i

        gamma0_t = 0.0d0
        do i = 1, N05_terms
            gamma0_t = gamma0_t + n0_r5(i) * J0_r5(i) * (tau**(J0_r5(i) - 1.0d0))
        end do
    end function calc_gamma0_tau_region5

    module pure elemental function calc_gamma0_tautau_region5(pi, tau) result(gamma0_tt)
        implicit none
        real(real64), intent(in) :: pi
        real(real64), intent(in) :: tau
        real(real64) :: gamma0_tt

        integer(int32) :: i

        gamma0_tt = 0.0d0
        do i = 1, N05_terms
            ! Check for terms that vanish upon second derivative
            gamma0_tt = gamma0_tt + n0_r5(i) * J0_r5(i) * (J0_r5(i) - 1.0d0) * (tau**(J0_r5(i) - 2.0d0))
        end do
    end function calc_gamma0_tautau_region5

    module pure elemental function calc_gamma0_pitau_region5(pi, tau) result(gamma0_pt)
        implicit none
        real(real64), intent(in) :: pi
        real(real64), intent(in) :: tau
        real(real64) :: gamma0_pt

        ! Ideally gas part has no mixed terms of pi and tau
        gamma0_pt = 0.0d0
    end function calc_gamma0_pitau_region5

    module pure elemental function calc_gammar_region5(pi, tau) result(gammar)
        implicit none
        real(real64), intent(in) :: pi
        real(real64), intent(in) :: tau
        real(real64) :: gammar

        integer(int32) :: i

        gammar = 0.0d0
        do i = 1, Nr5_terms
            gammar = gammar + nr_r5(i) * (pi**Ir_r5(i)) * (tau**Jr_r5(i))
        end do
    end function calc_gammar_region5

    module pure elemental function calc_gammar_pi_region5(pi, tau) result(gammar_p)
        implicit none
        real(real64), intent(in) :: pi
        real(real64), intent(in) :: tau
        real(real64) :: gammar_p

        integer(int32) :: i

        gammar_p = 0.0d0
        do i = 1, Nr5_terms
            gammar_p = gammar_p + nr_r5(i) * Ir_r5(i) * (pi**(Ir_r5(i) - 1.0d0)) * (tau**Jr_r5(i))
        end do
    end function calc_gammar_pi_region5

    module pure elemental function calc_gammar_pipi_region5(pi, tau) result(gammar_pp)
        implicit none
        real(real64), intent(in) :: pi
        real(real64), intent(in) :: tau
        real(real64) :: gammar_pp

        integer(int32) :: i

        gammar_pp = 0.0d0
        do i = 1, Nr5_terms
            gammar_pp = gammar_pp + nr_r5(i) * Ir_r5(i) * (Ir_r5(i) - 1.0d0) * (pi**(Ir_r5(i) - 2.0d0)) * (tau**Jr_r5(i))
        end do
    end function calc_gammar_pipi_region5

    module pure elemental function calc_gammar_tau_region5(pi, tau) result(gammar_t)
        implicit none
        real(real64), intent(in) :: pi
        real(real64), intent(in) :: tau
        real(real64) :: gammar_t

        integer(int32) :: i

        gammar_t = 0.0d0
        do i = 1, Nr5_terms
            gammar_t = gammar_t + nr_r5(i) * Jr_r5(i) * (pi**Ir_r5(i)) * (tau**(Jr_r5(i) - 1.0d0))
        end do
    end function calc_gammar_tau_region5

    module pure elemental function calc_gammar_tautau_region5(pi, tau) result(gammar_tt)
        implicit none
        real(real64), intent(in) :: pi
        real(real64), intent(in) :: tau
        real(real64) :: gammar_tt

        integer(int32) :: i

        gammar_tt = 0.0d0
        do i = 1, Nr5_terms
            gammar_tt = gammar_tt + nr_r5(i) * Jr_r5(i) * (Jr_r5(i) - 1.0d0) * (pi**Ir_r5(i)) * (tau**(Jr_r5(i) - 2.0d0))
        end do
    end function calc_gammar_tautau_region5

    module pure elemental function calc_gammar_pitau_region5(pi, tau) result(gammar_pt)
        implicit none
        real(real64), intent(in) :: pi
        real(real64), intent(in) :: tau
        real(real64) :: gammar_pt

        integer(int32) :: i

        gammar_pt = 0.0d0
        do i = 1, Nr5_terms
            gammar_pt = gammar_pt + nr_r5(i) * Ir_r5(i) * Jr_r5(i) * (pi**(Ir_r5(i) - 1.0d0)) * (tau**(Jr_r5(i) - 1.0d0))
        end do
    end function calc_gammar_pitau_region5

end submodule iapws97_base_region5

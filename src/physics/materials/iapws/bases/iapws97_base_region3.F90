submodule(physics_material_iapws) iapws97_base_region3
    implicit none
    !------------------------------------------------------------------------------------------
    ! Region3: High Pressure Liquid Water and Steam (IAPWS-IF97)
    !------------------------------------------------------------------------------------------
    integer(int32), parameter :: N3_terms = 40
    real(real64), parameter :: I_r3(2:N3_terms) = [ & !&
                                      0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0,  0.0d0,  1.0d0,  1.0d0, & !&
                               1.0d0, 1.0d0, 2.0d0, 2.0d0, 2.0d0, 2.0d0, 2.0d0,  2.0d0,  3.0d0,  3.0d0, & !&
                               3.0d0, 3.0d0, 3.0d0, 4.0d0, 4.0d0, 4.0d0, 4.0d0,  5.0d0,  5.0d0,  5.0d0, & !&
                               6.0d0, 6.0d0, 6.0d0, 7.0d0, 8.0d0, 9.0d0, 9.0d0, 10.0d0, 10.0d0, 11.0d0] !&
    real(real64), parameter :: J_r3(2:N3_terms) = [ & !&
                                        0.0d0,  1.0d0, 2.0d0,  7.0d0, 10.0d0, 12.0d0, 23.0d0, 2.0d0,  6.0d0, & !&
                               15.0d0, 17.0d0,  0.0d0, 2.0d0,  6.0d0,  7.0d0, 22.0d0, 26.0d0, 0.0d0,  2.0d0, & !&
                                4.0d0, 16.0d0, 26.0d0, 0.0d0,  2.0d0,  4.0d0, 26.0d0,  1.0d0, 3.0d0, 26.0d0, & !&
                                0.0d0,  2.0d0, 26.0d0, 2.0d0, 26.0d0,  2.0d0, 26.0d0,  0.0d0, 1.0d0, 26.0d0] !&
    real(real64), parameter :: n_r3(N3_terms) = [ & !&
                                0.10658070028513d1, & !&
                               -0.15732845290239d2, & !&
                                0.20944396974307d2, & !&
                               -0.76867707878716d1, & !&
                                0.26185947787954d1, & !&
                               -0.28080781148620d1, & !&
                                0.12053369696517d1, & !&
                               -0.84566812812502d-2, & !&
                               -0.12654315477714d1, & !&
                               -0.11524407806681d1, & !&
                                0.88521043984318d0, & !&
                               -0.64207765181607d0, & !&
                                0.38493460186671d0, & !&
                               -0.85214708824206d0, & !&
                                0.48972281541877d1, & !&
                               -0.30502617256965d1, & !&
                                0.39420536879154d-1, & !&
                                0.12558408424308d0, & !&
                               -0.27999329698710d0, & !&
                                0.13899799569460d1, & !&
                               -0.20189915023570d1, & !&
                               -0.82147637173963d-2, & !&
                               -0.47596035734923d0, & !&
                                0.43984074473500d-1, & !&
                               -0.44476435428739d0, & !&
                                0.90572070719733d0, & !&
                                0.70522450087967d0, & !&
                                0.10770512626332d0, & !&
                               -0.32913623258954d0, & !&
                               -0.50871062041158d0, & !&
                               -0.22175400873096d-1, & !&
                                0.94260751665092d-1, & !&
                                0.16436278447961d0, & !&
                               -0.13503372241348d-1, & !&
                               -0.14834345352472d-1, & !&
                                0.57922953628084d-3, & !&
                                0.32308904703711d-2, & !&
                                0.80964802996215d-4, & !&
                               -0.16557679795037d-3, & !&
                               -0.44923899061815d-4]

contains

    !> Calculate phi (Equation 28)
    module pure elemental function calc_phi_region3(delta, tau) result(phi)
        implicit none
        real(real64), intent(in) :: delta
        real(real64), intent(in) :: tau
        real(real64) :: phi
        integer(int32) :: i

        ! Term i=1: n1 * ln(delta)
        phi = n_r3(1) * log(delta)

        ! Terms i=2 to 40
        do i = 2, N3_terms
            phi = phi + n_r3(i) * (delta**I_r3(i)) * (tau**J_r3(i))
        end do
    end function calc_phi_region3

    !> Calculate phi_delta (1st derivative w.r.t delta)
    module pure elemental function calc_phi_d_region3(delta, tau) result(phi_d)
        implicit none
        real(real64), intent(in) :: delta
        real(real64), intent(in) :: tau
        real(real64) :: phi_d

        integer(int32) :: i

        ! i=1: n1 / delta
        phi_d = n_r3(1) / delta

        do i = 2, N3_terms
            ! n_i * I_i * delta^(I_i-1) * tau^J_i
            phi_d = phi_d + n_r3(i) * I_r3(i) * (delta**(I_r3(i) - 1.0d0)) * (tau**J_r3(i))
        end do
    end function calc_phi_d_region3

    !> Calculate phi_d_delta (2nd derivative w.r.t delta)
    module pure elemental function calc_phi_dd_region3(delta, tau) result(phi_dd)
        implicit none
        real(real64), intent(in) :: delta
        real(real64), intent(in) :: tau
        real(real64) :: phi_dd

        integer(int32) :: i

        ! i=1: -n1 / delta^2
        phi_dd = -n_r3(1) / (delta**2)

        do i = 2, N3_terms
            ! n_i * I_i * (I_i-1) * delta^(I_i-2) * tau^J_i
            phi_dd = phi_dd + n_r3(i) * I_r3(i) * (I_r3(i) - 1.0d0) * (delta**(I_r3(i) - 2.0d0)) * (tau**J_r3(i))
        end do
    end function calc_phi_dd_region3

    !> Calculate phi_tau (1st derivative w.r.t tau)
    module pure elemental function calc_phi_t_region3(delta, tau) result(phi_t)
        implicit none
        real(real64), intent(in) :: delta
        real(real64), intent(in) :: tau
        real(real64) :: phi_t

        integer(int32) :: i

        ! i=1 term depends only on delta, so derivative w.r.t tau is 0
        phi_t = 0.0d0

        do i = 2, N3_terms
            ! n_i * J_i * delta^I_i * tau^(J_i-1)
            phi_t = phi_t + n_r3(i) * J_r3(i) * (delta**I_r3(i)) * (tau**(J_r3(i) - 1.0d0))
        end do
    end function calc_phi_t_region3

    !> Calculate phi_t_tau (2nd derivative w.r.t tau)
    module pure elemental function calc_phi_tt_region3(delta, tau) result(phi_tt)
        implicit none
        real(real64), intent(in) :: delta
        real(real64), intent(in) :: tau
        real(real64) :: phi_tt

        integer(int32) :: i

        phi_tt = 0.0d0

        do i = 2, N3_terms
            ! n_i * J_i * (J_i-1) * delta^I_i * tau^(J_i-2)
            phi_tt = phi_tt + n_r3(i) * J_r3(i) * (J_r3(i) - 1.0d0) * (delta**I_r3(i)) * (tau**(J_r3(i) - 2.0d0))
        end do
    end function calc_phi_tt_region3

    !> Calculate phi_d_tau (Mixed derivative)
    module pure elemental function calc_phi_dt_region3(delta, tau) result(phi_dt)
        implicit none
        real(real64), intent(in) :: delta
        real(real64), intent(in) :: tau
        real(real64) :: phi_dt

        integer(int32) :: i

        ! i=1 term is 0 for mixed derivative
        phi_dt = 0.0d0

        do i = 2, N3_terms
            ! n_i * I_i * J_i * delta^(I_i-1) * tau^(J_i-1)
            phi_dt = phi_dt + n_r3(i) * I_r3(i) * J_r3(i) * (delta**(I_r3(i) - 1.0d0)) * (tau**(J_r3(i) - 1.0d0))
        end do
    end function calc_phi_dt_region3

end submodule iapws97_base_region3

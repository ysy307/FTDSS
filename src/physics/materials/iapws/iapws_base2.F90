submodule(physics_material_iapws) iapws_base2
    implicit none
    !------------------------------------------------------------------------------------------
    ! Reigon2: Superheated Steam (IAPWS-IF97)
    !------------------------------------------------------------------------------------------
    integer(int32), parameter :: No2_terms = 9
    real(real64), parameter :: Jo2(No2_terms) = [0.0d0, 1.0d0, -5.0d0, -4.0d0, -3.0d0, -2.0d0, -1.0d0, 2.0d0, 3.0d0]
    real(real64), parameter :: no2(No2_terms) = &
                               [-0.96927686500217d1,   0.10086655968018d2, -0.56087911283020d-2, & !&
                                 0.71452738081455d-1, -0.40710498223928d0,  0.14240819171444d1, & !&
                                -0.43839511319450d1,  -0.28408632460772d0,  0.21268463753307d-1]
    integer(int32), parameter :: Nr2_terms = 43
    real(real64), parameter :: Ir2(Nr2_terms) = [ & !&
                                1.0d0, 1.0d0,   1.0d0,  1.0d0,  1.0d0,  2.0d0,  2.0d0,  2.0d0,  2.0d0,  2.0d0, & !&
                                3.0d0, 3.0d0,   3.0d0,  3.0d0,  3.0d0,  4.0d0,  4.0d0,  4.0d0,  5.0d0,  6.0d0, & !&
                                6.0d0, 6.0d0,   7.0d0,  7.0d0,  7.0d0,  8.0d0,  8.0d0,  9.0d0, 10.0d0, 10.0d0, & !&
                               10.0d0, 16.0d0, 16.0d0, 18.0d0, 20.0d0, 20.0d0, 20.0d0, 21.0d0, 22.0d0, 23.0d0, & !&
                               24.0d0, 24.0d0, 24.0d0]
    real(real64), parameter :: Jr2(Nr2_terms) = [ & !&
                                0.0d0,  1.0d0,  2.0d0,  3.0d0,  6.0d0,  1.0d0,  2.0d0,  4.0d0,  7.0d0, 36.0d0, & !&
                                0.0d0,  1.0d0,  3.0d0,  6.0d0, 35.0d0,  1.0d0,  2.0d0,  3.0d0,  7.0d0,  3.0d0, & !&
                               16.0d0, 35.0d0,  0.0d0, 11.0d0, 25.0d0,  8.0d0, 36.0d0, 13.0d0,  4.0d0, 10.0d0, & !&
                               14.0d0, 29.0d0, 50.0d0, 57.0d0, 20.0d0, 35.0d0, 48.0d0, 21.0d0, 53.0d0, 39.0d0, & !&
                               26.0d0, 40.0d0, 58.0d0]
    real(real64), parameter :: nr2(Nr2_terms) = [-0.17731742473213d-02, & !&  1
                                                 -0.17834862292358d-01, & !&  2
                                                 -0.45996013696365d-01, & !&  3
                                                 -0.57581259083432d-01, & !&  4
                                                 -0.50325278727930d-01, & !&  5
                                                 -0.33032641670203d-04, & !&  6
                                                 -0.18948987516315d-03, & !&  7
                                                 -0.39392777243355d-02, & !&  8
                                                 -0.43797295650573d-01, & !&  9
                                                 -0.26674547914087d-04, & !& 10
                                                  0.20481737692309d-07, & !& 11
                                                  0.43870667284435d-06, & !& 12
                                                 -0.32277677238570d-04, & !& 13
                                                 -0.15033924542148d-02, & !& 14
                                                 -0.40668253562649d-01, & !& 15
                                                 -0.78847309559367d-09, & !& 16
                                                  0.12790717852285d-07, & !& 17
                                                  0.48225372718507d-06, & !& 18
                                                  0.22922076337661d-05, & !& 19
                                                 -0.16714766451061d-10, & !& 20
                                                 -0.21171472321355d-02, & !& 21
                                                 -0.23895741934104d+02, & !& 22
                                                 -0.59059564324270d-17, & !& 23
                                                 -0.12621808899101d-05, & !& 24
                                                 -0.38946842435739d-01, & !& 25
                                                  0.11256211360459d-10, & !& 26
                                                 -0.82311340897998d+01, & !& 27
                                                  0.19809712802088d-07, & !& 28
                                                  0.10406965210174d-18, & !& 29
                                                 -0.10234747095929d-12, & !& 30
                                                 -0.10018179379511d-08, & !& 31
                                                 -0.80882908646985d-10, & !& 32
                                                  0.10693031879409d0, & !& 33
                                                 -0.33662250574171d0, & !& 34
                                                  0.89185845355421d-24, & !& 35
                                                  0.30629316876232d-12, & !& 36
                                                 -0.42002467698208d-05, & !& 37
                                                 -0.59056029685639d-25, & !& 38
                                                  0.37826947613457d-05, & !& 39
                                                 -0.12768608934681d-14, & !& 40
                                                  0.73087610595061d-28, & !& 41
                                                  0.55414715350778d-16, & !& 42
                                                 -0.94369707241210d-06 & !& 43
                                                 ]
contains
    module pure elemental function get_gammao_region2(pi, tau) result(gammao)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Resulting \(\gamma^o\) value
        real(real64) :: gammao

        integer(int32) :: i
        real(real64) :: term

        gammao = log(pi)

        do i = 1, No2_terms
            term = no2(i) * tau**Jo2(i)
            gammao = gammao + term
        end do

    end function get_gammao_region2

    module pure elemental function get_gammao_pi_region2(pi, tau) result(gammao_pi)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma^o_{\pi}\)
        real(real64) :: gammao_pi

        gammao_pi = 1.0d0 / pi

    end function get_gammao_pi_region2

    module pure elemental function get_gammao_pipi_region2(pi, tau) result(gammao_pipi)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma^o_{\pi}\)
        real(real64) :: gammao_pipi

        gammao_pipi = -1.0d0 / pi**2.0d0

    end function get_gammao_pipi_region2

    module pure elemental function get_gammao_tau_region2(pi, tau) result(gammao_tau)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma^r_{\pi}\)
        real(real64) :: gammao_tau

        integer(int32) :: i
        real(real64) :: term_tau

        gammao_tau = 0.0d0

        do i = 1, No2_terms
            term_tau = no2(i) * Jo2(i) * tau**(Jo2(i) - 1.0d0)
            gammao_tau = gammao_tau + term_tau
        end do

    end function get_gammao_tau_region2

    module pure elemental function get_gammao_tautau_region2(pi, tau) result(gammao_tautau)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma^r_{\pi}\)
        real(real64) :: gammao_tautau

        integer(int32) :: i
        real(real64) :: term_tau

        gammao_tautau = 0.0d0

        do i = 1, No2_terms
            term_tau = no2(i) * Jo2(i) * (Jo2(i) - 1.0d0) * tau**(Jo2(i) - 2.0d0)
            gammao_tautau = gammao_tautau + term_tau
        end do

    end function get_gammao_tautau_region2

    module pure elemental function get_gammao_pitau_region2(pi, tau) result(gammao_pitau)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma^r_{\pi}\)
        real(real64) :: gammao_pitau

        gammao_pitau = 0.0d0

    end function get_gammao_pitau_region2

    module pure elemental function get_gammar_region2(pi, tau) result(gammar)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Resulting \(\gamma^r\) value
        real(real64) :: gammar

        integer(int32) :: i
        real(real64) :: term

        gammar = 0.0d0

        do i = 1, Nr2_terms
            term = nr2(i) * pi**Ir2(i) * (tau - 0.5d0)**Jr2(i)
            gammar = gammar + term
        end do

    end function get_gammar_region2

    module pure elemental function get_gammar_pi_region2(pi, tau) result(gammar_pi)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma^r_{\pi}\)
        real(real64) :: gammar_pi

        integer(int32) :: i
        real(real64) :: term_pi

        gammar_pi = 0.0d0

        do i = 1, Nr2_terms
            term_pi = nr2(i) * Ir2(i) * pi**(Ir2(i) - 1.0d0) * (tau - 0.5d0)**Jr2(i)
            gammar_pi = gammar_pi + term_pi
        end do

    end function get_gammar_pi_region2

    module pure elemental function get_gammar_pipi_region2(pi, tau) result(gammar_pipi)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma^r_{\pi\pi}\)
        real(real64) :: gammar_pipi

        integer(int32) :: i
        real(real64) :: term_pipi

        gammar_pipi = 0.0d0

        do i = 1, Nr2_terms
            term_pipi = nr2(i) * dble(Ir2(i)) * (dble(Ir2(i)) - 1) * pi**(Ir2(i) - 2) * (tau - 0.5d0)**Jr2(i)
            gammar_pipi = gammar_pipi + term_pipi
        end do

    end function get_gammar_pipi_region2

    module pure elemental function get_gammar_tau_region2(pi, tau) result(gammar_tau)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma^r_{\tau}\)
        real(real64) :: gammar_tau

        integer(int32) :: i
        real(real64) :: term_tau

        gammar_tau = 0.0d0

        do i = 1, Nr2_terms
            term_tau = nr2(i) * pi**Ir2(i) * Jr2(i) * (tau - 0.5d0)**(Jr2(i) - 1.0d0)
            gammar_tau = gammar_tau + term_tau
        end do

    end function get_gammar_tau_region2

    module pure elemental function get_gammar_tautau_region2(pi, tau) result(gammar_tautau)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma^r_{\tau\tau}\)
        real(real64) :: gammar_tautau

        integer(int32) :: i
        real(real64) :: term_tautau

        gammar_tautau = 0.0d0

        do i = 1, Nr2_terms
            term_tautau = nr2(i) * pi**Ir2(i) * Jr2(i) * (Jr2(i) - 1.0d0) * (tau - 0.5d0)**(Jr2(i) - 2.0d0)
            gammar_tautau = gammar_tautau + term_tautau
        end do

    end function get_gammar_tautau_region2

    module pure elemental function get_gammar_pitau_region2(pi, tau) result(gammar_pitau)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma^r_{\pi\tau}\)
        real(real64) :: gammar_pitau

        integer(int32) :: i
        real(real64) :: term_pitau

        gammar_pitau = 0.0d0

        do i = 1, Nr2_terms
            term_pitau = nr2(i) * Ir2(i) * pi**(Ir2(i) - 1.0d0) * Jr2(i) * (tau - 0.5d0)**(Jr2(i) - 1.0d0)
            gammar_pitau = gammar_pitau + term_pitau
        end do

    end function get_gammar_pitau_region2

end submodule iapws_base2

submodule(physics_material_iapws97_region1) iapws97_base_region1
    implicit none
    !------------------------------------------------------------------------------------------
    ! Region1: Saturated liquid water (IAPWS-IF97)
    !------------------------------------------------------------------------------------------
    integer(int32), parameter :: N1_terms = 34
    real(real64), parameter :: I_r1(N1_terms) = [ & !&
                                0.0d0,  0.0d0,  0.0d0,  0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0,  1.0d0,  1.0d0, & !&
                                1.0d0,  1.0d0,  1.0d0,  1.0d0, 2.0d0, 2.0d0, 2.0d0, 2.0d0,  2.0d0,  3.0d0, & !&
                                3.0d0,  3.0d0,  4.0d0,  4.0d0, 4.0d0, 5.0d0, 8.0d0, 8.0d0, 21.0d0, 23.0d0, & !&
                               29.0d0, 30.0d0, 31.0d0, 32.0d0]
    real(real64), parameter :: J_r1(N1_terms) = [ & !&
                                -2.0d0,  -1.0d0,   0.0d0,   1.0d0,  2.0d0,  3.0d0,   4.0d0,  5.0d0,  -9.0d0,  -7.0d0, & !&
                                -1.0d0,   0.0d0,   1.0d0,   3.0d0, -3.0d0,  0.0d0,   1.0d0,  3.0d0,  17.0d0,  -4.0d0, & !&
                                 0.0d0,   6.0d0,  -5.0d0,  -2.0d0, 10.0d0, -8.0d0, -11.0d0, -6.0d0, -29.0d0, -31.0d0, & !&
                               -38.0d0, -39.0d0, -40.0d0, -41.0d0]
    real(real64), parameter :: n_r1(N1_terms) = [ & !&
                                 0.14632971213167d0,   -0.84548187169114d0,   -0.37563603672040d1, & !&
                                 0.33855169168385d1,   -0.95791963387872d0,    0.15772038513228d0, & !&
                                -0.16616417199501d-1,   0.81214629983568d-3,   0.28319080123804d-3, & !&
                                -0.60706301565874d-3,  -0.18990068218419d-1,  -0.32529748770505d-1, & !&
                                -0.21841717175414d-1,  -0.52838357969930d-4,  -0.47184321073267d-3, & !&
                                -0.30001780793026d-3,   0.47661393906987d-4,  -0.44141845330846d-5, & !&
                                -0.72694996297594d-15, -0.31679644845054d-4,  -0.28270797985312d-5, & !&
                                -0.85205128120103d-9,  -0.22425281908000d-5,  -0.65171222895601d-6, & !&
                                -0.14341729937924d-12, -0.40516996860117d-6,  -0.12734301741641d-8, & !&
                                -0.17424871230634d-9,  -0.68762131295531d-18,  0.14478307828521d-19, & !&
                                 0.26335781662795d-22, -0.11947622640071d-22,  0.18228094581404d-23, & !&
                                -0.93537087292458d-25]
contains

    !> Calculate the dimensionless Gibbs free energy \(\gamma\) for Region 1.
    !> Formula: \(\gamma = \sum n_i (7.1 - \pi)^{I_i} (\tau - 1.222)^{J_i}\)
    module pure elemental function calc_gamma_region1(pi, tau) result(gamma)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Resulting \(\gamma\) value
        real(real64) :: gamma

        integer(int32) :: i
        real(real64) :: term

        gamma = 0.0d0

        do i = 1, N1_terms
            term = n_r1(i) * (7.1d0 - pi)**I_r1(i) * (tau - 1.222d0)**J_r1(i)
            gamma = gamma + term
        end do

    end function calc_gamma_region1

    !> Calculate the first derivative of \(\gamma\) with respect to \(\pi\).
    !> Computes \(\gamma_{\pi} = \left(\frac{\partial \gamma}{\partial \pi}\right)_{\tau}\).
    module pure elemental function calc_gamma_p_region1(pi, tau) result(gamma_p)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma_{\pi}\)
        real(real64) :: gamma_p

        integer(int32) :: i
        real(real64) :: term_pi

        gamma_p = 0.0d0

        do i = 1, N1_terms
            term_pi = -n_r1(i) * I_r1(i) * (7.1d0 - pi)**(I_r1(i) - 1) * (tau - 1.222d0)**J_r1(i)
            gamma_p = gamma_p + term_pi
        end do

    end function calc_gamma_p_region1

    !> Calculate the first derivative of \(\gamma\) with respect to \(\tau\).
    !> Computes \(\gamma_{\tau} = \left(\frac{\partial \gamma}{\partial \tau}\right)_{\pi}\).
    module pure elemental function calc_gamma_t_region1(pi, tau) result(gamma_t)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma_{\tau}\)
        real(real64) :: gamma_t

        integer(int32) :: i
        real(real64) :: term_tau

        gamma_t = 0.0d0

        do i = 1, N1_terms
            term_tau = n_r1(i) * (7.1d0 - pi)**I_r1(i) * J_r1(i) * (tau - 1.222d0)**(J_r1(i) - 1.0d0)
            gamma_t = gamma_t + term_tau
        end do

    end function calc_gamma_t_region1

    !> Calculate the second derivative of \(\gamma\) with respect to \(\pi\).
    !> Computes \(\gamma_{\pi\pi} = \left(\frac{\partial^2 \gamma}{\partial \pi^2}\right)_{\tau}\).
    module pure elemental function calc_gamma_pp_region1(pi, tau) result(gamma_pp)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma_{\pi\pi}\)
        real(real64) :: gamma_pp

        integer(int32) :: i
        real(real64) :: term_pipi

        gamma_pp = 0.0d0

        do i = 1, N1_terms
            term_pipi = n_r1(i) * I_r1(i) * (I_r1(i) - 1.0d0) * (7.1d0 - pi)**(I_r1(i) - 2.0d0) * (tau - 1.222d0)**J_r1(i)
            gamma_pp = gamma_pp + term_pipi
        end do

    end function calc_gamma_pp_region1

    !> Calculate the second derivative of \(\gamma\) with respect to \(\tau\).
    !> Computes \(\gamma_{\tau\tau} = \left(\frac{\partial^2 \gamma}{\partial \tau^2}\right)_{\pi}\).
    module pure elemental function calc_gamma_tt_region1(pi, tau) result(gamma_tt)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma_{\tau\tau}\)
        real(real64) :: gamma_tt

        integer(int32) :: i
        real(real64) :: term_tautau

        gamma_tt = 0.0d0

        do i = 1, N1_terms
            term_tautau = n_r1(i) * (7.1d0 - pi)**I_r1(i) * J_r1(i) * (J_r1(i) - 1.0d0) * (tau - 1.222d0)**(J_r1(i) - 2.0d0)
            gamma_tt = gamma_tt + term_tautau
        end do

    end function calc_gamma_tt_region1

    !> Calculate the mixed second derivative of \(\gamma\).
    !> Computes \(\gamma_{\pi\tau} = \frac{\partial^2 \gamma}{\partial \pi \partial \tau}\).
    module pure elemental function calc_gamma_pt_region1(pi, tau) result(gamma_pt)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma_{\pi\tau}\)
        real(real64) :: gamma_pt

        integer(int32) :: i
        real(real64) :: term_pitau

        gamma_pt = 0.0d0

        do i = 1, N1_terms
            term_pitau = -n_r1(i) * I_r1(i) * (7.1d0 - pi)**(I_r1(i) - 1.0d0) * J_r1(i) * (tau - 1.222d0)**(J_r1(i) - 1.0d0)
            gamma_pt = gamma_pt + term_pitau
        end do

    end function calc_gamma_pt_region1

end submodule iapws97_base_region1

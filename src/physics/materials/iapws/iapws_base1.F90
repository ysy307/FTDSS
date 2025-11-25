submodule(physics_material_iapws) iapws_base1
    implicit none
    !------------------------------------------------------------------------------------------
    ! Reigon1: Saturated liquid water (IAPWS-IF97)
    !------------------------------------------------------------------------------------------
    integer(int32), parameter :: N1_terms = 34
    real(real64), parameter :: I1(N1_terms) = [0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 1.0d0, 1.0d0, &
                                               1.0d0, 1.0d0, 1.0d0, 1.0d0, 2.0d0, 2.0d0, 2.0d0, 2.0d0, 2.0d0, 3.0d0, &
                                               3.0d0, 3.0d0, 4.0d0, 4.0d0, 4.0d0, 5.0d0, 8.0d0, 8.0d0, 21.0d0, 23.0d0, &
                                               29.0d0, 30.0d0, 31.0d0, 32.0d0]
    real(real64), parameter :: J1(N1_terms) = [-2.0d0, -1.0d0, 0.0d0, 1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0, -9.0d0, -7.0d0, &
                                               -1.0d0, 0.0d0, 1.0d0, 3.0d0, -3.0d0, 0.0d0, 1.0d0, 3.0d0, 17.0d0, -4.0d0, &
                                               0.0d0, 6.0d0, -5.0d0, -2.0d0, 10.0d0, -8.0d0, -11.0d0, -6.0d0, -29.0d0, -31.0d0, &
                                               -38.0d0, -39.0d0, -40.0d0, -41.0d0]
    real(real64), parameter :: n1(N1_terms) = &
                               [ 0.14632971213167d0,   -0.84548187169114d0,   -0.37563603672040d1, & !&
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
    module pure elemental function get_gamma_region1(pi, tau) result(gamma)
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
            term = n1(i) * (7.1d0 - pi)**I1(i) * (tau - 1.222d0)**J1(i)
            gamma = gamma + term
        end do

    end function get_gamma_region1

    !> Calculate the first derivative of \(\gamma\) with respect to \(\pi\).
    !> Computes \(\gamma_{\pi} = \left(\frac{\partial \gamma}{\partial \pi}\right)_{\tau}\).
    module pure elemental function get_gamma_pi_region1(pi, tau) result(gamma_pi)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma_{\pi}\)
        real(real64) :: gamma_pi

        integer(int32) :: i
        real(real64) :: term_pi

        gamma_pi = 0.0d0

        do i = 1, N1_terms
            term_pi = -n1(i) * I1(i) * (7.1d0 - pi)**(I1(i) - 1) * (tau - 1.222d0)**J1(i)
            gamma_pi = gamma_pi + term_pi
        end do

    end function get_gamma_pi_region1

    !> Calculate the first derivative of \(\gamma\) with respect to \(\tau\).
    !> Computes \(\gamma_{\tau} = \left(\frac{\partial \gamma}{\partial \tau}\right)_{\pi}\).
    module pure elemental function get_gamma_tau_region1(pi, tau) result(gamma_tau)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma_{\tau}\)
        real(real64) :: gamma_tau

        integer(int32) :: i
        real(real64) :: term_tau

        gamma_tau = 0.0d0

        do i = 1, N1_terms
            term_tau = n1(i) * (7.1d0 - pi)**I1(i) * J1(i) * (tau - 1.222d0)**(J1(i) - 1.0d0)
            gamma_tau = gamma_tau + term_tau
        end do

    end function get_gamma_tau_region1

    !> Calculate the second derivative of \(\gamma\) with respect to \(\pi\).
    !> Computes \(\gamma_{\pi\pi} = \left(\frac{\partial^2 \gamma}{\partial \pi^2}\right)_{\tau}\).
    module pure elemental function get_gamma_pipi_region1(pi, tau) result(gamma_pipi)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma_{\pi\pi}\)
        real(real64) :: gamma_pipi

        integer(int32) :: i
        real(real64) :: term_pipi

        gamma_pipi = 0.0d0

        do i = 1, N1_terms
            term_pipi = n1(i) * I1(i) * (I1(i) - 1.0d0) * (7.1d0 - pi)**(I1(i) - 2.0d0) * (tau - 1.222d0)**J1(i)
            gamma_pipi = gamma_pipi + term_pipi
        end do

    end function get_gamma_pipi_region1

    !> Calculate the second derivative of \(\gamma\) with respect to \(\tau\).
    !> Computes \(\gamma_{\tau\tau} = \left(\frac{\partial^2 \gamma}{\partial \tau^2}\right)_{\pi}\).
    module pure elemental function get_gamma_tautau_region1(pi, tau) result(gamma_tautau)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma_{\tau\tau}\)
        real(real64) :: gamma_tautau

        integer(int32) :: i
        real(real64) :: term_tautau

        gamma_tautau = 0.0d0

        do i = 1, N1_terms
            term_tautau = n1(i) * (7.1d0 - pi)**I1(i) * J1(i) * (J1(i) - 1.0d0) * (tau - 1.222d0)**(J1(i) - 2.0d0)
            gamma_tautau = gamma_tautau + term_tautau
        end do

    end function get_gamma_tautau_region1

    !> Calculate the mixed second derivative of \(\gamma\).
    !> Computes \(\gamma_{\pi\tau} = \frac{\partial^2 \gamma}{\partial \pi \partial \tau}\).
    module pure elemental function get_gamma_pitau_region1(pi, tau) result(gamma_pitau)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma_{\pi\tau}\)
        real(real64) :: gamma_pitau

        integer(int32) :: i
        real(real64) :: term_pitau

        gamma_pitau = 0.0d0

        do i = 1, N1_terms
            term_pitau = -n1(i) * I1(i) * (7.1d0 - pi)**(I1(i) - 1.0d0) * J1(i) * (tau - 1.222d0)**(J1(i) - 1.0d0)
            gamma_pitau = gamma_pitau + term_pitau
        end do

    end function get_gamma_pitau_region1

    !> Calculate the third derivative \(\gamma_{\pi\pi\tau}\).
    !> Computes \(\frac{\partial^3 \gamma}{\partial \pi^2 \partial \tau}\).
    module pure elemental function get_gamma_pipi_tau_region1(pi, tau) result(val)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma_{\pi\pi\tau}\)
        real(real64) :: val

        integer(int32) :: i

        val = 0.0d0
        do i = 1, N1_terms
            val = val + n1(i) * I1(i) * (I1(i) - 1.0d0) * (7.1d0 - pi)**(I1(i) - 2.0d0) &
                  * J1(i) * (tau - 1.222d0)**(J1(i) - 1.0d0)
        end do
    end function get_gamma_pipi_tau_region1

    !> Calculate the third derivative \(\gamma_{\pi\tau\tau}\).
    !> Computes \(\frac{\partial^3 \gamma}{\partial \pi \partial \tau^2}\).
    module pure elemental function get_gamma_pi_tautau_region1(pi, tau) result(val)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma_{\pi\tau\tau}\)
        real(real64) :: val

        integer(int32) :: i

        val = 0.0d0
        do i = 1, N1_terms
            val = val - n1(i) * I1(i) * (7.1d0 - pi)**(I1(i) - 1.0d0) &
                  * J1(i) * (J1(i) - 1.0d0) * (tau - 1.222d0)**(J1(i) - 2.0d0)
        end do
    end function get_gamma_pi_tautau_region1

    !> Calculate the third derivative \(\gamma_{\tau\tau\tau}\).
    !> Computes \(\frac{\partial^3 \gamma}{\partial \tau^3}\).
    module pure elemental function get_gamma_tautau_tau_region1(pi, tau) result(val)
        implicit none
        !> Dimensionless pressure \(\pi\)
        real(real64), intent(in) :: pi
        !> Dimensionless temperature \(\tau\)
        real(real64), intent(in) :: tau
        !> Derivative \(\gamma_{\tau\tau\tau}\)
        real(real64) :: val

        integer(int32) :: i

        val = 0.0d0
        do i = 1, N1_terms
            val = val + n1(i) * (7.1d0 - pi)**I1(i) &
                  * J1(i) * (J1(i) - 1.0d0) * (J1(i) - 2.0d0) * (tau - 1.222d0)**(J1(i) - 3.0d0)
        end do
    end function get_gamma_tautau_tau_region1
end submodule iapws_base1

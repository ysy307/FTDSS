submodule(models_hcf) hcf_vg
    implicit none
contains

    !> @brief Mualem relative permeability for the van Genuchten retention model.
    !>
    !> Mathematical definition (van Genuchten et al., 1991; Hansson et al., 2004,
    !> Eq. [3]) for INDEPENDENT \(m\) and \(n\):
    !> \[ k_r = S_e^{l}\,\bigl[I_{\zeta}(m + 1/n,\; 1 - 1/n)\bigr]^2, \qquad
    !>    \zeta = S_e^{1/m} \]
    !> where \(I_x(a,b)\) is the regularized incomplete beta function.  Only when
    !> \(m = 1 - 1/n\) does this reduce to the closed form
    !> \(k_r = S_e^{l}[1-(1-S_e^{1/m})^m]^2\); using the closed form with an
    !> independent \(m\) (e.g. the Kanagawa sandy loam fit m=0.2, n=1.48)
    !> underestimates \(k_r\) by up to an order of magnitude in the wet-to-medium
    !> suction range and correspondingly suppresses cryosuction redistribution.
    !>
    !> Assumptions: \(n > 1\) (otherwise the Mualem integral is ill-defined and
    !> the closed form is used as a fallback).  Computational complexity: O(1);
    !> the continued fraction converges in a few dozen terms.  Failure behavior:
    !> returns the closed-form value if the beta continued fraction fails to
    !> converge (does not abort).
    subroutine calc_kr_vg(alpha1, n1, m1, l, h, kr)
        implicit none
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: m1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: kr

        real(real64) :: Sw, p, q, zeta
        real(real64), parameter :: M_CONSISTENT_TOL = 1.0d-9

        if (h < 0.0d0) then
            Sw = (1.0d0 + (-alpha1 * h)**n1)**(-m1)
        else
            Sw = 1.0d0
        end if

        if (abs(m1 - (1.0d0 - 1.0d0 / n1)) <= M_CONSISTENT_TOL .or. n1 <= 1.0d0) then
            kr = Sw**l * (1.0d0 - (1.0d0 - Sw**(1.0d0 / m1))**m1)**2
        else
            p = m1 + 1.0d0 / n1
            q = 1.0d0 - 1.0d0 / n1
            zeta = Sw**(1.0d0 / m1)
            kr = Sw**l * incomplete_beta_regularized(p, q, zeta)**2
        end if

    end subroutine calc_kr_vg

    !----------------------------------------------------------------------------------------------------
    ! Wrapper of calculating kr for van-Genuchten model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module subroutine calc_kr_base_vg(self, h, kr)
        implicit none
        class(type_hcf_base_vg), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: kr

        associate (params => self%parent%config)
            call calc_kr_vg(params%alpha1, params%n1, params%m1, params%l, h, kr)
        end associate
    end subroutine calc_kr_base_vg

    !> @brief Regularized incomplete beta function \(I_x(a,b)\).
    !>
    !> Continued-fraction evaluation (modified Lentz), accurate to ~1e-14 for
    !> a, b > 0 and x in [0,1].  Cost: O(1) (< 200 iterations).
    pure function incomplete_beta_regularized(a, b, x) result(res)
        implicit none
        real(real64), intent(in) :: a, b, x
        real(real64) :: res

        real(real64) :: bt

        if (x <= 0.0d0) then
            res = 0.0d0
            return
        end if
        if (x >= 1.0d0) then
            res = 1.0d0
            return
        end if

        bt = exp(log_gamma(a + b) - log_gamma(a) - log_gamma(b) &
                 + a * log(x) + b * log(1.0d0 - x))

        if (x < (a + 1.0d0) / (a + b + 2.0d0)) then
            res = bt * beta_continued_fraction(a, b, x) / a
        else
            res = 1.0d0 - bt * beta_continued_fraction(b, a, 1.0d0 - x) / b
        end if
    end function incomplete_beta_regularized

    !> Continued fraction for the incomplete beta function (modified Lentz).
    pure function beta_continued_fraction(a, b, x) result(h)
        implicit none
        real(real64), intent(in) :: a, b, x
        real(real64) :: h

        integer(int32), parameter :: MAX_ITER = 200
        real(real64), parameter :: EPS_CF = 3.0d-15
        real(real64), parameter :: FPMIN = 1.0d-300

        real(real64) :: qab, qap, qam, c, d, aa, del
        integer(int32) :: m, m2

        qab = a + b
        qap = a + 1.0d0
        qam = a - 1.0d0
        c = 1.0d0
        d = 1.0d0 - qab * x / qap
        if (abs(d) < FPMIN) d = FPMIN
        d = 1.0d0 / d
        h = d

        do m = 1, MAX_ITER
            m2 = 2 * m
            aa = real(m, real64) * (b - real(m, real64)) * x / &
                 ((qam + real(m2, real64)) * (a + real(m2, real64)))
            d = 1.0d0 + aa * d
            if (abs(d) < FPMIN) d = FPMIN
            c = 1.0d0 + aa / c
            if (abs(c) < FPMIN) c = FPMIN
            d = 1.0d0 / d
            h = h * d * c
            aa = -(a + real(m, real64)) * (qab + real(m, real64)) * x / &
                 ((a + real(m2, real64)) * (qap + real(m2, real64)))
            d = 1.0d0 + aa * d
            if (abs(d) < FPMIN) d = FPMIN
            c = 1.0d0 + aa / c
            if (abs(c) < FPMIN) c = FPMIN
            d = 1.0d0 / d
            del = d * c
            h = h * del
            if (abs(del - 1.0d0) < EPS_CF) exit
        end do
    end function beta_continued_fraction

end submodule hcf_vg

submodule(models_hcf) hcf_mvg
    implicit none
contains

    !----------------------------------------------------------------------------------------------------
    ! Calculate kr for Modified van-Genuchten model
    subroutine calc_kr_mvg(theta_s, theta_r, alpha1, n1, m1, l, h_crit, h, kr)
        implicit none
        real(real64), intent(in) :: theta_s
        real(real64), intent(in) :: theta_r
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: m1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: h_crit
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: kr

        real(real64) :: Se, Se_crit
        real(real64) :: numer, denom
        real(real64) :: term_numer, term_denom

        if (h < h_crit) then
            ! Compute Se(h) and Se(h_crit) directly as (1+|ah|^n)^-m
            ! (dividing by (theta_s - theta_r) cancels in the ratio)
            Se = (1.0d0 + abs(alpha1 * h)**n1)**(-m1)
            Se_crit = (1.0d0 + abs(alpha1 * h_crit)**n1)**(-m1)

            ! Mualem integral numerator (at current h)
            !    Term = [ 1 - (1 - Se^(1/m))^m ]^2
            term_numer = 1.0d0 - Se**(1.0d0 / m1)
            if (term_numer < 0.0d0) term_numer = 0.0d0
            numer = (1.0d0 - term_numer**m1)**2.0d0

            ! Mualem integral denominator (normalization at h_crit)
            term_denom = 1.0d0 - Se_crit**(1.0d0 / m1)
            if (term_denom < 0.0d0) term_denom = 0.0d0
            denom = (1.0d0 - term_denom**m1)**2.0d0

            kr = (Se / Se_crit)**l * (numer / denom)
        else
            kr = 1.0d0
        end if

    end subroutine calc_kr_mvg

    !----------------------------------------------------------------------------------------------------
    ! Wrapper of calculating kr for Modified van-Genuchten model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module subroutine calc_kr_base_mvg(self, h, kr)
        implicit none
        class(type_hcf_base_mvg), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: kr

        associate (params => self%parent%config)
            call calc_kr_mvg(params%theta_s, params%theta_r, params%alpha1, params%n1, params%m1, params%l, params%h_crit, h, kr)
        end associate

    end subroutine calc_kr_base_mvg

end submodule hcf_mvg

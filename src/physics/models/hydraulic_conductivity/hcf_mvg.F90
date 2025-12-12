submodule(physics_models_hcf) hcf_mvg
    implicit none
contains

    !----------------------------------------------------------------------------------------------------
    ! Calculate kr for Modified van-Genuchten model
    !----------------------------------------------------------------------------------------------------
    pure elemental subroutine calc_kr_mvg(theta_s, theta_r, alpha1, n1, m1, l, h_crit, h, kr)
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
        real(real64) :: Sw, Qm

        Qm = theta_r + (theta_s - theta_r) * (1.0d0 + (-alpha1 * h_crit)**n1)**(-m1)

        if (h < h_crit) then
            Sw = (theta_s - theta_r) / (Qm - theta_r) * (1.0d0 + abs(alpha1 * h)**n1)**(-m1)
            kr = Sw**l * ((1.0d0 - (1.0d0 - Sw**(1.0d0 / m1))**m1) / (1.0d0 - (1.0d0 - 1.0d0**(1.0d0 / m1))**m1))**2
        else
            kr = 1.0d0
        end if

    end subroutine calc_kr_mvg

    !----------------------------------------------------------------------------------------------------
    ! Wrapper of calculating kr for Modified van-Genuchten model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module pure elemental subroutine calc_kr_base_mvg(self, h, kr)
        implicit none
        class(type_hcf_base_mvg), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: kr

        associate (params => self%parent%params)
            call calc_kr_mvg(params%theta_s, params%theta_r, params%alpha1, params%n1, params%m1, params%l, params%h_crit, h, kr)
        end associate

    end subroutine calc_kr_base_mvg

end submodule hcf_mvg

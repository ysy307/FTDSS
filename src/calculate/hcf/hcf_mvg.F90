submodule(Calculate_hcf) Calculate_hcf_mvg
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Constructe each types by using Modified van-Genuchten model
    !----------------------------------------------------------------------------------------------------
    module function construct_type_hcf_base_mvg(theta_s, theta_r, alpha1, n1, l, h_crit) result(structure)
        implicit none
        real(real64), intent(in) :: theta_s
        real(real64), intent(in) :: theta_r
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: h_crit
        class(abst_hcf_base), allocatable :: structure

        if (allocated(structure)) deallocate (structure)
        allocate (type_hcf_base_vg :: structure)

        structure%theta_s = theta_s
        structure%theta_r = theta_r
        structure%alpha1 = alpha1
        structure%n1 = n1
        structure%m1 = 1.0d0 - 1.0d0 / n1
        structure%l = l
        structure%h_crit = h_crit

    end function construct_type_hcf_base_mvg

    !----------------------------------------------------------------------------------------------------
    ! Calculate kr for Modified van-Genuchten model
    !----------------------------------------------------------------------------------------------------
    pure elemental function calc_kr_mvg(theta_s, theta_r, alpha1, n1, m1, l, h_crit, h) result(kr)
        implicit none
        real(real64), intent(in) :: theta_s
        real(real64), intent(in) :: theta_r
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: m1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: h_crit
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: s_w, theta_m

        theta_m = theta_r + (theta_s - theta_r) * (1.0d0 + (-alpha1 * h_crit)**n1)**(-m1)

        if (h < h_crit) then
            s_w = (theta_s - theta_r) / (theta_m - theta_r) * (1.0d0 + abs(alpha1 * h)**n1)**(-m1)
            kr = s_w**l * ((1.0d0 - (1.0d0 - s_w**(1.0d0 / m1))**m1) / (1.0d0 - (1.0d0 - 1.0d0**(1.0d0 / m1))**m1))**2.0d0
        else
            kr = 1.0d0
        end if

    end function calc_kr_mvg

    !----------------------------------------------------------------------------------------------------
    ! Wrapper of calculating kr for Modified van-Genuchten model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module pure elemental function calc_kr_base_mvg(self, h) result(kr)
        implicit none
        class(type_hcf_base_mvg), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = calc_kr_mvg(self%theta_s, self%theta_r, self%alpha1, self%n1, self%m1, self%l, self%h_crit, h)

    end function calc_kr_base_mvg

end submodule calculate_hcf_mvg

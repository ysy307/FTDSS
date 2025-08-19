submodule(calculate_hcf) calculate_hcf_dvgch
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Constructe each types by using Modified van-Genuchten model
    !----------------------------------------------------------------------------------------------------
    module function construct_type_hcf_base_dvgch(alpha1, n1, w1, n2, l) result(structure)
        implicit none
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: w1
        real(real64), intent(in) :: n2
        real(real64), intent(in) :: l
        class(abst_hcf_base), allocatable :: structure

        if (allocated(structure)) deallocate (structure)
        allocate (type_hcf_base_durner :: structure)

        structure%alpha1 = alpha1
        structure%n1 = n1
        structure%m1 = 1.0d0 - 1.0d0 / n1
        structure%w1 = w1
        structure%n2 = n2
        structure%m2 = 1.0d0 - 1.0d0 / n2
        structure%w2 = 1.0d0 - w1
        structure%l = l

    end function construct_type_hcf_base_dvgch

    !----------------------------------------------------------------------------------------------------
    ! Calculate kr for Modified van-Genuchten model
    !----------------------------------------------------------------------------------------------------
    function calc_kr_dvgch(alpha1, n1, m1, w1, n2, m2, w2, l, h) result(kr)
        implicit none
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1, n2
        real(real64), intent(in) :: m1, m2
        real(real64), intent(in) :: w1, w2
        real(real64), intent(in) :: l
        real(real64), intent(in) :: h
        real(real64) :: kr
        real(real64) :: Sw1, Sw2

        if (h < 0) then
            Sw1 = (1.0d0 + (-alpha1 * h)**n1)**(-m1)
            Sw2 = (1.0d0 + (-alpha1 * h)**n2)**(-m2)
            kr = (w1 * Sw1 + w2 * Sw2)**l &
                 * (w1 * alpha1 * (1.0d0 - (1.0d0 - Sw1**(1.0d0 / m1))**m1) &
                    + w2 * alpha1 * (1.0d0 - (1.0d0 - Sw2**(1.0d0 / m2))**m2))**2.0d0 &
                 / (w1 * alpha1 + w2 * alpha1)**2.0d0
        else
            kr = 1.0d0
        end if

    end function calc_kr_dvgch

    !----------------------------------------------------------------------------------------------------
    ! Wrapper of calculating kr for Modified van-Genuchten model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    module function calc_kr_base_dvgch(self, h) result(kr)
        implicit none
        class(Type_HCF_Base_DVGCH), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64) :: kr

        kr = calc_kr_dvgch(self%alpha1, self%n1, self%m1, self%w1, self%n2, self%m2, self%w2, self%l, h)

    end function calc_kr_base_dvgch

end submodule calculate_hcf_dvgch

submodule(physics_models_hcf) hcf_dvgch
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Calculate kr for Modified van-Genuchten model
    !----------------------------------------------------------------------------------------------------
    pure elemental subroutine calc_kr_dvgch(alpha1, n1, m1, w1, n2, m2, w2, l, h, kr)
        implicit none
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1, n2
        real(real64), intent(in) :: m1, m2
        real(real64), intent(in) :: w1, w2
        real(real64), intent(in) :: l
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: kr
        real(real64) :: Sw1, Sw2

        if (h < 0) then
            Sw1 = (1.0d0 + (-alpha1 * h)**n1)**(-m1)
            Sw2 = (1.0d0 + (-alpha1 * h)**n2)**(-m2)
            kr = (w1 * Sw1 + w2 * Sw2)**l &
                 * (w1 * alpha1 * (1.0d0 - (1.0d0 - Sw1**(1.0d0 / m1))**m1) &
                    + w2 * alpha1 * (1.0d0 - (1.0d0 - Sw2**(1.0d0 / m2))**m2))**2 &
                 / (w1 * alpha1 + w2 * alpha1)**2
        else
            kr = 1.0d0
        end if

    end subroutine calc_kr_dvgch

    !----------------------------------------------------------------------------------------------------
    ! Wrapper of calculating kr for Modified van-Genuchten model bounding different derived types
    !----------------------------------------------------------------------------------------------------
    pure elemental module subroutine calc_kr_base_dvgch(self, h, kr)
        implicit none
        class(Type_HCF_Base_DVGCH), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: kr

        associate (params => self%parent%params)
            call calc_kr_dvgch(params%alpha1, params%n1, params%m1, params%w1, &
                               params%n2, params%m2, params%w2, params%l, h, kr)
        end associate

    end subroutine calc_kr_base_dvgch

end submodule hcf_dvgch

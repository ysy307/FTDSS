submodule(physics_models_hcf) hcf_durner
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Calculate kr for Modified Durner model
    !----------------------------------------------------------------------------------------------------
    subroutine calc_kr_durner(alpha1, n1, m1, w1, alpha2, n2, m2, w2, l, h, kr)
        implicit none
        real(real64), intent(in) :: alpha1, alpha2
        real(real64), intent(in) :: n1, n2
        real(real64), intent(in) :: m1, m2
        real(real64), intent(in) :: w1, w2
        real(real64), intent(in) :: l
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: kr
        real(real64) :: Sw1, Sw2

        if (h < 0.0d0) then
            Sw1 = (1.0d0 + (-alpha1 * h)**n1)**(-m1)
            Sw2 = (1.0d0 + (-alpha2 * h)**n2)**(-m2)
            kr = (w1 * Sw1 + w2 * Sw2)**l * &
                 (w1 * alpha1 * (1.0d0 - (1.0d0 - Sw1**(1.0d0 / m1))**m1) &
                  + w2 * alpha2 * (1.0d0 - (1.0d0 - Sw2**(1.0d0 / m2))**m2))**2 / &
                 (w1 * alpha1 + w2 * alpha2)**2
        else
            kr = 1.0d0
        end if

    end subroutine calc_kr_durner

    module subroutine calc_kr_base_durner(self, h, kr)
        implicit none
        class(type_hcf_base_durner), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: kr

        associate (params => self%parent%config)
            call calc_kr_durner(params%alpha1, params%n1, params%m1, params%w1, &
                                params%alpha2, params%n2, params%m2, params%w2, params%l, h, kr)
        end associate

    end subroutine calc_kr_base_durner

end submodule hcf_durner

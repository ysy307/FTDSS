submodule(physics_models_hcf) hcf_ko
    implicit none
    real(real64), parameter :: sqrt_2 = sqrt(2.0d0)

contains
    !----------------------------------------------------------------------------------------------------
    ! Calculate kr for Kosugi model
    !----------------------------------------------------------------------------------------------------
    subroutine calc_kr_ko(alpha1, n1, l, h, kr)
        implicit none
        real(real64), intent(in) :: alpha1
        real(real64), intent(in) :: n1
        real(real64), intent(in) :: l
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: kr

        real(real64) :: Sw

        if (h < 0.0d0) then
            Sw = 0.5d0 * erfc(log(h / alpha1) / (n1 * sqrt_2))
            kr = Sw**l * (0.5d0 * erfc(log(h / alpha1) / (n1 * sqrt_2) + n1 / sqrt_2))**2
        else
            kr = 1.0d0
        end if

    end subroutine calc_kr_ko

    module subroutine calc_kr_base_ko(self, h, kr)
        implicit none
        class(type_hcf_base_ko), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: kr

        associate (params => self%parent%params)
            call calc_kr_ko(params%alpha1, params%n1, params%l, h, kr)
        end associate

    end subroutine calc_kr_base_ko

end submodule hcf_ko

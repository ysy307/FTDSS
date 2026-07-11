submodule(models_hcf) hcf_ko
    implicit none
    real(real64), parameter :: sqrt_2 = sqrt(2.0d0)

contains
    module subroutine calc_kr_base_ko(self, h, kr)
        implicit none
        class(type_hcf_base_ko), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: kr

        real(real64) :: effective_saturation

        associate (params => self%parent%config)
            if (h < 0.0d0) then
                effective_saturation = 0.5d0 * erfc(log(h / params%alpha1) / (params%n1 * sqrt_2))
                kr = effective_saturation**params%l * &
                     (0.5d0 * erfc(log(h / params%alpha1) / (params%n1 * sqrt_2) + &
                                   params%n1 / sqrt_2))**2
            else
                kr = 1.0d0
            end if
        end associate
    end subroutine calc_kr_base_ko

end submodule hcf_ko

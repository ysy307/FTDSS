submodule(models_hcf) hcf_mvg
    implicit none
contains

    module subroutine calc_kr_base_mvg(self, h, kr)
        implicit none
        class(type_hcf_base_mvg), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: kr

        real(real64) :: effective_saturation, critical_saturation
        real(real64) :: numerator, denominator
        real(real64) :: numerator_term, denominator_term

        associate (params => self%parent%config)
            if (h < params%h_crit) then
                effective_saturation = (1.0d0 + abs(params%alpha1 * h)**params%n1)**(-params%m1)
                critical_saturation = &
                    (1.0d0 + abs(params%alpha1 * params%h_crit)**params%n1)**(-params%m1)

                numerator_term = max(0.0d0, 1.0d0 - effective_saturation**(1.0d0 / params%m1))
                numerator = (1.0d0 - numerator_term**params%m1)**2
                denominator_term = max(0.0d0, 1.0d0 - critical_saturation**(1.0d0 / params%m1))
                denominator = (1.0d0 - denominator_term**params%m1)**2
                kr = (effective_saturation / critical_saturation)**params%l * numerator / denominator
            else
                kr = 1.0d0
            end if
        end associate
    end subroutine calc_kr_base_mvg

end submodule hcf_mvg

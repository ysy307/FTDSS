submodule(models_hcf) hcf_durner
    implicit none
contains
    module subroutine calc_kr_base_durner(self, h, kr)
        implicit none
        class(type_hcf_base_durner), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: kr

        real(real64) :: saturation_1, saturation_2

        associate (params => self%parent%config)
            if (h < 0.0d0) then
                saturation_1 = (1.0d0 + (-params%alpha1 * h)**params%n1)**(-params%m1)
                saturation_2 = (1.0d0 + (-params%alpha2 * h)**params%n2)**(-params%m2)
                kr = (params%w1 * saturation_1 + params%w2 * saturation_2)**params%l * &
                     (params%w1 * params%alpha1 * &
                      (1.0d0 - (1.0d0 - saturation_1**(1.0d0 / params%m1))**params%m1) + &
                      params%w2 * params%alpha2 * &
                      (1.0d0 - (1.0d0 - saturation_2**(1.0d0 / params%m2))**params%m2))**2 / &
                     (params%w1 * params%alpha1 + params%w2 * params%alpha2)**2
            else
                kr = 1.0d0
            end if
        end associate
    end subroutine calc_kr_base_durner

end submodule hcf_durner

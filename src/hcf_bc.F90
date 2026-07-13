submodule(models_hcf) hcf_bc
    implicit none
contains
    module subroutine calc_kr_base_bc(self, h, kr)
        implicit none
        class(type_hcf_base_bc), intent(in) :: self
        real(real64), intent(in) :: h
        real(real64), intent(inout) :: kr

        real(real64) :: effective_saturation

        associate (params => self%parent%config)
            if (h < params%alpha1) then
                effective_saturation = (h / params%alpha1)**(-params%n1)
            else
                effective_saturation = 1.0d0
            end if
            kr = effective_saturation**(2.0d0 / params%n1 + params%l + 2.0d0)
        end associate
    end subroutine calc_kr_base_bc

end submodule hcf_bc

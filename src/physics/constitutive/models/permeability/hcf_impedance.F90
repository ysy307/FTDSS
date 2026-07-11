submodule(models_hcf) hcf_impedance
    implicit none
contains

    module subroutine calc_impedance_exp(self, Qice, kr)
        implicit none
        class(type_hcf_impedance_exp), intent(in) :: self
        real(real64), intent(in) :: Qice
        real(real64), intent(inout) :: kr

        associate (params => self%parent%config)
            kr = 10.0d0**(-params%omega * Qice)
        end associate

    end subroutine calc_impedance_exp

end submodule hcf_impedance

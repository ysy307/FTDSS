submodule(physics_models_hcf) hcf_impedance
    implicit none
contains

    subroutine calc_impedance_exponential(omega, Qice, impedance)
        implicit none
        real(real64), intent(in) :: omega
        real(real64), intent(in) :: Qice
        real(real64), intent(inout) :: impedance

        impedance = 10.0d0**(-omega * Qice)

    end subroutine calc_impedance_exponential

    module subroutine calc_impedance_exp(self, Qice, kr)
        implicit none
        class(type_hcf_impedance_exp), intent(in) :: self
        real(real64), intent(in) :: Qice
        real(real64), intent(inout) :: kr

        associate (params => self%parent%params)
            call calc_impedance_exponential(params%omega, Qice, kr)
        end associate

    end subroutine calc_impedance_exp

end submodule hcf_impedance

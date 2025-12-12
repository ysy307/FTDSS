submodule(physics_models_hcf) hcf_viscosity
    implicit none
contains
    module subroutine initialize_abst_hcf_viscosity(self, temperature_critical)
        implicit none
        class(abst_hcf_viscosity), intent(inout) :: self
        real(real64), intent(in), optional :: temperature_critical

        real(real64) :: temp_critical

        if (present(temperature_critical)) then
            temp_critical = temperature_critical
        else
            temp_critical = 15.0d0
        end if

        call self%calc_mu(temp_critical, self%mu_zero)
    end subroutine initialize_abst_hcf_viscosity

    module pure elemental subroutine calc_kr_abst_hcf_viscosity(self, temperature, kr)
        implicit none
        class(abst_hcf_viscosity), intent(in) :: self
        real(real64), intent(in) :: temperature
        real(real64), intent(inout) :: kr

        real(real64) :: mu

        call self%calc_viscosity(temperature, mu)
        kr = self%mu_zero / mu

    end subroutine calc_kr_abst_hcf_viscosity

    module pure elemental subroutine calc_mu_exponential(self, temperature, mu)
        implicit none
        class(type_hcf_viscosity_exp), intent(in) :: self
        real(real64), intent(in) :: temperature
        real(real64), intent(inout) :: mu

        mu = 2.1d-6 * exp(1808.5d0 / (temperature + 273.15d0))

    end subroutine calc_mu_exponential

    module pure elemental subroutine calc_mu_exponential_supercooled(self, temperature, mu)
        implicit none
        class(type_hcf_viscosity_supercool), intent(in) :: self
        real(real64), intent(in) :: temperature
        real(real64), intent(inout) :: mu

        mu = 1.3788d-4 * ((273.15d0 + temperature) / 225.66d0 - 1.0d0)**(-1.6438d0)

    end subroutine calc_mu_exponential_supercooled

end submodule hcf_viscosity

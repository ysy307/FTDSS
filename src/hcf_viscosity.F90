submodule(calculate_hcf) calculate_hcf_viscosity
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Constructe the type
    !----------------------------------------------------------------------------------------------------
    module function construct_type_hcf_viscosity(water_viscosity_model) result(structure)
        implicit none
        integer(int32), intent(in) :: water_viscosity_model
        class(abst_hcf_viscosity), allocatable :: structure

        if (allocated(structure)) deallocate (structure)

        select case (water_viscosity_model)
        case (1)
            allocate (type_hcf_viscosity_exp :: structure)
        case (2)
            allocate (type_hcf_viscosity_supercool :: structure)
        end select

        select type (this => structure)
        type is (type_hcf_viscosity_exp)
            this%mu_zero = this%calc_viscosity(15.0d0)
        type is (type_hcf_viscosity_supercool)
            this%mu_zero = this%calc_viscosity(15.0d0)
        end select

    end function construct_type_hcf_viscosity

    !----------------------------------------------------------------------------------------------------
    ! Calculate water viscosity depending on the temperature
    !----------------------------------------------------------------------------------------------------
    module pure elemental function calc_viscosity_exp(self, temperature) result(kr)
        implicit none
        class(type_hcf_viscosity_exp), intent(in) :: self
        real(real64), intent(in) :: temperature
        real(real64) :: kr

        kr = self%mu_zero / calc_mu_exponential(temperature)

    end function calc_viscosity_exp

    module pure elemental function calc_viscosity_supercool(self, temperature) result(kr)
        implicit none
        class(type_hcf_viscosity_supercool), intent(in) :: self
        real(real64), intent(in) :: temperature
        real(real64) :: kr

        kr = self%mu_zero / calc_mu_exponential_supercooled(temperature)

    end function calc_viscosity_supercool

    pure elemental function calc_mu_exponential(temperature) result(viscosity)
        implicit none
        real(real64), intent(in) :: temperature
        real(real64) :: viscosity

        viscosity = 2.1d-6 * exp(1808.5d0 / (temperature + 273.15d0))

    end function calc_mu_exponential

    pure elemental function calc_mu_exponential_supercooled(temperature) result(viscosity)
        implicit none
        real(real64), intent(in) :: temperature
        real(real64) :: viscosity

        viscosity = 1.3788d-4 * ((273.15d0 + temperature) / 225.66d0 - 1.0d0)**(-1.6438d0)

    end function calc_mu_exponential_supercooled

end submodule calculate_hcf_viscosity

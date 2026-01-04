submodule(main_thermal) thermal_coefficients
    implicit none
contains

    module pure elemental subroutine calc_density_water_thermal(self, state, rho_water)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: rho_water

        call self%physics%calc_density_water(state, rho_water)

    end subroutine calc_density_water_thermal

    module pure elemental subroutine update_water_phases_thermal(self, material_id, state)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state

        call self%physics%update_water_phases(material_id, state)

    end subroutine update_water_phases_thermal

end submodule thermal_coefficients

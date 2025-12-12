submodule(physics_materials_density) density_multiphase
    implicit none
contains
    module pure elemental subroutine calc_den_gp_1phase(self, state, density)
        implicit none
        class(type_den_1phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density

        density = self%material1
    end subroutine calc_den_gp_1phase

    module pure elemental subroutine calc_den_gp_2phase(self, state, density)
        implicit none
        class(type_den_2phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density

        real(real64) :: phi1, phi2
        real(real64) :: density_water

        call self%get_phi(state, phi1, phi2)
        call self%calc_water_density(state, density_water)

        call calc_den_2(self%material1, phi1, density_water, phi2, density)
    end subroutine calc_den_gp_2phase

    module pure elemental subroutine calc_den_gp_3phase(self, state, density)
        implicit none
        class(type_den_3phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density

        real(real64) :: phi1, phi2, phi3
        real(real64) :: density_water, density_ice

        call self%get_phi(state, phi1, phi2, phi3)

        call self%calc_water_density(state, density_water)
        call self%calc_ice_density(state, density_ice)

        call calc_den_3(self%material1, phi1, density_water, phi2, density_ice, phi3, density)
    end subroutine calc_den_gp_3phase

    module pure elemental subroutine calc_den_gp_4phase(self, state, density)
        implicit none
        class(type_den_4phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density

        real(real64) :: phi1, phi2, phi3, phi4
        real(real64) :: density_water, density_ice, density_vapor

        call self%get_phi(state, phi1, phi2, phi3, phi4)

        call self%calc_water_density(state, density_water)
        call self%calc_ice_density(state, density_ice)
        call self%calc_vapor_density(state, density_vapor)

        call calc_den_4(self%material1, phi1, density_water, phi2, density_ice, phi3, density_vapor, phi4, density)
    end subroutine calc_den_gp_4phase

end submodule density_multiphase

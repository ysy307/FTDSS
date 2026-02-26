submodule(constitutive_materials_specific_heat) specific_heat_multiphase
    implicit none
contains
    module subroutine calc_sph_gp_1phase(self, state, specific_heat)
        implicit none
        class(type_sph_1phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: specific_heat

        specific_heat = self%material1
    end subroutine calc_sph_gp_1phase

    module subroutine calc_sph_gp_2phase(self, state, specific_heat)
        implicit none
        class(type_sph_2phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: specific_heat

        real(real64) :: phi1, phi2
        real(real64) :: sph_water

        call self%get_phi(state, phi1, phi2)

        call self%calc_water_cp(state, sph_water)

        call calc_sph_2(self%material1, phi1, sph_water, phi2, specific_heat)
    end subroutine calc_sph_gp_2phase

    module subroutine calc_sph_gp_3phase(self, state, specific_heat)
        implicit none
        class(type_sph_3phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: specific_heat

        real(real64) :: phi1, phi2, phi3
        real(real64) :: sph_water, sph_ice

        call self%get_phi(state, phi1, phi2, phi3)

        call self%calc_water_cp(state, sph_water)
        call self%calc_ice_cp(state, sph_ice)

        call calc_sph_3(self%material1, phi1, sph_water, phi2, sph_ice, phi3, specific_heat)
    end subroutine calc_sph_gp_3phase

    module subroutine calc_sph_gp_4phase(self, state, specific_heat)
        implicit none
        class(type_sph_4phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: specific_heat

        real(real64) :: phi1, phi2, phi3, phi4
        real(real64) :: sph_water, sph_ice, sph_vapor

        call self%get_phi(state, phi1, phi2, phi3, phi4)

        call self%calc_water_cp(state, sph_water)
        call self%calc_ice_cp(state, sph_ice)
        call self%calc_vapor_cp(state, sph_vapor)

        call calc_sph_4(self%material1, phi1, sph_water, phi2, sph_ice, phi3, sph_vapor, phi4, specific_heat)
    end subroutine calc_sph_gp_4phase

end submodule specific_heat_multiphase

submodule(physics_material_specific_heat) specific_heat_4phase
    implicit none
contains

    module subroutine initialize_sph_4phase(self, material_id, physics_info, water, ice)
        implicit none
        class(type_sph_4phase), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_physics_info), intent(in) :: physics_info
        type(type_iapws97), intent(in), target :: water
        type(type_iapws06), intent(in), target :: ice

        self%material_id = material_id

        self%material1 = physics_info%solid
        self%material2 = physics_info%water
        self%material3 = physics_info%ice
        self%material4 = physics_info%vapor

        self%water => water
        self%ice => ice

    end subroutine initialize_sph_4phase

    module pure elemental subroutine calc_sph_gp_4phase(self, state, specific_heat)
        implicit none
        class(type_sph_4phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: specific_heat

        real(real64) :: phi1, phi2, phi3, phi4
        real(real64) :: temp_K
        real(real64) :: sph_water, sph_ice, sph_vapor

        phi1 = 1.0d0 - state%porosity
        phi2 = state%water_content
        phi3 = state%ice_content
        phi4 = 1.0d0 - phi1 - phi2 - phi3

        temp_K = state%temperature + TtoK
        if (associated(self%water)) then
            call self%water%calc_cp(temp_K, state%pressure, sph_water)
            call self%water%calc_saturation_cp(temp_K, sph_vapor)
        else
            sph_water = self%material2
        end if

        if (associated(self%ice)) then
            call self%ice%calc_cp(temp_K, state%pressure, sph_ice)
        else
            sph_ice = self%material3
        end if

        call calc_sph_4(self%material1, phi1, sph_water, phi2, sph_ice, phi3, sph_vapor, phi4, specific_heat)
    end subroutine calc_sph_gp_4phase

end submodule specific_heat_4phase

submodule(physics_material_specific_heat) specific_heat_3phase
    implicit none
contains

    module subroutine initialize_sph_3phase(self, material_id, physics_info, water, ice)
        implicit none
        class(type_sph_3phase), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_physics_info), intent(in) :: physics_info
        type(type_iapws97), intent(in), target :: water
        type(type_iapws06), intent(in), target :: ice

        self%material_id = material_id

        self%material1 = physics_info%solid
        self%material2 = physics_info%water
        self%material3 = physics_info%ice

        self%water => water
        self%ice => ice

    end subroutine initialize_sph_3phase

    module pure elemental subroutine calc_sph_gp_3phase(self, state, specific_heat)
        implicit none
        class(type_sph_3phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: specific_heat

        real(real64) :: phi1, phi2, phi3
        real(real64) :: temp_K
        real(real64) :: sph_water, sph_ice

        phi1 = 1.0d0 - state%porosity
        phi2 = state%water_content
        phi3 = state%ice_content

        temp_K = state%temperature + TtoK
        if (associated(self%water)) then
            call self%water%calc_cp(temp_K, state%pressure, sph_water)
        else
            sph_water = self%material2
        end if

        if (associated(self%ice)) then
            call self%ice%calc_cp(temp_K, state%pressure, sph_ice)
        else
            sph_ice = self%material3
        end if

        call calc_sph_3(self%material1, phi1, sph_water, phi2, sph_ice, phi3, specific_heat)
    end subroutine calc_sph_gp_3phase

end submodule specific_heat_3phase

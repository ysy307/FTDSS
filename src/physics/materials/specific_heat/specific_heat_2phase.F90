submodule(physics_material_specific_heat) specific_heat_2phase
    implicit none
contains

    module subroutine initialize_sph_2phase(self, material_id, phase_info, water, ice)
        implicit none
        class(type_sph_2phase), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_physics_phase), intent(in) :: phase_info
        type(type_iapws97), intent(in), target :: water
        type(type_iapws06), intent(in), target :: ice

        self%material_id = material_id

        self%material1 = phase_info%solid
        self%material2 = phase_info%water
        self%material3 = phase_info%ice

        self%water => water
        self%ice => ice

    end subroutine initialize_sph_2phase

    module pure elemental subroutine calc_sph_gp_2phase(self, state, specific_heat)
        implicit none
        class(type_sph_2phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: specific_heat

        real(real64) :: phi1, phi2
        real(real64) :: temp_K
        real(real64) :: sph_water

        phi1 = 1.0d0 - state%porosity
        phi2 = state%porosity

        temp_K = state%temperature + TtoK
        if (associated(self%water)) then
            call self%water%calc_cp(temp_K, state%pressure, sph_water)
        else
            sph_water = self%material2
        end if

        call calc_sph_2(self%material1, phi1, sph_water, phi2, specific_heat)
    end subroutine calc_sph_gp_2phase

end submodule specific_heat_2phase

submodule(physics_material_density) density_4phase
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct each type of density
    !----------------------------------------------------------------------------------------------------
    module subroutine initialize_type_den_4phase(self, material_id, physics_info, water, ice)
        implicit none
        class(type_den_4phase), intent(inout) :: self
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

    end subroutine initialize_type_den_4phase

    module pure elemental subroutine calc_den_gp_4phase(self, state, density)
        implicit none
        class(type_den_4phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density

        real(real64) :: phi1, phi2, phi3, phi4
        real(real64) :: temp_K
        real(real64) :: density_water, density_ice, density_vapor

        phi1 = 1.0d0 - state%porosity
        phi2 = state%water_content
        phi3 = state%ice_content
        phi4 = 1.0d0 - phi1 - phi2 - phi3

        temp_K = state%temperature + TtoK
        if (associated(self%water)) then
            call self%water%calc_rho(temp_K, state%pressure, density_water)
            call self%water%calc_saturation_density(temp_K, density_vapor)
            density_vapor = max(density_vapor * state%relative_humidity, 1.0d-8)
        else
            density_water = self%material2
            density_vapor = self%material4
        end if

        if (associated(self%ice)) then
            call self%ice%calc_rho(temp_K, state%pressure, density_ice)
        else
            density_ice = self%material3
        end if

        call calc_den_4(self%material1, phi1, density_water, phi2, density_ice, phi3, density_vapor, phi4, density)
    end subroutine calc_den_gp_4phase

end submodule density_4phase

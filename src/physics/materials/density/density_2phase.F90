submodule(physics_material_density) density_2phase
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct each type of density
    !----------------------------------------------------------------------------------------------------
    module subroutine initialize_type_den_2phase(self, material_id, physics_info, water, ice)
        implicit none
        class(type_den_2phase), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_physics_info), intent(in) :: physics_info
        type(type_iapws97), intent(in), target :: water
        type(type_iapws06), intent(in), target :: ice

        self%material_id = material_id

        self%material1 = physics_info%solid
        self%material2 = physics_info%water

        self%water => water

    end subroutine initialize_type_den_2phase

    module pure elemental subroutine calc_den_gp_2phase(self, state, density)
        implicit none
        class(type_den_2phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: density

        real(real64) :: phi1, phi2, phi3
        real(real64) :: temp_K
        real(real64) :: density_water, density_ice

        phi1 = 1.0d0 - state%porosity
        phi2 = state%porosity

        temp_K = state%temperature + TtoK
        if (associated(self%water)) then
            call self%water%calc_rho(temp_K, state%pressure, density_water)
        else
            density_water = self%material2
        end if

        call calc_den_2(self%material1, phi1, density_water, phi2, density)
    end subroutine calc_den_gp_2phase

end submodule density_2phase

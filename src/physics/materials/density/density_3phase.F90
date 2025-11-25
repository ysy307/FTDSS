submodule(physics_material_density) density_3phase
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct each type of density
    !----------------------------------------------------------------------------------------------------
    module subroutine initialize_type_den_3phase(self, material_id, phase_info)
        implicit none
        class(type_den_3phase), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_physics_phase), intent(in) :: phase_info

        self%material_id = material_id

        self%material1 = phase_info%solid
        self%material2 = phase_info%water
        self%material3 = phase_info%ice

    end subroutine initialize_type_den_3phase

    module pure elemental function calc_den_gauss_point_3phase(self, state) result(density)
        implicit none
        class(type_den_3phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: density

        real(real64) :: phi1, phi2, phi3

        phi1 = 1.0d0 - state%porosity
        if (phi2 > state%porosity) phi2 = state%porosity
        if (phi2 < 0.0d0) phi2 = 0.0d0
        phi3 = 1.0d0 - phi1 - phi2

        density = calc_den_3(self%material1, phi1, self%material2, phi2, self%material3, phi3)
    end function calc_den_gauss_point_3phase

end submodule density_3phase

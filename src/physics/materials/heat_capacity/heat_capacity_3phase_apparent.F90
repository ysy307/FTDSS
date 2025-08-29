submodule(physics_material_heat_capacity) heat_capacity_3phase_apparent
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct each type of density
    !----------------------------------------------------------------------------------------------------
    module function construct_type_vhc_3phase_apparent(input, material_id) result(property)
        implicit none
        class(abst_vhc), allocatable :: property
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id

        if (allocated(property)) deallocate (property)
        allocate (type_vhc_3phase_apparent :: property)

        property%material_id = material_id

        property%material1 = input%basic%materials(material_id)%thermal%density(1) * &
                             input%basic%materials(material_id)%thermal%specific_heat(1)
        property%material2 = input%basic%materials(material_id)%thermal%density(2) * &
                             input%basic%materials(material_id)%thermal%specific_heat(2)
        property%material3 = input%basic%materials(material_id)%thermal%density(3) * &
                             input%basic%materials(material_id)%thermal%specific_heat(3)

    end function construct_type_vhc_3phase_apparent

    module pure elemental function calc_vhc_gauss_point_3phase_apparent(self, state) result(VHC)
        implicit none
        class(type_vhc_3phase_apparent), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: VHC

        real(real64) :: phi1, phi2, phi3

        phi1 = 1.0d0 - state%porosity
        phi2 = state%water_content
        if (phi2 > state%porosity) phi2 = state%porosity
        if (phi2 < 0.0d0) phi2 = 0.0d0
        phi3 = 1.0d0 - phi1 - phi2

        VHC = calc_vhc_3a(self%material1, phi1, self%material2, phi2, self%material3, phi3, &
                          state%latent_heat, state%density_water, state%dQw_dT)
    end function calc_vhc_gauss_point_3phase_apparent

end submodule heat_capacity_3phase_apparent

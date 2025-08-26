submodule(calculate_specific_heat) calc_sph_3phase
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct each type of density
    !----------------------------------------------------------------------------------------------------
    module function construct_sph_3phase(input, material_id) result(property)
        implicit none
        class(abst_sph), allocatable :: property
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id

        if (allocated(property)) deallocate (property)
        allocate (type_sph_3phase :: property)

        property%material_id = material_id

        property%material1 = input%basic%materials(material_id)%thermal%specific_heat(1)
        property%material2 = input%basic%materials(material_id)%thermal%specific_heat(2)
        property%material3 = input%basic%materials(material_id)%thermal%specific_heat(3)

    end function construct_sph_3phase

    module pure elemental function calc_sph_gauss_point_3phase(self, state) result(SPH)
        implicit none
        class(type_sph_3phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: SPH

        real(real64) :: phi1, phi2, phi3

        phi1 = 1.0d0 - state%porosity
        phi2 = state%water_content
        if (phi2 > state%porosity) phi2 = state%porosity
        if (phi2 < 0.0d0) phi2 = 0.0d0
        phi3 = 1.0d0 - phi1 - phi2

        SPH = calc_sph_3(self%material1, phi1, self%material2, phi2, self%material3, phi3)
    end function calc_sph_gauss_point_3phase

end submodule calc_sph_3phase

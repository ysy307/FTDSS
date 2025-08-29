submodule(physics_material_thermal_conductivity) thermal_conductivity_3phase
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct each type of heat conductivity
    !----------------------------------------------------------------------------------------------------
    module function construct_thc_3(input, material_id) result(property)
        implicit none
        class(abst_thc), allocatable :: property
        type(type_input), intent(in) :: Input
        integer(int32), intent(in) :: material_id

        if (allocated(property)) deallocate (property)
        allocate (type_thc_3phase :: property)

        property%material_id = material_id

        property%material1 = input%basic%materials(material_id)%thermal%thermal_conductivity(1)
        property%material2 = input%basic%materials(material_id)%thermal%thermal_conductivity(2)
        property%material3 = input%basic%materials(material_id)%thermal%thermal_conductivity(3)

    end function construct_thc_3

    module pure elemental function calc_thc_gauss_point_3phase(self, state) result(lambda)
        implicit none
        class(type_thc_3phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64) :: lambda

        real(real64) :: phi1, phi2, phi3

        phi1 = 1.0d0 - state%porosity
        phi2 = state%water_content
        if (phi2 > state%porosity) phi2 = state%porosity
        if (phi2 < 0.0d0) phi2 = 0.0d0
        phi3 = 1.0d0 - phi1 - phi2

        lambda = calc_thc_3(self%material1, phi1, self%material2, phi2, self%material3, phi3)

    end function calc_thc_gauss_point_3phase

end submodule thermal_conductivity_3phase

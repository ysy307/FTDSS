submodule(calculate_thermal_conductivity) calc_thc_3phase
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct each type of heat conductivity
    !----------------------------------------------------------------------------------------------------
    module function construct_thc_3(input, i_material) result(property)
        implicit none
        class(abst_thc), allocatable :: property
        type(type_input), intent(in) :: Input
        integer(int32), intent(in) :: i_material

        if (allocated(property)) deallocate (property)
        allocate (type_thc_3phase :: property)

        property%material_id = i_material

        property%material1 = input%basic%materials(i_material)%thermal%thermal_conductivity(1)
        property%material2 = input%basic%materials(i_material)%thermal%thermal_conductivity(2)
        property%material3 = input%basic%materials(i_material)%thermal%thermal_conductivity(3)

    end function construct_thc_3

    module function calc_thc_gauss_point_3phase(self, state) result(lambda)
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

end submodule calc_thc_3phase

submodule(calculate_density) calculate_den_3
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct each type of density
    !----------------------------------------------------------------------------------------------------
    module function construct_den_3phase(iRegion, Input) result(property)
        implicit none
        class(abst_den), allocatable :: property
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        if (allocated(property)) deallocate (property)
        allocate (Type_DEN_3Phase :: property)

        property%material1 = Input%Regions(iRegion)%Thermal%rho(1)
        property%material2 = Input%Regions(iRegion)%Thermal%rho(2)
        property%material3 = Input%Regions(iRegion)%Thermal%rho(3)

    end function construct_den_3phase

    module function calc_den_gauss_point_3phase(self, state) result(density)
        implicit none
        class(type_den_3phase), intent(in) :: self
        type(type_gauss_point_state), intent(in) :: state
        real(real64) :: density

        real(real64) :: phi1, phi2, phi3

        phi1 = 1.0d0 - state%porosity
        phi2 = state%water_content
        phi3 = 1.0d0 - phi1 - phi2

        density = Calc_DEN_3(self%material1, phi1, self%material2, phi2, self%material3, phi3)
    end function calc_den_gauss_point_3phase

end submodule calculate_den_3

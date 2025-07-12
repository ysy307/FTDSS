submodule(calculate_thermal_conductivity) calc_thc_3phase
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct each type of heat conductivity
    !----------------------------------------------------------------------------------------------------
    module function construct_thc_3(iRegion, Input) result(property)
        implicit none
        class(Abst_THC), allocatable :: property
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        if (allocated(property)) deallocate (property)
        allocate (type_thc_3phase :: property)

        property%region_id = iRegion

        property%material1 = Input%Regions(iRegion)%Thermal%lambda(1)
        property%material2 = Input%Regions(iRegion)%Thermal%lambda(2)
        property%material3 = Input%Regions(iRegion)%Thermal%lambda(3)

    end function construct_thc_3

    module function calc_thc_gauss_point_3phase(self, state) result(lambda)
        implicit none
        class(type_thc_3phase), intent(in) :: self
        type(type_gauss_point_state), intent(in) :: state
        real(real64) :: lambda

        real(real64) :: phi1, phi2, phi3

        phi1 = 1.0d0 - state%porosity
        phi2 = state%water_content
        phi3 = 1.0d0 - phi1 - phi2

        lambda = calc_thc_3(self%material1, phi1, self%material2, phi2, self%material3, phi3)

    end function calc_thc_gauss_point_3phase

end submodule calc_thc_3phase

submodule(calculate_specific_heat) calc_sph_3phase
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct each type of density
    !----------------------------------------------------------------------------------------------------
    module function construct_sph_3phase(iRegion, Input) result(property)
        implicit none
        class(Abst_SPH), allocatable :: property
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        if (allocated(property)) deallocate (property)
        allocate (Type_SPH_3Phase :: property)

        property%material1 = Input%Regions(iRegion)%Thermal%c(1)
        property%material2 = Input%Regions(iRegion)%Thermal%c(2)
        property%material3 = Input%Regions(iRegion)%Thermal%c(3)

    end function construct_sph_3phase

    module function calc_sph_gauss_point_3phase(self, state) result(SPH)
        implicit none
        class(Type_SPH_3Phase), intent(in) :: self
        type(type_gauss_point_state), intent(in) :: state
        real(real64) :: SPH

        real(real64) :: phi1, phi2, phi3

        phi1 = 1.0d0 - state%porosity
        phi2 = state%water_content
        phi3 = 1.0d0 - phi1 - phi2

        SPH = calc_sph_3(self%material1, phi1, self%material2, phi2, self%material3, phi3)
    end function calc_sph_gauss_point_3phase

end submodule calc_sph_3phase

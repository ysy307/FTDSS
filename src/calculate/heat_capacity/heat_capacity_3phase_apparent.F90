submodule(calculate_volumetric_heat_capacity) calc_vhc_3phase_apparent
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct each type of density
    !----------------------------------------------------------------------------------------------------
    module function construct_type_vhc_3phase_apparent(iRegion, Input) result(property)
        implicit none
        class(Abst_VHC), allocatable :: property
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        if (allocated(property)) deallocate (property)
        allocate (type_vhc_3phase_apparent :: property)

        property%material1 = Input%Regions(iRegion)%Thermal%Cp(1)
        property%material2 = Input%Regions(iRegion)%Thermal%Cp(2)
        property%material3 = Input%Regions(iRegion)%Thermal%Cp(3)

    end function construct_type_vhc_3phase_apparent

    module function calc_vhc_gauss_point_3phase_apparent_holder(self, state, DEN, LatentHeat, dQi_dT) result(VHC)
        implicit none
        class(type_vhc_3phase_apparent), intent(in) :: self
        type(type_gauss_point_state), intent(in) :: state
        type(holder_dens), intent(in), optional :: DEN
        real(real64), intent(in), optional :: LatentHeat
        real(real64), intent(in), optional :: dQi_dT
        real(real64) :: VHC

        real(real64) :: phi1, phi2, phi3

        phi1 = 1.0d0 - state%porosity
        phi2 = state%water_content
        phi3 = 1.0d0 - phi1 - phi2

        VHC = calc_vhc_3a(self%material1, phi1, self%material2, phi2, self%material3, phi3, &
                          LatentHeat, DEN%p%material3, dQi_dT)
    end function calc_vhc_gauss_point_3phase_apparent_holder

    module function calc_vhc_gauss_point_3phase_apparent_ptr(self, state, DEN, LatentHeat, dQi_dT) result(VHC)
        implicit none
        class(type_vhc_3phase_apparent), intent(in) :: self
        type(type_gauss_point_state), intent(in) :: state
        class(abst_den), pointer, intent(in), optional :: DEN
        real(real64), intent(in), optional :: LatentHeat
        real(real64), intent(in), optional :: dQi_dT
        real(real64) :: VHC

        real(real64) :: phi1, phi2, phi3

        phi1 = 1.0d0 - state%porosity
        phi2 = state%water_content
        phi3 = 1.0d0 - phi1 - phi2

        VHC = calc_vhc_3a(self%material1, phi1, self%material2, phi2, self%material3, phi3, &
                          LatentHeat, DEN%material3, dQi_dT)
    end function calc_vhc_gauss_point_3phase_apparent_ptr

end submodule calc_vhc_3phase_apparent

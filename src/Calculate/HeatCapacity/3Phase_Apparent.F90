submodule(Calculate_VolumetricHeatCapacity) Calc_VHC_3A
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct each type of density
    !----------------------------------------------------------------------------------------------------
    module function VHC_3A_Construct(iRegion, Input) result(Structure)
        implicit none
        class(Abst_VHC), allocatable :: Structure
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        if (allocated(Structure)) deallocate (Structure)
        allocate (Type_VHC_3Phase_Apparent :: Structure)

        Structure%Material1 = Input%Regions(iRegion)%Thermal%Cp(1)
        Structure%Material2 = Input%Regions(iRegion)%Thermal%Cp(2)
        Structure%Material3 = Input%Regions(iRegion)%Thermal%Cp(3)

    end function VHC_3A_Construct

    module function Calc_VHC_GaussPoint_3Phase_Apparent(self, state, DEN, LatentHeat, dQi_dT) result(VHC)
        implicit none
        class(Type_VHC_3Phase_Apparent), intent(in) :: self
        type(type_gauss_point_state), intent(in) :: state
        type(DENHolder), intent(in), optional :: DEN
        real(real64), intent(in), optional :: LatentHeat
        real(real64), intent(in), optional :: dQi_dT
        real(real64) :: VHC

        real(real64) :: phi1, phi2, phi3

        phi1 = 1.0d0 - state%porosity
        phi2 = state%water_content
        phi3 = 1.0d0 - phi1 - phi2
        ! print '(4es16.4)', phi1, phi2, phi3, dQi_dT
        ! stop

        VHC = Calc_VHC_3A(self%Material1, phi1, self%Material2, phi2, self%Material3, phi3, &
                          LatentHeat, DEN%d%Material3, dQi_dT)
    end function Calc_VHC_GaussPoint_3Phase_Apparent

end submodule Calc_VHC_3A

submodule(Calculate_VolumetricHeatCapacity) Calc_VHC_3
    implicit none
contains
    !----------------------------------------------------------------------------------------------------
    ! Construct each type of density
    !----------------------------------------------------------------------------------------------------
    module function VHC_3_Construct(iRegion, Input) result(Structure)
        implicit none
        class(Abst_VHC), allocatable :: Structure
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        if (allocated(Structure)) deallocate (Structure)
        allocate (Type_VHC_3Phase :: Structure)

        Structure%Material1 = Input%Regions(iRegion)%Thermal%Cp(1)
        Structure%Material2 = Input%Regions(iRegion)%Thermal%Cp(2)
        Structure%Material3 = Input%Regions(iRegion)%Thermal%Cp(3)

    end function VHC_3_Construct

    module function Calc_VHC_GaussPoint_3Phase(self, state, DEN, LatentHeat, dQi_dT) result(VHC)
        implicit none
        class(Type_VHC_3Phase), intent(in) :: self
        type(GaussPointState_t), intent(in) :: state
        type(DENHolder), intent(in), optional :: DEN
        real(real64), intent(in), optional :: LatentHeat
        real(real64), intent(in), optional :: dQi_dT
        real(real64) :: VHC

        real(real64) :: phi1, phi2, phi3

        phi1 = 1.0d0 - state%porosity
        phi2 = state%water_content
        phi3 = 1.0d0 - phi1 - phi2

        VHC = Calc_VHC_3(self%Material1, phi1, self%Material2, phi2, self%Material3, phi3)
    end function Calc_VHC_GaussPoint_3Phase

end submodule Calc_VHC_3

submodule(Calculate_VolumetricHeatCapacity) Calc_VHC_Base
    implicit none

contains

    module subroutine VHCHolder_initialize(self, iRegion, Input)
        implicit none
        class(VHCHolder), intent(inout) :: self
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        if (Input%Regions(iRegion)%Flag%is3Phase) then
            if (Input%Regions(iRegion)%Flag%isFrozen) then
                self%c = Type_VHC_3Phase_Apparent(iRegion, input)
            else
                self%c = Type_VHC_3Phase(iRegion, Input)
            end if
        end if

    end subroutine VHCHolder_initialize

    module function Calc_VHC_3(VHC_soil, phi_soil, &
                               VHC_water, phi_water, &
                               VHC_ice, phi_ice) result(VHC)
        implicit none
        real(real64), intent(in) :: VHC_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: VHC_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: VHC_ice
        real(real64), intent(in) :: phi_ice
        real(real64) :: VHC

        VHC = VHC_soil * phi_soil &
              + VHC_water * phi_water &
              + VHC_ice * phi_ice

    end function Calc_VHC_3

    module function Calc_VHC_3A(VHC_soil, phi_soil, VHC_water, phi_water, &
                                VHC_ice, phi_ice, Lf, DEN_ice, dQi_dT) result(VHC)
        implicit none
        real(real64), intent(in) :: VHC_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: VHC_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: VHC_ice
        real(real64), intent(in) :: phi_ice
        real(real64), intent(in) :: Lf
        real(real64), intent(in) :: DEN_ice
        real(real64), intent(in) :: dQi_dT
        real(real64) :: VHC

        VHC = VHC_soil * phi_soil + VHC_water * phi_water + VHC_ice * phi_ice &
              - Lf * DEN_ice * dQi_dT

    end function Calc_VHC_3A

end submodule Calc_VHC_Base

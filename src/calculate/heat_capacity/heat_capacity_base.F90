submodule(calculate_volumetric_heat_capacity) Calc_VHC_Base
    implicit none

contains

    module subroutine initialize_holder_vhcs(self, iRegion, Input)
        implicit none
        class(holder_vhcs), intent(inout) :: self
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        if (Input%Regions(iRegion)%Flag%is3Phase) then
            if (Input%Regions(iRegion)%Flag%isFrozen) then
                self%p = Type_VHC_3Phase_Apparent(iRegion, input)
            else
                self%p = Type_VHC_3Phase(iRegion, Input)
            end if
        end if

    end subroutine initialize_holder_vhcs

    module function calc_vhc_3(VHC_soil, phi_soil, &
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

        VHC = VHC_soil * phi_soil + VHC_water * phi_water + VHC_ice * phi_ice

    end function calc_vhc_3

    module function calc_vhc_3a(VHC_soil, phi_soil, VHC_water, phi_water, &
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

        VHC = VHC_soil * phi_soil + VHC_water * phi_water + VHC_ice * phi_ice - Lf * DEN_ice * dQi_dT

    end function calc_vhc_3a

end submodule Calc_VHC_Base

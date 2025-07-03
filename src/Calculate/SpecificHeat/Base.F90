submodule(Calculate_SpecificHeat) Calc_SPH_Base
    implicit none

contains

    module subroutine SPHHolder_initialize(self, iRegion, Input)
        implicit none
        class(SPHHolder), intent(inout) :: self
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        if (Input%Regions(iRegion)%Flag%is3Phase) then
            self%c = Type_SPH_3Phase(iRegion, Input)
        end if

    end subroutine SPHHolder_initialize

    module function Calc_SPH_3(SpecificHeat_soil, phi_soil, &
                               SpecificHeat_water, phi_water, &
                               SpecificHeat_ice, phi_ice) result(SpecificHeat)
        implicit none
        real(real64), intent(in) :: SpecificHeat_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: SpecificHeat_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: SpecificHeat_ice
        real(real64), intent(in) :: phi_ice
        real(real64) :: SpecificHeat

        SpecificHeat = SpecificHeat_soil * phi_soil &
                       + SpecificHeat_water * phi_water &
                       + SpecificHeat_ice * phi_ice

    end function Calc_SPH_3
end submodule Calc_SPH_Base

submodule(Calculate_Density) Calc_DEN_Base
    implicit none

contains

    module subroutine DENHolder_allocate(self, iRegion, Input)
        implicit none
        class(DENHolder), intent(inout) :: self
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        if (Input%Regions(iRegion)%Flag%is3Phase) then
            self%d = Type_Density_3Phase(iRegion, Input)
        end if

    end subroutine DENHolder_allocate

    module function Calc_DEN_3(density_soil, phi_soil, &
                               density_water, phi_water, &
                               density_ice, phi_ice) result(density)
        implicit none
        real(real64), intent(in) :: density_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: density_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: density_ice
        real(real64), intent(in) :: phi_ice
        real(real64) :: density

        density = density_soil * phi_soil &
                  + density_water * phi_water &
                  + density_ice * phi_ice

    end function Calc_DEN_3
end submodule Calc_DEN_Base

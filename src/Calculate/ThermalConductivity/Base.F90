submodule(Calculate_ThermalConductivity) Calc_THC_Base
    implicit none
contains
    module subroutine THCHolder_initialize(self, iRegion, Input)
        implicit none
        class(THCHolder), intent(inout) :: self
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        if (Input%Regions(iRegion)%Flag%is3Phase) then
            self%l = Type_THC_3Phase(iRegion, Input)
        end if

    end subroutine THCHolder_initialize

    module function Calc_THC_3(lambda_soil, phi_soil, &
                               lambda_water, phi_water, &
                               lambda_ice, phi_ice) result(lambda)
        implicit none
        real(real64), intent(in) :: lambda_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: lambda_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: lambda_ice
        real(real64), intent(in) :: phi_ice
        real(real64) :: lambda

        lambda = lambda_soil**phi_soil &
                 * lambda_water**phi_water &
                 * lambda_ice**phi_ice

    end function Calc_THC_3

end submodule Calc_THC_Base

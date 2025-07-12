submodule(calculate_thermal_conductivity) calc_thc_base
    implicit none
contains
    module subroutine initialize_holder_thcs(self, iRegion, Input)
        implicit none
        class(holder_thcs), intent(inout) :: self
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        if (Input%Regions(iRegion)%Flag%is3Phase) then
            self%p = Type_THC_3Phase(iRegion, Input)
        end if

    end subroutine initialize_holder_thcs

    module function calc_thc_3(lambda_soil, phi_soil, &
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

    end function calc_thc_3

end submodule calc_thc_base

submodule(calculate_density) calculate_density_den
    implicit none

contains

    module subroutine initialize_holder_den(self, iRegion, Input)
        implicit none
        class(holder_den), intent(inout) :: self
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        if (Input%Regions(iRegion)%Flag%is3Phase) then
            self%p = type_den_3phase(iRegion, Input)
        end if

    end subroutine initialize_holder_den

    module function calc_den_3(density_soil, phi_soil, &
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

    end function calc_den_3
end submodule calculate_density_den

submodule(calculate_specific_heat) calc_sph_base
    implicit none

contains

    module subroutine initialize_holder_sphs(self, iRegion, Input)
        implicit none
        class(holder_sphs), intent(inout) :: self
        integer(int32), intent(in) :: iRegion
        type(Type_Input), intent(in) :: Input

        if (Input%Regions(iRegion)%Flag%is3Phase) then
            self%p = type_sph_3phase(iRegion, Input)
        end if

    end subroutine initialize_holder_sphs

    module function calc_sph_3(SpecificHeat_soil, phi_soil, &
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

    end function calc_sph_3
end submodule calc_sph_base

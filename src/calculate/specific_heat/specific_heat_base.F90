submodule(calculate_specific_heat) calc_sph_base
    implicit none

contains

    module subroutine initialize_holder_sphs(self, input, i_material)
        implicit none
        class(holder_sphs), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: i_material

        select case (input%basic%materials(i_material)%phase)
        case (3)
            self%p = type_sph_3phase(input, i_material)
        end select

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

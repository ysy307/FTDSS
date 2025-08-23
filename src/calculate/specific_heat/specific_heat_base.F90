submodule(calculate_specific_heat) calc_sph_base
    implicit none

contains

    module subroutine initialize_holder_sphs(self, input, material_id)
        implicit none
        class(holder_sphs), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id

        select case (input%basic%materials(material_id)%phase)
        case (3)
            self%p = type_sph_3phase(input, material_id)
        end select

    end subroutine initialize_holder_sphs

    module pure elemental function calc_sph_3(specific_heat_soil, phi_soil, &
                                              specific_heat_water, phi_water, &
                                              specific_heat_ice, phi_ice) result(specific_heat)
        implicit none
        real(real64), intent(in) :: specific_heat_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: specific_heat_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: specific_heat_ice
        real(real64), intent(in) :: phi_ice
        real(real64) :: specific_heat

        specific_heat = specific_heat_soil * phi_soil &
                        + specific_heat_water * phi_water &
                        + specific_heat_ice * phi_ice

    end function calc_sph_3
end submodule calc_sph_base

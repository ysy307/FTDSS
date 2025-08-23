submodule(calculate_volumetric_heat_capacity) Calc_VHC_Base
    implicit none

contains

    module subroutine initialize_holder_vhcs(self, input, i_material)
        implicit none
        class(holder_vhcs), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: i_material

        select case (input%basic%materials(i_material)%phase)
        case (3)
            if (input%basic%materials(i_material)%is_frozen) then
                self%p = type_vhc_3phase_apparent(input, i_material)
            else
                self%p = type_vhc_3phase(input, i_material)
            end if
        end select

    end subroutine initialize_holder_vhcs

    module pure elemental function calc_vhc_3(VHC_soil, phi_soil, &
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

    module pure elemental function calc_vhc_3a(VHC_soil, phi_soil, VHC_water, phi_water, &
                                               VHC_ice, phi_ice, Lf, density_water, dQw_dT) result(VHC)
        implicit none
        real(real64), intent(in) :: VHC_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: VHC_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: VHC_ice
        real(real64), intent(in) :: phi_ice
        real(real64), intent(in) :: Lf
        real(real64), intent(in) :: density_water
        real(real64), intent(in) :: dQw_dT
        real(real64) :: VHC

        VHC = VHC_soil * phi_soil + VHC_water * phi_water + VHC_ice * phi_ice + Lf * density_water * dQw_dT

    end function calc_vhc_3a

end submodule Calc_VHC_Base

submodule(calculate_density) calc_den_base
    implicit none

contains

    module subroutine initialize_holder_dens(self, input, i_material)
        implicit none
        class(holder_dens), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: i_material

        select case (input%basic%materials(i_material)%phase)
        case (3)
            self%p = type_den_3phase(input, i_material)
        end select

    end subroutine initialize_holder_dens

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
end submodule calc_den_base

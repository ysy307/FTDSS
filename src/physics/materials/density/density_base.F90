submodule(physics_material_density) density_base
    implicit none

contains

    module subroutine initialize_holder_dens(self, input, material_id)
        implicit none
        class(holder_dens), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: material_id

        select case (input%basic%materials(material_id)%phase)
        case (3)
            self%p = type_den_3phase(input, material_id)
        end select

    end subroutine initialize_holder_dens

    module pure elemental function calc_den_3(density_soil, phi_soil, &
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
end submodule density_base

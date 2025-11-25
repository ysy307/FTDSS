submodule(physics_material_density) density_base
    implicit none

contains

    module subroutine initialize_holder_dens(self, material_id, phase_info)
        implicit none
        class(holder_dens), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_physics_phase), intent(in) :: phase_info

        select case (phase_info%num_phases)
        case (3)
            allocate (type_den_3phase :: self%p)
            call self%p%initialize(material_id, phase_info)
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

    !>
    !> Calculate the density of saturated water vapor
    module pure elemental function calc_den_saturated_vapor(temperature) result(density_vapor)
        implicit none
        !> Temperature [°C]
        real(real64), intent(in) :: temperature
        !> Density of saturated water vapor [kg/m^3]
        real(real64) :: density_vapor

        density_vapor = 1.0d-3 * exp(31.3716 - 6014.79 / (temperature + TtoK) - 7.92495d-3 * (temperature + TtoK)) &
                        / (temperature + TtoK)

    end function calc_den_saturated_vapor
end submodule density_base

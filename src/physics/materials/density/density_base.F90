submodule(physics_materials_density) density_base
    implicit none

contains

    module subroutine initialize_holder_dens(self, material_id, physics_info, water, ice)
        implicit none
        class(holder_dens), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_physics_info), intent(in) :: physics_info
        type(type_iapws97), intent(in), target :: water
        type(type_iapws06), intent(in), target :: ice

        select case (physics_info%num_phases)
        case (1)
            allocate (type_den_1phase :: self%p)
            call self%p%initialize(material_id, physics_info, water, ice)
        case (2)
            allocate (type_den_2phase :: self%p)
            call self%p%initialize(material_id, physics_info, water, ice)
        case (3)
            allocate (type_den_3phase :: self%p)
            call self%p%initialize(material_id, physics_info, water, ice)
        case (4)
            allocate (type_den_4phase :: self%p)
            call self%p%initialize(material_id, physics_info, water, ice)
        end select

    end subroutine initialize_holder_dens

    module pure elemental subroutine calc_den_2(density_soil, phi_soil, &
                                                density_water, phi_water, density)
        implicit none
        real(real64), intent(in) :: density_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: density_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(inout) :: density

        density = density_soil * phi_soil &
                  + density_water * phi_water
    end subroutine calc_den_2

    module pure elemental subroutine calc_den_3(density_soil, phi_soil, &
                                                density_water, phi_water, &
                                                density_ice, phi_ice, density)
        implicit none
        real(real64), intent(in) :: density_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: density_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: density_ice
        real(real64), intent(in) :: phi_ice
        real(real64), intent(inout) :: density

        density = density_soil * phi_soil &
                  + density_water * phi_water &
                  + density_ice * phi_ice

    end subroutine calc_den_3

    module pure elemental subroutine calc_den_4(density_soil, phi_soil, &
                                                density_water, phi_water, &
                                                density_ice, phi_ice, &
                                                density_vapor, phi_vapor, density)
        implicit none
        real(real64), intent(in) :: density_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: density_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: density_ice
        real(real64), intent(in) :: phi_ice
        real(real64), intent(in) :: density_vapor
        real(real64), intent(in) :: phi_vapor
        real(real64), intent(inout) :: density

        density = density_soil * phi_soil &
                  + density_water * phi_water &
                  + density_ice * phi_ice &
                  + density_vapor * phi_vapor

    end subroutine calc_den_4

    !>
    !> Calculate the density of saturated water vapor
    pure elemental subroutine calc_den_saturated_vapor_experimental(temperature, density_vapor)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: temperature
        !> Density of saturated water vapor [kg/m^3]
        real(real64), intent(inout) :: density_vapor

        density_vapor = 1.0d-3 * exp(31.3716 - 6014.79 / temperature - 7.92495d-3 * temperature) / temperature

    end subroutine calc_den_saturated_vapor_experimental

    module pure elemental subroutine calc_den_saturated_vapor(temperature, density_vapor, water)
        implicit none
        real(real64), intent(in) :: temperature
        real(real64), intent(inout) :: density_vapor
        type(type_iapws97), intent(in), optional :: water

        if (present(water)) then
            call water%calc_saturation_density(temperature, density_vapor)
        else
            call calc_den_saturated_vapor_experimental(temperature + TtoK, density_vapor)
        end if
    end subroutine calc_den_saturated_vapor

end submodule density_base

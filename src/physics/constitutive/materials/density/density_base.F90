submodule(constitutive_materials_density) density_base
    implicit none

contains
    module subroutine initialize_holder_dens(self, material_id, constitutive_info, water, ice)
        implicit none
        class(holder_dens), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_constitutive_info), intent(in) :: constitutive_info
        type(type_iapws97), intent(in), target :: water
        type(type_iapws06), intent(in), target :: ice

        select case (constitutive_info%num_phases)
        case (1)
            allocate (type_den_1phase :: self%p)
            call self%p%initialize(material_id, constitutive_info, water, ice)
        case (2)
            allocate (type_den_2phase :: self%p)
            call self%p%initialize(material_id, constitutive_info, water, ice)
        case (3)
            allocate (type_den_3phase :: self%p)
            call self%p%initialize(material_id, constitutive_info, water, ice)
        case (4)
            allocate (type_den_4phase :: self%p)
            call self%p%initialize(material_id, constitutive_info, water, ice)
        end select

    end subroutine initialize_holder_dens

    module subroutine calc_den_2(density_soil, phi_soil, &
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

    module subroutine calc_den_3(density_soil, phi_soil, &
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

    module subroutine calc_den_4(density_soil, phi_soil, &
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

end submodule density_base

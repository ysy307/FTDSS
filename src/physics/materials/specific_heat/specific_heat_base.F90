submodule(physics_materials_specific_heat) specific_heat_base
    implicit none

contains

    module subroutine initialize_holder_sphs(self, material_id, physics_info, water, ice)
        implicit none
        class(holder_sphs), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_physics_info), intent(in) :: physics_info
        type(type_iapws97), intent(in), target :: water
        type(type_iapws06), intent(in), target :: ice

        select case (physics_info%num_phases)
        case (2)
            allocate (type_sph_2phase :: self%p)
            call self%p%initialize(material_id, physics_info, water, ice)
        case (3)
            allocate (type_sph_3phase :: self%p)
            call self%p%initialize(material_id, physics_info, water, ice)
        case (4)
            allocate (type_sph_4phase :: self%p)
            call self%p%initialize(material_id, physics_info, water, ice)
        end select

    end subroutine initialize_holder_sphs

    module pure elemental subroutine calc_sph_2(specific_heat_soil, phi_soil, &
                                                specific_heat_water, phi_water, specific_heat)
        implicit none
        real(real64), intent(in) :: specific_heat_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: specific_heat_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(inout) :: specific_heat

        specific_heat = specific_heat_soil * phi_soil &
                        + specific_heat_water * phi_water
    end subroutine calc_sph_2

    module pure elemental subroutine calc_sph_3(specific_heat_soil, phi_soil, &
                                                specific_heat_water, phi_water, &
                                                specific_heat_ice, phi_ice, specific_heat)
        implicit none
        real(real64), intent(in) :: specific_heat_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: specific_heat_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: specific_heat_ice
        real(real64), intent(in) :: phi_ice
        real(real64), intent(inout) :: specific_heat

        specific_heat = specific_heat_soil * phi_soil &
                        + specific_heat_water * phi_water &
                        + specific_heat_ice * phi_ice

    end subroutine calc_sph_3

    module pure elemental subroutine calc_sph_4(specific_heat_soil, phi_soil, &
                                                specific_heat_water, phi_water, &
                                                specific_heat_ice, phi_ice, &
                                                specific_heat_vapor, phi_vapor, specific_heat)
        implicit none
        real(real64), intent(in) :: specific_heat_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: specific_heat_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: specific_heat_ice
        real(real64), intent(in) :: phi_ice
        real(real64), intent(in) :: specific_heat_vapor
        real(real64), intent(in) :: phi_vapor
        real(real64), intent(inout) :: specific_heat

        specific_heat = specific_heat_soil * phi_soil &
                        + specific_heat_water * phi_water &
                        + specific_heat_ice * phi_ice &
                        + specific_heat_vapor * phi_vapor
    end subroutine calc_sph_4
end submodule specific_heat_base

submodule(physics_materials_heat_capacity) heat_capacity_base
    implicit none

contains

    module subroutine initialize_holder_vhcs(self, material_id, physics_info, water, ice)
        implicit none
        class(holder_vhcs), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_physics_info), intent(in) :: physics_info
        type(type_iapws97), intent(in), target :: water
        type(type_iapws06), intent(in), target :: ice

        select case (physics_info%num_phases)
        case (2)
            allocate (type_vhc_2phase :: self%p)
            call self%p%initialize(material_id, physics_info, water, ice)
        case (3)
            allocate (type_vhc_3phase :: self%p)
            call self%p%initialize(material_id, physics_info, water, ice)
        case (4)
            allocate (type_vhc_4phase :: self%p)
            call self%p%initialize(material_id, physics_info, water, ice)
        end select

    end subroutine initialize_holder_vhcs

    module pure elemental subroutine calc_vhc_2(vhc_soil, phi_soil, &
                                                vhc_water, phi_water, vhc)
        implicit none
        real(real64), intent(in) :: vhc_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: vhc_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(inout) :: vhc

        vhc = vhc_soil * phi_soil + vhc_water * phi_water

    end subroutine calc_vhc_2

    module pure elemental subroutine calc_vhc_3a(vhc_soil, phi_soil, vhc_water, phi_water, &
                                                 vhc_ice, phi_ice, density_water, latent_heat_fusion, dQw_dT, vhc)
        implicit none
        real(real64), intent(in) :: vhc_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: vhc_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: vhc_ice
        real(real64), intent(in) :: phi_ice
        real(real64), intent(in) :: density_water
        real(real64), intent(in) :: latent_heat_fusion
        real(real64), intent(in) :: dQw_dT
        real(real64), intent(inout) :: vhc

        vhc = vhc_soil * phi_soil + vhc_water * phi_water + vhc_ice * phi_ice + latent_heat_fusion * density_water * dQw_dT
    end subroutine calc_vhc_3a

    module pure elemental subroutine calc_vhc_4a(vhc_soil, phi_soil, vhc_water, phi_water, &
                                                 vhc_ice, phi_ice, phi_vapor, vhc_vapor, density_water, latent_heat_fusion, &
                                                 dQw_dT, latent_heat_vaporization, dQv_dT, vhc)
        implicit none
        real(real64), intent(in) :: vhc_soil
        real(real64), intent(in) :: phi_soil
        real(real64), intent(in) :: vhc_water
        real(real64), intent(in) :: phi_water
        real(real64), intent(in) :: vhc_ice
        real(real64), intent(in) :: phi_ice
        real(real64), intent(in) :: phi_vapor
        real(real64), intent(in) :: vhc_vapor
        real(real64), intent(in) :: density_water
        real(real64), intent(in) :: latent_heat_fusion
        real(real64), intent(in) :: dQw_dT
        real(real64), intent(in) :: latent_heat_vaporization
        real(real64), intent(in) :: dQv_dT
        real(real64), intent(inout) :: vhc

        vhc = vhc_soil * phi_soil + vhc_water * phi_water + vhc_ice * phi_ice + vhc_vapor * phi_vapor &
              + latent_heat_fusion * density_water * dQw_dT + latent_heat_vaporization * density_water * dQv_dT
    end subroutine calc_vhc_4a

end submodule heat_capacity_base

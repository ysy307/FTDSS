submodule(physics_materials_heat_capacity) heat_capacity_multiphase
    implicit none
contains
    module pure elemental subroutine calc_vhc_gp_1phase(self, state, vhc)
        implicit none
        class(type_vhc_1phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: vhc

        vhc = self%material1
    end subroutine calc_vhc_gp_1phase

    module pure elemental subroutine calc_vhc_gp_2phase(self, state, vhc)
        implicit none
        class(type_vhc_2phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: vhc

        real(real64) :: phi1, phi2
        real(real64) :: sph_water
        real(real64) :: density_water
        real(real64) :: vhc_water

        call self%get_phi(state, phi1, phi2)

        call self%calc_water_density(state, density_water)
        call self%calc_water_cp(state, sph_water)
        vhc_water = density_water * sph_water

        call calc_vhc_2(self%material1, phi1, vhc_water, phi2, vhc)
    end subroutine calc_vhc_gp_2phase

    module pure elemental subroutine calc_vhc_gp_3phase(self, state, vhc)
        implicit none
        class(type_vhc_3phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: vhc

        real(real64) :: phi1, phi2, phi3
        real(real64) :: sph_water, sph_ice
        real(real64) :: density_water, density_ice
        real(real64) :: vhc_water, vhc_ice
        real(real64) :: latent_heat_fusion, dQw_dT

        call self%get_phi(state, phi1, phi2, phi3)

        call self%calc_water_density(state, density_water)
        call self%calc_ice_density(state, density_ice)

        call self%calc_water_cp(state, sph_water)
        call self%calc_ice_cp(state, sph_ice)

        vhc_water = density_water * sph_water
        vhc_ice = density_ice * sph_ice

        call state%latent_heat_fusion%get(latent_heat_fusion)
        call state%dQw_dT%get(dQw_dT)

        call calc_vhc_3a(self%material1, phi1, vhc_water, phi2, vhc_ice, phi3, &
                         density_water, latent_heat_fusion, dQw_dT, vhc)
    end subroutine calc_vhc_gp_3phase

    module pure elemental subroutine calc_vhc_gp_4phase(self, state, vhc)
        implicit none
        class(type_vhc_4phase), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: vhc

        real(real64) :: phi1, phi2, phi3, phi4
        real(real64) :: sph_water, sph_ice, sph_vapor
        real(real64) :: density_water, density_ice, density_vapor
        real(real64) :: vhc_water, vhc_ice, vhc_vapor
        real(real64) :: latent_heat_fusion, dQw_dT
        real(real64) :: latent_heat_vaporization, dQv_dT

        call self%get_phi(state, phi1, phi2, phi3, phi4)

        call self%calc_water_density(state, density_water)
        call self%calc_ice_density(state, density_ice)
        call self%calc_vapor_density(state, density_vapor)

        call self%calc_water_cp(state, sph_water)
        call self%calc_ice_cp(state, sph_ice)
        call self%calc_vapor_cp(state, sph_vapor)

        vhc_water = density_water * sph_water
        vhc_ice = density_ice * sph_ice
        ! Qvは液状水換算水分量なので、密度は水のまま計算する
        vhc_vapor = density_water * sph_vapor

        call state%latent_heat_fusion%get(latent_heat_fusion)
        call state%dQw_dT%get(dQw_dT)
        call state%latent_heat_vaporization%get(latent_heat_vaporization)
        call state%dQv_dT%get(dQv_dT)

        call calc_vhc_4a(self%material1, phi1, vhc_water, phi2, vhc_ice, phi3, vhc_vapor, phi4, density_water, &
                         latent_heat_fusion, dQw_dT, latent_heat_vaporization, dQv_dT, vhc)
    end subroutine calc_vhc_gp_4phase

end submodule heat_capacity_multiphase

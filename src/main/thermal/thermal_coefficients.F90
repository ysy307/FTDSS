submodule(main_thermal) thermal_coefficients
    implicit none
contains

    module pure elemental subroutine calc_density_water_thermal(self, state, rho_water)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: rho_water

        call self%physics%calc_density_water(state, rho_water)

    end subroutine calc_density_water_thermal

    module pure elemental subroutine calc_density_ice_thermal(self, state, rho_ice)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: rho_ice

        call self%physics%calc_density_ice(state, rho_ice)

    end subroutine calc_density_ice_thermal

    module pure elemental subroutine calc_density_vapor_saturation_thermal(self, state, rho_vapor_sat)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: rho_vapor_sat

        call self%physics%calc_density_vapor_saturation(state, rho_vapor_sat)

    end subroutine calc_density_vapor_saturation_thermal

    module pure elemental subroutine update_water_phases_thermal(self, material_id, state)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state

        call self%physics%update_water_phases(material_id, state)

    end subroutine update_water_phases_thermal

    module subroutine deriv_inner_heat_capacity_thermal(self, material_id, state, controls, dU_dt)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(in) :: state
        type(type_controls), intent(in) :: controls
        real(real64), intent(inout) :: dU_dt

        type(type_state) :: local_state
        real(real64), allocatable :: temperature_history(:)
        real(real64), allocatable :: pressure_history(:)

        real(real64), pointer, contiguous, dimension(:) :: bdf_coeffs => null()
        integer(int32) :: bdf_order

        real(real64) :: porosity, Qw, Qi, Qv
        real(real64) :: rho_s, rho_w, rho_i
        real(real64) :: c_s, c_w, c_i, c_v
        real(real64) :: Lf, Lv
        real(real64) :: T, Uj
        integer(int32) :: j, n

        call state%temperature_history%get(temperature_history)
        call state%pressure_history%get(pressure_history)
        call state%porosity%get(porosity)

        call controls%time%get_bdf_order(bdf_order)
        call controls%time%get_bdf_coeffs(bdf_coeffs)
        call local_state%copy(state)

        dU_dt = 0.0d0
        do j = 1, bdf_order + 1
            call local_state%temperature%set(temperature_history(j))
            call local_state%pressure%set(pressure_history(j))
            call self%update_water_phases(material_id, local_state)

            call local_state%temperature%get(T)
            call local_state%water_content%get(Qw)
            call local_state%ice_content%get(Qi)
            call local_state%vapor_content%get(Qv)

            call self%physics%get_density_solid(material_id, rho_s)
            call self%physics%calc_density_water(local_state, rho_w)
            call self%physics%calc_density_ice(local_state, rho_i)

            call self%physics%get_specific_heat_solid(material_id, c_s)
            call self%physics%calc_specific_heat_water(local_state, c_w)
            call self%physics%calc_specific_heat_ice(local_state, c_i)
            call self%physics%calc_specific_heat_vapor(local_state, c_v)

            call self%physics%calc_latent_heat_fusion(material_id, local_state, Lf)
            call self%physics%calc_latent_heat_vaporization(material_id, local_state, Lv)

            Uj = c_s * rho_s * (1.0d0 - porosity) * T &
                 + c_w * rho_w * Qw * T &
                 + c_i * rho_i * Qi * T &
                 + c_v * rho_w * Qv * T &
                 - rho_i * Lf * Qi &
                 + rho_w * Lv * Qv

            dU_dt = dU_dt + bdf_coeffs(j) * Uj
        end do

    end subroutine deriv_inner_heat_capacity_thermal

    module subroutine compute_mass_term_thermal(self, target_material_id, state, C_TT)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: target_material_id
        type(type_state), intent(in) :: state
        real(real64), intent(inout) :: C_TT

        real(real64) :: temperature
        real(real64) :: porosity, Qw, Qi, Qv
        real(real64) :: rho_s, rho_w, rho_i
        real(real64) :: c_s, c_w, c_i, c_v
        real(real64) :: drho_w_dT, drho_ice_dT
        real(real64) :: drho_w_dP, drho_ice_dP
        real(real64) :: dP_ice_dP_water
        real(real64) :: dQw_dT, dQi_dT, dQv_dT
        real(real64) :: dQw_dP, dQi_dP, dQv_dP
        real(real64) :: Lf, Lv

        ! Get state variables
        call state%temperature%get(temperature)
        call state%porosity%get(porosity)
        call state%water_content%get(Qw)
        call state%ice_content%get(Qi)
        call state%vapor_content%get(Qv)

        ! Derivatives
        call state%dQw_dT%get(dQw_dT)
        call state%dQi_dT%get(dQi_dT)
        call state%dQv_dT%get(dQv_dT)
        call state%dQw_dP%get(dQw_dP)
        call state%dQi_dP%get(dQi_dP)
        call state%dQv_dP%get(dQv_dP)

        ! Properties
        call self%physics%get_density_solid(target_material_id, rho_s)
        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_density_ice(state, rho_i)
        call self%physics%calc_density_water_derivatives(target_material_id, state, drho_w_dT, drho_w_dP)
        call self%physics%calc_density_ice_derivatives(target_material_id, state, drho_ice_dT, drho_ice_dP)
        call self%physics%get_specific_heat_solid(target_material_id, c_s)
        call self%physics%calc_specific_heat_water(state, c_w)
        call self%physics%calc_specific_heat_ice(state, c_i)
        call self%physics%calc_specific_heat_vapor(state, c_v)
        call self%physics%calc_latent_heat_fusion(target_material_id, state, Lf)
        call self%physics%calc_latent_heat_vaporization(target_material_id, state, Lv)
        call self%physics%calc_pressure_ice_water_derivative(target_material_id, state, dP_ice_dP_water)

        C_TT = 0.0d0
        ! Heat Capacity Calculation
        C_TT = c_s * rho_s * (1.0d0 - porosity) &
               + c_w * rho_w * Qw &
               + c_i * rho_i * Qi &
               + c_v * rho_w * Qv &
               - Lf * rho_i * dQi_dT &
               + Lv * rho_w * dQv_dT
        ! C_TT = c_s * rho_s * (1.0d0 - Qn) &
        !        + c_w * rho_w * Qw + c_w * Qw * temperature * drho_w_dT + c_w * rho_w * temperature * dQw_dT &
        !        + c_i * rho_i * Qi + c_i * Qi * temperature * drho_ice_dT + c_i * rho_i * temperature * dQi_dT &
        !        + c_v * rho_w * Qv + c_v * Qv * temperature * drho_w_dT + c_v * rho_w * temperature * dQv_dT &
        !        - Lf * Qi * drho_ice_dT - Lf * rho_i * dQi_dT &
        !        + Lv * Qv * drho_w_dT + Lv * rho_w * dQv_dT

    end subroutine compute_mass_term_thermal

    module subroutine compute_diffusion_term_thermal(self, target_material_id, state, D_TT)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: target_material_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: D_TT(:, :)

        type(type_thc_dispersivity) :: lambda

        call self%physics%calc_thermal_conductivity(target_material_id, state, lambda)

        D_TT(:, :) = 0.0d0
        select case (self%computation_type)
        case (COMP_TYPE_2D_XY)
            D_TT(1, 1) = lambda%lambda_xx
            D_TT(1, 2) = lambda%lambda_xy
            D_TT(2, 1) = lambda%lambda_xy
            D_TT(2, 2) = lambda%lambda_yy
        case (COMP_TYPE_2D_XZ)
            D_TT(1, 1) = lambda%lambda_xx
            D_TT(1, 2) = lambda%lambda_zx
            D_TT(2, 1) = lambda%lambda_zx
            D_TT(2, 2) = lambda%lambda_zz
        case (COMP_TYPE_3D)
            D_TT(1, 1) = lambda%lambda_xx
            D_TT(1, 2) = lambda%lambda_xy
            D_TT(1, 3) = lambda%lambda_zx
            D_TT(2, 1) = lambda%lambda_xy
            D_TT(2, 2) = lambda%lambda_yy
            D_TT(2, 3) = lambda%lambda_yz
            D_TT(3, 1) = lambda%lambda_zx
            D_TT(3, 2) = lambda%lambda_yz
            D_TT(3, 3) = lambda%lambda_zz
        end select

    end subroutine compute_diffusion_term_thermal

    module subroutine compute_advective_term_thermal(self, target_material_id, state, V_TT)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: target_material_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: V_TT(:)

        type(type_coordinate_dp) :: water_flux, vapor_flux
        real(real64) :: rho_w, c_w, c_v

        ! Get fluxes
        call state%water_flux%get(water_flux)
        call state%vapor_flux%get(vapor_flux)

        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_specific_heat_water(state, c_w)
        call self%physics%calc_specific_heat_vapor(state, c_v)

        V_TT(:) = 0.0d0
        select case (self%computation_type)
        case (COMP_TYPE_2D_XY)
            V_TT(1) = c_w * rho_w * water_flux%x + c_v * rho_w * vapor_flux%x
            V_TT(2) = c_w * rho_w * water_flux%y + c_v * rho_w * vapor_flux%y
        case (COMP_TYPE_2D_XZ)
            V_TT(1) = c_w * rho_w * water_flux%x + c_v * rho_w * vapor_flux%x
            V_TT(2) = c_w * rho_w * water_flux%z + c_v * rho_w * vapor_flux%z
        case (COMP_TYPE_3D)
            V_TT(1) = c_w * rho_w * water_flux%x + c_v * rho_w * vapor_flux%x
            V_TT(2) = c_w * rho_w * water_flux%y + c_v * rho_w * vapor_flux%y
            V_TT(3) = c_w * rho_w * water_flux%z + c_v * rho_w * vapor_flux%z
        end select

    end subroutine compute_advective_term_thermal

    module subroutine compute_latent_term_thermal(self, target_material_id, state, L_TT)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: target_material_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: L_TT

        real(real64) :: rho_w, L_v, K_vT

        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_latent_heat_vaporization(target_material_id, state, L_v)
        call self%physics%calc_KvT(target_material_id, state, K_vT)

        L_TT = 0.0d0
        L_TT = rho_w * L_v * K_vT

    end subroutine compute_latent_term_thermal

end submodule thermal_coefficients

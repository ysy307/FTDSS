submodule(main_thermal) thermal_base
    implicit none
contains
    module subroutine initialize_type_thermal(self, input, active_region_ids)
        implicit none
        class(type_thermal), intent(inout) :: self
        type(type_input), intent(in) :: input
        integer(int32), intent(in) :: active_region_ids(:)

        integer(int32) :: num_materials
        integer(int32) :: num_phases
        integer(int32) :: i, j

        type(type_physics_info), allocatable :: density_info(:)
        type(type_physics_info), allocatable :: specific_heat_info(:)
        type(type_physics_info), allocatable :: heat_capacity_info(:)
        type(type_physics_info), allocatable :: thermal_conductivity_info(:)
        integer(int32), allocatable :: wrf_ids(:)
        type(type_wrf_params), allocatable :: wrf_model_info(:)
        integer(int32), allocatable :: hcf_ids(:)
        type(type_hcf_params), allocatable :: hcf_model_info(:)
        integer(int32), allocatable :: gcc_model_ids(:)

        ! Initialize thermal physics components

        num_materials = input%basic%num_materials

        allocate (density_info(num_materials))
        allocate (specific_heat_info(num_materials))
        allocate (heat_capacity_info(num_materials))
        allocate (thermal_conductivity_info(num_materials))
        allocate (wrf_ids(num_materials))
        allocate (wrf_model_info(num_materials))
        allocate (hcf_ids(num_materials))
        allocate (hcf_model_info(num_materials))
        allocate (gcc_model_ids(num_materials))

        do i = 1, num_materials
            do j = 1, size(active_region_ids)
                if (input%basic%materials(i)%id == active_region_ids(j)) then
                    associate (material => input%basic%materials(i))
                        num_phases = material%phase

                        call density_info(i)%reset()
                        call specific_heat_info(i)%reset()
                        call heat_capacity_info(i)%reset()
                        call thermal_conductivity_info(i)%reset()

                        density_info(i)%num_phases = num_phases
                        specific_heat_info(i)%num_phases = num_phases
                        heat_capacity_info(i)%num_phases = num_phases
                        thermal_conductivity_info(i)%num_phases = num_phases

                        if (num_phases >= 1) then
                            density_info(i)%solid = material%thermal%density(1)
                            specific_heat_info(i)%solid = material%thermal%specific_heat(1)
                            heat_capacity_info(i)%solid = material%thermal%density(1) * material%thermal%specific_heat(1)
                            thermal_conductivity_info(i)%solid = material%thermal%thermal_conductivity(1)
                        end if

                        if (num_phases >= 2) then
                            density_info(i)%water = material%thermal%density(2)
                            specific_heat_info(i)%water = material%thermal%specific_heat(2)
                            heat_capacity_info(i)%water = material%thermal%density(2) * material%thermal%specific_heat(2)
                            thermal_conductivity_info(i)%water = material%thermal%thermal_conductivity(2)
                        end if

                        if (num_phases >= 3) then
                            density_info(i)%ice = material%thermal%density(3)
                            specific_heat_info(i)%ice = material%thermal%specific_heat(3)
                            heat_capacity_info(i)%ice = material%thermal%density(3) * material%thermal%specific_heat(3)
                            thermal_conductivity_info(i)%ice = material%thermal%thermal_conductivity(3)
                            if (allocated(material%thermal%thermal_conductivity_dispersity)) then
                                call allocate_array(thermal_conductivity_info(i)%dispersity, &
                                                    source=material%thermal%thermal_conductivity_dispersity)
                            end if
                        end if

                        if (num_phases >= 4) then
                            density_info(i)%vapor = material%thermal%density(4)
                            specific_heat_info(i)%vapor = material%thermal%specific_heat(4)
                            heat_capacity_info(i)%vapor = material%thermal%density(4) * material%thermal%specific_heat(4)
                            !! TBI: 体積熱容量の経験的パラメータ入力
                            thermal_conductivity_info(i)%vapor = material%thermal%thermal_conductivity(4)
                        end if

                        if (material%thermal%phase_change%gcc%is_segregation) then
                            gcc_model_ids(i) = GCC_SEGREGATION
                        else
                            gcc_model_ids(i) = GCC_NON_SEGREGATION
                        end if

                        wrf_ids(i) = i
                        wrf_model_info(i)%model_number = material%thermal%phase_change%wrf%model_number
                        !! TBI : 単位系対応
                        wrf_model_info(i)%unit_id = PHYSICS_UNIT_M
                        wrf_model_info(i)%alpha1 = material%thermal%phase_change%wrf%alpha1
                        wrf_model_info(i)%alpha2 = material%thermal%phase_change%wrf%alpha2
                        wrf_model_info(i)%h_crit = material%thermal%phase_change%wrf%h_crit
                        wrf_model_info(i)%theta_r = material%thermal%phase_change%wrf%theta_r
                        wrf_model_info(i)%theta_s = material%thermal%phase_change%wrf%theta_s
                        wrf_model_info(i)%n1 = material%thermal%phase_change%wrf%n1
                        wrf_model_info(i)%m1 = material%thermal%phase_change%wrf%m1
                        wrf_model_info(i)%n2 = material%thermal%phase_change%wrf%n2
                        wrf_model_info(i)%m2 = material%thermal%phase_change%wrf%m2
                        wrf_model_info(i)%w1 = material%thermal%phase_change%wrf%w1
                        wrf_model_info(i)%w2 = material%thermal%phase_change%wrf%w2

                        if (material%is_active(PHYSICS_TYPE_HYDRAULIC)) then
                            hcf_ids(i) = i
                            hcf_model_info(i)%model_number = material%hydraulic%model_number
                            hcf_model_info(i)%hcf_model_number = material%hydraulic%hcf%model_number
                            !! TBI : 単位系対応
                            hcf_model_info(i)%unit_id = PHYSICS_UNIT_M
                            hcf_model_info(i)%k_s = material%hydraulic%hydraulic_conductivity
                            hcf_model_info(i)%alpha1 = material%hydraulic%hcf%alpha1
                            hcf_model_info(i)%n1 = material%hydraulic%hcf%n1
                            hcf_model_info(i)%m1 = material%hydraulic%hcf%m1
                            hcf_model_info(i)%alpha2 = material%hydraulic%hcf%alpha2
                            hcf_model_info(i)%n2 = material%hydraulic%hcf%n2
                            hcf_model_info(i)%m2 = material%hydraulic%hcf%m2
                            hcf_model_info(i)%w1 = material%hydraulic%hcf%w1
                            hcf_model_info(i)%w2 = material%hydraulic%hcf%w2
                            hcf_model_info(i)%h_crit = material%hydraulic%hcf%h_crit
                            hcf_model_info(i)%gain_factor = 10.0d0
                            hcf_model_info(i)%omega = material%hydraulic%impedance_factor
                            hcf_model_info(i)%water_viscosity_model = material%hydraulic%water_viscosity_model
                        end if

                    end associate
                end if
            end do
        end do

        call self%physics%initialize(active_region_ids, density_info, &
                                     specific_heat_info, heat_capacity_info, thermal_conductivity_info, &
                                     wrf_ids, wrf_model_info, hcf_ids, hcf_model_info, gcc_model_ids)

        self%computation_type = input%basic%simulation_settings%calculate_type
        self%computation_dimension = input%basic%simulation_settings%calculate_dimension

    end subroutine initialize_type_thermal

    module pure elemental subroutine compute_C_T(self, target_id, state, C_TT, C_TH)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout), optional :: C_TT
        real(real64), intent(inout), optional :: C_TH

        real(real64) :: temperature
        real(real64) :: Qn, Qw, Qi, Qv
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
        call state%porosity%get(Qn)
        call self%physics%update_water_phases(target_id, state)
        call state%water_content%get(Qw)
        call state%ice_content%get(Qi)
        call state%vapor_content%get(Qv)
        call state%dQw_dT%get(dQw_dT)
        call state%dQi_dT%get(dQi_dT)
        call state%dQv_dT%get(dQv_dT)
        call state%dQw_dP%get(dQw_dP)
        call state%dQi_dP%get(dQi_dP)
        call state%dQv_dP%get(dQv_dP)

        ! Calculate densities and specific heats at current state
        call self%physics%get_density_solid(target_id, rho_s)
        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_density_ice(state, rho_i)
        call self%physics%calc_density_water_derivatives(target_id, state, drho_w_dT, drho_w_dP)
        call self%physics%calc_density_ice_derivatives(target_id, state, drho_ice_dT, drho_ice_dP)
        call self%physics%get_specific_heat_solid(target_id, c_s)
        call self%physics%calc_specific_heat_water(state, c_w)
        call self%physics%calc_specific_heat_ice(state, c_i)
        call self%physics%calc_specific_heat_vapor(state, c_v)
        call self%physics%calc_latent_heat_fusion(target_id, state, Lf)
        call self%physics%calc_latent_heat_vaporization(target_id, state, Lv)
        call self%physics%calc_pressure_ice_water_derivative(target_id, state, dP_ice_dP_water)

        if (present(C_TT)) then
            ! Calculate C_TT
            C_TT = c_s * rho_s * Qn &
                   + c_w * rho_w * Qw + c_w * Qw * temperature * drho_w_dT + c_w * rho_w * temperature * dQw_dT &
                   + c_i * rho_i * Qi + c_i * Qi * temperature * drho_ice_dT + c_i * rho_i * temperature * dQi_dT &
                   + c_v * rho_w * Qv + c_v * Qv * temperature * drho_w_dT + c_v * rho_w * temperature * dQv_dT &
                   - Lf * Qi * drho_ice_dT - Lf * rho_i * dQi_dT &
                   + Lv * Qv * drho_w_dT + Lv * rho_w * dQv_dT
        end if

        if (present(C_TH)) then
            ! Calculate C_TH
            C_TH = c_w * Qw * temperature * drho_w_dP + c_w * rho_w * temperature * dQw_dP &
                   + c_i * Qi * temperature * drho_ice_dP * dP_ice_dP_water + c_i * rho_i * temperature * dQi_dP &
                   + c_v * Qv * temperature * drho_w_dP + c_v * rho_w * temperature * dQv_dP &
                   - Lf * Qi * drho_ice_dP * dP_ice_dP_water - Lf * rho_i * dQi_dP &
                   + Lv * Qv * drho_w_dP + Lv * rho_w * dQv_dP
        end if

    end subroutine compute_C_T

    !>
    !> Calculates the thermal diffusion/transport tensors D_TT and D_TH.
    !>
    !> D_TT: Effective thermal conductivity tensor (including mass transfer effects)
    !>       = lambda + rho_w * (c_w * T * K_wT + c_v * T * K_vT + Lv * K_vT) * I
    !>
    !> D_TH: Pressure-driven thermal transport tensor
    !>       = rho_w * (c_w * T * K_wP + c_v * T * K_vP + Lv * K_vP) * I
    !>
    module pure subroutine compute_D_T(self, target_id, state, D_TT, D_TH)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout), optional :: D_TT(:, :)
        real(real64), intent(inout), optional :: D_TH(:, :)

        real(real64) :: temperature
        real(real64) :: rho_w
        real(real64) :: c_w, c_v
        real(real64) :: Lv

        ! Transport coefficients (Scalar assumption for mass transfer part)
        real(real64) :: K_wT, K_flh ! Liquid: Thermal-osmosis, Hydraulic
        real(real64) :: K_vT, K_vP ! Vapor:  Thermal-diffusion, Pressure-diffusion

        ! Thermal conductivity tensor
        type(type_thc_dispersity) :: lambda
        ! real(real64), allocatable :: lambda(:, :)

        ! Intermediate coefficients
        real(real64) :: coeff_mass_TT, coeff_mass_TH
        integer(int32) :: i, j

        ! --- 1. Get State Variables ---
        call state%temperature%get(temperature)

        ! --- 2. Get Properties ---
        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_specific_heat_water(state, c_w)
        call self%physics%calc_specific_heat_vapor(state, c_v)
        call self%physics%calc_latent_heat_vaporization(target_id, state, Lv)

        ! --- 3. Get Transport Coefficients ---
        !   lambda: Thermal conductivity tensor [W/m/K]
        call self%physics%calc_thermal_conductivity(target_id, state, lambda)

        !   K_wT, K_wP: Liquid transport coeff defined as j_L = -K_wT*gradT - K_wP*gradP
        !   K_vT, K_vP: Vapor transport coeff defined as  j_V = -K_vT*gradT - K_vP*gradP
        call self%physics%calc_Kflh(target_id, state, K_flh)
        call self%physics%calc_KlT(target_id, state, K_wT)
        call self%physics%calc_Kvh(target_id, state, K_vP)
        call self%physics%calc_KvT(target_id, state, K_vT)
        ! call self%physics%calc_vapor_transport_coeffs(target_id, state, K_vT, K_vP)

        ! --- 4. Calculate D_TT (Temperature Gradient Term) ---
        if (present(D_TT)) then
            ! Initialize with base Thermal Conductivity
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

            ! Calculate scalar coefficient for mass transfer contribution
            ! Term: rho_w * ( c_w * T * K_wT + c_v * T * K_vT + Lv * K_vT )
            coeff_mass_TT = rho_w * ( &
                            c_w * temperature * K_wT & ! Sensible heat (Liquid)
                            + c_v * temperature * K_vT & ! Sensible heat (Vapor)
                            + Lv * K_vT & ! Latent heat (Vapor)
                            )

            ! Add to diagonal elements (Isotropic assumption for mass transfer enhancement)
            do i = 1, self%computation_dimension
                D_TT(i, i) = D_TT(i, i) + coeff_mass_TT
            end do
        end if

        ! --- 5. Calculate D_TH (Pressure Gradient Term) ---
        if (present(D_TH)) then
            D_TH(:, :) = 0.0d0

            ! Calculate scalar coefficient
            ! Term: rho_w * ( c_w * T * K_wP + c_v * T * K_vP + Lv * K_vP )
            coeff_mass_TH = rho_w * ( &
                            c_w * temperature * K_flh & ! Sensible heat (Liquid)
                            + c_v * temperature * K_vP & ! Sensible heat (Vapor)
                            + Lv * K_vP & ! Latent heat (Vapor)
                            )

            ! Set diagonal elements
            do i = 1, self%computation_dimension
                D_TH(i, i) = coeff_mass_TH
            end do
        end if

    end subroutine compute_D_T

    module pure subroutine compute_V_T(self, target_id, state, V_TT, V_TH)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout), optional :: V_TT(:)
        real(real64), intent(inout), optional :: V_TH(:)

        real(real64) :: rho_w, c_w, c_v
        type(type_coordinate_dp) :: water_flux, vapor_flux

        ! Get fluxes
        call state%water_flux%get(water_flux)
        call state%vapor_flux%get(vapor_flux)

        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_specific_heat_water(state, c_w)
        call self%physics%calc_specific_heat_vapor(state, c_v)

        if (present(V_TT)) then
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
        end if

        if (present(V_TH)) then
            V_TH(:) = 0.0d0
        end if

    end subroutine compute_V_T

    !>
    !> Calculates the components of the thermal residual.
    !>
    !> R_T_C (Scalar): Storage/Capacity term (Time derivative part)
    !>        = C_TT * dT/dt + C_TH * dP/dt
    !>
    !> R_T_D (Vector): Energy Flux term
    !>        = -lambda.grad(T) + (c_w*rho_w*q_w + c_v*rho_w*q_v)*T + rho_w*Lv*q_v
    !>
    module pure subroutine compute_R_T(self, target_id, state, R_T_C, R_T_D)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: target_id
        type(type_state), intent(inout) :: state
        real(real64), intent(inout) :: R_T_C ! Scalar: Capacity/Storage term
        real(real64), intent(inout) :: R_T_D(:) ! Vector: Energy Flux term (j_E)

        ! --- Local Variables ---
        ! For Capacity Term
        real(real64) :: C_TT, C_TH
        real(real64) :: dot_T, dot_P

        ! For Flux Term
        real(real64) :: temperature
        type(type_coordinate_dp) :: grad_T
        type(type_coordinate_dp) :: water_flux, vapor_flux
        type(type_thc_dispersity) :: lambda

        real(real64) :: rho_w, c_w, c_v, Lv
        real(real64) :: term_adv_sensible(3) ! 顕熱移流
        real(real64) :: term_adv_latent(3) ! 潜熱移流
        real(real64) :: term_conduction(3) ! 熱伝導

        integer(int32) :: i

        ! ======================================================================
        ! 1. Calculate Storage Term (R_T_C)
        ! ======================================================================

        ! 1.1 Get Time Derivatives (provided by BDF/Time Integrator)
        call state%dot_T%get(dot_T)
        call state%dot_P%get(dot_P)

        ! 1.2 Calculate Capacities (C_TT, C_TH)
        !     Re-use the existing compute_C_T subroutine
        call self%compute_C_T(target_id, state, C_TT, C_TH)

        ! 1.3 Compute Scalar Residual Component
        R_T_C = C_TT * dot_T + C_TH * dot_P

        ! ======================================================================
        ! 2. Calculate Flux Term (R_T_D = j_E)
        ! ======================================================================

        ! 2.1 Get State and Properties
        call state%temperature%get(temperature)
        call state%grad_T%get(grad_T)
        call state%water_flux%get(water_flux) ! Volumetric flux [m/s]
        call state%vapor_flux%get(vapor_flux) ! Volumetric flux [m/s]

        call self%physics%calc_density_water(state, rho_w)
        call self%physics%calc_specific_heat_water(state, c_w)
        call self%physics%calc_specific_heat_vapor(state, c_v)
        call self%physics%calc_latent_heat_vaporization(target_id, state, Lv)

        ! Get thermal conductivity tensor
        call self%physics%calc_thermal_conductivity(target_id, state, lambda)

        ! Initialize terms
        term_conduction = 0.0d0
        term_adv_sensible = 0.0d0
        term_adv_latent = 0.0d0

        ! 2.2 Compute Flux Components based on Dimension
        select case (self%computation_type)
        case (COMP_TYPE_2D_XY)
            ! --- Conduction: -lambda * grad T ---
            term_conduction(1) = -(lambda%lambda_xx * grad_T%x + lambda%lambda_xy * grad_T%y)
            term_conduction(2) = -(lambda%lambda_xy * grad_T%x + lambda%lambda_yy * grad_T%y)

            ! --- Sensible Heat Advection: rho_w * (c_w*q_w + c_v*q_v) * T ---
            term_adv_sensible(1) = rho_w * (c_w * water_flux%x + c_v * vapor_flux%x) * temperature
            term_adv_sensible(2) = rho_w * (c_w * water_flux%y + c_v * vapor_flux%y) * temperature

            ! --- Latent Heat Advection: rho_w * Lv * q_v ---
            term_adv_latent(1) = rho_w * Lv * vapor_flux%x
            term_adv_latent(2) = rho_w * Lv * vapor_flux%y

        case (COMP_TYPE_2D_XZ)
            ! --- Conduction ---
            term_conduction(1) = -(lambda%lambda_xx * grad_T%x + lambda%lambda_zx * grad_T%z)
            term_conduction(2) = -(lambda%lambda_zx * grad_T%x + lambda%lambda_zz * grad_T%z)

            ! --- Sensible Heat Advection ---
            term_adv_sensible(1) = rho_w * (c_w * water_flux%x + c_v * vapor_flux%x) * temperature
            term_adv_sensible(2) = rho_w * (c_w * water_flux%z + c_v * vapor_flux%z) * temperature

            ! --- Latent Heat Advection ---
            term_adv_latent(1) = rho_w * Lv * vapor_flux%x
            term_adv_latent(2) = rho_w * Lv * vapor_flux%z

        case (COMP_TYPE_3D)
            ! --- Conduction ---
            term_conduction(1) = -(lambda%lambda_xx * grad_T%x + lambda%lambda_xy * grad_T%y + lambda%lambda_zx * grad_T%z)
            term_conduction(2) = -(lambda%lambda_xy * grad_T%x + lambda%lambda_yy * grad_T%y + lambda%lambda_yz * grad_T%z)
            term_conduction(3) = -(lambda%lambda_zx * grad_T%x + lambda%lambda_yz * grad_T%y + lambda%lambda_zz * grad_T%z)

            ! --- Sensible Heat Advection ---
            term_adv_sensible(1) = rho_w * (c_w * water_flux%x + c_v * vapor_flux%x) * temperature
            term_adv_sensible(2) = rho_w * (c_w * water_flux%y + c_v * vapor_flux%y) * temperature
            term_adv_sensible(3) = rho_w * (c_w * water_flux%z + c_v * vapor_flux%z) * temperature

            ! --- Latent Heat Advection ---
            term_adv_latent(1) = rho_w * Lv * vapor_flux%x
            term_adv_latent(2) = rho_w * Lv * vapor_flux%y
            term_adv_latent(3) = rho_w * Lv * vapor_flux%z

        end select

        ! 2.3 Sum components to get Total Energy Flux Vector
        ! R_T_D corresponds to j_E
        R_T_D(:) = 0.0d0 ! Safety clear
        do i = 1, self%computation_dimension
            R_T_D(i) = term_conduction(i) + term_adv_sensible(i) + term_adv_latent(i)
        end do

        ! Store calculated flux back to state for visualization or post-processing
        ! Assuming state%energy_flux%set accepts an array or components
        ! call state%energy_flux%set(R_T_D)

    end subroutine compute_R_T
end submodule thermal_base

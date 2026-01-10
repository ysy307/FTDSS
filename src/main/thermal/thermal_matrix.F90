submodule(main_thermal) thermal_matrix
    implicit none
contains

    ! !>
    ! !> Calculates the thermal capacity (diagonal mass matrix part).
    ! !>
    ! module pure elemental subroutine compute_C_T(self, target_material_id, controls, state, C_TT, C_TH)
    !     implicit none
    !     class(type_thermal), intent(in) :: self
    !     integer(int32), intent(in) :: target_material_id
    !     type(type_controls), intent(in) :: controls
    !     type(type_state), intent(inout) :: state
    !     real(real64), intent(inout), optional :: C_TT
    !     real(real64), intent(inout), optional :: C_TH

    !     real(real64) :: temperature
    !     real(real64) :: Qn, Qw, Qi, Qv
    !     real(real64) :: rho_s, rho_w, rho_i
    !     real(real64) :: c_s, c_w, c_i, c_v
    !     real(real64) :: drho_w_dT, drho_ice_dT
    !     real(real64) :: drho_w_dP, drho_ice_dP
    !     real(real64) :: dP_ice_dP_water
    !     real(real64) :: dQw_dT, dQi_dT, dQv_dT
    !     real(real64) :: dQw_dP, dQi_dP, dQv_dP
    !     real(real64) :: Lf, Lv

    !     ! Get state variables
    !     call state%temperature%get(temperature)
    !     call state%porosity%get(Qn)
    !     call state%water_content%get(Qw)
    !     call state%ice_content%get(Qi)
    !     call state%vapor_content%get(Qv)

    !     ! Derivatives
    !     call state%dQw_dT%get(dQw_dT)
    !     call state%dQi_dT%get(dQi_dT)
    !     call state%dQv_dT%get(dQv_dT)
    !     call state%dQw_dP%get(dQw_dP)
    !     call state%dQi_dP%get(dQi_dP)
    !     call state%dQv_dP%get(dQv_dP)

    !     ! Properties
    !     call self%physics%get_density_solid(target_material_id, rho_s)
    !     call self%physics%calc_density_water(state, rho_w)
    !     call self%physics%calc_density_ice(state, rho_i)
    !     call self%physics%calc_density_water_derivatives(target_material_id, state, drho_w_dT, drho_w_dP)
    !     call self%physics%calc_density_ice_derivatives(target_material_id, state, drho_ice_dT, drho_ice_dP)
    !     call self%physics%get_specific_heat_solid(target_material_id, c_s)
    !     call self%physics%calc_specific_heat_water(state, c_w)
    !     call self%physics%calc_specific_heat_ice(state, c_i)
    !     call self%physics%calc_specific_heat_vapor(state, c_v)
    !     call self%physics%calc_latent_heat_fusion(target_material_id, state, Lf)
    !     call self%physics%calc_latent_heat_vaporization(target_material_id, state, Lv)
    !     call self%physics%calc_pressure_ice_water_derivative(target_material_id, state, dP_ice_dP_water)

    !     if (present(C_TT)) then
    !         C_TT = 0.0d0
    !         ! Heat Capacity Calculation
    !         C_TT = c_s * rho_s * (1.0d0 - Qn) &
    !                + c_w * rho_w * Qw &
    !                + c_i * rho_i * Qi &
    !                + c_v * rho_w * Qv &
    !                - Lf * rho_i * dQi_dT &
    !                + Lv * rho_w * dQv_dT
    !         ! C_TT = c_s * rho_s * (1.0d0 - Qn) &
    !         !        + c_w * rho_w * Qw + c_w * Qw * temperature * drho_w_dT + c_w * rho_w * temperature * dQw_dT &
    !         !        + c_i * rho_i * Qi + c_i * Qi * temperature * drho_ice_dT + c_i * rho_i * temperature * dQi_dT &
    !         !        + c_v * rho_w * Qv + c_v * Qv * temperature * drho_w_dT + c_v * rho_w * temperature * dQv_dT &
    !         !        - Lf * Qi * drho_ice_dT - Lf * rho_i * dQi_dT &
    !         !        + Lv * Qv * drho_w_dT + Lv * rho_w * dQv_dT
    !     end if

    !     if (present(C_TH)) then
    !         C_TH = 0.0d0
    !         if (controls%is_target(PHYSICS_TYPE_HYDRAULIC, target_material_id)) then
    !             ! Pressure coupling
    !             C_TH = c_w * Qw * temperature * drho_w_dP + c_w * rho_w * temperature * dQw_dP &
    !                    + c_i * Qi * temperature * drho_ice_dP * dP_ice_dP_water + c_i * rho_i * temperature * dQi_dP &
    !                    + c_v * Qv * temperature * drho_w_dP + c_v * rho_w * temperature * dQv_dP &
    !                    - Lf * Qi * drho_ice_dP * dP_ice_dP_water - Lf * rho_i * dQi_dP &
    !                    + Lv * Qv * drho_w_dP + Lv * rho_w * dQv_dP
    !         else
    !             C_TH = 0.0d0
    !         end if
    !     end if

    ! end subroutine compute_C_T

    ! !>
    ! !> Calculates the thermal diffusion/transport tensors D_TT and D_TH.
    ! !> [RESTORED] Full Jacobian calculation including mass transfer coupling.
    ! !>
    ! module subroutine compute_D_T(self, target_material_id, controls, state, D_TT, D_TH)
    !     implicit none
    !     class(type_thermal), intent(in) :: self
    !     integer(int32), intent(in) :: target_material_id
    !     type(type_controls), intent(in) :: controls
    !     type(type_state), intent(inout) :: state
    !     real(real64), intent(inout), optional :: D_TT(:, :)
    !     real(real64), intent(inout), optional :: D_TH(:, :)

    !     real(real64) :: temperature
    !     real(real64) :: rho_w
    !     real(real64) :: c_w, c_v
    !     real(real64) :: Lv
    !     real(real64) :: K_wT, K_flh, K_vT, K_vP
    !     type(type_thc_dispersivity) :: lambda
    !     real(real64) :: term_conduction(3, 3)
    !     real(real64) :: term_mass_transfer(3, 3)
    !     real(real64) :: coeff_mass_TT, coeff_mass_TH
    !     integer(int32) :: i, j
    !     integer(int32) :: dim

    !     ! Initialize

    !     dim = self%computation_dimension

    !     ! --- 1. Get State Variables ---
    !     call state%temperature%get(temperature)

    !     ! --- 2. Get Properties ---
    !     call self%physics%calc_density_water(state, rho_w)
    !     call self%physics%calc_specific_heat_water(state, c_w)
    !     call self%physics%calc_specific_heat_vapor(state, c_v)
    !     call self%physics%calc_latent_heat_vaporization(target_material_id, state, Lv)

    !     ! --- 3. Get Transport Coefficients ---
    !     call self%physics%calc_thermal_conductivity(target_material_id, state, lambda)
    !     call self%physics%calc_Kflh(target_material_id, state, K_flh)
    !     call self%physics%calc_KlT(target_material_id, state, K_wT)
    !     call self%physics%calc_Kvh(target_material_id, state, K_vP)
    !     call self%physics%calc_KvT(target_material_id, state, K_vT)

    !     ! --- 4. Calculate D_TT (Temperature Gradient Term) ---
    !     if (present(D_TT)) then
    !         D_TT(:, :) = 0.0d0
    !         term_conduction(:, :) = 0.0d0
    !         select case (self%computation_type)
    !         case (COMP_TYPE_2D_XY)
    !             term_conduction(1, 1) = lambda%lambda_xx
    !             term_conduction(1, 2) = lambda%lambda_xy
    !             term_conduction(2, 1) = lambda%lambda_xy
    !             term_conduction(2, 2) = lambda%lambda_yy
    !         case (COMP_TYPE_2D_XZ)
    !             term_conduction(1, 1) = lambda%lambda_xx
    !             term_conduction(1, 2) = lambda%lambda_zx
    !             term_conduction(2, 1) = lambda%lambda_zx
    !             term_conduction(2, 2) = lambda%lambda_zz
    !         case (COMP_TYPE_3D)
    !             term_conduction(1, 1) = lambda%lambda_xx
    !             term_conduction(1, 2) = lambda%lambda_xy
    !             term_conduction(1, 3) = lambda%lambda_zx
    !             term_conduction(2, 1) = lambda%lambda_xy
    !             term_conduction(2, 2) = lambda%lambda_yy
    !             term_conduction(2, 3) = lambda%lambda_yz
    !             term_conduction(3, 1) = lambda%lambda_zx
    !             term_conduction(3, 2) = lambda%lambda_yz
    !             term_conduction(3, 3) = lambda%lambda_zz
    !         end select

    !         term_mass_transfer(:, :) = 0.0d0
    !         if (controls%is_target(PHYSICS_TYPE_HYDRAULIC, target_material_id)) then
    !             coeff_mass_TT = rho_w * ( &
    !                             c_w * temperature * K_wT &
    !                             + c_v * temperature * K_vT &
    !                             + Lv * K_vT &
    !                             )
    !             do i = 1, dim
    !                 term_mass_transfer(i, i) = coeff_mass_TT
    !             end do
    !         end if

    !         do i = 1, dim
    !             do j = 1, dim
    !                 D_TT(i, j) = term_conduction(i, j) + term_mass_transfer(i, j)
    !             end do
    !         end do
    !     end if

    !     ! --- 5. Calculate D_TH (Pressure Gradient Term) ---
    !     if (present(D_TH)) then
    !         D_TH(:, :) = 0.0d0
    !         if (controls%is_target(PHYSICS_TYPE_HYDRAULIC, target_material_id)) then
    !             coeff_mass_TH = rho_w * ( &
    !                             c_w * temperature * K_flh &
    !                             + c_v * temperature * K_vP &
    !                             + Lv * K_vP)
    !             do i = 1, dim
    !                 D_TH(i, i) = coeff_mass_TH
    !             end do
    !         end if
    !     end if

    ! end subroutine compute_D_T

    ! !>
    ! !> Calculates Advection Vector.
    ! !> [RESTORED] Original advection logic.
    ! !>
    ! module subroutine compute_V_T(self, target_material_id, controls, state, V_TT, V_TH)
    !     implicit none
    !     class(type_thermal), intent(in) :: self
    !     integer(int32), intent(in) :: target_material_id
    !     type(type_controls), intent(in) :: controls
    !     type(type_state), intent(inout) :: state
    !     real(real64), intent(inout), optional :: V_TT(:)
    !     real(real64), intent(inout), optional :: V_TH(:)

    !     real(real64) :: rho_w, c_w, c_v
    !     type(type_coordinate_dp) :: water_flux, vapor_flux
    !     real(real64) :: sensible_adv_water(3), sensible_adv_vapor(3)
    !     integer(int32) :: i
    !     integer(int32) :: dim

    !     ! 0. Initialize
    !     dim = self%computation_dimension

    !     ! Get fluxes
    !     call state%water_flux%get(water_flux)
    !     call state%vapor_flux%get(vapor_flux)

    !     call self%physics%calc_density_water(state, rho_w)
    !     call self%physics%calc_specific_heat_water(state, c_w)
    !     call self%physics%calc_specific_heat_vapor(state, c_v)

    !     if (present(V_TT)) then
    !         ! Initialize
    !         V_TT(:) = 0.0d0
    !         sensible_adv_water(:) = 0.0d0
    !         sensible_adv_vapor(:) = 0.0d0

    !         if (controls%is_target(PHYSICS_TYPE_HYDRAULIC, target_material_id)) then
    !             select case (self%computation_type)
    !             case (COMP_TYPE_2D_XY)
    !                 sensible_adv_water(1) = c_w * rho_w * water_flux%x
    !                 sensible_adv_water(2) = c_w * rho_w * water_flux%y
    !                 sensible_adv_vapor(1) = c_v * rho_w * vapor_flux%x
    !                 sensible_adv_vapor(2) = c_v * rho_w * vapor_flux%y
    !             case (COMP_TYPE_2D_XZ)
    !                 sensible_adv_water(1) = c_w * rho_w * water_flux%x
    !                 sensible_adv_water(2) = c_w * rho_w * water_flux%z
    !                 sensible_adv_vapor(1) = c_v * rho_w * vapor_flux%x
    !                 sensible_adv_vapor(2) = c_v * rho_w * vapor_flux%z
    !             case (COMP_TYPE_3D)
    !                 sensible_adv_water(1) = c_w * rho_w * water_flux%x
    !                 sensible_adv_water(2) = c_w * rho_w * water_flux%y
    !                 sensible_adv_water(3) = c_w * rho_w * water_flux%z
    !                 sensible_adv_vapor(1) = c_v * rho_w * vapor_flux%x
    !                 sensible_adv_vapor(2) = c_v * rho_w * vapor_flux%y
    !                 sensible_adv_vapor(3) = c_v * rho_w * vapor_flux%z
    !             end select

    !             do i = 1, dim
    !                 V_TT(i) = sensible_adv_water(i) + sensible_adv_vapor(i)
    !             end do
    !         end if
    !     end if

    !     if (present(V_TH)) then
    !         V_TH(:) = 0.0d0
    !     end if

    ! end subroutine compute_V_T

    ! !>
    ! !> Calculates the components of the thermal residual.
    ! !> [FIXED] Conduction term has NEGATIVE sign. Advection terms restored.
    ! !>
    ! module subroutine compute_R_T(self, target_material_id, controls, state, R_T_C, R_T_D)
    !     implicit none
    !     class(type_thermal), intent(in) :: self
    !     integer(int32), intent(in) :: target_material_id
    !     type(type_controls), intent(in) :: controls
    !     type(type_state), intent(inout) :: state
    !     ! real(real64), intent(in) :: bdf_coeffs(:)
    !     real(real64), intent(inout) :: R_T_C ! Capacity
    !     real(real64), intent(inout) :: R_T_D(:) ! Flux (j_E)

    !     ! Local Variables
    !     real(real64) :: temperature
    !     type(type_coordinate_dp) :: grad_T
    !     type(type_coordinate_dp) :: water_flux, vapor_flux
    !     type(type_thc_dispersivity) :: lambda
    !     real(real64) :: rho_w, c_w, c_v, Lv
    !     real(real64) :: term_conduction(3)
    !     real(real64) :: term_adv_sensible(3)
    !     real(real64) :: term_adv_latent(3)
    !     integer(int32) :: i
    !     integer(int32) :: dim

    !     dim = self%computation_dimension

    !     ! 1. Calculate Storage Term
    !     call self%compute_transient_term(target_material_id, state, controls, R_T_C)

    !     ! 2. Calculate Flux Term (R_T_D)
    !     call state%temperature%get(temperature)
    !     call state%grad_T%get(grad_T)
    !     call state%water_flux%get(water_flux)
    !     call state%vapor_flux%get(vapor_flux)
    !     call self%physics%calc_density_water(state, rho_w)
    !     call self%physics%calc_specific_heat_water(state, c_w)
    !     call self%physics%calc_specific_heat_vapor(state, c_v)
    !     call self%physics%calc_latent_heat_vaporization(target_material_id, state, Lv)
    !     call self%physics%calc_thermal_conductivity(target_material_id, state, lambda)

    !     term_conduction = 0.0d0
    !     term_adv_sensible = 0.0d0
    !     term_adv_latent = 0.0d0

    !     ! Conduction term
    !     !  term_conduction = - λ grad T
    !     ! Advection sensible term with water and vapor transfert
    !     !  term_adv_sensible = ρ_w (c_w * q_w + c_v * q_v) T
    !     ! Advection latent term under vaporation
    !     !  term_adv_latent = ρ_w Lv q_v T
    !     select case (self%computation_type)
    !     case (COMP_TYPE_2D_XY)
    !         term_conduction(1) = -(lambda%lambda_xx * grad_T%x + lambda%lambda_xy * grad_T%y)
    !         term_conduction(2) = -(lambda%lambda_xy * grad_T%x + lambda%lambda_yy * grad_T%y)

    !         term_adv_sensible(1) = rho_w * (c_w * water_flux%x + c_v * vapor_flux%x) * temperature
    !         term_adv_sensible(2) = rho_w * (c_w * water_flux%y + c_v * vapor_flux%y) * temperature
    !         term_adv_latent(1) = rho_w * Lv * vapor_flux%x
    !         term_adv_latent(2) = rho_w * Lv * vapor_flux%y

    !     case (COMP_TYPE_2D_XZ)
    !         term_conduction(1) = -(lambda%lambda_xx * grad_T%x + lambda%lambda_zx * grad_T%z)
    !         term_conduction(2) = -(lambda%lambda_zx * grad_T%x + lambda%lambda_zz * grad_T%z)

    !         term_adv_sensible(1) = rho_w * (c_w * water_flux%x + c_v * vapor_flux%x) * temperature
    !         term_adv_sensible(2) = rho_w * (c_w * water_flux%z + c_v * vapor_flux%z) * temperature
    !         term_adv_latent(1) = rho_w * Lv * vapor_flux%x
    !         term_adv_latent(2) = rho_w * Lv * vapor_flux%z

    !     case (COMP_TYPE_3D)
    !         term_conduction(1) = -(lambda%lambda_xx * grad_T%x + lambda%lambda_xy * grad_T%y + lambda%lambda_zx * grad_T%z)
    !         term_conduction(2) = -(lambda%lambda_xy * grad_T%x + lambda%lambda_yy * grad_T%y + lambda%lambda_yz * grad_T%z)
    !         term_conduction(3) = -(lambda%lambda_zx * grad_T%x + lambda%lambda_yz * grad_T%y + lambda%lambda_zz * grad_T%z)

    !         term_adv_sensible(1) = rho_w * (c_w * water_flux%x + c_v * vapor_flux%x) * temperature
    !         term_adv_sensible(2) = rho_w * (c_w * water_flux%y + c_v * vapor_flux%y) * temperature
    !         term_adv_sensible(3) = rho_w * (c_w * water_flux%z + c_v * vapor_flux%z) * temperature
    !         term_adv_latent(1) = rho_w * Lv * vapor_flux%x
    !         term_adv_latent(2) = rho_w * Lv * vapor_flux%y
    !         term_adv_latent(3) = rho_w * Lv * vapor_flux%z
    !     end select

    !     ! Sum components
    !     R_T_D(:) = 0.0d0
    !     if (controls%is_target(PHYSICS_TYPE_HYDRAULIC, target_material_id)) then
    !         do i = 1, dim
    !             R_T_D(i) = term_conduction(i) + term_adv_sensible(i) + term_adv_latent(i)
    !         end do
    !     else
    !         do i = 1, dim
    !             R_T_D(i) = term_conduction(i)
    !         end do
    !     end if

    ! end subroutine compute_R_T

    module subroutine assemble_local_thermal(self, controls, workspace, J_TT, J_TH, R_T)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_controls), intent(in) :: controls
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: J_TT
        type(type_matrix_dense), intent(inout), optional :: J_TH
        type(type_vector_dp), intent(inout), optional :: R_T

        integer(int32) :: i, j

        ! --- 1. Reset Workspace Arrays ---
        workspace%work_C(:) = 0.0d0
        workspace%work_D(:, :, :) = 0.0d0
        workspace%work_V(:, :) = 0.0d0
        workspace%work_L(:) = 0.0d0
        workspace%work_d_dt(:) = 0.0d0

        ! --- 2. Evaluate Physical Coefficients at Gauss Points ---
        do i = 1, workspace%num_fe_gauss
            ! Mass Term (Heat Capacity) for Jacobian: C_vol
            call self%compute_mass_term(workspace%material_id, workspace%state_gp(i), workspace%work_C(i))

            ! Diffusion Term (Thermal Conductivity) for Jacobian/Residual: R_tensor
            call self%compute_diffusion_term(workspace%material_id, workspace%state_gp(i), workspace%work_D(:, :, i))

            ! Transient Term (Energy Rate) for Residual: dU/dt
            call self%compute_transient_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%bdf_coeffs(1:workspace%bdf_order + 1), workspace%work_d_dt(i))
        end do

        ! (Optional) Hydraulic Coupling Terms
        if (controls%is_target(PHYSICS_TYPE_HYDRAULIC, workspace%material_id)) then
            do i = 1, workspace%num_fe_gauss
                call self%compute_advective_term(workspace%material_id, workspace%state_gp(i), workspace%work_V(:, i))
                call self%compute_latent_term(workspace%material_id, workspace%state_gp(i), workspace%work_L(i))
            end do
        end if

        ! --- 3. Assemble Capacity Terms (Mass Matrix) ---
        ! J_TT += alpha * M
        call workspace%compute_K1(workspace%work_C, workspace%work_matrix)
        if (present(J_TT)) then
            do i = 1, workspace%num_fe_nodes
                do j = 1, workspace%num_fe_nodes
                    call J_TT%set(OP_ADD, i, j, workspace%bdf_coeffs(1) * workspace%work_matrix(i, j))
                end do
            end do
        end if

        ! --- 4. Assemble Diffusion Terms (Stiffness Matrix) ---
        ! J_TT += K,  R_T -= K * T
        call workspace%compute_K2(workspace%work_D, workspace%work_matrix)

        if (present(J_TT)) then
            do i = 1, workspace%num_fe_nodes
                do j = 1, workspace%num_fe_nodes
                    call J_TT%set(OP_ADD, i, j, workspace%work_matrix(i, j))
                end do
            end do
        end if

        if (present(R_T)) then
            ! Flux Residual: F_int = K * T
            workspace%work_vec(:) = 0.0d0
            workspace%work_vec = matmul(workspace%work_matrix, workspace%T_node)
            do i = 1, workspace%num_fe_nodes
                call R_T%set(OP_ADD, i, -workspace%work_vec(i))
            end do
        end if

        ! --- 5. Assemble Transient Residual (Storage Vector) ---
        ! R_T -= Integral( psi * dU/dt )
        call workspace%compute_R1(workspace%work_d_dt, workspace%work_vec)
        if (present(R_T)) then
            do i = 1, workspace%num_fe_nodes
                call R_T%set(OP_ADD, i, -workspace%work_vec(i))
            end do
        end if

    end subroutine assemble_local_thermal
end submodule thermal_matrix

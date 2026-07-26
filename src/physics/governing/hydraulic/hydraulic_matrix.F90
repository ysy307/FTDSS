submodule(physics_governing_hydraulic) hydraulic_matrix
    use :: domain_fe_subcell, only: type_subcell_qp, SUBCELL_QP_CAP, build_interface_quadrature_points
    use :: models_phase_change_chemical_potential, only: calc_T_high_celsius
    implicit none

contains

    !> @brief Assemble local matrix and vector (Picard only)
    module subroutine assemble_local_hydraulic(self, control, workspace, K_HH, K_HT, F_H)
        implicit none
        class(type_hydraulic), intent(in) :: self
        type(type_control), intent(in) :: control
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: K_HH
        type(type_matrix_dense), intent(inout), optional :: K_HT
        type(type_vector_dp), intent(inout), optional :: F_H

        call self%assemble_local_picard(control, workspace, K_HH, K_HT, F_H)

    end subroutine assemble_local_hydraulic

    !> @brief Assemble Picard local components (backward Euler, no BDF history)
    !>
    !> All coefficients are evaluated directly at quadrature points from the
    !> interpolated state (no nodal pre-evaluation / lerp of coefficients).
    !>
    !> The transport coefficients can change sharply across the freezing
    !> interface \(\phi = T_{high}(p_w) - T = 0\).  Elements cut by the
    !> interface are therefore integrated with the interface-split subcell rule
    !> (build_interface_quadrature_points), which REPLACES the standard Gauss
    !> rule for the pressure, temperature, and gravity fluxes in those elements.
    !> Using one quadrature partition for all three parts of the Darcy flux
    !> avoids a discrete imbalance caused solely by sampling the same flux at
    !> different points.
    !>
    !> Storage C_eq, mixed transient, and segregation sink keep the standard
    !> Gauss rule in all elements.
    !>
    !> In Modified Picard, the transport coefficients are frozen at the current
    !> iterate while both primary increments remain coupled. The K_HT block is
    !> therefore K2(D_HT), matching the temperature-flux term in the residual.
    module subroutine assemble_local_picard_hydraulic(self, control, workspace, K_HH, K_HT, F_H)
        implicit none
        class(type_hydraulic), intent(in) :: self
        type(type_control), intent(in) :: control
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: K_HH
        type(type_matrix_dense), intent(inout), optional :: K_HT
        type(type_vector_dp), intent(inout), optional :: F_H

        integer(int32) :: i, j, d, n_nodes, n_gauss, n_dim, ierr
        real(real64) :: bdf0, dt_local

        ! --- Interface-split subcell variables ---
        type(type_subcell_qp) :: sub_qps(SUBCELL_QP_CAP)
        integer(int32) :: n_sub_qps, q_s
        logical :: use_subcell, is_cut
        real(real64) :: phi_nodes(workspace%num_fe_nodes)
        real(real64) :: porosity_nodes(workspace%num_fe_nodes)
        real(real64) :: rho_w_node_sub, T_high_node
        real(real64) :: T_q_sub, P_q_sub, porosity_q_sub, Qi_q_sub
        real(real64) :: D_HH_sub, D_HT_sub, eff_weight_sub, det_J_sub
        real(real64) :: V_sub(workspace%num_fe_dimension)
        real(real64) :: vec_V_sub(workspace%num_fe_nodes)
        real(real64) :: coeff_sub_mat(workspace%num_fe_dimension, workspace%num_fe_dimension)
        real(real64) :: mat_HH_sub(workspace%num_fe_nodes, workspace%num_fe_nodes)
        real(real64) :: mat_HT_sub(workspace%num_fe_nodes, workspace%num_fe_nodes)
        real(real64) :: dpsi_dx_sub(workspace%num_fe_dimension, workspace%num_fe_nodes)
        type(type_coordinate_dp) :: r_sub
        type(type_state) :: state_sub

        ! --- Standard assembly variables ---
        real(real64) :: local_vec_res(workspace%num_fe_nodes)
        real(real64) :: work_C_HT(workspace%num_fe_gauss)
        real(real64) :: work_sink(workspace%num_fe_gauss)
        real(real64) :: work_D_HT(workspace%num_fe_dimension, workspace%num_fe_dimension, workspace%num_fe_gauss)
        real(real64) :: work_matrix_coupling(workspace%num_fe_nodes, workspace%num_fe_nodes)
        real(real64) :: D_HT_tmp(workspace%num_fe_dimension, workspace%num_fe_dimension)
        ! Nodal total-potential head driving the liquid Darcy flux, expressed as
        ! a pressure: P_gen = -psi_eff = -(psi_cap + psi_cryo). It equals the
        ! pore pressure where unfrozen and adds the cryosuction where frozen,
        ! and because d psi_eff/d psi_cap = 1 the K_HH pressure diagonal remains
        ! the consistent tangent while the cryogenic part migrates water to the
        ! freezing front.
        real(real64) :: P_gen_node(workspace%num_fe_nodes), psi_eff_i
        ! dP_gen/dT [Pa/K] = -d psi_cryo/dT per node: the consistent temperature
        ! tangent of the cryosuction flux, assembled into K_HT.
        real(real64) :: dPgen_dT_node(workspace%num_fe_nodes), dh_dT_i
        logical :: thermal_target, coupling_mass_needed, coupling_flux_needed
        logical :: coupling_block_needed

        n_nodes = workspace%num_fe_nodes
        n_gauss = workspace%num_fe_gauss
        n_dim = workspace%num_fe_dimension
        thermal_target = control%is_target(PHYSICS_TYPES%THERMAL, workspace%material_id)
        coupling_mass_needed = present(K_HT) .and. thermal_target
        coupling_flux_needed = (present(F_H) .or. present(K_HT)) .and. thermal_target
        coupling_block_needed = present(K_HT) .and. thermal_target

        bdf0 = workspace%bdf_coeffs(1)
        dt_local = 0.0d0
        call control%get_dt(dt_local)

        workspace%work_C(:) = 0.0d0
        workspace%work_D(:, :, :) = 0.0d0
        workspace%work_V(:, :) = 0.0d0
        workspace%work_d_dt(:) = 0.0d0
        local_vec_res(:) = 0.0d0
        work_C_HT(:) = 0.0d0
        work_sink(:) = 0.0d0
        work_D_HT(:, :, :) = 0.0d0

        ! ----------------------------------------------------------------
        ! 0. Cut detection: nodal level set phi = T_high(p_w) - T.
        !    The subcell split supports 2D triangle/quad elements; other
        !    element families fall back to the standard rule.
        ! ----------------------------------------------------------------
        is_cut = .false.
        use_subcell = self%enable_fringe_subcell_quadrature &
                      .and. self%physics%has_cryo_transport(workspace%material_id) &
                      .and. n_dim == 2
        n_sub_qps = 0
        if (use_subcell) then
            do i = 1, n_nodes
                call self%physics%calc_density_water(workspace%state(i), rho_w_node_sub)
                call calc_T_high_celsius(workspace%P_node(i), rho_w_node_sub, T_high_node)
                phi_nodes(i) = T_high_node - workspace%T_node(i)
            end do
            is_cut = any(phi_nodes(1:n_nodes) > 0.0d0) .and. any(phi_nodes(1:n_nodes) <= 0.0d0)
            if (is_cut) then
                call build_interface_quadrature_points(workspace%fe, phi_nodes(1:n_nodes), sub_qps, n_sub_qps)
                ! Unsupported element family: fall back to the standard rule
                ! rather than silently dropping the diffusion integral.
                if (n_sub_qps == 0) is_cut = .false.
            end if
        end if

        ! ----------------------------------------------------------------
        ! 1. Gauss loop: storage and sink terms at all Gauss points;
        !    flux coefficients only for uncut elements.
        ! ----------------------------------------------------------------
        do i = 1, n_gauss
            call self%compute_iteration_capacity(workspace%material_id, workspace%state_gp(i), workspace%work_C(i))
            if (coupling_mass_needed) then
                call self%compute_coupling_mass_term(workspace%material_id, workspace%state_gp(i), work_C_HT(i))
            end if
            call self%compute_transient_term_mixed(workspace%material_id, workspace%state_gp(i), &
                                                   workspace%bdf_coeffs, workspace%work_d_dt(i))

            if (.not. is_cut) then
                call self%compute_advective_term(workspace%material_id, workspace%state_gp(i), &
                                                 workspace%work_V(:, i))
                call self%compute_diffusion_term(workspace%material_id, workspace%state_gp(i), workspace%work_D(:, :, i))

                if (coupling_flux_needed) then
                    D_HT_tmp(:, :) = 0.0d0
                    call self%compute_coupling_diffusion_term(workspace%material_id, &
                                                              workspace%state_gp(i), D_HT_tmp)
                    do d = 1, n_dim
                        work_D_HT(d, d, i) = D_HT_tmp(1, 1)
                    end do
                end if
            end if

            if (thermal_target) then
                call self%calc_segregation_sink(workspace%material_id, workspace%state_gp(i), dt_local, work_sink(i))
            end if
        end do

        ! Total-potential nodal driver (pore pressure + cryosuction) and its
        ! temperature sensitivity.
        do i = 1, n_nodes
            call workspace%state(i)%effective_suction%get(psi_eff_i)
            P_gen_node(i) = -psi_eff_i
        end do
        dPgen_dT_node(:) = 0.0d0
        if (coupling_block_needed) then
            do i = 1, n_nodes
                call self%physics%calc_cryo_head_dT(workspace%material_id, workspace%state(i), dh_dT_i)
                dPgen_dT_node(i) = rho_std * g * dh_dT_i
            end do
        end if

        ! ----------------------------------------------------------------
        ! 2. Mass Matrix K1 (LHS, factor bdf0)
        ! ----------------------------------------------------------------
        call workspace%compute_K1(workspace%work_C, workspace%work_matrix)
        if (present(K_HH)) then
            do j = 1, n_nodes
                do i = 1, n_nodes
                    call K_HH%set(MATRIX_OPS%ADD, i, j, bdf0 * workspace%work_matrix(i, j))
                end do
            end do
        end if

        if (coupling_mass_needed) then
            call workspace%compute_K1(work_C_HT, workspace%work_matrix)
            do j = 1, n_nodes
                do i = 1, n_nodes
                    call K_HT%set(MATRIX_OPS%ADD, i, j, bdf0 * workspace%work_matrix(i, j))
                end do
            end do
        end if

        ! ----------------------------------------------------------------
        ! 4. Flux terms.
        !    Uncut element: standard Gauss rule (coefficients from step 1).
        !    Cut element: interface-split subcell rule for D_HH, D_HT, and
        !    gravity advection (replacement, so nothing is double-counted).
        ! ----------------------------------------------------------------
        if (.not. is_cut) then
            if (coupling_flux_needed) then
                call workspace%compute_K2(work_D_HT, work_matrix_coupling)
                if (present(K_HT)) then
                    do j = 1, n_nodes
                        do i = 1, n_nodes
                            call K_HT%set(MATRIX_OPS%ADD, i, j, work_matrix_coupling(i, j))
                        end do
                    end do
                end if
                if (present(F_H)) then
                    workspace%work_vec(:) = 0.0d0
                    call matvec(work_matrix_coupling, workspace%T_node, workspace%work_vec, ierr)
                    local_vec_res(:) = local_vec_res(:) + workspace%work_vec(:)
                end if
            end if

            call workspace%compute_K2(workspace%work_D, workspace%work_matrix)
            if (present(K_HH)) then
                do j = 1, n_nodes
                    do i = 1, n_nodes
                        call K_HH%set(MATRIX_OPS%ADD, i, j, workspace%work_matrix(i, j))
                    end do
                end do
            end if
            ! K_HT: consistent T-tangent of the total-potential flux
            ! -K2(D_HH)*P_gen, i.e. column j of K2(D_HH) scaled by dP_gen/dT.
            ! This block is NOT optional once P_gen drives the residual:
            ! dP_gen/dT = -d psi_cryo/dT is ~1.2e6 Pa/K, so omitting it leaves
            ! the pressure equation absorbing ~1e5 Pa of unmodelled forcing per
            ! 0.1 K of temperature change - measured, resH/0 then runs to 5.6e3
            ! and the conserved increment overflows. The dangerous off-diagonal
            ! product with the thermal block is broken on the K_TH side instead
            ! (thermal_matrix.F90), giving one-way T->p coupling.
            if (coupling_block_needed) then
                do j = 1, n_nodes
                    do i = 1, n_nodes
                        call K_HT%set(MATRIX_OPS%ADD, i, j, workspace%work_matrix(i, j) * dPgen_dT_node(j))
                    end do
                end do
            end if
            if (present(F_H)) then
                do i = 1, n_nodes
                    do j = 1, n_nodes
                        local_vec_res(i) = local_vec_res(i) + workspace%work_matrix(i, j) * P_gen_node(j)
                    end do
                end do
            end if
        else
            do i = 1, n_nodes
                call workspace%state(i)%porosity%get(porosity_nodes(i))
            end do

            mat_HH_sub(:, :) = 0.0d0
            mat_HT_sub(:, :) = 0.0d0
            vec_V_sub(:) = 0.0d0

            do q_s = 1, n_sub_qps
                r_sub%x = sub_qps(q_s)%xi
                r_sub%y = sub_qps(q_s)%eta
                r_sub%z = 0.0d0

                call workspace%fe%lerp(r_sub, workspace%T_node(1:n_nodes), T_q_sub)
                call workspace%fe%lerp(r_sub, workspace%P_node(1:n_nodes), P_q_sub)
                call workspace%fe%lerp(r_sub, porosity_nodes(1:n_nodes), porosity_q_sub)
                call workspace%fe%lerp(r_sub, workspace%Qi_node(1:n_nodes), Qi_q_sub)

                call state_sub%copy(workspace%state(1))
                call state_sub%temperature%set(T_q_sub)
                call state_sub%pressure%set(P_q_sub)
                call state_sub%porosity%set(porosity_q_sub)
                call state_sub%ice_content%set(Qi_q_sub)
                call self%update_water_phases(workspace%material_id, state_sub)

                dpsi_dx_sub(:, :) = 0.0d0
                call workspace%fe%calc_shape_function(r_sub, workspace%coordinates, &
                                                      dpsi_dx=dpsi_dx_sub, determinant_jacobian=det_J_sub)

                eff_weight_sub = sub_qps(q_s)%weight * abs(det_J_sub)

                coeff_sub_mat(:, :) = 0.0d0
                call self%compute_diffusion_term(workspace%material_id, state_sub, coeff_sub_mat)
                D_HH_sub = coeff_sub_mat(1, 1)

                coeff_sub_mat(:, :) = 0.0d0
                call self%compute_coupling_diffusion_term(workspace%material_id, state_sub, coeff_sub_mat)
                D_HT_sub = coeff_sub_mat(1, 1)

                V_sub(:) = 0.0d0
                call self%compute_advective_term(workspace%material_id, state_sub, V_sub)

                do j = 1, n_nodes
                    do i = 1, n_nodes
                        mat_HH_sub(i, j) = mat_HH_sub(i, j) + eff_weight_sub * D_HH_sub * &
                                           dot_product(dpsi_dx_sub(:, i), dpsi_dx_sub(:, j))
                        mat_HT_sub(i, j) = mat_HT_sub(i, j) + eff_weight_sub * D_HT_sub * &
                                           dot_product(dpsi_dx_sub(:, i), dpsi_dx_sub(:, j))
                    end do
                end do
                do i = 1, n_nodes
                    vec_V_sub(i) = vec_V_sub(i) + eff_weight_sub * &
                                   dot_product(dpsi_dx_sub(:, i), V_sub)
                end do
            end do

            if (present(K_HH)) then
                do j = 1, n_nodes
                    do i = 1, n_nodes
                        call K_HH%set(MATRIX_OPS%ADD, i, j, mat_HH_sub(i, j))
                    end do
                end do
            end if
            if (present(K_HT) .and. thermal_target) then
                do j = 1, n_nodes
                    do i = 1, n_nodes
                        call K_HT%set(MATRIX_OPS%ADD, i, j, &
                                      mat_HT_sub(i, j) + mat_HH_sub(i, j) * dPgen_dT_node(j))
                    end do
                end do
            end if
            if (present(F_H)) then
                workspace%work_vec(:) = 0.0d0
                call matvec(mat_HH_sub, P_gen_node, workspace%work_vec, ierr)
                do i = 1, n_nodes
                    local_vec_res(i) = local_vec_res(i) + workspace%work_vec(i)
                end do
            end if

            if (present(F_H) .and. thermal_target) then
                workspace%work_vec(:) = 0.0d0
                call matvec(mat_HT_sub, workspace%T_node, workspace%work_vec, ierr)
                do i = 1, n_nodes
                    local_vec_res(i) = local_vec_res(i) + workspace%work_vec(i)
                end do
            end if
        end if

        ! ----------------------------------------------------------------
        ! 5. Residual Assembly
        ! ----------------------------------------------------------------
        if (present(F_H)) then
            workspace%work_vec(:) = 0.0d0
            call workspace%compute_R1(workspace%work_d_dt, workspace%work_vec)
            local_vec_res(:) = local_vec_res(:) + workspace%work_vec(:)

            if (is_cut) then
                local_vec_res(:) = local_vec_res(:) - vec_V_sub(:)
            else
                workspace%work_vec(:) = 0.0d0
                call workspace%compute_R2(workspace%work_V, workspace%work_vec)
                local_vec_res(:) = local_vec_res(:) - workspace%work_vec(:)
            end if

            workspace%work_vec(:) = 0.0d0
            call workspace%compute_R1(work_sink, workspace%work_vec)
            local_vec_res(:) = local_vec_res(:) + workspace%work_vec(:)

            do i = 1, n_nodes
                call F_H%set(VECTOR_OPS%ADD, i, -local_vec_res(i))
            end do
        end if

    end subroutine assemble_local_picard_hydraulic

end submodule hydraulic_matrix

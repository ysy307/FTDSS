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
    !> The diffusion terms D_HH and D_HT are discontinuous in the state across
    !> the freezing interface \(\phi = T_{high}(p_w) - T = 0\) (the capillary /
    !> cryogenic switch of the generalized suction).  Elements cut by the
    !> interface are therefore integrated with the interface-split subcell rule
    !> (build_interface_quadrature_points), which REPLACES the standard Gauss
    !> rule for the diffusion terms in those elements: each subcell point lies
    !> strictly on one side, and the subcell weights vary continuously with the
    !> nodal unknowns.  This keeps the assembled residual continuous in
    !> (T, p_w) at the moving free boundary - the property Picard/Newton needs
    !> to contract - without any regularization parameter.
    !>
    !> Terms whose integrands are continuous across the interface (storage
    !> C_eq, mixed transient, gravity advection, segregation sink) keep the
    !> standard Gauss rule in all elements.
    !>
    !> ### Fringe K-averaging
    !> Even away from \(\phi = 0\), K is not smooth enough for a low-order
    !> pointwise Gauss rule: the impedance factor \(10^{-\Omega Q(T)}\) rises
    !> steeply over a narrow band directly below \(T_{high}(p_w)\) on the frozen
    !> side. When an element/subcell's nodal T range straddles that band
    !> (fringe_transition_active), D_HH and D_HT are instead evaluated as their
    !> 1D path average of K(T) over that range (compute_diffusion_term_K_averaged),
    !> holding every other state variable fixed at the quadrature point's value.
    !> This REPLACES the pointwise evaluation at the affected points only; the
    !> weak form and the block Gauss-Seidel (T, p) lagging below are unchanged.
    !> Controlled by enable_fringe_K_averaging (default on); disabling it restores
    !> the pointwise rule everywhere.
    !>
    !> ### Linearization of the T-p coupling
    !> The K_HT block is intentionally left zero: the cryosuction flux
    !> K2(D_HT)*T and the mixed storage dTheta/dt enter the RESIDUAL exactly,
    !> so the converged solution is unchanged (block Gauss-Seidel lagging).
    !> Putting K2(D_HT) on the LHS creates, together with the latent-heat
    !> mass coupling C_TH of the thermal block, a dt-independent off-diagonal
    !> product \( C_{TH} K_2(D_{HT}) / (C_{eq} K_2(\lambda)) \gg 1 \) at
    !> freezing-interface nodes: the coupled linear solve then amplifies and
    !> drives T and p to the validity walls at any dt.  The lagged coupling
    !> is stabilized by the adaptive under-relaxation of the conserved
    !> Picard loop.
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
        real(real64) :: T_q_sub, P_q_sub, porosity_q_sub
        real(real64) :: D_HH_sub, D_HT_sub, eff_weight_sub, det_J_sub
        real(real64) :: coeff_sub_mat(workspace%num_fe_dimension, workspace%num_fe_dimension)
        real(real64) :: mat_HH_sub(workspace%num_fe_nodes, workspace%num_fe_nodes)
        real(real64) :: mat_HT_sub(workspace%num_fe_nodes, workspace%num_fe_nodes)
        real(real64) :: dpsi_dx_sub(workspace%num_fe_dimension, workspace%num_fe_nodes)
        type(type_coordinate_dp) :: r_sub
        type(type_state) :: state_sub

        ! --- Standard assembly variables ---
        real(real64) :: local_vec_res(workspace%num_fe_nodes)
        real(real64) :: work_sink(workspace%num_fe_gauss)
        real(real64) :: work_D_HT(workspace%num_fe_dimension, workspace%num_fe_dimension, workspace%num_fe_gauss)
        real(real64) :: D_HH_node(workspace%num_fe_nodes), D_HT_node(workspace%num_fe_nodes)
        real(real64) :: D_HH_elem, D_HT_elem
        real(real64) :: D_node_tmp(workspace%num_fe_dimension, workspace%num_fe_dimension)
        logical :: use_nodal_avg
        real(real64) :: work_matrix_coupling(workspace%num_fe_nodes, workspace%num_fe_nodes)
        real(real64) :: D_HT_tmp(workspace%num_fe_dimension, workspace%num_fe_dimension)
        logical :: thermal_target, coupling_flux_needed

        ! --- Fringe K-averaging variables ---
        logical :: use_K_averaging, fire_avg
        real(real64) :: T_min_elem, T_max_elem, rho_w_probe, T_high_probe
        real(real64) :: D_HH_avg, D_HT_avg

        n_nodes = workspace%num_fe_nodes
        n_gauss = workspace%num_fe_gauss
        n_dim = workspace%num_fe_dimension
        thermal_target = control%is_target(PHYSICS_TYPES%THERMAL, workspace%material_id)
        coupling_flux_needed = present(F_H) .and. thermal_target

        bdf0 = workspace%bdf_coeffs(1)
        dt_local = 0.0d0
        call control%get_dt(dt_local)

        workspace%work_C(:) = 0.0d0
        workspace%work_D(:, :, :) = 0.0d0
        workspace%work_V(:, :) = 0.0d0
        workspace%work_d_dt(:) = 0.0d0
        local_vec_res(:) = 0.0d0
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
        ! 0b. Fringe K-averaging setup: element-level nodal T range, used to
        !     decide (per Gauss/subcell point, since T_high depends on the
        !     local P) whether the pointwise K(T) evaluation is replaced by
        !     its 1D path average over that range. See fringe_transition_active.
        ! ----------------------------------------------------------------
        use_K_averaging = self%enable_fringe_K_averaging &
                          .and. self%physics%has_cryo_transport(workspace%material_id)
        T_min_elem = 0.0d0
        T_max_elem = 0.0d0
        if (use_K_averaging) then
            T_min_elem = minval(workspace%T_node(1:n_nodes))
            T_max_elem = maxval(workspace%T_node(1:n_nodes))
        end if

        ! ----------------------------------------------------------------
        ! 0b. Internodal (element-arithmetic) conductivity.
        !
        ! D_HH and D_HT are evaluated at the element NODES and averaged over the
        ! element, replacing the pointwise Gauss-point evaluation for the whole
        ! element (the interface subcell split and the K(T) path average, both of
        ! which refine the POINTWISE coefficient, are bypassed - they answer a
        ! different question). Across a freezing fringe K drops by many orders of
        ! magnitude within one element; the Galerkin stiffness built from
        ! pointwise K is then dominated by the cold-side value and the element
        ! conductance collapses, so liquid can no longer migrate into the frozen
        ! zone once the front has passed. The arithmetic internodal average keeps
        ! the element conductance of the order of the warm-side value, which is
        ! the flux discretization of the finite-difference codes this freezing
        ! model was calibrated against.
        ! ----------------------------------------------------------------
        use_nodal_avg = self%enable_nodal_K_averaging .and. self%physics%has_cryo_transport(workspace%material_id)
        D_HH_elem = 0.0d0
        D_HT_elem = 0.0d0
        if (use_nodal_avg) then
            do i = 1, n_nodes
                D_node_tmp(:, :) = 0.0d0
                call self%compute_diffusion_term(workspace%material_id, workspace%state(i), D_node_tmp)
                D_HH_node(i) = D_node_tmp(1, 1)
                D_HT_node(i) = 0.0d0
                if (coupling_flux_needed) then
                    D_node_tmp(:, :) = 0.0d0
                    call self%compute_coupling_diffusion_term(workspace%material_id, workspace%state(i), D_node_tmp)
                    D_HT_node(i) = D_node_tmp(1, 1)
                end if
            end do
            D_HH_elem = sum(D_HH_node(1:n_nodes)) / real(n_nodes, real64)
            if (coupling_flux_needed) D_HT_elem = sum(D_HT_node(1:n_nodes)) / real(n_nodes, real64)
            is_cut = .false.
            n_sub_qps = 0
        end if

        ! ----------------------------------------------------------------
        ! 1. Gauss loop: continuous-integrand terms at all Gauss points;
        !    diffusion coefficients only for uncut elements.
        ! ----------------------------------------------------------------
        do i = 1, n_gauss
            call self%compute_iteration_capacity(workspace%material_id, workspace%state_gp(i), workspace%work_C(i))
            call self%compute_advective_term(workspace%material_id, workspace%state_gp(i), workspace%work_V(:, i))
            call self%compute_transient_term_mixed(workspace%material_id, workspace%state_gp(i), &
                                                   workspace%bdf_coeffs, workspace%work_d_dt(i))

            if (use_nodal_avg) then
                workspace%work_D(:, :, i) = 0.0d0
                do d = 1, n_dim
                    workspace%work_D(d, d, i) = D_HH_elem
                end do
                if (coupling_flux_needed) then
                    do d = 1, n_dim
                        work_D_HT(d, d, i) = D_HT_elem
                    end do
                end if
            else if (.not. is_cut) then
                fire_avg = .false.
                if (use_K_averaging) then
                    call self%physics%calc_density_water(workspace%state_gp(i), rho_w_probe)
                    call calc_T_high_celsius(workspace%P_gp(i), rho_w_probe, T_high_probe)
                    fire_avg = fringe_transition_active(T_min_elem, T_max_elem, T_high_probe)
                end if

                if (fire_avg) then
                    call self%compute_diffusion_term_K_averaged(workspace%material_id, workspace%state_gp(i), &
                                                                 T_min_elem, T_max_elem, coupling_flux_needed, &
                                                                 D_HH_avg, D_HT_avg)
                    workspace%work_D(:, :, i) = 0.0d0
                    do d = 1, n_dim
                        workspace%work_D(d, d, i) = D_HH_avg
                    end do
                    if (coupling_flux_needed) then
                        do d = 1, n_dim
                            work_D_HT(d, d, i) = D_HT_avg
                        end do
                    end if
                else
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
            end if

            if (thermal_target) then
                call self%calc_segregation_sink(workspace%material_id, workspace%state_gp(i), dt_local, work_sink(i))
            end if
        end do

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

        ! K_HT stays zero: the T-p coupling is carried exactly by the mixed
        ! storage and the D_HT flux in the residual (see the header).

        ! ----------------------------------------------------------------
        ! 4. Diffusion terms.
        !    Uncut element: standard Gauss rule (coefficients from step 1).
        !    Cut element: interface-split subcell rule for BOTH D_HH and
        !    D_HT (replacement, so nothing is double-counted).
        ! ----------------------------------------------------------------
        if (.not. is_cut) then
            if (coupling_flux_needed) then
                call workspace%compute_K2(work_D_HT, work_matrix_coupling)
                workspace%work_vec(:) = 0.0d0
                call matvec(work_matrix_coupling, workspace%T_node, workspace%work_vec, ierr)
                do i = 1, n_nodes
                    local_vec_res(i) = local_vec_res(i) + workspace%work_vec(i)
                end do
            end if

            call workspace%compute_K2(workspace%work_D, workspace%work_matrix)
            if (present(K_HH)) then
                do j = 1, n_nodes
                    do i = 1, n_nodes
                        call K_HH%set(MATRIX_OPS%ADD, i, j, workspace%work_matrix(i, j))
                    end do
                end do
            end if
            if (present(F_H)) then
                do i = 1, n_nodes
                    do j = 1, n_nodes
                        local_vec_res(i) = local_vec_res(i) + workspace%work_matrix(i, j) * workspace%P_node(j)
                    end do
                end do
            end if
        else
            do i = 1, n_nodes
                call workspace%state(i)%porosity%get(porosity_nodes(i))
            end do

            mat_HH_sub(:, :) = 0.0d0
            mat_HT_sub(:, :) = 0.0d0

            do q_s = 1, n_sub_qps
                r_sub%x = sub_qps(q_s)%xi
                r_sub%y = sub_qps(q_s)%eta
                r_sub%z = 0.0d0

                call workspace%fe%lerp(r_sub, workspace%T_node(1:n_nodes), T_q_sub)
                call workspace%fe%lerp(r_sub, workspace%P_node(1:n_nodes), P_q_sub)
                call workspace%fe%lerp(r_sub, porosity_nodes(1:n_nodes), porosity_q_sub)

                call state_sub%copy(workspace%state(1))
                call state_sub%temperature%set(T_q_sub)
                call state_sub%pressure%set(P_q_sub)
                call state_sub%porosity%set(porosity_q_sub)
                call self%update_water_phases(workspace%material_id, state_sub)

                dpsi_dx_sub(:, :) = 0.0d0
                call workspace%fe%calc_shape_function(r_sub, workspace%coordinates, &
                                                      dpsi_dx=dpsi_dx_sub, determinant_jacobian=det_J_sub)

                eff_weight_sub = sub_qps(q_s)%weight * abs(det_J_sub)

                ! Cut elements already isolate the phi = 0 discontinuity via the
                ! subcell split; the steep Q(T) impedance rise that motivates
                ! K-averaging lives strictly on the frozen side of that boundary
                ! (see hydraulic_matrix.F90 module header / design memo), so the
                ! same element-level [T_min_elem, T_max_elem] trigger applies here
                ! (no distinct per-subcell nodal T range is available: subcells
                ! carry only quadrature points, not their own corner nodes).
                fire_avg = .false.
                if (use_K_averaging) then
                    call self%physics%calc_density_water(state_sub, rho_w_probe)
                    call calc_T_high_celsius(P_q_sub, rho_w_probe, T_high_probe)
                    fire_avg = fringe_transition_active(T_min_elem, T_max_elem, T_high_probe)
                end if

                if (fire_avg) then
                    call self%compute_diffusion_term_K_averaged(workspace%material_id, state_sub, &
                                                                 T_min_elem, T_max_elem, .true., &
                                                                 D_HH_sub, D_HT_sub)
                else
                    coeff_sub_mat(:, :) = 0.0d0
                    call self%compute_diffusion_term(workspace%material_id, state_sub, coeff_sub_mat)
                    D_HH_sub = coeff_sub_mat(1, 1)

                    coeff_sub_mat(:, :) = 0.0d0
                    call self%compute_coupling_diffusion_term(workspace%material_id, state_sub, coeff_sub_mat)
                    D_HT_sub = coeff_sub_mat(1, 1)
                end if

                do j = 1, n_nodes
                    do i = 1, n_nodes
                        mat_HH_sub(i, j) = mat_HH_sub(i, j) + eff_weight_sub * D_HH_sub * &
                                           dot_product(dpsi_dx_sub(:, i), dpsi_dx_sub(:, j))
                        mat_HT_sub(i, j) = mat_HT_sub(i, j) + eff_weight_sub * D_HT_sub * &
                                           dot_product(dpsi_dx_sub(:, i), dpsi_dx_sub(:, j))
                    end do
                end do
            end do

            if (present(K_HH)) then
                do j = 1, n_nodes
                    do i = 1, n_nodes
                        call K_HH%set(MATRIX_OPS%ADD, i, j, mat_HH_sub(i, j))
                    end do
                end do
            end if
            if (present(F_H)) then
                workspace%work_vec(:) = 0.0d0
                call matvec(mat_HH_sub, workspace%P_node, workspace%work_vec, ierr)
                do i = 1, n_nodes
                    local_vec_res(i) = local_vec_res(i) + workspace%work_vec(i)
                end do
            end if

            if (coupling_flux_needed) then
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

            workspace%work_vec(:) = 0.0d0
            call workspace%compute_R2(workspace%work_V, workspace%work_vec)
            local_vec_res(:) = local_vec_res(:) - workspace%work_vec(:)

            workspace%work_vec(:) = 0.0d0
            call workspace%compute_R1(work_sink, workspace%work_vec)
            local_vec_res(:) = local_vec_res(:) + workspace%work_vec(:)

            do i = 1, n_nodes
                call F_H%set(VECTOR_OPS%ADD, i, -local_vec_res(i))
            end do
        end if

    end subroutine assemble_local_picard_hydraulic

    !> Decide whether the pointwise K(T) evaluation at an element/subcell is
    !> replaced by its path average over the nodal range [T_min, T_max]
    !> (see compute_diffusion_term_K_averaged_hydraulic).
    !>
    !> Fires when [T_min, T_max] overlaps the band (T_high - BAND_MARGIN_K, T_high)
    !> below the freezing isotherm T_high(p_w), AND the element resolves more than
    !> DT_FLOOR_K of temperature variation (elements narrower than that cannot alias
    !> the transition, so the pointwise and averaged rules coincide to floating-point
    !> precision and the extra evaluations would be wasted).
    !>
    !> Constants (no free parameters): the Hansson impedance factor used by this
    !> model is \(10^{-\Omega Q(T)}\) with the calibrated exponent \(\Omega = 7\)
    !> (see hcf_base.F90 calc_impedance_ratio doc; project/Mizo-xz-Convex/Input/
    !> Basic.json sets impedance_factor = 7). Prior numerical investigation of this
    !> model's Q(T) profile (see design memo) found the steep rise of Q from 0 to
    !> ~1 concentrated within ~0.1 K directly below T_high, i.e. dQ/dT ~ 1/(0.1 K).
    !> One decade of impedance change requires \(\Delta Q = 1/\Omega\), hence
    !> \(\Delta T_{decade} = \Delta Q / (dQ/dT) \approx 0.1\,K/\Omega \approx 0.014\,K\);
    !> DT_FLOOR_K = 0.02 K is that decade-width rounded up (conservative: fires only
    !> when the element resolves at least one decade of impedance change).
    !> BAND_MARGIN_K = 1.0 K is a 10x safety margin over the empirical 0.1 K
    !> transition width, absorbing the nonlinearity of T_high(p_w) across element
    !> nodes and the arbitrary placement of the mesh relative to the transition.
    pure function fringe_transition_active(T_min, T_max, T_high) result(fire)
        implicit none
        real(real64), intent(in) :: T_min
        real(real64), intent(in) :: T_max
        real(real64), intent(in) :: T_high
        logical :: fire

        real(real64), parameter :: BAND_MARGIN_K = 1.0d0
        real(real64), parameter :: DT_FLOOR_K = 0.02d0

        fire = (T_min < T_high) .and. (T_max > T_high - BAND_MARGIN_K) .and. (T_max - T_min > DT_FLOOR_K)

    end function fringe_transition_active

end submodule hydraulic_matrix

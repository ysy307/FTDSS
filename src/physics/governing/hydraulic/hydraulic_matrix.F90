submodule(physics_governing_hydraulic) hydraulic_matrix
    use :: domain_fe_subcell, only: type_subcell_quadrature_point, SUBCELL_QUADRATURE_CAPACITY
    implicit none

    !> TEMPORARY Step-0 verification diagnostic (see report_subcell_debug_counts_hydraulic).
    !> Default .false.; flip to .true. only to re-measure the firing rate.
    logical, parameter :: DEBUG_SUBCELL_COUNTER = .false.

    integer(int32), save :: dbg_elements_considered = 0
    integer(int32), save :: dbg_elements_cut = 0
    real(real64), save :: dbg_phi_min = huge(1.0d0)
    real(real64), save :: dbg_phi_max = -huge(1.0d0)
    real(real64), save :: dbg_abs_phi_min = huge(1.0d0)

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
    !> interface, where ice first appears (see the cut-detection block below
    !> for the level-set definition).  Elements cut by the
    !> interface are therefore integrated with the interface-split subcell rule
    !> (build_interface_quadrature_points), which REPLACES the standard Gauss
    !> rule for the pressure, temperature, and gravity fluxes in those elements.
    !> Using one quadrature partition for all three parts of the Darcy flux
    !> avoids a discrete imbalance caused solely by sampling the same flux at
    !> different points.
    !>
    !> The storage term (C_eq -> K1, and its residual counterpart d(Theta)/dt)
    !> is likewise moved onto the subcell rule for cut elements, in the SAME
    !> q_s loop as the fluxes: dTheta/dp collapses across the same front the
    !> flux coefficients do, so sampling it at Gauss points would reintroduce
    !> the discrete imbalance the flux subcell rule was built to remove.
    !> The segregation sink keeps the standard Gauss rule in all elements.
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
        type(type_subcell_quadrature_point) :: subcell_quadrature_points(SUBCELL_QUADRATURE_CAPACITY)
        integer(int32) :: num_subcell_quadrature_points, q_s
        logical :: use_subcell, is_cut
        real(real64) :: phi_nodes(workspace%num_fe_nodes)
        real(real64) :: porosity_nodes(workspace%num_fe_nodes)
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
        ! --- Storage term on the subcell rule (Step 3: C_eq has the same
        !     sub-element front as the flux coefficients above) ---
        real(real64) :: psi_sub(workspace%num_fe_nodes)
        real(real64) :: C_eq_sub, C_HT_storage_sub, d_dt_sub
        real(real64) :: mat_C_sub(workspace%num_fe_nodes, workspace%num_fe_nodes)
        real(real64) :: mat_C_HT_sub(workspace%num_fe_nodes, workspace%num_fe_nodes)
        real(real64) :: vec_ddt_sub(workspace%num_fe_nodes)
        integer(int32) :: k, n_hist, hlen
        real(real64) :: T_hist_node(workspace%bdf_order + 1, workspace%num_fe_nodes)
        real(real64) :: P_hist_node(workspace%bdf_order + 1, workspace%num_fe_nodes)
        real(real64) :: Qi_hist_node(workspace%bdf_order + 1, workspace%num_fe_nodes)
        real(real64) :: T_hist_sub(workspace%bdf_order + 1)
        real(real64) :: P_hist_sub(workspace%bdf_order + 1)
        real(real64) :: Qi_hist_sub(workspace%bdf_order + 1)
        real(real64), pointer, contiguous, dimension(:) :: hist_ptr
        logical :: hist_is_set

        ! --- Standard assembly variables ---
        real(real64) :: local_vec_res(workspace%num_fe_nodes)
        real(real64) :: work_C_HT(workspace%num_fe_gauss)
        real(real64) :: work_sink(workspace%num_fe_gauss)
        real(real64) :: work_D_HT(workspace%num_fe_dimension, workspace%num_fe_dimension, workspace%num_fe_gauss)
        real(real64) :: work_matrix_coupling(workspace%num_fe_nodes, workspace%num_fe_nodes)
        real(real64) :: D_HT_tmp(workspace%num_fe_dimension, workspace%num_fe_dimension)
        ! Nodal transport coefficients and the element values formed from them.
        real(real64) :: D_HH_node(workspace%num_fe_nodes), D_HT_node(workspace%num_fe_nodes)
        real(real64) :: V_node_mag(workspace%num_fe_nodes)
        real(real64) :: D_HH_elem, D_HT_elem, V_elem_mag
        real(real64) :: V_dir(workspace%num_fe_dimension), V_node_tmp(workspace%num_fe_dimension)
        ! Nodal Darcy driver and its tangents in the two primary unknowns.
        real(real64) :: P_gen_node(workspace%num_fe_nodes), psi_eff_i
        real(real64) :: dPgen_dP_node(workspace%num_fe_nodes), dSeff_dP_i
        real(real64) :: dPgen_dT_node(workspace%num_fe_nodes), dSeff_dT_i
        integer(int32) :: nonlinear_iteration
        real(real64) :: capacity_regularization
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
        ! 0. Cut detection: nodal level set phi = (s_f - s_m) + eps_s, the
        !    SAME switch calc_phase_split uses to decide ice existence
        !    (s_m = -p_w, s_f = the raw generalized-Clapeyron suction; see
        !    type_fusion%calc_freezing_level_set for the full derivation and
        !    why phi>0 iff ice_content>0 exactly, given the compact-support
        !    smoothing in fusion.F90). Computing phi from the split's own
        !    s_m,s_f - rather than from a separately defined critical
        !    temperature T_high(p_w) - is what keeps "cut" elements from
        !    drifting away from where the constitutive split actually places
        !    the phase change.
        !    The subcell split supports 2D triangle/quad elements; other
        !    element families fall back to the standard rule.
        !
        !    workspace%compute_interface_subcell computes this ONCE per
        !    element and caches it on the workspace: THERMAL calls the same
        !    method with its own (identical) constitutive manager, so the two
        !    blocks always agree on where the interface is and never redo the
        !    per-node calc_freezing_level_set evaluation twice.
        ! ----------------------------------------------------------------
        use_subcell = self%enable_fringe_subcell_quadrature &
                      .and. self%physics%has_cryo_transport(workspace%material_id) &
                      .and. n_dim == 2
        call workspace%compute_interface_subcell(self%physics, workspace%material_id, &
                                                 self%enable_fringe_subcell_quadrature)
        is_cut = workspace%is_cut
        num_subcell_quadrature_points = workspace%num_subcell_quadrature_points
        if (num_subcell_quadrature_points > 0) then
            subcell_quadrature_points(1:num_subcell_quadrature_points) = &
                workspace%subcell_quadrature_points(1:num_subcell_quadrature_points)
        end if

        if (DEBUG_SUBCELL_COUNTER .and. use_subcell) then
            ! Diagnostic-only: phi is recomputed locally here (cheap, O(n_nodes))
            ! rather than exposed by compute_interface_subcell, since this
            ! counter is dead in production (DEBUG_SUBCELL_COUNTER = .false.).
            do i = 1, n_nodes
                call self%physics%calc_freezing_level_set(workspace%material_id, workspace%state(i), phi_nodes(i))
            end do
            !$OMP ATOMIC UPDATE
            dbg_elements_considered = dbg_elements_considered + 1
            if (is_cut) then
                !$OMP ATOMIC UPDATE
                dbg_elements_cut = dbg_elements_cut + 1
            end if
            !$OMP CRITICAL(subcell_debug_phi)
            dbg_phi_min = min(dbg_phi_min, minval(phi_nodes(1:n_nodes)))
            dbg_phi_max = max(dbg_phi_max, maxval(phi_nodes(1:n_nodes)))
            dbg_abs_phi_min = min(dbg_abs_phi_min, minval(abs(phi_nodes(1:n_nodes))))
            !$OMP END CRITICAL(subcell_debug_phi)
        end if

        ! ----------------------------------------------------------------
        ! 1. Gauss loop: storage and sink terms at all Gauss points;
        !    flux coefficients only for uncut elements.
        ! ----------------------------------------------------------------
        ! Pseudo-transient regularization of the pressure block.
        !
        ! At the freezing front the storage capacity dTheta/dp collapses on the
        ! pore-volume bound and the conductivity collapses under the impedance,
        ! so the pressure row loses both of its terms at once and the Jacobian
        ! becomes nearly singular. The Newton step is then unbounded - measured,
        ! max|du_p| = 2.8e5 Pa against an ambient 5.4e4 Pa, with the residual
        ! growing superlinearly in the step length, which is what left the line
        ! search with no admissible alpha.
        !
        ! The time-derivative term cannot regularize this: it enters as
        ! bdf0*K1(C_eq), so when C_eq itself vanishes the row stays singular no
        ! matter how small dt becomes. That is why cutting dt never helped -
        ! measured, dt = 26.9 s and 13.4 s failed where 73.4 s had succeeded.
        !
        ! A capacity is therefore added to the MATRIX only, decaying as 1/k with
        ! the nonlinear iteration so the operator starts strictly diagonally
        ! reinforced and approaches the true tangent as the iteration converges.
        ! The residual keeps the exact storage term, so the converged solution
        ! is untouched; only the path to it is bounded.
        call control%get_nonlinear_iter(nonlinear_iteration)
        capacity_regularization = 0.0d0
        ! Diagnostic-only bypass for the FD-Jacobian audit (see
        ! disable_capacity_regularization member doc): the term below is added
        ! to the MATRIX only, never the residual, so the audit's FD comparison
        ! target must be assembled without it to be comparable.
        if (CAPACITY_REGULARIZATION_ENABLED .and. .not. self%disable_capacity_regularization) then
            if (allocated(self%iteration_capacity_bound)) then
                if (workspace%material_id >= 1 .and. &
                    workspace%material_id <= size(self%iteration_capacity_bound)) then
                    capacity_regularization = self%iteration_capacity_bound(workspace%material_id) / &
                                              real(max(1, nonlinear_iteration), real64)
                end if
            end if
        end if

        do i = 1, n_gauss
            call self%compute_iteration_capacity(workspace%material_id, workspace%state_gp(i), workspace%work_C(i))
            workspace%work_C(i) = workspace%work_C(i) + capacity_regularization
            if (coupling_mass_needed) then
                call self%compute_coupling_mass_term(workspace%material_id, workspace%state_gp(i), work_C_HT(i))
            end if
            call self%compute_transient_term_mixed(workspace%material_id, workspace%state_gp(i), &
                                                   workspace%bdf_coeffs, workspace%work_d_dt(i))


            if (thermal_target) then
                call self%calc_segregation_sink(workspace%material_id, workspace%state_gp(i), dt_local, work_sink(i))
            end if
        end do

        ! ----------------------------------------------------------------
        ! Element transport coefficients from the GEOMETRIC mean of the nodal
        ! values, not from quadrature of the coefficient itself.
        !
        ! K collapses by about eight orders of magnitude over the first kelvin
        ! of undercooling - k_r falls with the cryosuction head and the Hansson
        ! impedance 10^(-Omega Q) falls with the ice fraction - while the
        ! driving potential does the opposite: psi_cryo runs at 1.22e6 Pa/K, so
        ! 0.1 K across a 1 mm element is a gradient of 1.2e8 Pa/m. Sampling K at
        ! Gauss points and integrating gives the element the conductivity of its
        ! warmest sample, which is 1e3 to 1e8 times the cold-side value, so an
        ! element straddling the front conducts as if it were unfrozen. With the
        ! warm-side K that is a Darcy flux of order 10 mm/h through a 200 mm
        ! column, against an experiment that redistributes a few percent of its
        ! water over 50 h - and the resulting imbalance is what left the
        ! hydraulic residual at 0.3 in water content per step, unreachable by
        ! any step length or time step.
        !
        ! The geometric mean is the standard internodal weighting for a
        ! coefficient that varies exponentially: it is exact for exponential K
        ! between two points and cannot exceed the larger nodal value.
        ! ----------------------------------------------------------------
        ! Element transport coefficients, shared by the standard and subcell
        ! paths so a cut/uncut flip does not change the discrete operator.
        do i = 1, n_nodes
            coeff_sub_mat(:, :) = 0.0d0
            call self%compute_diffusion_term(workspace%material_id, workspace%state(i), coeff_sub_mat)
            D_HH_node(i) = coeff_sub_mat(1, 1)

            D_HT_node(i) = 0.0d0
            if (coupling_flux_needed) then
                coeff_sub_mat(:, :) = 0.0d0
                call self%compute_coupling_diffusion_term(workspace%material_id, &
                                                          workspace%state(i), coeff_sub_mat)
                D_HT_node(i) = coeff_sub_mat(1, 1)
            end if

            V_node_tmp(:) = 0.0d0
            call self%compute_advective_term(workspace%material_id, workspace%state(i), V_node_tmp)
            V_node_mag(i) = sqrt(sum(V_node_tmp(:)**2))
            if (i == 1) then
                V_dir(:) = 0.0d0
                if (V_node_mag(i) > 0.0d0) V_dir(:) = V_node_tmp(:) / V_node_mag(i)
            end if
        end do

        D_HH_elem = geometric_mean(D_HH_node(1:n_nodes))
        D_HT_elem = signed_geometric_mean(D_HT_node(1:n_nodes))
        V_elem_mag = geometric_mean(V_node_mag(1:n_nodes))

        if (.not. is_cut) then
            do i = 1, n_gauss
                do d = 1, n_dim
                    workspace%work_D(d, d, i) = D_HH_elem
                    work_D_HT(d, d, i) = D_HT_elem
                end do
                workspace%work_V(:, i) = V_elem_mag * V_dir(:)
            end do
        end if

        ! p_w is the total-water-equivalent pressure of the closure, so the
        ! liquid pressure is p_l = -s_eff. s_eff is read from the state the
        ! phase split published (unclamped s_m), not from the public accessor.
        do i = 1, n_nodes
            if (HYDRAULIC_DRIVER_TOTAL_POTENTIAL) then
                call workspace%state(i)%effective_suction%get(psi_eff_i)
                call workspace%state(i)%dSeff_dP%get(dSeff_dP_i)
                call workspace%state(i)%dSeff_dT%get(dSeff_dT_i)
                P_gen_node(i) = -psi_eff_i
                dPgen_dP_node(i) = -dSeff_dP_i
                dPgen_dT_node(i) = -dSeff_dT_i
            else
                P_gen_node(i) = workspace%P_node(i)
                dPgen_dP_node(i) = 1.0d0
                dPgen_dT_node(i) = 0.0d0
            end if
        end do

        ! ----------------------------------------------------------------
        ! 2. Mass Matrix K1 (LHS, factor bdf0).
        !    Uncut element: standard Gauss rule (C_eq/C_HT from step 1).
        !    Cut element: interface-split subcell rule, built in the SAME
        !    q_s loop as the fluxes below (mat_C_sub/mat_C_HT_sub).
        ! ----------------------------------------------------------------
        if (.not. is_cut) then
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
            ! Column j of K2(D_HH) scaled by dP_gen/dp_w.
            if (present(K_HH)) then
                do j = 1, n_nodes
                    do i = 1, n_nodes
                        call K_HH%set(MATRIX_OPS%ADD, i, j, workspace%work_matrix(i, j) * dPgen_dP_node(j))
                    end do
                end do
            end if
            ! K_HT also takes K2(D_HH) scaled by dP_gen/dT, independent of the
            ! thermo-osmotic K2(D_HT) added above.
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
            mat_C_sub(:, :) = 0.0d0
            mat_C_HT_sub(:, :) = 0.0d0
            vec_ddt_sub(:) = 0.0d0

            ! Gather nodal BDF history once (T, P, ice content only -
            ! compute_transient_term_mixed does not read porosity history).
            ! Unset node history contributes zero, exactly as lerp_states
            ! fills state_gp's history (see governing_base.F90).
            n_hist = workspace%bdf_order + 1
            T_hist_node(:, :) = 0.0d0
            P_hist_node(:, :) = 0.0d0
            Qi_hist_node(:, :) = 0.0d0
            do i = 1, n_nodes
                nullify (hist_ptr)
                hist_is_set = .false.
                call workspace%state(i)%temperature_history%get(hist_ptr, is_set=hist_is_set)
                if (hist_is_set .and. associated(hist_ptr)) then
                    hlen = min(size(hist_ptr), n_hist)
                    T_hist_node(1:hlen, i) = hist_ptr(1:hlen)
                end if

                nullify (hist_ptr)
                hist_is_set = .false.
                call workspace%state(i)%pressure_history%get(hist_ptr, is_set=hist_is_set)
                if (hist_is_set .and. associated(hist_ptr)) then
                    hlen = min(size(hist_ptr), n_hist)
                    P_hist_node(1:hlen, i) = hist_ptr(1:hlen)
                end if

                nullify (hist_ptr)
                hist_is_set = .false.
                call workspace%state(i)%ice_content_history%get(hist_ptr, is_set=hist_is_set)
                if (hist_is_set .and. associated(hist_ptr)) then
                    hlen = min(size(hist_ptr), n_hist)
                    Qi_hist_node(1:hlen, i) = hist_ptr(1:hlen)
                end if
            end do

            do q_s = 1, num_subcell_quadrature_points
                r_sub%x = subcell_quadrature_points(q_s)%xi
                r_sub%y = subcell_quadrature_points(q_s)%eta
                r_sub%z = 0.0d0

                call workspace%fe%lerp(r_sub, workspace%T_node(1:n_nodes), T_q_sub)
                call workspace%fe%lerp(r_sub, workspace%P_node(1:n_nodes), P_q_sub)
                call workspace%fe%lerp(r_sub, porosity_nodes(1:n_nodes), porosity_q_sub)
                call workspace%fe%lerp(r_sub, workspace%Qi_node(1:n_nodes), Qi_q_sub)

                do k = 1, n_hist
                    call workspace%fe%lerp(r_sub, T_hist_node(k, 1:n_nodes), T_hist_sub(k))
                    call workspace%fe%lerp(r_sub, P_hist_node(k, 1:n_nodes), P_hist_sub(k))
                    call workspace%fe%lerp(r_sub, Qi_hist_node(k, 1:n_nodes), Qi_hist_sub(k))
                end do

                call state_sub%copy(workspace%state(1))
                call state_sub%temperature%set(T_q_sub)
                call state_sub%pressure%set(P_q_sub)
                call state_sub%porosity%set(porosity_q_sub)
                call state_sub%ice_content%set(Qi_q_sub)
                call state_sub%temperature_history%set(T_hist_sub(1:n_hist))
                call state_sub%pressure_history%set(P_hist_sub(1:n_hist))
                call state_sub%ice_content_history%set(Qi_hist_sub(1:n_hist))
                call self%update_water_phases(workspace%material_id, state_sub)

                psi_sub(:) = 0.0d0
                dpsi_dx_sub(:, :) = 0.0d0
                call workspace%fe%calc_shape_function(r_sub, workspace%coordinates, psi=psi_sub, &
                                                      dpsi_dx=dpsi_dx_sub, determinant_jacobian=det_J_sub)

                eff_weight_sub = subcell_quadrature_points(q_s)%weight * abs(det_J_sub)

                D_HH_sub = D_HH_elem
                D_HT_sub = D_HT_elem
                V_sub(:) = V_elem_mag * V_dir(:)

                ! Storage term (Step 3): C_eq = dTheta/dp at the subcell point,
                ! same pseudo-transient regularization as the Gauss-rule path,
                ! plus its residual counterpart d(Theta)/dt.
                call self%compute_iteration_capacity(workspace%material_id, state_sub, C_eq_sub)
                C_eq_sub = C_eq_sub + capacity_regularization
                C_HT_storage_sub = 0.0d0
                if (coupling_mass_needed) then
                    call self%compute_coupling_mass_term(workspace%material_id, state_sub, C_HT_storage_sub)
                end if
                call self%compute_transient_term_mixed(workspace%material_id, state_sub, &
                                                       workspace%bdf_coeffs, d_dt_sub)

                do j = 1, n_nodes
                    do i = 1, n_nodes
                        mat_HH_sub(i, j) = mat_HH_sub(i, j) + eff_weight_sub * D_HH_sub * &
                                           dot_product(dpsi_dx_sub(:, i), dpsi_dx_sub(:, j))
                        mat_HT_sub(i, j) = mat_HT_sub(i, j) + eff_weight_sub * D_HT_sub * &
                                           dot_product(dpsi_dx_sub(:, i), dpsi_dx_sub(:, j))
                        mat_C_sub(i, j) = mat_C_sub(i, j) + eff_weight_sub * C_eq_sub * psi_sub(i) * psi_sub(j)
                        mat_C_HT_sub(i, j) = mat_C_HT_sub(i, j) + &
                                             eff_weight_sub * C_HT_storage_sub * psi_sub(i) * psi_sub(j)
                    end do
                end do
                do i = 1, n_nodes
                    vec_V_sub(i) = vec_V_sub(i) + eff_weight_sub * &
                                   dot_product(dpsi_dx_sub(:, i), V_sub)
                    vec_ddt_sub(i) = vec_ddt_sub(i) + eff_weight_sub * d_dt_sub * psi_sub(i)
                end do
            end do

            if (present(K_HH)) then
                do j = 1, n_nodes
                    do i = 1, n_nodes
                        call K_HH%set(MATRIX_OPS%ADD, i, j, &
                                      mat_HH_sub(i, j) * dPgen_dP_node(j) + bdf0 * mat_C_sub(i, j))
                    end do
                end do
            end if
            if (present(K_HT) .and. thermal_target) then
                do j = 1, n_nodes
                    do i = 1, n_nodes
                        call K_HT%set(MATRIX_OPS%ADD, i, j, &
                                      mat_HT_sub(i, j) + mat_HH_sub(i, j) * dPgen_dT_node(j) + &
                                      bdf0 * mat_C_HT_sub(i, j))
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
            if (is_cut) then
                local_vec_res(:) = local_vec_res(:) + vec_ddt_sub(:)
            else
                workspace%work_vec(:) = 0.0d0
                call workspace%compute_R1(workspace%work_d_dt, workspace%work_vec)
                local_vec_res(:) = local_vec_res(:) + workspace%work_vec(:)
            end if

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

    !> Geometric mean of non-negative nodal values.
    !>
    !> Returns zero if any node is zero: a single impermeable node blocks the
    !> element, which is the physically correct series behaviour.
    pure function geometric_mean(values) result(mean_value)
        implicit none
        real(real64), intent(in) :: values(:)
        real(real64) :: mean_value

        integer(int32) :: i

        mean_value = 0.0d0
        if (size(values) == 0) return
        do i = 1, size(values)
            if (values(i) <= 0.0d0) return
        end do
        mean_value = exp(sum(log(values)) / real(size(values), real64))
    end function geometric_mean

    !> Geometric mean of values that share a sign; zero on a sign change.
    !>
    !> The thermo-osmotic coefficient is negative wherever the total-potential
    !> head is negative, so it is averaged on its magnitude and the common sign
    !> is restored. A sign change inside one element has no geometric mean and
    !> is treated as no coupling there rather than as an arbitrary branch.
    pure function signed_geometric_mean(values) result(mean_value)
        implicit none
        real(real64), intent(in) :: values(:)
        real(real64) :: mean_value

        integer(int32) :: i
        logical :: all_positive, all_negative

        mean_value = 0.0d0
        if (size(values) == 0) return
        all_positive = all(values > 0.0d0)
        all_negative = all(values < 0.0d0)
        if (.not. (all_positive .or. all_negative)) return
        mean_value = exp(sum(log(abs(values))) / real(size(values), real64))
        if (all_negative) mean_value = -mean_value
    end function signed_geometric_mean

    !> TEMPORARY Step-0 verification diagnostic; see interface doc comment.
    module subroutine report_subcell_debug_counts_hydraulic(self)
        implicit none
        class(type_hydraulic), intent(in) :: self

        if (.not. DEBUG_SUBCELL_COUNTER) return
        write (*, '(A,I0,A,I0,A,ES12.4,A,ES12.4,A,ES12.4,A)') '   [SUBCELL-DEBUG] elements cut = ', dbg_elements_cut, &
            ' / considered = ', dbg_elements_considered, ' | phi_min=', dbg_phi_min, ' phi_max=', dbg_phi_max, &
            ' min|phi|=', dbg_abs_phi_min, ' (this assembly)'
        dbg_elements_considered = 0
        dbg_elements_cut = 0
        dbg_phi_min = huge(1.0d0)
        dbg_phi_max = -huge(1.0d0)
        dbg_abs_phi_min = huge(1.0d0)
    end subroutine report_subcell_debug_counts_hydraulic

end submodule hydraulic_matrix

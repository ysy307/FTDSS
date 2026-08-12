submodule(physics_governing_thermal) thermal_matrix
    ! g (gravitational acceleration) is needed only by calc_subcell_fluxes
    ! below; the parent module (physics_governing_thermal) does not import it.
    use :: module_constitutive, only:g => gravity_acceleration
    implicit none

    !> Integrate the thermal element on the interface-split subcell rule when
    !> the element is cut.
    !>
    !> Separate from the runtime enable_fringe_subcell_quadrature switch, which
    !> gates the shared level set and therefore the HYDRAULIC block too. This
    !> parameter isolates the thermal block alone, so a run with the hydraulic
    !> treatment held fixed measures what the thermal cut rule by itself does:
    !> without it the only available comparison also changes the hydraulic
    !> fluxes, and the two effects cannot be separated.
    logical, parameter :: THERMAL_SUBCELL = .true.
contains

    module subroutine assemble_local_thermal(self, control, workspace, K_TT, K_TH, F_T)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_control), intent(in) :: control
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: K_TT
        type(type_matrix_dense), intent(inout), optional :: K_TH
        type(type_vector_dp), intent(inout), optional :: F_T

        call self%assemble_local_picard(control, workspace, K_TT, K_TH, F_T)

    end subroutine assemble_local_thermal

    !> @brief Assemble Picard local components (backward Euler, no BDF history)
    !>
    !> The Modified Picard matrix freezes the phase derivatives at the current
    !> iterate and couples both primary increments. The K_TH block is the
    !> pressure derivative of the current enthalpy term.
    !>
    !> The freezing front is thinner than one element (element ~0.6 mm vs. a
    !> ~65 um phase-change band at the measured surface gradient), and the
    !> apparent heat capacity carrying the latent heat swings from ~2e6 to
    !> ~2.8e8 J/(m3 K) across it. Sampling that swing at Gauss points makes the
    !> integrated latent heat jump discretely as the front sweeps between
    !> them, which is what stalled the nonlinear solver at the freezing onset
    !> (locT = 1.8-4.9 while locH ~ 1e-4). Elements cut by the interface
    !> (workspace%is_cut, shared with HYDRAULIC via
    !> workspace%compute_interface_subcell - see governing_base.F90 and
    !> hydraulic_matrix.F90) therefore replace the standard Gauss rule with
    !> the interface-split subcell rule for EVERY term that carries the
    !> front: mass/apparent-capacity, coupling mass, diffusion, advection, the
    !> transient (enthalpy-rate) residual and the segregation source. The
    !> residual and the matrix always use the SAME rule for a given element.
    module subroutine assemble_local_picard_thermal(self, control, workspace, K_TT, K_TH, F_T)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_control), intent(in) :: control
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: K_TT
        type(type_matrix_dense), intent(inout), optional :: K_TH
        type(type_vector_dp), intent(inout), optional :: F_T

        integer(int32) :: i, j
        integer(int32) :: ierr
        real(real64) :: val_T, bdf0, dt_local
        real(real64), pointer :: K_TT_val(:, :)
        real(real64), pointer :: K_TH_val(:, :)
        real(real64), pointer :: F_T_val(:)

        ! Automatic (stack) arrays — bounds taken from the dummy workspace at entry,
        ! so no per-element heap allocate/deallocate in the assembly hot path.
        real(real64) :: local_vec_diff_flux(workspace%num_fe_nodes)
        real(real64) :: local_vec_adv_flux(workspace%num_fe_nodes)
        real(real64) :: work_C_TH(workspace%num_fe_gauss)
        real(real64) :: work_Q_seg(workspace%num_fe_gauss)
        real(real64) :: S_seg, Lf, rho_w, grad_T_mag
        type(type_coordinate_dp), pointer :: grad_T_ptr
        integer(int32) :: n_nodes
        logical :: has_advection, hydraulic_target

        ! --- Interface-split subcell variables (see the cut branch below) ---
        logical :: is_cut
        integer(int32) :: num_subcell_quadrature_points, q_s, k, n_hist, hlen, d1, d2
        real(real64) :: T_q_sub, P_q_sub, porosity_q_sub, Qi_q_sub
        real(real64) :: eff_weight_sub, det_J_sub
        real(real64) :: psi_sub(workspace%num_fe_nodes)
        real(real64) :: dpsi_dx_sub(workspace%num_fe_dimension, workspace%num_fe_nodes)
        type(type_coordinate_dp) :: r_sub, grad_T_sub, grad_P_sub, grad_Pl_sub
        type(type_coordinate_dp) :: water_flux_sub, vapor_flux_sub
        type(type_state) :: state_sub
        real(real64) :: C_TT_sub, C_TH_sub, dU_dt_sub, Q_seg_sub
        real(real64) :: D_TT_sub(workspace%num_fe_dimension, workspace%num_fe_dimension)
        real(real64) :: V_TT_sub(workspace%num_fe_dimension)
        real(real64) :: M_grad_j(workspace%num_fe_dimension)
        real(real64) :: mat_TT_mass_sub(workspace%num_fe_nodes, workspace%num_fe_nodes)
        real(real64) :: mat_TH_mass_sub(workspace%num_fe_nodes, workspace%num_fe_nodes)
        real(real64) :: mat_TT_diff_sub(workspace%num_fe_nodes, workspace%num_fe_nodes)
        real(real64) :: mat_TT_adv_sub(workspace%num_fe_nodes, workspace%num_fe_nodes)
        real(real64) :: vec_seg_sub(workspace%num_fe_nodes)
        real(real64) :: vec_ddt_sub(workspace%num_fe_nodes)
        real(real64) :: local_vec_diff_flux_sub(workspace%num_fe_nodes)
        real(real64) :: local_vec_adv_flux_sub(workspace%num_fe_nodes)
        real(real64) :: T_hist_node(workspace%bdf_order + 1, workspace%num_fe_nodes)
        real(real64) :: P_hist_node(workspace%bdf_order + 1, workspace%num_fe_nodes)
        real(real64) :: porosity_hist_node(workspace%bdf_order + 1, workspace%num_fe_nodes)
        real(real64) :: Qi_hist_node(workspace%bdf_order + 1, workspace%num_fe_nodes)
        real(real64) :: T_hist_sub(workspace%bdf_order + 1)
        real(real64) :: P_hist_sub(workspace%bdf_order + 1)
        real(real64) :: porosity_hist_sub(workspace%bdf_order + 1)
        real(real64) :: Qi_hist_sub(workspace%bdf_order + 1)
        real(real64), pointer, contiguous, dimension(:) :: hist_ptr
        logical :: hist_is_set
        real(real64) :: grad_T_mag_sub, S_seg_sub, Lf_sub, rho_w_sub

        n_nodes = workspace%num_fe_nodes

        workspace%work_C(:) = 0.0d0
        workspace%work_D(:, :, :) = 0.0d0
        workspace%work_d_dt(:) = 0.0d0
        workspace%work_V(:, :) = 0.0d0
        work_C_TH(:) = 0.0d0

        local_vec_diff_flux(:) = 0.0d0
        local_vec_adv_flux(:) = 0.0d0

        has_advection = .false.
        hydraulic_target = control%is_target(PHYSICS_TYPES%HYDRAULIC, workspace%material_id)

        bdf0 = workspace%bdf_coeffs(1)
        dt_local = 0.0d0
        call control%get_dt(dt_local)

        nullify (K_TT_val)
        nullify (K_TH_val)
        nullify (F_T_val)
        nullify (grad_T_ptr)

        if (present(K_TT)) K_TT_val => K_TT%get_val()
        if (present(K_TH)) K_TH_val => K_TH%get_val()
        if (present(F_T)) F_T_val => F_T%get_data()

        work_Q_seg(:) = 0.0d0

        ! ----------------------------------------------------------------
        ! 0. Cut detection, shared with HYDRAULIC. workspace%compute_
        !    interface_subcell computes the level set and the interface-split
        !    subcell quadrature ONCE per element and caches the result on the
        !    workspace; whichever governing block (THERMAL or HYDRAULIC) runs
        !    first on this element populates it, the other reuses it, so the
        !    two blocks can never disagree on where the interface is. See
        !    governing_base.F90 for the shared method and
        !    hydraulic_matrix.F90 for the mirrored call.
        ! ----------------------------------------------------------------
        call workspace%compute_interface_subcell(self%physics, workspace%material_id, &
                                                  self%enable_fringe_subcell_quadrature)
        ! The shared level set is still computed above (HYDRAULIC consumes it);
        ! only this block's use of it is gated here.
        is_cut = workspace%is_cut .and. THERMAL_SUBCELL
        num_subcell_quadrature_points = workspace%num_subcell_quadrature_points

        if (.not. is_cut) then

            ! Gauss Loop
            do i = 1, workspace%num_fe_gauss
                ! Chord, not tangent, for the apparent heat capacity.
                !
                ! dU/dT at the iterate is the exact derivative there and the wrong
                ! number for a step that crosses the freezing point: the capacity
                ! rises by two to three orders of magnitude within 0.1 K of T_crit,
                ! so a step sized on the unfrozen side overshoots by that factor,
                ! and re-linearizing on the frozen side then makes the next step
                ! that much too small. Measured, one such step froze 401 nodes at
                ! once and the iterate stopped moving for 27 iterations. The chord
                ! [U(T^m) - U(T^n)]/(T^m - T^n) averages the capacity over the
                ! excursion the step actually makes, which is the quantity the step
                ! needs. The residual is assembled in enthalpy form, so this is an
                ! iteration matrix only: the converged solution is unchanged.
                call self%compute_mass_term(workspace%material_id, workspace%state_gp(i), workspace%work_C(i), &
                                            scheme_opt=SCHEME_SECANT)
                if (hydraulic_target .and. associated(K_TH_val)) then
                    call self%compute_coupling_mass_term(workspace%material_id, workspace%state_gp(i), work_C_TH(i))
                end if

                call self%compute_diffusion_term(workspace%material_id, workspace%state_gp(i), workspace%work_D(:, :, i))

                call self%compute_advective_term(workspace%material_id, workspace%state_gp(i), &
                                                 workspace%work_V(:, i))
                if (any(abs(workspace%work_V(:, i)) > 1.0d-30)) has_advection = .true.

                call self%compute_transient_term(workspace%material_id, workspace%state_gp(i), &
                                                 workspace%bdf_coeffs, workspace%work_d_dt(i))

                ! Segregation latent heat is paired with the hydraulic water sink.
                if (hydraulic_target) then
                    nullify (grad_T_ptr)
                    call workspace%state_gp(i)%grad_T%get(grad_T_ptr)
                    if (associated(grad_T_ptr)) then
                        grad_T_mag = sqrt(grad_T_ptr%x**2 + grad_T_ptr%y**2 + grad_T_ptr%z**2)
                        if (grad_T_mag > 0.0d0) then
                            S_seg = 0.0d0
                            call self%physics%calc_effective_segregation_sink( &
                                workspace%material_id, workspace%state_gp(i), grad_T_mag, dt_local, S_seg)
                            if (S_seg > 0.0d0) then
                                Lf = 0.0d0
                                rho_w = 0.0d0
                                call self%physics%calc_latent_heat_fusion( &
                                    workspace%material_id, workspace%state_gp(i), Lf)
                                call self%calc_density_water(workspace%state_gp(i), rho_w)
                                work_Q_seg(i) = Lf * rho_w * S_seg
                            end if
                        end if
                    end if
                end if
            end do

            ! Mass matrix (LHS, factor bdf0), consistent - this is the EXACT
            ! derivative of the residual's transient term.
            !
            ! The residual integrates the Gauss-point enthalpy rate consistently
            ! (compute_R1 below), so the discrete equations being solved are the
            ! consistent-mass ones and the converged solution is whatever zeroes
            ! that residual. A lumped K_TT is therefore only an iteration matrix:
            ! it cannot enforce a discrete maximum principle the residual does not
            ! contain, so the previous DMP justification for lumping here does not
            ! hold - lumping changed no solution, only the convergence rate.
            !
            ! And it changed it badly where it matters. d/dT_j of
            ! int psi_i * c0 * U(T) dOmega is c0 * int psi_i C_a psi_j dOmega; the
            ! row-sum lump diag(int psi_i C_a dOmega) approximates that well only
            ! when C_a is smooth over the element. At the freezing front C_a is
            ! dominated by -Lf*rho_i*dtheta_i/dT and varies by orders of magnitude
            ! between adjacent nodes, so the lumped tangent drives the Picard
            ! contraction factor toward one: measured, the thermal residual stalled
            ! at 11 percent of its step-initial value for all 30 iterations while
            ! the hydraulic block contracted at 0.82 per iteration.
            call workspace%compute_K1(workspace%work_C, workspace%work_matrix)
            if (associated(K_TT_val)) then
                K_TT_val(1:n_nodes, 1:n_nodes) = &
                    K_TT_val(1:n_nodes, 1:n_nodes) + &
                    bdf0 * workspace%work_matrix(1:n_nodes, 1:n_nodes)
            end if

            if (hydraulic_target .and. associated(K_TH_val)) then
                call workspace%compute_K1(work_C_TH, workspace%work_matrix)
                K_TH_val(1:n_nodes, 1:n_nodes) = &
                    K_TH_val(1:n_nodes, 1:n_nodes) + &
                    bdf0 * workspace%work_matrix(1:n_nodes, 1:n_nodes)
            end if

            ! Diffusion matrix (LHS, factor 1.0) + diffusion flux for F_T
            call workspace%compute_K2(workspace%work_D, workspace%work_matrix)
            if (associated(K_TT_val)) then
                K_TT_val(1:n_nodes, 1:n_nodes) = &
                    K_TT_val(1:n_nodes, 1:n_nodes) + &
                    workspace%work_matrix(1:n_nodes, 1:n_nodes)
            end if
            call matvec(workspace%work_matrix, workspace%T_node, local_vec_diff_flux, ierr)

            ! Advection matrix (LHS, factor 1.0) + advection flux for F_T
            if (has_advection) then
                call workspace%compute_K3(workspace%work_V, workspace%work_matrix)
                if (associated(K_TT_val)) then
                    K_TT_val(1:n_nodes, 1:n_nodes) = &
                        K_TT_val(1:n_nodes, 1:n_nodes) + &
                        workspace%work_matrix(1:n_nodes, 1:n_nodes)
                end if
                call matvec(workspace%work_matrix, workspace%T_node, local_vec_adv_flux, ierr)
            end if

            ! Residual vector
            if (associated(F_T_val)) then
                workspace%work_vec(:) = 0.0d0
                call workspace%compute_R1(work_Q_seg, workspace%work_vec)

                do i = 1, n_nodes
                    val_T = -local_vec_diff_flux(i) - local_vec_adv_flux(i) + workspace%work_vec(i)
                    F_T_val(i) = F_T_val(i) + val_T
                end do

                workspace%work_vec(:) = 0.0d0
                call workspace%compute_R1(workspace%work_d_dt, workspace%work_vec)
                do i = 1, n_nodes
                    F_T_val(i) = F_T_val(i) - workspace%work_vec(i)
                end do
            end if

        else
            ! ------------------------------------------------------------
            ! Cut element: interface-split subcell rule REPLACES the
            ! standard Gauss rule for every term above, so nothing is
            ! double-counted. Coefficients are evaluated from a state built
            ! at each subcell point by interpolating the nodal T/P/porosity/
            ! ice-content AND their full BDF history with the parent shape
            ! functions (fe%lerp) - mirroring governing_base.F90's
            ! lerp_states - so compute_transient_term reads a history
            ! resolved at the SAME sub-element location as the current
            ! value. A Gauss-point-only history would pair "today at the
            ! interface" with "yesterday at the Gauss point", which is not
            ! the same material point and would corrupt the BDF rate.
            ! ------------------------------------------------------------
            mat_TT_mass_sub(:, :) = 0.0d0
            mat_TH_mass_sub(:, :) = 0.0d0
            mat_TT_diff_sub(:, :) = 0.0d0
            mat_TT_adv_sub(:, :) = 0.0d0
            vec_seg_sub(:) = 0.0d0
            vec_ddt_sub(:) = 0.0d0

            ! Gather nodal BDF history once per element (T, P, porosity, ice
            ! content). Unset node history contributes zero, exactly as
            ! lerp_states fills state_gp's history for a node that never had
            ! history set (see governing_base.F90).
            n_hist = workspace%bdf_order + 1
            T_hist_node(:, :) = 0.0d0
            P_hist_node(:, :) = 0.0d0
            porosity_hist_node(:, :) = 0.0d0
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
                call workspace%state(i)%porosity_history%get(hist_ptr, is_set=hist_is_set)
                if (hist_is_set .and. associated(hist_ptr)) then
                    hlen = min(size(hist_ptr), n_hist)
                    porosity_hist_node(1:hlen, i) = hist_ptr(1:hlen)
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
                r_sub%x = workspace%subcell_quadrature_points(q_s)%xi
                r_sub%y = workspace%subcell_quadrature_points(q_s)%eta
                r_sub%z = 0.0d0

                call workspace%fe%lerp(r_sub, workspace%T_node(1:n_nodes), T_q_sub)
                call workspace%fe%lerp(r_sub, workspace%P_node(1:n_nodes), P_q_sub)
                call workspace%fe%lerp(r_sub, workspace%phi_node(1:n_nodes), porosity_q_sub)
                call workspace%fe%lerp(r_sub, workspace%Qi_node(1:n_nodes), Qi_q_sub)

                call workspace%fe%dlerp(r_sub, workspace%T_node(1:n_nodes), workspace%coordinates, &
                                       workspace%computation_type, grad_T_sub)
                call workspace%fe%dlerp(r_sub, workspace%P_node(1:n_nodes), workspace%coordinates, &
                                       workspace%computation_type, grad_P_sub)
                call workspace%fe%dlerp(r_sub, workspace%Pl_node(1:n_nodes), workspace%coordinates, &
                                       workspace%computation_type, grad_Pl_sub)

                do k = 1, n_hist
                    call workspace%fe%lerp(r_sub, T_hist_node(k, 1:n_nodes), T_hist_sub(k))
                    call workspace%fe%lerp(r_sub, P_hist_node(k, 1:n_nodes), P_hist_sub(k))
                    call workspace%fe%lerp(r_sub, porosity_hist_node(k, 1:n_nodes), porosity_hist_sub(k))
                    call workspace%fe%lerp(r_sub, Qi_hist_node(k, 1:n_nodes), Qi_hist_sub(k))
                end do

                call state_sub%copy(workspace%state(1))
                call state_sub%temperature%set(T_q_sub)
                call state_sub%pressure%set(P_q_sub)
                call state_sub%porosity%set(porosity_q_sub)
                call state_sub%ice_content%set(Qi_q_sub)
                call state_sub%temperature_history%set(T_hist_sub(1:n_hist))
                call state_sub%pressure_history%set(P_hist_sub(1:n_hist))
                call state_sub%porosity_history%set(porosity_hist_sub(1:n_hist))
                call state_sub%ice_content_history%set(Qi_hist_sub(1:n_hist))
                call state_sub%grad_T%set(grad_T_sub)
                call state_sub%grad_P%set(grad_P_sub)
                call state_sub%grad_Pl%set(grad_Pl_sub)
                call self%update_water_phases(workspace%material_id, state_sub)

                ! Water/vapor Darcy flux at the subcell point, needed by the
                ! advective term below. See calc_subcell_fluxes doc comment
                ! for why this cannot simply reuse
                ! calc_water_flux_ftcms/calc_vapor_flux_ftcms.
                if (hydraulic_target) then
                    call calc_subcell_fluxes(self, workspace%material_id, state_sub, grad_T_sub, grad_P_sub, &
                                             water_flux_sub, vapor_flux_sub)
                else
                    call water_flux_sub%set(0.0d0, 0.0d0, 0.0d0)
                    call vapor_flux_sub%set(0.0d0, 0.0d0, 0.0d0)
                end if
                call state_sub%set(water_flux=water_flux_sub, vapor_flux=vapor_flux_sub)

                psi_sub(:) = 0.0d0
                dpsi_dx_sub(:, :) = 0.0d0
                call workspace%fe%calc_shape_function(r_sub, workspace%coordinates, psi=psi_sub, &
                                                      dpsi_dx=dpsi_dx_sub, determinant_jacobian=det_J_sub)
                eff_weight_sub = workspace%subcell_quadrature_points(q_s)%weight * abs(det_J_sub)

                call self%compute_mass_term(workspace%material_id, state_sub, C_TT_sub, scheme_opt=SCHEME_SECANT)

                C_TH_sub = 0.0d0
                if (hydraulic_target .and. associated(K_TH_val)) then
                    call self%compute_coupling_mass_term(workspace%material_id, state_sub, C_TH_sub)
                end if

                D_TT_sub(:, :) = 0.0d0
                call self%compute_diffusion_term(workspace%material_id, state_sub, D_TT_sub)

                V_TT_sub(:) = 0.0d0
                call self%compute_advective_term(workspace%material_id, state_sub, V_TT_sub)

                dU_dt_sub = 0.0d0
                call self%compute_transient_term(workspace%material_id, state_sub, workspace%bdf_coeffs, dU_dt_sub)

                ! Segregation latent heat, paired with the hydraulic water
                ! sink exactly as in the uncut Gauss loop above.
                Q_seg_sub = 0.0d0
                if (hydraulic_target) then
                    grad_T_mag_sub = sqrt(grad_T_sub%x**2 + grad_T_sub%y**2 + grad_T_sub%z**2)
                    if (grad_T_mag_sub > 0.0d0) then
                        S_seg_sub = 0.0d0
                        call self%physics%calc_effective_segregation_sink( &
                            workspace%material_id, state_sub, grad_T_mag_sub, dt_local, S_seg_sub)
                        if (S_seg_sub > 0.0d0) then
                            Lf_sub = 0.0d0
                            rho_w_sub = 0.0d0
                            call self%physics%calc_latent_heat_fusion(workspace%material_id, state_sub, Lf_sub)
                            call self%calc_density_water(state_sub, rho_w_sub)
                            Q_seg_sub = Lf_sub * rho_w_sub * S_seg_sub
                        end if
                    end if
                end if

                ! Manual K1/K2/K3/R1 accumulation (fe%compute_K1/K2/K3/R1
                ! integrate over the ELEMENT's standard Gauss rule; here the
                ! subcell rule replaces it, so the same weighted sums are
                ! built directly from psi_sub/dpsi_dx_sub - see
                ! fe_compute.F90 for the matching standard-rule formulas).
                do j = 1, n_nodes
                    M_grad_j(:) = 0.0d0
                    do d2 = 1, workspace%num_fe_dimension
                        do d1 = 1, workspace%num_fe_dimension
                            M_grad_j(d1) = M_grad_j(d1) + D_TT_sub(d1, d2) * dpsi_dx_sub(d2, j)
                        end do
                    end do
                    do i = 1, n_nodes
                        mat_TT_mass_sub(i, j) = mat_TT_mass_sub(i, j) + &
                                                eff_weight_sub * C_TT_sub * psi_sub(i) * psi_sub(j)
                        mat_TH_mass_sub(i, j) = mat_TH_mass_sub(i, j) + &
                                                eff_weight_sub * C_TH_sub * psi_sub(i) * psi_sub(j)
                        mat_TT_diff_sub(i, j) = mat_TT_diff_sub(i, j) + &
                                                eff_weight_sub * dot_product(dpsi_dx_sub(:, i), M_grad_j)
                        mat_TT_adv_sub(i, j) = mat_TT_adv_sub(i, j) + &
                                               eff_weight_sub * psi_sub(i) * dot_product(V_TT_sub, dpsi_dx_sub(:, j))
                    end do
                end do
                do i = 1, n_nodes
                    vec_seg_sub(i) = vec_seg_sub(i) + eff_weight_sub * Q_seg_sub * psi_sub(i)
                    vec_ddt_sub(i) = vec_ddt_sub(i) + eff_weight_sub * dU_dt_sub * psi_sub(i)
                end do
            end do

            if (associated(K_TT_val)) then
                K_TT_val(1:n_nodes, 1:n_nodes) = K_TT_val(1:n_nodes, 1:n_nodes) + &
                                                 bdf0 * mat_TT_mass_sub(1:n_nodes, 1:n_nodes) + &
                                                 mat_TT_diff_sub(1:n_nodes, 1:n_nodes) + &
                                                 mat_TT_adv_sub(1:n_nodes, 1:n_nodes)
            end if
            if (hydraulic_target .and. associated(K_TH_val)) then
                K_TH_val(1:n_nodes, 1:n_nodes) = K_TH_val(1:n_nodes, 1:n_nodes) + &
                                                 bdf0 * mat_TH_mass_sub(1:n_nodes, 1:n_nodes)
            end if

            if (associated(F_T_val)) then
                local_vec_diff_flux_sub(:) = 0.0d0
                call matvec(mat_TT_diff_sub, workspace%T_node, local_vec_diff_flux_sub, ierr)
                local_vec_adv_flux_sub(:) = 0.0d0
                call matvec(mat_TT_adv_sub, workspace%T_node, local_vec_adv_flux_sub, ierr)

                do i = 1, n_nodes
                    F_T_val(i) = F_T_val(i) - local_vec_diff_flux_sub(i) - local_vec_adv_flux_sub(i) + &
                                 vec_seg_sub(i) - vec_ddt_sub(i)
                end do
            end if
        end if

    end subroutine assemble_local_picard_thermal

    !> Liquid water and vapor Darcy flux vectors at a state, needed by the
    !> thermal advective term (compute_advective_term reads state%water_flux
    !> / state%vapor_flux). At Gauss points these are precomputed at the app
    !> level (type_ftcms%update_physical_properties, ftcms_base.F90) because
    !> the formula combines a thermal coefficient (density) with hydraulic
    !> ones (K_lT/Kflh/KvT/Kvh); type_thermal has no reference to
    !> type_hydraulic to repeat that call for a subcell point that does not
    !> exist at the app level.
    !>
    !> Duplicates calc_water_flux_ftcms / calc_vapor_flux_ftcms
    !> (src/app/ftcms/ftcms_compute.F90): the underlying coefficients are NOT
    !> duplicated (both call the SAME self%physics%calc_KlT/calc_Kflh/
    !> calc_KvT/calc_Kvh on the constitutive manager every governing type
    !> privately holds), only the small assembly-into-a-vector formula
    !> (sign convention, gravity term, 2D/3D select) is. Must be kept in sync
    !> with those two routines if that formula changes.
    subroutine calc_subcell_fluxes(self, material_id, state, grad_T, grad_P, water_flux, vapor_flux)
        implicit none
        class(type_thermal), intent(in) :: self
        integer(int32), intent(in) :: material_id
        ! target: get_field_coord returns a pointer into state.
        type(type_state), intent(in), target :: state
        type(type_coordinate_dp), intent(in) :: grad_T, grad_P
        type(type_coordinate_dp), intent(inout) :: water_flux, vapor_flux

        real(real64) :: K_wT, K_wP_raw, K_wP, K_vT, K_vP_raw, K_vP
        real(real64) :: rho_w, gravity_term
        type(type_coordinate_dp), pointer :: grad_Pl_ptr
        logical :: is_set
        type(type_coordinate_dp) :: grad_Pgen

        call self%physics%calc_KlT(material_id, state, K_wT)
        call self%physics%calc_Kflh(material_id, state, K_wP_raw)
        call self%calc_density_water(state, rho_w)
        K_wP = merge(K_wP_raw / (rho_w * g), 0.0d0, rho_w > tiny(1.0d0))
        gravity_term = K_wP_raw

        nullify (grad_Pl_ptr)
        call water_flux%set(0.0d0, 0.0d0, 0.0d0)
        call vapor_flux%set(0.0d0, 0.0d0, 0.0d0)

        ! Matches driver_gradient in ftcms_compute.F90; vapour keeps grad p_w.
        grad_Pgen = grad_P
        if (HYDRAULIC_DRIVER_TOTAL_POTENTIAL) then
            is_set = .false.
            call state%grad_Pl%get(grad_Pl_ptr, is_set)
            if (is_set .and. associated(grad_Pl_ptr)) grad_Pgen = grad_Pl_ptr
        end if

        select case (self%computation_type)
        case (COMP_TYPES%XY_2D%ID)
            water_flux%x = -K_wT * grad_T%x - K_wP * grad_Pgen%x
            water_flux%y = -K_wT * grad_T%y - K_wP * grad_Pgen%y
        case (COMP_TYPES%XZ_2D%ID)
            water_flux%x = -K_wT * grad_T%x - K_wP * grad_Pgen%x
            water_flux%z = -K_wT * grad_T%z - K_wP * grad_Pgen%z - gravity_term
        case (COMP_TYPES%XYZ_3D%ID)
            water_flux%x = -K_wT * grad_T%x - K_wP * grad_Pgen%x
            water_flux%y = -K_wT * grad_T%y - K_wP * grad_Pgen%y
            water_flux%z = -K_wT * grad_T%z - K_wP * grad_Pgen%z - gravity_term
        end select

        if (self%enable_vapor_transport) then
            call self%physics%calc_KvT(material_id, state, K_vT)
            call self%physics%calc_Kvh(material_id, state, K_vP_raw)
            K_vP = merge(K_vP_raw / (rho_w * g), 0.0d0, rho_w > tiny(1.0d0))
            select case (self%computation_type)
            case (COMP_TYPES%XY_2D%ID)
                vapor_flux%x = -K_vT * grad_T%x - K_vP * grad_P%x
                vapor_flux%y = -K_vT * grad_T%y - K_vP * grad_P%y
            case (COMP_TYPES%XZ_2D%ID)
                vapor_flux%x = -K_vT * grad_T%x - K_vP * grad_P%x
                vapor_flux%z = -K_vT * grad_T%z - K_vP * grad_P%z
            case (COMP_TYPES%XYZ_3D%ID)
                vapor_flux%x = -K_vT * grad_T%x - K_vP * grad_P%x
                vapor_flux%y = -K_vT * grad_T%y - K_vP * grad_P%y
                vapor_flux%z = -K_vT * grad_T%z - K_vP * grad_P%z
            end select
        end if

    end subroutine calc_subcell_fluxes

end submodule thermal_matrix

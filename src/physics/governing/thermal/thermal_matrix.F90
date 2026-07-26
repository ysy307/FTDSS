submodule(physics_governing_thermal) thermal_matrix
    implicit none
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

        ! Gauss Loop
        do i = 1, workspace%num_fe_gauss
            call self%compute_mass_term(workspace%material_id, workspace%state_gp(i), workspace%work_C(i))
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

    end subroutine assemble_local_picard_thermal

end submodule thermal_matrix

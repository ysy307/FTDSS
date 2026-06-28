submodule(physics_governing_hydraulic) hydraulic_matrix
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
        real(real64), parameter :: C_min = 1.0d-12
        real(real64), parameter :: K_min = 1.0d-12

        ! Automatic (stack) arrays — bounds from the dummy workspace at entry,
        ! so no per-element heap allocate/deallocate in the assembly hot path.
        real(real64) :: local_vec_res(workspace%num_fe_nodes)
        real(real64) :: work_sink(workspace%num_fe_gauss)
        real(real64) :: work_C_HT(workspace%num_fe_gauss)
        real(real64) :: work_D_HT(workspace%num_fe_dimension, workspace%num_fe_dimension, workspace%num_fe_gauss)
        real(real64) :: work_matrix_coupling(workspace%num_fe_nodes, workspace%num_fe_nodes)
        real(real64) :: C_HT_nodes(workspace%num_fe_nodes)
        real(real64) :: D_HT_scalar_nodes(workspace%num_fe_nodes)
        real(real64) :: D_HT_tmp(workspace%num_fe_dimension, workspace%num_fe_dimension)
        type(type_coordinate_dp), pointer, contiguous, dimension(:) :: gp_coords
        real(real64) :: C_HT_gp, D_HT_gp_scalar

        n_nodes = workspace%num_fe_nodes
        n_gauss = workspace%num_fe_gauss
        n_dim = workspace%num_fe_dimension

        bdf0 = workspace%bdf_coeffs(1)
        dt_local = 0.0d0
        call control%get_dt(dt_local)

        workspace%work_C(:) = 0.0d0
        workspace%work_D(:, :, :) = 0.0d0
        workspace%work_V(:, :) = 0.0d0
        workspace%work_d_dt(:) = 0.0d0
        local_vec_res(:) = 0.0d0
        work_sink(:) = 0.0d0

        ! Coupling workspace
        nullify (gp_coords)
        if (present(K_HT)) then
            work_C_HT(:) = 0.0d0
            work_D_HT(:, :, :) = 0.0d0
            work_matrix_coupling(:, :) = 0.0d0
            call workspace%fe%get_gauss(gp_coords)
            do i = 1, n_nodes
                call self%compute_coupling_mass_term(workspace%material_id, workspace%state(i), C_HT_nodes(i))
                D_HT_tmp(:, :) = 0.0d0
                call self%compute_coupling_diffusion_term(workspace%material_id, workspace%state(i), D_HT_tmp)
                D_HT_scalar_nodes(i) = D_HT_tmp(1, 1)
            end do
            ! D_HT = K_flh/g * |dpsi_cryo/dT| is the cryosuction-driven moisture
            ! migration coupling: it must be active throughout the frozen fringe
            ! (0 > T > ~T_fringe, where liquid still flows) for water to migrate to
            ! the freezing front (otherwise the result is unphysical in-situ
            ! freezing). It self-limits in fully-frozen soil because the impedance
            ! collapses K_flh -> 0. The pressure-block near-singularity that a former
            ! transition-element-only guard worked around is now handled directly by
            ! the no-flow storage pin in compute_mass_term_hydraulic.
        end if

        ! 1. Gauss Loop
        do i = 1, n_gauss
            call self%compute_mass_term(workspace%material_id, workspace%state_gp(i), workspace%work_C(i))
            workspace%work_C(i) = max(workspace%work_C(i), C_min)
            call self%compute_diffusion_term(workspace%material_id, workspace%state_gp(i), workspace%work_D(:, :, i))
            do d = 1, n_dim
                workspace%work_D(d, d, i) = max(workspace%work_D(d, d, i), K_min)
            end do
            call self%compute_advective_term(workspace%material_id, workspace%state_gp(i), workspace%work_V(:, i))
            call self%compute_transient_term_mixed(workspace%material_id, workspace%state_gp(i), &
                                                   workspace%bdf_coeffs, workspace%work_d_dt(i))

            if (present(K_HT)) then
                call workspace%fe%lerp(gp_coords(i), C_HT_nodes(1:n_nodes), C_HT_gp)
                work_C_HT(i) = C_HT_gp
                D_HT_gp_scalar = 0.0d0
                call workspace%fe%lerp(gp_coords(i), D_HT_scalar_nodes(1:n_nodes), D_HT_gp_scalar)
                do d = 1, n_dim
                    work_D_HT(d, d, i) = D_HT_gp_scalar
                end do
            end if

            call self%calc_segregation_sink(workspace%material_id, workspace%state_gp(i), dt_local, work_sink(i))
        end do

        ! 2. Mass Matrix (LHS, factor bdf0)
        call workspace%compute_K1(workspace%work_C, workspace%work_matrix)
        if (present(K_HH)) then
            do j = 1, n_nodes
                do i = 1, n_nodes
                    call K_HH%set(MATRIX_OPS%ADD, i, j, bdf0 * workspace%work_matrix(i, j))
                end do
            end do
        end if

        ! 3. Coupling K_HT mass part (K1, factor bdf0, lerped C_HT)
        if (present(K_HT)) then
            call workspace%compute_K1(work_C_HT, work_matrix_coupling)
            do j = 1, n_nodes
                do i = 1, n_nodes
                    call K_HT%set(MATRIX_OPS%ADD, i, j, bdf0 * work_matrix_coupling(i, j))
                end do
            end do
        end if

        ! 4. Coupling K_HT diffusion part (K2 D_HT) + F_H coupling flux
        if (present(K_HT)) then
            call workspace%compute_K2(work_D_HT, work_matrix_coupling)
            do j = 1, n_nodes
                do i = 1, n_nodes
                    call K_HT%set(MATRIX_OPS%ADD, i, j, work_matrix_coupling(i, j))
                end do
            end do
            if (present(F_H)) then
                workspace%work_vec(:) = 0.0d0
                call matvec(work_matrix_coupling, workspace%T_node, workspace%work_vec, ierr)
                do i = 1, n_nodes
                    local_vec_res(i) = local_vec_res(i) + workspace%work_vec(i)
                end do
            end if
        end if

        ! 5. Diffusion Matrix (LHS, factor 1.0) + F_H diffusion flux
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

        ! 5. Residual Assembly
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

        if (associated(gp_coords)) nullify (gp_coords)

    end subroutine assemble_local_picard_hydraulic

    module subroutine assemble_element_hydraulic(self, material_id, bdf_coeffs, dt, workspace, J_elem, R_elem)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        real(real64), intent(in) :: bdf_coeffs(:)
        real(real64), intent(in) :: dt
        type(type_assemble_workspace), intent(inout) :: workspace
        real(real64), intent(inout) :: J_elem(:, :)
        real(real64), intent(inout) :: R_elem(:)

        integer(int32) :: gp, i, j, d
        integer(int32) :: n_nodes, n_gauss, n_dim
        real(real64) :: bdf0, wJ, detJ, Ceq, dTheta_dt
        real(real64), parameter :: K_min = 1.0d-12
        real(real64), parameter :: Ceq_min = 1.0d-12
        type(type_coordinate_dp), pointer, contiguous, dimension(:) :: gauss_pts
        real(real64), pointer, contiguous, dimension(:) :: weights

        ! Automatic (stack) arrays — bounds from the dummy workspace at entry.
        real(real64) :: D_HH(workspace%num_fe_dimension, workspace%num_fe_dimension)
        real(real64) :: D_HT(workspace%num_fe_dimension, workspace%num_fe_dimension)
        real(real64) :: V_H(workspace%num_fe_dimension)
        real(real64) :: grad_P(workspace%num_fe_dimension)
        real(real64) :: grad_T(workspace%num_fe_dimension)
        real(real64) :: flux(workspace%num_fe_dimension)
        real(real64) :: S_seg

        nullify (gauss_pts)
        nullify (weights)

        n_nodes = workspace%num_fe_nodes
        n_gauss = workspace%num_fe_gauss
        n_dim = workspace%num_fe_dimension
        bdf0 = bdf_coeffs(1)

        J_elem = 0.0d0
        R_elem = 0.0d0

        call workspace%fe%get_gauss(gauss_pts)
        call workspace%fe%get_weight(weights)

        do gp = 1, n_gauss
            workspace%work_psi(:) = 0.0d0
            workspace%work_dpsi_dx(:, :) = 0.0d0
            detJ = 0.0d0
            call workspace%fe%calc_shape_function(gauss_pts(gp), workspace%coordinates, &
                                                  psi=workspace%work_psi, &
                                                  dpsi_dx=workspace%work_dpsi_dx, &
                                                  determinant_jacobian=detJ)
            wJ = weights(gp) * abs(detJ)

            D_HH(:, :) = 0.0d0
            D_HT(:, :) = 0.0d0
            V_H(:) = 0.0d0
            Ceq = 0.0d0
            dTheta_dt = 0.0d0

            call self%compute_mass_term(material_id, workspace%state_gp(gp), Ceq)
            call self%compute_diffusion_term(material_id, workspace%state_gp(gp), D_HH)
            call self%compute_coupling_diffusion_term(material_id, workspace%state_gp(gp), D_HT)
            call self%compute_advective_term(material_id, workspace%state_gp(gp), V_H)
            call self%compute_transient_term_mixed(material_id, workspace%state_gp(gp), bdf_coeffs, dTheta_dt)

            if (abs(Ceq) > 1.0d120) then
                write (*, '(A,I0,A,I0,A,ES13.5)') 'Error: Hydraulic C_eq exploded. mat=', &
                    material_id, ', gp=', gp, ', Ceq=', Ceq
                error stop 'assemble_element_hydraulic: C_eq overflow.'
            end if
            if (abs(dTheta_dt) > 1.0d120) then
                write (*, '(A,I0,A,I0,A,ES13.5)') 'Error: Hydraulic dTheta/dt exploded. mat=', &
                    material_id, ', gp=', gp, ', dTdt=', dTheta_dt
                error stop 'assemble_element_hydraulic: dTheta_dt overflow.'
            end if

            ! Min cutoff: prevent zero diagonal in near-frozen state
            Ceq = max(Ceq, Ceq_min)
            do d = 1, n_dim
                D_HH(d, d) = max(D_HH(d, d), K_min)
            end do

            grad_P = matmul(workspace%work_dpsi_dx, workspace%P_node)
            grad_T = matmul(workspace%work_dpsi_dx, workspace%T_node)
            flux = matmul(D_HH, grad_P) + matmul(D_HT, grad_T) + V_H

            do i = 1, n_nodes
                R_elem(i) = R_elem(i) + wJ * (workspace%work_psi(i) * dTheta_dt + &
                                               dot_product(workspace%work_dpsi_dx(:, i), flux))
                J_elem(i, i) = J_elem(i, i) + wJ * workspace%work_psi(i) * bdf0 * Ceq
                do j = 1, n_nodes
                    J_elem(i, j) = J_elem(i, j) + wJ * &
                                   dot_product(workspace%work_dpsi_dx(:, i), &
                                               matmul(D_HH, workspace%work_dpsi_dx(:, j)))
                end do
            end do

            S_seg = 0.0d0
            call self%calc_segregation_sink(material_id, workspace%state_gp(gp), dt, S_seg)
            if (abs(S_seg) > 0.0d0) then
                do i = 1, n_nodes
                    R_elem(i) = R_elem(i) + wJ * workspace%work_psi(i) * S_seg
                end do
            end if
        end do

        nullify (gauss_pts)
        nullify (weights)

    end subroutine assemble_element_hydraulic

end submodule hydraulic_matrix

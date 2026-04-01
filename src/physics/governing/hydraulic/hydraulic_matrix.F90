submodule(physics_governing_hydraulic) hydraulic_matrix
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none
contains

    !> @brief Assemble Local Matrix and Vector (Wrapper)
    module subroutine assemble_local_hydraulic(self, control, workspace, K_HH, K_HT, F_H)
        implicit none
        class(type_hydraulic), intent(in) :: self
        type(type_control), intent(in) :: control
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: K_HH
        type(type_matrix_dense), intent(inout), optional :: K_HT
        type(type_vector_dp), intent(inout), optional :: F_H

        if (control%is_compute_newton()) then
            call self%assemble_local_newton(control, workspace, K_HH, K_HT, F_H)
        else if (control%is_compute_picard()) then
            call self%assemble_local_picard(control, workspace, K_HH, K_HT, F_H)
        end if

    end subroutine assemble_local_hydraulic

    !> @brief Assemble Newton-Raphson Local Components
    module subroutine assemble_local_newton_hydraulic(self, control, workspace, K_HH, K_HT, F_H)
        implicit none
        class(type_hydraulic), intent(in) :: self
        type(type_control), intent(in) :: control
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: K_HH
        type(type_matrix_dense), intent(inout), optional :: K_HT
        type(type_vector_dp), intent(inout), optional :: F_H

        integer(int32) :: i, j
        integer(int32) :: n_nodes, n_gauss, n_dim
        real(real64) :: bdf0

        real(real64), allocatable :: J_local(:, :)
        real(real64), allocatable :: R_local(:)

        real(real64), allocatable :: work_C_HT(:)
        real(real64), allocatable :: work_D_HT(:, :, :)
        real(real64), allocatable :: work_matrix_coupling(:, :)

        n_nodes = workspace%num_fe_nodes
        n_gauss = workspace%num_fe_gauss
        n_dim = workspace%num_fe_dimension
        bdf0 = workspace%bdf_coeffs(1)

        ! 1. J_HH + R_H: assemble_element (explicit Gauss loop, min cutoff, row equilibration)
        allocate (J_local(n_nodes, n_nodes), R_local(n_nodes))
        call self%assemble_element(workspace%material_id, &
                                   workspace%bdf_coeffs(1:workspace%bdf_order + 1), &
                                   workspace, J_local, R_local)

        if (present(K_HH)) then
            do j = 1, n_nodes
                do i = 1, n_nodes
                    call K_HH%set(MATRIX_OPS%ADD, i, j, J_local(i, j))
                end do
            end do
        end if

        if (present(F_H)) then
            do i = 1, n_nodes
                call F_H%set(VECTOR_OPS%ADD, i, -R_local(i))
            end do
        end if

        deallocate (J_local, R_local)

        ! 2. K_HT: coupling Jacobian (separate Gauss loop, unchanged)
        if (present(K_HT)) then
            allocate (work_C_HT(n_gauss))
            allocate (work_D_HT(n_dim, n_dim, n_gauss))
            allocate (work_matrix_coupling(n_nodes, n_nodes))
            work_C_HT(:) = 0.0d0
            work_D_HT(:, :, :) = 0.0d0
            work_matrix_coupling(:, :) = 0.0d0

            do i = 1, n_gauss
                call self%compute_coupling_mass_term(workspace%material_id, workspace%state_gp(i), work_C_HT(i))
                call self%compute_coupling_diffusion_term(workspace%material_id, workspace%state_gp(i), work_D_HT(:, :, i))
            end do

            ! C_HT mass coupling -> K_HT
            call workspace%compute_K1(work_C_HT, work_matrix_coupling)
            do j = 1, n_nodes
                do i = 1, n_nodes
                    call K_HT%set(MATRIX_OPS%ADD, i, j, bdf0 * work_matrix_coupling(i, j))
                end do
            end do

            ! D_HT diffusion coupling -> K_HT
            call workspace%compute_K2(work_D_HT, work_matrix_coupling)
            do j = 1, n_nodes
                do i = 1, n_nodes
                    call K_HT%set(MATRIX_OPS%ADD, i, j, work_matrix_coupling(i, j))
                end do
            end do

        end if

        if (allocated(work_C_HT)) deallocate (work_C_HT)
        if (allocated(work_D_HT)) deallocate (work_D_HT)
        if (allocated(work_matrix_coupling)) deallocate (work_matrix_coupling)

    end subroutine assemble_local_newton_hydraulic

    !> @brief Assemble Picard Local Components
    module subroutine assemble_local_picard_hydraulic(self, control, workspace, K_HH, K_HT, F_H)
        implicit none
        class(type_hydraulic), intent(in) :: self
        type(type_control), intent(in) :: control
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: K_HH
        type(type_matrix_dense), intent(inout), optional :: K_HT
        type(type_vector_dp), intent(inout), optional :: F_H

        integer(int32) :: i, j, d, n_nodes, n_gauss, n_dim, ierr
        real(real64) :: bdf0
        real(real64), allocatable :: local_vec_res(:)

        real(real64), allocatable :: work_C_HT(:)
        real(real64), allocatable :: work_D_HT(:, :, :)
        real(real64), allocatable :: work_matrix_coupling(:, :)

        n_nodes = workspace%num_fe_nodes
        n_gauss = workspace%num_fe_gauss
        n_dim = workspace%num_fe_dimension
        allocate (local_vec_res(n_nodes))
        bdf0 = workspace%bdf_coeffs(1)

        workspace%work_C(:) = 0.0d0
        workspace%work_D(:, :, :) = 0.0d0
        workspace%work_V(:, :) = 0.0d0
        workspace%work_d_dt(:) = 0.0d0
        local_vec_res(:) = 0.0d0

        ! Coupling workspace
        if (present(K_HT)) then
            allocate (work_C_HT(n_gauss))
            allocate (work_D_HT(n_dim, n_dim, n_gauss))
            allocate (work_matrix_coupling(n_nodes, n_nodes))
            work_C_HT(:) = 0.0d0
            work_D_HT(:, :, :) = 0.0d0
            work_matrix_coupling(:, :) = 0.0d0
        end if

        ! 1. Gauss Loop
        do i = 1, n_gauss
            call self%compute_mass_term(workspace%material_id, workspace%state_gp(i), workspace%work_C(i))
            call self%compute_diffusion_term(workspace%material_id, workspace%state_gp(i), workspace%work_D(:, :, i))
            ! Min cutoffs: prevent near-zero diagonals in near-frozen/dry state
            workspace%work_C(i) = max(workspace%work_C(i), 1.0d-20)
            do d = 1, n_dim
                workspace%work_D(d, d, i) = max(workspace%work_D(d, d, i), 1.0d-20)
            end do
            call self%compute_advective_term(workspace%material_id, workspace%state_gp(i), workspace%work_V(:, i))
            call self%compute_transient_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%bdf_coeffs(1:workspace%bdf_order + 1), &
                                             workspace%work_d_dt(i))

            if (present(K_HT)) then
                call self%compute_coupling_mass_term(workspace%material_id, workspace%state_gp(i), work_C_HT(i))
                call self%compute_coupling_diffusion_term(workspace%material_id, workspace%state_gp(i), work_D_HT(:, :, i))
            end if
        end do

        ! 2. Mass Matrix (LHS)
        call workspace%compute_K1(workspace%work_C, workspace%work_matrix)
        if (present(K_HH)) then
            do j = 1, n_nodes
                do i = 1, n_nodes
                    call K_HH%set(MATRIX_OPS%ADD, i, j, bdf0 * workspace%work_matrix(i, j))
                end do
            end do
        end if

        ! 3. Diffusion Matrix (LHS) & Flux Calculation
        call workspace%compute_K2(workspace%work_D, workspace%work_matrix)
        if (present(K_HH)) then
            do j = 1, n_nodes
                do i = 1, n_nodes
                    call K_HH%set(MATRIX_OPS%ADD, i, j, workspace%work_matrix(i, j))
                end do
            end do
        end if

        ! Calculate Diffusion Flux (Current K * Current P)
        if (present(F_H)) then
            do i = 1, n_nodes
                do j = 1, n_nodes
                    local_vec_res(i) = local_vec_res(i) + workspace%work_matrix(i, j) * workspace%P_node(j)
                end do
            end do
        end if

        ! 4. Coupling: K_HT assembly (temperature coupling)
        if (present(K_HT)) then
            ! C_HT mass coupling -> K_HT
            call workspace%compute_K1(work_C_HT, work_matrix_coupling)
            do j = 1, n_nodes
                do i = 1, n_nodes
                    call K_HT%set(MATRIX_OPS%ADD, i, j, bdf0 * work_matrix_coupling(i, j))
                end do
            end do

            ! D_HT diffusion coupling -> K_HT
            call workspace%compute_K2(work_D_HT, work_matrix_coupling)
            do j = 1, n_nodes
                do i = 1, n_nodes
                    call K_HT%set(MATRIX_OPS%ADD, i, j, work_matrix_coupling(i, j))
                end do
            end do

            ! Coupling flux contribution to F_H: -D_HT_matrix * T_node
            if (present(F_H)) then
                workspace%work_vec(:) = 0.0d0
                call matvec(work_matrix_coupling, workspace%T_node, workspace%work_vec, ierr)
                do i = 1, n_nodes
                    local_vec_res(i) = local_vec_res(i) + workspace%work_vec(i)
                end do
            end if
        end if

        ! 5. Residual Assembly
        if (present(F_H)) then
            ! Add Transient Term
            workspace%work_vec(:) = 0.0d0
            call workspace%compute_R1(workspace%work_d_dt, workspace%work_vec)
            local_vec_res(:) = local_vec_res(:) + workspace%work_vec(:)

            ! Add Gravity Term
            workspace%work_vec(:) = 0.0d0
            call workspace%compute_R2(workspace%work_V, workspace%work_vec)
            local_vec_res(:) = local_vec_res(:) + workspace%work_vec(:)

            ! F = - Residual
            do i = 1, n_nodes
                call F_H%set(VECTOR_OPS%ADD, i, -local_vec_res(i))
            end do
        end if

        if (allocated(local_vec_res)) deallocate (local_vec_res)
        if (allocated(work_C_HT)) deallocate (work_C_HT)
        if (allocated(work_D_HT)) deallocate (work_D_HT)
        if (allocated(work_matrix_coupling)) deallocate (work_matrix_coupling)

    end subroutine assemble_local_picard_hydraulic

    module subroutine assemble_element_hydraulic(self, material_id, bdf_coeffs, workspace, J_elem, R_elem)
        implicit none
        class(type_hydraulic), intent(in) :: self
        integer(int32), intent(in) :: material_id
        real(real64), intent(in) :: bdf_coeffs(:)
        type(type_assemble_workspace), intent(inout) :: workspace
        real(real64), intent(inout) :: J_elem(:, :)
        real(real64), intent(inout) :: R_elem(:)

        integer(int32) :: gp, i, j, d
        integer(int32) :: n_nodes, n_gauss, n_dim
        real(real64) :: bdf0, wJ, detJ, Ceq, dTheta_dt
        real(real64), parameter :: K_min = 1.0d-20
        real(real64), parameter :: Ceq_min = 1.0d-20

        type(type_coordinate_dp), pointer, contiguous, dimension(:) :: gauss_pts
        real(real64), pointer, contiguous, dimension(:) :: weights

        real(real64), allocatable :: D_HH(:, :), D_HT(:, :), V_H(:)
        real(real64), allocatable :: grad_P(:), grad_T(:), flux(:)
        real(real64), allocatable :: row_max(:)

        nullify (gauss_pts)
        nullify (weights)

        n_nodes = workspace%num_fe_nodes
        n_gauss = workspace%num_fe_gauss
        n_dim = workspace%num_fe_dimension
        bdf0 = bdf_coeffs(1)

        J_elem = 0.0d0
        R_elem = 0.0d0

        allocate (D_HH(n_dim, n_dim), D_HT(n_dim, n_dim), V_H(n_dim))
        allocate (grad_P(n_dim), grad_T(n_dim), flux(n_dim))
        allocate (row_max(n_nodes))

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

            call self%compute_C_eq(material_id, workspace%state_gp(gp), Ceq)
            call self%compute_diffusion_term(material_id, workspace%state_gp(gp), D_HH)
            call self%compute_coupling_diffusion_term(material_id, workspace%state_gp(gp), D_HT)
            call self%compute_advective_term(material_id, workspace%state_gp(gp), V_H)
            call self%compute_transient_term_mixed(material_id, workspace%state_gp(gp), bdf_coeffs, dTheta_dt)

            if (.not. ieee_is_finite(Ceq) .or. abs(Ceq) > 1.0d120) then
                write (*, '(A,I0,A,I0,A,ES13.5)') 'Error: Hydraulic C_eq exploded. mat=', &
                    material_id, ', gp=', gp, ', Ceq=', Ceq
                error stop 'assemble_element_hydraulic: C_eq overflow.'
            end if
            if (.not. ieee_is_finite(dTheta_dt) .or. abs(dTheta_dt) > 1.0d120) then
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
        end do

        ! Row equilibration: scale each row by its max absolute value
        row_max = max(maxval(abs(J_elem), dim=2), tiny(1.0d0))
        J_elem = J_elem / spread(row_max, 2, n_nodes)
        R_elem = R_elem / row_max

        nullify (gauss_pts)
        nullify (weights)
        deallocate (D_HH, D_HT, V_H, grad_P, grad_T, flux, row_max)

    end subroutine assemble_element_hydraulic

end submodule hydraulic_matrix

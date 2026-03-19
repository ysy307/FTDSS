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

        integer(int32) :: i, j, ierr
        integer(int32) :: n_nodes, n_gauss, n_dim
        real(real64) :: bdf0

        real(real64), allocatable :: work_C_HT(:)
        real(real64), allocatable :: work_D_HT(:, :, :)
        real(real64), allocatable :: work_matrix_coupling(:, :)

        n_nodes = workspace%num_fe_nodes
        n_gauss = workspace%num_fe_gauss
        n_dim = workspace%num_fe_dimension
        bdf0 = workspace%bdf_coeffs(1)

        ! Initialize Workspaces
        workspace%work_C(:) = 0.0d0
        workspace%work_D(:, :, :) = 0.0d0
        workspace%work_V(:, :) = 0.0d0
        workspace%work_d_dt(:) = 0.0d0

        ! Coupling workspace
        if (present(K_HT)) then
            allocate (work_C_HT(n_gauss))
            allocate (work_D_HT(n_dim, n_dim, n_gauss))
            allocate (work_matrix_coupling(n_nodes, n_nodes))
            work_C_HT(:) = 0.0d0
            work_D_HT(:, :, :) = 0.0d0
            work_matrix_coupling(:, :) = 0.0d0
        end if

        ! 1. Gauss Point Loop: Compute Physics Terms
        do i = 1, n_gauss
            call self%compute_mass_term(workspace%material_id, workspace%state_gp(i), &
                                        workspace%work_C(i))

            call self%compute_diffusion_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%work_D(:, :, i))

            call self%compute_advective_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%work_V(:, i))

            call self%compute_transient_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%bdf_coeffs(1:workspace%bdf_order + 1), &
                                             workspace%work_d_dt(i))

            if (present(K_HT)) then
                call self%compute_coupling_mass_term(workspace%material_id, workspace%state_gp(i), work_C_HT(i))
                call self%compute_coupling_diffusion_term(workspace%material_id, workspace%state_gp(i), work_D_HT(:, :, i))
            end if

            if (.not. ieee_is_finite(workspace%work_C(i)) .or. abs(workspace%work_C(i)) > 1.0d120) then
                write (*, '(A,I0,A,I0,A,ES13.5)') 'Error: Hydraulic mass term exploded. mat=', &
                    workspace%material_id, ', gp=', i, ', C=', workspace%work_C(i)
                error stop 'Hydraulic mass term overflow in local assembly.'
            end if

            if (.not. ieee_is_finite(workspace%work_d_dt(i)) .or. abs(workspace%work_d_dt(i)) > 1.0d120) then
                write (*, '(A,I0,A,I0,A,ES13.5)') 'Error: Hydraulic transient term exploded. mat=', &
                    workspace%material_id, ', gp=', i, ', dUdt=', workspace%work_d_dt(i)
                error stop 'Hydraulic transient overflow in local assembly.'
            end if

            if (any(.not. ieee_is_finite(workspace%work_D(:, :, i))) .or. any(abs(workspace%work_D(:, :, i)) > 1.0d120)) then
                write (*, '(A,I0,A,I0)') 'Error: Hydraulic diffusion tensor exploded. mat=', workspace%material_id, ', gp=', i
                error stop 'Hydraulic diffusion overflow in local assembly.'
            end if

            if (any(.not. ieee_is_finite(workspace%work_V(:, i))) .or. any(abs(workspace%work_V(:, i)) > 1.0d120)) then
                write (*, '(A,I0,A,I0)') 'Error: Hydraulic advection term exploded. mat=', workspace%material_id, ', gp=', i
                error stop 'Hydraulic advection overflow in local assembly.'
            end if
        end do

        ! 2. Mass Matrix Contribution (Accumulate to K_HH)
        call workspace%compute_K1(workspace%work_C, workspace%work_matrix)
        if (present(K_HH)) then
            do j = 1, n_nodes
                do i = 1, n_nodes
                    call K_HH%set(MATRIX_OPS%ADD, i, j, bdf0 * workspace%work_matrix(i, j))
                end do
            end do
        end if

        ! 3. Transient Residual Contribution (Accumulate to F_H)
        if (present(F_H)) then
            workspace%work_vec(:) = 0.0d0
            call workspace%compute_R1(workspace%work_d_dt, workspace%work_vec)
            do i = 1, n_nodes
                call F_H%set(VECTOR_OPS%ADD, i, -workspace%work_vec(i))
            end do
        end if

        ! 4. Diffusion Stiffness Contribution (Accumulate to K_HH)
        call workspace%compute_K2(workspace%work_D, workspace%work_matrix)
        if (present(K_HH)) then
            do j = 1, n_nodes
                do i = 1, n_nodes
                    call K_HH%set(MATRIX_OPS%ADD, i, j, workspace%work_matrix(i, j))
                end do
            end do
        end if

        ! 5. Diffusion Internal Force Contribution (Accumulate to F_H)
        if (present(F_H)) then
            workspace%work_vec(:) = 0.0d0
            call matvec(workspace%work_matrix, workspace%P_node, workspace%work_vec, ierr)
            do i = 1, n_nodes
                call F_H%set(VECTOR_OPS%ADD, i, -workspace%work_vec(i))
            end do
        end if

        ! 6. Gravity/Advection Flux Contribution (Accumulate to F_H)
        if (present(F_H)) then
            workspace%work_vec(:) = 0.0d0
            call workspace%compute_R2(workspace%work_V, workspace%work_vec)
            do i = 1, n_nodes
                call F_H%set(VECTOR_OPS%ADD, i, -workspace%work_vec(i))
            end do
        end if

        ! 7. Coupling: K_HT assembly (temperature coupling)
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
                    call F_H%set(VECTOR_OPS%ADD, i, -workspace%work_vec(i))
                end do
            end if
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

        integer(int32) :: i, j, n_nodes, n_gauss, n_dim, ierr
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

end submodule hydraulic_matrix

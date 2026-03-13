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
        real(real64) :: bdf0, c_abs_max, c_abs_min_nz, d_abs_max, d_abs_min_nz
        logical :: has_c_nz, has_d_nz
        logical, save :: debug_coeff_once = .false.

        bdf0 = workspace%bdf_coeffs(1)

        ! Initialize Workspaces
        workspace%work_C(:) = 0.0d0 ! Mass Term (C_HH)
        workspace%work_D(:, :, :) = 0.0d0 ! Diffusion Term (D_HH)
        workspace%work_V(:, :) = 0.0d0 ! Advective/Gravity Term (V_H)
        workspace%work_d_dt(:) = 0.0d0 ! Transient Term (drho/dt)

        ! 1. Gauss Point Loop: Compute Physics Terms
        do i = 1, workspace%num_fe_gauss
            ! (A) Mass Term: C_HH
            call self%compute_mass_term(workspace%material_id, workspace%state_gp(i), &
                                        workspace%work_C(i))

            ! (B) Diffusion Term: D_HH
            call self%compute_diffusion_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%work_D(:, :, i))

            ! (C) Advective/Gravity Term: V_H
            call self%compute_advective_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%work_V(:, i))

            ! (D) Transient Term: drho/dt
            call self%compute_transient_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%bdf_coeffs(1:workspace%bdf_order + 1), &
                                             workspace%work_d_dt(i))

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

        if (.not. debug_coeff_once) then
            has_c_nz = any(abs(workspace%work_C(:)) > 0.0d0)
            has_d_nz = any(abs(workspace%work_D(:, :, :)) > 0.0d0)

            c_abs_max = maxval(abs(workspace%work_C(:)))
            d_abs_max = maxval(abs(workspace%work_D(:, :, :)))
            c_abs_min_nz = 0.0d0
            d_abs_min_nz = 0.0d0
            if (has_c_nz) c_abs_min_nz = minval(abs(workspace%work_C(:)), mask=abs(workspace%work_C(:)) > 0.0d0)
            if (has_d_nz) d_abs_min_nz = minval(abs(workspace%work_D(:, :, :)), mask=abs(workspace%work_D(:, :, :)) > 0.0d0)

            write (*, '(A,I0,A,ES13.5,A,ES13.5,A,ES13.5,A,ES13.5)') &
                '   [DEBUG] Hydraulic coeff scale: mat=', workspace%material_id, &
                ', |C|_max=', c_abs_max, ', |C|_min_nz=', c_abs_min_nz, &
                ', |D|_max=', d_abs_max, ', |D|_min_nz=', d_abs_min_nz
            debug_coeff_once = .true.
        end if

        ! 2. Mass Matrix Contribution (Accumulate to K_HH)
        ! K += bdf0 * MassMatrix
        call workspace%compute_K1(workspace%work_C, workspace%work_matrix)
        if (present(K_HH)) then
            do j = 1, workspace%num_fe_nodes
                do i = 1, workspace%num_fe_nodes
                    call K_HH%set(MATRIX_OPS%ADD, i, j, bdf0 * workspace%work_matrix(i, j))
                end do
            end do
        end if

        ! 3. Transient Residual Contribution (Accumulate to F_H)
        ! F += - Integral(N^T * drho/dt)
        if (present(F_H)) then
            workspace%work_vec(:) = 0.0d0
            call workspace%compute_R1(workspace%work_d_dt, workspace%work_vec)
            do i = 1, workspace%num_fe_nodes
                call F_H%set(VECTOR_OPS%ADD, i, -workspace%work_vec(i))
            end do
        end if

        ! 4. Diffusion Stiffness Contribution (Accumulate to K_HH)
        ! K += Integral(gradN^T * D * gradN)
        call workspace%compute_K2(workspace%work_D, workspace%work_matrix)
        if (present(K_HH)) then
            do j = 1, workspace%num_fe_nodes
                do i = 1, workspace%num_fe_nodes
                    call K_HH%set(MATRIX_OPS%ADD, i, j, workspace%work_matrix(i, j))
                end do
            end do
        end if

        ! 5. Diffusion Internal Force Contribution (Accumulate to F_H)
        ! F += - K_diffusion * P_node
        if (present(F_H)) then
            workspace%work_vec(:) = 0.0d0
            call matvec(workspace%work_matrix, workspace%P_node, workspace%work_vec, ierr)
            do i = 1, workspace%num_fe_nodes
                call F_H%set(VECTOR_OPS%ADD, i, -workspace%work_vec(i))
            end do
        end if

        ! 6. Gravity/Advection Flux Contribution (Accumulate to F_H)
        ! F += Integral(gradN^T * V_H)
        ! Note: Weak form term for flux J is -Integral(gradPsi * J).
        ! J_grav = V_H. So term is -Integral(gradPsi * V_H).
        ! compute_R2 computes Integral(gradPsi * V).
        ! So we subtract the result.
        if (present(F_H)) then
            workspace%work_vec(:) = 0.0d0
            call workspace%compute_R2(workspace%work_V, workspace%work_vec)
            do i = 1, workspace%num_fe_nodes
                call F_H%set(VECTOR_OPS%ADD, i, -workspace%work_vec(i))
            end do
        end if

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

        integer(int32) :: i, j, n_nodes
        real(real64) :: bdf0, c_abs_max, c_abs_min_nz, d_abs_max, d_abs_min_nz
        logical :: has_c_nz, has_d_nz
        logical, save :: debug_coeff_once_picard = .false.
        real(real64), allocatable :: local_vec_res(:)

        n_nodes = workspace%num_fe_nodes
        allocate (local_vec_res(n_nodes))
        bdf0 = workspace%bdf_coeffs(1)

        workspace%work_C(:) = 0.0d0
        workspace%work_D(:, :, :) = 0.0d0
        workspace%work_V(:, :) = 0.0d0
        workspace%work_d_dt(:) = 0.0d0
        local_vec_res(:) = 0.0d0

        ! 1. Gauss Loop
        do i = 1, workspace%num_fe_gauss
            call self%compute_mass_term(workspace%material_id, workspace%state_gp(i), workspace%work_C(i))
            call self%compute_diffusion_term(workspace%material_id, workspace%state_gp(i), workspace%work_D(:, :, i))
            call self%compute_advective_term(workspace%material_id, workspace%state_gp(i), workspace%work_V(:, i))
            call self%compute_transient_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%bdf_coeffs(1:workspace%bdf_order + 1), &
                                             workspace%work_d_dt(i))
        end do

        if (.not. debug_coeff_once_picard) then
            has_c_nz = any(abs(workspace%work_C(:)) > 0.0d0)
            has_d_nz = any(abs(workspace%work_D(:, :, :)) > 0.0d0)

            c_abs_max = maxval(abs(workspace%work_C(:)))
            d_abs_max = maxval(abs(workspace%work_D(:, :, :)))
            c_abs_min_nz = 0.0d0
            d_abs_min_nz = 0.0d0
            if (has_c_nz) c_abs_min_nz = minval(abs(workspace%work_C(:)), mask=abs(workspace%work_C(:)) > 0.0d0)
            if (has_d_nz) d_abs_min_nz = minval(abs(workspace%work_D(:, :, :)), mask=abs(workspace%work_D(:, :, :)) > 0.0d0)

            write (*, '(A,I0,A,ES13.5,A,ES13.5,A,ES13.5,A,ES13.5)') &
                '   [DEBUG] Hydraulic coeff scale(picard): mat=', workspace%material_id, &
                ', |C|_max=', c_abs_max, ', |C|_min_nz=', c_abs_min_nz, &
                ', |D|_max=', d_abs_max, ', |D|_min_nz=', d_abs_min_nz
            debug_coeff_once_picard = .true.
        end if

        ! 2. Mass Matrix (LHS)
        call workspace%compute_K1(workspace%work_C, workspace%work_matrix)
        if (present(K_HH)) then
            do j = 1, workspace%num_fe_nodes
                do i = 1, workspace%num_fe_nodes
                    call K_HH%set(MATRIX_OPS%ADD, i, j, bdf0 * workspace%work_matrix(i, j))
                end do
            end do
        end if

        ! 3. Diffusion Matrix (LHS) & Flux Calculation
        call workspace%compute_K2(workspace%work_D, workspace%work_matrix)
        if (present(K_HH)) then
            do j = 1, workspace%num_fe_nodes
                do i = 1, workspace%num_fe_nodes
                    call K_HH%set(MATRIX_OPS%ADD, i, j, workspace%work_matrix(i, j))
                end do
            end do
        end if

        ! Calculate Diffusion Flux (Current K * Current P)
        if (present(F_H)) then
            do i = 1, workspace%num_fe_nodes
                do j = 1, workspace%num_fe_nodes
                    local_vec_res(i) = local_vec_res(i) + workspace%work_matrix(i, j) * workspace%P_node(j)
                end do
            end do
        end if

        ! 4. Residual Assembly
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
            do i = 1, workspace%num_fe_nodes
                call F_H%set(VECTOR_OPS%ADD, i, -local_vec_res(i))
            end do
        end if

        if (allocated(local_vec_res)) deallocate (local_vec_res)

    end subroutine assemble_local_picard_hydraulic

end submodule hydraulic_matrix

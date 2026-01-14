submodule(main_thermal) thermal_matrix
    implicit none
contains

    module subroutine assemble_local_thermal(self, controls, workspace, J_TT, J_TH, R_T)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_controls), intent(in) :: controls
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: J_TT
        type(type_matrix_dense), intent(inout), optional :: J_TH
        type(type_vector_dp), intent(inout), optional :: R_T

        integer(int32) :: i, j
        real(real64) :: row_sum_val

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
            ! === 変更点: Lumping処理 ===
            ! 行列の各行の値を合計して、対角成分に押し込める
            do i = 1, workspace%num_fe_nodes
                row_sum_val = 0.0d0
                do j = 1, workspace%num_fe_nodes
                    ! 行の和を計算 (Row-Sum)
                    row_sum_val = row_sum_val + workspace%work_matrix(i, j)
                end do

                ! 対角項 (i, i) にのみ、集約した値を加算する
                call J_TT%set(OP_ADD, i, i, workspace%bdf_coeffs(1) * row_sum_val)
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

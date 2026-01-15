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

        integer(int32) :: i, j, d
        real(real64) :: row_sum_val

        ! ローカル変数
        real(real64) :: local_vec(workspace%num_fe_nodes)
        real(real64) :: local_mass_diagonal(workspace%num_fe_nodes)
        ! 【削除】再配分用の変数は不要です
        ! real(real64) :: total_energy_rate_elem, total_mass_elem
        ! real(real64) :: lumped_residual

        ! --- 1. Reset Workspace Arrays ---
        workspace%work_C(:) = 0.0d0
        workspace%work_D(:, :, :) = 0.0d0
        workspace%work_L(:) = 0.0d0
        workspace%work_d_dt(:) = 0.0d0

        local_vec(:) = 0.0d0
        local_mass_diagonal(:) = 0.0d0

        ! --- 2. Evaluate Physical Coefficients at Gauss Points ---
        do i = 1, workspace%num_fe_gauss
            ! Mass Term (Heat Capacity)
            call self%compute_mass_term(workspace%material_id, workspace%state_gp(i), workspace%work_C(i))
            ! Diffusion Term (Thermal Conductivity)
            call self%compute_diffusion_term(workspace%material_id, workspace%state_gp(i), workspace%work_D(:, :, i))
            ! Transient Term (dU/dt)
            call self%compute_transient_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%bdf_coeffs(1:workspace%bdf_order + 1), workspace%work_d_dt(i))
        end do

        ! --- [Hydraulic Coupling] Latent Heat Diffusivity ---
        if (controls%is_target(PHYSICS_TYPE_HYDRAULIC, workspace%material_id)) then
            do i = 1, workspace%num_fe_gauss
                call self%compute_latent_term(workspace%material_id, workspace%state_gp(i), workspace%work_L(i))
                do d = 1, workspace%num_fe_dimension
                    workspace%work_D(d, d, i) = workspace%work_D(d, d, i) + workspace%work_L(i)
                end do
            end do
        end if

        ! ======================================================================
        ! 3. Capacity (Mass) Terms
        ! ======================================================================
        ! (A) Consistent Mass Matrix 計算
        call workspace%compute_K1(workspace%work_C, workspace%work_matrix)

        ! (B) Jacobian用: Mass Lumping (Row-Sum)
        ! 行列の対角化は数値安定性（対角優位性の確保）のために維持します
        do i = 1, workspace%num_fe_nodes
            row_sum_val = 0.0d0
            do j = 1, workspace%num_fe_nodes
                row_sum_val = row_sum_val + workspace%work_matrix(i, j)
            end do
            local_mass_diagonal(i) = row_sum_val
        end do

        ! Jacobianへの加算 (Negative: Solver expects R = F_ext - F_int)
        if (present(J_TT)) then
            do i = 1, workspace%num_fe_nodes
                ! J = - alpha * M_lumped
                call J_TT%set(OP_ADD, i, i, -1.0d0 * workspace%bdf_coeffs(1) * local_mass_diagonal(i))
            end do
        end if

        ! (C) Residualへの加算
        ! 【修正ポイント】再配分ロジックを廃止し、Consistentな積分値（local_vec）をそのまま使用する
        if (present(R_T)) then
            ! Step 1: 通常の積分（Consistent）でエネルギー変化量を計算
            call workspace%compute_R1(workspace%work_d_dt, local_vec)

            do i = 1, workspace%num_fe_nodes
                ! 内部力項（M * dU/dt）を減算 (R = F_ext - F_int)
                call R_T%set(OP_ADD, i, -local_vec(i))
            end do
        end if

        ! ======================================================================
        ! 4. Assemble Diffusion Terms (Stiffness Matrix)
        ! ======================================================================
        ! (A) Stiffness Matrix K
        call workspace%compute_K2(workspace%work_D, workspace%work_matrix)

        ! (B) Jacobian: Negative
        if (present(J_TT)) then
            do i = 1, workspace%num_fe_nodes
                do j = 1, workspace%num_fe_nodes
                    call J_TT%set(OP_ADD, i, j, -workspace%work_matrix(i, j))
                end do
            end do
        end if

        ! (C) Residual: Negative
        if (present(R_T)) then
            workspace%work_vec(:) = 0.0d0
            workspace%work_vec = matmul(workspace%work_matrix, workspace%T_node)

            do i = 1, workspace%num_fe_nodes
                ! 内部力項（K * T）を減算
                call R_T%set(OP_ADD, i, -workspace%work_vec(i))
            end do
        end if

    end subroutine assemble_local_thermal
end submodule thermal_matrix

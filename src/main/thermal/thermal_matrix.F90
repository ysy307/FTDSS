submodule(main_thermal) thermal_matrix
    implicit none
contains

!>
    !> @brief 局所（要素）行列と残差ベクトルのアセンブルを行う
    !>
    module subroutine assemble_local_thermal(self, controls, workspace, K_TT, K_TH, F_T)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_controls), intent(in) :: controls
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: K_TT
        type(type_matrix_dense), intent(inout), optional :: K_TH
        type(type_vector_dp), intent(inout), optional :: F_T

        if (controls%iteration%is_newton()) then
            call self%assemble_local_newton(controls, workspace, K_TT, K_TH, F_T)
        else if (controls%iteration%is_picard()) then
            call self%assemble_local_picard(controls, workspace, K_TT, K_TH, F_T)
        end if

    end subroutine assemble_local_thermal

    ! ==========================================================================
    ! Newton-Raphson Assembly (Tangent Stiffness & Enthalpy Residual)
    ! ==========================================================================
    module subroutine assemble_local_newton_thermal(self, controls, workspace, K_TT, K_TH, F_T)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_controls), intent(in) :: controls
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: K_TT
        type(type_matrix_dense), intent(inout), optional :: K_TH
        type(type_vector_dp), intent(inout), optional :: F_T

            integer(int32) :: i, j, d
            integer(int32) :: ierr
            real(real64) :: local_vec(workspace%num_fe_nodes)
            real(real64) :: flux_vec(workspace%num_fe_dimension)
            real(real64) :: bdf0

            bdf0 = workspace%bdf_coeffs(1)

            ! 1. 積分点ループ (状態量・係数計算)
            workspace%work_C(:) = 0.0d0
            workspace%work_D(:, :, :) = 0.0d0
            workspace%work_d_dt(:) = 0.0d0

            do i = 1, workspace%num_fe_gauss
                ! (A) Mass Term: 接線熱容量 (Tangent Heat Capacity)
                call self%compute_mass_term(workspace%material_id, workspace%state_gp(i), &
                                            workspace%work_C(i), scheme_opt=SCHEME_TANGENT)

                ! (B) Diffusion Term: 熱伝導率
                call self%compute_diffusion_term(workspace%material_id, workspace%state_gp(i), &
                                                 workspace%work_D(:, :, i))

                ! (C) Transient Residual: エンタルピー時間微分 (dU/dt)
                call self%compute_transient_term(workspace%material_id, workspace%state_gp(i), &
                                                 workspace%bdf_coeffs(1:workspace%bdf_order + 1), &
                                                 workspace%work_d_dt(i))
            end do

        !     ! 2. ヤコビアンの構築 (J = alpha * M_tan + K_tan)
        !     if (present(K_TT)) then
        !         ! Mass Matrix (M_tan)
        !         call workspace%compute_K1(workspace%work_C, workspace%work_matrix)
        !         do i = 1, workspace%num_fe_nodes
        !             do j = 1, workspace%num_fe_nodes
        !                 ! J += bdf_coeffs(1) * M
        !                 call K_TT%set(OP_ADD, i, j, bdf0 * workspace%work_matrix(i, j))
        !             end do
        !         end do

        !         ! Stiffness Matrix (K_tan)
        !         call workspace%compute_K2(workspace%work_D, workspace%work_matrix)
        !         do i = 1, workspace%num_fe_nodes
        !             do j = 1, workspace%num_fe_nodes
        !                 ! J += K
        !                 call K_TT%set(OP_ADD, i, j, workspace%work_matrix(i, j))
        !             end do
        !         end do
        !     end if

        !     ! 3. 残差ベクトルの構築 (R = - (Res_transient + Res_diffusion))
        !     if (present(F_T)) then
        !         ! Transient Residual (Integral N^T * dU/dt)
        !         local_vec(:) = 0.0d0
        !         call workspace%compute_R1(workspace%work_d_dt, local_vec)
        !         do i = 1, workspace%num_fe_nodes
        !             call F_T%set(OP_ADD, i, -local_vec(i))
        !         end do

        !         ! Diffusion Residual (Integral B^T * (-D * gradT))
        !         ! ※ compute_R2 は Flux を引数にとり、internal force (div q) を計算すると仮定
        !         ! workspace%work_V を Flux の一時置き場として利用
        !         workspace%work_V(:, :) = 0.0d0
        !         do i = 1, workspace%num_fe_gauss
        !             ! Flux q = - D * grad T
        !             flux_vec(:) = 0.0d0
        !             flux_vec = -matmul(workspace%work_D(:, :, i), workspace%state_gp(i)%grad_T%get_as_array())
        !             workspace%work_V(:, i) = flux_vec(:)
        !         end do

        !         local_vec(:) = 0.0d0
        !         call workspace%compute_R2(workspace%work_V, local_vec)
        !         do i = 1, workspace%num_fe_nodes
        !             call F_T%set(OP_ADD, i, -local_vec(i))
        !         end do
        !     end if

    end subroutine assemble_local_newton_thermal

    ! ==========================================================================
    ! Picard Assembly (Secant Stiffness & Linearized Residual)
    ! ==========================================================================
    module subroutine assemble_local_picard_thermal(self, controls, workspace, K_TT, K_TH, F_T)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_controls), intent(in) :: controls
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: K_TT
        type(type_matrix_dense), intent(inout), optional :: K_TH
        type(type_vector_dp), intent(inout), optional :: F_T

        !     integer(int32) :: i, j
        !     real(real64) :: local_vec(workspace%num_fe_nodes)
        !     real(real64) :: bdf0
        !     real(real64) :: T_val, hist_term

        !     bdf0 = workspace%bdf_coeffs(1)

        !     ! 1. 積分点ループ (係数計算)
        !     workspace%work_C(:) = 0.0d0
        !     workspace%work_D(:, :, :) = 0.0d0
        !     workspace%work_d_dt(:) = 0.0d0

        !     do i = 1, workspace%num_fe_gauss
        !         ! (A) Mass Term: 割線/有効熱容量 (Secant/Effective Heat Capacity)
        !         call self%compute_mass_term(workspace%material_id, workspace%state_gp(i), &
        !                                     workspace%work_C(i), scheme_opt=SCHEME_SECANT)

        !         ! (B) Diffusion Term: 熱伝導率
        !         call self%compute_diffusion_term(workspace%material_id, workspace%state_gp(i), &
        !                                          workspace%work_D(:, :, i))

        !         ! (C) Linearized Time Derivative: C_sec * (alpha0 * T + History)
        !         ! compute_history_term は C_sec * (sum alpha_k T_k) を返す
        !         call self%compute_history_term(workspace%material_id, workspace%state_gp(i), &
        !                                        workspace%bdf_coeffs(1:workspace%bdf_order + 1), &
        !                                        hist_term)

        !         call workspace%state_gp(i)%temperature%get(T_val)

        !         ! work_d_dt には「質量項の全寄与」を格納 (C * dT/dt 相当)
        !         ! dT/dt = alpha0 * T + (History_Sum) なので
        !         ! Term = C * alpha0 * T + (C * History_Sum)
        !         !      = C * alpha0 * T + hist_term
        !         workspace%work_d_dt(i) = workspace%work_C(i) * bdf0 * T_val + hist_term
        !     end do

        !     ! 2. システム行列 (Approximate Jacobian / Stiffness Matrix)
        !     ! J_picard = alpha0 * M_sec + K
        !     if (present(K_TT)) then
        !         ! Mass Part
        !         call workspace%compute_K1(workspace%work_C, workspace%work_matrix)
        !         do i = 1, workspace%num_fe_nodes
        !             do j = 1, workspace%num_fe_nodes
        !                 call K_TT%set(OP_ADD, i, j, bdf0 * workspace%work_matrix(i, j))
        !             end do
        !         end do

        !         ! Stiffness Part
        !         call workspace%compute_K2(workspace%work_D, workspace%work_matrix)
        !         do i = 1, workspace%num_fe_nodes
        !             do j = 1, workspace%num_fe_nodes
        !                 call K_TT%set(OP_ADD, i, j, workspace%work_matrix(i, j))
        !             end do
        !         end do
        !     end if

        !     ! 3. 残差ベクトル (Linearized Residual)
        !     ! R = - ( M_sec * dT/dt + K * T )
        !     if (present(F_T)) then
        !         ! Mass Residual
        !         local_vec(:) = 0.0d0
        !         call workspace%compute_R1(workspace%work_d_dt, local_vec)
        !         do i = 1, workspace%num_fe_nodes
        !             call F_T%set(OP_ADD, i, -local_vec(i))
        !         end do

        !         ! Diffusion Residual (K * T)
        !         ! ここでは work_D を使って行列ベクトル積を行うのが最も整合性がとれる
        !         local_vec(:) = 0.0d0
        !         call workspace%compute_K2(workspace%work_D, workspace%work_matrix)

        !         ! R -= K * T_node
        !         ! (work_vec を一時利用)
        !         workspace%work_vec(:) = 0.0d0
        !         do j = 1, workspace%num_fe_nodes
        !             do i = 1, workspace%num_fe_nodes
        !                 workspace%work_vec(i) = workspace%work_vec(i) + &
        !                                         workspace%work_matrix(i, j) * workspace%T_node(j)
        !             end do
        !         end do

        !         do i = 1, workspace%num_fe_nodes
        !             call F_T%set(OP_ADD, i, -workspace%work_vec(i))
        !         end do
        !     end if

    end subroutine assemble_local_picard_thermal

    ! module subroutine assemble_local_thermal(self, controls, workspace, K_TT, K_TH, F_T)
    !     implicit none
    !     class(type_thermal), intent(in) :: self
    !     type(type_controls), intent(in) :: controls
    !     type(type_assemble_workspace), intent(inout) :: workspace
    !     type(type_matrix_dense), intent(inout), optional :: K_TT
    !     type(type_matrix_dense), intent(inout), optional :: K_TH
    !     type(type_vector_dp), intent(inout), optional :: F_T

    !     integer(int32) :: i, j, d
    !     integer(int32) :: ierr
    !     real(real64) :: row_sum_val

    !     real(real64) :: local_vec(workspace%num_fe_nodes)
    !     real(real64) :: local_mass_diagonal(workspace%num_fe_nodes)

    !     workspace%work_C(:) = 0.0d0
    !     workspace%work_D(:, :, :) = 0.0d0
    !     workspace%work_L(:) = 0.0d0
    !     workspace%work_d_dt(:) = 0.0d0

    !     local_vec(:) = 0.0d0
    !     local_mass_diagonal(:) = 0.0d0

    !     do i = 1, workspace%num_fe_gauss
    !         call self%compute_mass_term(workspace%material_id, workspace%state_gp(i), workspace%work_C(i))
    !         call self%compute_diffusion_term(workspace%material_id, workspace%state_gp(i), workspace%work_D(:, :, i))
    !         call self%compute_history_term(workspace%material_id, workspace%state_gp(i), &
    !                                        workspace%bdf_coeffs(1:workspace%bdf_order + 1), workspace%work_d_dt(i))
    !     end do
    !     ! do i = 1, workspace%num_fe_nodes
    !     !     call self%compute_transient_term(workspace%material_id, workspace%state(i), &
    !     !                                      workspace%bdf_coeffs(1:workspace%bdf_order + 1), workspace%work_d_dt(i))
    !     ! end do

    !     ! if (controls%is_target(PHYSICS_TYPE_HYDRAULIC, workspace%material_id)) then
    !     !     do i = 1, workspace%num_fe_gauss
    !     !         call self%compute_latent_term(workspace%material_id, workspace%state_gp(i), workspace%work_L(i))
    !     !         do d = 1, workspace%num_fe_dimension
    !     !             workspace%work_D(d, d, i) = workspace%work_D(d, d, i) + workspace%work_L(i)
    !     !         end do
    !     !     end do
    !     ! end if

    !     call workspace%compute_K1(workspace%work_C, workspace%work_matrix)
    !     if (present(K_TT)) then
    !         do i = 1, workspace%num_fe_nodes
    !             do j = 1, workspace%num_fe_nodes
    !                 call K_TT%set(OP_ADD, i, j, workspace%bdf_coeffs(1) * workspace%work_matrix(i, j))
    !             end do
    !         end do
    !     end if

    !     if (present(F_T)) then
    !         local_vec(:) = 0.0d0
    !         call workspace%compute_R1(workspace%work_d_dt, local_vec)

    !         do i = 1, workspace%num_fe_nodes
    !             call F_T%set(OP_ADD, i, -local_vec(i))
    !         end do
    !     end if

    !     !!!! NRから変える必要なし
    !     call workspace%compute_K2(workspace%work_D, workspace%work_matrix)
    !     if (present(K_TT)) then
    !         do i = 1, workspace%num_fe_nodes
    !             do j = 1, workspace%num_fe_nodes
    !                 call K_TT%set(OP_ADD, i, j, workspace%work_matrix(i, j))
    !             end do
    !         end do
    !     end if

    !     ! if (present(F_T)) then
    !     !     workspace%work_vec(:) = 0.0d0
    !     !     call matvec(workspace%work_matrix, workspace%T_node, workspace%work_vec, ierr)

    !     !     do i = 1, workspace%num_fe_nodes
    !     !         call F_T%set(OP_ADD, i, -workspace%work_vec(i))
    !     !     end do
    !     ! end if

    ! end subroutine assemble_local_thermal
end submodule thermal_matrix

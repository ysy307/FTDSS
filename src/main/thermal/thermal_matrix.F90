submodule(main_thermal) thermal_matrix
    implicit none
contains

    !> @brief 局所（要素）行列と残差ベクトルのアセンブルを行う
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
    !> @brief Newton-Raphson法による接線剛性行列と残差ベクトルの計算
    module subroutine assemble_local_newton_thermal(self, controls, workspace, K_TT, K_TH, F_T)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_controls), intent(in) :: controls
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: K_TT
        type(type_matrix_dense), intent(inout), optional :: K_TH
        type(type_vector_dp), intent(inout), optional :: F_T

        integer(int32) :: i, j
        integer(int32) :: ierr
        real(real64) :: bdf0

        bdf0 = workspace%bdf_coeffs(1)

        ! 1. 積分点ループ (状態量・係数計算)
        workspace%work_C(:) = 0.0d0
        workspace%work_D(:, :, :) = 0.0d0
        workspace%work_d_dt(:) = 0.0d0

        do i = 1, workspace%num_fe_gauss
            ! (A) Mass Term: 接線熱容量 C_tan
            call self%compute_mass_term(workspace%material_id, workspace%state_gp(i), &
                                        workspace%work_C(i), scheme_opt=SCHEME_TANGENT)

            ! (B) Diffusion Term: 熱伝導率 D (現在の温度で評価)
            call self%compute_diffusion_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%work_D(:, :, i))

            ! (C) Transient Residual: エンタルピー時間微分 dH/dt
            call self%compute_transient_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%bdf_coeffs(1:workspace%bdf_order + 1), &
                                             workspace%work_d_dt(i))
        end do

        ! 2. 質量項（Mass Term）の寄与
        ! compute_K1: ∫ N^T * C * N dV を work_matrix に格納
        call workspace%compute_K1(workspace%work_C, workspace%work_matrix)

        if (present(K_TT)) then
            ! Jacobian += bdf0 * MassMatrix
            do j = 1, workspace%num_fe_nodes
                do i = 1, workspace%num_fe_nodes
                    call K_TT%set(OP_ADD, i, j, bdf0 * workspace%work_matrix(i, j))
                end do
            end do
        end if

        ! 3. 過渡項残差（Transient Residual）の寄与
        if (present(F_T)) then
            workspace%work_vec(:) = 0.0d0
            ! compute_R1: ∫ N^T * scalar dV (ここでは scalar = dH/dt)
            call workspace%compute_R1(workspace%work_d_dt, workspace%work_vec)

            do i = 1, workspace%num_fe_nodes
                call F_T%set(OP_ADD, i, -workspace%work_vec(i))
            end do
        end if

        ! 4. 拡散項（Diffusion Term）の寄与
        ! compute_K2: ∫ ∇N^T * D * ∇N dV を work_matrix に格納
        call workspace%compute_K2(workspace%work_D, workspace%work_matrix)

        if (present(K_TT)) then
            ! Jacobian += StiffnessMatrix
            do j = 1, workspace%num_fe_nodes
                do i = 1, workspace%num_fe_nodes
                    call K_TT%set(OP_ADD, i, j, workspace%work_matrix(i, j))
                end do
            end do
        end if

        ! 5. 内部力残差（Internal Force Residual）の寄与
        if (present(F_T)) then
            workspace%work_vec(:) = 0.0d0
            ! F_int = K * T_node (work_matrix は現在 K2 の結果を保持している)
            call matvec(workspace%work_matrix, workspace%T_node, workspace%work_vec, ierr)

            do i = 1, workspace%num_fe_nodes
                call F_T%set(OP_ADD, i, -workspace%work_vec(i))
            end do
        end if

    end subroutine assemble_local_newton_thermal

    ! ==========================================================================
    ! Picard Assembly (Secant Stiffness & Linearized Residual)
    ! ==========================================================================
    !> @brief 修正Picard法による線形化行列と右辺ベクトルの計算
    module subroutine assemble_local_picard_thermal(self, controls, workspace, K_TT, K_TH, F_T)
        implicit none
        class(type_thermal), intent(in) :: self
        type(type_controls), intent(in) :: controls
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: K_TT
        type(type_matrix_dense), intent(inout), optional :: K_TH
        type(type_vector_dp), intent(inout), optional :: F_T

        integer(int32) :: i, j
        real(real64) :: val_T, bdf0

        ! 作業用ベクトル
        real(real64) :: local_vec_transient(workspace%num_fe_nodes) ! エンタルピー時間変化項
        real(real64) :: local_vec_diff_flux(workspace%num_fe_nodes) ! 拡散フラックス (K*T)

        ! 初期化
        workspace%work_C(:) = 0.0d0
        workspace%work_D(:, :, :) = 0.0d0
        workspace%work_d_dt(:) = 0.0d0

        local_vec_transient(:) = 0.0d0
        local_vec_diff_flux(:) = 0.0d0

        bdf0 = workspace%bdf_coeffs(1)

        ! ----------------------------------------------------------------------
        ! 1. 積分点ループ
        ! ----------------------------------------------------------------------
        do i = 1, workspace%num_fe_gauss
            ! (A) 行列用: 瞬間熱容量 C (Picard用)
            call self%compute_mass_term(workspace%material_id, workspace%state_gp(i), workspace%work_C(i))

            ! (B) 行列用: 熱伝導率 D
            call self%compute_diffusion_term(workspace%material_id, workspace%state_gp(i), workspace%work_D(:, :, i))

            ! (C) [重要] 残差用: エンタルピー時間変化 dH/dt (Newtonと同じ厳密計算を使う)
            !     compute_history_term ではなく compute_transient_term を使う
            call self%compute_transient_term(workspace%material_id, workspace%state_gp(i), &
                                             workspace%bdf_coeffs(1:workspace%bdf_order + 1), &
                                             workspace%work_d_dt(i))
        end do

        ! ----------------------------------------------------------------------
        ! 2. 質量行列 (LHS) の構築 [変更なし]
        !    Picard行列として安定性を重視し，C_app (or C_vol) を使用
        ! ----------------------------------------------------------------------
        call workspace%compute_K1(workspace%work_C, workspace%work_matrix)
        if (present(K_TT)) then
            do i = 1, workspace%num_fe_nodes
                do j = 1, workspace%num_fe_nodes
                    call K_TT%set(OP_ADD, i, j, bdf0 * workspace%work_matrix(i, j))
                end do
            end do
        end if

        ! ※ ここで local_vec_KT (Mass分) を計算してはいけません！
        !    Mass項の残差は work_d_dt (エンタルピー差分) から直接作ります．

        ! ----------------------------------------------------------------------
        ! 3. 拡散行列 (LHS) & 拡散フラックス (RHS一部) の構築
        ! ----------------------------------------------------------------------
        call workspace%compute_K2(workspace%work_D, workspace%work_matrix)

        ! 行列 K_TT への加算
        if (present(K_TT)) then
            do i = 1, workspace%num_fe_nodes
                do j = 1, workspace%num_fe_nodes
                    call K_TT%set(OP_ADD, i, j, workspace%work_matrix(i, j))
                end do
            end do
        end if

        ! 拡散項の「現在のフラックス」だけは K*T で計算してOK
        ! (D(T) * grad T なので，行列積と物理的意味が一致するため)
        do i = 1, workspace%num_fe_nodes
            do j = 1, workspace%num_fe_nodes
                local_vec_diff_flux(i) = local_vec_diff_flux(i) + &
                                         workspace%work_matrix(i, j) * workspace%T_node(j)
            end do
        end do

        ! ----------------------------------------------------------------------
        ! 4. 残差ベクトル (Residual) の構築
        !    R = F_ext - ( dH/dt + K*T )
        ! ----------------------------------------------------------------------
        if (present(F_T)) then
            ! エンタルピー項 (dH/dt) の積分 -> local_vec_transient
            call workspace%compute_R1(workspace%work_d_dt, local_vec_transient)

            do i = 1, workspace%num_fe_nodes
                ! 残差 = - (過渡項 + 拡散項)
                ! ※ F_ext があればさらに足す

                val_T = -local_vec_transient(i) ! 厳密なエンタルピー変化
                val_T = val_T - local_vec_diff_flux(i) ! 現在の拡散流出

                call F_T%set(OP_ADD, i, val_T)
            end do
        end if

    end subroutine assemble_local_picard_thermal

end submodule thermal_matrix

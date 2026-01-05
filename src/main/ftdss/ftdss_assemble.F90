submodule(main_ftdss) ftdss_assemble
    implicit none

contains

    module subroutine assemble_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        type(type_matrix_dense) :: local_J_TT, local_J_TH, local_J_HH, local_J_HT
        type(type_vector_dp) :: local_R_T, local_R_H

        integer(int32) :: num_elements, num_nodes
        integer(int32), pointer, contiguous, dimension(:) :: p_connectivity
        integer(int32) :: i
        integer(int32) :: thermal_dof, hydraulic_dof

        ! BDF係数用
        real(real64), pointer, dimension(:) :: time_coef => null()
        real(real64) :: bdf_shift

        call self%controls%profiler%start("Assemble")

        call self%J%zero()
        call self%R%zero()

        ! --- BDF係数の取得 (0番目が 現在のステップの係数 * 1/dt) ---
        call self%controls%time%get_bdf_coeffs(time_coef)
        if (associated(time_coef)) then
            bdf_shift = time_coef(0)
        else
            ! フォールバック（通常ここには来ないはずですが）
            bdf_shift = 0.0d0
        end if

        ! 全要素数の取得
        num_elements = self%domain%get_num_elements()
        num_nodes = self%domain%get_num_nodes()

        ! --- 行列オブジェクトの初期化 ---
        call local_J_HH%initialize(num_nodes)
        call local_J_HT%initialize(num_nodes)
        call local_J_TT%initialize(num_nodes)
        call local_J_TH%initialize(num_nodes)
        call local_R_T%initialize(num_nodes)
        call local_R_H%initialize(num_nodes)

        call local_J_HH%zero()
        call local_J_HT%zero()
        call local_J_TT%zero()
        call local_J_TH%zero()
        call local_R_T%zero()
        call local_R_H%zero()

        do i = 1, num_elements
            ! bdf_shift を渡す
            call self%assemble_local(i, local_J_TT, local_J_TH, local_J_HH, local_J_HT, &
                                     local_R_T, local_R_H, bdf_shift=bdf_shift)

            call self%domain%get_element_connectivity(i, p_connectivity)
            call self%domain%get_target_dof(PHYSICS_TYPE_THERMAL, thermal_dof)
            call self%domain%get_target_dof(PHYSICS_TYPE_HYDRAULIC, hydraulic_dof)

            call self%J%add(thermal_dof, thermal_dof, p_connectivity, local_J_TT)
            ! call self%J%add(thermal_dof, hydraulic_dof, p_connectivity, local_J_TH)
            ! call self%J%add(hydraulic_dof, hydraulic_dof, p_connectivity, local_J_HH)
            ! call self%J%add(hydraulic_dof, thermal_dof, p_connectivity, local_J_HT)

            call self%R%add(thermal_dof, p_connectivity, local_R_T)
            ! call self%R%add(hydraulic_dof, p_connectivity, local_R_H)
        end do

        ! デストラクタがあるなら呼ぶ（あるいはScopeを抜ければ解放される設計か確認）
        call local_J_TT%destroy()
        call local_J_TH%destroy()
        call local_J_HH%destroy()
        call local_J_HT%destroy()

        call self%controls%profiler%stop("Assemble")

    end subroutine assemble_ftdss

    ! ==========================================================================
    ! メイン: 局所行列および残差ベクトルのアセンブリ
    ! ==========================================================================
    module subroutine assemble_local_ftdss(self, element_id, local_J_TT, local_J_TH, &
                                           local_J_HH, local_J_HT, local_R_T, local_R_H, &
                                           bdf_shift)
        implicit none
        class(type_ftdss), intent(inout) :: self
        integer(int32), intent(in) :: element_id
        type(type_matrix_dense), intent(inout), optional :: local_J_TT, local_J_TH, local_J_HH, local_J_HT
        type(type_vector_dp), intent(inout), optional :: local_R_T, local_R_H
        real(real64), intent(in), optional :: bdf_shift ! 追加

        class(abst_fe), pointer :: fe
        integer(int32) :: num_nodes, num_gauss, material_id, dim
        integer(int32), pointer, contiguous, dimension(:) :: connectivity
        real(real64), pointer, contiguous, dimension(:) :: weights
        type(type_coordinate_dp), pointer, contiguous, dimension(:) :: gauss_points
        real(real64), allocatable :: coordinates(:, :)

        ! 物理係数配列
        real(real64), allocatable :: C_TT(:), C_TH(:), C_HH(:), C_HT(:)
        real(real64), allocatable :: M_TT(:, :, :), M_TH(:, :, :), M_HH(:, :, :), M_HT(:, :, :)
        real(real64), allocatable :: V_TT(:, :), V_TH(:, :), V_HH(:, :), V_HT(:, :)
        real(real64), allocatable :: R_T_C(:), R_T_D(:, :), R_H_C(:), R_H_D(:, :)

        ! 作業用バッファ
        real(real64), allocatable :: local_J(:, :)
        real(real64), allocatable :: local_R(:)

        ! --- 初期化 ---
        if (present(local_J_TT)) call local_J_TT%zero()
        if (present(local_J_TH)) call local_J_TH%zero()
        if (present(local_J_HH)) call local_J_HH%zero()
        if (present(local_J_HT)) call local_J_HT%zero()
        if (present(local_R_T)) call local_R_T%zero()
        if (present(local_R_H)) call local_R_H%zero()

        ! --- 要素情報の取得 ---
        call self%domain%get_material_id(element_id, material_id)
        call self%domain%get_element(element_id, fe)
        call self%domain%get_element_connectivity(element_id, connectivity)
        call self%domain%get_element_coordinate(element_id, coordinates)

        dim = self%domain%get_computation_dimension()
        call fe%get_num_nodes(num_nodes)

        ! --- 配列確保 ---
        allocate (local_J(num_nodes, num_nodes))
        allocate (local_R(num_nodes))

        call allocate_coefficient_arrays(dim, num_nodes, &
                                         C_TT, C_TH, C_HH, C_HT, &
                                         M_TT, M_TH, M_HH, M_HT, &
                                         V_TT, V_TH, V_HH, V_HT, &
                                         R_T_C, R_T_D, R_H_C, R_H_D)

        ! --- 物理係数計算 ---
        call compute_nodal_coefficients(self, element_id, material_id, num_nodes, connectivity, &
                                        C_TT, C_TH, C_HH, C_HT, &
                                        M_TT, M_TH, M_HH, M_HT, &
                                        V_TT, V_TH, V_HH, V_HT, &
                                        R_T_C, R_T_D, R_H_C, R_H_D)

        ! --- アセンブリ (行列 & 残差) ---
        call assemble_matrices(fe, coordinates, dim, local_J, local_R, &
                               C_TT, C_TH, C_HH, C_HT, &
                               M_TT, M_TH, M_HH, M_HT, &
                               V_TT, V_TH, V_HH, V_HT, &
                               R_T_C, R_T_D, R_H_C, R_H_D, &
                               local_J_TT, local_J_TH, local_J_HH, local_J_HT, &
                               local_R_T, local_R_H, &
                               bdf_shift=bdf_shift) ! 渡す

    end subroutine assemble_local_ftdss

    ! ==========================================================================
    ! サブ: アセンブリ (Matrix & Residual)
    ! ==========================================================================
    subroutine assemble_matrices(fe, coordinates, dim, local_J, local_R, &
                                 C_TT, C_TH, C_HH, C_HT, &
                                 M_TT, M_TH, M_HH, M_HT, &
                                 V_TT, V_TH, V_HH, V_HT, &
                                 R_T_C, R_T_D, R_H_C, R_H_D, &
                                 local_J_TT, local_J_TH, local_J_HH, local_J_HT, &
                                 local_R_T, local_R_H, &
                                 bdf_shift) ! 追加
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: coordinates(:, :)
        integer(int32), intent(in) :: dim
        real(real64), intent(inout) :: local_J(:, :)
        real(real64), intent(inout) :: local_R(:)

        real(real64), intent(in) :: C_TT(:), C_TH(:), C_HH(:), C_HT(:)
        real(real64), intent(in) :: M_TT(:, :, :), M_TH(:, :, :), M_HH(:, :, :), M_HT(:, :, :)
        real(real64), intent(in) :: V_TT(:, :), V_TH(:, :), V_HH(:, :), V_HT(:, :)
        real(real64), intent(in) :: R_T_C(:), R_T_D(:, :), R_H_C(:), R_H_D(:, :)

        type(type_matrix_dense), intent(inout), optional :: local_J_TT, local_J_TH, local_J_HH, local_J_HT
        type(type_vector_dp), intent(inout), optional :: local_R_T, local_R_H
        real(real64), intent(in), optional :: bdf_shift

        real(real64) :: shift_val

        ! デフォルトは 1.0 だが、BDFを使う場合は必須
        shift_val = 1.0d0
        if (present(bdf_shift)) shift_val = bdf_shift

        ! ----------------------------------------------------------------------
        ! 1. Matrix Assembly
        ! ----------------------------------------------------------------------
        ! --- C Terms (Capacity) ---
        ! 容量項には bdf_shift (time_coef(0)) を乗算する
        if (present(local_J_TT)) call add_term_scalar(fe, coordinates, dim, C_TT, local_J, local_J_TT, scale=shift_val)
        if (present(local_J_TH)) call add_term_scalar(fe, coordinates, dim, C_TH, local_J, local_J_TH, scale=shift_val)
        if (present(local_J_HH)) call add_term_scalar(fe, coordinates, dim, C_HH, local_J, local_J_HH, scale=shift_val)
        if (present(local_J_HT)) call add_term_scalar(fe, coordinates, dim, C_HT, local_J, local_J_HT, scale=shift_val)

        ! --- M Terms (Diffusion) ---
        ! 拡散項はスケールしない（通常のStiffness Matrix）
        if (present(local_J_TT)) call add_term_tensor(fe, coordinates, dim, M_TT, local_J, local_J_TT)
        if (present(local_J_TH)) call add_term_tensor(fe, coordinates, dim, M_TH, local_J, local_J_TH)
        if (present(local_J_HH)) call add_term_tensor(fe, coordinates, dim, M_HH, local_J, local_J_HH)
        if (present(local_J_HT)) call add_term_tensor(fe, coordinates, dim, M_HT, local_J, local_J_HT)

        ! --- V Terms (Advection) ---
        if (present(local_J_TT)) call add_term_vector(fe, coordinates, dim, V_TT, local_J, local_J_TT)
        if (present(local_J_TH)) call add_term_vector(fe, coordinates, dim, V_TH, local_J, local_J_TH)
        if (present(local_J_HH)) call add_term_vector(fe, coordinates, dim, V_HH, local_J, local_J_HH)
        if (present(local_J_HT)) call add_term_vector(fe, coordinates, dim, V_HT, local_J, local_J_HT)

        ! ----------------------------------------------------------------------
        ! 2. Residual Assembly
        ! ----------------------------------------------------------------------
        ! 残差は time_coef を使わず、既に離散化された値が入っている R_T_C 等を使う想定
        if (present(local_R_T)) then
            call add_residual_scalar(fe, coordinates, dim, R_T_C, local_R, local_R_T)
            call add_residual_vector(fe, coordinates, dim, R_T_D, local_R, local_R_T)
        end if

        if (present(local_R_H)) then
            call add_residual_scalar(fe, coordinates, dim, R_H_C, local_R, local_R_H)
            call add_residual_vector(fe, coordinates, dim, R_H_D, local_R, local_R_H)
        end if

    end subroutine assemble_matrices

    ! ==========================================================================
    ! Helper Subroutines for Matrix
    ! ==========================================================================
    subroutine add_term_scalar(fe, coords, dim, coeff, buffer, target_mat, scale)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: coords(:, :)
        integer(int32), intent(in) :: dim
        real(real64), intent(in) :: coeff(:)
        real(real64), intent(inout) :: buffer(:, :)
        type(type_matrix_dense), intent(inout) :: target_mat
        real(real64), intent(in), optional :: scale ! 追加

        integer(int32) :: i, j, nd
        real(real64) :: s

        s = 1.0d0
        if (present(scale)) s = scale

        nd = size(buffer, 1)

        call fe%compute_K(coords, coeff, buffer)

        do i = 1, nd
            do j = 1, nd
                call target_mat%set(OP_ADD, i, j, buffer(i, j) * s)
            end do
        end do
    end subroutine add_term_scalar

    subroutine add_term_tensor(fe, coords, dim, coeff, buffer, target_mat, scale)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: coords(:, :)
        integer(int32), intent(in) :: dim
        real(real64), intent(in) :: coeff(:, :, :)
        real(real64), intent(inout) :: buffer(:, :)
        type(type_matrix_dense), intent(inout) :: target_mat
        real(real64), intent(in), optional :: scale

        integer(int32) :: i, j, nd
        real(real64) :: s

        s = 1.0d0
        if (present(scale)) s = scale

        nd = size(buffer, 1)

        call fe%compute_K(coords, coeff, buffer)

        do i = 1, nd
            do j = 1, nd
                call target_mat%set(OP_ADD, i, j, buffer(i, j) * s)
            end do
        end do
    end subroutine add_term_tensor

    subroutine add_term_vector(fe, coords, dim, coeff, buffer, target_mat, scale)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: coords(:, :)
        integer(int32), intent(in) :: dim
        real(real64), intent(in) :: coeff(:, :)
        real(real64), intent(inout) :: buffer(:, :)
        type(type_matrix_dense), intent(inout) :: target_mat
        real(real64), intent(in), optional :: scale

        integer(int32) :: i, j, nd
        real(real64) :: s

        s = 1.0d0
        if (present(scale)) s = scale

        nd = size(buffer, 1)

        call fe%compute_K(coords, coeff, buffer)

        do i = 1, nd
            do j = 1, nd
                call target_mat%set(OP_ADD, i, j, buffer(i, j) * s)
            end do
        end do
    end subroutine add_term_vector

    ! ==========================================================================
    ! Helper Subroutines for Residual
    ! ==========================================================================
    ! (残差のヘルパー関数は変更なし)
    subroutine add_residual_scalar(fe, coords, dim, coeff, buffer, target_vec)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: coords(:, :)
        integer(int32), intent(in) :: dim
        real(real64), intent(in) :: coeff(:)
        real(real64), intent(inout) :: buffer(:)
        type(type_vector_dp), intent(inout) :: target_vec
        integer(int32) :: i, nd
        nd = size(buffer, 1)
        call fe%compute_R(coords, coeff, buffer)
        do i = 1, nd
            call target_vec%set(OP_ADD, i, buffer(i))
        end do
    end subroutine add_residual_scalar

    subroutine add_residual_vector(fe, coords, dim, coeff, buffer, target_vec)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: coords(:, :)
        integer(int32), intent(in) :: dim
        real(real64), intent(in) :: coeff(:, :)
        real(real64), intent(inout) :: buffer(:)
        type(type_vector_dp), intent(inout) :: target_vec
        integer(int32) :: i, nd
        nd = size(buffer, 1)
        call fe%compute_R(coords, coeff, buffer)
        do i = 1, nd
            call target_vec%set(OP_ADD, i, buffer(i))
        end do
    end subroutine add_residual_vector

end submodule ftdss_assemble

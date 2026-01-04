submodule(main_ftdss) ftdss_assemble
    implicit none

contains

    ! ==========================================================================
    ! メイン: 局所行列のアセンブリ
    ! ==========================================================================
    module subroutine assemble_local_ftdss(self, element_id, local_J_TT, local_J_TH, local_J_HH, local_J_HT, local_R)
        implicit none
        class(type_ftdss), intent(inout) :: self
        integer(int32), intent(in) :: element_id
        type(type_matrix_dense), intent(inout), optional :: local_J_TT, local_J_TH, local_J_HH, local_J_HT
        type(type_vector_dp), intent(inout), optional :: local_R

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

        ! 作業用バッファ (dim x dim で固定)
        real(real64), allocatable :: local_J(:, :)

        ! --- 初期化 ---
        if (present(local_J_TT)) call local_J_TT%zero()
        if (present(local_J_TH)) call local_J_TH%zero()
        if (present(local_J_HH)) call local_J_HH%zero()
        if (present(local_J_HT)) call local_J_HT%zero()
        if (present(local_R)) call local_R%zero()

        ! --- 要素情報の取得 ---
        call self%domain%get_material_id(element_id, material_id)
        call self%domain%get_element(element_id, fe)
        call self%domain%get_element_connectivity(element_id, connectivity)
        call self%domain%get_element_coordinate(element_id, coordinates)

        dim = self%domain%get_computation_dimension()
        call fe%get_num_nodes(num_nodes)

        ! --- 配列確保 ---
        ! local_J は常に dim x dim のサイズで良い
        allocate (local_J(dim, dim))

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

        ! --- アセンブリ ---
        ! ここですべて dim x dim として処理する
        call assemble_matrices(fe, coordinates, dim, local_J, &
                               C_TT, C_TH, C_HH, C_HT, &
                               M_TT, M_TH, M_HH, M_HT, &
                               V_TT, V_TH, V_HH, V_HT, &
                               local_J_TT, local_J_TH, local_J_HH, local_J_HT)

    end subroutine assemble_local_ftdss

    ! ==========================================================================
    ! サブ: 配列確保
    ! ==========================================================================
    subroutine allocate_coefficient_arrays(dim, num_nodes, &
                                           C_TT, C_TH, C_HH, C_HT, &
                                           M_TT, M_TH, M_HH, M_HT, &
                                           V_TT, V_TH, V_HH, V_HT, &
                                           R_T_C, R_T_D, R_H_C, R_H_D)
        implicit none
        integer(int32), intent(in) :: dim, num_nodes
        real(real64), allocatable, intent(inout) :: C_TT(:), C_TH(:), C_HH(:), C_HT(:)
        real(real64), allocatable, intent(inout) :: M_TT(:, :, :), M_TH(:, :, :), M_HH(:, :, :), M_HT(:, :, :)
        real(real64), allocatable, intent(inout) :: V_TT(:, :), V_TH(:, :), V_HH(:, :), V_HT(:, :)
        real(real64), allocatable, intent(inout) :: R_T_C(:), R_T_D(:, :), R_H_C(:), R_H_D(:, :)

        call allocate_array(C_TT, num_nodes)
        call allocate_array(C_TH, num_nodes)
        call allocate_array(C_HH, num_nodes)
        call allocate_array(C_HT, num_nodes)

        call allocate_array(M_TT, dim, dim, num_nodes)
        call allocate_array(M_TH, dim, dim, num_nodes)
        call allocate_array(M_HH, dim, dim, num_nodes)
        call allocate_array(M_HT, dim, dim, num_nodes)

        call allocate_array(V_TT, dim, num_nodes)
        call allocate_array(V_TH, dim, num_nodes)
        call allocate_array(V_HH, dim, num_nodes)
        call allocate_array(V_HT, dim, num_nodes)

        call allocate_array(R_T_C, num_nodes)
        call allocate_array(R_T_D, dim, num_nodes)
        call allocate_array(R_H_C, num_nodes)
        call allocate_array(R_H_D, dim, num_nodes)

    end subroutine allocate_coefficient_arrays

    ! ==========================================================================
    ! サブ: 物理係数計算
    ! ==========================================================================
    subroutine compute_nodal_coefficients(self, element_id, material_id, num_nodes, connectivity, &
                                          C_TT, C_TH, C_HH, C_HT, &
                                          M_TT, M_TH, M_HH, M_HT, &
                                          V_TT, V_TH, V_HH, V_HT, &
                                          R_T_C, R_T_D, R_H_C, R_H_D)
        implicit none
        class(type_ftdss), intent(inout) :: self
        integer(int32), intent(in) :: element_id, material_id, num_nodes
        integer(int32), intent(in) :: connectivity(:)

        real(real64), intent(inout) :: C_TT(:), C_TH(:), C_HH(:), C_HT(:)
        real(real64), intent(inout) :: M_TT(:, :, :), M_TH(:, :, :), M_HH(:, :, :), M_HT(:, :, :)
        real(real64), intent(inout) :: V_TT(:, :), V_TH(:, :), V_HH(:, :), V_HT(:, :)
        real(real64), intent(inout) :: R_T_C(:), R_T_D(:, :), R_H_C(:), R_H_D(:, :)

        integer(int32) :: i
        type(type_state) :: state

        do i = 1, num_nodes
            call self%set_state(connectivity(i), element_id, state)

            call self%thermal%compute_C_T(material_id, state, C_TT(i), C_TH(i))
            call self%thermal%compute_D_T(material_id, state, M_TT(:, :, i), M_TH(:, :, i))
            call self%thermal%compute_V_T(material_id, state, V_TT(:, i), V_TH(:, i))
            call self%thermal%compute_R_T(material_id, state, R_T_C(i), R_T_D(:, i))

            call self%hydraulic%compute_C_H(material_id, state, C_HH(i), C_HT(i))
            call self%hydraulic%compute_D_H(material_id, state, M_HH(:, :, i), M_HT(:, :, i))
            call self%hydraulic%compute_V_H(material_id, state, V_HH(:, i), V_HT(:, i))
            call self%hydraulic%compute_R_H(material_id, state, R_H_C(i), R_H_D(:, i))
        end do
    end subroutine compute_nodal_coefficients

    ! ==========================================================================
    ! サブ: 行列のアセンブリ
    ! ==========================================================================
    subroutine assemble_matrices(fe, coordinates, dim, local_J, &
                                 C_TT, C_TH, C_HH, C_HT, &
                                 M_TT, M_TH, M_HH, M_HT, &
                                 V_TT, V_TH, V_HH, V_HT, &
                                 local_J_TT, local_J_TH, local_J_HH, local_J_HT)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: coordinates(:, :)
        integer(int32), intent(in) :: dim
        real(real64), intent(inout) :: local_J(dim, dim) ! Pre-allocated dim x dim

        real(real64), intent(in) :: C_TT(:), C_TH(:), C_HH(:), C_HT(:)
        real(real64), intent(in) :: M_TT(:, :, :), M_TH(:, :, :), M_HH(:, :, :), M_HT(:, :, :)
        real(real64), intent(in) :: V_TT(:, :), V_TH(:, :), V_HH(:, :), V_HT(:, :)

        type(type_matrix_dense), intent(inout), optional :: local_J_TT, local_J_TH, local_J_HH, local_J_HT

        ! --- C Terms (Scalar coeff input) -> dim x dim output ---
        if (present(local_J_TT)) call add_term_scalar(fe, coordinates, dim, C_TT, local_J, local_J_TT)
        if (present(local_J_TH)) call add_term_scalar(fe, coordinates, dim, C_TH, local_J, local_J_TH)
        if (present(local_J_HH)) call add_term_scalar(fe, coordinates, dim, C_HH, local_J, local_J_HH)
        if (present(local_J_HT)) call add_term_scalar(fe, coordinates, dim, C_HT, local_J, local_J_HT)

        ! --- M Terms (Tensor coeff input) -> dim x dim output ---
        if (present(local_J_TT)) call add_term_tensor(fe, coordinates, dim, M_TT, local_J, local_J_TT)
        if (present(local_J_TH)) call add_term_tensor(fe, coordinates, dim, M_TH, local_J, local_J_TH)
        if (present(local_J_HH)) call add_term_tensor(fe, coordinates, dim, M_HH, local_J, local_J_HH)
        if (present(local_J_HT)) call add_term_tensor(fe, coordinates, dim, M_HT, local_J, local_J_HT)

        ! --- V Terms (Vector coeff input) -> dim x dim output ---
        if (present(local_J_TT)) call add_term_vector(fe, coordinates, dim, V_TT, local_J, local_J_TT)
        if (present(local_J_TH)) call add_term_vector(fe, coordinates, dim, V_TH, local_J, local_J_TH)
        if (present(local_J_HH)) call add_term_vector(fe, coordinates, dim, V_HH, local_J, local_J_HH)
        if (present(local_J_HT)) call add_term_vector(fe, coordinates, dim, V_HT, local_J, local_J_HT)

    end subroutine assemble_matrices

    ! --------------------------------------------------------------------------
    ! ヘルパー: スカラー係数行列の加算
    ! --------------------------------------------------------------------------
    subroutine add_term_scalar(fe, coords, dim, coeff, buffer, target_mat)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: coords(:, :)
        integer(int32), intent(in) :: dim
        real(real64), intent(in) :: coeff(:)
        real(real64), intent(inout) :: buffer(:, :)
        type(type_matrix_dense), intent(inout) :: target_mat
        integer(int32) :: i, j

        call fe%compute_K(coords, coeff, buffer)

        do i = 1, dim
            do j = 1, dim
                call target_mat%set(OP_ADD, i, j, buffer(i, j))
            end do
        end do
    end subroutine add_term_scalar

    ! --------------------------------------------------------------------------
    ! ヘルパー: テンソル係数行列の加算
    ! --------------------------------------------------------------------------
    subroutine add_term_tensor(fe, coords, dim, coeff, buffer, target_mat)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: coords(:, :)
        integer(int32), intent(in) :: dim
        real(real64), intent(in) :: coeff(:, :, :)
        real(real64), intent(inout) :: buffer(:, :)
        type(type_matrix_dense), intent(inout) :: target_mat
        integer(int32) :: i, j

        call fe%compute_K(coords, coeff, buffer)

        do i = 1, dim
            do j = 1, dim
                call target_mat%set(OP_ADD, i, j, buffer(i, j))
            end do
        end do
    end subroutine add_term_tensor

    ! --------------------------------------------------------------------------
    ! ヘルパー: ベクトル係数行列の加算
    ! --------------------------------------------------------------------------
    subroutine add_term_vector(fe, coords, dim, coeff, buffer, target_mat)
        implicit none
        class(abst_fe), intent(in) :: fe
        real(real64), intent(in) :: coords(:, :)
        integer(int32), intent(in) :: dim
        real(real64), intent(in) :: coeff(:, :)
        real(real64), intent(inout) :: buffer(:, :)
        type(type_matrix_dense), intent(inout) :: target_mat
        integer(int32) :: i, j

        call fe%compute_K(coords, coeff, buffer)

        do i = 1, dim
            do j = 1, dim
                call target_mat%set(OP_ADD, i, j, buffer(i, j))
            end do
        end do
    end subroutine add_term_vector

end submodule ftdss_assemble

submodule(main_ftdss) ftdss_assemble
    implicit none

contains

    !> Perform the global assembly for the FTDSS solver.
    module subroutine assemble_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        ! ループ外で宣言（ここまではOK）
        type(type_matrix_dense) :: local_J_TT, local_J_TH, local_J_HH, local_J_HT
        type(type_vector_dp) :: local_R_T, local_R_H
        type(type_assemble_workspace) :: workspace

        integer(int32) :: num_elements
        integer(int32), pointer, contiguous, dimension(:) :: p_connectivity => null()
        integer(int32) :: i
        integer(int32) :: thermal_dof, hydraulic_dof

        call self%controls%profiler%start("Assemble")

        call self%J%zero()
        call self%R%zero()

        call self%domain%get_num_elements(num_elements)

        ! --- [重要] 要素ループ ---
        do i = 1, num_elements
            ! 1. 初期化・準備 (Allocateは初回またはサイズ変更時のみ。値はゼロクリアされる)
            call self%assemble_initialize(element_id=i, workspace=workspace, &
                                          local_J_TT=local_J_TT, local_J_TH=local_J_TH, &
                                          local_J_HH=local_J_HH, local_J_HT=local_J_HT, &
                                          local_R_T=local_R_T, local_R_H=local_R_H)

            ! 2. 局所行列の計算
            call self%assemble_local(workspace, local_J_TT, local_J_TH, local_J_HH, local_J_HT, &
                                     local_R_T, local_R_H)

            ! 3. 全体行列への組み込み
            call self%domain%get_element_connectivity(i, p_connectivity)
            call self%domain%get_target_dof(PHYSICS_TYPE_THERMAL, thermal_dof)
            call self%domain%get_target_dof(PHYSICS_TYPE_HYDRAULIC, hydraulic_dof)

            call self%J%add(thermal_dof, thermal_dof, p_connectivity, local_J_TT)
            ! call self%J%add(thermal_dof, hydraulic_dof, p_connectivity, local_J_TH)
            ! call self%J%add(hydraulic_dof, hydraulic_dof, p_connectivity, local_J_HH)
            ! call self%J%add(hydraulic_dof, thermal_dof, p_connectivity, local_J_HT)

            call self%R%add(thermal_dof, p_connectivity, local_R_T)
            ! call self%R%add(hydraulic_dof, p_connectivity, local_R_H)

            ! [修正] ここで finalize (destroy) を呼ばない！
            ! ループ内でのメモリ解放・再確保を防ぐため、次のループで変数を再利用する。
        end do

        ! [修正] ループが終わった後に一括でメモリ解放を行う
        call self%assemble_finalize(workspace, local_J_TT, local_J_TH, &
                                    local_J_HH, local_J_HT, local_R_T, local_R_H)

        call self%controls%profiler%stop("Assemble")

    end subroutine assemble_ftdss

    module subroutine assemble_initialize_ftdss(self, element_id, workspace, local_J_TT, local_J_TH, &
                                                local_J_HH, local_J_HT, local_R_T, local_R_H)
        implicit none
        class(type_ftdss), intent(inout) :: self
        integer(int32), intent(in) :: element_id
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: local_J_TT, local_J_TH, local_J_HH, local_J_HT
        type(type_vector_dp), intent(inout), optional :: local_R_T, local_R_H

        class(abst_fe), pointer :: fe => null()
        integer(int32), pointer, contiguous, dimension(:) :: connectivity => null()
        real(real64), allocatable :: coordinates(:, :)
        integer(int32) :: material_id
        integer(int32) :: computation_type
        integer(int32) :: num_nodes
        ! type(type_matrix_info) :: matrix_info ! 未使用なら削除

        integer(int32) :: i

        call self%domain%get_material_id(element_id, material_id)
        call self%domain%get_element(element_id, fe)
        call self%domain%get_element_connectivity(element_id, connectivity)
        call self%domain%get_computation_type(computation_type)
        call self%domain%get_element_coordinate(element_id, coordinates)

        ! Workspaceのセットアップ
        ! workspace%initialize は「メモリ確保」ではなく「値のリセットとポインタ設定」を行うように
        ! 内部実装されていることが望ましい。もし毎回 allocate しているなら、
        ! workspace%reset() のような軽量なメソッドを作るべき。
        call workspace%initialize(fe, material_id, element_id, computation_type, coordinates, self%controls)

        do i = 1, size(connectivity)
            call self%set_state(connectivity(i), element_id, workspace%state(i))
        end do

        call workspace%lerp()
        do i = 1, workspace%num_fe_gauss
            call self%thermal%update_water_phases(material_id, workspace%state_gp(i))
        end do

        ! [注意] workspace%coordinates へのコピーなどは workspace%initialize でポインタ接続する方が効率的
        ! ここではそのままにしておく
        workspace%coordinates = coordinates

        !!------ 行列・ベクトルの準備 ------
        call fe%get_num_nodes(num_nodes)

        ! 以下の check_initialize_matrix/vector 内で
        ! 「サイズが同じなら allocate しない」かつ「値をゼロクリアする」処理が必要
        if (present(local_J_TT)) call check_initialize_matrix(local_J_TT, num_nodes)
        if (present(local_J_TH)) call check_initialize_matrix(local_J_TH, num_nodes)
        if (present(local_J_HH)) call check_initialize_matrix(local_J_HH, num_nodes)
        if (present(local_J_HT)) call check_initialize_matrix(local_J_HT, num_nodes)

        if (present(local_R_T)) call check_initialize_vector(local_R_T, num_nodes)
        if (present(local_R_H)) call check_initialize_vector(local_R_H, num_nodes)

    end subroutine assemble_initialize_ftdss

    subroutine check_initialize_matrix(matrix, num_nodes)
        implicit none
        type(type_matrix_dense), intent(inout) :: matrix
        integer(int32), intent(in) :: num_nodes
        type(type_matrix_info) :: matrix_info

        if (.not. matrix%is_initialized()) then
            ! 未初期化なら確保 (ゼロクリア含む)
            call matrix%initialize(num_nodes)
        else
            call matrix%get_info(matrix_info)
            if (matrix_info%num_nodes /= num_nodes) then
                ! サイズが変わる場合のみ再確保
                call matrix%destroy()
                call matrix%initialize(num_nodes)
            else
                ! [修正] サイズが同じなら、メモリ解放せず値だけゼロにする
                call matrix%zero()
            end if
        end if
    end subroutine check_initialize_matrix

    subroutine check_initialize_vector(vector, num_nodes)
        implicit none
        type(type_vector_dp), intent(inout) :: vector
        integer(int32), intent(in) :: num_nodes
        integer(int32) :: vec_size

        if (.not. vector%is_initialized()) then
            call vector%initialize(num_nodes)
        else
            vec_size = vector%get_size()
            if (vec_size /= num_nodes) then
                call vector%destroy()
                call vector%initialize(num_nodes)
            else
                ! [修正] 値だけゼロにする
                call vector%zero()
            end if
        end if
    end subroutine check_initialize_vector

    !> Compute local matrices and residual vectors for a specific element.
    module subroutine assemble_local_ftdss(self, workspace, local_J_TT, local_J_TH, &
                                           local_J_HH, local_J_HT, local_R_T, local_R_H)
        implicit none
        class(type_ftdss), intent(inout) :: self
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: local_J_TT, local_J_TH, local_J_HH, local_J_HT
        type(type_vector_dp), intent(inout), optional :: local_R_T, local_R_H

        call self%thermal%assemble_local(controls=self%controls, workspace=workspace, &
                                         J_TT=local_J_TT, J_TH=local_J_TH, R_T=local_R_T)

    end subroutine assemble_local_ftdss

    !> Cleanup assembly variables
    !> [修正] ループ内ではなく、assemble_ftdss の最後に呼ばれることを想定
    module subroutine assemble_finalize_ftdss(self, workspace, local_J_TT, local_J_TH, &
                                              local_J_HH, local_J_HT, local_R_T, local_R_H)
        implicit none
        class(type_ftdss), intent(inout) :: self
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: local_J_TT, local_J_TH, local_J_HH, local_J_HT
        type(type_vector_dp), intent(inout), optional :: local_R_T, local_R_H

        call workspace%destroy()

        if (present(local_J_TT)) call local_J_TT%destroy()
        if (present(local_J_TH)) call local_J_TH%destroy()
        if (present(local_J_HH)) call local_J_HH%destroy()
        if (present(local_J_HT)) call local_J_HT%destroy()
        if (present(local_R_T)) call local_R_T%destroy()
        if (present(local_R_H)) call local_R_H%destroy()
    end subroutine assemble_finalize_ftdss

end submodule ftdss_assemble

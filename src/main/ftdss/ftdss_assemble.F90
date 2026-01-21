submodule(main_ftdss) ftdss_assemble
    implicit none

contains

    !> Perform the global assembly for the FTDSS solver.
    module subroutine assemble_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        type(type_matrix_dense) :: local_K_TT, local_K_TH, local_K_HH, local_K_HT
        type(type_vector_dp) :: local_F_T, local_F_H
        type(type_assemble_workspace) :: workspace

        integer(int32) :: num_elements
        integer(int32), pointer, contiguous, dimension(:) :: p_connectivity => null()
        integer(int32) :: i
        integer(int32) :: thermal_dof, hydraulic_dof

        call self%controls%profiler%start("Assemble")

        call self%K%zero()
        call self%F%zero()

        call self%domain%get_num_elements(num_elements)

        ! --- [重要] 要素ループ ---
        do i = 1, num_elements
            ! 1. 初期化・準備 (Allocateは初回またはサイズ変更時のみ。値はゼロクリアされる)
            call self%assemble_initialize(element_id=i, workspace=workspace, &
                                          local_K_TT=local_K_TT, local_K_TH=local_K_TH, &
                                          local_K_HH=local_K_HH, local_K_HT=local_K_HT, &
                                          local_F_T=local_F_T, local_F_H=local_F_H)

            ! 2. 局所行列の計算
            call self%assemble_local(workspace, local_K_TT, local_K_TH, local_K_HH, local_K_HT, &
                                     local_F_T, local_F_H)

            ! 3. 全体行列への組み込み
            call self%domain%get_element_connectivity(i, p_connectivity)
            call self%domain%get_target_dof(PHYSICS_TYPE_THERMAL, thermal_dof)
            call self%domain%get_target_dof(PHYSICS_TYPE_HYDRAULIC, hydraulic_dof)

            call self%K%add(thermal_dof, thermal_dof, p_connectivity, local_K_TT)
            ! call self%K%add(thermal_dof, hydraulic_dof, p_connectivity, local_K_TH)
            ! call self%K%add(hydraulic_dof, hydraulic_dof, p_connectivity, local_K_HH)
            ! call self%K%add(hydraulic_dof, thermal_dof, p_connectivity, local_K_HT)

            call self%F%add(thermal_dof, p_connectivity, local_F_T)
            ! call self%F%add(hydraulic_dof, p_connectivity, local_F_H)

        end do

        call self%assemble_finalize(workspace, local_K_TT, local_K_TH, &
                                    local_K_HH, local_K_HT, local_F_T, local_F_H)

        call self%controls%profiler%stop("Assemble")

    end subroutine assemble_ftdss

    module subroutine assemble_initialize_ftdss(self, element_id, workspace, local_K_TT, local_K_TH, &
                                                local_K_HH, local_K_HT, local_F_T, local_F_H)
        implicit none
        class(type_ftdss), intent(inout) :: self
        integer(int32), intent(in) :: element_id
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: local_K_TT, local_K_TH, local_K_HH, local_K_HT
        type(type_vector_dp), intent(inout), optional :: local_F_T, local_F_H

        class(abst_fe), pointer :: fe => null()
        integer(int32), pointer, contiguous, dimension(:) :: connectivity => null()
        real(real64), allocatable :: coordinates(:, :)
        integer(int32) :: material_id
        integer(int32) :: computation_type
        integer(int32) :: num_nodes

        integer(int32) :: i

        call self%domain%get_material_id(element_id, material_id)
        call self%domain%get_element(element_id, fe)
        call self%domain%get_element_connectivity(element_id, connectivity)
        call self%domain%get_computation_type(computation_type)
        call self%domain%get_element_coordinate(element_id, coordinates)

        call workspace%initialize(fe, material_id, element_id, computation_type, coordinates, self%controls)

        do i = 1, size(connectivity)
            call self%set_state(connectivity(i), element_id, workspace%state(i))
        end do

        call workspace%lerp()
        do i = 1, workspace%num_fe_gauss
            call self%thermal%update_water_phases(material_id, workspace%state_gp(i))
        end do

        workspace%coordinates = coordinates

        call fe%get_num_nodes(num_nodes)

        if (present(local_K_TT)) call check_initialize_matrix(local_K_TT, num_nodes)
        if (present(local_K_TH)) call check_initialize_matrix(local_K_TH, num_nodes)
        if (present(local_K_HH)) call check_initialize_matrix(local_K_HH, num_nodes)
        if (present(local_K_HT)) call check_initialize_matrix(local_K_HT, num_nodes)

        if (present(local_F_T)) call check_initialize_vector(local_F_T, num_nodes)
        if (present(local_F_H)) call check_initialize_vector(local_F_H, num_nodes)

    end subroutine assemble_initialize_ftdss

    subroutine check_initialize_matrix(matrix, num_nodes)
        implicit none
        type(type_matrix_dense), intent(inout) :: matrix
        integer(int32), intent(in) :: num_nodes
        type(type_matrix_info) :: matrix_info

        if (.not. matrix%is_initialized()) then
            call matrix%initialize(num_nodes)
        else
            call matrix%get_info(matrix_info)
            if (matrix_info%num_nodes /= num_nodes) then
                call matrix%destroy()
                call matrix%initialize(num_nodes)
            else
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
                call vector%zero()
            end if
        end if
    end subroutine check_initialize_vector

    module subroutine assemble_local_ftdss(self, workspace, local_K_TT, local_K_TH, &
                                           local_K_HH, local_K_HT, local_F_T, local_F_H)
        implicit none
        class(type_ftdss), intent(inout) :: self
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: local_K_TT, local_K_TH, local_K_HH, local_K_HT
        type(type_vector_dp), intent(inout), optional :: local_F_T, local_F_H

        call self%thermal%assemble_local(controls=self%controls, workspace=workspace, &
                                         K_TT=local_K_TT, K_TH=local_K_TH, F_T=local_F_T)

    end subroutine assemble_local_ftdss

    module subroutine assemble_finalize_ftdss(self, workspace, local_K_TT, local_K_TH, &
                                              local_K_HH, local_K_HT, local_F_T, local_F_H)
        implicit none
        class(type_ftdss), intent(inout) :: self
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: local_K_TT, local_K_TH, local_K_HH, local_K_HT
        type(type_vector_dp), intent(inout), optional :: local_F_T, local_F_H

        call workspace%destroy()

        if (present(local_K_TT)) call local_K_TT%destroy()
        if (present(local_K_TH)) call local_K_TH%destroy()
        if (present(local_K_HH)) call local_K_HH%destroy()
        if (present(local_K_HT)) call local_K_HT%destroy()
        if (present(local_F_T)) call local_F_T%destroy()
        if (present(local_F_H)) call local_F_H%destroy()
    end subroutine assemble_finalize_ftdss

end submodule ftdss_assemble

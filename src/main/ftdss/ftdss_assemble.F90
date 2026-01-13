submodule(main_ftdss) ftdss_assemble
    implicit none

contains

    !> Perform the global assembly for the FTDSS solver.
    module subroutine assemble_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        type(type_matrix_dense) :: local_J_TT, local_J_TH, local_J_HH, local_J_HT
        type(type_vector_dp) :: local_R_T, local_R_H

        type(type_assemble_workspace) :: workspace

        integer(int32) :: num_elements
        integer(int32), pointer, contiguous, dimension(:) :: p_connectivity
        integer(int32) :: i
        integer(int32) :: thermal_dof, hydraulic_dof

        call self%controls%profiler%start("Assemble")

        call self%J%zero()
        call self%R%zero()

        call self%domain%get_num_elements(num_elements)

        do i = 1, num_elements
            call self%assemble_initialize(element_id=i, workspace=workspace, &
                                          local_J_TT=local_J_TT, local_J_TH=local_J_TH, &
                                          local_J_HH=local_J_HH, local_J_HT=local_J_HT, &
                                          local_R_T=local_R_T, local_R_H=local_R_H)
            call self%assemble_local(workspace, local_J_TT, local_J_TH, local_J_HH, local_J_HT, &
                                     local_R_T, local_R_H)

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
        integer(int32) :: material_id
        integer(int32) :: computation_type
        integer(int32) :: num_nodes
        type(type_matrix_info) :: matrix_info

        integer(int32) :: i

        call self%domain%get_material_id(element_id, material_id)
        call self%domain%get_element(element_id, fe)
        call self%domain%get_element_connectivity(element_id, connectivity)
        call self%domain%get_computation_type(computation_type)

        !!------
        call workspace%initialize(fe, material_id, element_id, computation_type, self%controls)
        do i = 1, size(connectivity)
            call self%set_state(connectivity(i), element_id, workspace%state(i))
        end do
        call workspace%lerp()
        do i = 1, workspace%num_fe_gauss
            call self%thermal%update_water_phases(material_id, workspace%state_gp(i))
        end do
        call self%domain%get_element_coordinate(element_id, workspace%coordinates)
        !!------

        call fe%get_num_nodes(num_nodes)

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
            call matrix%initialize(num_nodes)
        else
            call matrix%get_info(matrix_info)
            if (matrix_info%num_nodes /= num_nodes) then
                call matrix%destroy()
                call matrix%initialize(num_nodes)
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

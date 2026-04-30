submodule(app_ftcms) ftcms_assemble
    implicit none

contains

    module subroutine assemble_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        type(type_matrix_dense) :: local_K_TT, local_K_TH, local_K_HH, local_K_HT
        type(type_vector_dp) :: local_F_T, local_F_H
        type(type_assemble_workspace) :: workspace

        real(real64), allocatable :: elem_coords(:, :)

        integer(int32) :: i_color, i_elem, elem_id
        integer(int32), pointer, contiguous, dimension(:) :: p_connectivity
        integer(int32) :: thermal_dof, hydraulic_dof
        integer(int32) :: num_nodes_local

        integer(int32) :: num_colors, num_elements_in_color
        integer(int32), pointer, contiguous, dimension(:) :: elements_list

        logical :: use_scatter, do_thermal, do_hydraulic

        call self%control%profiler_start(PROFILER_TYPES%ASSEMBLE)

        nullify (p_connectivity)
        nullify (elements_list)

        call self%K%zero()
        call self%F%zero()

        call self%domain%get_num_colors(num_colors)

        do_thermal = self%is_active_thermal()
        if (do_thermal) then
            call self%domain%get_start_dof_index(PHYSICS_TYPES%THERMAL, thermal_dof)
        end if

        do_hydraulic = self%is_active_hydraulic()
        if (do_hydraulic) then
            call self%domain%get_start_dof_index(PHYSICS_TYPES%HYDRAULIC, hydraulic_dof)
        end if

        use_scatter = .true.

        !$OMP PARALLEL IF(do_hydraulic) DEFAULT(NONE) &
        !$OMP SHARED(self, num_colors, elements_list, num_elements_in_color, &
        !$OMP        thermal_dof, hydraulic_dof, use_scatter, do_thermal, do_hydraulic) &
        !$OMP PRIVATE(i_color, i_elem, elem_id, p_connectivity, workspace, &
        !$OMP         local_K_TT, local_K_TH, local_K_HH, local_K_HT, &
        !$OMP         local_F_T, local_F_H, elem_coords, num_nodes_local)

        do i_color = 1, num_colors

            !$OMP SINGLE
            call self%domain%get_colored_elements(i_color, num_elements_in_color, elements_list)
            !$OMP END SINGLE
            !$OMP BARRIER

            if (num_elements_in_color > 0) then
                !$OMP DO SCHEDULE(STATIC)
                do i_elem = 1, num_elements_in_color

                    elem_id = elements_list(i_elem)
                    call self%assemble_initialize(element_id=elem_id, workspace=workspace, &
                                                  local_K_TT=local_K_TT, local_K_TH=local_K_TH, &
                                                  local_K_HH=local_K_HH, local_K_HT=local_K_HT, &
                                                  local_F_T=local_F_T, local_F_H=local_F_H, &
                                                  coordinates=elem_coords, connectivity=p_connectivity)

                    call self%assemble_local(workspace, local_K_TT, local_K_TH, local_K_HH, local_K_HT, &
                                             local_F_T, local_F_H)

                    num_nodes_local = workspace%num_fe_nodes

                    ! $OMP CRITICAL(ftcms_global_assembly)
                    if (do_thermal) then
                        if (use_scatter) then
                            call self%K%add(thermal_dof, thermal_dof, elem_id, num_nodes_local, local_K_TT)
                            if (do_hydraulic) then
                                call self%K%add(thermal_dof, hydraulic_dof, elem_id, num_nodes_local, local_K_TH)
                            end if
                        else
                            call self%K%add(thermal_dof, thermal_dof, p_connectivity, local_K_TT)
                            if (do_hydraulic) then
                                call self%K%add(thermal_dof, hydraulic_dof, p_connectivity, local_K_TH)
                            end if
                        end if
                        call self%F%add(thermal_dof, p_connectivity, local_F_T)
                    end if

                    if (do_hydraulic) then
                        if (use_scatter) then
                            call self%K%add(hydraulic_dof, hydraulic_dof, elem_id, num_nodes_local, local_K_HH)
                            if (do_thermal) then
                                call self%K%add(hydraulic_dof, thermal_dof, elem_id, num_nodes_local, local_K_HT)
                            end if
                        else
                            call self%K%add(hydraulic_dof, hydraulic_dof, p_connectivity, local_K_HH)
                            if (do_thermal) then
                                call self%K%add(hydraulic_dof, thermal_dof, p_connectivity, local_K_HT)
                            end if
                        end if
                        call self%F%add(hydraulic_dof, p_connectivity, local_F_H)
                    end if
                    ! $OMP END CRITICAL(ftcms_global_assembly)

                end do
                !$OMP END DO
            end if

            !$OMP BARRIER

        end do

        call self%assemble_destroy(workspace, local_K_TT, local_K_TH, &
                                   local_K_HH, local_K_HT, local_F_T, local_F_H)
        if (allocated(elem_coords)) deallocate (elem_coords)

        !$OMP END PARALLEL

        call self%control%profiler_stop(PROFILER_TYPES%ASSEMBLE)

    end subroutine assemble_ftcms

    module subroutine assemble_initialize_ftcms(self, element_id, workspace, local_K_TT, local_K_TH, &
                                                local_K_HH, local_K_HT, local_F_T, local_F_H, &
                                                coordinates, connectivity)
        implicit none

        class(type_ftcms), intent(inout) :: self
        integer(int32), intent(in) :: element_id
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: local_K_TT, local_K_TH, local_K_HH, local_K_HT
        type(type_vector_dp), intent(inout), optional :: local_F_T, local_F_H
        real(real64), allocatable, intent(inout) :: coordinates(:, :)
        integer(int32), pointer, contiguous, intent(inout), optional :: connectivity(:)

        class(abst_fe), pointer :: fe
        integer(int32), pointer, contiguous, dimension(:) :: connectivity_local

        integer(int32) :: material_id
        type(type_constant_id), pointer :: computation_type
        integer(int32) :: num_nodes

        nullify (fe)
        nullify (connectivity_local)

        call self%domain%get_material_id(element_id, material_id)
        call self%domain%get_fe(element_id, fe)
        call self%domain%get_fe_connectivity(element_id, connectivity_local)
        call self%domain%get_computation_type(computation_type)

        call self%domain%get_fe_coordinate(element_id, coordinates)

        call workspace%initialize(fe, material_id, element_id, computation_type%ID, coordinates, self%control)

        call self%set_states_from_connectivity(connectivity_local, element_id, workspace%state, calc_physics=.false.)

        call workspace%lerp()

        call self%update_physical_properties_bulk(material_id, workspace%state_gp)
        call fe%get_num_nodes(num_nodes)

        if (present(local_K_TT)) call check_initialize_matrix(local_K_TT, num_nodes)
        if (present(local_K_TH)) call check_initialize_matrix(local_K_TH, num_nodes)
        if (present(local_K_HH)) call check_initialize_matrix(local_K_HH, num_nodes)
        if (present(local_K_HT)) call check_initialize_matrix(local_K_HT, num_nodes)
        if (present(local_F_T)) call check_initialize_vector(local_F_T, num_nodes)
        if (present(local_F_H)) call check_initialize_vector(local_F_H, num_nodes)

        if (present(connectivity)) then
            connectivity => connectivity_local
        end if

    end subroutine assemble_initialize_ftcms

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

    module subroutine assemble_local_ftcms(self, workspace, local_K_TT, local_K_TH, &
                                           local_K_HH, local_K_HT, local_F_T, local_F_H)

        implicit none
        class(type_ftcms), intent(inout) :: self
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: local_K_TT, local_K_TH, local_K_HH, local_K_HT
        type(type_vector_dp), intent(inout), optional :: local_F_T, local_F_H

        if (self%is_active_thermal()) then
            call self%thermal%assemble_local(control=self%control, workspace=workspace, &
                                             K_TT=local_K_TT, K_TH=local_K_TH, F_T=local_F_T)
        end if

        if (self%is_active_hydraulic()) then
            call self%hydraulic%assemble_local(control=self%control, workspace=workspace, &
                                               K_HH=local_K_HH, K_HT=local_K_HT, F_H=local_F_H)
        end if

    end subroutine assemble_local_ftcms

    module subroutine assemble_destroy_ftcms(self, workspace, local_K_TT, local_K_TH, &
                                             local_K_HH, local_K_HT, local_F_T, local_F_H)
        implicit none
        class(type_ftcms), intent(inout) :: self
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
    end subroutine assemble_destroy_ftcms

end submodule ftcms_assemble

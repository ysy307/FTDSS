submodule(app_ftcms) ftcms_assemble
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
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
        integer(int32) :: i_local
        integer(int32), pointer, contiguous, dimension(:) :: p_connectivity
        integer(int32) :: thermal_dof, hydraulic_dof
        integer(int32) :: num_nodes_local

        integer(int32) :: num_colors, num_elements_in_color
        integer(int32), pointer, contiguous, dimension(:) :: elements_list
        real(real64), pointer :: local_matrix_vals(:, :)
        real(real64), pointer :: local_vector_vals(:)
        real(real64), pointer :: pre_bc_diag_data(:)
        real(real64) :: local_diag_sum, local_tt_diag_sum, local_hh_diag_sum, local_h_scale
        class(abst_matrix), pointer :: K_ptr
        type(type_vector_dp) :: pre_bc_diag_vec

        integer(int32) :: probe_tt_elems, probe_hh_elems
        integer(int32) :: probe_tt_zero_diag_elems, probe_hh_zero_diag_elems
        integer(int32) :: probe_hh_scaled_elems
        real(real64) :: probe_tt_diag_abs_sum, probe_hh_diag_abs_sum
        real(real64) :: probe_hh_scale_sum, probe_hh_scale_min, probe_hh_scale_max
        real(real64), parameter :: probe_diag_eps = 1.0d-20
        real(real64), parameter :: hydraulic_scale_eps = 1.0d-30
        real(real64), parameter :: hydraulic_scale_max = 1.0d6

        logical :: use_scatter, do_hydraulic

        call self%control%profiler_start(PROFILER_TYPES%ASSEMBLE)

        nullify (p_connectivity)
        nullify (elements_list)
        nullify (local_matrix_vals)
        nullify (local_vector_vals)
        nullify (pre_bc_diag_data)
        nullify (K_ptr)

        call self%K%zero()
        call self%F%zero()

        call self%domain%get_num_colors(num_colors)
        call self%domain%get_start_dof_index(PHYSICS_TYPES%THERMAL, thermal_dof)

        do_hydraulic = self%is_active_hydraulic()
        if (do_hydraulic) then
            call self%domain%get_start_dof_index(PHYSICS_TYPES%HYDRAULIC, hydraulic_dof)
        end if

        probe_tt_elems = 0
        probe_hh_elems = 0
        probe_tt_zero_diag_elems = 0
        probe_hh_zero_diag_elems = 0
        probe_hh_scaled_elems = 0
        probe_tt_diag_abs_sum = 0.0d0
        probe_hh_diag_abs_sum = 0.0d0
        probe_hh_scale_sum = 0.0d0
        probe_hh_scale_min = huge(1.0d0)
        probe_hh_scale_max = 1.0d0

        use_scatter = .true.

        !$OMP PARALLEL DEFAULT(NONE) &
        !$OMP SHARED(self, num_colors, elements_list, num_elements_in_color, &
        !$OMP        thermal_dof, hydraulic_dof, use_scatter, do_hydraulic) &
        !$OMP PRIVATE(i_color, i_elem, elem_id, p_connectivity, workspace, &
        !$OMP         local_K_TT, local_K_TH, local_K_HH, local_K_HT, &
        !$OMP         local_F_T, local_F_H, elem_coords, num_nodes_local, local_matrix_vals, local_vector_vals, &
        !$OMP         local_diag_sum, local_tt_diag_sum, local_hh_diag_sum, local_h_scale, i_local) &
        !$OMP REDUCTION(+:probe_tt_elems, probe_hh_elems, probe_tt_zero_diag_elems, probe_hh_zero_diag_elems, probe_hh_scaled_elems, &
        !$OMP&            probe_tt_diag_abs_sum, probe_hh_diag_abs_sum, probe_hh_scale_sum) &
        !$OMP REDUCTION(MIN:probe_hh_scale_min) REDUCTION(MAX:probe_hh_scale_max)

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

                    local_diag_sum = 0.0d0
                    local_matrix_vals => local_K_TT%get_val()
                    if (associated(local_matrix_vals)) then
                        do i_local = 1, workspace%num_fe_nodes
                            local_diag_sum = local_diag_sum + abs(local_matrix_vals(i_local, i_local))
                        end do
                        probe_tt_elems = probe_tt_elems + 1
                        probe_tt_diag_abs_sum = probe_tt_diag_abs_sum + local_diag_sum
                        if (local_diag_sum < probe_diag_eps) probe_tt_zero_diag_elems = probe_tt_zero_diag_elems + 1
                    end if
                    local_tt_diag_sum = local_diag_sum

                    if (do_hydraulic) then
                        local_diag_sum = 0.0d0
                        local_matrix_vals => local_K_HH%get_val()
                        if (associated(local_matrix_vals)) then
                            do i_local = 1, workspace%num_fe_nodes
                                local_diag_sum = local_diag_sum + abs(local_matrix_vals(i_local, i_local))
                            end do
                            probe_hh_elems = probe_hh_elems + 1
                            probe_hh_diag_abs_sum = probe_hh_diag_abs_sum + local_diag_sum
                            if (local_diag_sum < probe_diag_eps) probe_hh_zero_diag_elems = probe_hh_zero_diag_elems + 1
                        end if
                        local_hh_diag_sum = local_diag_sum

                        local_h_scale = 1.0d0
                        if (local_hh_diag_sum > hydraulic_scale_eps .and. local_tt_diag_sum > probe_diag_eps) then
                            local_h_scale = sqrt(local_tt_diag_sum / local_hh_diag_sum)
                        end if

                        if (.not. ieee_is_finite(local_h_scale) .or. local_h_scale < 1.0d0) local_h_scale = 1.0d0
                        if (local_h_scale > hydraulic_scale_max) local_h_scale = hydraulic_scale_max

                        if (local_h_scale > 1.0d0 + 1.0d-12) then
                            local_matrix_vals => local_K_HH%get_val()
                            if (associated(local_matrix_vals)) local_matrix_vals(:, :) = local_h_scale * local_matrix_vals(:, :)
                            local_vector_vals => local_F_H%get_data()
                            if (associated(local_vector_vals)) local_vector_vals(:) = local_h_scale * local_vector_vals(:)
                            nullify (local_vector_vals)

                            probe_hh_scaled_elems = probe_hh_scaled_elems + 1
                            probe_hh_scale_sum = probe_hh_scale_sum + local_h_scale
                            probe_hh_scale_min = min(probe_hh_scale_min, local_h_scale)
                            probe_hh_scale_max = max(probe_hh_scale_max, local_h_scale)
                        end if
                    end if
                    nullify (local_matrix_vals)

                    num_nodes_local = workspace%num_fe_nodes

                    if (use_scatter) then
                        call self%K%add(thermal_dof, thermal_dof, elem_id, num_nodes_local, local_K_TT)
                    else
                        call self%K%add(thermal_dof, thermal_dof, p_connectivity, local_K_TT)
                    end if
                    call self%F%add(thermal_dof, p_connectivity, local_F_T)

                    if (do_hydraulic) then
                        if (use_scatter) then
                            call self%K%add(hydraulic_dof, hydraulic_dof, elem_id, num_nodes_local, local_K_HH)
                        else
                            call self%K%add(hydraulic_dof, hydraulic_dof, p_connectivity, local_K_HH)
                        end if
                        call self%F%add(hydraulic_dof, p_connectivity, local_F_H)
                    end if

                end do
                !$OMP END DO
            end if

            !$OMP BARRIER

        end do

        call self%assemble_destroy(workspace, local_K_TT, local_K_TH, &
                                   local_K_HH, local_K_HT, local_F_T, local_F_H)
        if (allocated(elem_coords)) deallocate (elem_coords)

        !$OMP END PARALLEL

        write (*, '(A,2(I0,A),ES13.5)') '   [DEBUG] Local K_TT diag probe: elems=', probe_tt_elems, &
            ', zero_diag_elems=', probe_tt_zero_diag_elems, ', diag_abs_sum=', probe_tt_diag_abs_sum
        if (do_hydraulic) then
            write (*, '(A,2(I0,A),ES13.5)') '   [DEBUG] Local K_HH diag probe: elems=', probe_hh_elems, &
                ', zero_diag_elems=', probe_hh_zero_diag_elems, ', diag_abs_sum=', probe_hh_diag_abs_sum
            if (probe_hh_scaled_elems > 0) then
                write (*, '(A,I0,A,3(ES13.5,A))') '   [DEBUG] Hydraulic row scaling: scaled_elems=', probe_hh_scaled_elems, &
                    ', min=', probe_hh_scale_min, ', max=', probe_hh_scale_max, &
                    ', avg=', probe_hh_scale_sum/real(probe_hh_scaled_elems, real64), ''
            else
                write (*, '(A)') '   [DEBUG] Hydraulic row scaling: scaled_elems=0'
            end if
        end if

        K_ptr => self%K%get_matrix()
        if (associated(K_ptr)) then
            call pre_bc_diag_vec%initialize(self%K%get_size())
            call pre_bc_diag_vec%zero()
            call K_ptr%get_diagonal(pre_bc_diag_vec)
            pre_bc_diag_data => pre_bc_diag_vec%get_data()
            if (associated(pre_bc_diag_data)) then
                write (*, '(A,I0)') '   [DEBUG] K diag zeros (pre-bc): ', count(abs(pre_bc_diag_data) < probe_diag_eps)
            end if
            nullify (pre_bc_diag_data)
            call pre_bc_diag_vec%destroy()
        end if
        nullify (K_ptr)

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

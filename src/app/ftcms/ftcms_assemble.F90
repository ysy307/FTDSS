submodule(app_ftcms) ftcms_assemble
    implicit none

    !> Build the thermal-pressure coupling block by differencing the element
    !> thermal residual instead of using the hand-written storage tangent.
    !>
    !> The hand-written block is bdf0*K1(dU/dp) only. Audited against the
    !> element finite-difference tangent it is 18 times too small in the far
    !> field and 1798 times too small at the freezing front (relative Frobenius
    !> error 1.0, i.e. the assembled block is negligible against the true one),
    !> while TT, HT and HH agree to 1e-4. The omitted physics is the pressure
    !> dependence of the advective heat flux: water carries heat at a Darcy
    !> velocity containing -K_wP*grad p. With the coupling block that wrong, the
    !> pressure increment is over-predicted by the same factor, which is what
    !> drove the hydraulic residual to 1e3 times its step-initial value and left
    !> the line search with no admissible step.
    logical, parameter :: COUPLING_TH_FINITE_DIFFERENCE = .true.

    !> Solve the hydraulic block in the total potential rather than in the pore
    !> pressure.
    !>
    !> Measured over a failing nonlinear loop, max|du_p| ran at 1e5 to 3e6 Pa
    !> against an ambient -5.4e4 Pa, and the ratio du_p/du_T sat at about
    !> 1.0e6 Pa/K every iteration - which is d(psi_cryo)/dT = 1.22e6 Pa/K, the
    !> generalized Clapeyron slope. The pressure increment is therefore not a
    !> runaway but the equilibrium response: cooling a node by dT lowers the
    !> liquid pressure by 1.22e6 dT. In the pore pressure that stiffness is
    !> carried by the unknown itself, so the linearization is valid over a tiny
    !> fraction of the step. In the total potential P_gen = p - psi_cryo(T) the
    !> same state has du_g of ordinary size.
    !>
    !> Substituting du_p = du_g + c du_T, c = d(psi_cryo)/dT, adds K_TH c to
    !> K_TT and K_HH c to K_HT. Since the flux contributes -K2(D_HH) c to K_HT
    !> and K2(D_HH) to K_HH, that term cancels exactly - which is why the
    !> reference formulation solves for the total head.
    logical, parameter :: TOTAL_POTENTIAL_UNKNOWN = .false.

    !> Overwrite the Gauss-point dQi/dT with the node-interpolated value.
    !>
    !> update_water_phases computes dQi/dT consistently at the Gauss point from
    !> that point's own (T, p); replacing it by an interpolation of the nodal
    !> values makes the apparent heat capacity in K_TT a different number from
    !> the derivative of the enthalpy the residual integrates at that point.
    logical, parameter :: LERP_NODAL_DQI_DT = .true.

contains

    module subroutine assemble_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        type(type_matrix_dense) :: local_K_TT, local_K_TH, local_K_HH, local_K_HT
        type(type_vector_dp) :: local_F_T, local_F_H
        type(type_assemble_workspace) :: workspace

        real(real64), allocatable :: elem_coords(:, :)
        real(real64), allocatable :: raw_elem_coords(:, :)

        integer(int32) :: i_color, i_elem, elem_id
        integer(int32), pointer, contiguous, dimension(:) :: p_connectivity
        integer(int32) :: thermal_dof, hydraulic_dof
        integer(int32) :: num_nodes_local

        integer(int32) :: num_colors, num_elements_in_color
        integer(int32), pointer, contiguous, dimension(:) :: elements_list

        logical :: use_scatter, do_thermal, do_hydraulic
        logical :: do_thermal_elem, do_hydraulic_elem

        call self%control%profiler_start(PROFILER_TYPES%ASSEMBLE)

        nullify (p_connectivity)
        nullify (elements_list)

        call self%K%zero()
        call self%F%zero()

        if (TOTAL_POTENTIAL_UNKNOWN) then
            block
                integer(int32) :: num_nodes_total
                call self%domain%get_num_nodes(num_nodes_total)
                if (.not. allocated(self%cryo_slope)) allocate (self%cryo_slope(num_nodes_total))
                if (size(self%cryo_slope) /= num_nodes_total) then
                    deallocate (self%cryo_slope)
                    allocate (self%cryo_slope(num_nodes_total))
                end if
                self%cryo_slope(:) = 0.0d0
            end block
        end if

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

        !$OMP PARALLEL IF(do_hydraulic .or. do_thermal) DEFAULT(NONE) &
        !$OMP SHARED(self, num_colors, elements_list, num_elements_in_color, &
        !$OMP        thermal_dof, hydraulic_dof, use_scatter, do_thermal, do_hydraulic) &
        !$OMP PRIVATE(i_color, i_elem, elem_id, p_connectivity, workspace, &
        !$OMP         local_K_TT, local_K_TH, local_K_HH, local_K_HT, &
        !$OMP         local_F_T, local_F_H, elem_coords, raw_elem_coords, num_nodes_local, &
        !$OMP         do_thermal_elem, do_hydraulic_elem)

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
                                                  coordinates=elem_coords, raw_coordinates=raw_elem_coords, &
                                                  connectivity=p_connectivity)

                    do_thermal_elem = self%control%is_target(PHYSICS_TYPES%THERMAL, workspace%material_id)
                    do_hydraulic_elem = self%control%is_target(PHYSICS_TYPES%HYDRAULIC, workspace%material_id)

                    call self%assemble_local(workspace, local_K_TT, local_K_TH, local_K_HH, local_K_HT, &
                                             local_F_T, local_F_H)

                    if (COUPLING_TH_FINITE_DIFFERENCE .and. do_thermal_elem .and. do_hydraulic_elem) then
                        call self%assemble_coupling_fd(workspace, local_F_T, local_K_TH)
                    end if

                    if (TOTAL_POTENTIAL_UNKNOWN .and. do_thermal_elem .and. do_hydraulic_elem) then
                        call transform_to_total_potential(self, workspace, &
                                                          local_K_TT, local_K_TH, local_K_HH, local_K_HT)
                    end if

                    num_nodes_local = workspace%num_fe_nodes

                    ! $OMP CRITICAL(ftcms_global_assembly)
                    if (do_thermal_elem) then
                        call self%K%add(PHYSICS_TYPES%THERMAL%ID, PHYSICS_TYPES%THERMAL%ID, elem_id, num_nodes_local, local_K_TT)
                        call self%F%add(PHYSICS_TYPES%THERMAL%ID, p_connectivity, local_F_T)
                    end if

                    if (do_hydraulic_elem) then
                        call self%K%add(PHYSICS_TYPES%HYDRAULIC%ID, PHYSICS_TYPES%HYDRAULIC%ID, &
                                        elem_id, num_nodes_local, local_K_HH)
                        call self%F%add(PHYSICS_TYPES%HYDRAULIC%ID, p_connectivity, local_F_H)
                    end if

                    if (do_thermal_elem .and. do_hydraulic_elem) then
                        call self%K%add(PHYSICS_TYPES%THERMAL%ID, PHYSICS_TYPES%HYDRAULIC%ID, elem_id, num_nodes_local, local_K_TH)
                        call self%K%add(PHYSICS_TYPES%HYDRAULIC%ID, PHYSICS_TYPES%THERMAL%ID, elem_id, num_nodes_local, local_K_HT)
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
        if (allocated(raw_elem_coords)) deallocate (raw_elem_coords)

        !$OMP END PARALLEL

        call self%control%profiler_stop(PROFILER_TYPES%ASSEMBLE)

    end subroutine assemble_ftcms

    module subroutine assemble_initialize_ftcms(self, element_id, workspace, local_K_TT, local_K_TH, &
                                                local_K_HH, local_K_HT, local_F_T, local_F_H, &
                                                coordinates, raw_coordinates, connectivity)
        implicit none

        class(type_ftcms), intent(inout) :: self
        integer(int32), intent(in) :: element_id
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout), optional :: local_K_TT, local_K_TH, local_K_HH, local_K_HT
        type(type_vector_dp), intent(inout), optional :: local_F_T, local_F_H
        real(real64), allocatable, intent(inout) :: coordinates(:, :)
        real(real64), allocatable, intent(inout) :: raw_coordinates(:, :)
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

        call self%domain%get_fe_coordinate(element_id, coordinates, raw_coordinates=raw_coordinates)

        call workspace%initialize(fe, material_id, element_id, computation_type%ID, coordinates, self%control)

        call self%set_states_from_connectivity(connectivity_local, element_id, workspace%state, calc_physics=.true.)

        call workspace%lerp()

        call self%update_physical_properties_bulk(material_id, workspace%state_gp)

        if (LERP_NODAL_DQI_DT) call workspace%lerp_dqi_dt()

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
        logical :: do_thermal, do_hydraulic

        do_thermal = self%control%is_target(PHYSICS_TYPES%THERMAL, workspace%material_id)
        do_hydraulic = self%control%is_target(PHYSICS_TYPES%HYDRAULIC, workspace%material_id)

        if (do_thermal) then
            if (do_hydraulic .and. present(local_K_TH)) then
                call self%thermal%assemble_local(control=self%control, workspace=workspace, &
                                                 K_TT=local_K_TT, K_TH=local_K_TH, F_T=local_F_T)
            else
                call self%thermal%assemble_local(control=self%control, workspace=workspace, &
                                                 K_TT=local_K_TT, F_T=local_F_T)
            end if
        end if

        if (do_hydraulic) then
            if (do_thermal .and. present(local_K_HT)) then
                call self%hydraulic%assemble_local(control=self%control, workspace=workspace, &
                                                   K_HH=local_K_HH, K_HT=local_K_HT, F_H=local_F_H)
            else
                call self%hydraulic%assemble_local(control=self%control, workspace=workspace, &
                                                   K_HH=local_K_HH, F_H=local_F_H)
            end if
        end if

    end subroutine assemble_local_ftcms

    !> Apply the column substitution du_p = du_g + c du_T to one element.
    subroutine transform_to_total_potential(self, workspace, K_TT, K_TH, K_HH, K_HT)
        implicit none
        class(type_ftcms), intent(inout) :: self
        type(type_assemble_workspace), intent(inout) :: workspace
        type(type_matrix_dense), intent(inout) :: K_TT, K_TH, K_HH, K_HT

        real(real64), pointer :: TT(:, :), TH(:, :), HH(:, :), HT(:, :)
        real(real64) :: slope(workspace%num_fe_nodes), dh_dT
        integer(int32) :: i, j, n_nodes, node_id
        integer(int32), pointer, contiguous :: connectivity(:)

        n_nodes = workspace%num_fe_nodes
        nullify (connectivity)
        call self%domain%get_fe_connectivity(workspace%element_id, connectivity)

        do j = 1, n_nodes
            call self%hydraulic%calc_cryo_head_dT(workspace%material_id, workspace%state(j), dh_dT)
            ! dPgen/dT = rho_std g dh/dT and c = d psi_cryo/dT = -dPgen/dT.
            slope(j) = -rho_std * g * dh_dT
            if (associated(connectivity)) then
                node_id = connectivity(j)
                if (allocated(self%cryo_slope)) then
                    if (node_id >= 1 .and. node_id <= size(self%cryo_slope)) self%cryo_slope(node_id) = slope(j)
                end if
            end if
        end do

        nullify (TT, TH, HH, HT)
        TT => K_TT%get_val()
        TH => K_TH%get_val()
        HH => K_HH%get_val()
        HT => K_HT%get_val()
        do j = 1, n_nodes
            if (slope(j) == 0.0d0) cycle
            do i = 1, n_nodes
                TT(i, j) = TT(i, j) + TH(i, j) * slope(j)
                HT(i, j) = HT(i, j) + HH(i, j) * slope(j)
            end do
        end do
        nullify (TT, TH, HH, HT)
    end subroutine transform_to_total_potential

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

submodule(app_ftcms) ftcms_base
    use :: core_types_topology_system_topology, only:type_system_topology
    use :: module_linalg, only:vector_norm2
    implicit none
contains

    module subroutine initialize_type_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self
        type(type_input), save :: input
        type(type_ic_manager) :: ic

        integer(int32) :: max_bdf_order
        integer(int32), allocatable :: active_region_ids(:)
        integer(int32) :: num_nodes
        integer(int32) :: i
        integer(int32) :: computation_dimension
        integer(int32) :: num_total_dofs
        integer(int32) :: ierr
        integer(int32) :: solver_type_selected, preconditioner_type_selected, m_restart_selected
        integer(int32) :: projection_offset_selected, projection_stride_selected
        logical :: projection_enabled_selected
        real(real64), pointer, contiguous, dimension(:) :: phase_values

        type(type_config_control_manager) :: config_control_manager
        type(type_config_iteration) :: config_iteration
        type(type_config_time) :: config_time
        type(type_config_time_ats) :: config_time_ats
        type(type_config_output_manager) :: config_output_field
        type(type_config_output_manager) :: config_output_history
        type(type_config_acceleration) :: config_acceleration
        type(type_config_parallel_openmp) :: config_parallel_openmp

        type(type_config_ic) :: configs_ic(IC_TARGETS%NUM_ID)

        type(type_config_nodes) :: config_nodes
        type(type_config_elements) :: config_elements
        type(type_config_multicoloring) :: config_multicoloring
        type(type_config_elements), allocatable :: config_boundary_elements(:)

        type(type_solver_settings) :: solver_info
        type(type_preconditioner_settings) :: pc_info

        type(type_config_output) :: config_output
        type(type_config_observation) :: config_observation
        type(type_config_overall) :: config_overall

        type(type_config_bc), allocatable :: config_bcs(:)

        call self%control%initialize()
        call self%control%profiler_record(TIME_RECORDS%START)
        call self%control%profiler_start(PROFILER_TYPES%TOTAL)

        ! call setup_handler()

        call self%control%profiler_start(PROFILER_TYPES%IO)
        call input%initialize()
        call self%control%profiler_stop(PROFILER_TYPES%IO)

        call input_translator%execute(input, config_control_manager)
        call input_translator%execute(input, config_iteration)
        call input_translator%execute(input, config_time)
        call input_translator%execute(input, config_time_ats)
        call input_translator%execute(input, OUTPUT_TYPES%FIELD, config_output_field)
        call input_translator%execute(input, OUTPUT_TYPES%HISTORY, config_output_history)
        call input_translator%execute(input, config_acceleration)
        call input_translator%execute(input, config_parallel_openmp)

        call self%control%initialize(config_control_manager, config_iteration, config_time, config_time_ats, &
                                     config_output_field, config_output_history, &
                                     config_acceleration, config_parallel_openmp)

        if (self%is_active_thermal()) then
            call input_translator%execute(input, PHYSICS_TYPES%THERMAL, config_bcs)
            call self%bc(PHYSICS_TYPES%THERMAL%ID)%initialize(config_bcs)
            if (allocated(config_bcs)) deallocate (config_bcs)
        end if

        if (self%is_active_hydraulic()) then
            call input_translator%execute(input, PHYSICS_TYPES%HYDRAULIC, config_bcs)
            call self%bc(PHYSICS_TYPES%HYDRAULIC%ID)%initialize(config_bcs)
            if (allocated(config_bcs)) deallocate (config_bcs)
        end if

        call input_translator%execute(input, IC_TARGETS%POROSITY, configs_ic(IC_TARGETS%POROSITY%ID))
        call input_translator%execute(input, IC_TARGETS%THERMAL, configs_ic(IC_TARGETS%THERMAL%ID))
        call input_translator%execute(input, IC_TARGETS%HYDRAULIC, configs_ic(IC_TARGETS%HYDRAULIC%ID))
        call input_translator%execute(input, IC_TARGETS%MECHANICAL, configs_ic(IC_TARGETS%MECHANICAL%ID))
        call ic%initialize(configs_ic)

        if (input%output_settings%standard_output%print_progress) then
            call global_logger%configure(level=information_level, &
                                         time_stamp=.true., &
                                         max_width=0)
        else
            call global_logger%configure(level=warning_level, &
                                         time_stamp=.true., &
                                         max_width=0)
        end if

        call input_translator%execute(input, config_nodes)
        call input_translator%execute(input, config_elements, config_multicoloring)
        call input_translator%execute(input, config_boundary_elements)
        call self%domain%initialize(config_nodes, config_elements, config_multicoloring, config_boundary_elements, &
                                    input%basic%simulation_settings%calculate_type, config_control_manager%coupling_mode, &
                                    config_control_manager%compute_active)

        call self%domain%get_start_dof_index(PHYSICS_TYPES%THERMAL, self%thermal_start_dof)
        call self%domain%get_start_dof_index(PHYSICS_TYPES%HYDRAULIC, self%hydraulic_start_dof)

        call self%domain%get_total_dofs(num_total_dofs)
        call self%domain%get_num_nodes(num_nodes)

        ! A1 prototype closure flag (see ftcms_interface.F90 field doc). Read
        ! directly here (usage site), same pattern as enable_fringe_K_averaging
        ! but propagated to type_ftcms instead of type_hydraulic since the
        ! constraint/prognostic-update logic lives entirely at the app level.
        self%enable_clapeyron_pressure_constraint = input%basic%analysis_controls%enable_clapeyron_pressure_constraint

        block
            ! Domain-independent carrier injected into the system layer, so the
            ! Jacobian / residual no longer depend on type_domain directly.
            type(type_system_topology) :: topology

            call self%domain%export_topology(topology)
            call self%K%initialize(topology, config_control_manager%coupling_mode)
            call self%K%build_scatter_map(topology)
            call self%F%initialize(topology, config_control_manager%coupling_mode)
            call self%du%initialize(topology, config_control_manager%coupling_mode)
        end block

        max_bdf_order = input%basic%solver_settings%bdf_order
        call self%porosity%initialize(num_nodes, max_bdf_order)
        call ic%apply(IC_TARGETS%POROSITY, self%porosity)

        if (self%is_active_thermal()) then
            call self%temperature%initialize(num_nodes, max_bdf_order)
            call ic%apply(IC_TARGETS%THERMAL, self%temperature)
        end if

        if (self%is_active_hydraulic()) then
            call self%pressure%initialize(num_nodes, max_bdf_order)
            call ic%apply(IC_TARGETS%HYDRAULIC, self%pressure)
        end if

        call self%Qw%initialize(num_nodes, max_bdf_order)
        call self%Qi%initialize(num_nodes, max_bdf_order)
        call self%Qa%initialize(num_nodes, max_bdf_order)
        call self%Qv%initialize(num_nodes, max_bdf_order)

        ! A1 prototype closure state (see enable_clapeyron_pressure_constraint).
        ! Allocated unconditionally (cheap, num_nodes-sized) so downstream code
        ! can rely on these arrays being allocated whenever the flag might later
        ! be toggled by a call site; the lumped node volume is only worth its
        ! O(N_elements) cost when the closure is actually active.
        if (allocated(self%clapeyron_frozen_mask)) deallocate (self%clapeyron_frozen_mask)
        if (allocated(self%clapeyron_R_H_raw)) deallocate (self%clapeyron_R_H_raw)
        if (allocated(self%clapeyron_node_volume)) deallocate (self%clapeyron_node_volume)
        allocate (self%clapeyron_frozen_mask(num_nodes), source=.false.)
        allocate (self%clapeyron_R_H_raw(num_nodes), source=0.0d0)
        allocate (self%clapeyron_node_volume(num_nodes), source=0.0d0)
        if (self%enable_clapeyron_pressure_constraint) then
            call compute_clapeyron_node_volume(self)
        end if

        call self%domain%get_computation_dimension(computation_dimension)
        call input%geometry%vtk%get_active_region_info(active_region_ids, target_dim=computation_dimension)

        call self%thermal%initialize(input, active_region_ids)
        call self%hydraulic%initialize(input, active_region_ids)

        ! Apply initial Dirichlet boundary conditions to field variables
        call self%apply_bc()

        ! Initialize solver strictly from input settings.
        associate (linear_solver_settings => input%basic%solver_settings%linear_solver)
            solver_type_selected = linear_solver_settings%solver_type
            preconditioner_type_selected = linear_solver_settings%preconditioner_type
            m_restart_selected = linear_solver_settings%m_restarts
            projection_enabled_selected = .false.
            projection_offset_selected = 0
            projection_stride_selected = 0

            write (*, '(A,I0,A,I0)') 'Notice: linear solver type=', solver_type_selected, &
                ', preconditioner type=', preconditioner_type_selected

            ! Mean-zero nullspace projection is never enabled automatically: the
            ! transient hydraulic block is nonsingular (BDF storage term with
            ! C_eq >= L-scheme capacity > 0), and projecting a nonsingular
            ! system discards the global water-mass balance (the mean residual
            ! component) every Krylov iteration, biasing the solution.
            block
                integer(int32) :: solver_size
                if (self%control%is_staggered()) then
                    solver_size = num_nodes
                else
                    solver_size = num_total_dofs
                end if
                call solver_info%set(solver_type_selected, &
                                     solver_size, &
                                     linear_solver_settings%tolerance, &
                                     linear_solver_settings%max_iterations, &
                                     m_restart_selected, &
                                     projection_enabled=projection_enabled_selected, &
                                     projection_offset=projection_offset_selected, &
                                     projection_stride=projection_stride_selected)
            end block
            if (.not. self%control%is_staggered() .and. &
                (preconditioner_type_selected == PRECONDITIONER_TYPES%ILU%ID .or. &
                 preconditioner_type_selected == PRECONDITIONER_TYPES%ILUT%ID .or. &
                 preconditioner_type_selected == PRECONDITIONER_TYPES%SAAMG%ID)) then
                block
                    integer(int32) :: dofs_per_node_local
                    call self%K%get_num_dofs_per_node(dofs_per_node_local)
                    call pc_info%set(preconditioner_type_selected, num_nodes, dofs_per_node_local, &
                                     amg_strength_threshold=linear_solver_settings%amg_strength_threshold, &
                                     amg_smoother_sweeps=linear_solver_settings%amg_smoother_sweeps, &
                                     amg_max_agg_size=linear_solver_settings%amg_max_agg_size, &
                                     amg_drop_tolerance=linear_solver_settings%amg_drop_tolerance, &
                                     amg_drop_strategy=linear_solver_settings%amg_drop_strategy, &
                                     amg_smoother_type=linear_solver_settings%amg_smoother_type, &
                                     amg_rebuild_frequency=linear_solver_settings%amg_rebuild_frequency, &
                                     amg_rebuild_threshold=linear_solver_settings%amg_rebuild_threshold)
                end block
            else
                call pc_info%set(preconditioner_type_selected, &
                                 merge(num_nodes, num_total_dofs, self%control%is_staggered()))
            end if
            call create_solver(self%solver, solver_info, pc_info, ierr)

            if (self%is_active_thermal()) then
                if (self%control%is_staggered()) then
                    call pc_info%set(preconditioner_type_selected, num_nodes)
                    call solver_info%set(solver_type_selected, num_nodes, &
                                         linear_solver_settings%tolerance, &
                                         linear_solver_settings%max_iterations, &
                                         m_restart_selected, &
                                         relative_tolerance=5.0d-2)
                else
                    call pc_info%set(PRECONDITIONER_TYPES%JACOBI%ID, num_total_dofs)
                end if
                call create_solver(self%solver_thermal, solver_info, pc_info, ierr)
            end if
        end associate

        ! Capture initial mean pressure for steady-state all-Neumann null-mode anchoring only.
        if (self%is_active_hydraulic() .and. (.not. self%hydraulic_has_dirichlet_bc) &
            .and. projection_enabled_selected) then
            nullify (phase_values)
            call self%pressure%get_current(phase_values)
            if (associated(phase_values) .and. size(phase_values) > 0) then
                self%hydraulic_ref_mean = sum(phase_values) / real(size(phase_values), real64)
                self%hydraulic_ref_mean_set = .true.
            end if
            nullify (phase_values)
        end if

        ! Populate initial phase variables from initial T/P/porosity before first output.
        call self%update_variables()
        ! Sync previous-step values with IC so the BDF transient term is zero at t=0.
        ! Only active-physics state variables are initialized (see guarded init above);
        ! inactive ones are not retained, matching the is_active_* gating used throughout.
        nullify (phase_values)
        if (self%is_active_thermal()) then
            call self%temperature%get_current(phase_values)
            if (associated(phase_values)) call self%temperature%set_previous(phase_values)
            nullify (phase_values)
        end if
        if (self%is_active_hydraulic()) then
            call self%pressure%get_current(phase_values)
            if (associated(phase_values)) call self%pressure%set_previous(phase_values)
            nullify (phase_values)
        end if
        call self%Qw%get_current(phase_values)
        if (associated(phase_values)) call self%Qw%set_previous(phase_values)
        nullify (phase_values)
        call self%Qi%get_current(phase_values)
        if (associated(phase_values)) call self%Qi%set_previous(phase_values)
        nullify (phase_values)
        call self%Qa%get_current(phase_values)
        if (associated(phase_values)) call self%Qa%set_previous(phase_values)
        nullify (phase_values)
        call self%Qv%get_current(phase_values)
        if (associated(phase_values)) call self%Qv%set_previous(phase_values)
        nullify (phase_values)

        ! Global mass-bias acceptance gate settings (conserved-mode nonlinear loop).
        ! Read directly from solver_settings.nonlinear_solver.convergence.conserved
        ! and kept as plain type_ftcms components: the gate is evaluated in
        ! ftcms_solve.F90, where the residual vector, dt, and mesh integration are
        ! already available, so threading these two scalars through the control /
        ! iteration_manager layers would add indirection without benefit.
        self%mass_bias_tolerance = input%basic%solver_settings%nonlinear_solver%convergence%mass_bias_tolerance
        self%enable_mass_bias_gate = input%basic%solver_settings%nonlinear_solver%convergence%enable_mass_bias_gate
        self%mass_ref = 0.0d0
        if (self%is_active_hydraulic()) then
            ! M_ref is evaluated once from the initial state (Qw/Qi/Qv above are
            ! already consistent with the IC). A stale M_ref through the run is
            ! intentional: it is a fixed reference scale, not a moving target.
            self%mass_ref = self%compute_mass_reference()
        end if

        call input_translator%execute(input, config_output)
        call input_translator%execute(input, config_observation)
        call input_translator%execute(input, config_overall)

        if (config_observation%point_type == OUTPUT_OBSERVATION_TYPES%COORDINATES) then
            if (allocated(config_observation%observation_geometries)) then
                do i = 1, size(config_observation%observation_geometries)
                    call self%domain%get_config(config_observation%observation_geometries(i))
                end do
            end if
        end if

        call self%output%initialize(config_output, config_observation, config_overall)
        call self%output_fields()
        call self%output_history()

        call initialize_assimilation_ftcms(self, input%input_path)

        call open_solver_history_log(self)

    end subroutine initialize_type_ftcms

    !> Open Output/solver_history.log (rank 0 only): one record per time-step
    !> attempt with the nonlinear-convergence diagnostics that are otherwise
    !> invisible from outside (iterations, acceptance, omega, ||dQ||_W, LTE).
    subroutine open_solver_history_log(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        character(:), allocatable :: project_path_env
        character(*), parameter :: PROJECT_ENV = "FTCMS_PROJECT_PATH"
        integer(int32) :: myrank, ierr, ios

        self%solver_history_unit = -1
        call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)
        if (myrank /= 0) return

        call get_env_string(PROJECT_ENV, project_path_env)
        call modify_path_format(project_path_env)

        open (newunit=self%solver_history_unit, &
              file=trim(project_path_env)//"Output/solver_history.log", &
              status="replace", action="write", iostat=ios)
        if (ios /= 0) then
            self%solver_history_unit = -1
            return
        end if
        write (self%solver_history_unit, '(A)') &
            "# FTCMS solver history: one record per time-step attempt"
        write (self%solver_history_unit, '(A)') &
            "# step_attempt  time_end[s]      dt[s]        nl_iter  accepted  omega     dq_norm_W    lte_rel"// &
            "      mass_bias_ratio"
        flush (self%solver_history_unit)
    end subroutine open_solver_history_log

    subroutine initialize_assimilation_ftcms(self, input_path)
        use :: json_module, only: json_file
        implicit none
        class(type_ftcms), intent(inout) :: self
        character(*), intent(in) :: input_path

        type(json_file) :: jf
        type(type_da_config) :: da_cfg
        character(len=512) :: da_file
        character(:), allocatable :: str_val
        logical :: found, file_ok
        integer(int32) :: ierr, myrank
        logical :: da_enabled_flag

        call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)

        da_file = trim(input_path)//'DataAssimilation.json'
        inquire (file=trim(da_file), exist=file_ok)
        if (.not. file_ok) return

        call jf%initialize()
        call jf%load(trim(da_file))
        if (jf%failed()) return

        da_enabled_flag = .true.
        call jf%get('enabled', da_enabled_flag, found)
        if (found .and. .not. da_enabled_flag) then
            call jf%destroy()
            return
        end if

        call jf%get('ensemble.size', da_cfg%ensemble_size, found)
        call jf%get('ensemble.max_height', da_cfg%max_height, found)
        call jf%get('ensemble.num_nodes', da_cfg%num_nodes, found)
        call jf%get('observation.csv_file', str_val, found)
        if (found .and. allocated(str_val)) da_cfg%csv_file = str_val
        call jf%get('observation.upper_bc_file', str_val, found)
        if (found .and. allocated(str_val)) da_cfg%upper_bc_file = str_val
        call jf%get('observation.interval_seconds', da_cfg%interval_seconds, found)
        call jf%get('observation.sigma_T', da_cfg%sigma_T, found)
        call jf%get('observation.sigma_q', da_cfg%sigma_q, found)
        call jf%get('observation.sigma_U', da_cfg%sigma_U, found)
        call jf%get('surface.z0', da_cfg%z0, found)
        call jf%get('surface.albedo', da_cfg%albedo, found)
        call jf%get('surface.emissivity', da_cfg%emissivity, found)
        call jf%get('surface.Pmin', da_cfg%Pmin, found)
        call jf%get('surface.Pmax', da_cfg%Pmax, found)
        call jf%get('surface.lambda_soil', da_cfg%lambda_soil, found)
        call jf%get('surface.stomatal_resistance', da_cfg%stomatal_resistance, found)
        call jf%get('reference_datetime', str_val, found)
        if (found .and. allocated(str_val)) da_cfg%reference_datetime = str_val
        call jf%get('solar.latitude', da_cfg%latitude, found)
        call jf%get('solar.longitude', da_cfg%longitude, found)
        call jf%get('solar.tau_atm', da_cfg%tau_atm, found)
        call jf%get('solar.utc_offset_hours', da_cfg%utc_offset_hours, found)
        call jf%destroy()

        call self%assimilation%initialize(da_cfg, &
            bc_entity_thermal=3, bc_entity_hydraulic=3)
        self%assimilation_enabled = .true.

        if (myrank == 0) then
            write (*, '(A)') '[DA] Data assimilation enabled from DataAssimilation.json.'
        end if
    end subroutine initialize_assimilation_ftcms

    !> Lumped nodal control volume V_i = sum over incident elements of
    !> (element measure / n_local_nodes), i.e. an equal partition of each
    !> element's measure among its local nodes. Same lumping pattern as
    !> compute_mass_reference_ftcms's elem_vol/n_local average, computed once
    !> here (rather than per-element) since it is a purely geometric, time-
    !> invariant quantity. Used by apply_prognostic_ice_update as the volume
    !> normalizing the nodal mass residual R_H,i into a rate.
    subroutine compute_clapeyron_node_volume(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        integer(int32) :: i_elem, num_elem, i_local, node_id, n_local, num_nodes
        integer(int32), pointer, contiguous :: connectivity(:)
        real(real64) :: elem_vol, share

        call self%domain%get_num_nodes(num_nodes)
        if (.not. allocated(self%clapeyron_node_volume)) return
        if (size(self%clapeyron_node_volume) /= num_nodes) return
        self%clapeyron_node_volume(:) = 0.0d0

        nullify (connectivity)
        call self%domain%get_num_fe(num_elem)

        do i_elem = 1, num_elem
            call self%domain%get_fe_connectivity(i_elem, connectivity)
            call self%domain%calc_measure(i_elem, elem_vol)

            n_local = 0
            do i_local = 1, size(connectivity)
                if (connectivity(i_local) >= 1) n_local = n_local + 1
            end do
            if (n_local <= 0) then
                nullify (connectivity)
                cycle
            end if
            share = elem_vol / real(n_local, real64)

            do i_local = 1, size(connectivity)
                node_id = connectivity(i_local)
                if (node_id < 1 .or. node_id > num_nodes) cycle
                self%clapeyron_node_volume(node_id) = self%clapeyron_node_volume(node_id) + share
            end do
            nullify (connectivity)
        end do

    end subroutine compute_clapeyron_node_volume

    module subroutine output_fields_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        integer(int32) :: iter
        real(real64) :: current_time

        real(real64), pointer, contiguous, dimension(:) :: temperature
        real(real64), pointer, contiguous, dimension(:) :: pressure
        real(real64), pointer, contiguous, dimension(:) :: water_content
        real(real64), pointer, contiguous, dimension(:) :: ice_pore
        real(real64), pointer, contiguous, dimension(:) :: vapor_content

        type(type_coordinate_array_dp) :: water_flux_arr
        integer(int32) :: num_nodes_wf, num_fe_wf, i_elem_wf, i_local_wf, node_id_wf
        integer(int32), pointer, contiguous :: conn_wf(:)
        type(type_state) :: state_wf
        type(type_coordinate_dp), pointer :: wf_ptr
        logical :: has_water_flux

        call self%control%profiler_start(PROFILER_TYPES%IO)

        nullify (temperature)
        nullify (pressure)
        nullify (ice_pore)
        nullify (vapor_content)
        nullify (water_content)

        call self%control%get_time(current_time)

        if (self%control%is_output_triggered(OUTPUT_TYPES%FIELD, current_time)) then
            call self%control%get_output_step(OUTPUT_TYPES%FIELD, iter)
            if (self%is_active_thermal()) then
                call self%temperature%get_current(temperature)
            end if
            if (self%is_active_hydraulic()) then
                call self%pressure%get_current(pressure)
            end if
            call self%Qw%get_current(water_content)
            call self%Qi%get_current(ice_pore)
            call self%Qv%get_current(vapor_content)

            has_water_flux = .false.
            if (self%is_active_hydraulic()) then
                nullify (conn_wf)
                nullify (wf_ptr)
                call self%domain%get_num_nodes(num_nodes_wf)
                call self%domain%get_num_fe(num_fe_wf)
                call water_flux_arr%initialize(num_nodes_wf)
                do i_elem_wf = 1, num_fe_wf
                    call self%domain%get_fe_connectivity(i_elem_wf, conn_wf)
                    do i_local_wf = 1, size(conn_wf)
                        node_id_wf = conn_wf(i_local_wf)
                        if (node_id_wf < 1) cycle
                        call self%set_state(node_id_wf, i_elem_wf, state_wf, calc_physics=.true.)
                        nullify (wf_ptr)
                        call state_wf%get(water_flux=wf_ptr)
                        if (associated(wf_ptr)) then
                            water_flux_arr%x(node_id_wf) = wf_ptr%x
                            water_flux_arr%y(node_id_wf) = wf_ptr%y
                            water_flux_arr%z(node_id_wf) = wf_ptr%z
                        end if
                    end do
                    nullify (conn_wf)
                end do
                has_water_flux = .true.
            end if

            if (has_water_flux) then
                call self%output%output_fields(file_counts=iter, &
                                               temperature=temperature, &
                                               water_content=water_content, &
                                               ice_content=ice_pore, &
                                               vapor_content=vapor_content, &
                                               pressure=pressure, &
                                               water_flux=water_flux_arr)
            else
                call self%output%output_fields(file_counts=iter, &
                                               temperature=temperature, &
                                               water_content=water_content, &
                                               ice_content=ice_pore, &
                                               vapor_content=vapor_content, &
                                               pressure=pressure)
            end if

            call self%control%update_output(OUTPUT_TYPES%FIELD, current_time)

            call water_flux_arr%destroy()
            nullify (temperature)
            nullify (pressure)
            nullify (ice_pore)
            nullify (water_content)
            nullify (vapor_content)
        end if

        call self%control%profiler_stop(PROFILER_TYPES%IO)
    end subroutine output_fields_ftcms

    module subroutine output_history_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        real(real64) :: current_time, current_time_converted
        real(real64), pointer, contiguous, dimension(:) :: temperature
        real(real64), pointer, contiguous, dimension(:) :: pressure
        real(real64), pointer, contiguous, dimension(:) :: water_content
        real(real64), pointer, contiguous, dimension(:) :: ice_pore
        real(real64), pointer, contiguous, dimension(:) :: vapor_content
        type(type_coordinate_array_dp) :: liq_flux_arr, vap_flux_arr
        integer(int32) :: num_nodes_hf, i_obs_fe, i_local_hf, node_id_hf
        integer(int32), pointer, contiguous :: conn_hf(:)
        integer(int32), allocatable :: obs_fe_ids(:)
        type(type_state) :: state_hf
        type(type_coordinate_dp), pointer :: wf_liq_ptr, wf_vap_ptr
        logical :: has_flux

        call self%control%profiler_start(PROFILER_TYPES%IO)

        nullify (temperature)
        nullify (pressure)
        nullify (water_content)
        nullify (ice_pore)
        nullify (vapor_content)

        call self%control%get_time(current_time)

        if (self%control%is_output_triggered(OUTPUT_TYPES%HISTORY, current_time)) then
            if (self%is_active_thermal()) then
                call self%temperature%get_current(temperature)
            end if
            if (self%is_active_hydraulic()) then
                call self%pressure%get_current(pressure)
            end if
            call self%Qw%get_current(water_content)
            call self%Qi%get_current(ice_pore)
            call self%Qv%get_current(vapor_content)

            ! Compute per-node liquid and vapor fluxes for observation elements only
            has_flux = .false.
            if (self%is_active_hydraulic()) then
                call self%output%get_obs_fe_ids(obs_fe_ids)
                if (size(obs_fe_ids) > 0 .and. all(obs_fe_ids > 0)) then
                    nullify (conn_hf)
                    call self%domain%get_num_nodes(num_nodes_hf)
                    call liq_flux_arr%initialize(num_nodes_hf)
                    call vap_flux_arr%initialize(num_nodes_hf)
                    do i_obs_fe = 1, size(obs_fe_ids)
                        call self%domain%get_fe_connectivity(obs_fe_ids(i_obs_fe), conn_hf)
                        do i_local_hf = 1, size(conn_hf)
                            node_id_hf = conn_hf(i_local_hf)
                            if (node_id_hf < 1) cycle
                            nullify (wf_liq_ptr, wf_vap_ptr)
                            call self%set_state(node_id_hf, obs_fe_ids(i_obs_fe), state_hf, calc_physics=.true.)
                            call state_hf%get(water_flux=wf_liq_ptr, vapor_flux=wf_vap_ptr)
                            if (associated(wf_liq_ptr)) then
                                liq_flux_arr%x(node_id_hf) = wf_liq_ptr%x
                                liq_flux_arr%y(node_id_hf) = wf_liq_ptr%y
                                liq_flux_arr%z(node_id_hf) = wf_liq_ptr%z
                            end if
                            if (associated(wf_vap_ptr)) then
                                vap_flux_arr%x(node_id_hf) = wf_vap_ptr%x
                                vap_flux_arr%y(node_id_hf) = wf_vap_ptr%y
                                vap_flux_arr%z(node_id_hf) = wf_vap_ptr%z
                            end if
                        end do
                        nullify (conn_hf)
                    end do
                    has_flux = .true.
                end if
                if (allocated(obs_fe_ids)) deallocate (obs_fe_ids)
            end if

            call self%control%get_output_time(OUTPUT_TYPES%HISTORY, current_time, current_time_converted)
            if (has_flux) then
                call self%output%output_history(time=current_time_converted, &
                                                temperature=temperature, &
                                                water_content=water_content, &
                                                ice_content=ice_pore, &
                                                vapor_content=vapor_content, &
                                                pressure=pressure, &
                                                water_flux=liq_flux_arr, &
                                                vapor_flux=vap_flux_arr)
            else
                call self%output%output_history(time=current_time_converted, &
                                                temperature=temperature, &
                                                water_content=water_content, &
                                                ice_content=ice_pore, &
                                                vapor_content=vapor_content, &
                                                pressure=pressure)
            end if
            call self%control%update_output(OUTPUT_TYPES%HISTORY, current_time)

            if (has_flux) then
                call liq_flux_arr%destroy()
                call vap_flux_arr%destroy()
            end if
            nullify (water_content)
            nullify (ice_pore)
            nullify (vapor_content)
            nullify (temperature)
            nullify (pressure)
        end if

        call self%control%profiler_stop(PROFILER_TYPES%IO)
    end subroutine output_history_ftcms

    module subroutine get_variable_increment_ftcms(self, variable_id, variable)
        implicit none
        class(type_ftcms), intent(inout) :: self
        type(type_constant_id), intent(in) :: variable_id
        real(real64), intent(inout), allocatable :: variable(:)

        real(real64), pointer, dimension(:) :: du

        integer(int32) :: target_dof
        integer(int32) :: num_nodes, num_dofs_per_node
        integer(int32) :: start_idx, end_idx

        nullify (du)
        call deallocate_array(variable)

        if (.not. PHYSICS_TYPES%is_valid(variable_id)) then
            call allocate_array(variable, 0)
            return
        end if

        if (self%control%is_physics_active(variable_id)) then
            call self%domain%get_num_nodes(num_nodes)
            call allocate_array(variable, num_nodes)

            if (self%control%is_staggered()) then
                du => self%du%get_data(variable_id%ID)
                if (.not. associated(du)) then
                    variable(:) = 0.0d0
                else
                    variable(:) = du(1:num_nodes)
                    nullify (du)
                end if
            else
                du => self%du%get_data()
                if (.not. associated(du)) then
                    variable(:) = 0.0d0
                else
                    call self%domain%get_num_dof_per_node(num_dofs_per_node)
                    call self%domain%get_start_dof_index(variable_id, target_dof)
                    start_idx = target_dof
                    end_idx = num_dofs_per_node * (num_nodes - 1) + target_dof
                    variable(:) = du(start_idx:end_idx:num_dofs_per_node)
                    nullify (du)
                end if
            end if
        else
            call allocate_array(variable, 0)
        end if

    end subroutine get_variable_increment_ftcms

    module subroutine get_variable_residual_ftcms(self, variable_id, variable)
        implicit none
        class(type_ftcms), intent(inout) :: self
        type(type_constant_id), intent(in) :: variable_id
        real(real64), intent(inout), allocatable :: variable(:)

        real(real64), pointer, dimension(:) :: F

        integer(int32) :: target_dof
        integer(int32) :: num_nodes, num_dofs_per_node
        integer(int32) :: start_idx, end_idx

        nullify (F)
        call deallocate_array(variable)

        if (.not. PHYSICS_TYPES%is_valid(variable_id)) then
            call allocate_array(variable, 0)
            return
        end if

        if (self%control%is_physics_active(variable_id)) then
            call self%domain%get_num_nodes(num_nodes)
            call allocate_array(variable, num_nodes)

            if (self%control%is_staggered()) then
                F => self%F%get_data(variable_id%ID)
                if (.not. associated(F)) then
                    variable(:) = 0.0d0
                else
                    variable(:) = F(1:num_nodes)
                    nullify (F)
                end if
            else
                F => self%F%get_data()
                if (.not. associated(F)) then
                    variable(:) = 0.0d0
                else
                    call self%domain%get_num_dof_per_node(num_dofs_per_node)
                    call self%domain%get_start_dof_index(variable_id, target_dof)
                    start_idx = target_dof
                    end_idx = num_dofs_per_node * (num_nodes - 1) + target_dof
                    variable(:) = F(start_idx:end_idx:num_dofs_per_node)
                    nullify (F)
                end if
            end if
        else
            call allocate_array(variable, 0)
        end if

    end subroutine get_variable_residual_ftcms

    module subroutine set_state_ftcms(self, node_id, element_id, state, calc_physics)
        implicit none
        class(type_ftcms), intent(inout) :: self
        integer(int32), intent(in) :: node_id
        integer(int32), intent(in) :: element_id
        type(type_state), intent(inout) :: state
        logical, intent(in), optional :: calc_physics

        integer(int32) :: material_id
        integer(int32) :: bdf_order
        integer(int32) :: start_dof_thermal, start_dof_hydraulic
        real(real64) :: temperature, pressure, porosity
        type(type_coordinate_dp) :: grad_T, grad_P
        real(real64) :: temperature_history(8), pressure_history(8), porosity_history(8)

        logical :: do_calc
        logical :: temperature_set, pressure_set

        ! Default: compute physics (for backward compatibility)
        do_calc = .true.
        if (present(calc_physics)) do_calc = calc_physics
        call state%reset()

        call self%control%get_bdf_coeffs(bdf_order=bdf_order)

        start_dof_thermal = self%thermal_start_dof
        if (start_dof_thermal > 0) then
            call self%temperature%get_current(node_id, temperature)
            call self%temperature%get_current_gradient(node_id, grad_T)
            call self%temperature%get_history(node_id, temperature_history)
        else
            temperature = 0.0d0
            call grad_T%reset()
            temperature_history = 0.0d0
        end if
        call state%set(temperature=temperature, &
                       grad_T=grad_T, &
                       temperature_history=temperature_history(1:bdf_order + 1))

        start_dof_hydraulic = self%hydraulic_start_dof
        if (start_dof_hydraulic > 0) then
            call self%pressure%get_current(node_id, pressure)
            call self%pressure%get_current_gradient(node_id, grad_P)
            call self%pressure%get_history(node_id, pressure_history)
        else
            pressure = 0.0d0
            call grad_P%reset()
            pressure_history = 0.0d0
        end if
        call state%set(pressure=pressure, &
                       grad_P=grad_P, &
                       pressure_history=pressure_history(1:bdf_order + 1))

        call self%porosity%get_current(node_id, porosity)
        call self%porosity%get_history(node_id, porosity_history)
        call state%set(porosity=porosity, &
                       porosity_history=porosity_history(1:bdf_order + 1))

        block
            real(real64) :: qw_val, qi_val, qa_val, qv_val
            call self%Qw%get_current(node_id, qw_val)
            call self%Qi%get_current(node_id, qi_val)
            call self%Qa%get_current(node_id, qa_val)
            call self%Qv%get_current(node_id, qv_val)
            call state%water_content%set(qw_val)
            call state%ice_content%set(qi_val)
            call state%air_content%set(qa_val)
            call state%vapor_content%set(qv_val)
        end block

        call state%temperature%get(temperature, temperature_set)
        call state%pressure%get(pressure, pressure_set)
        if (.not. temperature_set .or. .not. pressure_set) then
            !$omp critical (ftcms_state_diag)
            write (*, '(A,I0,A,I0,A,L1,A,L1,A,I0,A,I0)') 'Error: set_state_ftcms unset primary state. node=', node_id, &
                ', elem=', element_id, ', T_set=', temperature_set, ', P_set=', pressure_set, &
                ', T_dof=', start_dof_thermal, ', H_dof=', start_dof_hydraulic
            !$omp end critical (ftcms_state_diag)
            error stop 'set_state_ftcms: temperature/pressure unset before constitutive update.'
        end if

        ! Run expensive physics computations only when flagged
        if (do_calc) then
            call self%domain%get_material_id(element_id, material_id)
            call self%update_physical_properties(material_id, state)
            call override_prognostic_ice_ftcms(self, node_id, state)
        end if

    end subroutine set_state_ftcms

    !> A1 prototype closure (see enable_clapeyron_pressure_constraint): replace
    !> the equilibrium ice_content that update_physical_properties just computed
    !> with the prognostic value held in self%Qi, at pressure-constrained nodes
    !> only. Without this, state%ice_content would always be re-derived from
    !> Theta(psi_cap) every call, which collapses to ~0 once P is pinned at
    !> P_eq(T) (psi_cap = psi_cryo there) - exactly the equilibrium-closure bias
    !> this prototype replaces. No-op when the flag is off, the node is not
    !> frozen, or the mask is not yet allocated/sized (defensive, e.g. during
    !> IC application before self%Qi holds a meaningful value).
    subroutine override_prognostic_ice_ftcms(self, node_id, state)
        implicit none
        class(type_ftcms), intent(inout) :: self
        integer(int32), intent(in) :: node_id
        type(type_state), intent(inout) :: state

        real(real64) :: qi_prognostic

        if (.not. self%enable_clapeyron_pressure_constraint) return
        if (.not. allocated(self%clapeyron_frozen_mask)) return
        if (node_id < 1 .or. node_id > size(self%clapeyron_frozen_mask)) return
        if (.not. self%clapeyron_frozen_mask(node_id)) return

        call self%Qi%get_current(node_id, qi_prognostic)
        call state%ice_content%set(qi_prognostic)
    end subroutine override_prognostic_ice_ftcms

    module subroutine set_states_from_connectivity_ftcms(self, connectivity, element_id, states, calc_physics)
        implicit none
        class(type_ftcms), intent(inout) :: self
        integer(int32), intent(in) :: connectivity(:)
        integer(int32), intent(in) :: element_id
        type(type_state), intent(inout) :: states(:)
        logical, intent(in), optional :: calc_physics

        integer(int32) :: i, node_id
        integer(int32) :: material_id
        logical :: do_calc

        if (size(states) /= size(connectivity)) then
            error stop 'set_states_from_connectivity_ftcms: size(states) /= size(connectivity)'
        end if

        do_calc = .true.
        if (present(calc_physics)) do_calc = calc_physics

        do i = 1, size(connectivity)
            node_id = connectivity(i)
            if (node_id < 1) then
                write (*, '(A, I0, A, I0, A, I0)') 'invalid node_id=', node_id, ', elem=', element_id, ', local=', i
                error stop 'set_states_from_connectivity_ftcms: node_id out of range'
            end if
            call self%set_state(node_id, element_id, states(i), calc_physics=.false.)
        end do

        if (do_calc) then
            call self%domain%get_material_id(element_id, material_id)
            call self%update_physical_properties_bulk(material_id, states)
            do i = 1, size(connectivity)
                call override_prognostic_ice_ftcms(self, connectivity(i), states(i))
            end do
        end if
    end subroutine set_states_from_connectivity_ftcms

    ! Update all physical quantities (phase, fluxes) for a given state
    module subroutine update_physical_properties_ftcms(self, material_id, state)
        implicit none
        class(type_ftcms), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: state

        type(type_coordinate_dp) :: water_flux, vapor_flux
        type(type_coordinate_dp), pointer :: grad_T, grad_P
        type(type_coordinate_dp), target :: zero_grad

        nullify (grad_T)
        nullify (grad_P)
        call zero_grad%set(0.0d0, 0.0d0, 0.0d0)

        ! 1. Phase change calculation (Ice/Water Content)
        call self%thermal%update_water_phases(material_id, state)

        ! 2. Flux calculation (used in advection and diffusion)
        if (self%control%is_target(PHYSICS_TYPES%HYDRAULIC, material_id)) then
            call state%grad_T%get(grad_T)
            call state%grad_P%get(grad_P)
            ! Guard against null pointer when gradient is not set
            if (.not. associated(grad_T)) grad_T => zero_grad
            if (.not. associated(grad_P)) grad_P => zero_grad
            call self%calc_water_flux(material_id, state, grad_T, grad_P, water_flux)
            call self%calc_vapor_flux(material_id, state, grad_T, grad_P, vapor_flux)
        else
            call water_flux%set(0.0d0, 0.0d0, 0.0d0)
            call vapor_flux%set(0.0d0, 0.0d0, 0.0d0)
        end if

        call state%set(water_flux=water_flux, vapor_flux=vapor_flux)
    end subroutine update_physical_properties_ftcms

    module subroutine update_physical_properties_bulk_ftcms(self, material_id, states)
        implicit none
        class(type_ftcms), intent(inout) :: self
        integer(int32), intent(in) :: material_id
        type(type_state), intent(inout) :: states(:)

        integer(int32) :: i

        do i = 1, size(states)
            call self%update_physical_properties(material_id, states(i))
        end do
    end subroutine update_physical_properties_bulk_ftcms

    module subroutine shift_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        call self%control%profiler_start(PROFILER_TYPES%SETUP)

        if (self%is_active_thermal()) then
            call self%temperature%advance()
        end if

        if (self%is_active_hydraulic()) then
            call self%pressure%advance()
        end if

        call self%porosity%advance()
        call self%Qw%advance()
        call self%Qi%advance()
        call self%Qa%advance()
        call self%Qv%advance()

        call self%control%profiler_stop(PROFILER_TYPES%SETUP)
    end subroutine shift_ftcms

    module subroutine reflect_variables_ftcms(self, step_scale)
        implicit none
        class(type_ftcms), intent(inout) :: self
        real(real64), intent(in), optional :: step_scale

        integer(int32) :: iter
        real(real64), pointer, contiguous, dimension(:) :: bdf_coeffs
        integer(int32) :: bdf_order
        real(real64), pointer, contiguous, dimension(:) :: current
        real(real64), allocatable :: du(:)
        real(real64), allocatable :: current_prev(:)
        real(real64), allocatable :: du_eff(:)

        real(real64) :: relaxation_factor
        logical :: is_none
        logical :: is_conserved_mode

        ! --- Anderson(1) mixing of the conserved coupled update ---
        ! x_{k+1} = x_k + w*g_k - gamma*[(x_k - x_{k-1}) + w*(g_k - g_{k-1})],
        ! gamma = <g_k, g_k - g_{k-1}>_W / ||g_k - g_{k-1}||_W^2 (Walker & Ni, 2011),
        ! evaluated jointly over (T, p) with head-equivalent weighting of p so both
        ! blocks are mixed consistently. Pure fixed-point acceleration: no Jacobian,
        ! the modified-Picard structure and the adaptive damping w are unchanged.
        logical :: aa_active
        real(real64) :: aa_gamma
        ! Safeguarded: the mixing is applied only while the weighted norm of the
        ! fixed-point increment ||g_k||_W is non-increasing; on growth the
        ! iteration falls back to plain relaxed Picard for that iterate (gamma=0),
        ! which lets the adaptive omega re-establish contraction before mixing
        ! resumes. Together with the kappa-corrected acceptance this closes the
        ! omega-floor stall observed at the freezing onset.
        logical, parameter :: AA_ENABLED = .true.
        real(real64), parameter :: AA_GAMMA_MAX = 2.0d0
        real(real64), parameter :: AA_WEIGHT_P = 1.0d0 / 9.81d3  ! [K/Pa] head-equivalent

        real(real64) :: max_du, alpha
        real(real64), parameter :: PICARD_MAX_DT_STEP = 2.0d1
        real(real64), parameter :: PICARD_MAX_DT_STEP_PHASE = 0.5d0   ! K near T_melt
        real(real64), parameter :: PICARD_PHASE_ZONE = 2.0d0           ! °C half-width
        ! Legacy step caps (used only by the non-conserved convergence modes; the
        ! universal conserved mode below uses adaptive under-relaxation instead).
        real(real64), parameter :: PICARD_MAX_DP_STEP = 5.0d3
        real(real64), parameter :: TEMP_MIN_C = WALL_TEMP_MIN_C
        real(real64), parameter :: TEMP_MAX_C = WALL_TEMP_MAX_C
        real(real64), parameter :: PRESS_MIN_PA = WALL_PRESS_MIN_PA
        real(real64), parameter :: PRESS_MAX_PA = WALL_PRESS_MAX_PA

        call self%control%profiler_start(PROFILER_TYPES%SETUP)

        nullify (current)
        nullify (bdf_coeffs)

        call self%control%get_nonlinear_iter(iter)

        call self%control%get_bdf_coeffs(bdf_order, bdf_coeffs)

        is_none = self%control%is_none()
        is_conserved_mode = self%control%is_conserved()

        ! Anderson(1): compute the joint mixing coefficient from this iterate's
        ! (T, p) increments and the stored previous pair, BEFORE any update.
        aa_gamma = 0.0d0
        aa_active = AA_ENABLED .and. is_conserved_mode .and. (.not. is_none) .and. &
                    (.not. self%control%is_staggered()) .and. &
                    self%is_active_thermal() .and. self%is_active_hydraulic()
        if (aa_active) call compute_aa_gamma(self, aa_gamma, aa_active)

        if (self%is_active_thermal()) then
            call self%get_variable_increment(PHYSICS_TYPES%THERMAL, du)
            call self%temperature%get_current(current)
            if (associated(current)) then
                call allocate_array(current_prev, size(current))
                current_prev(:) = current(:)

                if (allocated(du) .and. size(du) > 0) then
                    if (is_conserved_mode) then
                        ! Conserved mode uses one coupled Picard damping factor for
                        ! T and p. The factor is adapted from the conserved-quantity
                        ! contraction rate after each accepted nonlinear iterate.
                        relaxation_factor = self%control%get_conserved_relaxation()
                        if (present(step_scale)) relaxation_factor = relaxation_factor * step_scale
                        call allocate_array(du_eff, size(du))
                        du_eff(:) = relaxation_factor * du(:)
                        if (aa_active .and. self%aa_has_prev .and. &
                            allocated(self%aa_T_prev) .and. allocated(self%aa_duT_prev)) then
                            if (size(self%aa_T_prev) == size(current) .and. &
                                size(self%aa_duT_prev) == size(du)) then
                                du_eff(:) = du_eff(:) - aa_gamma * (current(:) - self%aa_T_prev(:) + &
                                                                    relaxation_factor * (du(:) - self%aa_duT_prev(:)))
                            end if
                        end if
                        ! Store this iterate (pre-update) for the next AA mixing.
                        if (aa_active) then
                            call copy_into(self%aa_T_prev, current)
                            call copy_into(self%aa_duT_prev, du)
                        end if
                        alpha = min(1.0d0, bounded_step_factor(current, du_eff, TEMP_MIN_C, TEMP_MAX_C))
                        current(:) = current(:) + alpha * du_eff(:)
                    else
                        max_du = maxval(abs(du))
                        ! Step limiter: tighter limit near T_melt to prevent C-C amplification
                        block
                            real(real64) :: local_max_step
                            if (minval(abs(current)) < PICARD_PHASE_ZONE) then
                                local_max_step = PICARD_MAX_DT_STEP_PHASE
                            else
                                local_max_step = PICARD_MAX_DT_STEP
                            end if
                            if (max_du > local_max_step) then
                                alpha = local_max_step / max_du
                            else
                                alpha = 1.0d0
                            end if
                        end block
                        ! Backtracking line search: halve alpha until T stays within bounds
                        block
                            real(real64) :: alpha_ls
                            real(real64), parameter :: LS_FACTOR = 0.5d0
                            real(real64), parameter :: LS_MIN = 1.0d-4
                            integer(int32) :: ls_iter
                            alpha_ls = alpha
                            do ls_iter = 1, 20
                                if (minval(current + alpha_ls * du) >= TEMP_MIN_C .and. &
                                    maxval(current + alpha_ls * du) <= TEMP_MAX_C) exit
                                alpha_ls = alpha_ls * LS_FACTOR
                                if (alpha_ls < LS_MIN) then
                                    alpha_ls = LS_MIN
                                    exit
                                end if
                            end do
                            alpha = alpha_ls
                        end block
                        if (.not. is_none) then
                            call self%control%compute_relaxation(PHYSICS_TYPES%THERMAL, iter, alpha * du, current)
                            call self%control%get_current_relaxation(PHYSICS_TYPES%THERMAL, relaxation_factor)
                        else
                            relaxation_factor = alpha
                            current(:) = current(:) + alpha * du(:)
                        end if
                    end if
                end if

                if (minval(current(:)) < TEMP_MIN_C .or. maxval(current(:)) > TEMP_MAX_C) then
                    current(:) = current_prev(:)
                    if (allocated(du) .and. size(du) > 0) then
                        alpha = bounded_step_factor(current, du, TEMP_MIN_C, TEMP_MAX_C)
                        current(:) = current(:) + alpha * du(:)
                    end if
                end if
                call self%temperature%set_delta(current(:) - current_prev(:))

                call deallocate_array(current_prev)
            end if

            call self%calc_gradient_temperature()
            call self%temperature%compute_time_derivative(bdf_coeffs, bdf_order)

            call deallocate_array(du)

            if (self%control%is_staggered()) then
                block
                    real(real64), pointer :: du_raw(:) => null()
                    du_raw => self%du%get_data(PHYSICS_TYPES%THERMAL%ID)
                    if (associated(du_raw)) du_raw(:) = 0.0d0
                    nullify (du_raw)
                end block
            end if
        end if

        if (self%is_active_hydraulic()) then
            call self%get_variable_increment(PHYSICS_TYPES%HYDRAULIC, du)
            call self%pressure%get_current(current)
            if (associated(current)) then
                call allocate_array(current_prev, size(current))
                current_prev(:) = current(:)

                if (allocated(du) .and. size(du) > 0) then
                    if (is_conserved_mode) then
                        ! Same coupled Picard damping factor as the thermal branch.
                        relaxation_factor = self%control%get_conserved_relaxation()
                        if (present(step_scale)) relaxation_factor = relaxation_factor * step_scale
                        call allocate_array(du_eff, size(du))
                        du_eff(:) = relaxation_factor * du(:)
                        if (aa_active .and. self%aa_has_prev .and. &
                            allocated(self%aa_P_prev) .and. allocated(self%aa_duP_prev)) then
                            if (size(self%aa_P_prev) == size(current) .and. &
                                size(self%aa_duP_prev) == size(du)) then
                                du_eff(:) = du_eff(:) - aa_gamma * (current(:) - self%aa_P_prev(:) + &
                                                                    relaxation_factor * (du(:) - self%aa_duP_prev(:)))
                            end if
                        end if
                        if (aa_active) then
                            call copy_into(self%aa_P_prev, current)
                            call copy_into(self%aa_duP_prev, du)
                            self%aa_has_prev = .true.
                        end if
                        alpha = min(1.0d0, bounded_step_factor(current, du_eff, PRESS_MIN_PA, PRESS_MAX_PA))
                        current(:) = current(:) + alpha * du_eff(:)
                    else
                        max_du = maxval(abs(du))
                        ! Step limiter: prevent large pressure updates regardless of solver mode
                        if (max_du > PICARD_MAX_DP_STEP) then
                            alpha = PICARD_MAX_DP_STEP / max_du
                        else
                            alpha = 1.0d0
                        end if
                        ! Backtracking line search: halve alpha until P stays within bounds
                        block
                            real(real64) :: alpha_ls
                            real(real64), parameter :: LS_FACTOR = 0.5d0
                            real(real64), parameter :: LS_MIN = 1.0d-4
                            integer(int32) :: ls_iter
                            alpha_ls = alpha
                            do ls_iter = 1, 20
                                if (minval(current + alpha_ls * du) >= PRESS_MIN_PA .and. &
                                    maxval(current + alpha_ls * du) <= PRESS_MAX_PA) exit
                                alpha_ls = alpha_ls * LS_FACTOR
                                if (alpha_ls < LS_MIN) then
                                    alpha_ls = LS_MIN
                                    exit
                                end if
                            end do
                            alpha = alpha_ls
                        end block
                        if (.not. is_none) then
                            call self%control%compute_relaxation(PHYSICS_TYPES%HYDRAULIC, iter, alpha * du, current)
                            call self%control%get_current_relaxation(PHYSICS_TYPES%HYDRAULIC, relaxation_factor)
                        else
                            relaxation_factor = alpha
                            current(:) = current(:) + alpha * du(:)
                        end if
                    end if
                end if

                if (minval(current(:)) < PRESS_MIN_PA .or. maxval(current(:)) > PRESS_MAX_PA) then
                    current(:) = current_prev(:)
                    if (allocated(du) .and. size(du) > 0) then
                        alpha = bounded_step_factor(current, du, PRESS_MIN_PA, PRESS_MAX_PA)
                        current(:) = current(:) + alpha * du(:)
                    end if
                end if
                call self%pressure%set_delta(current(:) - current_prev(:))

                call deallocate_array(current_prev)
            end if

            call self%calc_gradient_pressure()
            call self%pressure%compute_time_derivative(bdf_coeffs, bdf_order)

            call deallocate_array(du)

            if (self%control%is_staggered()) then
                block
                    real(real64), pointer :: du_raw(:) => null()
                    du_raw => self%du%get_data(PHYSICS_TYPES%HYDRAULIC%ID)
                    if (associated(du_raw)) du_raw(:) = 0.0d0
                    nullify (du_raw)
                end block
            end if
        end if

        call self%update_nodal_phases()

        call self%control%profiler_stop(PROFILER_TYPES%SETUP)

    contains
        pure function bounded_step_factor(vec, step, lower, upper) result(factor)
            implicit none
            real(real64), intent(in) :: vec(:)
            real(real64), intent(in) :: step(:)
            real(real64), intent(in) :: lower, upper
            real(real64) :: factor
            real(real64) :: candidate
            integer(int32) :: i, n

            factor = 1.0d0
            n = min(size(vec), size(step))
            do i = 1, n
                if (step(i) > 0.0d0) then
                    candidate = (upper - vec(i)) / step(i)
                    factor = min(factor, candidate)
                else if (step(i) < 0.0d0) then
                    candidate = (lower - vec(i)) / step(i)
                    factor = min(factor, candidate)
                end if
            end do
            factor = max(0.0d0, min(1.0d0, factor))
        end function bounded_step_factor

        !> Joint Anderson(1) mixing coefficient over the (T, p) increments.
        !> \( \gamma = \langle g_k, g_k - g_{k-1}\rangle_W / \|g_k - g_{k-1}\|_W^2 \)
        !> with W scaling p to head-equivalent units. gamma = 0 (plain relaxed
        !> Picard) when no previous pair is stored, sizes changed, the
        !> difference is degenerate, the result is non-finite, or the
        !> monotonicity safeguard trips: mixing requires \( \|g_k\|_W \le
        !> \|g_{k-1}\|_W \), so a diverging fixed-point sequence is never
        !> extrapolated. |gamma| is clipped to AA_GAMMA_MAX as the standard
        !> safeguard.
        subroutine compute_aa_gamma(self, gamma, active)
            implicit none
            class(type_ftcms), intent(inout) :: self
            real(real64), intent(inout) :: gamma
            logical, intent(inout) :: active

            real(real64), allocatable :: g_T(:), g_P(:)
            real(real64) :: numer, denom, dg, gnorm, gnorm_prev
            integer(int32) :: i

            gamma = 0.0d0
            call self%get_variable_increment(PHYSICS_TYPES%THERMAL, g_T)
            call self%get_variable_increment(PHYSICS_TYPES%HYDRAULIC, g_P)
            if (.not. (allocated(g_T) .and. allocated(g_P))) then
                active = .false.
                return
            end if

            gnorm = 0.0d0
            do i = 1, size(g_T)
                gnorm = gnorm + g_T(i) * g_T(i)
            end do
            do i = 1, size(g_P)
                gnorm = gnorm + (g_P(i) * AA_WEIGHT_P)**2
            end do
            gnorm = sqrt(gnorm)
            gnorm_prev = self%aa_gnorm_prev
            self%aa_gnorm_prev = gnorm

            if (.not. self%aa_has_prev) return
            if (.not. (allocated(self%aa_duT_prev) .and. allocated(self%aa_duP_prev))) return
            if (size(self%aa_duT_prev) /= size(g_T) .or. size(self%aa_duP_prev) /= size(g_P)) then
                self%aa_has_prev = .false.
                return
            end if
            ! Monotonicity safeguard: mix only while the increment sequence
            ! contracts; otherwise fall back to plain relaxed Picard and let
            ! the adaptive omega restore contraction first.
            if (gnorm_prev >= 0.0d0 .and. gnorm > gnorm_prev) return

            numer = 0.0d0
            denom = 0.0d0
            do i = 1, size(g_T)
                dg = g_T(i) - self%aa_duT_prev(i)
                numer = numer + g_T(i) * dg
                denom = denom + dg * dg
            end do
            do i = 1, size(g_P)
                dg = (g_P(i) - self%aa_duP_prev(i)) * AA_WEIGHT_P
                numer = numer + (g_P(i) * AA_WEIGHT_P) * dg
                denom = denom + dg * dg
            end do

            if (denom > tiny(1.0d0)) then
                gamma = numer / denom
                if (.not. (gamma == gamma .and. abs(gamma) < huge(1.0d0))) gamma = 0.0d0
                gamma = max(-AA_GAMMA_MAX, min(AA_GAMMA_MAX, gamma))
            end if
        end subroutine compute_aa_gamma

        !> (Re)allocate dst to the shape of src and copy.
        subroutine copy_into(dst, src)
            implicit none
            real(real64), allocatable, intent(inout) :: dst(:)
            real(real64), intent(in) :: src(:)

            if (allocated(dst)) then
                if (size(dst) /= size(src)) deallocate (dst)
            end if
            if (.not. allocated(dst)) allocate (dst(size(src)))
            dst(:) = src(:)
        end subroutine copy_into

    end subroutine reflect_variables_ftcms

    !> Correct temperature for nodes crossing T_melt during a Picard step.
    !> Uses the available-energy (Flerchinger-type) method: finds T_r satisfying
    !> \[ H(T_r) = H_{sensible}(T_{new}) \]
    !> via a Secant iteration, ensuring energy conservation at the phase-change front.
    module subroutine apply_phase_change_temperature_correction_ftcms(self, T_old, T_new)
        implicit none
        class(type_ftcms), intent(inout) :: self
        real(real64), intent(in) :: T_old(:)
        real(real64), intent(inout) :: T_new(:)

        real(real64), parameter :: T_F = 0.0d0
        real(real64), parameter :: DT_FD = 0.5d0
        integer(int32), parameter :: MAX_SECANT = 15
        real(real64), parameter :: SECANT_RTOL = 1.0d-4
        ! Lower bound of the secant search. update_water_phases requires a positive
        ! absolute temperature (T_K = T + 273.15 > 0); a diverging Picard iterate can
        ! push T_new far below physical range, which would make the root finder probe
        ! update_water_phases at T_K <= 0 (fatal). Confine the search to the same
        ! physical floor used by the solution update (reflect_variables TEMP_MIN_C),
        ! so a diverging step fails the convergence test cleanly and the ATS reduces
        ! dt, instead of aborting the run inside this root finder.
        real(real64), parameter :: T_PHYS_MIN = -80.0d0
        real(real64) :: T_lo

        integer(int32) :: i_elem, num_elem, i_local, node_id, material_id, n_nodes, iter_s
        integer(int32), pointer, contiguous :: connectivity(:)
        type(type_state) :: ev
        real(real64) :: T_old_i, T_new_i, P_i, phi_i
        real(real64) :: H_low, H_high, C_unf, H_target, H_r
        real(real64) :: G0, G1, T_r0, T_r1, T_r_new
        logical, allocatable :: processed(:)

        nullify (connectivity)
        call self%domain%get_num_fe(num_elem)
        call self%domain%get_num_nodes(n_nodes)
        if (n_nodes <= 0) return

        allocate (processed(n_nodes))
        processed = .false.

        do i_elem = 1, num_elem
            call self%domain%get_fe_connectivity(i_elem, connectivity)
            call self%domain%get_material_id(i_elem, material_id)

            do i_local = 1, size(connectivity)
                node_id = connectivity(i_local)
                if (node_id < 1 .or. node_id > n_nodes) cycle
                if (processed(node_id)) cycle

                T_old_i = T_old(node_id)
                T_new_i = T_new(node_id)
                if (.not. ((T_old_i > T_F .and. T_new_i < T_F) .or. &
                           (T_old_i < T_F .and. T_new_i > T_F))) cycle
                processed(node_id) = .true.

                ! Pressure is only retained when hydraulic is active; use the
                ! reference pressure otherwise (same convention as set_state_ftcms).
                if (self%is_active_hydraulic()) then
                    call self%pressure%get_current(node_id, P_i)
                else
                    P_i = 0.0d0
                end if
                call self%porosity%get_current(node_id, phi_i)

                ! Unfrozen heat capacity C_unf via finite difference just above T_f
                call ev%reset()
                call ev%temperature%set(T_F + DT_FD)
                call ev%pressure%set(P_i)
                call ev%porosity%set(phi_i)
                call self%thermal%update_water_phases(material_id, ev)
                call self%thermal%calc_enthalpy_density(material_id, ev, H_low)

                call ev%reset()
                call ev%temperature%set(T_F + 2.0d0 * DT_FD)
                call ev%pressure%set(P_i)
                call ev%porosity%set(phi_i)
                call self%thermal%update_water_phases(material_id, ev)
                call self%thermal%calc_enthalpy_density(material_id, ev, H_high)

                C_unf = (H_high - H_low) / DT_FD
                if (C_unf <= 0.0d0) cycle

                ! Sensible-only target enthalpy at T_new (no phase-change latent heat)
                H_target = H_low + C_unf * (T_new_i - (T_F + DT_FD))

                ! Secant: G(T_r) = H(T_r) - H_target = 0
                ! G(T_f) = H(T_f) - H_target = -C_unf*(T_new_i - T_f) > 0
                ! Confine the search to [max(T_new_i, T_PHYS_MIN), T_F] so probes stay
                ! in the valid domain of update_water_phases (T_K > 0).
                T_lo = max(T_new_i, T_PHYS_MIN)
                T_r0 = T_F
                G0 = -C_unf * (T_new_i - T_F)

                T_r1 = max(T_lo, T_F - 2.0d0 * DT_FD)

                do iter_s = 1, MAX_SECANT
                    call ev%reset()
                    call ev%temperature%set(T_r1)
                    call ev%pressure%set(P_i)
                    call ev%porosity%set(phi_i)
                    call self%thermal%update_water_phases(material_id, ev)
                    call self%thermal%calc_enthalpy_density(material_id, ev, H_r)

                    G1 = H_r - H_target
                    if (abs(G1) < SECANT_RTOL * abs(C_unf)) exit
                    if (abs(G1 - G0) < 1.0d-30 * abs(C_unf)) exit

                    T_r_new = T_r1 - G1 * (T_r1 - T_r0) / (G1 - G0)
                    T_r0 = T_r1
                    G0 = G1
                    T_r1 = min(T_r_new, T_F)
                    T_r1 = max(T_r1, T_lo)
                end do

                T_new(node_id) = min(max(T_r1, T_lo), T_F)
            end do
        end do

        deallocate (processed)
    end subroutine apply_phase_change_temperature_correction_ftcms

    module subroutine update_nodal_phases_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        integer(int32) :: i_elem, num_elem, i_local, node_id
        integer(int32), pointer, contiguous :: connectivity(:)
        type(type_state) :: state
        real(real64) :: qw_val, qi_val, qa_val, qv_val
        logical :: qi_set, qw_set, qa_set, qv_set

        nullify (connectivity)
        call self%domain%get_num_fe(num_elem)
        do i_elem = 1, num_elem
            call self%domain%get_fe_connectivity(i_elem, connectivity)
            do i_local = 1, size(connectivity)
                node_id = connectivity(i_local)
                if (node_id < 1) cycle
                call self%set_state(node_id, i_elem, state, calc_physics=.true.)
                qi_set = .false.; qw_set = .false.; qa_set = .false.
                qv_set = .false.
                call state%ice_content%get(qi_val, qi_set)
                call state%water_content%get(qw_val, qw_set)
                call state%air_content%get(qa_val, qa_set)
                call state%vapor_content%get(qv_val, qv_set)
                if (qi_set) call self%Qi%set_current(node_id, qi_val)
                if (qw_set) call self%Qw%set_current(node_id, qw_val)
                if (qa_set) call self%Qa%set_current(node_id, qa_val)
                if (qv_set) call self%Qv%set_current(node_id, qv_val)
            end do
            nullify (connectivity)
        end do

    end subroutine update_nodal_phases_ftcms

    !> A1 prototype closure (see enable_clapeyron_pressure_constraint). Advance
    !> the prognostic ice content at every pressure-constrained node from the
    !> mass residual that the constraint excluded from the hydraulic solve, then
    !> clip to the physical bounds. Called once per ACCEPTED step, before shift().
    !>
    !> Sign derivation (required by design memo, verified against
    !> hydraulic_matrix.F90:assemble_local_picard_hydraulic):
    !>   local_vec_res(i) = R1(dTheta/dt) + K2(D_HH)*P + K2(D_HT)*T - R2(V) + R1(sink)
    !>   F_H(i) = -local_vec_res(i)                                   [F_H%set(ADD,i,-local_vec_res(i))]
    !> local_vec_res(i) is the (Galerkin) mass-conservation residual: it is < 0
    !> at a node with net flux CONVERGENCE (inflow), e.g. for a 2-node 1D
    !> diffusion element with P1 > P2 (flow 1->2, node 2 gains mass):
    !> (K*P)_2 = -(D/L)(P1-P2) < 0 - confirmed by direct construction of the
    !> element stiffness matrix. At a constrained node, Qi is held fixed
    !> throughout the nonlinear loop, so the assembled dTheta/dt already
    !> excludes any true ice growth; the complete mass balance requires
    !>   (rho_i/rho_w) * dQi_true/dt = -local_vec_res(i) / V_i
    !> i.e. net inflow (local_vec_res < 0) must increase Qi. Substituting
    !> local_vec_res(i) = -F_H(i) = -clapeyron_R_H_raw(node_id):
    !>   dQi_true/dt = (rho_w/rho_i) * clapeyron_R_H_raw(node_id) / V_i
    !>   Delta Qi_i  = (rho_w/rho_i) * clapeyron_R_H_raw(node_id) * dt / V_i
    !> (equivalently Delta Qi_i = -(rho_w/rho_i)*R_H,i*dt/V_i with the design
    !> memo's R_H,i = local_vec_res(i) = -clapeyron_R_H_raw(node_id)).
    module subroutine apply_prognostic_ice_update_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        logical, parameter :: CLAPEYRON_VERBOSE = .true.

        integer(int32) :: i_elem, num_elem, i_local, node_id, material_id, num_nodes
        integer(int32), pointer, contiguous :: connectivity(:)
        logical, allocatable :: processed(:)
        type(type_state) :: probe_state
        real(real64) :: dt_current, T_node, P_node, phi_node, theta_w, theta_s_cap
        real(real64) :: rho_w, rho_i, R_raw, dQi, Qi_old, Qi_new, Qi_new_raw, Qi_cap
        integer(int32) :: num_constrained, num_clipped
        real(real64) :: max_abs_dQi, max_P_unconstrained_frozen
        logical :: any_unconstrained_frozen

        if (.not. self%enable_clapeyron_pressure_constraint) return
        if (self%control%is_staggered()) return
        if (.not. allocated(self%clapeyron_frozen_mask)) return

        call self%domain%get_num_nodes(num_nodes)
        if (size(self%clapeyron_frozen_mask) /= num_nodes) return

        call self%control%get_dt(dt_current)

        num_constrained = 0
        num_clipped = 0
        max_abs_dQi = 0.0d0
        max_P_unconstrained_frozen = -huge(1.0d0)
        any_unconstrained_frozen = .false.

        allocate (processed(num_nodes))
        processed = .false.

        nullify (connectivity)
        call self%domain%get_num_fe(num_elem)

        do i_elem = 1, num_elem
            call self%domain%get_fe_connectivity(i_elem, connectivity)
            call self%domain%get_material_id(i_elem, material_id)

            do i_local = 1, size(connectivity)
                node_id = connectivity(i_local)
                if (node_id < 1 .or. node_id > num_nodes) cycle
                if (processed(node_id)) cycle
                processed(node_id) = .true.

                call self%set_state(node_id, i_elem, probe_state, calc_physics=.false.)
                call probe_state%temperature%get(T_node)
                call probe_state%pressure%get(P_node)

                ! Diagnostic only: an unconstrained node with T<0 should not
                ! occur once the active-set classification is self-consistent;
                ! its presence signals a stale/lagged classification worth
                ! reporting (see design memo item 5).
                if (T_node < 0.0d0 .and. .not. self%clapeyron_frozen_mask(node_id)) then
                    any_unconstrained_frozen = .true.
                    max_P_unconstrained_frozen = max(max_P_unconstrained_frozen, P_node)
                end if

                if (.not. self%clapeyron_frozen_mask(node_id)) cycle
                num_constrained = num_constrained + 1

                R_raw = self%clapeyron_R_H_raw(node_id)

                call self%thermal%calc_density_water(probe_state, rho_w)
                call self%thermal%calc_density_ice(probe_state, rho_i)
                if (rho_i <= tiny(1.0d0)) cycle

                dQi = (rho_w / rho_i) * R_raw * dt_current
                if (self%clapeyron_node_volume(node_id) > tiny(1.0d0)) then
                    dQi = dQi / self%clapeyron_node_volume(node_id)
                else
                    dQi = 0.0d0
                end if

                call self%Qi%get_current(node_id, Qi_old)
                Qi_new_raw = Qi_old + dQi

                ! Clip to [0, cap], cap from theta_w + Qi*(rho_i/rho_w) <= theta_s
                ! (theta_s = porosity, the same liquid-equivalent Theta convention
                ! used to derive Delta Qi above). Clipping discards the excess
                ! mass rather than redistributing it (accepted prototype
                ! simplification, see design memo item 4); occurrences are
                ! counted below rather than silently dropped.
                call self%porosity%get_current(node_id, phi_node)
                call self%Qw%get_current(node_id, theta_w)
                theta_s_cap = phi_node
                Qi_cap = max(0.0d0, (theta_s_cap - theta_w) * (rho_w / rho_i))

                Qi_new = min(max(Qi_new_raw, 0.0d0), Qi_cap)
                if (abs(Qi_new - Qi_new_raw) > 1.0d-15 * max(1.0d0, abs(Qi_new_raw))) then
                    num_clipped = num_clipped + 1
                end if

                call self%Qi%set_current(node_id, Qi_new)
                max_abs_dQi = max(max_abs_dQi, abs(Qi_new - Qi_old))
            end do
            nullify (connectivity)
        end do

        deallocate (processed)

        if (CLAPEYRON_VERBOSE) then
            if (any_unconstrained_frozen) then
                write (*, '(A,I0,A,ES11.3,A,I0,A,ES11.3)') &
                    '   [CLAPEYRON] constrained=', num_constrained, ', max|dQi|=', max_abs_dQi, &
                    ', clipped=', num_clipped, ', max(P|T<0,unconstrained)=', max_P_unconstrained_frozen
            else
                write (*, '(A,I0,A,ES11.3,A,I0,A)') &
                    '   [CLAPEYRON] constrained=', num_constrained, ', max|dQi|=', max_abs_dQi, &
                    ', clipped=', num_clipped, ', max(P|T<0,unconstrained)=n/a'
            end if
        end if

    end subroutine apply_prognostic_ice_update_ftcms

    !> Evaluate per-node conserved quantities at the current iterate.
    !>
    !> Mathematical definition:
    !> - enthalpy(j)  = volumetric enthalpy density H_j(T_j, p_{w,j}) [J/m3]
    !> - density(j)   = pore-water effective density rho_eff,j         [kg/m3]
    !>
    !> Each node is visited once (processed mask). The nodal state is rebuilt with
    !> set_state(calc_physics=.true.) so the phase contents are consistent with the
    !> current (T, p_w) before evaluating H and rho_eff. Inactive physics yields a
    !> zero field (contributes nothing to the weighted norm). Cost: O(N_nd).
    module subroutine compute_nodal_conserved_ftcms(self, enthalpy, density)
        implicit none
        class(type_ftcms), intent(inout) :: self
        real(real64), allocatable, intent(inout) :: enthalpy(:)
        real(real64), allocatable, intent(inout) :: density(:)

        integer(int32) :: i_elem, num_elem, i_local, node_id, material_id, n_nodes
        integer(int32), pointer, contiguous :: connectivity(:)
        type(type_state) :: state
        logical, allocatable :: processed(:)
        logical :: active_thermal, active_hydraulic
        real(real64) :: H_j, rho_j

        nullify (connectivity)
        call self%domain%get_num_nodes(n_nodes)
        call self%domain%get_num_fe(num_elem)

        if (allocated(enthalpy)) deallocate (enthalpy)
        if (allocated(density)) deallocate (density)
        allocate (enthalpy(max(n_nodes, 1)))
        allocate (density(max(n_nodes, 1)))
        enthalpy = 0.0d0
        density = 0.0d0
        if (n_nodes <= 0) return

        active_thermal = self%is_active_thermal()
        active_hydraulic = self%is_active_hydraulic()

        allocate (processed(n_nodes))
        processed = .false.

        do i_elem = 1, num_elem
            call self%domain%get_fe_connectivity(i_elem, connectivity)
            call self%domain%get_material_id(i_elem, material_id)

            do i_local = 1, size(connectivity)
                node_id = connectivity(i_local)
                if (node_id < 1 .or. node_id > n_nodes) cycle
                if (processed(node_id)) cycle
                processed(node_id) = .true.

                call self%set_state(node_id, i_elem, state, calc_physics=.true.)

                H_j = 0.0d0
                rho_j = 0.0d0
                if (active_thermal) call self%thermal%calc_enthalpy_density(material_id, state, H_j)
                if (active_hydraulic) call self%hydraulic%calc_effective_density_value(state, rho_j)

                enthalpy(node_id) = H_j
                density(node_id) = rho_j
            end do
            nullify (connectivity)
        end do

        deallocate (processed)
    end subroutine compute_nodal_conserved_ftcms

    !> Implementation: one-time domain integral of Theta = Qw + (rho_i/rho_w)*Qi + Qv,
    !> the exact mixed storage variable whose BDF derivative is assembled into the
    !> hydraulic residual (compute_transient_term_mixed_hydraulic). Per element, Theta
    !> is evaluated at every local node (set_state + calc_theta_value) and the element's
    !> contribution to the integral is approximated as (element measure) times the
    !> arithmetic mean of the local nodal values - a first-order lumped quadrature built
    !> from the existing element-measure machinery (domain%calc_measure), matching the
    !> pattern already used by update_variables_ftcms. This need not match the
    !> consistent-Gauss residual assembly to machine precision: M_ref only sets the
    !> scale of the mass_bias_tolerance ratio in the acceptance gate (ftcms_solve.F90),
    !> so an O(1%) lumping error is immaterial. Cost: O(N_elements).
    module function compute_mass_reference_ftcms(self) result(m_ref)
        implicit none
        class(type_ftcms), intent(inout) :: self
        real(real64) :: m_ref

        integer(int32) :: i_elem, num_elem, i_local, node_id, n_local
        integer(int32), pointer, contiguous :: connectivity(:)
        type(type_state) :: state
        real(real64) :: elem_vol, theta_j, theta_sum

        m_ref = 0.0d0
        if (.not. self%is_active_hydraulic()) return

        nullify (connectivity)
        call self%domain%get_num_fe(num_elem)

        do i_elem = 1, num_elem
            call self%domain%get_fe_connectivity(i_elem, connectivity)
            call self%domain%calc_measure(i_elem, elem_vol)

            theta_sum = 0.0d0
            n_local = 0
            do i_local = 1, size(connectivity)
                node_id = connectivity(i_local)
                if (node_id < 1) cycle
                call self%set_state(node_id, i_elem, state, calc_physics=.true.)
                call self%hydraulic%calc_theta_value(state, theta_j)
                theta_sum = theta_sum + theta_j
                n_local = n_local + 1
            end do
            if (n_local > 0) m_ref = m_ref + elem_vol * (theta_sum / real(n_local, real64))
            nullify (connectivity)
        end do

    end function compute_mass_reference_ftcms

    !> See the interface for the mathematical definition. Cost: O(N_dof) per step.
    module function compute_lte_error_ftcms(self) result(error_rel)
        implicit none
        class(type_ftcms), intent(inout) :: self
        real(real64) :: error_rel

        real(real64) :: dt_n, e_thermal, e_hydraulic

        error_rel = -1.0d0
        call self%control%get_dt(dt_n)
        if (dt_n <= 0.0d0) return

        e_thermal = -1.0d0
        e_hydraulic = -1.0d0

        if (self%is_active_thermal()) call physics_lte(self%temperature, self%lte_ydot_prev_thermal, e_thermal)
        if (self%is_active_hydraulic()) call physics_lte(self%pressure, self%lte_ydot_prev_hydraulic, e_hydraulic)

        ! Combine the per-physics relative errors by a maximum (most restrictive).
        if (self%lte_has_prev) error_rel = max(e_thermal, e_hydraulic)

        self%lte_prev_dt = dt_n
        self%lte_has_prev = .true.

    contains

        subroutine physics_lte(var, ydot_prev, e_rel)
            implicit none
            type(type_variable), intent(inout) :: var
            real(real64), allocatable, intent(inout) :: ydot_prev(:)
            real(real64), intent(inout) :: e_rel

            real(real64), pointer, contiguous :: ydot(:), y(:)
            real(real64), allocatable :: dydot(:)
            real(real64) :: lte, ynorm, dt_factor

            e_rel = -1.0d0
            nullify (ydot); nullify (y)
            call var%get_diff(ydot)     ! ydot_n = (y_n - y_{n-1})/dt_n for BDF1
            call var%get_current(y)
            if (.not. (associated(ydot) .and. associated(y))) return

            if (self%lte_has_prev .and. allocated(ydot_prev)) then
                if (size(ydot_prev) == size(ydot) .and. (dt_n + self%lte_prev_dt) > 0.0d0) then
                    allocate (dydot(size(ydot)))
                    dydot(:) = ydot(:) - ydot_prev(:)
                    dt_factor = dt_n * dt_n / (dt_n + self%lte_prev_dt)
                    lte = vector_norm2(dydot) * dt_factor
                    ynorm = vector_norm2(y)
                    e_rel = lte / max(ynorm, tiny(1.0d0))
                    deallocate (dydot)
                end if
            end if

            ! Store the current derivative as the previous for the next step.
            if (allocated(ydot_prev)) then
                if (size(ydot_prev) /= size(ydot)) deallocate (ydot_prev)
            end if
            if (.not. allocated(ydot_prev)) allocate (ydot_prev(size(ydot)))
            ydot_prev(:) = ydot(:)
        end subroutine physics_lte

    end function compute_lte_error_ftcms

    !> See the interface. Cost: O(N_dof). Combines the energy and water residual
    !> blocks by Euclidean norm; used only as a monotone merit for the line search.
    module function nonlinear_residual_norm_ftcms(self) result(rnorm)
        implicit none
        class(type_ftcms), intent(inout) :: self
        real(real64) :: rnorm

        real(real64), allocatable :: res(:)
        real(real64) :: sumsq

        sumsq = 0.0d0
        if (self%is_active_thermal()) then
            call self%get_variable_residual(PHYSICS_TYPES%THERMAL, res)
            if (allocated(res)) then
                if (size(res) > 0) sumsq = sumsq + vector_norm2(res)**2
            end if
        end if
        if (self%is_active_hydraulic()) then
            call self%get_variable_residual(PHYSICS_TYPES%HYDRAULIC, res)
            if (allocated(res)) then
                if (size(res) > 0) sumsq = sumsq + vector_norm2(res)**2
            end if
        end if
        rnorm = sqrt(sumsq)
    end function nonlinear_residual_norm_ftcms

    module subroutine reset_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        call self%control%reset_iteration()

    end subroutine reset_ftcms

    module function is_active_thermal_ftcms(self) result(is_active)
        implicit none
        class(type_ftcms), intent(in) :: self
        logical :: is_active

        is_active = self%control%is_physics_active(PHYSICS_TYPES%THERMAL)

    end function is_active_thermal_ftcms

    module function is_active_hydraulic_ftcms(self) result(is_active)
        implicit none
        class(type_ftcms), intent(in) :: self
        logical :: is_active

        is_active = self%control%is_physics_active(PHYSICS_TYPES%HYDRAULIC)

    end function is_active_hydraulic_ftcms

    module subroutine destroy_type_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        integer(int32) :: log_io_unit
        ! --- Stop and Record Profiler ---
        call self%control%profiler_stop(PROFILER_TYPES%TOTAL)
        call self%control%profiler_record(TIME_RECORDS%END)

        call self%output%output_system_log()
        call self%output%get_log_io_unit(log_io_unit)
        call self%control%display_profiler(log_io_unit)

        if (self%solver_history_unit /= -1) then
            close (self%solver_history_unit)
            self%solver_history_unit = -1
        end if

    end subroutine destroy_type_ftcms
end submodule ftcms_base

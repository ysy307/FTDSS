submodule(app_ftcms) ftcms_base
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

        call self%K%initialize(self%domain, config_control_manager%coupling_mode)
        call self%K%build_scatter_map(self%domain)
        call self%F%initialize(self%domain, config_control_manager%coupling_mode)
        call self%du%initialize(self%domain, config_control_manager%coupling_mode)

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
        call self%Qi_seg%initialize(num_nodes, max_bdf_order)
        call self%Qa%initialize(num_nodes, max_bdf_order)
        call self%Qv%initialize(num_nodes, max_bdf_order)

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

            if (self%is_active_hydraulic() .and. (.not. self%hydraulic_has_dirichlet_bc)) then
                projection_enabled_selected = .true.
                projection_offset_selected = self%hydraulic_start_dof
                call self%K%get_num_dofs_per_node(projection_stride_selected)
                write (*, '(A)') 'Notice: Enabling mean-zero nullspace projection for all-Neumann hydraulic component.'
            end if

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

        ! Capture initial mean pressure to anchor the all-Neumann null-mode
        ! without shifting the absolute level (WRF depends on absolute P).
        if (self%is_active_hydraulic() .and. (.not. self%hydraulic_has_dirichlet_bc)) then
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
        nullify (phase_values)
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

        ! !
        ! call global_logger%log_information(message="FTCMS module initialized successfully.")
    end subroutine initialize_type_ftcms

    module subroutine output_fields_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        integer(int32) :: iter
        real(real64) :: current_time

        real(real64), pointer, contiguous, dimension(:) :: temperature
        real(real64), pointer, contiguous, dimension(:) :: pressure
        real(real64), pointer, contiguous, dimension(:) :: water_content
        real(real64), pointer, contiguous, dimension(:) :: ice_pore
        real(real64), pointer, contiguous, dimension(:) :: ice_seg
        real(real64), allocatable, target :: ice_content(:)
        real(real64), pointer, contiguous, dimension(:) :: vapor_content

        call self%control%profiler_start(PROFILER_TYPES%IO)

        nullify (temperature)
        nullify (pressure)
        nullify (ice_pore)
        nullify (ice_seg)
        nullify (vapor_content)
        nullify (water_content)

        call self%control%get_time(current_time)

        if (self%control%is_output_triggered(OUTPUT_TYPES%FIELD, current_time)) then
            call self%control%get_output_step(OUTPUT_TYPES%FIELD, iter)
            if (self%is_active_thermal()) then
                call self%temperature%get_previous(temperature)
            end if
            if (self%is_active_hydraulic()) then
                call self%pressure%get_previous(pressure)
            end if
            call self%Qw%get_previous(water_content)
            call self%Qi%get_previous(ice_pore)
            call self%Qi_seg%get_previous(ice_seg)
            call self%Qv%get_previous(vapor_content)
            allocate (ice_content(size(ice_pore)))
            ice_content(:) = ice_pore(:) + ice_seg(:)
            call self%output%output_fields(file_counts=iter, &
                                           temperature=temperature, &
                                           water_content=water_content, &
                                           ice_content=ice_content, &
                                           vapor_content=vapor_content, &
                                           pressure=pressure)

            call self%control%update_output(OUTPUT_TYPES%FIELD, current_time)

            deallocate (ice_content)
            nullify (temperature)
            nullify (pressure)
            nullify (ice_pore)
            nullify (ice_seg)
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
        real(real64), pointer, contiguous, dimension(:) :: ice_seg
        real(real64), allocatable, target :: ice_content(:)
        call self%control%profiler_start(PROFILER_TYPES%IO)

        nullify (temperature)
        nullify (pressure)
        nullify (water_content)
        nullify (ice_pore)
        nullify (vapor_content)
        nullify (ice_seg)

        call self%control%get_time(current_time)

        if (self%control%is_output_triggered(OUTPUT_TYPES%HISTORY, current_time)) then
            if (self%is_active_thermal()) then
                call self%temperature%get_previous(temperature)
            end if
            if (self%is_active_hydraulic()) then
                call self%pressure%get_previous(pressure)
            end if
            call self%Qw%get_previous(water_content)
            call self%Qi%get_previous(ice_pore)
            call self%Qv%get_previous(vapor_content)
            call self%Qi_seg%get_previous(ice_seg)
            allocate (ice_content(size(ice_pore)))
            ice_content(:) = ice_pore(:) + ice_seg(:)
            call self%control%get_output_time(OUTPUT_TYPES%HISTORY, current_time, current_time_converted)
            call self%output%output_history(time=current_time_converted, &
                                            temperature=temperature, &
                                            water_content=water_content, &
                                            ice_content=ice_content, &
                                            vapor_content=vapor_content, &
                                            pressure=pressure)
            call self%control%update_output(OUTPUT_TYPES%HISTORY, current_time)

            deallocate (ice_content)
            nullify (water_content)
            nullify (ice_pore)
            nullify (vapor_content)
            nullify (ice_seg)
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
            temperature = min(max(temperature, -80.0d0), 80.0d0)
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
            pressure = min(max(pressure, -1.0d7), 1.0d7)
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
            real(real64) :: qi_seg_val
            call self%Qi_seg%get_current(node_id, qi_seg_val)
            call state%ice_content_seg%set(qi_seg_val)
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
        end if

    end subroutine set_state_ftcms

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
        call self%Qi_seg%advance()
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

        real(real64) :: relaxation_factor
        logical :: is_none

        real(real64) :: max_du, alpha
        real(real64), parameter :: PICARD_MAX_DT_STEP = 2.0d1
        real(real64), parameter :: PICARD_MAX_DP_STEP = 2.0d5
        real(real64), parameter :: TEMP_MIN_C = -80.0d0
        real(real64), parameter :: TEMP_MAX_C = 80.0d0
        real(real64), parameter :: PRESS_MIN_PA = -1.0d7
        real(real64), parameter :: PRESS_MAX_PA = 1.0d7

        call self%control%profiler_start(PROFILER_TYPES%SETUP)

        nullify (current)
        nullify (bdf_coeffs)

        call self%control%get_nonlinear_iter(iter)

        call self%control%get_bdf_coeffs(bdf_order, bdf_coeffs)

        is_none = self%control%is_none()

        if (self%is_active_thermal()) then
            call self%get_variable_increment(PHYSICS_TYPES%THERMAL, du)
            call self%temperature%get_current(current)
            if (associated(current)) then
                call allocate_array(current_prev, size(current))
                current_prev(:) = current(:)

                if (allocated(du) .and. size(du) > 0 .and. &
                    ((.not. allocated(self%solver_thermal)) .or. self%solver_thermal%is_success())) then
                    max_du = maxval(abs(du))
                    write (*, '(A,ES13.5,A,L1)') '   [REFLECT] thermal max|du|=', max_du, ', is_none=', is_none
                    if (.not. is_none) then
                        if (max_du > PICARD_MAX_DT_STEP) then
                            alpha = PICARD_MAX_DT_STEP / max_du
                            write (*, '(A,2(ES13.5,A))') '   [REFLECT] thermal Picard cap active: max|du|=', &
                                max_du, ', alpha=', alpha, ''
                        else
                            alpha = 1.0d0
                        end if
                        call self%control%compute_relaxation(PHYSICS_TYPES%THERMAL, iter, alpha * du, current)
                        call self%control%get_current_relaxation(PHYSICS_TYPES%THERMAL, relaxation_factor)
                        current(:) = current(:) + relaxation_factor * alpha * du(:)
                    else
                        relaxation_factor = 1.0d0
                        current(:) = current(:) + du(:)
                    end if
                end if

                current(:) = min(max(current(:), TEMP_MIN_C), TEMP_MAX_C)
                call self%temperature%set_delta(current(:) - current_prev(:))

                call deallocate_array(current_prev)
            end if

            call self%calc_gradient_temperature()
            call self%temperature%compute_time_derivative(bdf_coeffs, bdf_order)

            call deallocate_array(du)

            ! Zero the stored thermal increment to prevent double-application
            ! when reflect_variables is called again from the hydraulic phase.
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
                    if (.not. is_none) then
                        max_du = maxval(abs(du))
                        if (max_du > PICARD_MAX_DP_STEP) then
                            alpha = PICARD_MAX_DP_STEP / max_du
                            write (*, '(A,2(ES13.5,A))') '   [REFLECT] hydraulic Picard cap active: max|du|=', &
                                max_du, ', alpha=', alpha, ''
                        else
                            alpha = 1.0d0
                        end if
                        call self%control%compute_relaxation(PHYSICS_TYPES%HYDRAULIC, iter, alpha * du, current)
                        call self%control%get_current_relaxation(PHYSICS_TYPES%HYDRAULIC, relaxation_factor)
                        current(:) = current(:) + relaxation_factor * alpha * du(:)
                    else
                        relaxation_factor = 1.0d0
                        current(:) = current(:) + du(:)
                    end if
                end if

                current(:) = min(max(current(:), PRESS_MIN_PA), PRESS_MAX_PA)
                call self%pressure%set_delta(current(:) - current_prev(:))

                call deallocate_array(current_prev)
            end if

            call self%calc_gradient_pressure()
            call self%pressure%compute_time_derivative(bdf_coeffs, bdf_order)

            call deallocate_array(du)

            ! Zero the stored hydraulic increment to prevent double-application.
            if (self%control%is_staggered()) then
                block
                    real(real64), pointer :: du_raw(:) => null()
                    du_raw => self%du%get_data(PHYSICS_TYPES%HYDRAULIC%ID)
                    if (associated(du_raw)) du_raw(:) = 0.0d0
                    nullify (du_raw)
                end block
            end if
        end if

        call self%control%profiler_stop(PROFILER_TYPES%SETUP)

    end subroutine reflect_variables_ftcms

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
#ifdef _MPI
        integer(int32) :: ierr
#endif

        ! --- Stop and Record Profiler ---
        call self%control%profiler_stop(PROFILER_TYPES%TOTAL)
        call self%control%profiler_record(TIME_RECORDS%END)

        call self%output%output_system_log()
        call self%output%get_log_io_unit(log_io_unit)
        call self%control%display_profiler(log_io_unit)

#ifdef _MPI
        call MPI_Finalize(ierr)
#endif

    end subroutine destroy_type_ftcms
end submodule ftcms_base

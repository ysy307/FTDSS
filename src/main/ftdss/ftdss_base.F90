submodule(main_ftdss) ftdss_base
    implicit none
contains

    module subroutine initialize_type_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        type(type_input) :: input
        type(type_ic_manager) :: ic

        integer(int32) :: max_bdf_order
        integer(int32), allocatable :: active_region_ids(:)
        integer(int32) :: num_nodes
        character(len=10), allocatable :: profiler_labels(:)
        real(real64) :: current_time
        integer(int32) :: computation_dimension
        integer(int32) :: num_total_dofs
        integer(int32) :: ierr

        type(type_solver_settings) :: solver_info
        type(type_preconditioner_settings) :: pc_info

        profiler_labels = [character(len=10) :: "IO", "Setup", "Assemble", "Solve", "Total"]
        call self%controls%profiler%initialize(profiler_labels)
        call self%controls%profiler%record(TIME_RECORD_START)
        call self%controls%profiler%start("Total")

        ! call setup_handler()

        
        call self%controls%profiler%start("IO")
        call input%initialize()
        call self%controls%profiler%stop("IO")
        call self%controls%initialize(input)
        call ic%initialize(input)

        if (input%output_settings%standard_output%print_progress) then
            call global_logger%configure(level=information_level, &
                                         time_stamp=.true., &
                                         max_width=0)
        else
            call global_logger%configure(level=warning_level, &
                                         time_stamp=.true., &
                                         max_width=0)
        end if

        num_nodes = input%geometry%vtk%num_points
        call self%domain%initialize(input, self%controls)
        call self%domain%get_total_dofs(num_total_dofs)

        call self%K%initialize(self%domain)
        call self%F%initialize(self%domain)
        call self%du%initialize(self%domain)

        max_bdf_order = input%basic%solver_settings%bdf_order
        call self%porosity%initialize(num_nodes, max_bdf_order)
        call ic%apply(IC_TARGET_POROSITY, self%porosity)

        if (self%is_active_thermal()) then
            call self%temperature%initialize(num_nodes, max_bdf_order)
            call ic%apply(IC_TARGET_THERMAL, self%temperature)
        end if

        if (self%is_active_hydraulic()) then
            call self%pressure%initialize(num_nodes, max_bdf_order)
            call ic%apply(IC_TARGET_HYDRAULIC, self%pressure)
        end if

        call self%Qw%initialize(num_nodes, max_bdf_order)
        call self%Qi%initialize(num_nodes, max_bdf_order)
        call self%Qa%initialize(num_nodes, max_bdf_order)
        call self%Qv%initialize(num_nodes, max_bdf_order)

        call self%domain%get_computation_dimension(computation_dimension)
        call input%geometry%vtk%get_active_region_info(active_region_ids, target_dim=computation_dimension)

        call self%thermal%initialize(input, active_region_ids)
        call self%hydraulic%initialize(input, active_region_ids)

        ! ソルバーの初期化
        associate (solver_settings => input%basic%solver_settings%linear_solver)
            call solver_info%set(solver_settings%solver_type, &
                                 num_total_dofs, &
                                 solver_settings%tolerance, &
                                 solver_settings%max_iterations, &
                                 solver_settings%m_restarts)
            call pc_info%set(solver_settings%preconditioner_type, num_total_dofs)
            call create_solver(self%solver, solver_info, pc_info, ierr)
        end associate

        ! 初期化時にBCを適用（Dirichlet値をフィールドに設定）
        call self%apply_bc()

        call self%output%initialize(input, self%controls, self%domain)
        call self%output_fields()
        call self%output_history()

        !
        call global_logger%log_information(message="FTDSS module initialized successfully.")
    end subroutine initialize_type_ftdss

    module subroutine output_fields_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        integer(int32) :: iter
        real(real64) :: current_time

        real(real64), pointer, contiguous, dimension(:) :: porosity => null()
        real(real64), pointer, contiguous, dimension(:) :: temperature => null()
        real(real64), pointer, contiguous, dimension(:) :: pressure => null()
        real(real64), pointer, contiguous, dimension(:) :: ice_content => null()

        call self%controls%profiler%start("IO")

        call self%controls%time%get_time(current_time)

        if (self%controls%out_field%is_due(current_time)) then
            call self%controls%out_field%get_step(iter)
            call self%porosity%get_previous(porosity)
            if (self%is_active_thermal()) then
                call self%temperature%get_previous(temperature)
            end if
            if (self%is_active_hydraulic()) then
                call self%pressure%get_previous(pressure)
            end if
            call self%Qi%get_previous(ice_content)
            call self%output%output_fields(iter, self%domain, porosity, &
                                           temperature, ice_content, pressure)
            call self%controls%out_field%update(current_time)

            nullify (porosity)
            nullify (temperature)
            nullify (pressure)
            nullify (ice_content)
        end if

        call self%controls%profiler%stop("IO")
    end subroutine output_fields_ftdss

    module subroutine output_history_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        real(real64) :: current_time, current_time_converted
        real(real64), pointer, contiguous, dimension(:) :: porosity => null()
        real(real64), pointer, contiguous, dimension(:) :: temperature => null()
        real(real64), pointer, contiguous, dimension(:) :: pressure => null()

        call self%controls%profiler%start("IO")

        call self%controls%time%get_time(current_time)

        if (self%controls%out_history%is_due(current_time)) then
            call self%porosity%get_previous(porosity)
            if (self%is_active_thermal()) then
                call self%temperature%get_previous(temperature)
            end if
            if (self%is_active_hydraulic()) then
                call self%pressure%get_previous(pressure)
            end if
            current_time_converted = self%controls%out_history%convert_output_time(current_time)
            call self%output%output_history(current_time_converted, self%domain, porosity, &
                                            temperature, pressure)
            call self%controls%out_history%update(current_time)

            nullify (porosity)
            nullify (temperature)
            nullify (pressure)
        end if

        call self%controls%profiler%stop("IO")
    end subroutine output_history_ftdss

    module subroutine get_variable_increment_ftdss(self, variable_id, variable)
        implicit none
        class(type_ftdss), intent(inout) :: self
        type(type_constant_id), intent(in) :: variable_id
        real(real64), intent(inout), allocatable :: variable(:)

        real(real64), pointer, contiguous, dimension(:) :: du => null()

        integer(int32) :: target_dof
        integer(int32) :: num_nodes, num_dofs_per_node
        integer(int32) :: start_idx, end_idx

        call deallocate_array(variable)

        if (.not. PHYSICS_TYPES%is_valid(variable_id)) then
            call allocate_array(variable, 0)
            return
        end if

        du => self%du%get_data()
        if (.not. associated(du)) then
            call allocate_array(variable, 0)
            return
        end if

        if (self%controls%is_physics_active(variable_id)) then
            call self%domain%get_num_nodes(num_nodes)
            call self%domain%get_num_dofs_per_node(num_dofs_per_node)
            call self%domain%get_target_dof(variable_id%id, target_dof)

            call allocate_array(variable, num_nodes)

            start_idx = target_dof
            end_idx = num_dofs_per_node * (num_nodes - 1) + target_dof

            variable(:) = du(start_idx:end_idx:num_dofs_per_node)
        else
            call allocate_array(variable, 0)
        end if

        nullify (du)

    end subroutine get_variable_increment_ftdss

    module subroutine get_variable_residual_ftdss(self, variable_id, variable)
        implicit none
        class(type_ftdss), intent(inout) :: self
        type(type_constant_id), intent(in) :: variable_id
        real(real64), intent(inout), allocatable :: variable(:)

        real(real64), pointer, contiguous, dimension(:) :: F => null()

        integer(int32) :: target_dof
        integer(int32) :: num_nodes, num_dofs_per_node
        integer(int32) :: start_idx, end_idx

        call deallocate_array(variable)

        if (.not. PHYSICS_TYPES%is_valid(variable_id)) then
            call allocate_array(variable, 0)
            return
        end if

        F => self%F%get_data()
        if (.not. associated(F)) then
            call allocate_array(variable, 0)
            return
        end if

        if (self%controls%is_physics_active(variable_id)) then
            call self%domain%get_num_nodes(num_nodes)
            call self%domain%get_num_dofs_per_node(num_dofs_per_node)
            call self%domain%get_target_dof(variable_id%id, target_dof)

            call allocate_array(variable, num_nodes)

            start_idx = target_dof
            end_idx = num_dofs_per_node * (num_nodes - 1) + target_dof

            variable(:) = F(start_idx:end_idx:num_dofs_per_node)
        else
            call allocate_array(variable, 0)
        end if

    end subroutine get_variable_residual_ftdss

    module subroutine set_state_ftdss(self, node_id, element_id, state)
        implicit none
        class(type_ftdss), intent(inout) :: self
        integer(int32), intent(in) :: node_id
        integer(int32), intent(in) :: element_id
        type(type_state), intent(inout) :: state

        integer(int32) :: material_id
        integer(int32) :: bdf_order
        real(real64) :: temperature, pressure, porosity
        type(type_coordinate_dp) :: grad_T, grad_P
        type(type_coordinate_dp) :: water_flux, vapor_flux
        real(real64) :: K_wT, K_wP, K_vT, K_vP
        real(real64) :: temperature_history(8), pressure_history(8), porosity_history(8)
        ! real(real64), pointer, contiguous, dimension(:) :: temperature_history => null()
        ! real(real64), pointer, contiguous, dimension(:) :: pressure_history => null()
        ! real(real64), pointer, contiguous, dimension(:) :: porosity_history => null()

        call state%reset()

        call self%controls%time%get_bdf_order(bdf_order)

        if (self%controls%is_physics_active(PHYSICS_TYPES%THERMAL)) then
            call self%temperature%get_current(node_id, temperature)
            call self%temperature%get_current_gradient(node_id, grad_T)
            call self%temperature%get_history(node_id, temperature_history)
            call state%set(temperature=temperature, &
                           grad_T=grad_T, &
                           temperature_history=temperature_history(1:bdf_order + 1))
        end if
        if (self%controls%is_physics_active(PHYSICS_TYPES%HYDRAULIC)) then
            call self%pressure%get_current(node_id, pressure)
            call self%pressure%get_current_gradient(node_id, grad_P)
            call self%pressure%get_history(node_id, pressure_history)
            call state%set(pressure=pressure, &
                           grad_P=grad_P, &
                           pressure_history=pressure_history(1:bdf_order + 1))
        end if
        call self%porosity%get_current(node_id, porosity)
        call self%porosity%get_history(node_id, porosity_history)
        call state%set(porosity=porosity, &
                       porosity_history=porosity_history(1:bdf_order + 1))

        call self%domain%get_material_id(element_id, material_id)
        call self%thermal%update_water_phases(material_id, state)

        if (self%controls%is_target(PHYSICS_TYPE_HYDRAULIC, material_id)) then
            call self%calc_water_flux(material_id, state, grad_T, grad_P, water_flux)
            call self%calc_vapor_flux(material_id, state, grad_T, grad_P, vapor_flux)
        else
            call water_flux%set(0.0d0, 0.0d0, 0.0d0)
            call vapor_flux%set(0.0d0, 0.0d0, 0.0d0)
        end if

        call state%set(water_flux=water_flux, vapor_flux=vapor_flux)
    end subroutine set_state_ftdss

    module subroutine shift_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        call self%controls%profiler%start("Setup")

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

        call self%controls%profiler%stop("Setup")
    end subroutine shift_ftdss

    module subroutine reflect_variables_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        integer(int32) :: iter
        real(real64), pointer, dimension(:) :: bdf_coeffs => null()
        integer(int32) :: bdf_order
        real(real64), pointer, contiguous, dimension(:) :: current => null()
        real(real64), allocatable :: du(:)

        real(real64) :: relaxation_factor
        logical :: is_none

        call self%controls%profiler%start("Setup")

        call self%controls%iteration%get_nonlinear_iter(iter)

        call self%controls%time%get_bdf_coeffs(bdf_coeffs)
        call self%controls%time%get_bdf_order(bdf_order)

        is_none = self%controls%iteration%is_none()

        if (self%is_active_thermal()) then
            call self%get_variable_increment(PHYSICS_TYPES%THERMAL, du)
            call self%temperature%get_current(current)
            if (associated(current)) then
                if (.not. is_none) then
                    if (iter > 1) then
                        call self%controls%aitken%compute_relaxation(PHYSICS_TYPES%THERMAL, du)
                    end if
                    call self%controls%aitken%get_relaxation(PHYSICS_TYPES%THERMAL, relaxation_factor)
                    write (*, '("   [Aitken] Iter:", I3, " Omega:", F6.4)') iter, relaxation_factor
                else
                    relaxation_factor = 1.0d0
                end if
                current(:) = current(:) + relaxation_factor * du(:)
                call self%temperature%set_delta(relaxation_factor * du(:))
                if (.not. is_none) then
                    call self%controls%aitken%set_du(PHYSICS_TYPES%THERMAL, du)
                end if
            end if

            call self%calc_gradient_temperature()
            call self%temperature%compute_time_derivative(bdf_coeffs, bdf_order)

            call deallocate_array(du)
        end if

        if (self%is_active_hydraulic()) then
            call self%get_variable_increment(PHYSICS_TYPES%HYDRAULIC, du)
            call self%pressure%get_current(current)
            if (associated(current)) then
                if (.not. is_none) then
                    if (iter > 1) then
                        call self%controls%aitken%compute_relaxation(PHYSICS_TYPES%HYDRAULIC, du)
                    end if
                    call self%controls%aitken%get_relaxation(PHYSICS_TYPES%HYDRAULIC, relaxation_factor)
                    write (*, '("   [Aitken] Iter:", I3, " Omega:", F6.4)') iter, relaxation_factor
                else
                    relaxation_factor = 1.0d0
                end if
                current(:) = current(:) + relaxation_factor * du(:)
                call self%pressure%set_delta(relaxation_factor * du(:))
                if (.not. is_none) then
                    call self%controls%aitken%set_du(PHYSICS_TYPES%HYDRAULIC, du)
                end if
            end if

            call self%calc_gradient_pressure()
            call self%pressure%compute_time_derivative(bdf_coeffs, bdf_order)

            call deallocate_array(du)
        end if

        call self%controls%profiler%stop("Setup")

    end subroutine reflect_variables_ftdss

    module subroutine reset_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        call self%controls%iteration%reset()

    end subroutine reset_ftdss

    module function is_active_thermal_ftdss(self) result(is_active)
        implicit none
        class(type_ftdss), intent(in) :: self
        logical :: is_active

        is_active = self%controls%is_physics_active(PHYSICS_TYPES%THERMAL)

    end function is_active_thermal_ftdss

    module function is_active_hydraulic_ftdss(self) result(is_active)
        implicit none
        class(type_ftdss), intent(in) :: self
        logical :: is_active

        is_active = self%controls%is_physics_active(PHYSICS_TYPES%HYDRAULIC)

    end function is_active_hydraulic_ftdss

    module subroutine finalize_type_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        call self%controls%profiler%stop("Total")
        call self%controls%profiler%record(TIME_RECORD_END)
        call self%output%output_system_log(self%controls)

    end subroutine finalize_type_ftdss
end submodule ftdss_base

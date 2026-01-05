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
        integer(int32) :: num_total_dofs
        integer(int32) :: ierr

        type(type_solver_settings) :: matrix_info
        type(type_preconditioner_settings) :: pc_info

        profiler_labels = [character(len=10) :: "IO", "Setup", "Assemble", "Solve", "Total"]
        call self%controls%profiler%initialize(profiler_labels)
        call self%controls%profiler%record(TIME_RECORD_START)
        call self%controls%profiler%start("Total")

        call setup_handler()

        call self%controls%profiler%stop("IO")
        call input%initialize()
        call self%controls%profiler%start("IO")
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
        num_total_dofs = self%domain%get_total_dofs()

        call self%J%initialize(self%domain)
        call self%R%initialize(self%domain)
        call self%delta%initialize(self%domain)

        max_bdf_order = input%basic%solver_settings%bdf_order
        call self%porosity%initialize(num_nodes, max_bdf_order)
        call ic%apply(IC_TARGET_POROSITY, self%porosity)

        if (self%controls%is_physics_active(PHYSICS_TYPE_THERMAL)) then
            call self%temperature%initialize(num_nodes, max_bdf_order)
            call ic%apply(IC_TARGET_THERMAL, self%temperature)
        end if

        if (self%controls%is_physics_active(PHYSICS_TYPE_HYDRAULIC)) then
            call self%pressure%initialize(num_nodes, max_bdf_order)
            call ic%apply(IC_TARGET_HYDRAULIC, self%pressure)
        end if

        call self%Qw%initialize(num_nodes, max_bdf_order)
        call self%Qi%initialize(num_nodes, max_bdf_order)
        call self%Qa%initialize(num_nodes, max_bdf_order)
        call self%Qv%initialize(num_nodes, max_bdf_order)

        call input%geometry%vtk%get_active_region_info(active_region_ids, target_dim=self%domain%get_computation_dimension())

        call self%thermal%initialize(input, active_region_ids)
        call self%hydraulic%initialize(input, active_region_ids)

        ! ソルバーの初期化
        associate (solver_settings => input%basic%solver_settings%linear_solver)
            call matrix_info%set(solver_settings%solver_type, &
                                 num_total_dofs, &
                                 solver_settings%tolerance, &
                                 solver_settings%max_iterations, &
                                 solver_settings%m_restarts)
            call pc_info%set(solver_settings%preconditioner_type, num_total_dofs)
            call create_solver(self%solver, matrix_info, pc_info, ierr)
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

        call self%controls%profiler%start("IO")

        call self%controls%time%get_time(current_time)

        if (self%controls%out_field%is_due(current_time)) then
            call self%controls%out_field%get_step(iter)
            call self%output%output_fields(iter, self%domain, self%porosity%pre, &
                                           self%temperature%pre, self%Qw%pre, self%pressure%pre)
            call self%controls%out_field%update(current_time)
        end if

        call self%controls%profiler%stop("IO")
    end subroutine output_fields_ftdss

    module subroutine output_history_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        real(real64) :: current_time

        call self%controls%profiler%start("IO")

        call self%controls%time%get_time(current_time)

        if (self%controls%out_history%is_due(current_time)) then
            call self%output%output_history(current_time, self%domain, self%porosity%pre, &
                                            self%temperature%pre, self%pressure%pre)
            call self%controls%out_history%update(current_time)
        end if

        call self%controls%profiler%stop("IO")
    end subroutine output_history_ftdss

    module subroutine set_state_ftdss(self, node_id, element_id, state)
        implicit none
        class(type_ftdss), intent(inout) :: self
        integer(int32), intent(in) :: node_id
        integer(int32), intent(in) :: element_id
        type(type_state), intent(inout) :: state

        integer(int32) :: material_id
        type(type_coordinate_dp) :: grad_T, grad_P
        type(type_coordinate_dp) :: water_flux, vapor_flux
        real(real64) :: K_wT, K_wP, K_vT, K_vP

        call self%controls%profiler%start("Setup")

        call state%reset()

        grad_T%x = self%temperature%grad%x(node_id)
        grad_T%y = self%temperature%grad%y(node_id)
        grad_T%z = self%temperature%grad%z(node_id)
        grad_P%x = self%pressure%grad%x(node_id)
        grad_P%y = self%pressure%grad%y(node_id)
        grad_P%z = self%pressure%grad%z(node_id)

        call state%set(temperature=self%temperature%new(node_id), &
                       pressure=self%pressure%new(node_id), &
                       porosity=self%porosity%new(node_id), &
                       dot_T=self%temperature%dif(node_id), &
                       dot_P=self%pressure%dif(node_id), &
                       grad_T=grad_T, &
                       grad_P=grad_P)

        call self%domain%get_material_id(element_id, material_id)
        if (self%controls%is_target(PHYSICS_TYPE_HYDRAULIC, material_id)) then
            call self%calc_water_flux(material_id, state, grad_T, grad_P, water_flux)
            call self%calc_vapor_flux(material_id, state, grad_T, grad_P, vapor_flux)
            call state%set(water_flux=water_flux, vapor_flux=vapor_flux)
        end if

        call self%thermal%update_water_phases(material_id, state)

        call self%controls%profiler%stop("Setup")

    end subroutine set_state_ftdss

    module subroutine shift_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        call self%controls%profiler%start("Setup")

        if (self%controls%is_physics_active(PHYSICS_TYPE_THERMAL)) then
            call self%temperature%shift()
        end if

        if (self%controls%is_physics_active(PHYSICS_TYPE_HYDRAULIC)) then
            call self%pressure%shift()
        end if

        call self%porosity%shift()
        call self%Qw%shift()
        call self%Qi%shift()
        call self%Qa%shift()
        call self%Qv%shift()

        call self%controls%time%shift()

        call self%controls%profiler%stop("Setup")
    end subroutine shift_ftdss

    module subroutine reflect_variables_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        type(type_vector_dp), pointer :: delta_prt => null()
        real(real64), pointer, dimension(:) :: data => null()
        integer(int32) :: target_dof
        integer(int32) :: start_idx, end_idx, num_nondes, num_dofs_per_nonde
        real(real64), pointer, dimension(:) :: time_coef => null()

        call self%controls%profiler%start("Setup")

        delta_prt => self%delta%get_vector()
        data => delta_prt%get_data()

        num_nondes = self%domain%get_num_nodes()
        num_dofs_per_nonde = self%domain%get_num_dofs_per_node()

        call self%controls%time%get_bdf_coeffs(time_coef)

        if (self%controls%is_physics_active(PHYSICS_TYPE_THERMAL)) then
            call self%domain%get_target_dof(PHYSICS_TYPE_THERMAL, target_dof)
            start_idx = target_dof
            end_idx = num_dofs_per_nonde * (num_nondes - 1) + target_dof
            self%temperature%new(:) = self%temperature%new(:) + data(start_idx:end_idx:num_dofs_per_nonde)
            call self%calc_gradient_temperature()
            call self%temperature%compute_derivative(time_coef)
        end if

        if (self%controls%is_physics_active(PHYSICS_TYPE_HYDRAULIC)) then
            call self%domain%get_target_dof(PHYSICS_TYPE_HYDRAULIC, target_dof)
            start_idx = target_dof
            end_idx = num_dofs_per_nonde * (num_nondes - 1) + target_dof
            self%pressure%new(:) = self%pressure%new(:) + data(start_idx:end_idx:num_dofs_per_nonde)
            call self%calc_gradient_pressure()
            call self%pressure%compute_derivative(time_coef)
        end if

        call self%controls%profiler%stop("Setup")

    end subroutine reflect_variables_ftdss

end submodule ftdss_base

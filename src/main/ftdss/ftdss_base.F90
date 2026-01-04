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
        call self%controls%profiler%start("IO")

        call setup_handler()

        call input%initialize()
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

        call self%output%output_fields(0, self%domain, self%porosity%pre, &
                                       self%temperature%pre, self%Qw%pre, self%pressure%pre)
        call self%controls%time%get_time(current_time)
        call self%output%output_history(current_time, self%domain, self%porosity%pre, &
                                        self%temperature%pre, self%pressure%pre)

        call self%controls%profiler%stop("IO")
        call global_logger%log_information(message="FTDSS module initialized successfully.")
    end subroutine initialize_type_ftdss

end submodule ftdss_base

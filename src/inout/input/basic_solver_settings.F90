submodule(inout_input_basic) inout_input_basic_solver_settings
    use omp_lib, only: omp_get_max_threads
    implicit none
    !!------------------------------------------------------------------------------------------------------------------------------
    ! JSON key names for solver settings
    !!------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: solver_settings = "solver_settings"
    character(*), parameter :: bdf_order = "bdf_order"
    character(*), parameter :: nonlinear_solver = "nonlinear_solver"
    character(*), parameter :: method = "method"
    character(len=16), parameter :: valid_nonlinear_solver_methods(4) = &
                                    [character(len=16) :: "none", "newton", "modified_newton", "picard"]
    character(*), parameter :: update_frequency = "update_frequency"
    character(*), parameter :: max_iterations = "max_iterations"
    character(*), parameter :: convergence = "convergence"
    character(*), parameter :: use_criteria = "use_criteria"
    character(len=16), parameter :: valid_criteria_types(3) = [character(len=16) :: "residual", "update", "both"]
    character(*), parameter :: logic_between_criteria = "logic_between_criteria"
    character(len=16), parameter :: valid_logic_types(2) = [character(len=16) :: "and", "or"]
    character(*), parameter :: residual = "residual"
    character(*), parameter :: update = "update"
    character(*), parameter :: criteria = "criteria"
    character(len=16), parameter :: valid_local_criteria_types(3) = [character(len=16) :: "absolute", "relative", "both"]
    character(*), parameter :: logic = "logic"
    character(*), parameter :: absolute_tolerance = "absolute_tolerance"
    character(*), parameter :: relative_tolerance = "relative_tolerance"
    character(*), parameter :: linear_solver = "linear_solver"
    character(len=16), parameter :: valid_linear_solver_methods(2) = [character(len=16) :: "direct", "iterative"]
    character(*), parameter :: iterative_solver = "iterative_solver"
    character(*), parameter :: solver_type = "solver_type"
    character(*), parameter :: preconditioner_type = "preconditioner_type"
    character(*), parameter :: tolerance = "tolerance"
    character(*), parameter :: parallel_settings = "parallel_settings"
    character(*), parameter :: threads = "threads"
    character(*), parameter :: is_parallel = "is_parallel"
    character(*), parameter :: num_threads = "num_threads"
    character(*), parameter :: schedule = "schedule"
    character(len=16), parameter :: valid_schedule_types(6) = &
                                    [character(len=16) :: "affinity", "auto", "dynamic", "guided", "runtime", "static"]
    character(*), parameter :: max_active_levels = "max_active_levels"
    character(*), parameter :: thermal = "thermal"
    character(*), parameter :: hydraulic = "hydraulic"
    character(*), parameter :: mechanical = "mechanical"

contains

    module subroutine read_parameters_solver_settings(self, json)
        implicit none
        class(type_input_basic), intent(inout) :: self
        type(json_file), intent(inout) :: json

        character(256) :: buffer(2)

        buffer(1) = solver_settings
        buffer(2) = bdf_order
        call get_json_value(json, join(buffer), self%solver_settings%bdf_order, &
                            is_required=.true., default_value=1, valid_range=[1, 6])

        call read_parameters_solver_settings_nonlinear(self, json)
        call read_parameters_solver_settings_linear(self, json)
        call read_parameters_solver_parallel_settings(self, json)

    end subroutine read_parameters_solver_settings

    subroutine read_parameters_solver_settings_nonlinear(self, json)
        implicit none
        class(type_input_basic), intent(inout) :: self
        type(json_file), intent(inout) :: json
        character(256) :: buffer(4)

        buffer(1) = solver_settings
        buffer(2) = nonlinear_solver

        buffer(3) = method
        call get_json_value(json, join(buffer(1:3)), self%solver_settings%nonlinear_solver%method, &
                            is_required=.true., default_value="none", valid_list=valid_nonlinear_solver_methods)

        if (self%solver_settings%nonlinear_solver%method == valid_nonlinear_solver_methods(3)) then
            buffer(3) = update_frequency
            call get_json_value(json, join(buffer(1:3)), self%solver_settings%nonlinear_solver%update_frequency, &
                                is_required=.true., default_value=5, valid_range=[1, huge(1)])
        end if

        if (any(self%solver_settings%nonlinear_solver%method == valid_nonlinear_solver_methods(2:4))) then
            buffer(3) = max_iterations
            call get_json_value(json, join(buffer(1:3)), self%solver_settings%nonlinear_solver%max_iterations, &
                                is_required=.true., default_value=1000, valid_range=[1, huge(1)])

            buffer(3) = convergence
            buffer(4) = use_criteria
            call get_json_value(json, join(buffer), self%solver_settings%nonlinear_solver%convergence%use_criteria, &
                                is_required=.true., default_value="both", valid_list=valid_criteria_types)

            if (self%solver_settings%nonlinear_solver%convergence%use_criteria == "both") then
                buffer(4) = logic_between_criteria
                call get_json_value(json, join(buffer), self%solver_settings%nonlinear_solver%convergence%use_logic, &
                                    is_required=.true., default_value="and", valid_list=valid_logic_types)
            end if

            if (any(self%solver_settings%nonlinear_solver%convergence%use_criteria == valid_criteria_types([1, 3]))) then
                buffer(4) = residual
                call read_parameters_solver_settings_nonlinear_convergence( &
                    self%solver_settings%nonlinear_solver%convergence%residual, json, buffer, 4)
            end if
            if (any(self%solver_settings%nonlinear_solver%convergence%use_criteria == valid_criteria_types([2, 3]))) then
                buffer(4) = update
                call read_parameters_solver_settings_nonlinear_convergence( &
                    self%solver_settings%nonlinear_solver%convergence%update, json, buffer, 4)
            end if
        end if
    end subroutine read_parameters_solver_settings_nonlinear

    subroutine read_parameters_solver_settings_nonlinear_convergence(convergence_obj, json, buffer, end_index)
        implicit none
        type(type_convergence_criteria), intent(inout) :: convergence_obj
        type(json_file), intent(inout) :: json
        character(*), intent(in) :: buffer(:)
        integer(int32), intent(in) :: end_index
        character(len=256), allocatable :: local_buffer(:)

        allocate (local_buffer(size(buffer) + 1))
        local_buffer(1:end_index) = buffer(1:end_index)

        local_buffer(end_index + 1) = criteria
        call get_json_value(json, join(local_buffer), convergence_obj%criteria, &
                            is_required=.true., default_value="both", valid_list=valid_local_criteria_types)

        if (convergence_obj%criteria == "both") then
            local_buffer(end_index + 1) = logic
            call get_json_value(json, join(local_buffer), convergence_obj%logic, &
                                is_required=.true., default_value="and", valid_list=valid_logic_types)
        end if

        if (any(convergence_obj%criteria == valid_local_criteria_types([1, 3]))) then
            local_buffer(end_index + 1) = absolute_tolerance
            call get_json_value(json, join(local_buffer), convergence_obj%absolute_tolerance, &
                                is_required=.true., default_value=1.0d-6, valid_range=[0.0d0, huge(0.0d0)])
        end if
        if (any(convergence_obj%criteria == valid_local_criteria_types([2, 3]))) then
            local_buffer(end_index + 1) = relative_tolerance
            call get_json_value(json, join(local_buffer), convergence_obj%relative_tolerance, &
                                is_required=.true., default_value=1.0d-6, valid_range=[0.0d0, huge(0.0d0)])
        end if
    end subroutine read_parameters_solver_settings_nonlinear_convergence

    subroutine read_parameters_solver_settings_linear(self, json)
        implicit none
        class(type_input_basic), intent(inout) :: self
        type(json_file), intent(inout) :: json
        character(256) :: buffer(3)

        buffer(1) = solver_settings
        buffer(2) = linear_solver

        if (self%analysis_controls%calculate_thermal) then
            buffer(3) = thermal
            call read_parameters_solver_settings_linear_local(self%solver_settings%linear_solver%thermal, json, buffer, 3)
        end if
        if (self%analysis_controls%calculate_hydraulic) then
            buffer(3) = hydraulic
            call read_parameters_solver_settings_linear_local(self%solver_settings%linear_solver%hydraulic, json, buffer, 3)
        end if
        if (self%analysis_controls%calculate_mechanical) then
            buffer(3) = mechanical
            call read_parameters_solver_settings_linear_local(self%solver_settings%linear_solver%mechanical, json, buffer, 3)
        end if
    end subroutine read_parameters_solver_settings_linear

    subroutine read_parameters_solver_settings_linear_local(solver_setting, json, buffer, end_index)
        implicit none
        type(type_linear_solver_settings), intent(inout) :: solver_setting
        type(json_file), intent(inout) :: json
        character(*), intent(in) :: buffer(:)
        integer(int32), intent(in) :: end_index
        character(len=256), allocatable :: local_buffer(:)

        allocate (local_buffer(size(buffer) + 2))
        local_buffer(1:end_index) = buffer(1:end_index)

        local_buffer(end_index + 1) = method
        call get_json_value(json, join(local_buffer(1:end_index + 1)), solver_setting%method, &
                            is_required=.true., valid_list=valid_linear_solver_methods)

        if (solver_setting%method == "iterative") then
            local_buffer(end_index + 1) = iterative_solver

            local_buffer(end_index + 2) = solver_type
            call get_json_value(json, join(local_buffer), solver_setting%iterative_solver%solver_type, is_required=.true.)
            local_buffer(end_index + 2) = preconditioner_type
            call get_json_value(json, join(local_buffer), solver_setting%iterative_solver%preconditioner_type, is_required=.true.)
            local_buffer(end_index + 2) = max_iterations
            call get_json_value(json, join(local_buffer), solver_setting%iterative_solver%max_iterations, &
                                is_required=.true., default_value=10000, valid_range=[1, huge(1)])
            local_buffer(end_index + 2) = tolerance
            call get_json_value(json, join(local_buffer), solver_setting%iterative_solver%tolerance, &
                                is_required=.true., default_value=1.0d-6, valid_range=[0.0d0, huge(0.0d0)])
        end if
    end subroutine read_parameters_solver_settings_linear_local

    subroutine read_parameters_solver_parallel_settings(self, json)
        implicit none
        class(type_input_basic), intent(inout) :: self
        type(json_file), intent(inout) :: json
        character(256) :: buffer(4)

        buffer(1) = solver_settings
        buffer(2) = parallel_settings
        buffer(3) = threads

        buffer(4) = is_parallel
        call get_json_value(json, join(buffer), self%solver_settings%parallel_settings%threads%is_parallel, &
                            is_required=.true., default_value=.false.)

        if (self%solver_settings%parallel_settings%threads%is_parallel) then
            buffer(4) = num_threads
            call get_json_value(json, join(buffer), self%solver_settings%parallel_settings%threads%num_threads, &
                                is_required=.true., valid_range=[1, huge(1)])

            ! Cap the number of threads to the maximum available if OpenMP is used
            if (self%solver_settings%parallel_settings%threads%num_threads > omp_get_max_threads()) then
                call global_logger%log_warning( &
                    message="Number of threads exceeds available threads. Using maximum available threads.")
                self%solver_settings%parallel_settings%threads%num_threads = omp_get_max_threads()
            end if

            buffer(4) = schedule
            call get_json_value(json, join(buffer), self%solver_settings%parallel_settings%threads%schedule, &
                                is_required=.true., default_value="static", valid_list=valid_schedule_types)

            buffer(4) = max_active_levels
            call get_json_value(json, join(buffer), self%solver_settings%parallel_settings%threads%max_active_levels, &
                                is_required=.true., default_value=1, valid_range=[1, huge(1)])
        end if
    end subroutine read_parameters_solver_parallel_settings

!======================================================================
! Display Routines for Solver Settings (Refactored)
!======================================================================

    module subroutine display_parameters_solver_settings(self)
        !> Display all solver settings. This is the main entry point.
        implicit none
        class(type_solver_settings), intent(in) :: self

        write (*, '(/a)') "======================================================================"
        write (*, '(a)') "                      Solver Settings"
        write (*, '(a)') "======================================================================"

        write (*, '(a, i0)') "  BDF Order             : ", self%bdf_order

        call display_solver_settings_nonlinear(self%nonlinear_solver)
        call display_solver_settings_linear(self%linear_solver)
        call display_solver_settings_parallel(self%parallel_settings)

        write (*, '(a)') "======================================================================"
    end subroutine display_parameters_solver_settings

    subroutine display_solver_settings_nonlinear(nonlinear)
        !> Displays nonlinear solver settings.
        implicit none
        type(type_nonlinear_solver), intent(in) :: nonlinear

        write (*, '(/a)') "  --- Nonlinear Solver ---"
        write (*, '(a, a)') "    Method                : ", trim(nonlinear%method)

        if (nonlinear%method == valid_nonlinear_solver_methods(3)) then ! "modified_newton"
            write (*, '(a, i0)') "    Update Frequency      : ", nonlinear%update_frequency
        end if

        ! ## 修正点2: 文字配列コンストラクタを valid_* パラメータに置き換え
        if (any(nonlinear%method == valid_nonlinear_solver_methods(2:4))) then
            write (*, '(a, i0)') "    Max Iterations        : ", nonlinear%max_iterations
            write (*, '(a)') "    --- Convergence ---"
            write (*, '(a, a)') "      Use Criteria          : ", trim(nonlinear%convergence%use_criteria)
            if (nonlinear%convergence%use_criteria == valid_criteria_types(3)) then ! "both"
                write (*, '(a, a)') "      Logic Between         : ", trim(nonlinear%convergence%use_logic)
            end if

            ! ## 修正点2: 文字配列コンストラクタを valid_* パラメータに置き換え
            if (any(nonlinear%convergence%use_criteria == valid_criteria_types([1, 3]))) then ! "residual" or "both"
                call display_solver_convergence_criteria(nonlinear%convergence%residual, "      Residual Criteria")
            end if
            ! ## 修正点2: 文字配列コンストラクタを valid_* パラメータに置き換え
            if (any(nonlinear%convergence%use_criteria == valid_criteria_types([2, 3]))) then ! "update" or "both"
                call display_solver_convergence_criteria(nonlinear%convergence%update, "      Update Criteria")
            end if
        end if
    end subroutine display_solver_settings_nonlinear

    subroutine display_solver_convergence_criteria(criteria_obj, title)
        !> Reusable helper to display convergence criteria details.
        implicit none
        type(type_convergence_criteria), intent(in) :: criteria_obj
        character(*), intent(in) :: title

        write (*, '(a, a)') trim(title)//'        : ', trim(criteria_obj%criteria)
        if (criteria_obj%criteria == valid_local_criteria_types(3)) then ! "both"
            write (*, '(a, a)') "        Logic             : ", trim(criteria_obj%logic)
        end if
        ! ## 修正点2: 文字配列コンストラクタを valid_* パラメータに置き換え
        if (any(criteria_obj%criteria == valid_local_criteria_types([1, 3]))) then ! "absolute" or "both"
            write (*, '(a, es12.4e2)') "        Absolute Tol.     : ", criteria_obj%absolute_tolerance
        end if
        ! ## 修正点2: 文字配列コンストラクタを valid_* パラメータに置き換え
        if (any(criteria_obj%criteria == valid_local_criteria_types([2, 3]))) then ! "relative" or "both"
            write (*, '(a, es12.4e2)') "        Relative Tol.     : ", criteria_obj%relative_tolerance
        end if
    end subroutine display_solver_convergence_criteria

! ## 修正点1: 引数からcontrolsを削除し、内部ロジックを変更
    subroutine display_solver_settings_linear(linear)
        !> Displays linear solver settings by checking if each method is defined.
        implicit none
        type(type_linear_solver), intent(in) :: linear

        write (*, '(/a)') "  --- Linear Solver ---"

        ! `method`が空文字列でなければ、その物理場が有効であると判断して表示する
        if (len(trim(linear%thermal%method)) > 0) then
            call display_solver_settings_linear_local(linear%thermal, "    Thermal")
        end if
        if (len(trim(linear%hydraulic%method)) > 0) then
            call display_solver_settings_linear_local(linear%hydraulic, "    Hydraulic")
        end if
        if (len(trim(linear%mechanical%method)) > 0) then
            call display_solver_settings_linear_local(linear%mechanical, "    Mechanical")
        end if
    end subroutine display_solver_settings_linear

    subroutine display_solver_settings_linear_local(local_solver, title)
        !> Reusable helper to display settings for a specific linear solver.
        implicit none
        type(type_linear_solver_settings), intent(in) :: local_solver
        character(*), intent(in) :: title

        write (*, '(a, a)') trim(title)//" Method         : ", trim(local_solver%method)
        if (local_solver%method == valid_linear_solver_methods(2)) then ! "iterative"
            write (*, '(a, i0)') "      Solver Type         : ", local_solver%iterative_solver%solver_type
            write (*, '(a, i0)') "      Preconditioner Type : ", local_solver%iterative_solver%preconditioner_type
            write (*, '(a, i0)') "      Max Iterations      : ", local_solver%iterative_solver%max_iterations
            write (*, '(a, es12.4e2)') "      Tolerance           : ", local_solver%iterative_solver%tolerance
        end if
    end subroutine display_solver_settings_linear_local

    subroutine display_solver_settings_parallel(parallel)
        !> Displays parallel processing settings.
        implicit none
        type(type_parallel_settings), intent(in) :: parallel

        write (*, '(/a)') "  --- Parallel Settings ---"
        write (*, '(a, L1)') "    Use Parallel          : ", parallel%threads%is_parallel
        if (parallel%threads%is_parallel) then
            write (*, '(a, i0)') "    Num Threads           : ", parallel%threads%num_threads
            write (*, '(a, a)') "    Schedule              : ", trim(parallel%threads%schedule)
            write (*, '(a, i0)') "    Max Active Levels     : ", parallel%threads%max_active_levels
        end if
    end subroutine display_solver_settings_parallel
end submodule inout_input_basic_solver_settings

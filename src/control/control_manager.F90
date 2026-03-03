module core_control_manager
    use, intrinsic :: iso_fortran_env
    use :: stdlib_strings, only:strip
    use :: stdlib_optval, only:optval
    use :: module_core
    use :: module_linalg
    use :: control_acceleration, only:abst_acceleration, type_acceleration_aitken
    use :: control_time, only:type_time
    use :: control_time_profiler, only:type_time_profiler
    use :: control_iteration, only:type_iteration
    use :: control_scheduler, only:type_scheduler_manager
    use :: control_parallel, only:initialize_openmp
    implicit none
    private

    public :: type_control

    type :: type_control
        logical, private, allocatable :: compute_active(:)
        type(type_constant_id), private :: coupling_mode
        logical, private, allocatable :: physics_active(:, :)

        type(type_iteration), private :: iteration
        type(type_time), private :: time
        type(type_time_profiler), private :: profiler

        type(type_scheduler_manager), private :: scheduler_field
        type(type_scheduler_manager), private :: scheduler_history

        class(abst_acceleration), private, allocatable :: acceleration

    contains
        ! ---- Lifecycle ----
        procedure, public, pass(self) :: initialize => initialize_type_control

        ! ---- Mutator ----
        procedure, public, pass(self) :: update => update_controls
        procedure, public, pass(self) :: update_output => update_output_control
        ! - iteration
        procedure, public, pass(self) :: reset_iteration => reset_iteration_control
        procedure, public, pass(self) :: increment_nonlinear => increment_nonlinear_control
        procedure, public, pass(self) :: increment_total => increment_total_control
        procedure, public, pass(self) :: set_nonlinear_solver => set_nonlinear_solver_control
        procedure, public, pass(self) :: set_converged => set_converged_control
        procedure, public, pass(self) :: set_diverged => set_diverged_control
        ! - acceleration
        procedure, public, pass(self) :: reset_acceleration => reset_acceleration_control
        ! ---- Algorithm / Operation ----
        ! - profiler
        procedure, public, pass(self) :: profiler_start => profiler_start_control
        procedure, public, pass(self) :: profiler_stop => profiler_stop_control
        procedure, public, pass(self) :: profiler_record => profiler_record_control
        ! - iteration
        procedure, public, pass(self) :: check_convergence => check_convergence_control
        ! - acceleration
        procedure, public, pass(self) :: compute_relaxation => compute_relaxation_control

        ! ---- Inquiry ----
        procedure, public, pass(self) :: is_physics_active => is_physics_active_control
        procedure, public, pass(self) :: is_target => is_target_control
        procedure, public, pass(self) :: is_monolithic => is_monolithic_control
        procedure, public, pass(self) :: is_staggered => is_staggered_control
        procedure, public, pass(self) :: is_end_time => is_end_time_control
        procedure, public, pass(self) :: is_output_triggered => is_output_triggered_control
        ! - iteration
        procedure, public, pass(self) :: is_diverged => is_diverged_control
        procedure, public, pass(self) :: is_converged => is_converged_control
        procedure, public, pass(self) :: is_compute_newton => is_compute_newton_control
        procedure, public, pass(self) :: is_compute_picard => is_compute_picard_control
        procedure, public, pass(self) :: is_compute_none => is_compute_none_control
        procedure, public, pass(self) :: is_newton => is_newton_control
        procedure, public, pass(self) :: is_picard => is_picard_control
        procedure, public, pass(self) :: is_none => is_none_control
        procedure, public, pass(self) :: should_continue => should_continue_control
        ! - acceleration
        procedure, public, pass(self) :: reach_minimum_relaxation => reach_minimum_relaxation_control
        procedure, public, pass(self) :: reach_maximum_relaxation => reach_maximum_relaxation_control

        ! ---- Getter ----
        procedure, public, pass(self) :: get_coupling_mode => get_coupling_mode_control
        procedure, public, pass(self) :: get_output_time => get_output_time_control
        procedure, public, pass(self) :: get_output_step => get_output_step_control
        procedure, public, pass(self) :: get_bdf_coeffs => get_bdf_coeffs_control
        ! - iteration
        procedure, public, pass(self) :: get_nonlinear_solver => get_nonlinear_solver_control
        procedure, public, pass(self) :: get_nonlinear_iter => get_nonlinear_iter_control
        procedure, public, pass(self) :: get_total_iter => get_total_iter_control
        procedure, public, pass(self) :: get_max_iterations => get_max_iterations_control
        procedure, public, pass(self) :: get_update_frequency => get_update_frequency_control
        procedure, public, pass(self) :: get_current_norm => get_current_norm_control
        procedure, public, pass(self) :: get_tolerances => get_tolerances_control
        ! - time
        procedure, public, pass(self) :: get_time => get_time_control
        ! - acceleration
        procedure, public, pass(self) :: get_current_relaxation => get_current_relaxation_control
        procedure, public, pass(self) :: get_previous_relaxation => get_previous_relaxation_control

        ! ---- Meta / Utility ----
        procedure, public, pass(self) :: display => display_control
        procedure, public, pass(self) :: display_profiler => display_profiler_control

    end type type_control

contains
    subroutine initialize_type_control(self, config_control_manager, config_iteration, config_time, config_time_ats, &
                                       config_output_field, config_output_history, config_acceleration, config_parallel_openmp)
        implicit none
        class(type_control), intent(inout) :: self
        type(type_config_control_manager), intent(in), optional :: config_control_manager
        type(type_config_iteration), intent(in), optional :: config_iteration
        type(type_config_time), intent(in), optional :: config_time
        type(type_config_time_ats), intent(in), optional :: config_time_ats
        class(type_config_output_manager), intent(in), optional :: config_output_field
        class(type_config_output_manager), intent(in), optional :: config_output_history
        class(type_config_acceleration), intent(in), optional :: config_acceleration
        type(type_config_parallel_openmp), intent(in), optional :: config_parallel_openmp

        real(real64) :: current_time_s

        if (present(config_control_manager)) then
            call allocate_array(self%compute_active, source=config_control_manager%compute_active)
            call allocate_array(self%physics_active, source=config_control_manager%physics_active)
            self%coupling_mode = config_control_manager%coupling_mode
        end if

        ! Control time settings initialization
        if (present(config_time) .and. present(config_time_ats)) then
            call self%time%initialize(config_time, config_time_ats)
        end if

        ! Control iteration settings initialization
        if (present(config_iteration)) then
            call self%iteration%initialize(config_iteration)
        end if

        ! OpenMP settings initialization
        if (present(config_parallel_openmp)) then
            call initialize_openmp(config_parallel_openmp)
        end if

        ! Acceleration method initialization
        if (present(config_acceleration)) then
            select case (config_acceleration%method%ID)
            case (ACCELERATION_METHODS%AITKEN%ID)
                allocate (type_acceleration_aitken :: self%acceleration)
            case (ACCELERATION_METHODS%ANDERSON%ID)
                error stop "Anderson acceleration is not implemented yet."
            case default
                error stop "Unknown acceleration method: "//trim(config_acceleration%method%name)
            end select
            call self%acceleration%initialize(config_acceleration)
        end if

        ! Output managers initialization with the current time
        if ((present(config_time) .and. present(config_time_ats)) .or. &
            self%time%is_initialized()) then
            call self%time%get_time(current_time_s)
            if (present(config_output_field)) then
                call self%scheduler_field%initialize(config_output_field, current_time_s)
            end if
            if (present(config_output_history)) then
                call self%scheduler_history%initialize(config_output_history, current_time_s)
            end if
        end if

        call self%profiler%initialize()

    end subroutine initialize_type_control

    ! -----------------------------------------------------------------
    ! 指定された物理現象と材料IDが計算対象かどうかを判定する
    ! -----------------------------------------------------------------
    pure function is_target_control(self, target_physics, material_id) result(compute_active)
        implicit none
        class(type_control), intent(in) :: self
        type(type_constant_id), intent(in) :: target_physics
        integer(int32), intent(in) :: material_id
        logical :: compute_active
        integer(int32) :: pid

        pid = target_physics%ID

        if (pid < 1 .or. pid > PHYSICS_TYPES%NUM_ID) then
            compute_active = .false.
            return
        end if

        if (.not. self%compute_active(pid)) then
            compute_active = .false.
            return
        end if

        if (.not. allocated(self%physics_active)) then
            compute_active = .false.
            return
        end if

        if (material_id < 1 .or. material_id > size(self%physics_active, 2)) then
            compute_active = .false.
            return
        end if

        compute_active = self%physics_active(pid, material_id)

    end function is_target_control
    !>
    !> 指定された物理定数が計算対象かどうかを判定する
    pure function is_physics_active_control(self, physics_type) result(compute_active)
        implicit none
        !> Instance of control settings
        class(type_control), intent(in) :: self
        !> Physics type identifier in PHYSICS_TYPES
        type(type_constant_id), intent(in) :: physics_type
        !> Returns `true` if the physics type is active
        logical :: compute_active

        if (.not. PHYSICS_TYPES%is_valid(physics_type)) then
            compute_active = .false.
            return
        end if

        compute_active = self%compute_active(physics_type%ID)

    end function is_physics_active_control

    subroutine get_coupling_mode_control(self, coupling_mode)
        implicit none
        class(type_control), intent(in), target :: self
        type(type_constant_id), intent(inout), pointer :: coupling_mode

        coupling_mode => self%coupling_mode
    end subroutine get_coupling_mode_control

    subroutine get_bdf_coeffs_control(self, bdf_order, bdf_coeffs)
        implicit none
        class(type_control), intent(in) :: self
        integer(int32), intent(inout), optional :: bdf_order
        real(real64), intent(inout), pointer, contiguous, dimension(:), optional :: bdf_coeffs

        if (present(bdf_order)) then
            call self%time%get_bdf_order(bdf_order)
        end if
        if (present(bdf_coeffs)) then
            call self%time%get_bdf_coeffs(bdf_coeffs)
        end if

    end subroutine get_bdf_coeffs_control

    pure function is_monolithic_control(self) result(is_monolithic)
        implicit none
        class(type_control), intent(in) :: self
        logical :: is_monolithic

        is_monolithic = (self%coupling_mode == COUPLING_MODES%MONOLITHIC)

    end function is_monolithic_control

    pure function is_staggered_control(self) result(is_staggered)
        implicit none
        class(type_control), intent(in) :: self
        logical :: is_staggered

        is_staggered = (self%coupling_mode == COUPLING_MODES%STAGGERED)

    end function is_staggered_control

    pure function is_compute_newton_control(self) result(is_compute_newton)
        implicit none
        class(type_control), intent(in) :: self
        logical :: is_compute_newton

        is_compute_newton = self%iteration%is_compute_newton()

    end function is_compute_newton_control

    pure function is_compute_picard_control(self) result(is_compute_picard)
        implicit none
        class(type_control), intent(in) :: self
        logical :: is_compute_picard

        is_compute_picard = self%iteration%is_compute_picard()

    end function is_compute_picard_control

    pure function is_compute_none_control(self) result(is_compute_none)
        implicit none
        class(type_control), intent(in) :: self
        logical :: is_compute_none

        is_compute_none = self%iteration%is_compute_none()

    end function is_compute_none_control

    pure function is_newton_control(self) result(is_newton)
        implicit none
        class(type_control), intent(in) :: self
        logical :: is_newton

        is_newton = self%iteration%is_newton()

    end function is_newton_control

    pure function is_picard_control(self) result(is_picard)
        implicit none
        class(type_control), intent(in) :: self
        logical :: is_picard

        is_picard = self%iteration%is_picard()

    end function is_picard_control

    pure function is_none_control(self) result(is_none)
        implicit none
        class(type_control), intent(in) :: self
        logical :: is_none

        is_none = self%iteration%is_none()

    end function is_none_control

    subroutine reset_controls(self)
        implicit none
        class(type_control), intent(inout) :: self

        call self%iteration%reset()

    end subroutine reset_controls

    subroutine display_control(self, unit_in)
        implicit none
        class(type_control), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: unit

        unit = optval(unit_in, output_unit)
        ! integer(int32) :: pid, mid

        ! write (*, '(a)') "# Control Settings"
        ! write (*, '(a)') "## Active Physics Types:"

        ! do pid = 1, PHYSICS_TYPES%NUM_ID
        !     if (self%compute_active(pid)) then
        !         write (*, '(a)') "- "//trim(PHYSICS_TYPES%to_object(pid)%name)
        !     end if
        ! end do

        ! write (*, '(a)') "## Coupling Mode:"
        ! write (*, '(a)') "- "//trim(self%coupling_mode%name)

        ! if (allocated(self%physics_active)) then
        !     write (*, '(a)') "### Material Flags:"
        !     do pid = 1, PHYSICS_TYPES%NUM_ID
        !         if (.not. self%compute_active(pid)) cycle
        !         write (*, '(a)') "Physics: "//trim(PHYSICS_TYPES%to_object(pid)%name)
        !         do mid = 1, size(self%physics_active, 2)
        !             write (*, '(a,i0,a,l1)') "- Material ", mid, ": ", &
        !                 self%physics_active(pid, mid)
        !         end do
        !     end do
        ! end if

        ! write (*, '(a)') "## Time Settings"
        ! call self%time%display()

    end subroutine display_control

    pure function is_end_time_control(self) result(is_end_time)
        implicit none
        class(type_control), intent(in) :: self
        logical :: is_end_time

        is_end_time = self%time%is_end_time()
    end function is_end_time_control

    subroutine update_controls(self, success)
        implicit none
        class(type_control), intent(inout) :: self
        logical, intent(in) :: success
        integer(int32) :: iter_count
        real(real64) :: t_target
        real(real64) :: t_arrival ! 計算によって到達する時刻
        real(real64) :: t_target_out
        real(real64) :: current_time_s, dt_s

        ! 1. 反復回数の取得
        call self%iteration%get_nonlinear_iter(iter_count)

        ! 2. 到達予測時刻 (t_new = t_old + dt)
        !    成功していれば，この後 time%update で時刻がここまで進む
        call self%time%get_time(current_time_s)
        call self%time%get_dt(dt_s)
        t_arrival = current_time_s + dt_s

        ! 3. ターゲット時刻の決定
        call self%time%get_end_time(t_target)

        ! 到達時刻(300) を渡して，次のターゲット(600) を取得する
        if (self%scheduler_field%is_active()) then
            call self%scheduler_field%get_next_target_time(t_arrival, t_target_out)
            t_target = min(t_target, t_target_out)
        end if
        if (self%scheduler_history%is_active()) then
            call self%scheduler_history%get_next_target_time(t_arrival, t_target_out)
            t_target = min(t_target, t_target_out)
        end if

        ! 4. 更新実行 (t_target=600 なので，残り300に合わせて dt が制限される)
        call self%time%update(success, iter_count, t_target)

    end subroutine update_controls

    function is_output_triggered_control(self, output_type, current_time_seconds) result(is_output_triggered)
        implicit none
        class(type_control), intent(in) :: self
        type(type_constant_id), intent(in) :: output_type
        real(real64), intent(in) :: current_time_seconds
        logical :: is_output_triggered

        select case (output_type%ID)
        case (OUTPUT_TYPES%FIELD%ID)
            is_output_triggered = self%scheduler_field%is_output_triggered(current_time_seconds)
        case (OUTPUT_TYPES%HISTORY%ID)
            is_output_triggered = self%scheduler_history%is_output_triggered(current_time_seconds)
        case default
            is_output_triggered = .false.
        end select

    end function is_output_triggered_control

    pure subroutine get_output_time_control(self, output_type, current_time_seconds, converted_time)
        implicit none
        class(type_control), intent(in) :: self
        type(type_constant_id), intent(in) :: output_type
        real(real64), intent(in) :: current_time_seconds
        real(real64), intent(inout) :: converted_time

        select case (output_type%ID)
        case (OUTPUT_TYPES%FIELD%ID)
            call self%scheduler_field%get_output_time(current_time_seconds, converted_time)
        case (OUTPUT_TYPES%HISTORY%ID)
            call self%scheduler_history%get_output_time(current_time_seconds, converted_time)
        case default
            converted_time = current_time_seconds
        end select

    end subroutine get_output_time_control

    pure subroutine get_output_step_control(self, output_type, step)
        implicit none
        class(type_control), intent(in) :: self
        type(type_constant_id), intent(in) :: output_type
        integer(int32), intent(inout) :: step

        select case (output_type%ID)
        case (OUTPUT_TYPES%FIELD%ID)
            call self%scheduler_field%get_step(step)
        case (OUTPUT_TYPES%HISTORY%ID)
            call self%scheduler_history%get_step(step)
        case default
            step = -1
        end select

    end subroutine get_output_step_control

    subroutine update_output_control(self, output_type, current_time_seconds)
        implicit none
        class(type_control), intent(inout) :: self
        type(type_constant_id), intent(in) :: output_type
        real(real64), intent(in) :: current_time_seconds

        select case (output_type%ID)
        case (OUTPUT_TYPES%FIELD%ID)
            call self%scheduler_field%update(current_time_seconds)
        case (OUTPUT_TYPES%HISTORY%ID)
            call self%scheduler_history%update(current_time_seconds)
        case default
            ! 何もしない
        end select

    end subroutine update_output_control

    subroutine profiler_start_control(self, label)
        implicit none
        !> Profiler manager object
        class(type_control), intent(inout) :: self
        !> Identifier for the profiler record
        type(type_constant_id), intent(in) :: label

        call self%profiler%start(label)

    end subroutine profiler_start_control

    subroutine profiler_stop_control(self, label)
        implicit none
        class(type_control), intent(inout) :: self
        type(type_constant_id), intent(in) :: label

        call self%profiler%stop(label)

    end subroutine profiler_stop_control

    subroutine profiler_record_control(self, label)
        implicit none
        class(type_control), intent(inout) :: self
        type(type_constant_id), intent(in) :: label

        call self%profiler%record(label)

    end subroutine profiler_record_control

    subroutine get_time_control(self, time)
        implicit none
        class(type_control), intent(in) :: self
        real(real64), intent(inout) :: time

        call self%time%get_time(time)

    end subroutine get_time_control

    subroutine display_profiler_control(self, unit_in)
        implicit none
        class(type_control), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        call self%profiler%display(unit_in)

    end subroutine display_profiler_control

    pure function is_diverged_control(self) result(is_diverged)
        implicit none
        class(type_control), intent(in) :: self
        logical :: is_diverged

        is_diverged = self%iteration%is_diverged()

    end function is_diverged_control

    pure function is_converged_control(self) result(is_converged)
        implicit none
        class(type_control), intent(in) :: self
        logical :: is_converged

        is_converged = self%iteration%is_converged()

    end function is_converged_control

    pure function should_continue_control(self) result(should_continue)
        implicit none
        class(type_control), intent(in) :: self
        logical :: should_continue

        should_continue = self%iteration%should_continue()

    end function should_continue_control

    subroutine check_convergence_control(self, physics_type, residual_vector, update_vector)
        implicit none
        class(type_control), intent(inout) :: self
        type(type_constant_id), intent(in) :: physics_type
        real(real64), intent(in), optional :: residual_vector(:)
        real(real64), intent(in), optional :: update_vector(:)

        call self%iteration%check_convergence(physics_type, residual_vector, update_vector)

    end subroutine check_convergence_control

    subroutine get_nonlinear_solver_control(self, nonlinear_solver_type)
        implicit none
        class(type_control), intent(in), target :: self
        type(type_constant_id), intent(inout), pointer :: nonlinear_solver_type

        call self%iteration%get_nonlinear_solver(nonlinear_solver_type)

    end subroutine get_nonlinear_solver_control

    subroutine get_nonlinear_iter_control(self, nonlinear_iter)
        implicit none
        class(type_control), intent(in) :: self
        integer(int32), intent(inout) :: nonlinear_iter

        call self%iteration%get_nonlinear_iter(nonlinear_iter)
    end subroutine get_nonlinear_iter_control

    subroutine get_total_iter_control(self, total_iter)
        implicit none
        class(type_control), intent(in) :: self
        integer(int32), intent(inout) :: total_iter

        call self%iteration%get_total_iter(total_iter)
    end subroutine get_total_iter_control

    subroutine get_max_iterations_control(self, max_iterations)
        implicit none
        class(type_control), intent(in) :: self
        integer(int32), intent(inout) :: max_iterations

        call self%iteration%get_max_iterations(max_iterations)
    end subroutine get_max_iterations_control

    subroutine get_update_frequency_control(self, update_frequency)
        implicit none
        class(type_control), intent(in) :: self
        integer(int32), intent(inout) :: update_frequency

        call self%iteration%get_update_frequency(update_frequency)
    end subroutine get_update_frequency_control

    subroutine get_current_norm_control(self, physics_type, criteria_type, norm_type, current_norm)
        implicit none
        class(type_control), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        type(type_constant_id), intent(in) :: criteria_type
        type(type_constant_id), intent(in) :: norm_type
        real(real64), intent(inout) :: current_norm

        call self%iteration%get_current_norm(physics_type, criteria_type, norm_type, current_norm)
    end subroutine get_current_norm_control

    subroutine get_tolerances_control(self, physics_type, absolute_tolerance, relative_tolerance)
        implicit none
        class(type_control), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        real(real64), intent(inout), optional :: absolute_tolerance
        real(real64), intent(inout), optional :: relative_tolerance

        call self%iteration%get_tolerances(physics_type, absolute_tolerance, relative_tolerance)
    end subroutine get_tolerances_control

    subroutine reset_iteration_control(self)
        implicit none
        class(type_control), intent(inout) :: self

        call self%iteration%reset()

    end subroutine reset_iteration_control

    subroutine increment_nonlinear_control(self)
        implicit none
        class(type_control), intent(inout) :: self

        call self%iteration%increment_nonlinear()

    end subroutine increment_nonlinear_control

    subroutine increment_total_control(self)
        implicit none
        class(type_control), intent(inout) :: self

        call self%iteration%increment_total()
    end subroutine increment_total_control

    subroutine set_nonlinear_solver_control(self, nonlinear_solver_type)
        implicit none
        class(type_control), intent(inout) :: self
        type(type_constant_id), intent(in) :: nonlinear_solver_type

        call self%iteration%set_nonlinear_solver(nonlinear_solver_type)
    end subroutine set_nonlinear_solver_control

    subroutine set_converged_control(self, physics_type, converged)
        implicit none
        class(type_control), intent(inout) :: self
        type(type_constant_id), intent(in) :: physics_type
        logical, intent(in) :: converged

        call self%iteration%set_converged(physics_type, converged)
    end subroutine set_converged_control

    subroutine set_diverged_control(self, physics_type, diverged)
        implicit none
        class(type_control), intent(inout) :: self
        type(type_constant_id), intent(in) :: physics_type
        logical, intent(in) :: diverged

        call self%iteration%set_diverged(physics_type, diverged)

    end subroutine set_diverged_control

    subroutine reset_acceleration_control(self)
        implicit none
        class(type_control), intent(inout) :: self

        if (allocated(self%acceleration)) then
            call self%acceleration%reset()
        end if

    end subroutine reset_acceleration_control

    subroutine compute_relaxation_control(self, physics_type, iter, du, vec)
        implicit none
        !> Aitken acceleration object
        class(type_control), intent(inout) :: self
        !> Identifier for the physics type
        type(type_constant_id), intent(in) :: physics_type
        !> Current iteration number
        integer(int32), intent(in) :: iter
        !> Increment vector \(\Delta u_k\)
        real(real64), intent(in) :: du(:)
        !> State vector \(u_k\) on entry
        !> Overwritten by updated vector \(u_{k+1}\) on exit
        real(real64), intent(inout) :: vec(:)

        call self%acceleration%compute_relaxation(physics_type, iter, du, vec)

    end subroutine compute_relaxation_control

    pure function reach_minimum_relaxation_control(self, physics_type) result(reached)
        implicit none
        class(type_control), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        logical :: reached

        reached = self%acceleration%reach_minimum_relaxation(physics_type)
    end function reach_minimum_relaxation_control

    pure function reach_maximum_relaxation_control(self, physics_type) result(reached)
        implicit none
        class(type_control), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        logical :: reached

        reached = self%acceleration%reach_maximum_relaxation(physics_type)
    end function reach_maximum_relaxation_control

    subroutine get_current_relaxation_control(self, physics_type, relaxation)
        implicit none
        class(type_control), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        real(real64), intent(inout) :: relaxation

        call self%acceleration%get_current_relaxation(physics_type, relaxation)
    end subroutine get_current_relaxation_control

    subroutine get_previous_relaxation_control(self, physics_type, relaxation)
        implicit none
        class(type_control), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        real(real64), intent(inout) :: relaxation

        call self%acceleration%get_previous_relaxation(physics_type, relaxation)
    end subroutine get_previous_relaxation_control

end module core_control_manager

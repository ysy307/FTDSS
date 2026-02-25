module module_control
    use, intrinsic :: iso_fortran_env
    use :: stdlib_strings, only:strip
    use :: module_core
    use :: module_input, only:type_input
    use :: control_acceleration, only:abst_acceleration, type_acceleration_aitken
    use :: control_time, only:type_time
    use :: control_time_profiler, only:type_time_profiler
    use :: control_iteration, only:type_iteration
    use :: control_output, only:type_output_manager
    use :: control_openmp, only:initialize_openmp
    use :: module_linalg
    implicit none
    private

    public :: type_time
    public :: type_iteration
    public :: type_control

    type :: type_control
        private
        logical :: is_active(PHYSICS_TYPES%NUM_ID) = .false.
        type(type_constant_id) :: coupling_mode
        logical, allocatable :: physics_active(:, :)

        type(type_iteration), public :: iteration
        type(type_time), public :: time
        type(type_time_profiler), public :: profiler

        type(type_output_manager), private :: out_field
        type(type_output_manager), private :: out_history

        class(abst_acceleration), allocatable, public :: acceleration

    contains
        procedure, pass(self), public :: initialize => initialize_type_control
        procedure, pass(self), public :: is_physics_active => is_physics_active_control
        procedure, pass(self), public :: is_target => is_target_control
        procedure, pass(self), public :: get_coupling_mode => get_coupling_mode_control

        procedure, pass(self), public :: is_monolithic => is_monolithic_control
        procedure, pass(self), public :: is_staggered => is_staggered_control

        procedure, pass(self), public :: is_end_time => is_end_time_control

        procedure, pass(self), public :: update => update_controls

        procedure, pass(self), public :: display => display_controls
        ! output series
        procedure, pass(self), public :: is_output_triggered => is_output_triggered_control
        procedure, pass(self), public :: get_output_time => get_output_time_control
        procedure, pass(self), public :: get_output_step => get_output_step_control
        procedure, pass(self), public :: update_output => update_output_control
    end type type_control

contains
    subroutine initialize_type_control(self, input, config_time, config_time_ats, config_output_field, &
                                       config_output_history, config_acceleration)
        implicit none
        class(type_control), intent(inout) :: self
        class(type_input), intent(in) :: input
        type(type_config_time), intent(in), optional :: config_time
        type(type_config_time_ats), intent(in), optional :: config_time_ats
        class(type_config_output_manager), intent(in), optional :: config_output_field
        class(type_config_output_manager), intent(in), optional :: config_output_history
        class(type_config_acceleration), intent(in), optional :: config_acceleration

        integer(int32), allocatable :: unique_material_ids(:)
        integer(int32) :: i, num_unique_regions, max_region_id
        integer(int32) :: current_material_id, pid
        real(real64) :: current_time_s

        call input%geometry%vtk%get_active_region_info(unique_material_ids)

        if (.not. allocated(unique_material_ids) .or. size(unique_material_ids) == 0) then
            print *, "Error: No active material regions found."
            stop 1
        end if

        num_unique_regions = size(unique_material_ids)
        max_region_id = maxval(unique_material_ids)

        ! -------------------------
        ! physics_active 配列確保
        ! -------------------------
        allocate (self%physics_active(PHYSICS_TYPES%NUM_ID, max_region_id))
        self%physics_active = .false.

        ! -------------------------
        ! 物理有効フラグ
        ! -------------------------
        do pid = 1, PHYSICS_TYPES%NUM_ID
            self%is_active(pid) = input%basic%analysis_controls%is_active(pid)
        end do

        ! -------------------------
        ! 材料別フラグ設定
        ! -------------------------
        do i = 1, num_unique_regions
            current_material_id = unique_material_ids(i)

            if (current_material_id > size(input%basic%materials)) cycle

            do pid = 1, PHYSICS_TYPES%NUM_ID
                if (self%is_active(pid)) then
                    self%physics_active(pid, current_material_id) = &
                        input%basic%materials(current_material_id)%is_active(pid)
                end if
            end do
        end do

        self%coupling_mode = &
            COUPLING_MODES%to_object(input%basic%analysis_controls%coupling_mode)

        if (present(config_time) .and. present(config_time_ats)) then
            call self%time%initialize(config_time, config_time_ats)
        end if
        call self%iteration%initialize(input)
        call initialize_openmp(input)

        if (present(config_acceleration)) then
            select case (config_acceleration%method%ID)
            case (ACCELERATION_METHODS%AITKEN%ID)
                allocate (type_acceleration_aitken :: self%acceleration)
            case (ACCELERATION_METHODS%ANDERSON%ID)
                ! 将来実装
            end select
            call self%acceleration%initialize(config_acceleration)
        end if

        call self%time%get_time(current_time_s)
        if (present(config_output_field)) then
            call self%out_field%initialize(config_output_field, current_time_s)
        end if
        if (present(config_output_history)) then
            call self%out_history%initialize(config_output_history, current_time_s)
        end if

        call self%profiler%initialize()

        deallocate (unique_material_ids)

    end subroutine initialize_type_control

    ! -----------------------------------------------------------------
    ! 指定された物理現象と材料IDが計算対象かどうかを判定する
    ! -----------------------------------------------------------------
    pure function is_target_control(self, target_physics, material_id) result(is_active)
        implicit none
        class(type_control), intent(in) :: self
        type(type_constant_id), intent(in) :: target_physics
        integer(int32), intent(in) :: material_id
        logical :: is_active
        integer(int32) :: pid

        pid = target_physics%ID

        if (pid < 1 .or. pid > PHYSICS_TYPES%NUM_ID) then
            is_active = .false.
            return
        end if

        if (.not. self%is_active(pid)) then
            is_active = .false.
            return
        end if

        if (.not. allocated(self%physics_active)) then
            is_active = .false.
            return
        end if

        if (material_id < 1 .or. material_id > size(self%physics_active, 2)) then
            is_active = .false.
            return
        end if

        is_active = self%physics_active(pid, material_id)

    end function is_target_control
    !>
    !> 指定された物理定数が計算対象かどうかを判定する
    pure function is_physics_active_control(self, physics_type) result(is_active)
        implicit none
        !> Instance of control settings
        class(type_control), intent(in) :: self
        !> Physics type identifier in PHYSICS_TYPES
        type(type_constant_id), intent(in) :: physics_type
        !> Returns `true` if the physics type is active
        logical :: is_active

        if (.not. PHYSICS_TYPES%is_valid(physics_type)) then
            is_active = .false.
            return
        end if

        is_active = self%is_active(physics_type%ID)

    end function is_physics_active_control

    subroutine get_coupling_mode_control(self, coupling_mode)
        implicit none
        class(type_control), intent(in), target :: self
        type(type_constant_id), intent(inout), pointer :: coupling_mode

        coupling_mode => self%coupling_mode
    end subroutine get_coupling_mode_control

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

    subroutine reset_controls(self)
        implicit none
        class(type_control), intent(inout) :: self

        call self%iteration%reset()

    end subroutine reset_controls

    subroutine display_controls(self)

        class(type_control), intent(in) :: self
        ! integer(int32) :: pid, mid

        ! write (*, '(a)') "# Control Settings"
        ! write (*, '(a)') "## Active Physics Types:"

        ! do pid = 1, PHYSICS_TYPES%NUM_ID
        !     if (self%is_active(pid)) then
        !         write (*, '(a)') "- "//trim(PHYSICS_TYPES%to_object(pid)%name)
        !     end if
        ! end do

        ! write (*, '(a)') "## Coupling Mode:"
        ! write (*, '(a)') "- "//trim(self%coupling_mode%name)

        ! if (allocated(self%physics_active)) then
        !     write (*, '(a)') "### Material Flags:"
        !     do pid = 1, PHYSICS_TYPES%NUM_ID
        !         if (.not. self%is_active(pid)) cycle
        !         write (*, '(a)') "Physics: "//trim(PHYSICS_TYPES%to_object(pid)%name)
        !         do mid = 1, size(self%physics_active, 2)
        !             write (*, '(a,i0,a,l1)') "- Material ", mid, ": ", &
        !                 self%physics_active(pid, mid)
        !         end do
        !     end do
        ! end if

        ! write (*, '(a)') "## Time Settings"
        ! call self%time%display()

    end subroutine display_controls

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
        if (self%out_field%is_active()) then
            call self%out_field%get_next_target_time(t_arrival, t_target_out)
            t_target = min(t_target, t_target_out)
        end if
        if (self%out_history%is_active()) then
            call self%out_history%get_next_target_time(t_arrival, t_target_out)
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
            is_output_triggered = self%out_field%is_output_triggered(current_time_seconds)
        case (OUTPUT_TYPES%HISTORY%ID)
            is_output_triggered = self%out_history%is_output_triggered(current_time_seconds)
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
            call self%out_field%get_output_time(current_time_seconds, converted_time)
        case (OUTPUT_TYPES%HISTORY%ID)
            call self%out_history%get_output_time(current_time_seconds, converted_time)
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
            call self%out_field%get_step(step)
        case (OUTPUT_TYPES%HISTORY%ID)
            call self%out_history%get_step(step)
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
            call self%out_field%update(current_time_seconds)
        case (OUTPUT_TYPES%HISTORY%ID)
            call self%out_history%update(current_time_seconds)
        case default
            ! 何もしない
        end select

    end subroutine update_output_control
end module module_control

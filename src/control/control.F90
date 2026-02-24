module module_control
    use, intrinsic :: iso_fortran_env
    use :: stdlib_strings, only:strip
    use :: module_core
    use :: module_input, only:type_input
    use :: control_acceleration, only:abst_acceleration, type_acceleration_aitken
    use :: control_time, only:type_time
    use :: control_time_profiler, only:type_profiler
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
        logical, allocatable :: thermal(:)
        logical, allocatable :: hydraulic(:)
        logical, allocatable :: mechanical(:)

        type(type_iteration), public :: iteration
        type(type_time), public :: time
        type(type_profiler), public :: profiler

        type(type_output_manager), public :: out_field
        type(type_output_manager), public :: out_history

        class(abst_acceleration), allocatable, public :: acceleration
        ! type(type_aitken_params), public :: aitken

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
    end type type_control

contains
    subroutine initialize_type_control(self, input, config_acceleration)
        implicit none
        class(type_control), intent(inout) :: self
        class(type_input), intent(in) :: input
        class(type_config_acceleration), intent(in) :: config_acceleration

        integer(int32), allocatable :: unique_material_ids(:)
        integer(int32) :: ierr
        integer(int32) :: i, num_unique_regions, max_region_id
        integer(int32) :: current_material_id
        character(len=10), allocatable :: profiler_labels(:)
        real(real64) :: current_time_s

        ierr = 0
        call input%geometry%vtk%get_active_region_info(unique_material_ids)
        if (ierr /= 0) return
        if (.not. allocated(unique_material_ids) .or. size(unique_material_ids) == 0) then
            ierr = -1
            print *, "Error: No active material regions found."
            stop 1
        end if

        num_unique_regions = size(unique_material_ids)
        max_region_id = maxval(unique_material_ids)

        self%is_active(PHYSICS_TYPES%THERMAL%ID) = input%basic%analysis_controls%is_active(PHYSICS_TYPES%THERMAL%ID)
        if (self%is_active(PHYSICS_TYPES%THERMAL%ID)) then
            allocate (self%thermal(max_region_id))
            self%thermal = .false.
        end if

        self%is_active(PHYSICS_TYPES%HYDRAULIC%ID) = input%basic%analysis_controls%is_active(PHYSICS_TYPES%HYDRAULIC%ID)
        if (self%is_active(PHYSICS_TYPES%HYDRAULIC%ID)) then
            allocate (self%hydraulic(max_region_id))
            self%hydraulic = .false.
        end if

        self%is_active(PHYSICS_TYPES%MECHANICAL%ID) = input%basic%analysis_controls%is_active(PHYSICS_TYPES%MECHANICAL%ID)
        if (self%is_active(PHYSICS_TYPES%MECHANICAL%ID)) then
            allocate (self%mechanical(max_region_id))
            self%mechanical = .false.
        end if

        do i = 1, num_unique_regions
            current_material_id = unique_material_ids(i)
            if (current_material_id > size(input%basic%materials)) cycle

            if (self%is_active(PHYSICS_TYPES%THERMAL%ID)) then
                self%thermal(current_material_id) = &
                    input%basic%materials(current_material_id)%is_active(PHYSICS_TYPES%THERMAL%ID)
            end if

            if (self%is_active(PHYSICS_TYPES%HYDRAULIC%ID)) then
                self%hydraulic(current_material_id) = &
                    input%basic%materials(current_material_id)%is_active(PHYSICS_TYPES%HYDRAULIC%ID)
            end if

            if (self%is_active(PHYSICS_TYPES%MECHANICAL%ID)) then
                self%mechanical(current_material_id) = &
                    input%basic%materials(current_material_id)%is_active(PHYSICS_TYPES%MECHANICAL%ID)
            end if
        end do

        self%coupling_mode = COUPLING_MODES%to_object(input%basic%analysis_controls%coupling_mode)

        call self%time%initialize(input)
        call self%iteration%initialize(input)
        call initialize_openmp(input)

        select case (config_acceleration%method%ID)
        case (ACCELERATION_METHODS%AITKEN%ID)
            allocate (type_acceleration_aitken :: self%acceleration)
        case (ACCELERATION_METHODS%ANDERSON%ID)
            ! Anderson Acceleration implementation would go here
        end select
        call self%acceleration%initialize(config_acceleration)

        call self%time%get_time(current_time_s)
        associate (field_output => input%output_settings%field_output)
            call self%out_field%initialize(field_output%output_interval_step, &
                                           field_output%output_interval_unit, &
                                           field_output%output_time_unit, &
                                           field_output%file_format, &
                                           current_time_s)
        end associate
        associate (history_output => input%output_settings%history_output)
            call self%out_history%initialize(history_output%output_interval_step, &
                                             history_output%output_interval_unit, &
                                             history_output%output_time_unit, &
                                             history_output%file_format, &
                                             current_time_s)
        end associate

        call deallocate_array(unique_material_ids)

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

        if (.not. self%is_active(target_physics%ID)) then
            is_active = .false.
            return
        end if

        is_active = .false.

        if (target_physics == PHYSICS_TYPES%THERMAL) then
            if (allocated(self%thermal)) then
                if (material_id <= ubound(self%thermal, 1)) then
                    is_active = self%thermal(material_id)
                end if
            end if
        else if (target_physics == PHYSICS_TYPES%HYDRAULIC) then
            if (allocated(self%hydraulic)) then
                if (material_id <= ubound(self%hydraulic, 1)) then
                    is_active = self%hydraulic(material_id)
                end if
            end if
        else if (target_physics == PHYSICS_TYPES%MECHANICAL) then
            if (allocated(self%mechanical)) then
                if (material_id <= ubound(self%mechanical, 1)) then
                    is_active = self%mechanical(material_id)
                end if
            end if
        end if
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

        if (PHYSICS_TYPES%in_group(physics_type)) then
            if (physics_type%ID < 1 .or. physics_type%ID > PHYSICS_TYPES%NUM_ID) then
                is_active = .false.
            end if
            is_active = self%is_active(physics_type%ID)
        else
            is_active = .false.
        end if

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
        implicit none
        class(type_control), intent(in) :: self
        integer(int32) :: i

        write (*, '(a)') "# Control Settings"
        write (*, '(a)') "## Active Physics Types:"
        if (self%is_active(PHYSICS_TYPES%THERMAL%ID)) then
            write (*, '(a)') "- Thermal"
        end if
        if (self%is_active(PHYSICS_TYPES%HYDRAULIC%ID)) then
            write (*, '(a)') "- Hydraulic"
        end if
        if (self%is_active(PHYSICS_TYPES%MECHANICAL%ID)) then
            write (*, '(a)') "- Mechanical"
        end if

        write (*, '(a)') "## Coupling Mode:"
        write (*, '(a)') "- "//trim(self%coupling_mode%name)

        if (allocated(self%thermal)) then
            write (*, '(a)') "### Thermal Material Flags:"
            do i = 1, size(self%thermal)
                write (*, '(a,i0,a,l1)') "- Material ID ", i, ": ", self%thermal(i)
            end do
        end if

        if (allocated(self%hydraulic)) then
            write (*, '(a)') "### Hydraulic Material Flags:"
            do i = 1, size(self%hydraulic)
                write (*, '(a,i0,a,l1)') "- Material ID ", i, ": ", self%hydraulic(i)
            end do
        end if

        if (allocated(self%mechanical)) then
            write (*, '(a)') "### Mechanical Material Flags:"
            do i = 1, size(self%mechanical)
                write (*, '(a,i0,a,l1)') "- Material ID ", i, ": ", self%mechanical(i)
            end do
        end if

        write (*, '(a)') "## Time and Iteration Settings"
        call self%time%display()
        ! call self%iteration%display()

        write (*, '(a)') "---"
    end subroutine display_controls

    ! subroutine initialize_aitken_params(self, num_dofs)
    !     implicit none
    !     class(type_aitken_params), intent(inout) :: self
    !     integer(int32), intent(in) :: num_dofs

    !     call allocate_array(self%du_raw, num_dofs, PHYSICS_TYPES%NUM_ID)

    !     call self%reset()
    ! end subroutine initialize_aitken_params

    ! subroutine destory_aitken_params(self)
    !     implicit none
    !     class(type_aitken_params), intent(inout) :: self

    !     call deallocate_array(self%du_raw)

    !     self%relaxation_factor(:) = 0.0d0
    !     self%previous_relaxation_factor(:) = 0.0d0

    ! end subroutine destory_aitken_params

    ! subroutine reset_aitken_params(self)
    !     implicit none
    !     class(type_aitken_params), intent(inout) :: self

    !     self%relaxation_factor(:) = 0.5d0
    !     self%previous_relaxation_factor(:) = 0.5d0
    ! end subroutine reset_aitken_params

    ! subroutine compute_aitken_relaxation(self, physics_type, du_new)
    !     implicit none
    !     class(type_aitken_params), intent(inout), target :: self
    !     type(type_constant_id), intent(in) :: physics_type
    !     real(real64), intent(in) :: du_new(:)

    !     real(real64), pointer, contiguous, dimension(:) :: du_old => null()

    !     integer(int32) :: pid
    !     real(real64) :: numerator
    !     real(real64) :: denominator

    !     if (.not. PHYSICS_TYPES%is_valid(physics_type)) then
    !         call raise_error(ERROR_CODES%INVALID_TYPE, opt=strip(physics_type%name))
    !     end if

    !     pid = physics_type%ID
    !     du_old => self%du_raw(:, pid)

    !     numerator = vector_dot((du_new - du_old), du_old)
    !     denominator = vector_dot(du_new - du_old, du_new - du_old)

    !     if (denominator > epsilon(1.0d0)) then
    !         self%relaxation_factor(pid) = -self%previous_relaxation_factor(pid) * (numerator / denominator)
    !         ! Relaxation factor limits
    !         if (self%relaxation_factor(pid) < self%min_relaxation) then
    !             self%relaxation_factor(pid) = self%min_relaxation
    !         else if (self%relaxation_factor(pid) > self%max_relaxation) then
    !             self%relaxation_factor(pid) = self%max_relaxation
    !         end if
    !         self%previous_relaxation_factor(pid) = self%relaxation_factor(pid)
    !     else
    !         ! If denominator is too small, keep previous relaxation factor
    !         self%relaxation_factor(pid) = self%previous_relaxation_factor(pid)
    !     end if

    ! end subroutine compute_aitken_relaxation

    ! pure subroutine get_aitken_relaxation(self, physics_type, relaxation_factor)
    !     implicit none
    !     class(type_aitken_params), intent(in) :: self
    !     type(type_constant_id), intent(in) :: physics_type
    !     real(real64), intent(inout) :: relaxation_factor

    !     if (.not. PHYSICS_TYPES%is_valid(physics_type)) then
    !         call raise_error(ERROR_CODES%INVALID_TYPE, opt=strip(physics_type%name))
    !     end if

    !     relaxation_factor = self%relaxation_factor(physics_type%ID)

    ! end subroutine get_aitken_relaxation

    ! subroutine set_du_raw_aitken(self, physics_type, du)
    !     implicit none
    !     class(type_aitken_params), intent(inout) :: self
    !     type(type_constant_id), intent(in) :: physics_type
    !     real(real64), intent(in) :: du(:)

    !     if (.not. PHYSICS_TYPES%is_valid(physics_type)) then
    !         call raise_error(ERROR_CODES%INVALID_TYPE, opt=strip(physics_type%name))
    !     end if

    !     self%du_raw(:, physics_type%ID) = du(:)

    ! end subroutine set_du_raw_aitken

    ! pure function reach_min_relaxation_aitken(self, physics_type) result(is_min_exceeded)
    !     implicit none
    !     class(type_aitken_params), intent(in) :: self
    !     type(type_constant_id), intent(in) :: physics_type
    !     logical :: is_min_exceeded

    !     if (.not. PHYSICS_TYPES%is_valid(physics_type)) then
    !         call raise_error(ERROR_CODES%INVALID_TYPE, opt=strip(physics_type%name))
    !     end if

    !     is_min_exceeded = self%relaxation_factor(physics_type%ID) <= self%min_relaxation

    ! end function reach_min_relaxation_aitken

    pure function is_end_time_control(self) result(is_end_time)
        implicit none
        class(type_control), intent(in) :: self
        logical :: is_end_time

        is_end_time = self%time%is_end_time()
    end function is_end_time_control

    ! !> Comprehensive update of all control states (Time, ATS, Iteration, Output)
    ! subroutine update_controls(self, success)
    !     implicit none
    !     class(type_control), intent(inout) :: self
    !     logical, intent(in) :: success
    !     integer(int32) :: iter_count
    !     real(real64) :: t_target

    !     ! 1. 現在のステップで要した反復回数を取得
    !     call self%iteration%get_nonlinear_iter(iter_count)

    !     ! 2. 同期のターゲット時刻を決定（終了時刻，または各出力時刻の最小値）
    !     call self%time%get_end_time(t_target)

    !     if (self%out_field%is_enabled()) then
    !         t_target = min(t_target, self%out_field%get_next_time())
    !     end if
    !     if (self%out_history%is_enabled()) then
    !         t_target = min(t_target, self%out_history%get_next_time())
    !     end if

    !     ! 3. 時間管理・ATSロジックを一括実行
    !     call self%time%update(success, iter_count, t_target)

    ! end subroutine update_controls

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
        if (self%out_field%is_enabled()) then
            call self%out_field%get_next_target_time(t_arrival, t_target_out)
            t_target = min(t_target, t_target_out)
        end if
        if (self%out_history%is_enabled()) then
            call self%out_history%get_next_target_time(t_arrival, t_target_out)
            t_target = min(t_target, t_target_out)
        end if

        ! 4. 更新実行 (t_target=600 なので，残り300に合わせて dt が制限される)
        call self%time%update(success, iter_count, t_target)

    end subroutine update_controls

end module module_control

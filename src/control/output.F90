module control_output
    use, intrinsic :: iso_fortran_env
    use :: stdlib_optval, only:optval
    use :: stdlib_strings, only:strip
    use :: module_core
    use :: control_time
    implicit none
    private

    public :: type_output_manager

    type :: type_output_manager
        private
        logical :: is_active = .false.
        real(real64) :: interval_seconds = 0.0d0 ! 常に[秒]で保持
        real(real64) :: next_output_seconds = 0.0d0 ! 常に[秒]で保持
        integer(int32) :: current_step = 0
        type(type_constant_value) :: output_time_unit ! 出力時の変換用
    contains
        procedure, pass(self), public :: initialize => initialize_manager
        procedure, pass(self), public :: is_due => check_output_timing
        procedure, pass(self), public :: update => update_state
        procedure, pass(self), public :: get_step
        procedure, pass(self), public :: convert_output_time => convert_output_time_value
        procedure, pass(self), public :: is_enabled
        procedure, pass(self), public :: get_next_time
        procedure, pass(self), public :: get_next_target_time
    end type type_output_manager

contains

    subroutine initialize_manager(self, interval_val, interval_unit_id, output_unit_id, &
                                  file_format, current_time, initial_step)
        implicit none
        class(type_output_manager), intent(inout) :: self
        real(real64), intent(in) :: interval_val
        integer(int32), intent(in) :: interval_unit_id
        integer(int32), intent(in) :: output_unit_id
        character(*), intent(in) :: file_format
        real(real64), intent(in) :: current_time
        integer(int32), intent(in), optional :: initial_step

        type(type_constant_value) :: interval_time_unit

        if (strip(file_format) == "none") then
            self%is_active = .false.
            return
        end if

        self%is_active = .true.

        ! 1. 間隔を「秒」に変換して保存
        interval_time_unit = TIME_UNITS%to_object(interval_unit_id)
        self%interval_seconds = interval_val * interval_time_unit%value

        ! 2. 出力単位オブジェクトを保存（表示・変換用）
        self%output_time_unit = TIME_UNITS%to_object(output_unit_id)

        ! 3. 次回出力時刻を「秒」で設定
        self%next_output_seconds = current_time

        self%current_step = optval(initial_step, 0)
    end subroutine initialize_manager

    ! ----------------------------------------------------------------------
    ! 判定: 引数のcurrent_time(秒)と比較．引数は変更しない (intent(in))
    ! ----------------------------------------------------------------------
    pure function check_output_timing(self, current_time_seconds) result(is_ready)
        implicit none
        class(type_output_manager), intent(in) :: self
        real(real64), intent(in) :: current_time_seconds
        logical :: is_ready
        real(real64), parameter :: tolerance = 1.0d-9

        if (.not. self%is_active) then
            is_ready = .false.
            return
        end if

        if (current_time_seconds >= self%next_output_seconds - tolerance) then
            is_ready = .true.
        else
            is_ready = .false.
        end if
    end function check_output_timing

    ! ----------------------------------------------------------------------
    ! 更新: 次回時刻(秒)を進める
    ! ----------------------------------------------------------------------
    subroutine update_state(self, current_time_seconds)
        implicit none
        class(type_output_manager), intent(inout) :: self
        real(real64), intent(in) :: current_time_seconds
        real(real64), parameter :: tolerance = 1.0d-9
        real(real64) :: diff, steps_to_add

        if (.not. self%is_active) return

        if (self%next_output_seconds <= current_time_seconds + tolerance) then
            diff = (current_time_seconds + tolerance) - self%next_output_seconds

            if (self%interval_seconds > 0.0d0) then
                steps_to_add = floor(diff / self%interval_seconds) + 1.0d0
                self%next_output_seconds = self%next_output_seconds + steps_to_add * self%interval_seconds
            end if
        end if

        self%current_step = self%current_step + 1
    end subroutine update_state

    pure function convert_output_time_value(self, current_time_seconds) result(converted_time)
        implicit none
        class(type_output_manager), intent(in) :: self
        real(real64), intent(in) :: current_time_seconds
        real(real64) :: converted_time

        if (self%output_time_unit%value > 0.0d0) then
            converted_time = current_time_seconds / self%output_time_unit%value
        else
            converted_time = current_time_seconds
        end if
    end function convert_output_time_value

    pure subroutine get_step(self, step)
        implicit none
        class(type_output_manager), intent(in) :: self
        integer(int32), intent(inout) :: step
        step = self%current_step
    end subroutine get_step

    !> 出力が有効かどうかを判定する
    pure function is_enabled(self) result(is_active)
        implicit none
        class(type_output_manager), intent(in) :: self
        logical :: is_active

        is_active = self%is_active
    end function is_enabled

    !> 次回の出力予定時刻（秒）を取得する
    pure function get_next_time(self) result(next_output_seconds)
        implicit none
        class(type_output_manager), intent(in) :: self
        real(real64) :: next_output_seconds

        next_output_seconds = self%next_output_seconds
    end function get_next_time

    !> 現在時刻に基づき，次に同期すべきターゲット時刻を返す
    subroutine get_next_target_time(self, current_time, target_time)
        implicit none
        class(type_output_manager), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(inout) :: target_time

        real(real64), parameter :: tolerance = 1.0d-9

        if (.not. self%is_active) then
            target_time = 1.0d+30 ! 無効なら十分遠い未来
            return
        end if

        ! もし現在時刻がすでに出力予定時刻に達している（または過ぎている）なら，
        ! その「次の間隔」をターゲットとする
        if (current_time >= self%next_output_seconds - tolerance) then
            target_time = self%next_output_seconds + self%interval_seconds
        else
            target_time = self%next_output_seconds
        end if
    end subroutine get_next_target_time

end module control_output

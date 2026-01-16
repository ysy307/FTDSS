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
        procedure, pass(self), public :: get_output_time => get_output_time_value
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
    function check_output_timing(self, current_time_seconds) result(is_ready)
        implicit none
        class(type_output_manager), intent(inout) :: self ! 内部状態を変えないならinでも良い
        real(real64), intent(in) :: current_time_seconds
        logical :: is_ready
        real(real64), parameter :: tolerance = 1.0d-9

        if (.not. self%is_active) then
            is_ready = .false.
            return
        end if

        ! 秒同士で比較
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

    ! ----------------------------------------------------------------------
    ! 便利機能: 現在設定されている出力単位での時間を返す
    ! ----------------------------------------------------------------------
    pure function get_output_time_value(self, current_time_seconds) result(formatted_time)
        implicit none
        class(type_output_manager), intent(in) :: self
        real(real64), intent(in) :: current_time_seconds
        real(real64) :: formatted_time

        ! ここで割り算を行う
        if (self%output_time_unit%value > 0.0d0) then
            formatted_time = current_time_seconds / self%output_time_unit%value
        else
            formatted_time = current_time_seconds
        end if
    end function get_output_time_value

    ! (get_step は変更なし)
    pure subroutine get_step(self, step)
        implicit none
        class(type_output_manager), intent(in) :: self
        integer(int32), intent(inout) :: step
        step = self%current_step
    end subroutine get_step

end module control_output

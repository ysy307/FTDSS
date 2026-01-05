module control_output
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: control_time
    use :: module_input, only:type_input
    implicit none
    private

    public :: type_output_manager

    ! ----------------------------------------------------------------------
    ! 出力タイミングおよびステップ数管理用クラス
    ! ----------------------------------------------------------------------
    type :: type_output_manager
        private
        logical :: is_active = .false.
        real(real64) :: interval_seconds = 0.0d0
        real(real64) :: next_output_time = 0.0d0

        ! [追加] ステップ数（反復回数）カウンタ
        integer(int32) :: current_step = 0
    contains
        procedure, pass(self), public :: initialize => initialize_manager
        procedure, pass(self), public :: is_due => check_output_timing
        procedure, pass(self), public :: update => update_state
        procedure, pass(self), public :: get_step
    end type type_output_manager

contains

    ! ----------------------------------------------------------------------
    ! 初期化
    ! ----------------------------------------------------------------------
    subroutine initialize_manager(self, interval_val, interval_unit, file_format, time_manager, initial_step)
        implicit none
        class(type_output_manager), intent(inout) :: self
        real(real64), intent(in) :: interval_val
        integer(int32), intent(in) :: interval_unit
        character(*), intent(in) :: file_format
        type(type_time), intent(in) :: time_manager
        integer(int32), intent(in), optional :: initial_step

        real(real64) :: conv, current_t

        if (trim(file_format) == "none") then
            self%is_active = .false.
            return
        end if

        self%is_active = .true.

        ! 時間間隔の設定
        call time_manager%convert_time_unit(interval_unit, TIME_UNIT_SECONDS, conv)
        self%interval_seconds = interval_val * conv

        ! 初回出力時刻の設定
        call time_manager%get_time(current_t)
        self%next_output_time = current_t

        ! ステップ数の初期化
        if (present(initial_step)) then
            self%current_step = initial_step
        else
            self%current_step = 0
        end if
    end subroutine initialize_manager

    ! ----------------------------------------------------------------------
    ! 出力判定
    ! ----------------------------------------------------------------------
    function check_output_timing(self, current_time) result(is_ready)
        implicit none
        class(type_output_manager), intent(inout) :: self
        real(real64), intent(in) :: current_time
        logical :: is_ready
        real(real64), parameter :: tolerance = 1.0d-9

        if (.not. self%is_active) then
            is_ready = .false.
            return
        end if

        if (current_time >= self%next_output_time - tolerance) then
            is_ready = .true.
        else
            is_ready = .false.
        end if
    end function check_output_timing

    ! ----------------------------------------------------------------------
    ! 状態更新: 次回時刻の設定とステップ数のインクリメント
    ! ----------------------------------------------------------------------
    subroutine update_state(self, current_time)
        implicit none
        class(type_output_manager), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), parameter :: tolerance = 1.0d-9

        real(real64) :: diff, steps_to_add

        if (.not. self%is_active) return

        ! 次回時刻が現在時刻以下である限り，間隔を加算し続ける（追いつく処理）
        ! ※ do while でも良いですが，計算で一発で求める方が高速かつ安全です
        if (self%next_output_time <= current_time + tolerance) then
            ! 遅れている分を計算
            diff = (current_time + tolerance) - self%next_output_time

            if (self%interval_seconds > 0.0d0) then
                steps_to_add = floor(diff / self%interval_seconds) + 1.0d0
                self%next_output_time = self%next_output_time + steps_to_add * self%interval_seconds
            end if
        end if

        ! ファイル番号（ステップ数）は出力回数に応じて1つだけ進める
        self%current_step = self%current_step + 1

    end subroutine update_state

    ! ----------------------------------------------------------------------
    ! 現在のステップ数を取得
    ! ----------------------------------------------------------------------
    pure subroutine get_step(self, step)
        implicit none
        class(type_output_manager), intent(in) :: self
        integer(int32), intent(inout) :: step

        step = self%current_step
    end subroutine get_step

end module control_output

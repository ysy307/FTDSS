module core_types_datetime
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
    private

    public :: type_datetime

    type :: type_datetime
        integer(int32), private :: year = 1970
        integer(int32), private :: month = 1
        integer(int32), private :: day = 1
        integer(int32), private :: hour = 0
        integer(int32), private :: minute = 0
        integer(int32), private :: second = 0
        integer(int32), private :: millisecond = 0
        integer(int32), private :: timezone_offset = 0
    contains
        procedure, pass(self), public :: set => set_datetime
        procedure, pass(self), public :: set_now => set_now_datetime
        procedure, pass(self), public :: format => format_iso_datetime

        procedure, pass(self), public :: get => get_datetime

        procedure, pass(self), private :: get_doy_int => get_doy_int_datetime
        procedure, pass(self), private :: get_doy_dp => get_doy_dp_datetime
        generic, public :: get_doy => get_doy_int, get_doy_dp

        procedure, pass(self), private :: is_leap_year
    end type type_datetime

contains

    !---------------------------------------------------------------------------
    ! 日時を手動設定
    !---------------------------------------------------------------------------
    subroutine set_datetime(self, year, month, day, hour, minute, second, ms)
        implicit none
        class(type_datetime), intent(inout) :: self
        integer(int32), intent(in) :: year, month, day
        integer(int32), intent(in), optional :: hour, minute, second, ms

        self%year = year
        self%month = month
        self%day = day

        if (present(hour)) self%hour = hour
        if (present(minute)) self%minute = minute
        if (present(second)) self%second = second
        if (present(ms)) self%millisecond = ms
    end subroutine set_datetime

    !---------------------------------------------------------------------------
    ! 現在時刻を取得して設定
    !---------------------------------------------------------------------------
    subroutine set_now_datetime(self)
        implicit none
        class(type_datetime), intent(inout) :: self
        integer(int32) :: time_values(8)

        call date_and_time(values=time_values)

        self%year = time_values(1)
        self%month = time_values(2)
        self%day = time_values(3)
        self%timezone_offset = time_values(4)
        self%hour = time_values(5)
        self%minute = time_values(6)
        self%second = time_values(7)
        self%millisecond = time_values(8)
    end subroutine set_now_datetime

    !---------------------------------------------------------------------------
    ! ISO 8601形式の文字列を返す (YYYY-MM-DDThh:mm:ss.sss)
    !---------------------------------------------------------------------------
    function format_iso_datetime(self) result(str)
        implicit none
        class(type_datetime), intent(in) :: self
        character(len=23) :: str

        write (str, '(I4.4, "-", I2.2, "-", I2.2, "T", I2.2, ":", I2.2, ":", I2.2, ".", I3.3)') &
            self%year, self%month, self%day, &
            self%hour, self%minute, self%second, self%millisecond
    end function format_iso_datetime

    !---------------------------------------------------------------------------
    ! 通算日 (DOY) を整数で返す (1月1日 = 1)
    !---------------------------------------------------------------------------
    pure subroutine get_doy_int_datetime(self, doy)
        implicit none
        class(type_datetime), intent(in) :: self
        integer(int32), intent(inout) :: doy

        integer(int32) :: days_in_month(12)
        integer(int32) :: i

        ! 各月の日数 (平年)
        days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

        ! うるう年の場合，2月を29日に変更
        if (self%is_leap_year()) then
            days_in_month(2) = 29
        end if

        doy = 0
        ! 前月までの日数を加算
        do i = 1, self%month - 1
            doy = doy + days_in_month(i)
        end do

        ! 当月の日を加算
        doy = doy + self%day
    end subroutine get_doy_int_datetime

    !---------------------------------------------------------------------------
    ! 通算日 (DOY) を倍精度実数で返す (時刻情報を小数部として付加)
    ! 例: 1月1日 12:00:00 -> 1.5
    !---------------------------------------------------------------------------
    subroutine get_doy_dp_datetime(self, doy)
        implicit none
        class(type_datetime), intent(in) :: self
        real(real64), intent(inout) :: doy

        integer(int32) :: doy_int
        real(real64) :: sec_in_day

        ! 整数のDOYを取得
        call self%get_doy_int(doy_int)
        doy = real(doy_int, kind=real64)

        ! 1日の経過秒数を計算し，日の小数部に変換 (1日 = 86400秒)
        sec_in_day = real(self%hour, real64) * 3600.0d0 + &
                     real(self%minute, real64) * 60.0d0 + &
                     real(self%second, real64) + &
                     real(self%millisecond, real64) / 1000.0d0

        ! 日数に時刻の割合を加算 (DOYは1始まりだが，時刻00:00は.0なのでそのまま加算)
        doy = doy + (sec_in_day / 86400.0d0)

    end subroutine get_doy_dp_datetime

    subroutine get_datetime(self, year, month, day, hour, minute, second, tz_offset)
        implicit none
        class(type_datetime), intent(in) :: self
        integer(int32), intent(inout) :: year, month, day
        integer(int32), intent(inout), optional :: hour, minute, second, tz_offset

        year = self%year
        month = self%month
        day = self%day
        if (present(hour)) hour = self%hour
        if (present(minute)) minute = self%minute
        if (present(second)) second = self%second
        if (present(tz_offset)) tz_offset = self%timezone_offset

    end subroutine get_datetime
    !---------------------------------------------------------------------------
    ! うるう年判定 (Private Helper)
    !---------------------------------------------------------------------------
    pure function is_leap_year(self) result(is_leap)
        implicit none
        class(type_datetime), intent(in) :: self
        logical :: is_leap

        if (mod(self%year, 400) == 0) then
            is_leap = .true.
        else if (mod(self%year, 100) == 0) then
            is_leap = .false.
        else if (mod(self%year, 4) == 0) then
            is_leap = .true.
        else
            is_leap = .false.
        end if
    end function is_leap_year

end module core_types_datetime

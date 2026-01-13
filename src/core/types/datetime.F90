!> Base module for date and time management.
!>
!> This module defines the base class for handling date and time.
!> It provides methods to set, get, and format datetime information.
module core_types_datetime
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
    private

    public :: type_datetime

    !> A class to hold and manipulate date and time information.
    !>
    !> Manages year, month, day, hour, minute, second, millisecond, and timezone offset.
    type :: type_datetime
        private
        !> Year
        integer(int32) :: year = 1970
        !> Month
        integer(int32) :: month = 1
        !> Day
        integer(int32) :: day = 1
        !> Hour
        integer(int32) :: hour = 0
        !> Minute
        integer(int32) :: minute = 0
        !> Second
        integer(int32) :: second = 0
        !> Millisecond
        integer(int32) :: millisecond = 0
        !> Timezone offset in minutes
        integer(int32) :: timezone_offset = 0
    contains
        !> Sets the date and time manually.
        procedure, private, pass(self) :: set_datetime
        !> Sets the date and time from an ISO 8601 string.
        procedure, private, pass(self) :: set_from_iso_string
        !> Sets the current system time.
        procedure, private, pass(self) :: set_now_datetime
        generic, public :: set => set_datetime, set_from_iso_string, set_now_datetime
        !> Returns the date and time as an ISO 8601 string (YYYY-MM-DDThh:mm:ss+HH:MM).
        procedure, public, pass(self) :: format => format_iso_datetime
        !> Gets the component values.
        procedure, public, pass(self) :: get => get_datetime_components
    end type type_datetime

contains

    !> Manually sets the date and time.
    subroutine set_datetime(self, year, month, day, hour, minute, second, millisecond, timezone_offset)
        implicit none
        !> The type_datetime instance
        class(type_datetime), intent(inout) :: self
        !> Year
        integer(int32), intent(in) :: year
        !> Month
        integer(int32), intent(in) :: month
        !> Day
        integer(int32), intent(in) :: day
        !> Hour
        integer(int32), intent(in), optional :: hour
        !> Minute
        integer(int32), intent(in), optional :: minute
        !> Second
        integer(int32), intent(in), optional :: second
        !> Millisecond
        integer(int32), intent(in), optional :: millisecond
        !> Timezone offset in minutes
        integer(int32), intent(in), optional :: timezone_offset

        self%year = year
        self%month = month
        self%day = day
        if (present(hour)) self%hour = hour
        if (present(minute)) self%minute = minute
        if (present(second)) self%second = second
        if (present(millisecond)) self%millisecond = millisecond
        if (present(timezone_offset)) self%timezone_offset = timezone_offset
    end subroutine set_datetime

    !> Sets the date and time from an ISO 8601 string.
    !>
    !> Expected format: `YYYY-MM-DDThh:mm:ss+HH:MM` (Length 25)
    subroutine set_from_iso_string(self, iso_str, stat)
        implicit none
        !> The type_datetime instance
        class(type_datetime), intent(inout) :: self
        !> ISO 8601 formatted string
        character(len=*), intent(in) :: iso_str
        !> Status flag (0: success, non-zero: error). Optional.
        integer(int32), intent(inout), optional :: stat

        integer(int32) :: y, m, d, h, n, s, tzh, tzm
        character(len=1) :: sig
        integer(int32) :: ios

        ! 初期化
        if (present(stat)) stat = 0

        ! 文字列長の簡易チェック
        if (len_trim(iso_str) < 25) then
            if (present(stat)) stat = -1
            return
        end if

        ! 内部読み込み (Internal Read)
        ! Format: YYYY(I4) -(1x) MM(I2) -(1x) DD(I2) T(1x) hh(I2) :(1x) mm(I2) :(1x) ss(I2) sig(A1) HH(I2) :(1x) MM(I2)
        read (iso_str, '(i4, 1x, i2, 1x, i2, 1x, i2, 1x, i2, 1x, i2, a1, i2, 1x, i2)', iostat=ios) &
            y, m, d, h, n, s, sig, tzh, tzm

        if (ios /= 0) then
            if (present(stat)) stat = ios
            return
        end if

        ! 値の設定
        self%year = y
        self%month = m
        self%day = d
        self%hour = h
        self%minute = n
        self%second = s
        self%millisecond = 0 ! ISO文字列にミリ秒が含まれないため0リセット

        ! タイムゾーンオフセットの計算 (分単位)
        self%timezone_offset = tzh * 60 + tzm
        if (sig == '-') then
            self%timezone_offset = -self%timezone_offset
        end if
    end subroutine set_from_iso_string

    !> Sets the current time from the system clock.
    subroutine set_now_datetime(self)
        implicit none
        !> The type_datetime instance
        class(type_datetime), intent(inout) :: self

        !> Array to hold date_and_time output
        integer(int32) :: values(8)

        ! date_and_time returns: [year, month, day, diff(min), hour, min, sec, millisecond]
        call date_and_time(values=values)

        self%year = values(1)
        self%month = values(2)
        self%day = values(3)
        self%timezone_offset = values(4)
        self%hour = values(5)
        self%minute = values(6)
        self%second = values(7)
        self%millisecond = values(8)
    end subroutine set_now_datetime

    !> Gets the components of the datetime.
    subroutine get_datetime_components(self, y, m, d, h, min, sec, tz)
        implicit none
        !> The type_datetime instance
        class(type_datetime), intent(in) :: self
        !> Year
        integer(int32), intent(inout) :: y
        !> Month
        integer(int32), intent(inout) :: m
        !> Day
        integer(int32), intent(inout) :: d
        !> Hour
        integer(int32), intent(inout) :: h
        !> Minute
        integer(int32), intent(inout) :: min
        !> Second
        integer(int32), intent(inout) :: sec
        !> Timezone offset
        integer(int32), intent(inout) :: tz

        y = self%year
        m = self%month
        d = self%day
        h = self%hour
        min = self%minute
        sec = self%second
        tz = self%timezone_offset
    end subroutine get_datetime_components

    !> Formats the date and time as an ISO 8601 string with timezone.
    !>
    !> Format: `YYYY-MM-DDThh:mm:ss+HH:MM` (Length: 25)
    function format_iso_datetime(self) result(res)
        implicit none
        !> The type_datetime instance
        class(type_datetime), intent(in) :: self
        !> The formatted string (length 25)
        character(len=25) :: res

        character(len=1) :: sign
        integer(int32) :: abs_tz, tz_h, tz_m

        ! Determine timezone sign
        if (self%timezone_offset >= 0) then
            sign = '+'
        else
            sign = '-'
        end if

        ! Calculate timezone hours and minutes
        abs_tz = abs(self%timezone_offset)
        tz_h = abs_tz / 60
        tz_m = mod(abs_tz, 60)

        write (res, '(i4.4, "-", i2.2, "-", i2.2, "T", i2.2, ":", i2.2, ":", i2.2, A, i2.2, ":", i2.2)') &
            self%year, self%month, self%day, &
            self%hour, self%minute, self%second, &
            sign, tz_h, tz_m
    end function format_iso_datetime

end module core_types_datetime

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
        procedure, public, pass(self) :: set => set_datetime
        !> Sets the current system time.
        procedure, public, pass(self) :: set_now => set_now_datetime
        !> Returns the date and time as an ISO 8601 string.
        procedure, public, pass(self) :: format => format_iso_datetime
        !> Gets the component values.
        procedure, public, pass(self) :: get => get_datetime_components
    end type type_datetime

contains

    !> Manually sets the date and time.
    subroutine set_datetime(self, year, month, day, hour, minute, second, ms, tz_off)
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
        integer(int32), intent(in), optional :: ms
        !> Timezone offset in minutes
        integer(int32), intent(in), optional :: tz_off

        self%year = year
        self%month = month
        self%day = day
        if (present(hour)) self%hour = hour
        if (present(minute)) self%minute = minute
        if (present(second)) self%second = second
        if (present(ms)) self%millisecond = ms
        if (present(tz_off)) self%timezone_offset = tz_off
    end subroutine set_datetime

    !> Sets the current time from the system clock.
    subroutine set_now_datetime(self)
        implicit none
        !> The type_datetime instance
        class(type_datetime), intent(inout) :: self

        !> Array to hold date_and_time output
        integer(int32) :: values(8)

        ! date_and_time returns: [year, month, day, diff(min), hour, min, sec, ms]
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
    !>
    !> Uses intent(inout) for output variables to comply with project conventions.
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

    !> Formats the date and time as an ISO 8601 string.
    !>
    !> Format: `YYYY-MM-DDThh:mm:ss.sss`
    function format_iso_datetime(self) result(res)
        implicit none
        !> The type_datetime instance
        class(type_datetime), intent(in) :: self
        !> The formatted string (length 23)
        character(len=23) :: res

        write (res, '(I4.4, "-", I2.2, "-", I2.2, "T", I2.2, ":", I2.2, ":", I2.2, ".", I3.3)') &
            self%year, self%month, self%day, &
            self%hour, self%minute, self%second, self%millisecond
    end function format_iso_datetime

end module core_types_datetime

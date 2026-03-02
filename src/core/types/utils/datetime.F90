!> Base module for date and time management.
!>
!> This module defines the base class for handling date and time.
!> It provides methods to set, get, and format datetime information.
module types_utils_datetime
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_optval, only:optval
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
        !> Timezone offset from UTC in minutes
        integer(int32) :: timezone_offset = 0
    contains
        !> Sets the date and time manually.
        procedure, private, pass(self) :: set_datetime
        !> Sets the date and time from an ISO 8601 string.
        procedure, private, pass(self) :: set_from_iso_string
        !> Sets the current system time.
        procedure, private, pass(self) :: set_now_datetime
        generic, public :: set => set_datetime, set_from_iso_string, set_now_datetime

        !> Validates if a string is in a supported ISO 8601 format.
        procedure, public, pass(self) :: is_iso => is_iso_string_datetime
        !> Returns the date and time as an ISO 8601 string.
        procedure, public, pass(self) :: format => format_iso_datetime
        !> Gets the component values.
        procedure, public, pass(self) :: get => get_datetime_components
    end type type_datetime

contains

    !> Manually sets the date and time components.
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
        self%hour = optval(hour, 0)
        self%minute = optval(minute, 0)
        self%second = optval(second, 0)
        self%millisecond = optval(millisecond, 0)
        self%timezone_offset = optval(timezone_offset, 0)
    end subroutine set_datetime

    !> Parses an ISO 8601 string and updates the object.
    subroutine set_from_iso_string(self, iso_str, stat)
        implicit none
        !> The type_datetime instance
        class(type_datetime), intent(inout) :: self
        !> ISO 8601 formatted string
        character(len=*), intent(in) :: iso_str
        !> Status flag (0: success, non-zero: error). Optional.
        integer(int32), intent(inout), optional :: stat

        integer(int32) :: length, ios, tzh, tzm
        character(len=1) :: sig

        if (present(stat)) stat = 0
        length = len_trim(iso_str)

        ! 1. Validate format
        if (.not. self%is_iso(iso_str)) then
            if (present(stat)) stat = -1
            return
        end if

        ! 2. Reset defaults
        self%hour = 0
        self%minute = 0
        self%second = 0
        self%millisecond = 0
        self%timezone_offset = 0

        ! 3. Parse components based on string length
        ! Date: YYYY-MM-DD
        read (iso_str(1:10), '(i4, 1x, i2, 1x, i2)', iostat=ios) self%year, self%month, self%day
        if (ios /= 0) then
            if (present(stat)) stat = ios
            return
        end if

        ! Time: hh:mm
        if (length >= 16) then
            read (iso_str(12:16), '(i2, 1x, i2)', iostat=ios) self%hour, self%minute
            if (ios /= 0) then
                if (present(stat)) stat = ios
                return
            end if
        end if

        ! Seconds: ss
        if (length >= 19) then
            read (iso_str(18:19), '(i2)', iostat=ios) self%second
            if (ios /= 0) then
                if (present(stat)) stat = ios
                return
            end if
        end if

        ! Timezone Offset: Z or +/-HH:MM
        if (index(iso_str, 'Z') > 0) then
            self%timezone_offset = 0
        else if (length >= 25) then
            read (iso_str(20:25), '(a1, i2, 1x, i2)', iostat=ios) sig, tzh, tzm
            if (ios == 0) then
                self%timezone_offset = tzh * 60 + tzm
                if (sig == '-') self%timezone_offset = -self%timezone_offset
            else
                if (present(stat)) stat = ios
                return
            end if
        end if
    end subroutine set_from_iso_string

    !> Sets components using the system clock.
    subroutine set_now_datetime(self)
        implicit none
        !> The type_datetime instance
        class(type_datetime), intent(inout) :: self

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

    !> Validates ISO 8601 format (pure function for safety).
    pure function is_iso_string_datetime(self, iso_str) result(is_iso)
        implicit none
        !> The type_datetime instance
        class(type_datetime), intent(in) :: self
        !> ISO 8601 formatted string
        character(len=*), intent(in) :: iso_str
        !> Result flag
        logical :: is_iso

        integer(int32) :: length, ios, y, m, d, h, n, s
        character(len=1) :: sep_t, sep_z

        is_iso = .false.
        length = len_trim(iso_str)
        if (length < 10) return

        ! YYYY-MM-DD
        if (iso_str(5:5) /= '-' .or. iso_str(8:8) /= '-') return
        read (iso_str(1:4), '(i4)', iostat=ios) y
        if (ios /= 0) return
        read (iso_str(6:7), '(i2)', iostat=ios) m
        if (ios /= 0 .or. m < 1 .or. m > 12) return
        read (iso_str(9:10), '(i2)', iostat=ios) d
        if (ios /= 0 .or. d < 1 .or. d > 31) return

        if (length == 10) then
            is_iso = .true.
            return
        end if

        ! Time Part (T)
        sep_t = iso_str(11:11)
        if (sep_t /= 'T' .and. sep_t /= ' ') return

        ! hh:mm
        if (length >= 16) then
            if (iso_str(14:14) /= ':') return
            read (iso_str(12:13), '(i2)', iostat=ios) h
            if (ios /= 0 .or. h < 0 .or. h > 23) return
            read (iso_str(15:16), '(i2)', iostat=ios) n
            if (ios /= 0 .or. n < 0 .or. n > 59) return
        end if

        ! ss
        if (length >= 19) then
            if (iso_str(17:17) /= ':') return
            read (iso_str(18:19), '(i2)', iostat=ios) s
            if (ios /= 0 .or. s < 0 .or. s > 59) return
        end if

        ! Timezone Suffix
        if (length > 19) then
            sep_z = iso_str(20:20)
            if (sep_z /= 'Z' .and. sep_z /= '+' .and. sep_z /= '-') return
        end if

        is_iso = .true.
    end function is_iso_string_datetime

    !> Outputs the state as a full ISO 8601 string.
    function format_iso_datetime(self) result(formatted_strings)
        implicit none
        !> The type_datetime instance
        class(type_datetime), intent(in) :: self
        !> The formatted string (length 25)
        character(len=25) :: formatted_strings

        character(len=1) :: sign
        integer(int32) :: abs_tz, tz_h, tz_m

        !> Determine the sign for the timezone offset
        if (self%timezone_offset >= 0) then
            sign = '+'
        else
            sign = '-'
        end if

        ! Calculate timezone hours and minutes
        abs_tz = abs(self%timezone_offset)
        tz_h = abs_tz / 60
        tz_m = mod(abs_tz, 60)

        write (formatted_strings, '(i4.4, "-", i2.2, "-", i2.2, "T", i2.2, ":", i2.2, ":", i2.2, A, i2.2, ":", i2.2)') &
            self%year, self%month, self%day, self%hour, self%minute, self%second, sign, tz_h, tz_m
    end function format_iso_datetime

    !> Returns components via arguments.
    subroutine get_datetime_components(self, y, m, d, h, min, sec, tz)
        implicit none
        !> The type_datetime instance
        class(type_datetime), intent(in) :: self
        !> Year component
        integer(int32), intent(inout) :: y
        !> Month component
        integer(int32), intent(inout) :: m
        !> Day component
        integer(int32), intent(inout) :: d
        !> Hour component
        integer(int32), intent(inout) :: h
        !> Minute component
        integer(int32), intent(inout) :: min
        !> Second component
        integer(int32), intent(inout) :: sec
        !> Timezone offset component
        integer(int32), intent(inout) :: tz
        
        y = self%year
        m = self%month
        d = self%day
        h = self%hour
        min = self%minute
        sec = self%second
        tz = self%timezone_offset
    end subroutine get_datetime_components

end module types_utils_datetime

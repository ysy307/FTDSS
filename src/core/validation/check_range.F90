!>
!> Provides a generic function for checking if a numeric value is within an
!> inclusive range.
!>
module core_check_range
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    public :: value_in_range

    !>
    !> A generic interface for checking if a value is within a given range.
    !>
    interface value_in_range
        module procedure :: value_in_range_int8
        module procedure :: value_in_range_int16
        module procedure :: value_in_range_int32
        module procedure :: value_in_range_int64
        module procedure :: value_in_range_real32
        module procedure :: value_in_range_real64
        module procedure :: value_in_range_real128
    end interface value_in_range

contains

    !>
    !> Checks if an 8-bit integer value is within the inclusive range [min, max].
    !>
    function value_in_range_int8(value, min, max) result(in_range)
        implicit none
        !> The value to check.
        integer(int8), intent(in) :: value
        !> The minimum value of the inclusive range.
        integer(int8), intent(in) :: min
        !> The maximum value of the inclusive range.
        integer(int8), intent(in) :: max
        !> Returns .true. if value is in [min, max], otherwise .false.
        logical :: in_range

        in_range = (value >= min .and. value <= max)
    end function value_in_range_int8

    !>
    !> Checks if a 16-bit integer value is within the inclusive range [min, max].
    !>
    function value_in_range_int16(value, min, max) result(in_range)
        implicit none
        !> The value to check.
        integer(int16), intent(in) :: value
        !> The minimum value of the inclusive range.
        integer(int16), intent(in) :: min
        !> The maximum value of the inclusive range.
        integer(int16), intent(in) :: max
        !> Returns .true. if value is in [min, max], otherwise .false.
        logical :: in_range

        in_range = (value >= min .and. value <= max)
    end function value_in_range_int16

    !>
    !> Checks if a 32-bit integer value is within the inclusive range [min, max].
    !>
    function value_in_range_int32(value, min, max) result(in_range)
        implicit none
        !> The value to check.
        integer(int32), intent(in) :: value
        !> The minimum value of the inclusive range.
        integer(int32), intent(in) :: min
        !> The maximum value of the inclusive range.
        integer(int32), intent(in) :: max
        !> Returns .true. if value is in [min, max], otherwise .false.
        logical :: in_range

        in_range = (value >= min .and. value <= max)
    end function value_in_range_int32

    !>
    !> Checks if a 64-bit integer value is within the inclusive range [min, max].
    !>
    function value_in_range_int64(value, min, max) result(in_range)
        implicit none
        !> The value to check.
        integer(int64), intent(in) :: value
        !> The minimum value of the inclusive range.
        integer(int64), intent(in) :: min
        !> The maximum value of the inclusive range.
        integer(int64), intent(in) :: max
        !> Returns .true. if value is in [min, max], otherwise .false.
        logical :: in_range

        in_range = (value >= min .and. value <= max)
    end function value_in_range_int64

    !>
    !> Checks if a single precision real value is within the inclusive range [min, max].
    !>
    function value_in_range_real32(value, min, max) result(in_range)
        implicit none
        !> The value to check.
        real(real32), intent(in) :: value
        !> The minimum value of the inclusive range.
        real(real32), intent(in) :: min
        !> The maximum value of the inclusive range.
        real(real32), intent(in) :: max
        !> Returns .true. if value is in [min, max], otherwise .false.
        logical :: in_range

        in_range = (value >= min .and. value <= max)
    end function value_in_range_real32

    !>
    !> Checks if a double precision real value is within the inclusive range [min, max].
    !>
    function value_in_range_real64(value, min, max) result(in_range)
        implicit none
        !> The value to check.
        real(real64), intent(in) :: value
        !> The minimum value of the inclusive range.
        real(real64), intent(in) :: min
        !> The maximum value of the inclusive range.
        real(real64), intent(in) :: max
        !> Returns .true. if value is in [min, max], otherwise .false.
        logical :: in_range

        in_range = (value >= min .and. value <= max)
    end function value_in_range_real64

    !>
    !> Checks if a quad precision real value is within the inclusive range [min, max].
    !>
    function value_in_range_real128(value, min, max) result(in_range)
        implicit none
        !> The value to check.
        real(real128), intent(in) :: value
        !> The minimum value of the inclusive range.
        real(real128), intent(in) :: min
        !> The maximum value of the inclusive range.
        real(real128), intent(in) :: max
        !> Returns .true. if value is in [min, max], otherwise .false.
        logical :: in_range

        in_range = (value >= min .and. value <= max)
    end function value_in_range_real128

end module core_check_range

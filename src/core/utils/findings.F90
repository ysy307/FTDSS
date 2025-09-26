!>
!> Provides a generic binary search function for finding a value in a sorted
!> integer array.
!>
module core_findings
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    public :: binary_find

    !>
    !> A generic interface for performing a binary search on integer arrays of
    !> different kinds.
    !>
    interface binary_find
        module procedure :: binary_find_int8
        module procedure :: binary_find_int16
        module procedure :: binary_find_int32
        module procedure :: binary_find_int64
    end interface

contains

    !>
    !> Performs a binary search for an 8-bit integer value in a sorted array
    !> or a sub-section of it.
    !>
    pure function binary_find_int8(value, array, start_idx, end_idx) result(idx)
        implicit none
        !> The value to search for.
        integer(int8), intent(in) :: value
        !> The sorted array to search within.
        integer(int8), intent(in) :: array(:)
        !> 1-based starting index of the search range. Defaults to the array's lower bound.
        integer(int32), intent(in), optional :: start_idx
        !> 1-based ending index of the search range. Defaults to the array's upper bound.
        integer(int32), intent(in), optional :: end_idx
        !> The 1-based index of the value if found; otherwise, 0.
        integer(int32) :: idx
        integer(int32) :: low, high, mid

        if (present(start_idx)) then
            low = start_idx
        else
            low = lbound(array, 1)
        end if
        if (present(end_idx)) then
            high = end_idx
        else
            high = ubound(array, 1)
        end if
        idx = 0

        do while (low <= high)
            mid = low + (high - low) / 2
            if (array(mid) == value) then
                idx = mid
                return
            else if (array(mid) < value) then
                low = mid + 1
            else
                high = mid - 1
            end if
        end do
    end function binary_find_int8

    !>
    !> Performs a binary search for a 16-bit integer value in a sorted array
    !> or a sub-section of it.
    !>
    pure function binary_find_int16(value, array, start_idx, end_idx) result(idx)
        implicit none
        !> The value to search for.
        integer(int16), intent(in) :: value
        !> The sorted array to search within.
        integer(int16), intent(in) :: array(:)
        !> 1-based starting index of the search range. Defaults to the array's lower bound.
        integer(int32), intent(in), optional :: start_idx
        !> 1-based ending index of the search range. Defaults to the array's upper bound.
        integer(int32), intent(in), optional :: end_idx
        !> The 1-based index of the value if found; otherwise, 0.
        integer(int32) :: idx
        integer(int32) :: low, high, mid

        if (present(start_idx)) then
            low = start_idx
        else
            low = lbound(array, 1)
        end if
        if (present(end_idx)) then
            high = end_idx
        else
            high = ubound(array, 1)
        end if
        idx = 0

        do while (low <= high)
            mid = low + (high - low) / 2
            if (array(mid) == value) then
                idx = mid
                return
            else if (array(mid) < value) then
                low = mid + 1
            else
                high = mid - 1
            end if
        end do
    end function binary_find_int16

    !>
    !> Performs a binary search for a 32-bit integer value in a sorted array
    !> or a sub-section of it.
    !>
    pure function binary_find_int32(value, array, start_idx, end_idx) result(idx)
        implicit none
        !> The value to search for.
        integer(int32), intent(in) :: value
        !> The sorted array to search within.
        integer(int32), intent(in) :: array(:)
        !> 1-based starting index of the search range. Defaults to the array's lower bound.
        integer(int32), intent(in), optional :: start_idx
        !> 1-based ending index of the search range. Defaults to the array's upper bound.
        integer(int32), intent(in), optional :: end_idx
        !> The 1-based index of the value if found; otherwise, 0.
        integer(int32) :: idx
        integer(int32) :: low, high, mid

        if (present(start_idx)) then
            low = start_idx
        else
            low = lbound(array, 1)
        end if
        if (present(end_idx)) then
            high = end_idx
        else
            high = ubound(array, 1)
        end if
        idx = 0

        do while (low <= high)
            mid = low + (high - low) / 2
            if (array(mid) == value) then
                idx = mid
                return
            else if (array(mid) < value) then
                low = mid + 1
            else
                high = mid - 1
            end if
        end do
    end function binary_find_int32

    !>
    !> Performs a binary search for a 64-bit integer value in a sorted array
    !> or a sub-section of it.
    !>
    pure function binary_find_int64(value, array, start_idx, end_idx) result(idx)
        implicit none
        !> The value to search for.
        integer(int64), intent(in) :: value
        !> The sorted array to search within.
        integer(int64), intent(in) :: array(:)
        !> 1-based starting index of the search range. Defaults to the array's lower bound.
        integer(int64), intent(in), optional :: start_idx
        !> 1-based ending index of the search range. Defaults to the array's upper bound.
        integer(int64), intent(in), optional :: end_idx
        !> The 1-based index of the value if found; otherwise, 0.
        integer(int64) :: idx
        integer(int64) :: low, high, mid

        if (present(start_idx)) then
            low = start_idx
        else
            low = lbound(array, 1)
        end if
        if (present(end_idx)) then
            high = end_idx
        else
            high = ubound(array, 1)
        end if
        idx = 0

        do while (low <= high)
            mid = low + (high - low) / 2
            if (array(mid) == value) then
                idx = mid
                return
            else if (array(mid) < value) then
                low = mid + 1
            else
                high = mid - 1
            end if
        end do
    end function binary_find_int64

end module core_findings

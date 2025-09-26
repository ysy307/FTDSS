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
    !> Performs a binary search for an 8-bit integer value in a sorted array.
    !>
    pure function binary_find_int8(value, array, start_idx, end_idx) result(idx)
        ! Arguments
        integer(int8), intent(in) :: value
        integer(int8), intent(in) :: array(:)
        integer(int32), intent(in), optional :: start_idx
        integer(int32), intent(in), optional :: end_idx
        ! Result
        integer(int32) :: idx
        ! Local variables
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
    !> Performs a binary search for a 16-bit integer value in a sorted array.
    !>
    pure function binary_find_int16(value, array, start_idx, end_idx) result(idx)
        ! Arguments
        integer(int16), intent(in) :: value
        integer(int16), intent(in) :: array(:)
        integer(int32), intent(in), optional :: start_idx
        integer(int32), intent(in), optional :: end_idx
        ! Result
        integer(int32) :: idx
        ! Local variables
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
    !> Performs a binary search for a 32-bit integer value in a sorted array.
    !>
    pure function binary_find_int32(value, array, start_idx, end_idx) result(idx)
        ! Arguments
        integer(int32), intent(in) :: value
        integer(int32), intent(in) :: array(:)
        integer(int32), intent(in), optional :: start_idx
        integer(int32), intent(in), optional :: end_idx
        ! Result
        integer(int32) :: idx
        ! Local variables
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
    !> Performs a binary search for a 64-bit integer value in a sorted array.
    !>
    pure function binary_find_int64(value, array, start_idx, end_idx) result(idx)
        ! Arguments
        integer(int64), intent(in) :: value
        integer(int64), intent(in) :: array(:)
        integer(int64), intent(in), optional :: start_idx
        integer(int64), intent(in), optional :: end_idx
        ! Result
        integer(int64) :: idx
        ! Local variables
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

module core_findings
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    public :: binary_find

    interface binary_find
        module procedure :: binary_find_int8
        module procedure :: binary_find_int16
        module procedure :: binary_find_int32
        module procedure :: binary_find_int64
    end interface

contains
    !------------------------------------------------------------
    ! 汎用二分探索: ソート済み整数配列から値を探索
    ! 引数:
    !   value   : 探したい値
    !   array   : ソート済み配列
    ! 戻り値:
    !   見つかった位置 (1-based)
    !   見つからなければ 0
    !------------------------------------------------------------
    function binary_find_int8(value, array) result(idx)
        implicit none
        integer(int8), intent(in) :: value
        integer(int8), intent(in) :: array(:)
        integer(int8) :: idx
        integer(int8) :: low, high, mid

        low = lbound(array, 1)
        high = ubound(array, 1)
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

    function binary_find_int16(value, array) result(idx)
        implicit none
        integer(int16), intent(in) :: value
        integer(int16), intent(in) :: array(:)
        integer(int16) :: idx
        integer(int16) :: low, high, mid

        low = lbound(array, 1)
        high = ubound(array, 1)
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

    function binary_find_int32(value, array) result(idx)
        implicit none
        integer(int32), intent(in) :: value
        integer(int32), intent(in) :: array(:)
        integer(int32) :: idx
        integer(int32) :: low, high, mid

        low = lbound(array, 1)
        high = ubound(array, 1)
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

    function binary_find_int64(value, array) result(idx)
        implicit none
        integer(int64), intent(in) :: value
        integer(int64), intent(in) :: array(:)
        integer(int64) :: idx
        integer(int64) :: low, high, mid

        low = lbound(array, 1)
        high = ubound(array, 1)
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
!------------------------------------------------------------
end module core_findings

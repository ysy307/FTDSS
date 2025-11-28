!>
!> Provides a generic binary search function for finding a value in a sorted
!> integer/real array.
!>
!> Algorithm: Lower Bound (finds the first occurrence or first insertion point).
!>
module core_findings
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128
    use :: stdlib_optval, only:optval
    implicit none
    private

    public :: binary_find

    !>
    !> A generic interface for performing a binary search.
    !>
    interface binary_find
        module procedure :: binary_find_int8
        module procedure :: binary_find_int16
        module procedure :: binary_find_int32
        module procedure :: binary_find_int64
        module procedure :: binary_find_real32
        module procedure :: binary_find_real64
        module procedure :: binary_find_real128
    end interface

contains

    ! ==========================================================================
    ! INTEGER IMPLEMENTATIONS (8, 16, 32, 64)
    ! ==========================================================================

    !> Finds an int8 value (Lower Bound).
    pure function binary_find_int8(value, array, start_index, end_index, &
                                   max_iter, return_insertion) result(idx)
        integer(int8), intent(in) :: value
        integer(int8), intent(in) :: array(:)
        integer(int32), intent(in), optional :: start_index, end_index, max_iter
        logical, intent(in), optional :: return_insertion
        integer(int32) :: idx

        integer(int32) :: low, high, mid, iter, limit
        logical :: mode_ins

        ! optval によるオプショナル引数の解決
        low = optval(start_index, lbound(array, 1))
        high = optval(end_index, ubound(array, 1))
        limit = optval(max_iter, huge(0))
        mode_ins = optval(return_insertion, .false.)

        iter = 0
        idx = 0 ! Default: Not found / Failure

        do while (low <= high)
            ! Safety Break: 上限に達したら失敗(0)としてリターン
            if (iter >= limit) return
            iter = iter + 1

            mid = low + (high - low) / 2

            ! Lower Bound Logic:
            ! 一致した場合(else側)も左側(high = mid - 1)を探索し続けることで、
            ! 重複時に最も左のインデックスに収束させる。
            if (array(mid) < value) then
                low = mid + 1
            else
                high = mid - 1
            end if
        end do

        if (mode_ins) then
            idx = low
        else
            if (low >= lbound(array, 1) .and. low <= ubound(array, 1)) then
                if (array(low) == value) idx = low
            end if
        end if
    end function binary_find_int8

    !> Finds an int16 value (Lower Bound).
    pure function binary_find_int16(value, array, start_index, end_index, &
                                    max_iter, return_insertion) result(idx)
        integer(int16), intent(in) :: value
        integer(int16), intent(in) :: array(:)
        integer(int32), intent(in), optional :: start_index, end_index, max_iter
        logical, intent(in), optional :: return_insertion
        integer(int32) :: idx

        integer(int32) :: low, high, mid, iter, limit
        logical :: mode_ins

        low = optval(start_index, lbound(array, 1))
        high = optval(end_index, ubound(array, 1))
        limit = optval(max_iter, huge(0))
        mode_ins = optval(return_insertion, .false.)

        iter = 0
        idx = 0

        do while (low <= high)
            if (iter >= limit) return
            iter = iter + 1
            mid = low + (high - low) / 2

            if (array(mid) < value) then
                low = mid + 1
            else
                high = mid - 1
            end if
        end do

        if (mode_ins) then
            idx = low
        else
            if (low >= lbound(array, 1) .and. low <= ubound(array, 1)) then
                if (array(low) == value) idx = low
            end if
        end if
    end function binary_find_int16

    !> Finds an int32 value (Lower Bound).
    pure function binary_find_int32(value, array, start_index, end_index, &
                                    max_iter, return_insertion) result(idx)
        integer(int32), intent(in) :: value
        integer(int32), intent(in) :: array(:)
        integer(int32), intent(in), optional :: start_index, end_index, max_iter
        logical, intent(in), optional :: return_insertion
        integer(int32) :: idx

        integer(int32) :: low, high, mid, iter, limit
        logical :: mode_ins

        low = optval(start_index, lbound(array, 1))
        high = optval(end_index, ubound(array, 1))
        limit = optval(max_iter, huge(0))
        mode_ins = optval(return_insertion, .false.)

        iter = 0
        idx = 0

        do while (low <= high)
            if (iter >= limit) return
            iter = iter + 1
            mid = low + (high - low) / 2

            if (array(mid) < value) then
                low = mid + 1
            else
                high = mid - 1
            end if
        end do

        if (mode_ins) then
            idx = low
        else
            if (low >= lbound(array, 1) .and. low <= ubound(array, 1)) then
                if (array(low) == value) idx = low
            end if
        end if
    end function binary_find_int32

    !> Finds an int64 value (Lower Bound).
    pure function binary_find_int64(value, array, start_index, end_index, &
                                    max_iter, return_insertion) result(idx)
        integer(int64), intent(in) :: value
        integer(int64), intent(in) :: array(:)
        integer(int32), intent(in), optional :: start_index, end_index, max_iter
        logical, intent(in), optional :: return_insertion
        integer(int32) :: idx

        integer(int32) :: low, high, mid, iter, limit
        logical :: mode_ins

        low = optval(start_index, lbound(array, 1))
        high = optval(end_index, ubound(array, 1))
        limit = optval(max_iter, huge(0))
        mode_ins = optval(return_insertion, .false.)

        iter = 0
        idx = 0

        do while (low <= high)
            if (iter >= limit) return
            iter = iter + 1
            mid = low + (high - low) / 2

            if (array(mid) < value) then
                low = mid + 1
            else
                high = mid - 1
            end if
        end do

        if (mode_ins) then
            idx = low
        else
            if (low >= lbound(array, 1) .and. low <= ubound(array, 1)) then
                if (array(low) == value) idx = low
            end if
        end if
    end function binary_find_int64

    ! ==========================================================================
    ! REAL IMPLEMENTATIONS (32, 64, 128)
    ! ==========================================================================

    !> Finds a real32 value (Lower Bound) with tolerance.
    pure function binary_find_real32(value, array, start_index, end_index, &
                                     tol, max_iter, return_insertion) result(idx)
        real(real32), intent(in) :: value
        real(real32), intent(in) :: array(:)
        integer(int32), intent(in), optional :: start_index, end_index, max_iter
        real(real32), intent(in), optional :: tol
        logical, intent(in), optional :: return_insertion
        integer(int32) :: idx

        integer(int32) :: low, high, mid, iter, limit
        real(real32) :: epsilon
        logical :: mode_ins

        low = optval(start_index, lbound(array, 1))
        high = optval(end_index, ubound(array, 1))
        epsilon = abs(optval(tol, 1.0e-6_real32))
        limit = optval(max_iter, huge(0))
        mode_ins = optval(return_insertion, .false.)

        iter = 0
        idx = 0

        do while (low <= high)
            if (iter >= limit) return
            iter = iter + 1
            mid = low + (high - low) / 2

            ! Lower Bound with Tolerance:
            ! "value - epsilon" より小さい場合のみ右へ。
            ! それ以外(近似的に等しい or 大きい)は左へ寄せる。
            if (array(mid) < value - epsilon) then
                low = mid + 1
            else
                high = mid - 1
            end if
        end do

        if (mode_ins) then
            idx = low
        else
            if (low >= lbound(array, 1) .and. low <= ubound(array, 1)) then
                if (abs(array(low) - value) <= epsilon) idx = low
            end if
        end if
    end function binary_find_real32

    !> Finds a real64 value (Lower Bound) with tolerance.
    pure function binary_find_real64(value, array, start_index, end_index, &
                                     tol, max_iter, return_insertion) result(idx)
        real(real64), intent(in) :: value
        real(real64), intent(in) :: array(:)
        integer(int32), intent(in), optional :: start_index, end_index, max_iter
        real(real64), intent(in), optional :: tol
        logical, intent(in), optional :: return_insertion
        integer(int32) :: idx

        integer(int32) :: low, high, mid, iter, limit
        real(real64) :: epsilon
        logical :: mode_ins

        low = optval(start_index, lbound(array, 1))
        high = optval(end_index, ubound(array, 1))
        epsilon = abs(optval(tol, 1.0e-12_real64))
        limit = optval(max_iter, huge(0))
        mode_ins = optval(return_insertion, .false.)

        iter = 0
        idx = 0

        do while (low <= high)
            if (iter >= limit) return
            iter = iter + 1
            mid = low + (high - low) / 2

            if (array(mid) < value - epsilon) then
                low = mid + 1
            else
                high = mid - 1
            end if
        end do

        if (mode_ins) then
            idx = low
        else
            if (low >= lbound(array, 1) .and. low <= ubound(array, 1)) then
                if (abs(array(low) - value) <= epsilon) idx = low
            end if
        end if
    end function binary_find_real64

    !> Finds a real128 value (Lower Bound) with tolerance.
    pure function binary_find_real128(value, array, start_index, end_index, &
                                      tol, max_iter, return_insertion) result(idx)
        real(real128), intent(in) :: value
        real(real128), intent(in) :: array(:)
        integer(int32), intent(in), optional :: start_index, end_index, max_iter
        real(real128), intent(in), optional :: tol
        logical, intent(in), optional :: return_insertion
        integer(int32) :: idx

        integer(int32) :: low, high, mid, iter, limit
        real(real128) :: epsilon
        logical :: mode_ins

        low = optval(start_index, lbound(array, 1))
        high = optval(end_index, ubound(array, 1))
        epsilon = abs(optval(tol, 1.0e-24_real128))
        limit = optval(max_iter, huge(0))
        mode_ins = optval(return_insertion, .false.)

        iter = 0
        idx = 0

        do while (low <= high)
            if (iter >= limit) return
            iter = iter + 1
            mid = low + (high - low) / 2

            if (array(mid) < value - epsilon) then
                low = mid + 1
            else
                high = mid - 1
            end if
        end do

        if (mode_ins) then
            idx = low
        else
            if (low >= lbound(array, 1) .and. low <= ubound(array, 1)) then
                if (abs(array(low) - value) <= epsilon) idx = low
            end if
        end if
    end function binary_find_real128

end module core_findings

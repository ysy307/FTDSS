!>
!> Provides a generic interface and implementations for finding the unique
!> elements in integer arrays of various kinds.
!>
module core_unique
    use, intrinsic :: iso_fortran_env
    use :: stdlib_sorting, only:sort
    use :: core_allocate, only:allocate_array
    implicit none
    private

    public :: unique

    interface unique
        module procedure :: unique_int8
        module procedure :: unique_int16
        module procedure :: unique_int32
        module procedure :: unique_int64
    end interface

contains

    !>
    !> Finds the unique elements in an 8-bit integer array and returns them in sorted order.
    !>
    subroutine unique_int8(array, unique_array)
        implicit none
        !> The input array of 8-bit integers.
        integer(int8), intent(in) :: array(:)
        !> An allocatable output array that will contain the sorted unique elements.
        integer(int8), intent(inout), allocatable :: unique_array(:)
        integer(int8), allocatable :: sorted_array(:)
        integer(int32) :: i, count

        if (size(array) == 0) then
            call Allocate_Array(unique_array, 0_int32)
            return
        end if

        ! Create a sorted copy of the input array
        sorted_array = array
        call sort(sorted_array)

        ! ==========================================================
        ! First pass: Count the number of unique elements
        ! ==========================================================
        count = 1 ! The first element is always unique
        do i = 2, size(sorted_array)
            if (sorted_array(i) /= sorted_array(i - 1)) count = count + 1
        end do

        ! ==========================================================
        ! Second pass: Populate the output array with unique elements
        ! ==========================================================
        call Allocate_Array(unique_array, count)
        unique_array(1) = sorted_array(1)
        count = 1
        do i = 2, size(sorted_array)
            if (sorted_array(i) /= sorted_array(i - 1)) then
                count = count + 1
                unique_array(count) = sorted_array(i)
            end if
        end do
    end subroutine unique_int8

    !>
    !> Finds the unique elements in a 16-bit integer array and returns them in sorted order.
    !>
    subroutine unique_int16(array, unique_array)
        implicit none
        !> The input array of 16-bit integers.
        integer(int16), intent(in) :: array(:)
        !> An allocatable output array that will contain the sorted unique elements.
        integer(int16), intent(inout), allocatable :: unique_array(:)
        integer(int16), allocatable :: sorted_array(:)
        integer(int32) :: i, count

        if (size(array) == 0) then
            call Allocate_Array(unique_array, 0_int32)
            return
        end if

        ! Create a sorted copy of the input array
        sorted_array = array
        call sort(sorted_array)

        ! First pass: Count the number of unique elements
        count = 1
        do i = 2, size(sorted_array)
            if (sorted_array(i) /= sorted_array(i - 1)) count = count + 1
        end do

        ! Second pass: Populate the output array with unique elements
        call Allocate_Array(unique_array, count)
        unique_array(1) = sorted_array(1)
        count = 1
        do i = 2, size(sorted_array)
            if (sorted_array(i) /= sorted_array(i - 1)) then
                count = count + 1
                unique_array(count) = sorted_array(i)
            end if
        end do
    end subroutine unique_int16

    !>
    !> Finds the unique elements in a 32-bit integer array and returns them in sorted order.
    !>
    subroutine unique_int32(array, unique_array)
        implicit none
        !> The input array of 32-bit integers.
        integer(int32), intent(in) :: array(:)
        !> An allocatable output array that will contain the sorted unique elements.
        integer(int32), intent(inout), allocatable :: unique_array(:)
        integer(int32), allocatable :: sorted_array(:)
        integer(int32) :: i, count

        if (size(array) == 0) then
            call Allocate_Array(unique_array, 0_int32)
            return
        end if

        ! Create a sorted copy of the input array
        sorted_array = array
        call sort(sorted_array)

        ! First pass: Count the number of unique elements
        count = 1
        do i = 2, size(sorted_array)
            if (sorted_array(i) /= sorted_array(i - 1)) count = count + 1
        end do

        ! Second pass: Populate the output array with unique elements
        call Allocate_Array(unique_array, count)
        unique_array(1) = sorted_array(1)
        count = 1
        do i = 2, size(sorted_array)
            if (sorted_array(i) /= sorted_array(i - 1)) then
                count = count + 1
                unique_array(count) = sorted_array(i)
            end if
        end do
    end subroutine unique_int32

    !>
    !> Finds the unique elements in a 64-bit integer array and returns them in sorted order.
    !>
    subroutine unique_int64(array, unique_array)
        implicit none
        !> The input array of 64-bit integers.
        integer(int64), intent(in) :: array(:)
        !> An allocatable output array that will contain the sorted unique elements.
        integer(int64), intent(inout), allocatable :: unique_array(:)
        integer(int64), allocatable :: sorted_array(:)
        integer(int64) :: i, count

        if (size(array) == 0) then
            call Allocate_Array(unique_array, 0_int64)
            return
        end if

        ! Create a sorted copy of the input array
        sorted_array = array
        call sort(sorted_array)

        ! First pass: Count the number of unique elements
        count = 1
        do i = 2, size(sorted_array)
            if (sorted_array(i) /= sorted_array(i - 1)) count = count + 1
        end do

        ! Second pass: Populate the output array with unique elements
        call Allocate_Array(unique_array, count)
        unique_array(1) = sorted_array(1)
        count = 1
        do i = 2, size(sorted_array)
            if (sorted_array(i) /= sorted_array(i - 1)) then
                count = count + 1
                unique_array(count) = sorted_array(i)
            end if
        end do
    end subroutine unique_int64

end module core_unique

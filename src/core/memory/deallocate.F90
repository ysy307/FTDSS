!>
!> Provides a generic interface and safe wrappers for deallocating allocatable arrays.
!>
module core_deallocate
    use, intrinsic :: iso_fortran_env
    use :: core_error, only:error_message
    implicit none
    private

    public :: deallocate_array

    interface deallocate_array
        module procedure :: deallocate_rank1_int8
        module procedure :: deallocate_rank1_int32
        module procedure :: deallocate_rank1_int64
        module procedure :: deallocate_rank1_real32
        module procedure :: deallocate_rank1_real64
        module procedure :: deallocate_rank1_real128
        module procedure :: deallocate_rank1_logical1
        module procedure :: deallocate_rank1_logical4
        module procedure :: deallocate_rank1_logical8
        module procedure :: deallocate_rank2_int8
        module procedure :: deallocate_rank2_int32
        module procedure :: deallocate_rank2_int64
        module procedure :: deallocate_rank2_real32
        module procedure :: deallocate_rank2_real64
        module procedure :: deallocate_rank2_real128
        module procedure :: deallocate_rank2_logical1
        module procedure :: deallocate_rank2_logical4
        module procedure :: deallocate_rank2_logical8
        module procedure :: deallocate_rank3_int8
        module procedure :: deallocate_rank3_int16
        module procedure :: deallocate_rank3_int32
        module procedure :: deallocate_rank3_int64
        module procedure :: deallocate_rank3_real32
        module procedure :: deallocate_rank3_real64
        module procedure :: deallocate_rank3_real128
        module procedure :: deallocate_rank3_logical1
        module procedure :: deallocate_rank3_logical4
        module procedure :: deallocate_rank3_logical8
    end interface

contains

    !>
    !> Safely deallocates a rank-1 8-bit integer array.
    !> If the array is not allocated, the routine does nothing.
    !>
    subroutine deallocate_rank1_int8(array)
        implicit none
        !> The allocatable array to be deallocated.
        integer(int8), allocatable, intent(inout) :: array(:)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(971)
        end if

    end subroutine deallocate_rank1_int8

    !>
    !> Safely deallocates a rank-1 32-bit integer array.
    !> If the array is not allocated, the routine does nothing.
    !>
    subroutine deallocate_rank1_int32(array)
        implicit none
        !> The allocatable array to be deallocated.
        integer(int32), allocatable, intent(inout) :: array(:)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if

    end subroutine deallocate_rank1_int32

    !>
    !> Safely deallocates a rank-1 64-bit integer array.
    !> If the array is not allocated, the routine does nothing.
    !>
    subroutine deallocate_rank1_int64(array)
        implicit none
        !> The allocatable array to be deallocated.
        integer(int64), allocatable, intent(inout) :: array(:)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if

    end subroutine deallocate_rank1_int64

    !>
    !> Safely deallocates a rank-1 single precision real array.
    !> If the array is not allocated, the routine does nothing.
    !>
    subroutine deallocate_rank1_real32(array)
        implicit none
        !> The allocatable array to be deallocated.
        real(real32), allocatable, intent(inout) :: array(:)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if

    end subroutine deallocate_rank1_real32

    !>
    !> Safely deallocates a rank-1 double precision real array.
    !> If the array is not allocated, the routine does nothing.
    !>
    subroutine deallocate_rank1_real64(array)
        implicit none
        !> The allocatable array to be deallocated.
        real(real64), allocatable, intent(inout) :: array(:)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if

    end subroutine deallocate_rank1_real64

    !>
    !> Safely deallocates a rank-1 quad precision real array.
    !> If the array is not allocated, the routine does nothing.
    !>
    subroutine deallocate_rank1_real128(array)
        implicit none
        !> The allocatable array to be deallocated.
        real(real128), allocatable, intent(inout) :: array(:)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if

    end subroutine deallocate_rank1_real128

    !>
    !> Safely deallocates a rank-1 logical (kind=8) array.
    !> If the array is not allocated, the routine does nothing.
    !>
    subroutine deallocate_rank1_logical1(array)
        implicit none
        !> The allocatable array to be deallocated.
        logical(logical8), allocatable, intent(inout) :: array(:)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if

    end subroutine deallocate_rank1_logical1

    !>
    !> Safely deallocates a rank-1 logical (kind=32) array.
    !> If the array is not allocated, the routine does nothing.
    !>
    subroutine deallocate_rank1_logical4(array)
        implicit none
        !> The allocatable array to be deallocated.
        logical(logical32), allocatable, intent(inout) :: array(:)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if

    end subroutine deallocate_rank1_logical4

    !>
    !> Safely deallocates a rank-1 logical (kind=64) array.
    !> If the array is not allocated, the routine does nothing.
    !>
    subroutine deallocate_rank1_logical8(array)
        implicit none
        !> The allocatable array to be deallocated.
        logical(logical64), allocatable, intent(inout) :: array(:)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if

    end subroutine deallocate_rank1_logical8

    !>
    !> Safely deallocates a rank-2 8-bit integer array.
    !> If the array is not allocated, the routine does nothing.
    !>
    subroutine deallocate_rank2_int8(array)
        implicit none
        !> The allocatable array to be deallocated.
        integer(int8), allocatable, intent(inout) :: array(:, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank2_int8

    !>
    !> Safely deallocates a rank-2 32-bit integer array.
    !> If the array is not allocated, the routine does nothing.
    !>
    subroutine deallocate_rank2_int32(array)
        implicit none
        !> The allocatable array to be deallocated.
        integer(int32), allocatable, intent(inout) :: array(:, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank2_int32

    !>
    !> Safely deallocates a rank-2 64-bit integer array.
    !> If the array is not allocated, the routine does nothing.
    !>
    subroutine deallocate_rank2_int64(array)
        implicit none
        !> The allocatable array to be deallocated.
        integer(int64), allocatable, intent(inout) :: array(:, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank2_int64

    !>
    !> Safely deallocates a rank-2 single precision real array.
    !> If the array is not allocated, the routine does nothing.
    !>
    subroutine deallocate_rank2_real32(array)
        implicit none
        !> The allocatable array to be deallocated.
        real(real32), allocatable, intent(inout) :: array(:, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank2_real32

    !>
    !> Safely deallocates a rank-2 double precision real array.
    !> If the array is not allocated, the routine does nothing.
    !>
    subroutine deallocate_rank2_real64(array)
        implicit none
        !> The allocatable array to be deallocated.
        real(real64), allocatable, intent(inout) :: array(:, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank2_real64

    !>
    !> Safely deallocates a rank-2 quad precision real array.
    !> If the array is not allocated, the routine does nothing.
    !>
    subroutine deallocate_rank2_real128(array)
        implicit none
        !> The allocatable array to be deallocated.
        real(real128), allocatable, intent(inout) :: array(:, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank2_real128

    !>
    !> Safely deallocates a rank-2 logical (kind=8) array.
    !> If the array is not allocated, the routine does nothing.
    !>
    subroutine deallocate_rank2_logical1(array)
        implicit none
        !> The allocatable array to be deallocated.
        logical(logical8), allocatable, intent(inout) :: array(:, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank2_logical1

    !>
    !> Safely deallocates a rank-2 logical (kind=32) array.
    !> If the array is not allocated, the routine does nothing.
    !>
    subroutine deallocate_rank2_logical4(array)
        implicit none
        !> The allocatable array to be deallocated.
        logical(logical32), allocatable, intent(inout) :: array(:, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank2_logical4

    !>
    !> Safely deallocates a rank-2 logical (kind=64) array.
    !> If the array is not allocated, the routine does nothing.
    !>
    subroutine deallocate_rank2_logical8(array)
        implicit none
        !> The allocatable array to be deallocated.
        logical(logical64), allocatable, intent(inout) :: array(:, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank2_logical8

    !>
    !> Safely deallocates a rank-3 integer array.
    !> If the array is not allocated, the routine does nothing.
    !>

    subroutine deallocate_rank3_int8(array)
        implicit none
        !> The allocatable array to be deallocated.
        integer(int8), allocatable, intent(inout) :: array(:, :, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank3_int8

    subroutine deallocate_rank3_int16(array)
        implicit none
        !> The allocatable array to be deallocated.
        integer(int16), allocatable, intent(inout) :: array(:, :, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank3_int16

    subroutine deallocate_rank3_int32(array)
        implicit none
        !> The allocatable array to be deallocated.
        integer(int32), allocatable, intent(inout) :: array(:, :, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank3_int32

    subroutine deallocate_rank3_int64(array)
        implicit none
        !> The allocatable array to be deallocated.
        integer(int64), allocatable, intent(inout) :: array(:, :, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank3_int64

    subroutine deallocate_rank3_real32(array)
        implicit none
        !> The allocatable array to be deallocated.
        real(real32), allocatable, intent(inout) :: array(:, :, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank3_real32

    subroutine deallocate_rank3_real64(array)
        implicit none
        !> The allocatable array to be deallocated.
        real(real64), allocatable, intent(inout) :: array(:, :, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank3_real64

    subroutine deallocate_rank3_real128(array)
        implicit none
        !> The allocatable array to be deallocated.
        real(real128), allocatable, intent(inout) :: array(:, :, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank3_real128

    subroutine deallocate_rank3_logical1(array)
        implicit none
        !> The allocatable array to be deallocated.
        logical(logical8), allocatable, intent(inout) :: array(:, :, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank3_logical1

    subroutine deallocate_rank3_logical4(array)
        implicit none
        !> The allocatable array to be deallocated.
        logical(logical32), allocatable, intent(inout) :: array(:, :, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank3_logical4

    subroutine deallocate_rank3_logical8(array)
        implicit none
        !> The allocatable array to be deallocated.
        logical(logical64), allocatable, intent(inout) :: array(:, :, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank3_logical8

end module core_deallocate

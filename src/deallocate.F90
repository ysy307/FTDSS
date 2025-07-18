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
    end interface

contains

    subroutine deallocate_rank1_int8(array)
        implicit none
        integer(int8), allocatable, intent(inout) :: array(:)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if

    end subroutine deallocate_rank1_int8

    subroutine deallocate_rank1_int32(array)
        implicit none
        integer(int32), allocatable, intent(inout) :: array(:)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if

    end subroutine deallocate_rank1_int32

    subroutine deallocate_rank1_int64(array)
        implicit none
        integer(int64), allocatable, intent(inout) :: array(:)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if

    end subroutine deallocate_rank1_int64

    subroutine deallocate_rank1_real32(array)
        implicit none
        real(real32), allocatable, intent(inout) :: array(:)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if

    end subroutine deallocate_rank1_real32

    subroutine deallocate_rank1_real64(array)
        implicit none
        real(real64), allocatable, intent(inout) :: array(:)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if

    end subroutine deallocate_rank1_real64

    subroutine deallocate_rank1_real128(array)
        implicit none
        real(real128), allocatable, intent(inout) :: array(:)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if

    end subroutine deallocate_rank1_real128

    subroutine deallocate_rank1_logical1(array)
        implicit none
        logical(logical8), allocatable, intent(inout) :: array(:)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if

    end subroutine deallocate_rank1_logical1

    subroutine deallocate_rank1_logical4(array)
        implicit none
        logical(logical32), allocatable, intent(inout) :: array(:)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if

    end subroutine deallocate_rank1_logical4

    subroutine deallocate_rank1_logical8(array)
        implicit none
        logical(logical64), allocatable, intent(inout) :: array(:)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if

    end subroutine deallocate_rank1_logical8

    subroutine deallocate_rank2_int8(array)
        implicit none
        integer(int8), allocatable, intent(inout) :: array(:, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank2_int8

    subroutine deallocate_rank2_int32(array)
        implicit none
        integer(int32), allocatable, intent(inout) :: array(:, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank2_int32

    subroutine deallocate_rank2_int64(array)
        implicit none
        integer(int64), allocatable, intent(inout) :: array(:, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank2_int64

    subroutine deallocate_rank2_real32(array)
        implicit none
        real(real32), allocatable, intent(inout) :: array(:, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank2_real32

    subroutine deallocate_rank2_real64(array)
        implicit none
        real(real64), allocatable, intent(inout) :: array(:, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank2_real64

    subroutine deallocate_rank2_real128(array)
        implicit none
        real(real128), allocatable, intent(inout) :: array(:, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank2_real128

    subroutine deallocate_rank2_logical1(array)
        implicit none
        logical(logical8), allocatable, intent(inout) :: array(:, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank2_logical1

    subroutine deallocate_rank2_logical4(array)
        implicit none
        logical(logical32), allocatable, intent(inout) :: array(:, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank2_logical4

    subroutine deallocate_rank2_logical8(array)
        implicit none
        logical(logical64), allocatable, intent(inout) :: array(:, :)
        integer(int32) :: stat

        if (allocated(array)) then
            deallocate (array, stat=stat)
            if (stat /= 0) call error_message(961)
        end if
    end subroutine deallocate_rank2_logical8

end module core_deallocate

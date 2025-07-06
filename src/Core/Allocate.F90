module core_allocate
    use, intrinsic :: iso_fortran_env
    use :: core_error, only:error_message
    implicit none
    private

    public :: allocate_array
    public :: allocate_pointer

    interface allocate_array
        procedure :: allocate_rank1_int8
        procedure :: allocate_rank1_int16
        procedure :: allocate_rank1_int32
        procedure :: allocate_rank1_int64
        procedure :: allocate_rank1_real32
        procedure :: allocate_rank1_real64
        procedure :: allocate_rank1_real128
        procedure :: allocate_rank1_logical1
        procedure :: allocate_rank1_logical4
        procedure :: allocate_rank1_logical8
        procedure :: allocate_rank1_int32_specify
        procedure :: allocate_rank1_int64_specify
        procedure :: allocate_rank1_real32_specify
        procedure :: allocate_rank1_real64_specify
        procedure :: allocate_rank1_real128_specify
        procedure :: allocate_rank1_logical1_specify
        procedure :: allocate_rank1_logical4_specify
        procedure :: allocate_rank1_logical8_specify
        procedure :: allocate_rank2_int8
        procedure :: allocate_rank2_int16
        procedure :: allocate_rank2_int32
        procedure :: allocate_rank2_int64
        procedure :: allocate_rank2_real32
        procedure :: allocate_rank2_real64
        procedure :: allocate_rank2_real128
        procedure :: allocate_rank2_logical1
        procedure :: allocate_rank2_logical4
        procedure :: allocate_rank2_logical8
        procedure :: allocate_rank1_int32_pointer
        procedure :: allocate_rank1_int64_pointer
        procedure :: allocate_rank1_real32_pointer
        procedure :: allocate_rank1_real64_pointer
        procedure :: allocate_rank1_real128_pointer
        procedure :: allocate_rank1_logical_pointer
        procedure :: allocate_rank1_int32_specify_pointer
        procedure :: allocate_rank1_int64_specify_pointer
        procedure :: allocate_rank1_real32_specify_pointer
        procedure :: allocate_rank1_real64_specify_pointer
        procedure :: allocate_rank1_real128_specify_pointer
        procedure :: allocate_rank1_logical_specify_pointer
    end interface

    interface allocate_pointer
        procedure :: allocate_pointer_int32
        procedure :: allocate_pointer_int64
        procedure :: allocate_pointer_real32
        procedure :: allocate_pointer_real64
        procedure :: allocate_pointer_real128
    end interface

contains

    ! rank-1 配列の割り当て
    subroutine allocate_rank1_int8(array, size)
        implicit none
        integer(int8), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: size
        integer(int32) :: stat

        ! Check if the array is already allocated
        if (allocated(array)) call error_message(951)

        ! Check for invalid size
        if (size <= 0) call error_message(952)
        ! if (size > huge(array)) call error_message(953)

        allocate (array(size), stat=stat)

        ! Check if allocation was successful
        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_int8

    subroutine allocate_rank1_int16(array, size)
        implicit none
        integer(int16), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: size
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (size <= 0) call error_message(952)
        ! if (size > huge(array)) call error_message(953)

        allocate (array(size), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_int16

    subroutine allocate_rank1_int32(array, size)
        implicit none
        integer(int32), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: size
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (size <= 0) call error_message(952)
        ! if (size > huge(array)) call error_message(953)

        allocate (array(size), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_int32

    subroutine allocate_rank1_int64(array, size)
        implicit none
        integer(int64), intent(inout), allocatable :: array(:)
        integer(int64), intent(in) :: size
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (size <= 0) call error_message(952)
        ! if (size > huge(array)) call error_message(953)

        allocate (array(size), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_int64

    subroutine allocate_rank1_real32(array, size)
        implicit none
        real(real32), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: size
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (size <= 0) call error_message(952)
        ! if (size > huge(size)) call error_message(953)

        allocate (array(size), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_real32

    subroutine allocate_rank1_real64(array, size)
        implicit none
        real(real64), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: size
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (size <= 0) call error_message(952)
        ! if (size > huge(size)) call error_message(953)

        allocate (array(size), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_real64

    subroutine allocate_rank1_real128(array, size)
        implicit none
        real(real128), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: size
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (size <= 0) call error_message(952)
        ! if (size > huge(size)) call error_message(953)

        allocate (array(size), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_real128

    subroutine allocate_rank1_logical1(array, size)
        implicit none
        logical(logical8), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: size
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (size <= 0) call error_message(952)
        ! if (size > huge(size)) call error_message(953)

        allocate (array(size), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_logical1

    subroutine allocate_rank1_logical4(array, size)
        implicit none
        logical(logical32), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: size
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (size <= 0) call error_message(952)
        ! if (size > huge(size)) call error_message(953)

        allocate (array(size), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_logical4

    subroutine allocate_rank1_logical8(array, size)
        implicit none
        logical(logical64), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: size
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (size <= 0) call error_message(952)
        ! if (size > huge(size)) call error_message(953)

        allocate (array(size), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_logical8

    subroutine allocate_rank2_int8(array, nrow, ncol)
        implicit none
        integer(int8), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: nrow, ncol
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (nrow <= 0 .or. ncol <= 0) call error_message(952)
        ! if (nrow * ncol > huge(ncol)) call error_message(953)

        allocate (array(nrow, ncol), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank2_int8

    subroutine allocate_rank2_int16(array, nrow, ncol)
        implicit none
        integer(int16), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: nrow, ncol
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (nrow <= 0 .or. ncol <= 0) call error_message(952)
        ! if (nrow * ncol > huge(ncol)) call error_message(953)

        allocate (array(nrow, ncol), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank2_int16

    subroutine allocate_rank2_int32(array, nrow, ncol)
        implicit none
        integer(int32), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: nrow, ncol
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (nrow <= 0 .or. ncol <= 0) call error_message(952)
        ! if (nrow * ncol > huge(ncol)) call error_message(953)

        allocate (array(nrow, ncol), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank2_int32

    subroutine allocate_rank2_int64(array, nrow, ncol)
        implicit none
        integer(int64), intent(inout), allocatable :: array(:, :)
        integer(int64), intent(in) :: nrow, ncol
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (nrow <= 0 .or. ncol <= 0) call error_message(952)
        ! if (nrow * ncol > huge(ncol)) call error_message(953)

        allocate (array(nrow, ncol), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank2_int64

    subroutine allocate_rank2_real32(array, nrow, ncol)
        implicit none
        real(real32), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: nrow, ncol
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (nrow <= 0 .or. ncol <= 0) call error_message(952)
        ! if (nrow * ncol > huge(ncol)) call error_message(953)

        allocate (array(nrow, ncol), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank2_real32

    subroutine allocate_rank2_real64(array, nrow, ncol)
        implicit none
        real(real64), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: nrow, ncol
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (nrow <= 0 .or. ncol <= 0) call error_message(952)
        ! if (nrow * ncol > huge(ncol)) call error_message(953)

        allocate (array(nrow, ncol), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank2_real64

    subroutine allocate_rank2_real128(array, nrow, ncol)
        implicit none
        real(real128), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: nrow, ncol
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (nrow <= 0 .or. ncol <= 0) call error_message(952)
        ! if (nrow * ncol > huge(ncol)) call error_message(953)

        allocate (array(nrow, ncol), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank2_real128

    subroutine allocate_rank2_logical1(array, nrow, ncol)
        implicit none
        logical(logical8), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: nrow, ncol
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (nrow <= 0 .or. ncol <= 0) call error_message(952)
        ! if (nrow * ncol > huge(ncol)) call error_message(953)

        allocate (array(nrow, ncol), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank2_logical1

    subroutine allocate_rank2_logical4(array, nrow, ncol)
        implicit none
        logical(logical32), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: nrow, ncol
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (nrow <= 0 .or. ncol <= 0) call error_message(952)
        ! if (nrow * ncol > huge(ncol)) call error_message(953)

        allocate (array(nrow, ncol), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank2_logical4

    subroutine allocate_rank2_logical8(array, nrow, ncol)
        implicit none
        logical(logical64), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: nrow, ncol
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (nrow <= 0 .or. ncol <= 0) call error_message(952)
        ! if (nrow * ncol > huge(ncol)) call error_message(953)

        allocate (array(nrow, ncol), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank2_logical8

    subroutine allocate_rank1_int32_specify(array, first, last)
        implicit none
        integer(int32), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: first, last
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (first > last) call error_message(954)
        ! if (last - first > huge(array)) call error_message(953)

        allocate (array(first:last), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_int32_specify

    subroutine allocate_rank1_int64_specify(array, first, last)
        implicit none
        integer(int64), intent(inout), allocatable :: array(:)
        integer(int64), intent(in) :: first, last
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (first > last) call error_message(954)
        ! if (last - first > huge(array)) call error_message(953)

        allocate (array(first:last), stat=stat)

        if (stat /= 0) call error_message(955)
    end subroutine allocate_rank1_int64_specify

    subroutine allocate_rank1_real32_specify(array, first, last)
        implicit none
        real(real32), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: first, last
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (first > last) call error_message(954)
        ! if (last - first > huge(first)) call error_message(953)

        allocate (array(first:last), stat=stat)

        if (stat /= 0) call error_message(955)
    end subroutine allocate_rank1_real32_specify

    subroutine allocate_rank1_real64_specify(array, first, last)
        implicit none
        real(real64), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: first, last
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (first > last) call error_message(954)
        ! if (last - first > huge(first)) call error_message(953)

        allocate (array(first:last), stat=stat)

        if (stat /= 0) call error_message(955)
    end subroutine allocate_rank1_real64_specify

    subroutine allocate_rank1_real128_specify(array, first, last)
        implicit none
        real(real128), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: first, last
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (first > last) call error_message(954)
        ! if (last - first > huge(first)) call error_message(953)

        allocate (array(first:last), stat=stat)

        if (stat /= 0) call error_message(955)
    end subroutine allocate_rank1_real128_specify

    subroutine allocate_rank1_logical1_specify(array, first, last)
        implicit none
        logical(logical8), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: first, last
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (first > last) call error_message(954)
        ! if (last - first > huge(first)) call error_message(953)

        allocate (array(first:last), stat=stat)

        if (stat /= 0) call error_message(955)
    end subroutine allocate_rank1_logical1_specify

    subroutine allocate_rank1_logical4_specify(array, first, last)
        implicit none
        logical(logical32), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: first, last
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (first > last) call error_message(954)
        ! if (last - first > huge(first)) call error_message(953)

        allocate (array(first:last), stat=stat)

        if (stat /= 0) call error_message(955)
    end subroutine allocate_rank1_logical4_specify

    subroutine allocate_rank1_logical8_specify(array, first, last)
        implicit none
        logical(logical64), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: first, last
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (first > last) call error_message(954)
        ! if (last - first > huge(first)) call error_message(953)

        allocate (array(first:last), stat=stat)

        if (stat /= 0) call error_message(955)
    end subroutine allocate_rank1_logical8_specify

    subroutine allocate_rank1_int32_pointer(array, size)
        implicit none
        integer(int32), intent(inout), dimension(:), pointer :: array
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. associated(array)) then
            allocate (array(size))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_int32_pointer

    subroutine allocate_rank1_int64_pointer(array, size)
        implicit none
        integer(int64), intent(inout), dimension(:), pointer :: array
        integer(int64), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. associated(array)) then
            allocate (array(size))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_int64_pointer

    subroutine allocate_rank1_real32_pointer(array, size)
        implicit none
        real(real32), intent(inout), dimension(:), pointer :: array
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. associated(array)) then
            allocate (array(size))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_real32_pointer

    subroutine allocate_rank1_real64_pointer(array, size)
        implicit none
        real(real64), intent(inout), dimension(:), pointer :: array
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. associated(array)) then
            allocate (array(size))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_real64_pointer

    subroutine allocate_rank1_real128_pointer(array, size)
        implicit none
        real(real128), intent(inout), dimension(:), pointer :: array
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. associated(array)) then
            allocate (array(size))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_real128_pointer

    subroutine allocate_rank1_logical_pointer(array, size)
        implicit none
        logical, intent(inout), dimension(:), pointer :: array
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. associated(array)) then
            allocate (array(size))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_logical_pointer

    subroutine allocate_rank1_int32_specify_pointer(array, first, last)
        implicit none
        integer(int32), intent(inout), dimension(:), pointer :: array
        integer(int32), intent(in) :: first, last

        if (first > last) call error_message(952)
        if (.not. associated(array)) then
            allocate (array(first:last))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_int32_specify_pointer

    subroutine allocate_rank1_int64_specify_pointer(array, first, last)
        implicit none
        integer(int64), intent(inout), dimension(:), pointer :: array
        integer(int64), intent(in) :: first, last

        if (first > last) call error_message(952)
        if (.not. associated(array)) then
            allocate (array(first:last))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_int64_specify_pointer

    subroutine allocate_rank1_real32_specify_pointer(array, first, last)
        implicit none
        real(real32), intent(inout), dimension(:), pointer :: array
        integer(int32), intent(in) :: first, last

        if (first > last) call error_message(952)
        if (.not. associated(array)) then
            allocate (array(first:last))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_real32_specify_pointer

    subroutine allocate_rank1_real64_specify_pointer(array, first, last)
        implicit none
        real(real64), intent(inout), dimension(:), pointer :: array
        integer(int32), intent(in) :: first, last

        if (first > last) call error_message(952)
        if (.not. associated(array)) then
            allocate (array(first:last))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_real64_specify_pointer

    subroutine allocate_rank1_real128_specify_pointer(array, first, last)
        implicit none
        real(real128), intent(inout), dimension(:), pointer :: array
        integer(int32), intent(in) :: first, last

        if (first > last) call error_message(952)
        if (.not. associated(array)) then
            allocate (array(first:last))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_real128_specify_pointer

    subroutine allocate_rank1_logical_specify_pointer(array, first, last)
        implicit none
        logical(4), intent(inout), dimension(:), pointer :: array
        integer(int32), intent(in) :: first, last

        if (first > last) call error_message(952)
        if (.not. associated(array)) then
            allocate (array(first:last))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_logical_specify_pointer

    ! ポインタ用の割り当て
    subroutine allocate_pointer_int32(iptr)
        implicit none
        integer(int32), pointer :: iptr

        if (.not. associated(iptr)) then
            allocate (iptr)
        else
            call error_message(955)
        end if
    end subroutine allocate_pointer_int32

    subroutine allocate_pointer_int64(iptr)
        implicit none
        integer(int64), pointer :: iptr

        if (.not. associated(iptr)) then
            allocate (iptr)
        else
            call error_message(955)
        end if
    end subroutine allocate_pointer_int64

    subroutine allocate_pointer_real32(dptr)
        implicit none
        real(real32), pointer :: dptr

        if (.not. associated(dptr)) then
            allocate (dptr)
        else
            call error_message(955)
        end if
    end subroutine allocate_pointer_real32

    subroutine allocate_pointer_real64(dptr)
        implicit none
        real(real64), pointer :: dptr

        if (.not. associated(dptr)) then
            allocate (dptr)
        else
            call error_message(955)
        end if
    end subroutine allocate_pointer_real64

    subroutine allocate_pointer_real128(dptr)
        implicit none
        real(real128), pointer :: dptr

        if (.not. associated(dptr)) then
            allocate (dptr)
        else
            call error_message(955)
        end if
    end subroutine allocate_pointer_real128
end module core_allocate

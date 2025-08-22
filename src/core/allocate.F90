module core_allocate
    use, intrinsic :: iso_fortran_env
    use :: core_error, only:error_message
    implicit none
    private

    public :: allocate_array
    public :: allocate_pointer

    interface allocate_array
        ! Rank-1 arrays
        module procedure :: allocate_rank1_int8
        module procedure :: allocate_rank1_int16
        module procedure :: allocate_rank1_int32
        module procedure :: allocate_rank1_int64
        module procedure :: allocate_rank1_real32
        module procedure :: allocate_rank1_real64
        module procedure :: allocate_rank1_real128
        module procedure :: allocate_rank1_logical1
        module procedure :: allocate_rank1_logical4
        module procedure :: allocate_rank1_logical8
        ! Rank-2 arrays
        module procedure :: allocate_rank2_int8
        module procedure :: allocate_rank2_int16
        module procedure :: allocate_rank2_int32
        module procedure :: allocate_rank2_int64
        module procedure :: allocate_rank2_real32
        module procedure :: allocate_rank2_real64
        module procedure :: allocate_rank2_real128
        module procedure :: allocate_rank2_logical1
        module procedure :: allocate_rank2_logical4
        module procedure :: allocate_rank2_logical8
    end interface

    interface allocate_pointer
        ! Scalar pointers
        module procedure :: allocate_pointer_int32
        module procedure :: allocate_pointer_int64
        module procedure :: allocate_pointer_real32
        module procedure :: allocate_pointer_real64
        module procedure :: allocate_pointer_real128

        ! Array pointers
        module procedure :: allocate_rank1_int32_pointer
        module procedure :: allocate_rank1_int64_pointer
        module procedure :: allocate_rank1_real32_pointer
        module procedure :: allocate_rank1_real64_pointer
        module procedure :: allocate_rank1_real128_pointer
        module procedure :: allocate_rank1_logical_pointer
        module procedure :: allocate_rank1_int32_specify_pointer
        module procedure :: allocate_rank1_int64_specify_pointer
        module procedure :: allocate_rank1_real32_specify_pointer
        module procedure :: allocate_rank1_real64_specify_pointer
        module procedure :: allocate_rank1_real128_specify_pointer
        module procedure :: allocate_rank1_logical_specify_pointer
    end interface allocate_pointer
contains

    ! rank-1 配列の割り当て
    subroutine allocate_rank1_int8(array, length, bounds)
        implicit none
        integer(int8), intent(inout), allocatable :: array(:)
        integer(int32), intent(in), optional :: length
        integer(int32), intent(in), optional :: bounds(:)
        integer(int32) :: stat
        logical :: length_present, bounds_present
        integer(int64) :: requested_size
        integer(int32) :: first, last

        length_present = present(length)
        bounds_present = present(bounds)

        ! --- Argument validation ---
#ifdef USE_DEBUG
        if (length_present .and. bounds_present) call error_message(956)
        if (.not. length_present .and. .not. bounds_present) call error_message(957)
        if (bounds_present) then
            if (size(bounds) /= 2) call error_message(958)
        end if
#endif

        ! --- Main logic ---
        if (allocated(array)) call error_message(951)

        if (length_present) then
            ! Allocate by length
#ifdef USE_DEBUG
            if (length <= 0) call error_message(952)
            if (int(length, kind=int64) > huge(0_int32)) call error_message(953)
#endif
            allocate (array(length), stat=stat)
        else if (bounds_present) then
            ! Allocate by bounds
            first = bounds(1)
            last = bounds(2)
#ifdef USE_DEBUG
            if (first > last) call error_message(954)

            requested_size = int(last, kind=int64) - int(first, kind=int64) + 1_int64
            if (requested_size > huge(0_int64) / 4) call error_message(953)
#endif
            allocate (array(first:last), stat=stat)
        end if

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_int8

    subroutine allocate_rank1_int16(array, length, bounds)
        implicit none
        integer(int16), intent(inout), allocatable :: array(:)
        integer(int32), intent(in), optional :: length
        integer(int32), intent(in), optional :: bounds(:)
        integer(int32) :: stat
        logical :: length_present, bounds_present
        integer(int64) :: requested_size
        integer(int32) :: first, last

        length_present = present(length)
        bounds_present = present(bounds)

        ! --- Argument validation ---
#ifdef USE_DEBUG
        if (length_present .and. bounds_present) call error_message(956)
        if (.not. length_present .and. .not. bounds_present) call error_message(957)
        if (bounds_present) then
            if (size(bounds) /= 2) call error_message(958)
        end if
#endif

        ! --- Main logic ---
        if (allocated(array)) call error_message(951)

        if (length_present) then
            ! Allocate by length
#ifdef USE_DEBUG
            if (length <= 0) call error_message(952)
            if (int(length, kind=int64) > huge(0_int32)) call error_message(953)
#endif
            allocate (array(length), stat=stat)
        else if (bounds_present) then
            ! Allocate by bounds
            first = bounds(1)
            last = bounds(2)
#ifdef USE_DEBUG
            if (first > last) call error_message(954)

            requested_size = int(last, kind=int64) - int(first, kind=int64) + 1_int64
            if (requested_size > huge(0_int64) / 4) call error_message(953)
#endif
            allocate (array(first:last), stat=stat)
        end if

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_int16

    subroutine allocate_rank1_int32(array, length, bounds)
        implicit none
        integer(int32), intent(inout), allocatable :: array(:)
        integer(int32), intent(in), optional :: length
        integer(int32), intent(in), optional :: bounds(:)
        integer(int32) :: stat
        logical :: length_present, bounds_present
        integer(int64) :: requested_size
        integer(int32) :: first, last

        length_present = present(length)
        bounds_present = present(bounds)

        ! --- Argument validation ---
#ifdef USE_DEBUG
        if (length_present .and. bounds_present) call error_message(956)
        if (.not. length_present .and. .not. bounds_present) call error_message(957)
        if (bounds_present) then
            if (size(bounds) /= 2) call error_message(958)
        end if
#endif

        ! --- Main logic ---
        if (allocated(array)) call error_message(951)

        if (length_present) then
            ! Allocate by length
#ifdef USE_DEBUG
            if (length <= 0) call error_message(952)
            if (int(length, kind=int64) > huge(0_int32)) call error_message(953)
#endif
            allocate (array(length), stat=stat)
        else if (bounds_present) then
            ! Allocate by bounds
            first = bounds(1)
            last = bounds(2)

#ifdef USE_DEBUG
            if (first > last) call error_message(954)

            requested_size = int(last, kind=int64) - int(first, kind=int64) + 1_int64
            if (requested_size > huge(0_int64) / 4) call error_message(953)
#endif
            allocate (array(first:last), stat=stat)
        end if

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_int32

    subroutine allocate_rank1_int64(array, length, bounds)
        implicit none
        integer(int64), intent(inout), allocatable :: array(:)
        integer(int64), intent(in), optional :: length
        integer(int64), intent(in), optional :: bounds(:)
        integer(int32) :: stat
        logical :: length_present, bounds_present
        integer(int64) :: requested_size
        integer(int32) :: first, last

        length_present = present(length)
        bounds_present = present(bounds)

        ! --- Argument validation ---
#ifdef USE_DEBUG
        if (length_present .and. bounds_present) call error_message(956)
        if (.not. length_present .and. .not. bounds_present) call error_message(957)
        if (bounds_present) then
            if (size(bounds) /= 2) call error_message(958)
        end if
#endif

        ! --- Main logic ---
        if (allocated(array)) call error_message(951)

        if (length_present) then
            ! Allocate by length
#ifdef USE_DEBUG
            if (length <= 0) call error_message(952)
            if (int(length, kind=int64) > huge(0_int64)) call error_message(953)
#endif
            allocate (array(length), stat=stat)
        else if (bounds_present) then
            ! Allocate by bounds
            first = bounds(1)
            last = bounds(2)

#ifdef USE_DEBUG
            if (first > last) call error_message(954)

            requested_size = int(last, kind=int64) - int(first, kind=int64) + 1_int64
            if (requested_size > huge(0_int64) / 4) call error_message(953)
#endif
            allocate (array(first:last), stat=stat)
        end if

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_int64

    subroutine allocate_rank1_real32(array, length, bounds)
        implicit none
        real(real32), intent(inout), allocatable :: array(:)
        integer(int32), intent(in), optional :: length
        integer(int32), intent(in), optional :: bounds(:)
        integer(int32) :: stat
        logical :: length_present, bounds_present
        integer(int64) :: requested_size
        integer(int32) :: first, last

        length_present = present(length)
        bounds_present = present(bounds)

        ! --- Argument validation ---
#ifdef USE_DEBUG
        if (length_present .and. bounds_present) call error_message(956)
        if (.not. length_present .and. .not. bounds_present) call error_message(957)
        if (bounds_present) then
            if (size(bounds) /= 2) call error_message(958)
        end if
#endif

        ! --- Main logic ---
        if (allocated(array)) call error_message(951)

        if (length_present) then
            ! Allocate by length
#ifdef USE_DEBUG
            if (length <= 0) call error_message(952)
            if (int(length, kind=int64) > huge(0_int32)) call error_message(953)
#endif
            allocate (array(length), stat=stat)
        else if (bounds_present) then
            ! Allocate by bounds
            first = bounds(1)
            last = bounds(2)

#ifdef USE_DEBUG
            if (first > last) call error_message(954)

            requested_size = int(last, kind=int64) - int(first, kind=int64) + 1_int64
            if (requested_size > huge(0_int64) / 4) call error_message(953)
#endif
            allocate (array(first:last), stat=stat)
        end if

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_real32

    subroutine allocate_rank1_real64(array, length, bounds)
        implicit none
        real(real64), intent(inout), allocatable :: array(:)
        integer(int32), intent(in), optional :: length
        integer(int32), intent(in), optional :: bounds(:)
        integer(int32) :: stat
        logical :: length_present, bounds_present
        integer(int64) :: requested_size
        integer(int32) :: first, last

        length_present = present(length)
        bounds_present = present(bounds)

        ! --- Argument validation ---
#ifdef USE_DEBUG
        if (length_present .and. bounds_present) call error_message(956)
        if (.not. length_present .and. .not. bounds_present) call error_message(957)
        if (bounds_present) then
            if (size(bounds) /= 2) call error_message(958)
        end if
#endif

        ! --- Main logic ---
        if (allocated(array)) call error_message(951)

        if (length_present) then
            ! Allocate by length
#ifdef USE_DEBUG
            if (length <= 0) call error_message(952)
            if (int(length, kind=int64) > huge(0_int32)) call error_message(953)
#endif
            allocate (array(length), stat=stat)
        else if (bounds_present) then
            ! Allocate by bounds
            first = bounds(1)
            last = bounds(2)

#ifdef USE_DEBUG
            if (first > last) call error_message(954)

            requested_size = int(last, kind=int64) - int(first, kind=int64) + 1_int64
            if (requested_size > huge(0_int64) / 4) call error_message(953)
#endif
            allocate (array(first:last), stat=stat)
        end if

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_real64

    subroutine allocate_rank1_real128(array, length, bounds)
        implicit none
        real(real128), intent(inout), allocatable :: array(:)
        integer(int32), intent(in), optional :: length
        integer(int32), intent(in), optional :: bounds(:)
        integer(int32) :: stat
        logical :: length_present, bounds_present
        integer(int64) :: requested_size
        integer(int32) :: first, last

        length_present = present(length)
        bounds_present = present(bounds)

        ! --- Argument validation ---
#ifdef USE_DEBUG
        if (length_present .and. bounds_present) call error_message(956)
        if (.not. length_present .and. .not. bounds_present) call error_message(957)
        if (bounds_present) then
            if (size(bounds) /= 2) call error_message(958)
        end if
#endif

        ! --- Main logic ---
        if (allocated(array)) call error_message(951)

        if (length_present) then
            ! Allocate by length
#ifdef USE_DEBUG
            if (length <= 0) call error_message(952)
            if (int(length, kind=int64) > huge(0_int32)) call error_message(953)
#endif
            allocate (array(length), stat=stat)
        else if (bounds_present) then
            ! Allocate by bounds
            first = bounds(1)
            last = bounds(2)

#ifdef USE_DEBUG
            if (first > last) call error_message(954)

            requested_size = int(last, kind=int64) - int(first, kind=int64) + 1_int64
            if (requested_size > huge(0_int64) / 4) call error_message(953)
#endif
            allocate (array(first:last), stat=stat)
        end if

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_real128

    subroutine allocate_rank1_logical1(array, length, bounds)
        implicit none
        logical(logical8), intent(inout), allocatable :: array(:)
        integer(int32), intent(in), optional :: length
        integer(int32), intent(in), optional :: bounds(:)
        integer(int32) :: stat
        logical :: length_present, bounds_present
        integer(int64) :: requested_size
        integer(int32) :: first, last

        length_present = present(length)
        bounds_present = present(bounds)

        ! --- Argument validation ---
#ifdef USE_DEBUG
        if (length_present .and. bounds_present) call error_message(956)
        if (.not. length_present .and. .not. bounds_present) call error_message(957)
        if (bounds_present) then
            if (size(bounds) /= 2) call error_message(958)
        end if
#endif

        ! --- Main logic ---
        if (allocated(array)) call error_message(951)

        if (length_present) then
            ! Allocate by length
#ifdef USE_DEBUG
            if (length <= 0) call error_message(952)
            if (int(length, kind=int64) > huge(0_int32)) call error_message(953)
#endif
            allocate (array(length), stat=stat)
        else if (bounds_present) then
            ! Allocate by bounds
            first = bounds(1)
            last = bounds(2)

            if (first > last) call error_message(954)

            requested_size = int(last, kind=int64) - int(first, kind=int64) + 1_int64
            if (requested_size > huge(0_int64) / 4) call error_message(953)
            allocate (array(first:last), stat=stat)
        end if

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_logical1

    subroutine allocate_rank1_logical4(array, length, bounds)
        implicit none
        logical(logical32), intent(inout), allocatable :: array(:)
        integer(int32), intent(in), optional :: length
        integer(int32), intent(in), optional :: bounds(:)
        integer(int32) :: stat
        logical :: length_present, bounds_present
        integer(int64) :: requested_size
        integer(int32) :: first, last

        length_present = present(length)
        bounds_present = present(bounds)

        ! --- Argument validation ---
#ifdef USE_DEBUG
        if (length_present .and. bounds_present) call error_message(956)
        if (.not. length_present .and. .not. bounds_present) call error_message(957)
        if (bounds_present) then
            if (size(bounds) /= 2) call error_message(958)
        end if
#endif

        ! --- Main logic ---
        if (allocated(array)) call error_message(951)

        if (length_present) then
            ! Allocate by length
#ifdef USE_DEBUG
            if (length <= 0) call error_message(952)
            if (int(length, kind=int64) > huge(0_int32)) call error_message(953)
#endif
            allocate (array(length), stat=stat)
        else if (bounds_present) then
            ! Allocate by bounds
            first = bounds(1)
            last = bounds(2)

#ifdef USE_DEBUG
            if (first > last) call error_message(954)

            requested_size = int(last, kind=int64) - int(first, kind=int64) + 1_int64
            if (requested_size > huge(0_int64) / 4) call error_message(953)
#endif
            allocate (array(first:last), stat=stat)
        end if

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_logical4

    subroutine allocate_rank1_logical8(array, length, bounds)
        implicit none
        logical(logical64), intent(inout), allocatable :: array(:)
        integer(int32), intent(in), optional :: length
        integer(int32), intent(in), optional :: bounds(:)
        integer(int32) :: stat
        logical :: length_present, bounds_present
        integer(int64) :: requested_size
        integer(int32) :: first, last

        length_present = present(length)
        bounds_present = present(bounds)

        ! --- Argument validation ---
#ifdef USE_DEBUG
        if (length_present .and. bounds_present) call error_message(956)
        if (.not. length_present .and. .not. bounds_present) call error_message(957)
        if (bounds_present) then
            if (size(bounds) /= 2) call error_message(958)
        end if
#endif

        ! --- Main logic ---
        if (allocated(array)) call error_message(951)

        if (length_present) then
            ! Allocate by length
#ifdef USE_DEBUG
            if (length <= 0) call error_message(952)
            if (int(length, kind=int64) > huge(0_int32)) call error_message(953)
#endif
            allocate (array(length), stat=stat)
        else if (bounds_present) then
            ! Allocate by bounds
            first = bounds(1)
            last = bounds(2)

#ifdef USE_DEBUG
            if (first > last) call error_message(954)

            requested_size = int(last, kind=int64) - int(first, kind=int64) + 1_int64
            if (requested_size > huge(0_int64) / 4) call error_message(953)
#endif
            allocate (array(first:last), stat=stat)
        end if

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank1_logical8

    subroutine allocate_rank2_int8(array, nrow, ncol)
        implicit none
        integer(int8), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: nrow, ncol
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)

#ifdef USE_DEBUG
        if (nrow <= 0 .or. ncol <= 0) call error_message(952)
#endif

        allocate (array(nrow, ncol), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank2_int8

    subroutine allocate_rank2_int16(array, num_row, num_col)
        implicit none
        integer(int16), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: num_row, num_col
        integer(int32) :: stat
        integer(int64) :: total_size

        if (allocated(array)) call error_message(951)

#ifdef USE_DEBUG
        if (num_row <= 0 .or. num_col <= 0) call error_message(952)

        total_size = int(num_row, kind=int64) * int(num_col, kind=int64)
        if (total_size > huge(0_int64) / 4) call error_message(953)
#endif

        allocate (array(num_row, num_col), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank2_int16

    subroutine allocate_rank2_int32(array, num_row, num_col)
        implicit none
        integer(int32), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: num_row, num_col
        integer(int32) :: stat
        integer(int64) :: total_size

        if (allocated(array)) call error_message(951)

#ifdef USE_DEBUG
        if (num_row <= 0 .or. num_col <= 0) call error_message(952)

        total_size = int(num_row, kind=int64) * int(num_col, kind=int64)
        if (total_size > huge(0_int64) / 4) call error_message(953)
#endif

        allocate (array(num_row, num_col), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank2_int32

    subroutine allocate_rank2_int64(array, num_row, num_col)
        implicit none
        integer(int64), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: num_row, num_col
        integer(int32) :: stat
        integer(int64) :: total_size

        if (allocated(array)) call error_message(951)

#ifdef USE_DEBUG
        if (num_row <= 0 .or. num_col <= 0) call error_message(952)

        total_size = int(num_row, kind=int64) * int(num_col, kind=int64)
        if (total_size > huge(0_int64) / 8) call error_message(953)
#endif

        allocate (array(num_row, num_col), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank2_int64

    subroutine allocate_rank2_real32(array, num_row, num_col)
        implicit none
        real(real32), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: num_row, num_col
        integer(int32) :: stat
        integer(int64) :: total_size

        if (allocated(array)) call error_message(951)

#ifdef USE_DEBUG
        if (num_row <= 0 .or. num_col <= 0) call error_message(952)

        total_size = int(num_row, kind=int64) * int(num_col, kind=int64)
        if (total_size > huge(0_int64) / 4) call error_message(953)
#endif

        allocate (array(num_row, num_col), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank2_real32

    subroutine allocate_rank2_real64(array, num_row, num_col)
        implicit none
        real(real64), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: num_row, num_col
        integer(int32) :: stat
        integer(int64) :: total_size

        if (allocated(array)) call error_message(951)

#ifdef USE_DEBUG
        if (num_row <= 0 .or. num_col <= 0) call error_message(952)

        total_size = int(num_row, kind=int64) * int(num_col, kind=int64)
        if (total_size > huge(0_int64) / 8) call error_message(953)
#endif

        allocate (array(num_row, num_col), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank2_real64

    subroutine allocate_rank2_real128(array, num_row, num_col)
        implicit none
        real(real128), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: num_row, num_col
        integer(int32) :: stat
        integer(int64) :: total_size

        if (allocated(array)) call error_message(951)

#ifdef USE_DEBUG
        if (num_row <= 0 .or. num_col <= 0) call error_message(952)

        total_size = int(num_row, kind=int64) * int(num_col, kind=int64)
        if (total_size > huge(0_int64) / 16) call error_message(953)
#endif

        allocate (array(num_row, num_col), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank2_real128

    subroutine allocate_rank2_logical1(array, num_row, num_col)
        implicit none
        logical(logical8), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: num_row, num_col
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)
        if (num_row <= 0 .or. num_col <= 0) call error_message(952)

        allocate (array(num_row, num_col), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank2_logical1

    subroutine allocate_rank2_logical4(array, num_row, num_col)
        implicit none
        logical(logical32), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: num_row, num_col
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)

#ifdef USE_DEBUG
        if (num_row <= 0 .or. num_col <= 0) call error_message(952)
#endif

        allocate (array(num_row, num_col), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank2_logical4

    subroutine allocate_rank2_logical8(array, num_row, num_col)
        implicit none
        logical(logical64), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: num_row, num_col
        integer(int32) :: stat

        if (allocated(array)) call error_message(951)

#ifdef USE_DEBUG
        if (num_row <= 0 .or. num_col <= 0) call error_message(952)
#endif

        allocate (array(num_row, num_col), stat=stat)

        if (stat /= 0) call error_message(955)

    end subroutine allocate_rank2_logical8

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

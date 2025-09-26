!>
!> Provides generic interfaces and safe wrappers for allocating memory for
!> allocatable arrays and pointers of various intrinsic types and ranks.
!>
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

    ! ==========================================================
    ! Rank-1 Allocatable Arrays
    ! ==========================================================

    !>
    !> Safely allocates a rank-1 allocatable 8-bit integer array.
    !> Exactly one of 'length', 'bounds', or 'source' must be provided.
    !>
    subroutine allocate_rank1_int8(array, length, bounds, source)
        implicit none
        !> The allocatable array to be allocated.
        integer(int8), intent(inout), allocatable :: array(:)
        !> The desired size of the array (1-based index).
        integer(int32), intent(in), optional :: length
        !> A two-element array specifying the lower and upper bounds.
        integer(int32), intent(in), optional :: bounds(:)
        !> An existing array to use as a source for allocation and value copy.
        integer(int8), intent(in), optional :: source(:)

        integer(int32) :: stat
        logical :: length_present, bounds_present, source_present
        integer(int64) :: requested_size
        integer(int32) :: first, last

        length_present = present(length)
        bounds_present = present(bounds)
        source_present = present(source)

        ! Argument validation
#ifdef USE_DEBUG
        if ((merge(1, 0, length_present) + merge(1, 0, bounds_present) + merge(1, 0, source_present)) > 1) call error_message(956)
        if (.not. length_present .and. .not. bounds_present .and. .not. source_present) call error_message(957)
        if (bounds_present) then
            if (size(bounds) /= 2) call error_message(958)
        end if
#endif

        if (allocated(array)) call error_message(951)

        ! Main allocation logic
        if (length_present) then
#ifdef USE_DEBUG
            if (length <= 0) call error_message(952)
#endif
            allocate (array(length), stat=stat)
        else if (bounds_present) then
            first = bounds(1)
            last = bounds(2)
#ifdef USE_DEBUG
            if (first > last) call error_message(954)
#endif
            allocate (array(first:last), stat=stat)
        else if (source_present) then
            allocate (array, source=source, stat=stat)
        end if

#ifdef USE_DEBUG
        if (stat /= 0) call error_message(955)
#endif
    end subroutine allocate_rank1_int8

    !>
    !> Safely allocates a rank-1 allocatable 16-bit integer array.
    !> Exactly one of 'length', 'bounds', or 'source' must be provided.
    !>
    subroutine allocate_rank1_int16(array, length, bounds, source)
        implicit none
        !> The allocatable array to be allocated.
        integer(int16), intent(inout), allocatable :: array(:)
        !> The desired size of the array (1-based index).
        integer(int32), intent(in), optional :: length
        !> A two-element array specifying the lower and upper bounds.
        integer(int32), intent(in), optional :: bounds(:)
        !> An existing array to use as a source for allocation and value copy.
        integer(int16), intent(in), optional :: source(:)

        integer(int32) :: stat
        logical :: length_present, bounds_present, source_present
        integer(int64) :: requested_size
        integer(int32) :: first, last

        length_present = present(length)
        bounds_present = present(bounds)
        source_present = present(source)

#ifdef USE_DEBUG
        if ((merge(1, 0, length_present) + merge(1, 0, bounds_present) + merge(1, 0, source_present)) > 1) call error_message(956)
        if (.not. length_present .and. .not. bounds_present .and. .not. source_present) call error_message(957)
        if (bounds_present) then
            if (size(bounds) /= 2) call error_message(958)
        end if
#endif

        if (allocated(array)) call error_message(951)

        if (length_present) then
#ifdef USE_DEBUG
            if (length <= 0) call error_message(952)
            if (int(length, kind=int64) > huge(0_int64) / 2) call error_message(953)
#endif
            allocate (array(length), stat=stat)
        else if (bounds_present) then
            first = bounds(1)
            last = bounds(2)
#ifdef USE_DEBUG
            if (first > last) call error_message(954)
            requested_size = int(last, kind=int64) - int(first, kind=int64) + 1_int64
            if (requested_size > huge(0_int64) / 2) call error_message(953)
#endif
            allocate (array(first:last), stat=stat)
        else if (source_present) then
            allocate (array, source=source, stat=stat)
        end if

#ifdef USE_DEBUG
        if (stat /= 0) call error_message(955)
#endif
    end subroutine allocate_rank1_int16

    !>
    !> Safely allocates a rank-1 allocatable 32-bit integer array.
    !> Exactly one of 'length', 'bounds', or 'source' must be provided.
    !>
    subroutine allocate_rank1_int32(array, length, bounds, source)
        implicit none
        !> The allocatable array to be allocated.
        integer(int32), intent(inout), allocatable :: array(:)
        !> The desired size of the array (1-based index).
        integer(int32), intent(in), optional :: length
        !> A two-element array specifying the lower and upper bounds.
        integer(int32), intent(in), optional :: bounds(:)
        !> An existing array to use as a source for allocation and value copy.
        integer(int32), intent(in), optional :: source(:)

        integer(int32) :: stat
        logical :: length_present, bounds_present, source_present
        integer(int64) :: requested_size
        integer(int32) :: first, last

        length_present = present(length)
        bounds_present = present(bounds)
        source_present = present(source)

#ifdef USE_DEBUG
        if ((merge(1, 0, length_present) + merge(1, 0, bounds_present) + merge(1, 0, source_present)) > 1) call error_message(956)
        if (.not. length_present .and. .not. bounds_present .and. .not. source_present) call error_message(957)
        if (bounds_present) then
            if (size(bounds) /= 2) call error_message(958)
        end if
#endif

        if (allocated(array)) call error_message(951)

        if (length_present) then
#ifdef USE_DEBUG
            if (length <= 0) call error_message(952)
            if (int(length, kind=int64) > huge(0_int64) / 4) call error_message(953)
#endif
            allocate (array(length), stat=stat)
        else if (bounds_present) then
            first = bounds(1)
            last = bounds(2)
#ifdef USE_DEBUG
            if (first > last) call error_message(954)
            requested_size = int(last, kind=int64) - int(first, kind=int64) + 1_int64
            if (requested_size > huge(0_int64) / 4) call error_message(953)
#endif
            allocate (array(first:last), stat=stat)
        else if (source_present) then
            allocate (array, source=source, stat=stat)
        end if

#ifdef USE_DEBUG
        if (stat /= 0) call error_message(955)
#endif
    end subroutine allocate_rank1_int32

    !>
    !> Safely allocates a rank-1 allocatable 64-bit integer array.
    !> Exactly one of 'length', 'bounds', or 'source' must be provided.
    !>
    subroutine allocate_rank1_int64(array, length, bounds, source)
        implicit none
        !> The allocatable array to be allocated.
        integer(int64), intent(inout), allocatable :: array(:)
        !> The desired size of the array (1-based index).
        integer(int64), intent(in), optional :: length
        !> A two-element array specifying the lower and upper bounds.
        integer(int64), intent(in), optional :: bounds(:)
        !> An existing array to use as a source for allocation and value copy.
        integer(int64), intent(in), optional :: source(:)

        integer(int32) :: stat
        logical :: length_present, bounds_present, source_present
        integer(int64) :: requested_size
        integer(int64) :: first, last

        length_present = present(length)
        bounds_present = present(bounds)
        source_present = present(source)

#ifdef USE_DEBUG
        if ((merge(1, 0, length_present) + merge(1, 0, bounds_present) + merge(1, 0, source_present)) > 1) call error_message(956)
        if (.not. length_present .and. .not. bounds_present .and. .not. source_present) call error_message(957)
        if (bounds_present) then
            if (size(bounds) /= 2) call error_message(958)
        end if
#endif

        if (allocated(array)) call error_message(951)

        if (length_present) then
#ifdef USE_DEBUG
            if (length <= 0) call error_message(952)
            if (length > huge(0_int64) / 8) call error_message(953)
#endif
            allocate (array(length), stat=stat)
        else if (bounds_present) then
            first = bounds(1)
            last = bounds(2)
#ifdef USE_DEBUG
            if (first > last) call error_message(954)
            requested_size = last - first + 1_int64
            if (requested_size > huge(0_int64) / 8) call error_message(953)
#endif
            allocate (array(first:last), stat=stat)
        else if (source_present) then
            allocate (array, source=source, stat=stat)
        end if

#ifdef USE_DEBUG
        if (stat /= 0) call error_message(955)
#endif
    end subroutine allocate_rank1_int64

    !>
    !> Safely allocates a rank-1 allocatable single precision real array.
    !> Exactly one of 'length', 'bounds', or 'source' must be provided.
    !>
    subroutine allocate_rank1_real32(array, length, bounds, source)
        implicit none
        !> The allocatable array to be allocated.
        real(real32), intent(inout), allocatable :: array(:)
        !> The desired size of the array (1-based index).
        integer(int32), intent(in), optional :: length
        !> A two-element array specifying the lower and upper bounds.
        integer(int32), intent(in), optional :: bounds(:)
        !> An existing array to use as a source for allocation and value copy.
        real(real32), intent(in), optional :: source(:)

        integer(int32) :: stat
        logical :: length_present, bounds_present, source_present
        integer(int64) :: requested_size
        integer(int32) :: first, last

        length_present = present(length)
        bounds_present = present(bounds)
        source_present = present(source)

#ifdef USE_DEBUG
        if ((merge(1, 0, length_present) + merge(1, 0, bounds_present) + merge(1, 0, source_present)) > 1) call error_message(956)
        if (.not. length_present .and. .not. bounds_present .and. .not. source_present) call error_message(957)
        if (bounds_present) then
            if (size(bounds) /= 2) call error_message(958)
        end if
#endif

        if (allocated(array)) call error_message(951)

        if (length_present) then
#ifdef USE_DEBUG
            if (length <= 0) call error_message(952)
            if (int(length, kind=int64) > huge(0_int64) / 4) call error_message(953)
#endif
            allocate (array(length), stat=stat)
        else if (bounds_present) then
            first = bounds(1)
            last = bounds(2)
#ifdef USE_DEBUG
            if (first > last) call error_message(954)
            requested_size = int(last, kind=int64) - int(first, kind=int64) + 1_int64
            if (requested_size > huge(0_int64) / 4) call error_message(953)
#endif
            allocate (array(first:last), stat=stat)
        else if (source_present) then
            allocate (array, source=source, stat=stat)
        end if

#ifdef USE_DEBUG
        if (stat /= 0) call error_message(955)
#endif
    end subroutine allocate_rank1_real32

    !>
    !> Safely allocates a rank-1 allocatable double precision real array.
    !> Exactly one of 'length', 'bounds', or 'source' must be provided.
    !>
    subroutine allocate_rank1_real64(array, length, bounds, source)
        implicit none
        !> The allocatable array to be allocated.
        real(real64), intent(inout), allocatable :: array(:)
        !> The desired size of the array (1-based index).
        integer(int32), intent(in), optional :: length
        !> A two-element array specifying the lower and upper bounds.
        integer(int32), intent(in), optional :: bounds(:)
        !> An existing array to use as a source for allocation and value copy.
        real(real64), intent(in), optional :: source(:)

        integer(int32) :: stat
        logical :: length_present, bounds_present, source_present
        integer(int64) :: requested_size
        integer(int32) :: first, last

        length_present = present(length)
        bounds_present = present(bounds)
        source_present = present(source)

#ifdef USE_DEBUG
        if ((merge(1, 0, length_present) + merge(1, 0, bounds_present) + merge(1, 0, source_present)) > 1) call error_message(956)
        if (.not. length_present .and. .not. bounds_present .and. .not. source_present) call error_message(957)
        if (bounds_present) then
            if (size(bounds) /= 2) call error_message(958)
        end if
#endif

        if (allocated(array)) call error_message(951)

        if (length_present) then
#ifdef USE_DEBUG
            if (length <= 0) call error_message(952)
            if (int(length, kind=int64) > huge(0_int64) / 8) call error_message(953)
#endif
            allocate (array(length), stat=stat)
        else if (bounds_present) then
            first = bounds(1)
            last = bounds(2)
#ifdef USE_DEBUG
            if (first > last) call error_message(954)
            requested_size = int(last, kind=int64) - int(first, kind=int64) + 1_int64
            if (requested_size > huge(0_int64) / 8) call error_message(953)
#endif
            allocate (array(first:last), stat=stat)
        else if (source_present) then
            allocate (array, source=source, stat=stat)
        end if

#ifdef USE_DEBUG
        if (stat /= 0) call error_message(955)
#endif
    end subroutine allocate_rank1_real64

    !>
    !> Safely allocates a rank-1 allocatable quad precision real array.
    !> Exactly one of 'length', 'bounds', or 'source' must be provided.
    !>
    subroutine allocate_rank1_real128(array, length, bounds, source)
        implicit none
        !> The allocatable array to be allocated.
        real(real128), intent(inout), allocatable :: array(:)
        !> The desired size of the array (1-based index).
        integer(int32), intent(in), optional :: length
        !> A two-element array specifying the lower and upper bounds.
        integer(int32), intent(in), optional :: bounds(:)
        !> An existing array to use as a source for allocation and value copy.
        real(real128), intent(in), optional :: source(:)

        integer(int32) :: stat
        logical :: length_present, bounds_present, source_present
        integer(int64) :: requested_size
        integer(int32) :: first, last

        length_present = present(length)
        bounds_present = present(bounds)
        source_present = present(source)

#ifdef USE_DEBUG
        if ((merge(1, 0, length_present) + merge(1, 0, bounds_present) + merge(1, 0, source_present)) > 1) call error_message(956)
        if (.not. length_present .and. .not. bounds_present .and. .not. source_present) call error_message(957)
        if (bounds_present) then
            if (size(bounds) /= 2) call error_message(958)
        end if
#endif

        if (allocated(array)) call error_message(951)

        if (length_present) then
#ifdef USE_DEBUG
            if (length <= 0) call error_message(952)
            if (int(length, kind=int64) > huge(0_int64) / 16) call error_message(953)
#endif
            allocate (array(length), stat=stat)
        else if (bounds_present) then
            first = bounds(1)
            last = bounds(2)
#ifdef USE_DEBUG
            if (first > last) call error_message(954)
            requested_size = int(last, kind=int64) - int(first, kind=int64) + 1_int64
            if (requested_size > huge(0_int64) / 16) call error_message(953)
#endif
            allocate (array(first:last), stat=stat)
        else if (source_present) then
            allocate (array, source=source, stat=stat)
        end if

#ifdef USE_DEBUG
        if (stat /= 0) call error_message(955)
#endif
    end subroutine allocate_rank1_real128

    !>
    !> Safely allocates a rank-1 allocatable logical (kind=8) array.
    !> Exactly one of 'length', 'bounds', or 'source' must be provided.
    !>
    subroutine allocate_rank1_logical1(array, length, bounds, source)
        implicit none
        !> The allocatable array to be allocated.
        logical(logical8), intent(inout), allocatable :: array(:)
        !> The desired size of the array (1-based index).
        integer(int32), intent(in), optional :: length
        !> A two-element array specifying the lower and upper bounds.
        integer(int32), intent(in), optional :: bounds(:)
        !> An existing array to use as a source for allocation and value copy.
        logical(logical8), intent(in), optional :: source(:)

        integer(int32) :: stat
        logical :: length_present, bounds_present, source_present
        integer(int32) :: first, last

        length_present = present(length)
        bounds_present = present(bounds)
        source_present = present(source)

#ifdef USE_DEBUG
        if ((merge(1, 0, length_present) + merge(1, 0, bounds_present) + merge(1, 0, source_present)) > 1) call error_message(956)
        if (.not. length_present .and. .not. bounds_present .and. .not. source_present) call error_message(957)
        if (bounds_present) then
            if (size(bounds) /= 2) call error_message(958)
        end if
#endif

        if (allocated(array)) call error_message(951)

        if (length_present) then
#ifdef USE_DEBUG
            if (length <= 0) call error_message(952)
#endif
            allocate (array(length), stat=stat)
        else if (bounds_present) then
            first = bounds(1)
            last = bounds(2)
#ifdef USE_DEBUG
            if (first > last) call error_message(954)
#endif
            allocate (array(first:last), stat=stat)
        else if (source_present) then
            allocate (array, source=source, stat=stat)
        end if

#ifdef USE_DEBUG
        if (stat /= 0) call error_message(955)
#endif
    end subroutine allocate_rank1_logical1

    !>
    !> Safely allocates a rank-1 allocatable logical (kind=32) array.
    !> Exactly one of 'length', 'bounds', or 'source' must be provided.
    !>
    subroutine allocate_rank1_logical4(array, length, bounds, source)
        implicit none
        !> The allocatable array to be allocated.
        logical(logical32), intent(inout), allocatable :: array(:)
        !> The desired size of the array (1-based index).
        integer(int32), intent(in), optional :: length
        !> A two-element array specifying the lower and upper bounds.
        integer(int32), intent(in), optional :: bounds(:)
        !> An existing array to use as a source for allocation and value copy.
        logical(logical32), intent(in), optional :: source(:)

        integer(int32) :: stat
        logical :: length_present, bounds_present, source_present
        integer(int64) :: requested_size
        integer(int32) :: first, last

        length_present = present(length)
        bounds_present = present(bounds)
        source_present = present(source)

#ifdef USE_DEBUG
        if ((merge(1, 0, length_present) + merge(1, 0, bounds_present) + merge(1, 0, source_present)) > 1) call error_message(956)
        if (.not. length_present .and. .not. bounds_present .and. .not. source_present) call error_message(957)
        if (bounds_present) then
            if (size(bounds) /= 2) call error_message(958)
        end if
#endif

        if (allocated(array)) call error_message(951)

        if (length_present) then
#ifdef USE_DEBUG
            if (length <= 0) call error_message(952)
            if (int(length, kind=int64) > huge(0_int64) / 4) call error_message(953)
#endif
            allocate (array(length), stat=stat)
        else if (bounds_present) then
            first = bounds(1)
            last = bounds(2)
#ifdef USE_DEBUG
            if (first > last) call error_message(954)
            requested_size = int(last, kind=int64) - int(first, kind=int64) + 1_int64
            if (requested_size > huge(0_int64) / 4) call error_message(953)
#endif
            allocate (array(first:last), stat=stat)
        else if (source_present) then
            allocate (array, source=source, stat=stat)
        end if

#ifdef USE_DEBUG
        if (stat /= 0) call error_message(955)
#endif
    end subroutine allocate_rank1_logical4

    !>
    !> Safely allocates a rank-1 allocatable logical (kind=64) array.
    !> Exactly one of 'length', 'bounds', or 'source' must be provided.
    !>
    subroutine allocate_rank1_logical8(array, length, bounds, source)
        implicit none
        !> The allocatable array to be allocated.
        logical(logical64), intent(inout), allocatable :: array(:)
        !> The desired size of the array (1-based index).
        integer(int32), intent(in), optional :: length
        !> A two-element array specifying the lower and upper bounds.
        integer(int32), intent(in), optional :: bounds(:)
        !> An existing array to use as a source for allocation and value copy.
        logical(logical64), intent(in), optional :: source(:)

        integer(int32) :: stat
        logical :: length_present, bounds_present, source_present
        integer(int64) :: requested_size
        integer(int32) :: first, last

        length_present = present(length)
        bounds_present = present(bounds)
        source_present = present(source)

#ifdef USE_DEBUG
        if ((merge(1, 0, length_present) + merge(1, 0, bounds_present) + merge(1, 0, source_present)) > 1) call error_message(956)
        if (.not. length_present .and. .not. bounds_present .and. .not. source_present) call error_message(957)
        if (bounds_present) then
            if (size(bounds) /= 2) call error_message(958)
        end if
#endif

        if (allocated(array)) call error_message(951)

        if (length_present) then
#ifdef USE_DEBUG
            if (length <= 0) call error_message(952)
            if (int(length, kind=int64) > huge(0_int64) / 8) call error_message(953)
#endif
            allocate (array(length), stat=stat)
        else if (bounds_present) then
            first = bounds(1)
            last = bounds(2)
#ifdef USE_DEBUG
            if (first > last) call error_message(954)
            requested_size = int(last, kind=int64) - int(first, kind=int64) + 1_int64
            if (requested_size > huge(0_int64) / 8) call error_message(953)
#endif
            allocate (array(first:last), stat=stat)
        else if (source_present) then
            allocate (array, source=source, stat=stat)
        end if

#ifdef USE_DEBUG
        if (stat /= 0) call error_message(955)
#endif
    end subroutine allocate_rank1_logical8

    ! ==========================================================
    ! Rank-2 Allocatable Arrays
    ! ==========================================================

    !>
    !> Safely allocates a rank-2 allocatable 8-bit integer array.
    !> Either the shape (nrow, ncol) or a source array must be provided.
    !>
    subroutine allocate_rank2_int8(array, nrow, ncol, source)
        implicit none
        !> The allocatable array to be allocated.
        integer(int8), intent(inout), allocatable :: array(:, :)
        !> The number of rows.
        integer(int32), intent(in), optional :: nrow
        !> The number of columns.
        integer(int32), intent(in), optional :: ncol
        !> An existing array to use as a source for allocation and value copy.
        integer(int8), intent(in), optional :: source(:, :)

        integer(int32) :: stat
        logical :: shape_present, source_present

        shape_present = present(nrow) .and. present(ncol)
        source_present = present(source)

#ifdef USE_DEBUG
        if (shape_present .and. source_present) call error_message(956)
        if (.not. shape_present .and. .not. source_present) call error_message(957)
        if (present(nrow) .neqv. present(ncol)) call error_message(958)
#endif

        if (allocated(array)) call error_message(951)

        if (shape_present) then
#ifdef USE_DEBUG
            if (nrow <= 0 .or. ncol <= 0) call error_message(952)
#endif
            allocate (array(nrow, ncol), stat=stat)
        else if (source_present) then
            allocate (array, source=source, stat=stat)
        end if

#ifdef USE_DEBUG
        if (stat /= 0) call error_message(955)
#endif
    end subroutine allocate_rank2_int8

    !>
    !> Safely allocates a rank-2 allocatable 16-bit integer array.
    !> Either the shape (num_row, num_col) or a source array must be provided.
    !>
    subroutine allocate_rank2_int16(array, num_row, num_col, source)
        implicit none
        !> The allocatable array to be allocated.
        integer(int16), intent(inout), allocatable :: array(:, :)
        !> The number of rows.
        integer(int32), intent(in), optional :: num_row
        !> The number of columns.
        integer(int32), intent(in), optional :: num_col
        !> An existing array to use as a source for allocation and value copy.
        integer(int16), intent(in), optional :: source(:, :)

        integer(int32) :: stat
        integer(int64) :: total_size
        logical :: shape_present, source_present

        shape_present = present(num_row) .and. present(num_col)
        source_present = present(source)

#ifdef USE_DEBUG
        if (shape_present .and. source_present) call error_message(956)
        if (.not. shape_present .and. .not. source_present) call error_message(957)
        if (present(num_row) .neqv. present(num_col)) call error_message(958)
#endif

        if (allocated(array)) call error_message(951)

        if (shape_present) then
#ifdef USE_DEBUG
            if (num_row <= 0 .or. num_col <= 0) call error_message(952)
            total_size = int(num_row, kind=int64) * int(num_col, kind=int64)
            if (total_size > huge(0_int64) / 2) call error_message(953)
#endif
            allocate (array(num_row, num_col), stat=stat)
        else if (source_present) then
            allocate (array, source=source, stat=stat)
        end if

#ifdef USE_DEBUG
        if (stat /= 0) call error_message(955)
#endif
    end subroutine allocate_rank2_int16

    !>
    !> Safely allocates a rank-2 allocatable 32-bit integer array.
    !> Either the shape (num_row, num_col) or a source array must be provided.
    !>
    subroutine allocate_rank2_int32(array, num_row, num_col, source)
        implicit none
        !> The allocatable array to be allocated.
        integer(int32), intent(inout), allocatable :: array(:, :)
        !> The number of rows.
        integer(int32), intent(in), optional :: num_row
        !> The number of columns.
        integer(int32), intent(in), optional :: num_col
        !> An existing array to use as a source for allocation and value copy.
        integer(int32), intent(in), optional :: source(:, :)

        integer(int32) :: stat
        integer(int64) :: total_size
        logical :: shape_present, source_present

        shape_present = present(num_row) .and. present(num_col)
        source_present = present(source)

#ifdef USE_DEBUG
        if (shape_present .and. source_present) call error_message(956)
        if (.not. shape_present .and. .not. source_present) call error_message(957)
        if (present(num_row) .neqv. present(num_col)) call error_message(958)
#endif

        if (allocated(array)) call error_message(951)

        if (shape_present) then
#ifdef USE_DEBUG
            if (num_row <= 0 .or. num_col <= 0) call error_message(952)
            total_size = int(num_row, kind=int64) * int(num_col, kind=int64)
            if (total_size > huge(0_int64) / 4) call error_message(953)
#endif
            allocate (array(num_row, num_col), stat=stat)
        else if (source_present) then
            allocate (array, source=source, stat=stat)
        end if

#ifdef USE_DEBUG
        if (stat /= 0) call error_message(955)
#endif
    end subroutine allocate_rank2_int32

    !>
    !> Safely allocates a rank-2 allocatable 64-bit integer array.
    !> Either the shape (num_row, num_col) or a source array must be provided.
    !>
    subroutine allocate_rank2_int64(array, num_row, num_col, source)
        implicit none
        !> The allocatable array to be allocated.
        integer(int64), intent(inout), allocatable :: array(:, :)
        !> The number of rows.
        integer(int32), intent(in), optional :: num_row
        !> The number of columns.
        integer(int32), intent(in), optional :: num_col
        !> An existing array to use as a source for allocation and value copy.
        integer(int64), intent(in), optional :: source(:, :)

        integer(int32) :: stat
        integer(int64) :: total_size
        logical :: shape_present, source_present

        shape_present = present(num_row) .and. present(num_col)
        source_present = present(source)

#ifdef USE_DEBUG
        if (shape_present .and. source_present) call error_message(956)
        if (.not. shape_present .and. .not. source_present) call error_message(957)
        if (present(num_row) .neqv. present(num_col)) call error_message(958)
#endif

        if (allocated(array)) call error_message(951)

        if (shape_present) then
#ifdef USE_DEBUG
            if (num_row <= 0 .or. num_col <= 0) call error_message(952)
            total_size = int(num_row, kind=int64) * int(num_col, kind=int64)
            if (total_size > huge(0_int64) / 8) call error_message(953)
#endif
            allocate (array(num_row, num_col), stat=stat)
        else if (source_present) then
            allocate (array, source=source, stat=stat)
        end if

#ifdef USE_DEBUG
        if (stat /= 0) call error_message(955)
#endif
    end subroutine allocate_rank2_int64

    !>
    !> Safely allocates a rank-2 allocatable single precision real array.
    !> Either the shape (num_row, num_col) or a source array must be provided.
    !>
    subroutine allocate_rank2_real32(array, num_row, num_col, source)
        implicit none
        !> The allocatable array to be allocated.
        real(real32), intent(inout), allocatable :: array(:, :)
        !> The number of rows.
        integer(int32), intent(in), optional :: num_row
        !> The number of columns.
        integer(int32), intent(in), optional :: num_col
        !> An existing array to use as a source for allocation and value copy.
        real(real32), intent(in), optional :: source(:, :)

        integer(int32) :: stat
        integer(int64) :: total_size
        logical :: shape_present, source_present

        shape_present = present(num_row) .and. present(num_col)
        source_present = present(source)

#ifdef USE_DEBUG
        if (shape_present .and. source_present) call error_message(956)
        if (.not. shape_present .and. .not. source_present) call error_message(957)
        if (present(num_row) .neqv. present(num_col)) call error_message(958)
#endif

        if (allocated(array)) call error_message(951)

        if (shape_present) then
#ifdef USE_DEBUG
            if (num_row <= 0 .or. num_col <= 0) call error_message(952)
            total_size = int(num_row, kind=int64) * int(num_col, kind=int64)
            if (total_size > huge(0_int64) / 4) call error_message(953)
#endif
            allocate (array(num_row, num_col), stat=stat)
        else if (source_present) then
            allocate (array, source=source, stat=stat)
        end if

#ifdef USE_DEBUG
        if (stat /= 0) call error_message(955)
#endif
    end subroutine allocate_rank2_real32

    !>
    !> Safely allocates a rank-2 allocatable double precision real array.
    !> Either the shape (num_row, num_col) or a source array must be provided.
    !>
    subroutine allocate_rank2_real64(array, num_row, num_col, source)
        implicit none
        !> The allocatable array to be allocated.
        real(real64), intent(inout), allocatable :: array(:, :)
        !> The number of rows.
        integer(int32), intent(in), optional :: num_row
        !> The number of columns.
        integer(int32), intent(in), optional :: num_col
        !> An existing array to use as a source for allocation and value copy.
        real(real64), intent(in), optional :: source(:, :)

        integer(int32) :: stat
        integer(int64) :: total_size
        logical :: shape_present, source_present

        shape_present = present(num_row) .and. present(num_col)
        source_present = present(source)

#ifdef USE_DEBUG
        if (shape_present .and. source_present) call error_message(956)
        if (.not. shape_present .and. .not. source_present) call error_message(957)
        if (present(num_row) .neqv. present(num_col)) call error_message(958)
#endif

        if (allocated(array)) call error_message(951)

        if (shape_present) then
#ifdef USE_DEBUG
            if (num_row <= 0 .or. num_col <= 0) call error_message(952)
            total_size = int(num_row, kind=int64) * int(num_col, kind=int64)
            if (total_size > huge(0_int64) / 8) call error_message(953)
#endif
            allocate (array(num_row, num_col), stat=stat)
        else if (source_present) then
            allocate (array, source=source, stat=stat)
        end if

#ifdef USE_DEBUG
        if (stat /= 0) call error_message(955)
#endif
    end subroutine allocate_rank2_real64

    !>
    !> Safely allocates a rank-2 allocatable quad precision real array.
    !> Either the shape (num_row, num_col) or a source array must be provided.
    !>
    subroutine allocate_rank2_real128(array, num_row, num_col, source)
        implicit none
        !> The allocatable array to be allocated.
        real(real128), intent(inout), allocatable :: array(:, :)
        !> The number of rows.
        integer(int32), intent(in), optional :: num_row
        !> The number of columns.
        integer(int32), intent(in), optional :: num_col
        !> An existing array to use as a source for allocation and value copy.
        real(real128), intent(in), optional :: source(:, :)

        integer(int32) :: stat
        integer(int64) :: total_size
        logical :: shape_present, source_present

        shape_present = present(num_row) .and. present(num_col)
        source_present = present(source)

#ifdef USE_DEBUG
        if (shape_present .and. source_present) call error_message(956)
        if (.not. shape_present .and. .not. source_present) call error_message(957)
        if (present(num_row) .neqv. present(num_col)) call error_message(958)
#endif

        if (allocated(array)) call error_message(951)

        if (shape_present) then
#ifdef USE_DEBUG
            if (num_row <= 0 .or. num_col <= 0) call error_message(952)
            total_size = int(num_row, kind=int64) * int(num_col, kind=int64)
            if (total_size > huge(0_int64) / 16) call error_message(953)
#endif
            allocate (array(num_row, num_col), stat=stat)
        else if (source_present) then
            allocate (array, source=source, stat=stat)
        end if

#ifdef USE_DEBUG
        if (stat /= 0) call error_message(955)
#endif
    end subroutine allocate_rank2_real128

    !>
    !> Safely allocates a rank-2 allocatable logical (kind=8) array.
    !> Either the shape (num_row, num_col) or a source array must be provided.
    !>
    subroutine allocate_rank2_logical1(array, num_row, num_col, source)
        implicit none
        !> The allocatable array to be allocated.
        logical(logical8), intent(inout), allocatable :: array(:, :)
        !> The number of rows.
        integer(int32), intent(in), optional :: num_row
        !> The number of columns.
        integer(int32), intent(in), optional :: num_col
        !> An existing array to use as a source for allocation and value copy.
        logical(logical8), intent(in), optional :: source(:, :)

        integer(int32) :: stat
        logical :: shape_present, source_present

        shape_present = present(num_row) .and. present(num_col)
        source_present = present(source)

#ifdef USE_DEBUG
        if (shape_present .and. source_present) call error_message(956)
        if (.not. shape_present .and. .not. source_present) call error_message(957)
        if (present(num_row) .neqv. present(num_col)) call error_message(958)
#endif

        if (allocated(array)) call error_message(951)

        if (shape_present) then
#ifdef USE_DEBUG
            if (num_row <= 0 .or. num_col <= 0) call error_message(952)
#endif
            allocate (array(num_row, num_col), stat=stat)
        else if (source_present) then
            allocate (array, source=source, stat=stat)
        end if

#ifdef USE_DEBUG
        if (stat /= 0) call error_message(955)
#endif
    end subroutine allocate_rank2_logical1

    !>
    !> Safely allocates a rank-2 allocatable logical (kind=32) array.
    !> Either the shape (num_row, num_col) or a source array must be provided.
    !>
    subroutine allocate_rank2_logical4(array, num_row, num_col, source)
        implicit none
        !> The allocatable array to be allocated.
        logical(logical32), intent(inout), allocatable :: array(:, :)
        !> The number of rows.
        integer(int32), intent(in), optional :: num_row
        !> The number of columns.
        integer(int32), intent(in), optional :: num_col
        !> An existing array to use as a source for allocation and value copy.
        logical(logical32), intent(in), optional :: source(:, :)

        integer(int32) :: stat
        logical :: shape_present, source_present

        shape_present = present(num_row) .and. present(num_col)
        source_present = present(source)

#ifdef USE_DEBUG
        if (shape_present .and. source_present) call error_message(956)
        if (.not. shape_present .and. .not. source_present) call error_message(957)
        if (present(num_row) .neqv. present(num_col)) call error_message(958)
#endif

        if (allocated(array)) call error_message(951)

        if (shape_present) then
#ifdef USE_DEBUG
            if (num_row <= 0 .or. num_col <= 0) call error_message(952)
#endif
            allocate (array(num_row, num_col), stat=stat)
        else if (source_present) then
            allocate (array, source=source, stat=stat)
        end if

#ifdef USE_DEBUG
        if (stat /= 0) call error_message(955)
#endif
    end subroutine allocate_rank2_logical4

    !>
    !> Safely allocates a rank-2 allocatable logical (kind=64) array.
    !> Either the shape (num_row, num_col) or a source array must be provided.
    !>
    subroutine allocate_rank2_logical8(array, num_row, num_col, source)
        implicit none
        !> The allocatable array to be allocated.
        logical(logical64), intent(inout), allocatable :: array(:, :)
        !> The number of rows.
        integer(int32), intent(in), optional :: num_row
        !> The number of columns.
        integer(int32), intent(in), optional :: num_col
        !> An existing array to use as a source for allocation and value copy.
        logical(logical64), intent(in), optional :: source(:, :)

        integer(int32) :: stat
        logical :: shape_present, source_present

        shape_present = present(num_row) .and. present(num_col)
        source_present = present(source)

#ifdef USE_DEBUG
        if (shape_present .and. source_present) call error_message(956)
        if (.not. shape_present .and. .not. source_present) call error_message(957)
        if (present(num_row) .neqv. present(num_col)) call error_message(958)
#endif

        if (allocated(array)) call error_message(951)

        if (shape_present) then
#ifdef USE_DEBUG
            if (num_row <= 0 .or. num_col <= 0) call error_message(952)
#endif
            allocate (array(num_row, num_col), stat=stat)
        else if (source_present) then
            allocate (array, source=source, stat=stat)
        end if

#ifdef USE_DEBUG
        if (stat /= 0) call error_message(955)
#endif
    end subroutine allocate_rank2_logical8

    ! ==========================================================
    ! Pointer Allocations
    ! ==========================================================

    !>
    !> Allocates a rank-1 32-bit integer pointer array of a given size.
    !>
    subroutine allocate_rank1_int32_pointer(array, size)
        implicit none
        !> The pointer array to allocate.
        integer(int32), intent(inout), dimension(:), pointer :: array
        !> The desired size of the array.
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. associated(array)) then
            allocate (array(size))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_int32_pointer

    !>
    !> Allocates a rank-1 64-bit integer pointer array of a given size.
    !>
    subroutine allocate_rank1_int64_pointer(array, size)
        implicit none
        !> The pointer array to allocate.
        integer(int64), intent(inout), dimension(:), pointer :: array
        !> The desired size of the array.
        integer(int64), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. associated(array)) then
            allocate (array(size))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_int64_pointer

    !>
    !> Allocates a rank-1 single precision real pointer array of a given size.
    !>
    subroutine allocate_rank1_real32_pointer(array, size)
        implicit none
        !> The pointer array to allocate.
        real(real32), intent(inout), dimension(:), pointer :: array
        !> The desired size of the array.
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. associated(array)) then
            allocate (array(size))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_real32_pointer

    !>
    !> Allocates a rank-1 double precision real pointer array of a given size.
    !>
    subroutine allocate_rank1_real64_pointer(array, size)
        implicit none
        !> The pointer array to allocate.
        real(real64), intent(inout), dimension(:), pointer :: array
        !> The desired size of the array.
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. associated(array)) then
            allocate (array(size))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_real64_pointer

    !>
    !> Allocates a rank-1 quad precision real pointer array of a given size.
    !>
    subroutine allocate_rank1_real128_pointer(array, size)
        implicit none
        !> The pointer array to allocate.
        real(real128), intent(inout), dimension(:), pointer :: array
        !> The desired size of the array.
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. associated(array)) then
            allocate (array(size))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_real128_pointer

    !>
    !> Allocates a rank-1 logical pointer array of a given size.
    !>
    subroutine allocate_rank1_logical_pointer(array, size)
        implicit none
        !> The pointer array to allocate.
        logical, intent(inout), dimension(:), pointer :: array
        !> The desired size of the array.
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. associated(array)) then
            allocate (array(size))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_logical_pointer

    !>
    !> Allocates a rank-1 32-bit integer pointer array with specified bounds.
    !>
    subroutine allocate_rank1_int32_specify_pointer(array, first, last)
        implicit none
        !> The pointer array to allocate.
        integer(int32), intent(inout), dimension(:), pointer :: array
        !> The lower bound of the array.
        integer(int32), intent(in) :: first
        !> The upper bound of the array.
        integer(int32), intent(in) :: last

        if (first > last) call error_message(952)
        if (.not. associated(array)) then
            allocate (array(first:last))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_int32_specify_pointer

    !>
    !> Allocates a rank-1 64-bit integer pointer array with specified bounds.
    !>
    subroutine allocate_rank1_int64_specify_pointer(array, first, last)
        implicit none
        !> The pointer array to allocate.
        integer(int64), intent(inout), dimension(:), pointer :: array
        !> The lower bound of the array.
        integer(int64), intent(in) :: first
        !> The upper bound of the array.
        integer(int64), intent(in) :: last

        if (first > last) call error_message(952)
        if (.not. associated(array)) then
            allocate (array(first:last))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_int64_specify_pointer

    !>
    !> Allocates a rank-1 single precision real pointer array with specified bounds.
    !>
    subroutine allocate_rank1_real32_specify_pointer(array, first, last)
        implicit none
        !> The pointer array to allocate.
        real(real32), intent(inout), dimension(:), pointer :: array
        !> The lower bound of the array.
        integer(int32), intent(in) :: first
        !> The upper bound of the array.
        integer(int32), intent(in) :: last

        if (first > last) call error_message(952)
        if (.not. associated(array)) then
            allocate (array(first:last))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_real32_specify_pointer

    !>
    !> Allocates a rank-1 double precision real pointer array with specified bounds.
    !>
    subroutine allocate_rank1_real64_specify_pointer(array, first, last)
        implicit none
        !> The pointer array to allocate.
        real(real64), intent(inout), dimension(:), pointer :: array
        !> The lower bound of the array.
        integer(int32), intent(in) :: first
        !> The upper bound of the array.
        integer(int32), intent(in) :: last

        if (first > last) call error_message(952)
        if (.not. associated(array)) then
            allocate (array(first:last))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_real64_specify_pointer

    !>
    !> Allocates a rank-1 quad precision real pointer array with specified bounds.
    !>
    subroutine allocate_rank1_real128_specify_pointer(array, first, last)
        implicit none
        !> The pointer array to allocate.
        real(real128), intent(inout), dimension(:), pointer :: array
        !> The lower bound of the array.
        integer(int32), intent(in) :: first
        !> The upper bound of the array.
        integer(int32), intent(in) :: last

        if (first > last) call error_message(952)
        if (.not. associated(array)) then
            allocate (array(first:last))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_real128_specify_pointer

    !>
    !> Allocates a rank-1 logical pointer array with specified bounds.
    !>
    subroutine allocate_rank1_logical_specify_pointer(array, first, last)
        implicit none
        !> The pointer array to allocate.
        logical(4), intent(inout), dimension(:), pointer :: array
        !> The lower bound of the array.
        integer(int32), intent(in) :: first
        !> The upper bound of the array.
        integer(int32), intent(in) :: last

        if (first > last) call error_message(952)
        if (.not. associated(array)) then
            allocate (array(first:last))
        else
            ! call error_message(953)
        end if
    end subroutine allocate_rank1_logical_specify_pointer

    !>
    !> Allocates a scalar 32-bit integer pointer.
    !>
    subroutine allocate_pointer_int32(iptr)
        implicit none
        !> The scalar pointer to allocate.
        integer(int32), pointer :: iptr

        if (.not. associated(iptr)) then
            allocate (iptr)
        else
            call error_message(955)
        end if
    end subroutine allocate_pointer_int32

    !>
    !> Allocates a scalar 64-bit integer pointer.
    !>
    subroutine allocate_pointer_int64(iptr)
        implicit none
        !> The scalar pointer to allocate.
        integer(int64), pointer :: iptr

        if (.not. associated(iptr)) then
            allocate (iptr)
        else
            call error_message(955)
        end if
    end subroutine allocate_pointer_int64

    !>
    !> Allocates a scalar single precision real pointer.
    !>
    subroutine allocate_pointer_real32(dptr)
        implicit none
        !> The scalar pointer to allocate.
        real(real32), pointer :: dptr

        if (.not. associated(dptr)) then
            allocate (dptr)
        else
            call error_message(955)
        end if
    end subroutine allocate_pointer_real32

    !>
    !> Allocates a scalar double precision real pointer.
    !>
    subroutine allocate_pointer_real64(dptr)
        implicit none
        !> The scalar pointer to allocate.
        real(real64), pointer :: dptr

        if (.not. associated(dptr)) then
            allocate (dptr)
        else
            call error_message(955)
        end if
    end subroutine allocate_pointer_real64

    !>
    !> Allocates a scalar quad precision real pointer.
    !>
    subroutine allocate_pointer_real128(dptr)
        implicit none
        !> The scalar pointer to allocate.
        real(real128), pointer :: dptr

        if (.not. associated(dptr)) then
            allocate (dptr)
        else
            call error_message(955)
        end if
    end subroutine allocate_pointer_real128

end module core_allocate

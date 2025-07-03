module Core_Allocate
    use, intrinsic :: iso_fortran_env
    use :: Core_Error
    implicit none
    private

    public :: Allocate_Array
    public :: Allocate_Pointer

    interface Allocate_Array
        module procedure :: Allocate_Rank1_int8
        module procedure :: Allocate_Rank1_int16
        module procedure :: Allocate_Rank1_int32
        module procedure :: Allocate_Rank1_int64
        module procedure :: Allocate_Rank1_real32
        module procedure :: Allocate_Rank1_real64
        module procedure :: Allocate_Rank1_real128
        module procedure :: Allocate_Rank1_logical1
        module procedure :: Allocate_Rank1_logical4
        module procedure :: Allocate_Rank1_logical8
        module procedure :: Allocate_Rank1_int32_specify
        module procedure :: Allocate_Rank1_int64_specify
        module procedure :: Allocate_Rank1_real32_specify
        module procedure :: Allocate_Rank1_real64_specify
        module procedure :: Allocate_Rank1_real128_specify
        module procedure :: Allocate_Rank1_logical_specify
        module procedure :: Allocate_Rank2_int8
        module procedure :: Allocate_Rank2_int16
        module procedure :: Allocate_Rank2_int32
        module procedure :: Allocate_Rank2_int64
        module procedure :: Allocate_Rank2_real32
        module procedure :: Allocate_Rank2_real64
        module procedure :: Allocate_Rank2_real128
        module procedure :: Allocate_Rank2_logical1
        module procedure :: Allocate_Rank2_logical4
        module procedure :: Allocate_Rank2_logical8
        module procedure :: Allocate_Rank1_int32_Pointer
        module procedure :: Allocate_Rank1_int64_Pointer
        module procedure :: Allocate_Rank1_real32_Pointer
        module procedure :: Allocate_Rank1_real64_Pointer
        module procedure :: Allocate_Rank1_real128_Pointer
        module procedure :: Allocate_Rank1_logical_Pointer
        module procedure :: Allocate_Rank1_int32_specify_Pointer
        module procedure :: Allocate_Rank1_int64_specify_Pointer
        module procedure :: Allocate_Rank1_real32_specify_Pointer
        module procedure :: Allocate_Rank1_real64_specify_Pointer
        module procedure :: Allocate_Rank1_real128_specify_Pointer
        module procedure :: Allocate_Rank1_logical_specify_Pointer
    end interface

    interface Allocate_Pointer
        module procedure :: Allocate_Pointer_int32
        module procedure :: Allocate_Pointer_int64
        module procedure :: Allocate_Pointer_real32
        module procedure :: Allocate_Pointer_real64
        module procedure :: Allocate_Pointer_real128
    end interface

contains

    ! Rank-1 配列の割り当て
    subroutine Allocate_Rank1_int8(array, size)
        implicit none
        integer(int8), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. allocated(array)) then
            allocate (array(size))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_int8

    ! Rank-1 配列の割り当て
    subroutine Allocate_Rank1_int16(array, size)
        implicit none
        integer(int16), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. allocated(array)) then
            allocate (array(size))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_int16

    subroutine Allocate_Rank1_int32(array, size)
        implicit none
        integer(int32), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. allocated(array)) then
            allocate (array(size))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_int32

    subroutine Allocate_Rank1_int64(array, size)
        implicit none
        integer(int64), intent(inout), allocatable :: array(:)
        integer(int64), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. allocated(array)) then
            allocate (array(size))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_int64

    subroutine Allocate_Rank1_real32(array, size)
        implicit none
        real(real32), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. allocated(array)) then
            allocate (array(size))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_real32

    subroutine Allocate_Rank1_real64(array, size)
        implicit none
        real(real64), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. allocated(array)) then
            allocate (array(size))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_real64

    subroutine Allocate_Rank1_real128(array, size)
        implicit none
        real(real128), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. allocated(array)) then
            allocate (array(size))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_real128

    subroutine Allocate_Rank1_logical1(array, size)
        implicit none
        logical(1), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. allocated(array)) then
            allocate (array(size))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_logical1

    subroutine Allocate_Rank1_logical4(array, size)
        implicit none
        logical(4), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. allocated(array)) then
            allocate (array(size))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_logical4

    subroutine Allocate_Rank1_logical8(array, size)
        implicit none
        logical(8), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. allocated(array)) then
            allocate (array(size))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_logical8

    subroutine Allocate_Rank1_int32_specify(array, first, last)
        implicit none
        integer(int32), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: first, last

        if (first > last) call error_message(952)
        if (.not. allocated(array)) then
            allocate (array(first:last))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_int32_specify

    subroutine Allocate_Rank1_int64_specify(array, first, last)
        implicit none
        integer(int64), intent(inout), allocatable :: array(:)
        integer(int64), intent(in) :: first, last

        if (first > last) call error_message(952)
        if (.not. allocated(array)) then
            allocate (array(first:last))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_int64_specify

    subroutine Allocate_Rank1_real32_specify(array, first, last)
        implicit none
        real(real32), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: first, last

        if (first > last) call error_message(952)
        if (.not. allocated(array)) then
            allocate (array(first:last))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_real32_specify

    subroutine Allocate_Rank1_real64_specify(array, first, last)
        implicit none
        real(real64), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: first, last

        if (first > last) call error_message(952)
        if (.not. allocated(array)) then
            allocate (array(first:last))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_real64_specify

    subroutine Allocate_Rank1_real128_specify(array, first, last)
        implicit none
        real(real128), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: first, last

        if (first > last) call error_message(952)
        if (.not. allocated(array)) then
            allocate (array(first:last))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_real128_specify

    subroutine Allocate_Rank1_logical_specify(array, first, last)
        implicit none
        logical(4), intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: first, last

        if (first > last) call error_message(952)
        if (.not. allocated(array)) then
            allocate (array(first:last))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_logical_specify

    subroutine Allocate_Rank1_int32_Pointer(array, size)
        implicit none
        integer(int32), intent(inout), dimension(:), pointer :: array
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. associated(array)) then
            allocate (array(size))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_int32_Pointer

    subroutine Allocate_Rank1_int64_Pointer(array, size)
        implicit none
        integer(int64), intent(inout), dimension(:), pointer :: array
        integer(int64), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. associated(array)) then
            allocate (array(size))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_int64_Pointer

    subroutine Allocate_Rank1_real32_Pointer(array, size)
        implicit none
        real(real32), intent(inout), dimension(:), pointer :: array
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. associated(array)) then
            allocate (array(size))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_real32_Pointer

    subroutine Allocate_Rank1_real64_Pointer(array, size)
        implicit none
        real(real64), intent(inout), dimension(:), pointer :: array
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. associated(array)) then
            allocate (array(size))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_real64_Pointer

    subroutine Allocate_Rank1_real128_Pointer(array, size)
        implicit none
        real(real128), intent(inout), dimension(:), pointer :: array
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. associated(array)) then
            allocate (array(size))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_real128_Pointer

    subroutine Allocate_Rank1_logical_Pointer(array, size)
        implicit none
        logical, intent(inout), dimension(:), pointer :: array
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. associated(array)) then
            allocate (array(size))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_logical_Pointer

    subroutine Allocate_Rank1_int32_specify_Pointer(array, first, last)
        implicit none
        integer(int32), intent(inout), dimension(:), pointer :: array
        integer(int32), intent(in) :: first, last

        if (first > last) call error_message(952)
        if (.not. associated(array)) then
            allocate (array(first:last))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_int32_specify_Pointer

    subroutine Allocate_Rank1_int64_specify_Pointer(array, first, last)
        implicit none
        integer(int64), intent(inout), dimension(:), pointer :: array
        integer(int64), intent(in) :: first, last

        if (first > last) call error_message(952)
        if (.not. associated(array)) then
            allocate (array(first:last))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_int64_specify_Pointer

    subroutine Allocate_Rank1_real32_specify_Pointer(array, first, last)
        implicit none
        real(real32), intent(inout), dimension(:), pointer :: array
        integer(int32), intent(in) :: first, last

        if (first > last) call error_message(952)
        if (.not. associated(array)) then
            allocate (array(first:last))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_real32_specify_Pointer

    subroutine Allocate_Rank1_real64_specify_Pointer(array, first, last)
        implicit none
        real(real64), intent(inout), dimension(:), pointer :: array
        integer(int32), intent(in) :: first, last

        if (first > last) call error_message(952)
        if (.not. associated(array)) then
            allocate (array(first:last))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_real64_specify_Pointer

    subroutine Allocate_Rank1_real128_specify_Pointer(array, first, last)
        implicit none
        real(real128), intent(inout), dimension(:), pointer :: array
        integer(int32), intent(in) :: first, last

        if (first > last) call error_message(952)
        if (.not. associated(array)) then
            allocate (array(first:last))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_real128_specify_Pointer

    subroutine Allocate_Rank1_logical_specify_Pointer(array, first, last)
        implicit none
        logical(4), intent(inout), dimension(:), pointer :: array
        integer(int32), intent(in) :: first, last

        if (first > last) call error_message(952)
        if (.not. associated(array)) then
            allocate (array(first:last))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_logical_specify_Pointer

    ! Rank-2 配列の割り当て
    subroutine Allocate_Rank2_int8(array, size1, size2)
        implicit none
        integer(int8), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: size1, size2

        if (size1 <= 0 .or. size2 <= 0) call error_message(952)
        if (.not. allocated(array)) then
            allocate (array(size1, size2))
        else
            call error_message(954)
        end if
    end subroutine Allocate_Rank2_int8

    subroutine Allocate_Rank2_int16(array, size1, size2)
        implicit none
        integer(int16), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: size1, size2

        if (size1 <= 0 .or. size2 <= 0) call error_message(952)
        if (.not. allocated(array)) then
            allocate (array(size1, size2))
        else
            call error_message(954)
        end if
    end subroutine Allocate_Rank2_int16

    subroutine Allocate_Rank2_int32(array, size1, size2)
        implicit none
        integer(int32), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: size1, size2

        if (size1 <= 0 .or. size2 <= 0) call error_message(952)
        if (.not. allocated(array)) then
            allocate (array(size1, size2))
        else
            call error_message(954)
        end if
    end subroutine Allocate_Rank2_int32

    subroutine Allocate_Rank2_int64(array, size1, size2)
        implicit none
        integer(int64), intent(inout), allocatable :: array(:, :)
        integer(int64), intent(in) :: size1, size2

        if (size1 <= 0 .or. size2 <= 0) call error_message(952)
        if (.not. allocated(array)) then
            allocate (array(size1, size2))
        else
            call error_message(954)
        end if
    end subroutine Allocate_Rank2_int64

    subroutine Allocate_Rank2_real32(array, size1, size2)
        implicit none
        real(real32), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: size1, size2

        if (size1 <= 0 .or. size2 <= 0) call error_message(952)
        if (.not. allocated(array)) then
            allocate (array(size1, size2))
        else
            call error_message(954)
        end if
    end subroutine Allocate_Rank2_real32

    subroutine Allocate_Rank2_real64(array, size1, size2)
        implicit none
        real(real64), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: size1, size2

        if (size1 <= 0 .or. size2 <= 0) call error_message(952)
        if (.not. allocated(array)) then
            allocate (array(size1, size2))
        else
            call error_message(954)
        end if
    end subroutine Allocate_Rank2_real64

    subroutine Allocate_Rank2_real128(array, size1, size2)
        implicit none
        real(real128), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: size1, size2

        if (size1 <= 0 .or. size2 <= 0) call error_message(952)
        if (.not. allocated(array)) then
            allocate (array(size1, size2))
        else
            call error_message(954)
        end if
    end subroutine Allocate_Rank2_real128

    subroutine Allocate_Rank2_logical1(array, size1, size2)
        implicit none
        logical(logical8), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: size1, size2

        if (size1 <= 0 .or. size2 <= 0) call error_message(952)
        if (.not. allocated(array)) then
            allocate (array(size1, size2))
        else
            call error_message(954)
        end if
    end subroutine Allocate_Rank2_logical1

    subroutine Allocate_Rank2_logical4(array, size1, size2)
        implicit none
        logical(logical32), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: size1, size2

        if (size1 <= 0 .or. size2 <= 0) call error_message(952)
        if (.not. allocated(array)) then
            allocate (array(size1, size2))
        else
            call error_message(954)
        end if
    end subroutine Allocate_Rank2_logical4

    subroutine Allocate_Rank2_logical8(array, size1, size2)
        implicit none
        logical(logical64), intent(inout), allocatable :: array(:, :)
        integer(int32), intent(in) :: size1, size2

        if (size1 <= 0 .or. size2 <= 0) call error_message(952)
        if (.not. allocated(array)) then
            allocate (array(size1, size2))
        else
            call error_message(954)
        end if
    end subroutine Allocate_Rank2_logical8

    ! ポインタ用の割り当て
    subroutine Allocate_Pointer_int32(iptr)
        implicit none
        integer(int32), pointer :: iptr

        if (.not. associated(iptr)) then
            allocate (iptr)
        else
            call error_message(955)
        end if
    end subroutine Allocate_Pointer_int32

    subroutine Allocate_Pointer_int64(iptr)
        implicit none
        integer(int64), pointer :: iptr

        if (.not. associated(iptr)) then
            allocate (iptr)
        else
            call error_message(955)
        end if
    end subroutine Allocate_Pointer_int64

    subroutine Allocate_Pointer_real32(dptr)
        implicit none
        real(real32), pointer :: dptr

        if (.not. associated(dptr)) then
            allocate (dptr)
        else
            call error_message(955)
        end if
    end subroutine Allocate_Pointer_real32

    subroutine Allocate_Pointer_real64(dptr)
        implicit none
        real(real64), pointer :: dptr

        if (.not. associated(dptr)) then
            allocate (dptr)
        else
            call error_message(955)
        end if
    end subroutine Allocate_Pointer_real64

    subroutine Allocate_Pointer_real128(dptr)
        implicit none
        real(real128), pointer :: dptr

        if (.not. associated(dptr)) then
            allocate (dptr)
        else
            call error_message(955)
        end if
    end subroutine Allocate_Pointer_real128
end module Core_Allocate

module Allocate_Allocate
    use, intrinsic :: iso_fortran_env
    use :: error
    ! use :: Types
    implicit none
    private

    interface Allocate_Array
        procedure :: Allocate_Rank1_int32
        procedure :: Allocate_Rank1_int64
        procedure :: Allocate_Rank1_real32
        procedure :: Allocate_Rank1_real64
        procedure :: Allocate_Rank1_real128
        procedure :: Allocate_Rank1_logical
        procedure :: Allocate_Rank2_int32
        procedure :: Allocate_Rank2_int64
        procedure :: Allocate_Rank2_real32
        procedure :: Allocate_Rank2_real64
        procedure :: Allocate_Rank2_real128
        procedure :: Allocate_Rank1_int32_specify
        procedure :: Allocate_Rank1_int64_specify
        procedure :: Allocate_Rank1_real32_specify
        procedure :: Allocate_Rank1_real64_specify
        procedure :: Allocate_Rank1_real128_specify
        procedure :: Allocate_Rank1_logical_specify
    end interface

    interface Allocate_Pointer
        procedure :: Allocate_Pointer_int32
        procedure :: Allocate_Pointer_int64
        procedure :: Allocate_Pointer_real32
        procedure :: Allocate_Pointer_real64
        procedure :: Allocate_Pointer_real128
    end interface

    public :: Allocate_Array
    public :: Allocate_Pointer
    ! public :: Duplicate_CRS

contains

    ! Rank-1 配列の割り当て
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

    subroutine Allocate_Rank1_logical(array, size)
        implicit none
        logical, intent(inout), allocatable :: array(:)
        integer(int32), intent(in) :: size

        if (size <= 0) call error_message(951)
        if (.not. allocated(array)) then
            allocate (array(size))
        else
            call error_message(953)
        end if
    end subroutine Allocate_Rank1_logical

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

    ! Rank-2 配列の割り当て
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

    ! subroutine Duplicate_CRS(A, B)
    !     implicit none
    !     type(CRS), intent(in) :: A
    !     type(CRS), intent(inout) :: B

    !     B%nnz = A%nnz
    !     if (.not. allocated(B%Ptr)) then
    !         allocate (B%Ptr, source=A%Ptr)
    !     else
    !         ! call error_message(951)
    !     end if
    !     if (.not. allocated(B%Ind)) then
    !         allocate (B%Ind, source=A%Ind)
    !     else
    !         ! call error_message(951)
    !     end if
    !     if (.not. allocated(B%val)) then
    !         allocate (B%val, source=A%Val)
    !     else
    !         ! call error_message(951)
    !     end if
    !     B%val = 0.0d0

    ! end subroutine Duplicate_CRS
end module Allocate_Allocate

module Allocate
    use, intrinsic :: iso_fortran_env, only : int32, real64
    use :: error
    use :: Types
    implicit none
    private

    interface Allocate_Vector
        procedure :: Check_Allocate_rank1_int32
        procedure :: Check_Allocate_rank1_real64
        procedure :: Check_Allocate_rank1_logical
        procedure :: Check_Allocate_rank1_int32_specify
        procedure :: Check_Allocate_rank1_real64_specify
    end interface

    interface Allocate_Matrix
        procedure :: Check_Allocate_rank2_int32
        procedure :: Check_Allocate_rank2_real64
    end interface

    interface Allocate_Pointer
        procedure :: Check_Allocate_Pointer_real64
        procedure :: Check_Allocate_Pointer_int32
    end interface

    public :: Allocate_Vector
    public :: Allocate_Matrix
    public :: Allocate_Pointer
    public :: Duplicate_CRS

    contains

    subroutine Check_Allocate_rank1_int32(iar, ar_size)
        implicit none
        integer(int32), intent(in)                 :: ar_size
        integer(int32), intent(inout), allocatable :: iar(:)

        if (ar_size <= 0) call error_message(951)

        if (.not. allocated(iar)) then
            allocate(iar(ar_size))
        else
            call error_message(953)
        end if

    end subroutine Check_Allocate_rank1_int32

    subroutine Check_Allocate_rank1_real64(dar, ar_size)
        implicit none
        integer(int32), intent(in)                 :: ar_size
        real(real64),   intent(inout), allocatable :: dar(:)

        if (ar_size <= 0) call error_message(951)

        if (.not. allocated(dar)) then
            allocate(dar(ar_size))
        else
            call error_message(953)
        end if

    end subroutine Check_Allocate_rank1_real64

    subroutine Check_Allocate_rank1_logical(lar, ar_size)
        implicit none
        integer(int32), intent(in)                 :: ar_size
        logical,        intent(inout), allocatable :: lar(:)

        if (ar_size <= 0) call error_message(951)

        if (.not. allocated(lar)) then
            allocate(lar(ar_size))
        else
            call error_message(953)
        end if

    end subroutine Check_Allocate_rank1_logical

    subroutine Check_Allocate_rank1_int32_specify(iar, ar_first, ar_last)
        implicit none
        integer(int32), intent(in)                 :: ar_first, ar_last
        integer(int32), intent(inout), allocatable :: iar(:)

        if (.not. allocated(iar)) then
            allocate(iar(ar_first:ar_last))
        else
            call error_message(953)
        end if

    end subroutine Check_Allocate_rank1_int32_specify

    subroutine Check_Allocate_rank1_real64_specify(dar, ar_first, ar_last)
        implicit none
        integer(int32), intent(in)                 :: ar_first, ar_last
        real(real64),   intent(inout), allocatable :: dar(:)

        if (.not. allocated(dar)) then
            allocate(dar(ar_first:ar_last))
        else
            call error_message(953)
        end if

    end subroutine Check_Allocate_rank1_real64_specify

    subroutine Check_Allocate_rank2_int32(imt, mt_size_1, mt_size_2)
        implicit none
        integer(int32), intent(in)                 :: mt_size_1, mt_size_2
        integer(int32), intent(inout), allocatable :: imt(:,:)

        if (mt_size_1 <= 0 .or. mt_size_2 <= 0) call error_message(952)

        if (.not. allocated(imt)) then
            allocate(imt(mt_size_1, mt_size_2))
        else
            call error_message(954)
        end if

    end subroutine Check_Allocate_rank2_int32

    subroutine Check_Allocate_rank2_real64(dmt, mt_size_1, mt_size_2)
        implicit none
        integer(int32), intent(in) :: mt_size_1, mt_size_2
        real(real64), intent(inout), allocatable :: dmt(:,:)

        if (mt_size_1 <= 0 .or. mt_size_2 <= 0) call error_message(952)
        if (.not. allocated(dmt)) then
        allocate(dmt(mt_size_1, mt_size_2))
        else
        call error_message(954)
        end if

    end subroutine Check_Allocate_rank2_real64

    subroutine Check_Allocate_Pointer_int32(iptr)
        implicit none
        integer(int32), pointer :: iptr
        
        if (.not. associated(iptr)) then
            allocate(iptr)
        else
            call error_message(955)
        end if
    end subroutine Check_Allocate_Pointer_int32

    subroutine Check_Allocate_Pointer_real64(dptr)
        implicit none
        real(real64), pointer :: dptr

        if (.not. associated(dptr)) then
            allocate(dptr)
        else
            call error_message(955)
        end if
    end subroutine Check_Allocate_Pointer_real64

    subroutine Duplicate_CRS(A, B)
        implicit none
        type(CRS), intent(in)    :: A
        type(CRS), intent(inout) :: B

        B%nnz = A%nnz
        if (.not. allocated(B%Ptr)) then
            allocate(B%Ptr, source=A%Ptr)
        else
            ! call error_message(951)
        end if
        if (.not. allocated(B%Ind)) then
            allocate(B%Ind, source=A%Ind)
        else
            ! call error_message(951)
        end if
        if (.not. allocated(B%val)) then
            allocate(B%val, source=A%Val)
        else
            ! call error_message(951)
        end if
        B%val = 0.0d0

    end subroutine Duplicate_CRS
end module Allocate
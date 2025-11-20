module core_check_length
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    public :: check_match_length

    interface check_match_length
        module procedure :: check_match_length_real32
        module procedure :: check_match_length_real64
        module procedure :: check_match_length_real128
        module procedure :: check_match_length_int8
        module procedure :: check_match_length_int16
        module procedure :: check_match_length_int32
        module procedure :: check_match_length_int64
    end interface
contains

    !>
    !> Checks if two single precision arrays have the same size.
    subroutine check_match_length_real32(a, b, routine_name)
        implicit none
        !> The first array.
        real(real32), intent(in) :: a(:)
        !> The second array.
        real(real32), intent(in) :: b(:)
        !> The name of the calling routine for error messages.
        character(len=*), intent(in) :: routine_name

        if (size(a) /= size(b)) then
            write (*, '(A,A,A)') "ERROR in ", trim(routine_name), ": Array sizes do not match."
            error stop 1
        end if
    end subroutine check_match_length_real32

    !>
    !> Checks if two double precision arrays have the same size.
    subroutine check_match_length_real64(a, b, routine_name)
        implicit none
        !> The first array.
        real(real64), intent(in) :: a(:)
        !> The second array.
        real(real64), intent(in) :: b(:)
        !> The name of the calling routine for error messages.
        character(len=*), intent(in) :: routine_name

        if (size(a) /= size(b)) then
            write (*, '(A,A,A)') "ERROR in ", trim(routine_name), ": Array sizes do not match."
            error stop 1
        end if
    end subroutine check_match_length_real64

    !>
    !> Checks if two quadruple precision arrays have the same size.
    subroutine check_match_length_real128(a, b, routine_name)
        implicit none
        !> The first array.
        real(real128), intent(in) :: a(:)
        !> The second array.
        real(real128), intent(in) :: b(:)
        !> The name of the calling routine for error messages.
        character(len=*), intent(in) :: routine_name

        if (size(a) /= size(b)) then
            write (*, '(A,A,A)') "ERROR in ", trim(routine_name), ": Array sizes do not match."
            error stop 1
        end if
    end subroutine check_match_length_real128

    !>
    !> Checks if two integer arrays have the same size.
    subroutine check_match_length_int8(a, b, routine_name)
        implicit none
        !> The first array.
        integer(int8), intent(in) :: a(:)
        !> The second array.
        integer(int8), intent(in) :: b(:)
        !> The name of the calling routine for error messages.
        character(len=*), intent(in) :: routine_name

        if (size(a) /= size(b)) then
            write (*, '(A,A,A)') "ERROR in ", trim(routine_name), ": Array sizes do not match."
            error stop 1
        end if
    end subroutine check_match_length_int8
    !>
    !> Checks if two integer arrays have the same size.
    subroutine check_match_length_int16(a, b, routine_name)
        implicit none
        !> The first array.
        integer(int16), intent(in) :: a(:)
        !> The second array.
        integer(int16), intent(in) :: b(:)
        !> The name of the calling routine for error messages.
        character(len=*), intent(in) :: routine_name

        if (size(a) /= size(b)) then
            write (*, '(A,A,A)') "ERROR in ", trim(routine_name), ": Array sizes do not match."
            error stop 1
        end if
    end subroutine check_match_length_int16
    !>
    !> Checks if two integer arrays have the same size.
    subroutine check_match_length_int32(a, b, routine_name)
        implicit none
        !> The first array.
        integer(int32), intent(in) :: a(:)
        !> The second array.
        integer(int32), intent(in) :: b(:)
        !> The name of the calling routine for error messages.
        character(len=*), intent(in) :: routine_name

        if (size(a) /= size(b)) then
            write (*, '(A,A,A)') "ERROR in ", trim(routine_name), ": Array sizes do not match."
            error stop 1
        end if
    end subroutine check_match_length_int32
    !>
    !> Checks if two integer arrays have the same size.
    subroutine check_match_length_int64(a, b, routine_name)
        implicit none
        !> The first array.
        integer(int64), intent(in) :: a(:)
        !> The second array.
        integer(int64), intent(in) :: b(:)
        !> The name of the calling routine for error messages.
        character(len=*), intent(in) :: routine_name

        if (size(a) /= size(b)) then
            write (*, '(A,A,A)') "ERROR in ", trim(routine_name), ": Array sizes do not match."
            error stop 1
        end if
    end subroutine check_match_length_int64

end module core_check_length

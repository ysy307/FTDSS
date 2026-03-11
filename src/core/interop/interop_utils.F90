module core_interop_utils
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer, c_char, c_null_char

    implicit none
    public :: c_ptr_to_string

contains

    function c_ptr_to_string(c_ptr_in) result(f_string)
        use, intrinsic :: iso_c_binding
        implicit none

        type(c_ptr), value, intent(in) :: c_ptr_in
        character(:), allocatable :: f_string

        character(kind=c_char), pointer :: c_char_array_ptr(:)
        integer(int32) :: i, length
        ! Upper bound size for scanning (2^30)
        integer(int32), parameter :: large_size = 2**30

        ! --- 1. Determine the string length ---
        if (.not. c_associated(c_ptr_in)) then
            f_string = ""
            return
        end if

        ! Map the C pointer to a large Fortran pointer array
        call c_f_pointer(c_ptr_in, c_char_array_ptr, [large_size])

        ! Scan the array to find the null terminator
        length = 0
        do i = 1, large_size
            if (c_char_array_ptr(i) == c_null_char) then
                length = i - 1
                exit
            end if
        end do

        ! --- 2. Allocate Fortran string with the determined length ---
        allocate (character(len=length) :: f_string)
        if (length == 0) return

        ! --- 3. Copy characters from the array ---
        do i = 1, length
            f_string(i:i) = c_char_array_ptr(i)
        end do

    end function c_ptr_to_string
end module core_interop_utils

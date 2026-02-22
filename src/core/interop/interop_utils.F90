module core_c_utils
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer, c_char, c_null_char

    implicit none
    public :: c_ptr_to_string

contains

! in module core/interop/interop_utils.F90

    function c_ptr_to_string(c_ptr_in) result(f_string)
        use, intrinsic :: iso_c_binding
        implicit none

        type(c_ptr), value, intent(in) :: c_ptr_in
        character(:), allocatable :: f_string

        character(kind=c_char), pointer :: c_char_array_ptr(:)
        integer(int32) :: i, length
        ! 非常に大きなサイズ（ここでは2^30）を指定
        integer(int32), parameter :: large_size = 2**30

        ! --- 1. 長さを決定する ---
        if (.not. c_associated(c_ptr_in)) then
            f_string = ""
            return
        end if

        ! Cポインタを、仮の巨大なFortranポインタ配列にマップする
        call c_f_pointer(c_ptr_in, c_char_array_ptr, [large_size])

        ! 配列を走査してヌル文字(c_null_char)を探す
        length = 0
        do i = 1, large_size
            if (c_char_array_ptr(i) == c_null_char) then
                length = i - 1
                exit
            end if
        end do

        ! --- 2. 決定した長さでFortran文字列を確保 ---
        allocate (character(len=length) :: f_string)
        if (length == 0) return

        ! --- 3. 配列全体を一度に代入（高速） ---
        ! Fortranではポインタ配列を直接文字列に代入できる
        do i = 1, length
            f_string(i:i) = c_char_array_ptr(i)
        end do

    end function c_ptr_to_string
end module core_c_utils

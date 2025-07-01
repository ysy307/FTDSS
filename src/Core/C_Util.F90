module Core_C_Util
    use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer, c_char, c_null_char
    implicit none
    public :: c_ptr_to_string

contains

    function c_ptr_to_string(ptr) result(fstr)
        type(c_ptr), intent(in) :: ptr
        character(len=:), allocatable :: fstr
        character(kind=c_char), pointer :: cstr(:)
        integer :: len, i

        ! 1) C ポインタを 256 要素の CHARACTER(C_CHAR) 配列にマップ
        call c_f_pointer(ptr, cstr, [256])

        ! 2) ヌル終端までカウント
        len = 0
        do while (len < 256 .and. cstr(len + 1) /= c_null_char)
            len = len + 1
        end do

        ! 3) allocatable 文字列を len で確保
        allocate (character(len=len) :: fstr)
        ! 4) １文字ずつコピー
        do i = 1, len
            fstr(i:i) = cstr(i)
        end do
    end function c_ptr_to_string

end module Core_C_Util

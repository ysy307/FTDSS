module core_string_utils
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    private

    public :: join

contains

    function join(strings, delimiter) result(key)
        character(*), intent(in) :: strings(:)
        character(*), intent(in), optional :: delimiter
        character(:), allocatable :: key

        integer(int32) :: i, n, total_len, current_pos
        integer(int32) :: length_delimiter, length_strings
        character(len=1) :: write_delimiter

        if (present(delimiter)) then
            length_delimiter = 0
            length_delimiter = len_trim(delimiter)
            write_delimiter = trim(adjustl(delimiter))
        else
            length_delimiter = 1
            write_delimiter = "."
        end if

        n = size(strings)
        if (n == 0) then
            key = ""
            return
        end if

        ! 1. 連結後の全体の長さを計算する
        total_len = len_trim(strings(1))
        do i = 2, n
            total_len = total_len + length_delimiter + len_trim(strings(i))
        end do

        ! 2. 計算した長さでメモリを一度だけ確保する
        allocate (character(len=total_len) :: key)

        ! 3. 確保したメモリに文字列を直接書き込んでいく
        current_pos = 1

        length_strings = len_trim(strings(1))
        key(current_pos:current_pos + length_strings - 1) = trim(adjustl(strings(1)))
        current_pos = current_pos + length_strings

        ! 2番目以降の要素
        do i = 2, n
            key(current_pos:current_pos + length_delimiter - 1) = write_delimiter
            current_pos = current_pos + length_delimiter

            length_strings = len_trim(strings(i))
            if (length_strings == 0) cycle ! 空の文字列はスキップ

            key(current_pos:current_pos + length_strings - 1) = trim(adjustl(strings(i)))
            current_pos = current_pos + length_strings
        end do

    end function join
end module

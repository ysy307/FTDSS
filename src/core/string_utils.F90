module core_string_utils
    use, intrinsic :: iso_fortran_env, only: int32
    use :: core_allocate, only:allocate_array
    implicit none
    private

    public :: join
    public :: filter

    interface filter
        module procedure :: filter_character_array
    end interface

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

    !+
    ! 概要:
    !   入力文字配列(input_array)から、有効な文字列リスト(valid_list)に
    !   含まれる要素だけを抽出し、新しい配列(filtered_array)に格納する。
    !-
    subroutine filter_character_array(input_array, valid_list, filtered_array)
        implicit none
        ! --- 引数 ---
        ! IN: フィルタリング対象の配列
        character(*), intent(in) :: input_array(:)
        ! IN: 有効な文字列のリスト
        character(*), intent(in) :: valid_list(:)
        ! OUT: フィルタリング結果を格納する配列
        character(:), allocatable, intent(inout) :: filtered_array(:)

        ! --- ローカル変数 ---
        integer(int32) :: i
        character(len=len(input_array(1))), allocatable :: packed_array(:)
        logical, allocatable :: mask(:)

        ! --- 処理 ---
        if (size(input_array) == 0) then
            if (allocated(filtered_array)) deallocate (filtered_array)
            allocate (character(len=0) :: filtered_array(0))
            return
        end if

        allocate (mask(size(input_array)))

        ! input_arrayの各要素がvalid_listに存在するかチェックし、マスクを作成
        mask = .false.
        do i = 1, size(input_array)
            mask(i) = any(valid_list(:) == trim(adjustl(input_array(i))))
        end do

        ! マスクを使って有効な要素だけを抽出
        packed_array = pack(input_array, mask)

        ! 結果を出力引数にコピー
        ! (source= を使うことで、文字長も自動で合わせてくれる)
        allocate (filtered_array, source=packed_array)

        ! ローカル配列の解放
        deallocate (mask, packed_array)

    end subroutine filter_character_array
end module core_string_utils

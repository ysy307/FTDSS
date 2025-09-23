module core_string_utils
    use, intrinsic :: iso_fortran_env, only: int32
    use :: stdlib_strings, only:strip
    use :: core_constants
    use :: core_allocate, only:allocate_array
    implicit none
    private

    public :: join
    public :: filter
    public :: modify_path_format
    public :: get_bc_type_from_string

    interface filter
        module procedure :: filter_character_array
    end interface

contains

    function join(strings, delimiter) result(key)
        implicit none
        character(*), intent(in) :: strings(:)
        character(*), intent(in), optional :: delimiter
        character(:), allocatable :: key

        integer :: i, n, total_len, current_pos
        integer :: length_strings
        character(:), allocatable :: effective_delimiter
        integer :: length_delimiter
        logical :: is_first_element

        ! 使用する区切り文字を決定する
        if (present(delimiter)) then
            effective_delimiter = strip(delimiter)
            ! 区切り文字が空白のみの場合はデフォルトの "." を使用する
            if (len(effective_delimiter) == 0) then
                effective_delimiter = "."
            end if
        else
            effective_delimiter = "."
        end if
        length_delimiter = len(effective_delimiter)

        n = size(strings)
        if (n == 0) then
            key = ""
            return
        end if

        ! 1. 連結後の全体の長さを計算する (修正)
        !    - 中身のある文字列だけを数える
        total_len = 0
        is_first_element = .true.
        do i = 1, n
            length_strings = len_trim(strings(i))
            ! 中身が空でない文字列のみを処理の対象とする
            if (length_strings > 0) then
                if (is_first_element) then
                    ! 最初の有効な要素の場合、その長さだけを加算
                    total_len = length_strings
                    is_first_element = .false.
                else
                    ! 2番目以降の有効な要素の場合、区切り文字の長さと文字列の長さを加算
                    total_len = total_len + length_delimiter + length_strings
                end if
            end if
        end do

        ! 全ての要素が空だった場合
        if (total_len == 0) then
            key = ""
            return
        end if

        ! 2. 計算した長さでメモリを一度だけ確保する
        allocate (character(len=total_len) :: key)

        ! 3. 確保したメモリに文字列を直接書き込んでいく (修正)
        current_pos = 1
        is_first_element = .true.
        do i = 1, n
            length_strings = len_trim(strings(i))
            if (length_strings > 0) then
                if (is_first_element) then
                    is_first_element = .false.
                else
                    ! 2番目以降の有効な要素の前に区切り文字を書き込む
                    key(current_pos:current_pos + length_delimiter - 1) = effective_delimiter
                    current_pos = current_pos + length_delimiter
                end if

                ! 文字列本体を書き込む
                key(current_pos:current_pos + length_strings - 1) = strip(strings(i))
                current_pos = current_pos + length_strings
            end if
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
        character(64), allocatable :: packed_array(:)
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
            mask(i) = any(valid_list(:) == strip(input_array(i)))
        end do

        ! マスクを使って有効な要素だけを抽出
        packed_array = pack(input_array, mask)

        ! 結果を出力引数にコピー
        ! (source= を使うことで、文字長も自動で合わせてくれる)
        if (allocated(filtered_array)) deallocate (filtered_array)
        allocate (filtered_array, source=packed_array)

        ! ローカル配列の解放
        deallocate (mask, packed_array)

    end subroutine filter_character_array

    subroutine modify_path_format(path)
        implicit none
        character(len=:), allocatable, intent(inout) :: path
        integer :: i

        ! バックスラッシュをフォワードスラッシュに置換
        do i = 1, len(path)
            if (path(i:i) == '\') then
                path(i:i) = '/'
            end if
        end do

        ! パスが空でなく、かつスラッシュで終わらない場合にスラッシュを追加
        if (len_trim(path) > 0 .and. path(len_trim(path):len_trim(path)) /= "/") then
            path = trim(path)//"/"
        end if
    end subroutine modify_path_format

    pure function get_bc_type_from_string(str, physics_type_id) result(bc_type)
        implicit none
        character(*), intent(in) :: str
        integer(int32), intent(in) :: physics_type_id
        integer(int32) :: bc_type

        select case (physics_type_id)
        case (PHYSICS_TYPE_THERMAL)
            select case (strip(str))
            case ("dirichlet")
                bc_type = THERMAL_BC_DIRICHLET
            case ("neumann")
                bc_type = THERMAL_BC_NEUMANN
            case ("flux")
                bc_type = THERMAL_BC_FLUX
            case ("robin")
                bc_type = THERMAL_BC_ROBIN
            case ("convective")
                bc_type = THERMAL_BC_CONVECTIVE
            case ("radiation")
                bc_type = THERMAL_BC_RADIATION
            case ("adiabatic")
                bc_type = THERMAL_BC_ADIABATIC
            case ("free")
                bc_type = THERMAL_BC_FREE
            case default
                bc_type = -1
            end select
        case (PHYSICS_TYPE_HYDRAULIC)
            select case (strip(str))
            case ("dirichlet")
                bc_type = HYDRAULIC_BC_DIRICHLET
            case ("neumann")
                bc_type = HYDRAULIC_BC_NEUMANN
            case ("flux")
                bc_type = HYDRAULIC_BC_FLUX
            case ("impermeable")
                bc_type = HYDRAULIC_BC_IMPERMEABLE
            case ("seepage")
                bc_type = HYDRAULIC_BC_SEEPAGE
            case default
                bc_type = -1
            end select
        end select
    end function get_bc_type_from_string

end module core_string_utils

module core_check_range
    use, intrinsic :: iso_fortran_env
    implicit none
    private ! モジュール内の要素をデフォルトで非公開にする

    ! publicにするインターフェースと定数を指定
    public :: value_in_range

    ! ジェネリックインターフェースの定義
    interface value_in_range
        module procedure :: value_in_range_int8
        module procedure :: value_in_range_int16
        module procedure :: value_in_range_int32
        module procedure :: value_in_range_int64
        module procedure :: value_in_range_real32
        module procedure :: value_in_range_real64
        module procedure :: value_in_range_real128
    end interface value_in_range

contains

    function value_in_range_int8(value, min, max) result(in_range)
        implicit none
        integer(int8), intent(in) :: value, min, max
        logical :: in_range

        ! より簡潔な書き方
        in_range = (value >= min .and. value <= max)
    end function value_in_range_int8

    function value_in_range_int16(value, min, max) result(in_range)
        implicit none
        integer(int16), intent(in) :: value, min, max
        logical :: in_range

        ! より簡潔な書き方
        in_range = (value >= min .and. value <= max)
    end function value_in_range_int16

    ! 整数(int32)用の範囲チェック関数
    function value_in_range_int32(value, min, max) result(in_range)
        implicit none
        integer(int32), intent(in) :: value, min, max
        logical :: in_range

        ! より簡潔な書き方
        in_range = (value >= min .and. value <= max)
    end function value_in_range_int32

    function value_in_range_int64(value, min, max) result(in_range)
        implicit none
        integer(int64), intent(in) :: value, min, max
        logical :: in_range

        ! より簡潔な書き方
        in_range = (value >= min .and. value <= max)
    end function value_in_range_int64

    function value_in_range_real32(value, min, max) result(in_range)
        implicit none
        real(real32), intent(in) :: value, min, max
        logical :: in_range

        ! より簡潔な書き方
        in_range = (value >= min .and. value <= max)
    end function value_in_range_real32

    ! 実数(real64)用の範囲チェック関数
    function value_in_range_real64(value, min, max) result(in_range)
        implicit none
        real(real64), intent(in) :: value, min, max
        logical :: in_range

        ! より簡潔な書き方
        in_range = (value >= min .and. value <= max)
    end function value_in_range_real64

    function value_in_range_real128(value, min, max) result(in_range)
        implicit none
        real(real128), intent(in) :: value, min, max
        logical :: in_range

        ! より簡潔な書き方
        in_range = (value >= min .and. value <= max)
    end function value_in_range_real128

end module core_check_range

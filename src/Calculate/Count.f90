module Calculate_Count
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    use :: error

    implicit none

    interface Count_if
        procedure :: Count_if_int32
        procedure :: Count_if_int32_Array
    end interface

contains

    function Count_if_int32(array, condition) result(count)
        !> 条件を満たす整数の個数をカウントする関数
        implicit none
        integer, intent(in) :: array(:) !! 入力配列
        logical, pointer :: condition !! 条件を満たすか判定する関数ポインタ
        integer :: count !! 条件を満たす要素の個数
        integer :: i

        count = 0
        do i = 1, size(array)
            if (condition(array(i))) count = count + 1
        end do
    end function Count_if_int32

    ! 2. 条件配列とともに整数の個数をカウントする関数
    function Count_if_int32_Array(array, condition, condition_array) result(count)
        implicit none
        integer, intent(in) :: array(:) ! 入力配列
        logical, pointer :: condition ! 条件を満たすか判定する関数ポインタ
        integer, intent(in) :: condition_array(:) ! 条件配列
        integer :: count ! 条件を満たす要素の個数
        integer :: i

        count = 0
        do i = 1, size(array)
            if (condition(array(i), condition_array)) count = count + 1
        end do
    end function Count_if_int32_Array

end module Calculate_Count

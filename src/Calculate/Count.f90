module Calculate_Count
    use, intrinsic :: iso_fortran_env, only : int32, real64
    use :: Types
    use :: error

    implicit none

    interface Count_if
        procedure :: Count_if_int32
        procedure :: Count_if_int32_Array
    end interface

    contains

    function Count_if_int32(array, condition) result(count)
        implicit none
        interface
            logical function Condition(x)
                integer, intent(in) :: x
            end function Condition
        end interface
        integer, intent(in) :: array(:)       ! 入力配列
        integer :: count                      ! 条件を満たす要素の個数
        integer :: i

        count = 0
        do i = 1, size(array)
            if (Condition(array(i))) count = count + 1
        end do
    end function Count_if_int32

    function Count_if_int32_Array(array, condition, condition_array) result(count)
        implicit none
        interface
            logical function Condition(x, condition_array)
                integer, intent(in) :: x
                integer, intent(in) :: condition_array(:)
            end function Condition
        end interface
        integer, intent(in) :: array(:)            ! 入力配列
        integer, intent(in) :: condition_array(:)  ! 条件配列
        integer :: count                           ! 条件を満たす要素の個数
        integer :: i

        count = 0
        do i = 1, size(array)
            if (Condition(array(i), condition_array)) count = count + 1
        end do
    end function Count_if_int32_Array

end module Calculate_Count
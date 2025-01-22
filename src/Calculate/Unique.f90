module Calculate_Unique
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_sorting
    implicit none

    interface Unique
        procedure :: Unique_int32
    end interface

contains

    subroutine Unique_int32(array, unique_array)
        !> 整数型の配列からユニークな要素を取得する関数
        implicit none
        integer(int32), intent(in) :: array(:) !! 入力配列
        integer(int32), intent(inout), allocatable :: unique_array(:) !! ユニークな要素の配列
        integer(int32), allocatable :: sorted_array(:)
        integer :: i, count

        if (size(array) == 0) then
            ! 空の配列が渡された場合
            allocate (unique_array(0))
            return
        end if

        ! 入力配列をソート（Fortranではソート関数がないため、merge sortなどを実装するか、代替手段を用いる）
        sorted_array = array
        call sort(sorted_array)

        ! ユニークな要素をカウント
        count = 1 ! 最初の要素は必ずユニーク
        do i = 2, size(sorted_array)
            if (sorted_array(i) /= sorted_array(i - 1)) count = count + 1
        end do

        ! ユニークな要素を格納
        allocate (unique_array(count))
        unique_array(1) = sorted_array(1)
        count = 1
        do i = 2, size(sorted_array)
            if (sorted_array(i) /= sorted_array(i - 1)) then
                count = count + 1
                unique_array(count) = sorted_array(i)
            end if
        end do
    end subroutine Unique_int32

end module Calculate_Unique

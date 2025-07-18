module core_unique
    use, intrinsic :: iso_fortran_env
    use :: stdlib_sorting, only:sort
    use :: core_allocate, only:allocate_array
    implicit none
    private

    public :: unique

    interface unique
        procedure :: unique_int8
        procedure :: unique_int16
        procedure :: unique_int32
        procedure :: unique_int64
    end interface

contains

    subroutine unique_int8(array, unique_array)
        !> 整数型の配列からユニークな要素を取得する関数
        implicit none
        integer(int8), intent(in) :: array(:) !! 入力配列
        integer(int8), intent(inout), allocatable :: unique_array(:) !! ユニークな要素の配列
        integer(int8), allocatable :: sorted_array(:)
        integer(int32) :: i, count

        if (size(array) == 0) then
            ! 空の配列が渡された場合
            call Allocate_Array(unique_array, 0_int32)
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
        call Allocate_Array(unique_array, count)
        unique_array(1) = sorted_array(1)
        count = 1
        do i = 2, size(sorted_array)
            if (sorted_array(i) /= sorted_array(i - 1)) then
                count = count + 1
                unique_array(count) = sorted_array(i)
            end if
        end do
    end subroutine unique_int8

    subroutine unique_int16(array, unique_array)
        !> 整数型の配列からユニークな要素を取得する関数
        implicit none
        integer(int16), intent(in) :: array(:) !! 入力配列
        integer(int16), intent(inout), allocatable :: unique_array(:) !! ユニークな要素の配列
        integer(int16), allocatable :: sorted_array(:)
        integer(int32) :: i, count

        if (size(array) == 0) then
            ! 空の配列が渡された場合
            call Allocate_Array(unique_array, 0_int32)
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
        call Allocate_Array(unique_array, count)
        unique_array(1) = sorted_array(1)
        count = 1
        do i = 2, size(sorted_array)
            if (sorted_array(i) /= sorted_array(i - 1)) then
                count = count + 1
                unique_array(count) = sorted_array(i)
            end if
        end do
    end subroutine unique_int16

    subroutine unique_int32(array, unique_array)
        !> 整数型の配列からユニークな要素を取得する関数
        implicit none
        integer(int32), intent(in) :: array(:) !! 入力配列
        integer(int32), intent(inout), allocatable :: unique_array(:) !! ユニークな要素の配列
        integer(int32), allocatable :: sorted_array(:)
        integer(int32) :: i, count

        if (size(array) == 0) then
            ! 空の配列が渡された場合
            call Allocate_Array(unique_array, 0_int32)
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
        call Allocate_Array(unique_array, count)
        unique_array(1) = sorted_array(1)
        count = 1
        do i = 2, size(sorted_array)
            if (sorted_array(i) /= sorted_array(i - 1)) then
                count = count + 1
                unique_array(count) = sorted_array(i)
            end if
        end do
    end subroutine unique_int32

    subroutine unique_int64(array, unique_array)
        !> 整数型の配列からユニークな要素を取得する関数
        implicit none
        integer(int64), intent(in) :: array(:) !! 入力配列
        integer(int64), intent(inout), allocatable :: unique_array(:) !! ユニークな要素の配列
        integer(int64), allocatable :: sorted_array(:)
        integer(int64) :: i, count

        if (size(array) == 0) then
            ! 空の配列が渡された場合
            call Allocate_Array(unique_array, 0_int64)
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
        call Allocate_Array(unique_array, count)
        unique_array(1) = sorted_array(1)
        count = 1
        do i = 2, size(sorted_array)
            if (sorted_array(i) /= sorted_array(i - 1)) then
                count = count + 1
                unique_array(count) = sorted_array(i)
            end if
        end do
    end subroutine unique_int64

end module core_unique

module core_types_variable
    use, intrinsic :: iso_fortran_env, only: real64, int32
    use :: core_allocate, only:allocate_array
    implicit none
    private

    public :: type_variable

    type :: type_variable
        integer(int32) :: rank
        integer(int32) :: length
        real(real64), allocatable :: new(:)
        real(real64), allocatable :: pre(:)
        real(real64), allocatable :: old(:, :)
        real(real64), allocatable :: dif(:)
    contains
        procedure, pass(self) :: initialize => type_variable_initialize
        procedure, pass(self) :: shift => type_variable_shift
        procedure, pass(self) :: predict => type_variable_predict
        procedure, pass(self) :: set => type_variable_set
    end type type_variable

contains

    subroutine type_variable_initialize(self, length, rank)
        class(type_variable), intent(inout) :: self
        integer(int32), intent(in) :: length
        integer(int32), intent(in) :: rank

        self%rank = rank
        self%length = length

        call allocate_array(self%new, length)
        call allocate_array(self%pre, length)
        call allocate_array(self%old, length, self%rank + 1_int32)
        call allocate_array(self%dif, length)

        self%new(:) = 0.0d0
        self%pre(:) = 0.0d0
        self%old(:, :) = 0.0d0
        self%dif(:) = 0.0d0

    end subroutine type_variable_initialize

    subroutine type_variable_shift(self, reverse)
        class(type_variable), intent(inout) :: self
        logical, intent(in), optional :: reverse
        integer(int32) :: i

        ! ⏪ reverse オプションが指定され、かつ .true. の場合
        if (present(reverse)) then
            if (reverse) then
                ! ステップを1つ巻き戻す
                self%new(:) = self%pre(:)

                if (self%rank > 0) then
                    self%pre(:) = self%old(:, 1)
                    ! 履歴を1つ未来へずらす (old(1) <- old(2), old(2) <- old(3), ...)
                    do i = 1, self%rank - 1
                        self%old(:, i) = self%old(:, i + 1)
                    end do
                    ! 最後の履歴はクリア
                    self%old(:, self%rank) = 0.0d0
                end if
            end if
            ! ⏩ 通常の順方向シフトの場合
        else
            ! 履歴を1つ過去へずらす (old(2) <- old(1), old(3) <- old(2), ...)
            if (self%rank > 0) then
                do i = self%rank, 2, -1
                    self%old(:, i) = self%old(:, i - 1)
                end do
                self%old(:, 1) = self%pre(:)
            end if
            ! pre に new の値を格納
            self%pre(:) = self%new(:)
        end if

    end subroutine type_variable_shift

    subroutine type_variable_predict(self, dt1, dt2, dt3)
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: dt1
        real(real64), intent(in), optional :: dt2, dt3

        integer(int32) :: i
        real(real64) :: w0, w1, w2
        real(real64) :: t0, t1, t2, t3
        real(real64) :: l0, l1, l2

        select case (self%rank)

            ! --------------------------------------
        case (1)
            ! BDF1 → 1次：preをそのまま代入
            !$omp parallel do
            do i = 1, self%length
                self%new(i) = self%pre(i)
            end do
            !$omp end parallel do

            ! --------------------------------------
        case (2)
            if (.not. present(dt2)) stop "BDF2 predict needs dt2"

            ! Lagrange 外挿：点 (t_n, t_{n-1}) から t_{n+1} を予測
            w0 = (dt1 + dt2) / dt2
            w1 = -dt1 / dt2

            !$omp parallel do
            do i = 1, self%length
                self%new(i) = w0 * self%pre(i) + w1 * self%old(i, 1)
            end do
            !$omp end parallel do

            ! --------------------------------------
        case (3)
            if (.not. (present(dt2) .and. present(dt3))) stop "BDF3 predict needs dt2 and dt3"

            ! 時刻：t3 (最古), t2, t1 (現pre), t0 (新step)
            t3 = 0.0d0
            t2 = t3 + dt3
            t1 = t2 + dt2
            t0 = t1 + dt1

            !$omp parallel do private(l0, l1, l2)
            do i = 1, self%length
                l0 = (t0 - t1) * (t0 - t2) / ((t3 - t1) * (t3 - t2)) ! old(:,2)
                l1 = (t0 - t3) * (t0 - t2) / ((t1 - t3) * (t1 - t2)) ! old(:,1)
                l2 = (t0 - t3) * (t0 - t1) / ((t2 - t3) * (t2 - t1)) ! pre

                self%new(i) = l0 * self%old(i, 2) + l1 * self%old(i, 1) + l2 * self%pre(i)
            end do
            !$omp end parallel do

            ! --------------------------------------
        case default
            stop "predict supports only BDF1 to BDF3"
        end select
    end subroutine type_variable_predict

    subroutine type_variable_set(self, value)
        implicit none
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: value(:)
        integer(int32) :: i

        ! new, pre に値を設定
        self%new(:) = value(:)
        self%pre(:) = value(:)

        ! old の全履歴に値を設定
        ! rankの値に関わらず、ループで全ての履歴を一度に設定する
        if (self%rank > 0) then
            do i = 1, self%rank
                self%old(:, i) = value(:)
            end do
        end if

        ! 時間微分項はゼロに初期化
        self%dif(:) = 0.0d0

    end subroutine type_variable_set

end module core_types_variable

module Core_BaseTypes
    use, intrinsic :: iso_fortran_env
    use :: Core_Allocate, only:Allocate_Array
    implicit none
    private
#ifdef _MPI
    include 'mpif.h'
#endif

    public :: Vector2D, Vector3D
    public :: DP2d, DP3d
    public :: INT2d, INT3d
    public :: Variables
    public :: RealPointer
    public :: Type_Iteration
    public :: Belonging

    public :: GaussPointState_t

    public :: assignment(=)

    type :: Vector2D
        sequence
        real(real64) :: x, y
    end type Vector2D

    type :: Vector3D
        sequence
        real(real64) :: x, y, z
    end type Vector3D

    type :: DP2d
        real(real64), allocatable :: x(:), y(:)
    end type DP2d

    type :: DP3d
        real(real64), allocatable :: x(:), y(:), z(:)
    contains
        procedure, pass(self) :: allocate => DP3d_Allocate
    end type DP3d

    type :: INT2d
        integer(int32), allocatable :: x(:), y(:)
    end type INT2d

    type :: INT3d
        integer(int32), allocatable :: x(:), y(:), z(:)
    end type INT3d

    type :: GaussPointState_t
        real(real64) :: temperature
        real(real64) :: pressure
        real(real64) :: water_content
        real(real64) :: porosity
        ! ... 必要に応じて他の状態変数を追加 ...
    end type GaussPointState_t

    type :: Variables
        integer(int32) :: rank
        integer(int32) :: nsize
        real(real64), allocatable :: new(:)
        real(real64), allocatable :: pre(:)
        real(real64), allocatable :: old(:, :)
        real(real64), allocatable :: dif(:)
    contains
        procedure, pass(self) :: shift => Variables_Shift
        procedure, pass(self) :: allocate => Variables_Allocate
        procedure, pass(self) :: predict => Variables_Predictor
        procedure, pass(self) :: set => Variables_Set
    end type Variables

    type :: Belonging
        integer(int32) :: nsize
        integer(int32), allocatable :: group(:)
    contains
        procedure, pass(self) :: allocate => Belonging_Allocate
        procedure, pass(self) :: value => Belonging_Value
    end type Belonging

    type :: Type_Iteration
        integer(int32) :: iter
        integer(int32) :: max_iter

        logical(4) :: isConverged
        integer(int32) :: step
    end type Type_Iteration

    !--------------------------------------------------------------------------------------
    ! Pointer type for real numbers
    !  - This is used to manage the memory of coorinate values in a polymorphic way
    !  - The pointer is initialized to null and can be associated with coorinate values
    !--------------------------------------------------------------------------------------
    type :: RealPointer
        real(real64), pointer :: val => null()
    end type RealPointer

    interface assignment(=)
        module procedure DP3d_Assignment
    end interface

contains
    subroutine Variables_Shift(self, reverse)
        class(Variables), intent(inout) :: self
        logical(4), intent(in), optional :: reverse

        if (present(reverse)) then
            if (reverse) then
                select case (self%rank)
                case (1)
                    self%pre(:) = self%old(:, 1)
                    self%old(:, 1) = self%old(:, 2)
                case (2)
                    self%pre(:) = self%old(:, 1)
                    self%old(:, 1) = self%old(:, 2)
                    self%old(:, 2) = self%old(:, 3)
                case (3)
                    self%new(:) = self%pre(:)
                    self%pre(:) = self%old(:, 1)
                    self%old(:, 1) = self%old(:, 2)
                    self%old(:, 2) = self%old(:, 3)
                    self%old(:, 3) = self%old(:, 4)
                case (4)
                    self%new(:) = self%pre(:)
                    self%pre(:) = self%old(:, 1)
                    self%old(:, 1) = self%old(:, 2)
                    self%old(:, 2) = self%old(:, 3)
                    self%old(:, 3) = self%old(:, 4)
                    self%old(:, 4) = self%old(:, 5)
                case (5)
                    self%new(:) = self%pre(:)
                    self%pre(:) = self%old(:, 1)
                    self%old(:, 1) = self%old(:, 2)
                    self%old(:, 2) = self%old(:, 3)
                    self%old(:, 3) = self%old(:, 4)
                    self%old(:, 4) = self%old(:, 5)
                    self%old(:, 5) = self%old(:, 6)
                end select
                return
            end if
        end if

        select case (self%rank)
        case (1)
            self%old(:, 2) = self%old(:, 1)
            self%old(:, 1) = self%pre(:)
            self%pre(:) = self%new(:)
        case (2)
            self%old(:, 3) = self%old(:, 2)
            self%old(:, 2) = self%old(:, 1)
            self%old(:, 1) = self%pre(:)
            self%pre(:) = self%new(:)
        case (3)
            self%old(:, 4) = self%old(:, 3)
            self%old(:, 3) = self%old(:, 2)
            self%old(:, 2) = self%old(:, 1)
            self%old(:, 1) = self%pre(:)
            self%pre(:) = self%new(:)
        case (4)
            self%old(:, 5) = self%old(:, 4)
            self%old(:, 4) = self%old(:, 3)
            self%old(:, 3) = self%old(:, 2)
            self%old(:, 2) = self%old(:, 1)
            self%old(:, 1) = self%pre(:)
            self%pre(:) = self%new(:)
        case (5)
            self%old(:, 6) = self%old(:, 5)
            self%old(:, 5) = self%old(:, 4)
            self%old(:, 4) = self%old(:, 3)
            self%old(:, 3) = self%old(:, 2)
            self%old(:, 2) = self%old(:, 1)
            self%old(:, 1) = self%pre(:)
            self%pre(:) = self%new(:)
        end select
    end subroutine Variables_Shift

    subroutine Variables_Allocate(self, nsize, rank)
        class(Variables), intent(inout) :: self
        integer(int32), intent(in) :: nsize
        integer(int32), intent(in) :: rank

        self%rank = rank
        self%nsize = nsize

        call Allocate_Array(self%new, nsize)
        call Allocate_Array(self%pre, nsize)
        call Allocate_Array(self%old, nsize, self%rank + 1_int32)
        call Allocate_Array(self%dif, nsize)

        self%new(:) = 0.0d0
        self%pre(:) = 0.0d0
        self%old(:, :) = 0.0d0
        self%dif(:) = 0.0d0

    end subroutine Variables_Allocate

    subroutine Variables_Predictor(self, dt1, dt2, dt3)
        class(Variables), intent(inout) :: self
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
            do i = 1, self%nsize
                self%new(i) = self%pre(i)
            end do
            !$omp end parallel do

            ! --------------------------------------
        case (2)
            if (.not. present(dt2)) stop "BDF2 predictor needs dt2"

            ! Lagrange 外挿：点 (t_n, t_{n-1}) から t_{n+1} を予測
            w0 = (dt1 + dt2) / dt2
            w1 = -dt1 / dt2

            !$omp parallel do
            do i = 1, self%nsize
                self%new(i) = w0 * self%pre(i) + w1 * self%old(i, 1)
            end do
            !$omp end parallel do

            ! --------------------------------------
        case (3)
            if (.not. (present(dt2) .and. present(dt3))) stop "BDF3 predictor needs dt2 and dt3"

            ! 時刻：t3 (最古), t2, t1 (現pre), t0 (新step)
            t3 = 0.0d0
            t2 = t3 + dt3
            t1 = t2 + dt2
            t0 = t1 + dt1

            !$omp parallel do private(l0, l1, l2)
            do i = 1, self%nsize
                l0 = (t0 - t1) * (t0 - t2) / ((t3 - t1) * (t3 - t2)) ! old(:,2)
                l1 = (t0 - t3) * (t0 - t2) / ((t1 - t3) * (t1 - t2)) ! old(:,1)
                l2 = (t0 - t3) * (t0 - t1) / ((t2 - t3) * (t2 - t1)) ! pre

                self%new(i) = l0 * self%old(i, 2) + l1 * self%old(i, 1) + l2 * self%pre(i)
            end do
            !$omp end parallel do

            ! --------------------------------------
        case default
            stop "Predictor supports only BDF1 to BDF3"
        end select
    end subroutine Variables_Predictor

    subroutine Variables_Set(self, value)
        implicit none
        class(Variables), intent(inout) :: self
        real(real64), intent(in) :: value(:)

        self%new(:) = value(:)
        self%pre(:) = value(:)
        self%old(:, 1) = value(:)
        self%old(:, 2) = value(:)
        if (self%rank > 2) self%old(:, 3) = value(:)
        if (self%rank > 3) self%old(:, 4) = value(:)
        if (self%rank > 4) self%old(:, 5) = value(:)
        if (self%rank > 5) self%old(:, 6) = value(:)
        self%dif(:) = 0.0d0

    end subroutine Variables_Set

    subroutine DP3d_Allocate(self, nsize)
        class(DP3d), intent(inout) :: self
        integer(int32), intent(in) :: nsize

        call Allocate_Array(self%x, nsize)
        call Allocate_Array(self%y, nsize)
        call Allocate_Array(self%z, nsize)

    end subroutine DP3d_Allocate

    subroutine DP3d_Assignment(X, Y)
        class(DP3d), intent(inout) :: X
        class(DP3d), intent(in) :: Y

        X%x(:) = Y%x(:)
        X%y(:) = Y%y(:)
        X%z(:) = Y%z(:)

    end subroutine DP3d_Assignment

    subroutine Belonging_Allocate(self, nsize)
        class(Belonging), intent(inout) :: self
        integer(int32), intent(in) :: nsize

        call Allocate_Array(self%group, nsize)
        self%nsize = nsize
        self%group(:) = 0

    end subroutine Belonging_Allocate

    function Belonging_Value(self, array) result(avg_value)
        class(Belonging), intent(inout) :: self
        real(real64), intent(in) :: array(:)
        real(real64) :: avg_value

        avg_value = sum(array(self%group(:))) / self%nsize

    end function Belonging_Value
end module Core_BaseTypes

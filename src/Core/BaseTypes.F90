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

    type :: Variables
        integer(int32) :: rank
        integer(int32) :: nsize
        real(real64), allocatable :: new(:)
        real(real64), allocatable :: pre(:)
        real(real64), allocatable :: old(:, :)
        real(real64), allocatable :: dif(:)
    contains
        procedure, pass(self) :: Shift => Variables_Shift
        procedure, pass(self) :: allocate => Variables_Allocate
    end type Variables

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
        call Allocate_Array(self%old, nsize, self%rank + 1)
        call Allocate_Array(self%dif, nsize)

        self%new(:) = 0.0d0
        self%pre(:) = 0.0d0
        self%old(:, :) = 0.0d0
        self%dif(:) = 0.0d0

    end subroutine Variables_Allocate

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
end module Core_BaseTypes

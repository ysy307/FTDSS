module Types
    use, intrinsic :: iso_fortran_env, only: int8, int32, real64
    implicit none
    public
#ifdef _MPI
    include 'mpif.h'
#endif
    integer(int32), parameter :: Temperature = 1, Pressure = 2, Stress = 3
    integer(int32), parameter :: Linear = 1, pTransition = 2, NonLinear = 3, nTransition = 4
    real(real64), parameter :: GravityAcceleration = 9.80655d0
    integer(int32), parameter :: undumped = 0, dumped = 1

    type :: VC
        sequence
        real(real64) :: x, y
    end type VC

    type :: Vector2D
        sequence
        real(real64) :: x, y
    end type Vector2D

    type :: Vector3D
        sequence
        real(real64) :: x, y, z
    end type Vector3D

    type :: DP2d
        sequence
        real(real64), allocatable :: x(:), y(:)
    end type DP2d

    type :: DP3d
        real(real64), allocatable :: x(:), y(:), z(:)
    contains
        procedure, pass(self) :: allocate => DP3d_Allocate
    end type DP3d

    type :: INT2d
        sequence
        integer(int32), allocatable :: x(:), y(:)
    end type INT2d

    type :: INT3d
        sequence
        integer(int32), allocatable :: x(:), y(:), z(:)
    end type INT3d

    type :: PH
        sequence
        real(real64) :: soil, water, ice
    end type PH

    type :: Phases
        sequence
        real(real64) :: soil, water, ice
    end type Phases

    type :: Type_VTK
        character(:), allocatable :: format !! ASCII or BINARY
        character(:), allocatable :: dataset !! STRUCTURED_POINTS, STRUCTURED_GRID, RECTILINEAR_GRID, POLYDATA, UNSTRUCTURED_GRID
        character(:), allocatable :: POINTS_DATATYPE !! dataType is one of the types bit, unsigned_char, char, unsigned_short, short, unsigned_int, int,  unsigned_long, long, float, or double.
        integer(int32) :: numPoints !! Number of points
        type(DP3d) :: POINTS !! VTK 3D geometry coordinates
        integer(int32) :: numTotalCells !! Number of cells
        integer(int32) :: maxCellTypes
        integer(int32) :: numCellTypes !! Number of cell types

        type(Type_VTK_CELLS), allocatable :: CELLS(:) !! Cell information
        integer(int32), allocatable :: Invalid_CELLS_LIST(:) !! Cell information in the list
        integer(int32), allocatable :: CellEntityIds(:) !! Cell entity IDs

    end type Type_VTK

    type :: Type_VTK_CELLS
        logical(4) :: exists
        integer(int32) :: numCells !! Number of cells beloging to the region
        integer(int8), allocatable :: offset(:)
        integer(int32), allocatable :: Nodes(:, :) !! Node numbers of the cells
    end type Type_VTK_CELLS

    !!KEEP THIS TYPE
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

    type :: Type_Time
        real(real64) :: start_time
        real(real64) :: end_time
        real(real64) :: time
        real(real64) :: time_old
        real(real64) :: dt
        real(real64), allocatable :: dt_old(:)
        real(real64) :: dt_max
        real(real64) :: dt_min
    end type Type_Time

    type :: Type_Iteration
        integer(int32) :: iter
        integer(int32) :: max_iter

        logical(4) :: isConverged
        integer(int32) :: step
    end type Type_Iteration

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

        allocate (self%new(nsize))
        allocate (self%pre(nsize))
        allocate (self%old(nsize, self%rank + 1))
        allocate (self%dif(nsize))

        self%new(:) = 0.0d0
        self%pre(:) = 0.0d0
        self%old(:, :) = 0.0d0
        self%dif(:) = 0.0d0

    end subroutine Variables_Allocate

    subroutine DP3d_Allocate(self, nsize)
        class(DP3d), intent(inout) :: self
        integer(int32), intent(in) :: nsize

        allocate (self%x(nsize))
        allocate (self%y(nsize))
        allocate (self%z(nsize))

    end subroutine DP3d_Allocate

    subroutine DP3d_Assignment(X, Y)
        class(DP3d), intent(inout) :: X
        class(DP3d), intent(in) :: Y

        X%x(:) = Y%x(:)
        X%y(:) = Y%y(:)
        X%z(:) = Y%z(:)

    end subroutine DP3d_Assignment
end module Types

module Core_VTK
    use, intrinsic :: iso_fortran_env
    use :: Core_BaseTypes, only:DP3d
    use :: Core_Allocate, only:Allocate_Array
    implicit none
    private

    type :: VTK_CELL_NAMES
        integer(int32) :: VTK_VERTEX = 1
        integer(int32) :: VTK_POLY_VERTEX = 2
        integer(int32) :: VTK_LINE = 3
        integer(int32) :: VTK_POLY_LINE = 4
        integer(int32) :: VTK_TRIANGLE = 5
        integer(int32) :: VTK_TRIANGLE_STRIP = 6
        integer(int32) :: VTK_POLYGON = 7
        integer(int32) :: VTK_PIXEL = 8
        integer(int32) :: VTK_QUAD = 9
        integer(int32) :: VTK_TETRA = 10
        integer(int32) :: VTK_VOXEL = 11
        integer(int32) :: VTK_HEXAHEDRON = 12
        integer(int32) :: VTK_WEDGE = 13
        integer(int32) :: VTK_PYRAMID = 14
        integer(int32) :: VTK_QUADRATIC_EDGE = 21
        integer(int32) :: VTK_QUADRATIC_TRIANGLE = 22
        integer(int32) :: VTK_QUADRATIC_QUAD = 23
        integer(int32) :: VTK_QUADRATIC_TETRA = 24
        integer(int32) :: VTK_QUADRATIC_HEXAHEDRON = 25
    end type VTK_CELL_NAMES

    type :: Type_VTK_CELLS
        integer(int8) :: offset
        integer(int32) :: CellEntityId
        integer(int32) :: CellType
        integer(int32), allocatable :: Connectivity(:) !! Node numbers of the cells
    end type Type_VTK_CELLS

    type :: Type_VTK
        character(:), allocatable :: format !! ASCII or BINARY
        character(:), allocatable :: dataset !! STRUCTURED_POINTS, STRUCTURED_GRID, RECTILINEAR_GRID, POLYDATA, UNSTRUCTURED_GRID
        character(:), allocatable :: POINTS_DATATYPE !! dataType is one of the types bit, unsigned_char, char, unsigned_short, short, unsigned_int, int,  unsigned_long, long, float, or double.
        integer(int32) :: numPoints !! Number of points
        type(DP3d) :: POINTS !! VTK 3D geometry coordinates
        integer(int32) :: numTotalCells !! Number of cells
        integer(int32) :: numCellTypes !! Number of cell types

        type(Type_VTK_CELLS), allocatable :: CELLS(:) !! Cell information
        type(VTK_CELL_NAMES) :: Names !! Cell names
    contains
        procedure :: Is_In => Core_VTK_IN_CellType
    end type Type_VTK

    character(*), parameter :: c_ASCII = "ASCII"
    character(*), parameter :: c_BINARY = "BINARY"

    character(*), parameter :: c_DATASET = "DATASET"
    character(*), parameter :: c_STRUCTURED_POINTS = "STRUCTURED_POINTS"
    character(*), parameter :: c_STRUCTURED_GRID = "STRUCTURED_GRID"
    character(*), parameter :: c_RECTILINEAR_GRID = "RECTILINEAR_GRID"
    character(*), parameter :: c_POLYDATA = "POLYDATA"
    character(*), parameter :: c_UNSTRUCTURED_GRID = "UNSTRUCTURED_GRID"
    character(*), parameter :: c_FIELD = "FIELD"

    character(*), parameter :: c_POINTS = "POINTS"
    character(*), parameter :: c_CELLS = "CELLS"
    character(*), parameter :: c_CELL_TYPES = "CELL_TYPES"
    character(*), parameter :: c_CELL_DATA = "CELL_DATA"
    character(*), parameter :: c_CellEntityIds = "CellEntityIds"

    character(*), parameter :: c_unsigned_char = "unsigned_char"
    character(*), parameter :: c_char = "char"
    character(*), parameter :: c_unsigned_short = "unsigned_short"
    character(*), parameter :: c_short = "short"
    character(*), parameter :: c_unsigned_int = "unsigned_int"
    character(*), parameter :: c_int = "int"
    character(*), parameter :: c_unsigned_long = "unsigned_long"
    character(*), parameter :: c_long = "long"
    character(*), parameter :: c_float = "float"
    character(*), parameter :: c_double = "double"
    character(*), parameter :: space = " "

    public :: Type_VTK

    interface Type_VTK
        procedure :: Core_VTK_Read
    end interface

contains

    function Core_VTK_Read(filename) result(vtk)
        !> Read VTK file
        implicit none
        character(*), intent(in) :: filename !! ***.VTK file name
        type(Type_VTK) :: vtk !! VTK data
        integer(int32) :: new_unit
        logical :: exists

        inquire (file=filename, exist=exists)
        if (.not. exists) then
            write (*, *) "File does not exist: ", filename
            stop
        end if

        open (newunit=new_unit, file=filename, status="old", action="read")
        call Core_VTK_Read_Header(new_unit, vtk)
        call Core_VTK_Read_Data(new_unit, vtk)
        close (new_unit)

    end function Core_VTK_Read

    subroutine Core_VTK_Read_Header(unit, vtk)
        !> Read VTK header
        implicit none
        integer(int32), intent(in) :: unit !! Unit number
        type(Type_VTK), intent(inout) :: vtk !! VTK data

        character(256) :: line
        integer(int32) :: iostat

        read (unit, '(a)', iostat=iostat) line !! data version information
        read (unit, '(a)', iostat=iostat) line !! data file header
        read (unit, '(a)', iostat=iostat) line !! file format
        select case (trim(line))
        case (c_ASCII)
            vtk%format = c_ASCII
        case (c_BINARY)
            vtk%format = c_BINARY
        end select

        read (unit, '(a)', iostat=iostat) line
        ! DATASET DATASET_NAME
        ! 12345678901234567890
        ! len(DATASET) = 7 is final character of DATASET_NAME
        ! Start DATASET_NAME from len(DATASET) + 2 (add space and 1)
        select case (trim(line(len(c_DATASET) + 2:)))
        case (c_STRUCTURED_POINTS)
            vtk%dataset = c_STRUCTURED_POINTS
        case (c_STRUCTURED_GRID)
            vtk%dataset = c_STRUCTURED_GRID
        case (c_RECTILINEAR_GRID)
            vtk%dataset = c_RECTILINEAR_GRID
        case (c_POLYDATA)
            vtk%dataset = c_POLYDATA
        case (c_UNSTRUCTURED_GRID)
            vtk%dataset = c_UNSTRUCTURED_GRID
        end select

    end subroutine Core_VTK_Read_Header

    subroutine Core_VTK_Read_Data(unit, vtk)
        !> Read VTK data
        implicit none
        integer(int32), intent(in) :: unit !! Unit number
        type(Type_VTK), intent(inout) :: vtk !! VTK data

        character(256) :: line, dtype
        character(16) :: keyword
        integer(int32) :: iostat, numPoints
        integer(int32) :: pos1, pos2
        integer(int32) :: iPoints

        !! DATASET (UNSTRUCTURED_GRID)
        read (unit, '(a)', iostat=iostat) line
        if (iostat /= 0) stop

        line = trim(adjustl(line))

        pos1 = index(line, space)
        if (pos1 == 0) stop
        keyword = line(1:pos1 - 1)

        if (keyword == c_POINTS) then
            call Core_VTK_Read_Data_Points(unit, vtk, line)
        end if

        read (unit, '(a)', iostat=iostat) line
        if (iostat /= 0) stop
        line = trim(adjustl(line))

        pos1 = index(line, space)
        if (pos1 == 0) stop
        keyword = line(1:pos1 - 1)

        if (keyword == c_CELLS) then
            call Core_VTK_Read_Data_Cells(unit, vtk, line)
        end if

        read (unit, '(a)', iostat=iostat) line
        if (iostat /= 0) stop
        line = trim(adjustl(line))

        pos1 = index(line, space)
        if (pos1 == 0) stop
        keyword = line(1:pos1 - 1)

        if (keyword == c_CELL_TYPES) then
            call Core_VTK_Read_Data_Cells_Types(unit, vtk, line)
        end if

        read (unit, '(a)', iostat=iostat) line
        if (iostat /= 0) stop
        line = trim(adjustl(line))

        pos1 = index(line, space)
        if (pos1 == 0) stop
        keyword = line(1:pos1 - 1)

        if (keyword == c_CELL_DATA) then
            call Core_VTK_Read_Data_CellEntityIds(unit, vtk, line)
        end if

    end subroutine Core_VTK_Read_Data

    subroutine Core_VTK_Read_Data_Points(unit, vtk, headline)
        !> Read VTK data points
        implicit none
        integer(int32), intent(in) :: unit !! Unit number
        type(Type_VTK), intent(inout) :: vtk !! VTK data
        character(*), intent(in) :: headline !! Headline

        character(256) :: dtype
        integer(int32) :: iostat, numPoints
        integer(int32) :: pos1, pos2
        integer(int32) :: iPoint

        !! line
        !! POINTS ***** double
        pos1 = index(headline, space)
        pos2 = index(headline(pos1 + 1:), space) + pos1
        if (pos2 == pos1) stop

        read (headline(pos1 + 1:pos2 - 1), '(i)') numPoints
        dtype = headline(pos2 + 1:)

        vtk%numPoints = numPoints
        select case (trim(adjustl(dtype)))
        case (c_unsigned_char)
            vtk%POINTS_DATATYPE = c_unsigned_char
        case (c_char)
            vtk%POINTS_DATATYPE = c_char
        case (c_unsigned_short)
            vtk%POINTS_DATATYPE = c_unsigned_short
        case (c_short)
            vtk%POINTS_DATATYPE = c_short
        case (c_unsigned_int)
            vtk%POINTS_DATATYPE = c_unsigned_int
        case (c_int)
            vtk%POINTS_DATATYPE = c_int
        case (c_unsigned_long)
            vtk%POINTS_DATATYPE = c_unsigned_long
        case (c_long)
            vtk%POINTS_DATATYPE = c_long
        case (c_float)
            vtk%POINTS_DATATYPE = c_float
        case (c_double)
            vtk%POINTS_DATATYPE = c_double
        end select

        call vtk%POINTS%allocate(numPoints)

        do iPoint = 1, vtk%numPoints
            read (unit, *, iostat=iostat) vtk%POINTS%x(iPoint), vtk%POINTS%y(iPoint), vtk%POINTS%z(iPoint)
            if (iostat /= 0) stop
        end do

        read (unit, '(a)', iostat=iostat) ! Skip

    end subroutine Core_VTK_Read_Data_Points

    subroutine Core_VTK_Read_Data_Cells(unit, vtk, headline)
        !> Read VTK data cells
        implicit none
        integer(int32), intent(in) :: unit !! Unit number
        type(Type_VTK), intent(inout) :: vtk !! VTK data
        character(*), intent(in) :: headline !! Headline
        integer(int32) :: iostat, numTotalCells
        integer(int32) :: pos1, pos2
        integer(int32) :: iCell
        character(256) :: line

        pos1 = index(headline, space)
        pos2 = index(headline(pos1 + 1:), space) + pos1
        if (pos2 == pos1) stop

        read (headline(pos1 + 1:pos2 - 1), '(i)') numTotalCells

        vtk%numTotalCells = numTotalCells

        allocate (vtk%CELLS(vtk%numTotalCells))

        do iCell = 1, vtk%numTotalCells
            read (unit, '(a)', iostat=iostat) line
            if (iostat /= 0) stop

            pos1 = index(line, space)

            read (line(:pos1), '(i)') vtk%CELLS(iCell)%offset
            if (iostat /= 0) stop

            call Allocate_Array(vtk%CELLS(iCell)%Connectivity, transfer(vtk%CELLS(iCell)%offset, iCell))
            read (line(pos1 + 1:), *) vtk%CELLS(iCell)%Connectivity(:)
        end do

        do iCell = 1, vtk%numTotalCells
            vtk%CELLS(iCell)%Connectivity(:) = vtk%CELLS(iCell)%Connectivity(:) + 1
        end do

        read (unit, '(a)', iostat=iostat) ! Skip

    end subroutine Core_VTK_Read_Data_Cells

    subroutine Core_VTK_Read_Data_Cells_Types(unit, vtk, headline)
        !> Read VTK data cells
        implicit none
        integer(int32), intent(in) :: unit !! Unit number
        type(Type_VTK), intent(inout) :: vtk !! VTK data
        character(*), intent(in) :: headline !! Headline

        character(256) :: line
        integer(int32) :: iostat, numCellTypes
        integer(int32) :: pos1, pos2
        integer(int32) :: iCell
        integer(int32) :: dum

        pos1 = index(headline, space)
        pos2 = index(headline(pos1 + 1:), space) + pos1
        if (pos2 == pos1) stop

        read (headline(pos1 + 1:pos2 - 1), '(i)') numCellTypes

        do iCell = 1, numCellTypes
            read (unit, *, iostat=iostat) vtk%CELLS(iCell)%CellType
            if (iostat /= 0) stop
        end do

        read (unit, '(a)', iostat=iostat) ! Skip

    end subroutine Core_VTK_Read_Data_Cells_Types

    subroutine Core_VTK_Read_Data_CellEntityIds(unit, vtk, headline)
        !> Read VTK data cell entity ids
        ! implicit none
        integer(int32), intent(in) :: unit !! Unit number
        type(Type_VTK), intent(inout) :: vtk !! VTK data
        character(*), intent(in) :: headline !! Headline

        character(256) :: line, CellEntity
        integer(int32) :: pos1, pos2
        integer(int32) :: numCellEntityIds
        integer(int32) :: iCellEntityId

        pos1 = index(headline, space)
        pos2 = index(headline(pos1 + 1:), space) + pos1
        if (pos2 == pos1) stop
        read (headline(pos1 + 1:pos2 - 1), '(i)') numCellEntityIds

        read (unit, '(A)') line
        pos1 = index(line, space)
        pos2 = index(line(pos1 + 1:), space) + pos1
        read (line(pos1 + 1:pos2 - 1), '(a)') CellEntity
        if (CellEntity == c_CellEntityIds) then
            read (unit, *) ! Skip
            do iCellEntityId = 1, numCellEntityIds
                read (unit, '(i)') vtk%CELLS(iCellEntityId)%CellEntityId
            end do
        end if

    end subroutine Core_VTK_Read_Data_CellEntityIds

    function Core_VTK_IN_CellType(self, iCellType, Shape_Dimention) result(isIn)
        !> Check if cell type is in VTK
        implicit none
        class(Type_VTK), intent(in) :: self !! VTK data
        integer(int32), intent(in) :: iCellType !! Cell type
        integer(int32), intent(in) :: Shape_Dimention !! Shape dimension
        logical(4) :: isIn
        integer(int32) :: i

        isIn = .false.
        select case (Shape_Dimention)
        case (1)
            if (iCellType == self%Names%VTK_LINE .or. &
                iCellType == self%Names%VTK_QUADRATIC_EDGE &
                ) then
                isIn = .true.
            end if
        case (2)
            if (iCellType == self%Names%VTK_TRIANGLE .or. &
                iCellType == self%Names%VTK_PIXEL .or. &
                iCellType == self%Names%VTK_QUAD .or. &
                iCellType == self%Names%VTK_QUADRATIC_TRIANGLE .or. &
                iCellType == self%Names%VTK_QUADRATIC_QUAD &
                ) then
                isIn = .true.
            end if
        end select

    end function Core_VTK_IN_CellType

end module Core_VTK

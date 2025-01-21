module Inout_VTK
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types, only:Type_VTK
    use :: allocate, only:Allocate_Vector, Allocate_Matrix
    use :: Allocate_Structure, only:Allocate_DP
    implicit none
    private

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

    integer(int32), parameter :: VTK_VERTEX = 1
    integer(int32), parameter :: VTK_POLY_VERTEX = 2
    integer(int32), parameter :: VTK_LINE = 3
    integer(int32), parameter :: VTK_POLY_LINE = 4
    integer(int32), parameter :: VTK_TRIANGLE = 5
    integer(int32), parameter :: VTK_TRIANGLE_STRIP = 6
    integer(int32), parameter :: VTK_POLYGON = 7
    integer(int32), parameter :: VTK_PIXEL = 8
    integer(int32), parameter :: VTK_QUAD = 9
    integer(int32), parameter :: VTK_TETRA = 10
    integer(int32), parameter :: VTK_VOXEL = 11
    integer(int32), parameter :: VTK_HEXAHEDRON = 12
    integer(int32), parameter :: VTK_WEDGE = 13
    integer(int32), parameter :: VTK_PYRAMID = 14
    integer(int32), parameter :: VTK_QUADRATIC_EDGE = 21
    integer(int32), parameter :: VTK_QUADRATIC_TRIANGLE = 22
    integer(int32), parameter :: VTK_QUADRATIC_QUAD = 23
    integer(int32), parameter :: VTK_QUADRATIC_TETRA = 24
    integer(int32), parameter :: VTK_QUADRATIC_HEXAHEDRON = 25

    character(*), parameter :: space = " "

    public :: Inout_VTK_Read

contains

    subroutine Inout_VTK_Read(filename, vtk)
        !> Read VTK file
        implicit none
        character(*), intent(in) :: filename !! .VTK file name
        type(Type_VTK), intent(inout) :: vtk !! VTK data
        integer(int32) :: new_unit
        logical :: exists

        inquire (file=filename, exist=exists)
        if (.not. exists) then
            write (*, *) "File does not exist: ", filename
            stop
        end if

        open (newunit=new_unit, file=filename, status="old", action="read")
        call Inout_VTK_Read_Header(new_unit, vtk)
        call Inout_VTK_Read_Data(new_unit, vtk)
        close (new_unit)

    end subroutine Inout_VTK_Read

    subroutine Inout_VTK_Read_Header(unit, vtk)
        !> Read VTK header
        implicit none
        integer(int32), intent(in) :: unit !! Unit number
        type(Type_VTK), intent(inout) :: vtk !! VTK data

        character(256) :: line
        integer(int32) :: iostat

        read (unit, '(a)', iostat=iostat) line
        read (unit, '(a)', iostat=iostat) line
        read (unit, '(a)', iostat=iostat) line
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

    end subroutine Inout_VTK_Read_Header

    subroutine Inout_VTK_Read_Data(unit, vtk)
        !> Read VTK data
        implicit none
        integer(int32), intent(in) :: unit !! Unit number
        type(Type_VTK), intent(inout) :: vtk !! VTK data

        character(256) :: line, dtype
        character(256), allocatable :: lines(:)
        character(16) :: keyword
        integer(int32) :: iostat, numPoints
        integer(int32) :: pos1, pos2
        integer(int32) :: iPoints

        read (unit, '(a)', iostat=iostat) line
        if (iostat /= 0) stop

        line = trim(adjustl(line))

        pos1 = index(line, space)
        if (pos1 == 0) stop
        keyword = line(1:pos1 - 1)

        if (keyword == c_POINTS) then
            call Inout_VTK_Read_Data_Points(unit, vtk, line)
        end if

        read (unit, '(a)', iostat=iostat) line
        if (iostat /= 0) stop
        line = trim(adjustl(line))

        pos1 = index(line, space)
        if (pos1 == 0) stop
        keyword = line(1:pos1 - 1)

        if (keyword == c_CELLS) then
            call Inout_VTK_Read_Data_Cells(unit, vtk, line, lines)
        end if

        read (unit, '(a)', iostat=iostat) line
        if (iostat /= 0) stop
        line = trim(adjustl(line))

        pos1 = index(line, space)
        if (pos1 == 0) stop
        keyword = line(1:pos1 - 1)

        if (keyword == c_CELL_TYPES) then
            call Inout_VTK_Read_Data_Cells_Types(unit, vtk, line, lines)
        end if

        read (unit, '(a)', iostat=iostat) line
        if (iostat /= 0) stop
        line = trim(adjustl(line))

        pos1 = index(line, space)
        if (pos1 == 0) stop
        keyword = line(1:pos1 - 1)

        if (keyword == c_CELL_DATA) then
            call Inout_VTK_Read_Data_CellEntityIds(unit, vtk, line)
        end if

        deallocate (lines)

    end subroutine Inout_VTK_Read_Data

    subroutine Inout_VTK_Read_Data_Points(unit, vtk, headline)
        !> Read VTK data points
        implicit none
        integer(int32), intent(in) :: unit !! Unit number
        type(Type_VTK), intent(inout) :: vtk !! VTK data
        character(*), intent(in) :: headline !! Headline

        character(256) :: dtype
        integer(int32) :: iostat, numPoints
        integer(int32) :: pos1, pos2
        integer(int32) :: iPoint

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

        call Allocate_DP(vtk%POINTS, numPoints)

        do iPoint = 1, vtk%numPoints
            read (unit, *, iostat=iostat) vtk%POINTS%x(iPoint), vtk%POINTS%y(iPoint), vtk%POINTS%z(iPoint)
            if (iostat /= 0) stop
        end do

        read (unit, '(a)', iostat=iostat) ! Skip

    end subroutine Inout_VTK_Read_Data_Points

    subroutine Inout_VTK_Read_Data_Cells(unit, vtk, headline, lines)
        !> Read VTK data cells
        implicit none
        integer(int32), intent(in) :: unit !! Unit number
        type(Type_VTK), intent(inout) :: vtk !! VTK data
        character(*), intent(in) :: headline !! Headline
        character(256), allocatable, intent(inout) :: lines(:)
        integer(int32) :: iostat, numCells, numCellsList
        integer(int32) :: pos1, pos2
        integer(int32) :: iCell

        pos1 = index(headline, space)
        pos2 = index(headline(pos1 + 1:), space) + pos1
        if (pos2 == pos1) stop

        read (headline(pos1 + 1:pos2 - 1), '(i)') numCells
        read (headline(pos2 + 1:), '(i)') numCellsList

        ! allocate (vtk%CELLS(25))

        vtk%numCells = numCells
        vtk%numCellsList = numCellsList

        allocate (lines(vtk%numCells))
        ! call Allocate_Vector(CellType, vtk%numCells)

        do iCell = 1, vtk%numCells
            read (unit, '(a)', iostat=iostat) lines(iCell)
            if (iostat /= 0) stop
        end do

        do iCell = 1, vtk%numCells
            pos1 = index(lines(iCell), space)
            lines(iCell) = lines(iCell) (pos1 + 1:)
        end do
        read (unit, '(a)', iostat=iostat) ! Skip

    end subroutine Inout_VTK_Read_Data_Cells

    subroutine Inout_VTK_Read_Data_Cells_Types(unit, vtk, headline, lines)
        !> Read VTK data cells
        implicit none
        integer(int32), intent(in) :: unit !! Unit number
        type(Type_VTK), intent(inout) :: vtk !! VTK data
        character(*), intent(in) :: headline !! Headline
        character(256), allocatable, intent(inout) :: lines(:)

        character(256) :: line
        integer(int32) :: iostat, numCellTypes
        integer(int32) :: pos1, pos2
        integer(int32) :: iCell, iiCell
        integer(int32), allocatable :: CellType(:)
        integer(int32) :: counts

        pos1 = index(headline, space)
        pos2 = index(headline(pos1 + 1:), space) + pos1
        if (pos2 == pos1) stop

        read (headline(pos1 + 1:pos2 - 1), '(i)') numCellTypes
        allocate (vtk%CELLS(25))

        vtk%numCellTypes = numCellTypes
        call Allocate_Vector(CellType, vtk%numCellTypes)

        do iCell = 1, vtk%numCellTypes
            read (unit, '(i)', iostat=iostat) CellType(iCell)
            if (iostat /= 0) stop
        end do

        vtk%CELLS(:)%nCells = 0
        do iCell = 1, vtk%numCells
            select case (CellType(iCell))
            case (VTK_VERTEX)
                vtk%CELLS(VTK_VERTEX)%nCells = vtk%CELLS(VTK_VERTEX)%nCells + 1
            case (VTK_POLY_VERTEX)
                vtk%CELLS(VTK_POLY_VERTEX)%nCells = vtk%CELLS(VTK_POLY_VERTEX)%nCells + 1
            case (VTK_LINE)
                vtk%CELLS(VTK_LINE)%nCells = vtk%CELLS(VTK_LINE)%nCells + 1
            case (VTK_POLY_LINE)
                vtk%CELLS(VTK_POLY_LINE)%nCells = vtk%CELLS(VTK_POLY_LINE)%nCells + 1
            case (VTK_TRIANGLE)
                vtk%CELLS(VTK_TRIANGLE)%nCells = vtk%CELLS(VTK_TRIANGLE)%nCells + 1
            case (VTK_TRIANGLE_STRIP)
                vtk%CELLS(VTK_TRIANGLE_STRIP)%nCells = vtk%CELLS(VTK_TRIANGLE_STRIP)%nCells + 1
            case (VTK_POLYGON)
                vtk%CELLS(VTK_POLYGON)%nCells = vtk%CELLS(VTK_POLYGON)%nCells + 1
            case (VTK_PIXEL)
                vtk%CELLS(VTK_PIXEL)%nCells = vtk%CELLS(VTK_PIXEL)%nCells + 1
            case (VTK_QUAD)
                vtk%CELLS(VTK_QUAD)%nCells = vtk%CELLS(VTK_QUAD)%nCells + 1
            case (VTK_TETRA)
                vtk%CELLS(VTK_TETRA)%nCells = vtk%CELLS(VTK_TETRA)%nCells + 1
            case (VTK_VOXEL)
                vtk%CELLS(VTK_VOXEL)%nCells = vtk%CELLS(VTK_VOXEL)%nCells + 1
            case (VTK_HEXAHEDRON)
                vtk%CELLS(VTK_HEXAHEDRON)%nCells = vtk%CELLS(VTK_HEXAHEDRON)%nCells + 1
            case (VTK_WEDGE)
                vtk%CELLS(VTK_WEDGE)%nCells = vtk%CELLS(VTK_WEDGE)%nCells + 1
            case (VTK_PYRAMID)
                vtk%CELLS(VTK_PYRAMID)%nCells = vtk%CELLS(VTK_PYRAMID)%nCells + 1
            case (VTK_QUADRATIC_EDGE)
                vtk%CELLS(VTK_QUADRATIC_EDGE)%nCells = vtk%CELLS(VTK_QUADRATIC_EDGE)%nCells + 1
            case (VTK_QUADRATIC_TRIANGLE)
                vtk%CELLS(VTK_QUADRATIC_TRIANGLE)%nCells = vtk%CELLS(VTK_QUADRATIC_TRIANGLE)%nCells + 1
            case (VTK_QUADRATIC_QUAD)
                vtk%CELLS(VTK_QUADRATIC_QUAD)%nCells = vtk%CELLS(VTK_QUADRATIC_QUAD)%nCells + 1
            case (VTK_QUADRATIC_TETRA)
                vtk%CELLS(VTK_QUADRATIC_TETRA)%nCells = vtk%CELLS(VTK_QUADRATIC_TETRA)%nCells + 1
            case (VTK_QUADRATIC_HEXAHEDRON)
                vtk%CELLS(VTK_QUADRATIC_HEXAHEDRON)%nCells = vtk%CELLS(VTK_QUADRATIC_HEXAHEDRON)%nCells + 1
            end select
        end do

        counts = 0
        do iCell = 1, 25
            if (vtk%CELLS(iCell)%nCells > 0) then
                counts = counts + 1
                select case (iCell)
                case (VTK_VERTEX, VTK_POLY_VERTEX, VTK_POLY_LINE, VTK_TRIANGLE_STRIP, VTK_POLYGON)
                    call Allocate_Vector(vtk%CELLS(iCell)%Nodes_Array, vtk%CELLS(iCell)%nCells)
                case (VTK_LINE, VTK_QUADRATIC_EDGE)
                    call Allocate_Matrix(vtk%CELLS(iCell)%Nodes, 2, vtk%CELLS(iCell)%nCells)
                case (VTK_TRIANGLE)
                    call Allocate_Matrix(vtk%CELLS(iCell)%Nodes, 3, vtk%CELLS(iCell)%nCells)
                case (VTK_PIXEL, VTK_QUAD, VTK_TETRA)
                    call Allocate_Matrix(vtk%CELLS(iCell)%Nodes, 4, vtk%CELLS(iCell)%nCells)
                case (VTK_PYRAMID)
                    call Allocate_Matrix(vtk%CELLS(iCell)%Nodes, 5, vtk%CELLS(iCell)%nCells)
                case (VTK_WEDGE, VTK_QUADRATIC_TRIANGLE)
                    call Allocate_Matrix(vtk%CELLS(iCell)%Nodes, 6, vtk%CELLS(iCell)%nCells)
                case (VTK_VOXEL, VTK_HEXAHEDRON, VTK_QUADRATIC_QUAD)
                    call Allocate_Matrix(vtk%CELLS(iCell)%Nodes, 8, vtk%CELLS(iCell)%nCells)
                case (VTK_QUADRATIC_TETRA)
                    call Allocate_Matrix(vtk%CELLS(iCell)%Nodes, 10, vtk%CELLS(iCell)%nCells)
                case (VTK_QUADRATIC_HEXAHEDRON)
                    call Allocate_Matrix(vtk%CELLS(iCell)%Nodes, 20, vtk%CELLS(iCell)%nCells)
                end select
            end if
        end do

        call Allocate_Vector(vtk%Invalid_CELLS_LIST, counts)
        iiCell = 0
        do iCell = 1, 25
            if (vtk%CELLS(iCell)%nCells > 0) then
                iiCell = iiCell + 1
                vtk%Invalid_CELLS_LIST(iiCell) = iCell
            end if
        end do

        do iCell = 1, vtk%numCells
            select case (CellType(iCell))
            case (VTK_VERTEX)
                read (lines(iCell), *, iostat=iostat) vtk%CELLS(VTK_VERTEX)%Nodes_Array(iCell)
            case (VTK_POLY_VERTEX)
                read (lines(iCell), *, iostat=iostat) vtk%CELLS(VTK_POLY_VERTEX)%Nodes_Array(iCell)
            case (VTK_LINE)
                read (lines(iCell), *, iostat=iostat) vtk%CELLS(VTK_LINE)%Nodes(1:2, iCell)
            case (VTK_POLY_LINE)
                read (lines(iCell), *, iostat=iostat) vtk%CELLS(VTK_POLY_LINE)%Nodes_Array(iCell)
            case (VTK_TRIANGLE)
                read (lines(iCell), *, iostat=iostat) vtk%CELLS(VTK_TRIANGLE)%Nodes(1:3, iCell)
            case (VTK_TRIANGLE_STRIP)
                read (lines(iCell), *, iostat=iostat) vtk%CELLS(VTK_TRIANGLE_STRIP)%Nodes_Array(iCell)
            case (VTK_POLYGON)
                read (lines(iCell), *, iostat=iostat) vtk%CELLS(VTK_POLYGON)%Nodes_Array(iCell)
            case (VTK_PIXEL)
                read (lines(iCell), *, iostat=iostat) vtk%CELLS(VTK_PIXEL)%Nodes(1:4, iCell)
            case (VTK_QUAD)
                read (lines(iCell), *, iostat=iostat) vtk%CELLS(VTK_QUAD)%Nodes(1:4, iCell)
            case (VTK_TETRA)
                read (lines(iCell), *, iostat=iostat) vtk%CELLS(VTK_TETRA)%Nodes(1:4, iCell)
            case (VTK_VOXEL)
                read (lines(iCell), *, iostat=iostat) vtk%CELLS(VTK_VOXEL)%Nodes(1:8, iCell)
            case (VTK_HEXAHEDRON)
                read (lines(iCell), *, iostat=iostat) vtk%CELLS(VTK_HEXAHEDRON)%Nodes(1:8, iCell)
            case (VTK_WEDGE)
                read (lines(iCell), *, iostat=iostat) vtk%CELLS(VTK_WEDGE)%Nodes(1:6, iCell)
            case (VTK_PYRAMID)
                read (lines(iCell), *, iostat=iostat) vtk%CELLS(VTK_PYRAMID)%Nodes(1:5, iCell)
            case (VTK_QUADRATIC_EDGE)
                read (lines(iCell), *, iostat=iostat) vtk%CELLS(VTK_QUADRATIC_EDGE)%Nodes(1:3, iCell)
            case (VTK_QUADRATIC_TRIANGLE)
                read (lines(iCell), *, iostat=iostat) vtk%CELLS(VTK_QUADRATIC_TRIANGLE)%Nodes(1:6, iCell)
            case (VTK_QUADRATIC_QUAD)
                read (lines(iCell), *, iostat=iostat) vtk%CELLS(VTK_QUADRATIC_QUAD)%Nodes(1:8, iCell)
            case (VTK_QUADRATIC_TETRA)
                read (lines(iCell), *, iostat=iostat) vtk%CELLS(VTK_QUADRATIC_TETRA)%Nodes(1:10, iCell)
            case (VTK_QUADRATIC_HEXAHEDRON)
                read (lines(iCell), *, iostat=iostat) vtk%CELLS(VTK_QUADRATIC_HEXAHEDRON)%Nodes(1:20, iCell)
            end select
        end do

        do iCell = 1, 25
            if (allocated(vtk%CELLS(iCell)%Nodes_Array) .or. allocated(vtk%CELLS(iCell)%Nodes)) then
                if (allocated(vtk%CELLS(iCell)%Nodes_Array)) then
                    vtk%CELLS(iCell)%Nodes_Array(:) = vtk%CELLS(iCell)%Nodes_Array(:) + 1
                else if (allocated(vtk%CELLS(iCell)%Nodes)) then
                    vtk%CELLS(iCell)%Nodes(:, :) = vtk%CELLS(iCell)%Nodes(:, :) + 1
                end if
            end if
        end do

        read (unit, '(a)', iostat=iostat) ! Skip

        deallocate (CellType)

    end subroutine Inout_VTK_Read_Data_Cells_Types

    subroutine Inout_VTK_Read_Data_CellEntityIds(unit, vtk, headline)
        !> Read VTK data cell entity ids
        implicit none
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
        call Allocate_Vector(vtk%CellEntityIds, numCellEntityIds)

        read (unit, '(A)') line
        pos1 = index(line, space)
        pos2 = index(line(pos1 + 1:), space) + pos1
        read (line(pos1 + 1:pos2 - 1), '(a)') CellEntity
        if (CellEntity == c_CellEntityIds) then
            read (unit, *) ! Skip
            do iCellEntityId = 1, numCellEntityIds
                read (unit, '(i)') vtk%CellEntityIds(iCellEntityId)
            end do
        end if

    end subroutine Inout_VTK_Read_Data_CellEntityIds

end module Inout_VTK

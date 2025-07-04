module Core_VTK
    use, intrinsic :: iso_fortran_env
    use :: Core_BaseTypes, only:DP3d
    use :: Core_Allocate, only:Allocate_Array
    use :: Core_VTK_Constants
    use :: Core_Unique, only:Unique
    use :: stdlib_sorting, only:sort
    implicit none
    private

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
        ! type(VTK_CELL_NAMES) :: Names !! Cell names
    contains
        procedure :: Is_In => Core_VTK_IN_CellType
        procedure :: get_active_region_info
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
            if (iCellType == VTK_LINE .or. &
                iCellType == VTK_QUADRATIC_EDGE &
                ) then
                isIn = .true.
            end if
        case (2)
            if (iCellType == VTK_TRIANGLE .or. &
                iCellType == VTK_PIXEL .or. &
                iCellType == VTK_QUAD .or. &
                iCellType == VTK_QUADRATIC_TRIANGLE .or. &
                iCellType == VTK_QUADRATIC_QUAD &
                ) then
                isIn = .true.
            end if
        end select

    end function Core_VTK_IN_CellType

    subroutine get_active_region_info(self, unique_ids, ierr)
        ! --- 引数 ---
        implicit none
        class(Type_VTK), intent(in) :: self !! VTK data
        integer(int32), allocatable, intent(inout) :: unique_ids(:)
        integer(int32), intent(out) :: ierr

        ! --- ローカル変数 ---
        integer(int32) :: max_dim
        integer(int32), allocatable :: collected_ids(:)
        integer(int32) :: i_cell, count
        logical(4) :: is_max_dim_element

        max_dim = 0
        ierr = 0

        ! --- ステップ1: メッシュ内の最大次元を判定 ---
        do i_cell = 1, self%numTotalCells
            select case (self%CELLS(i_cell)%CellType)
            case (VTK_TETRA, VTK_HEXAHEDRON, &
                  VTK_WEDGE, VTK_PYRAMID, &
                  VTK_QUADRATIC_TETRA, VTK_QUADRATIC_HEXAHEDRON)
                max_dim = 3
                exit ! 3Dが見つかったら、それ以上探す必要はない
            case (VTK_TRIANGLE, VTK_PIXEL, &
                  VTK_QUAD, VTK_QUADRATIC_TRIANGLE, &
                  VTK_QUADRATIC_QUAD)
                max_dim = max(max_dim, 2)
            case (VTK_LINE, VTK_QUADRATIC_EDGE)
                max_dim = max(max_dim, 1)
            end select
        end do

        if (max_dim == 0) then
            ierr = -1
            return ! アクティブな要素がない
        end if

        ! --- ステップ2: 最大次元を持つ要素から、すべてのCellEntityIdを収集 ---
        allocate (collected_ids(self%numTotalCells))
        count = 0
        do i_cell = 1, self%numTotalCells
            is_max_dim_element = .false.
            select case (self%CELLS(i_cell)%CellType)
            case (VTK_TETRA, VTK_HEXAHEDRON, &
                  VTK_WEDGE, VTK_PYRAMID, &
                  VTK_QUADRATIC_TETRA, VTK_QUADRATIC_HEXAHEDRON)
                if (max_dim == 3) is_max_dim_element = .true.
            case (VTK_TRIANGLE, VTK_PIXEL, &
                  VTK_QUAD, VTK_QUADRATIC_TRIANGLE, &
                  VTK_QUADRATIC_QUAD)
                if (max_dim == 2) is_max_dim_element = .true.
            case (VTK_LINE, VTK_QUADRATIC_EDGE)
                if (max_dim == 1) is_max_dim_element = .true.
            end select

            if (is_max_dim_element) then
                count = count + 1
                collected_ids(count) = self%CELLS(i_cell)%CellEntityId
            end if
        end do

        ! --- ステップ3: 収集したIDリストから、ユニークなものだけを抽出 ---
        ! (これはFortranの標準的なユニーク化のアルゴリズム)
        if (count > 0) then
            call sort(collected_ids(1:count))
            call Unique(collected_ids(1:count), unique_ids)
        else
            allocate (unique_ids(0))
        end if

    end subroutine get_active_region_info

end module Core_VTK

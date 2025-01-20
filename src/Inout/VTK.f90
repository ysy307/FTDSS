module Inout_VTK
    use, intrinsic :: iso_fortran_env
    use :: Types
    implicit none

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
        call Inout_VTK_Read_Data_Points(new_unit, vtk)
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

    subroutine Inout_VTK_Read_Data_Points(unit, vtk)
        !> Read VTK data points
        implicit none
        integer(int32), intent(in) :: unit !! Unit number
        type(Type_VTK), intent(inout) :: vtk !! VTK data

        character(256) :: line, dtype
        character(16) :: keyword
        integer(int32) :: iostat, numPoints
        integer :: pos1, pos2

        read (unit, '(A)', iostat=iostat) line
        if (iostat /= 0) stop

        line = trim(adjustl(line))

        pos1 = index(line, space)
        if (pos1 == 0) stop
        keyword = line(1:pos1 - 1)

        pos2 = index(line(pos1 + 1:), space) + pos1
        if (pos2 == pos1) stop

        read (line(pos1 + 1:pos2 - 1), '(I)') numPoints

        dtype = line(pos2 + 1:)

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

    end subroutine Inout_VTK_Read_Data_Points

    subroutine Inout_VTK_Set_Data_Type(vtk_dtype, dtype)
        !> Set VTK data type
        implicit none
        character(*), intent(inout) :: vtk_dtype !! VTK data data type if you want to set
        character(*), intent(in) :: dtype !! Data type

        select case (dtype)
        case (c_unsigned_char)
            vtk_dtype = c_unsigned_char
        case (c_char)
            vtk_dtype = c_char
        case (c_unsigned_short)
            vtk_dtype = c_unsigned_short
        case (c_short)
            vtk_dtype = c_short
        case (c_unsigned_int)
            vtk_dtype = c_unsigned_int
        case (c_int)
            vtk_dtype = c_int
        case (c_unsigned_long)
            vtk_dtype = c_unsigned_long
        case (c_long)
            vtk_dtype = c_long
        case (c_float)
            vtk_dtype = c_float
        case (c_double)
            vtk_dtype = c_double
        case default
            print *, "Unknown data type: ", dtype
            stop
        end select

    end subroutine Inout_VTK_Set_Data_Type

end module Inout_VTK

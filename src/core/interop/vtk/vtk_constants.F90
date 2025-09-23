
module core_vtk_vtk_constants
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    private

    public :: initialize_vtk_constants
    public :: vtk_constants

    type :: type_vtk_constants_local
        character(:), allocatable :: cell_name
        integer(int32) :: cell_type
        integer(int32) :: num_nodes_in_cell
        integer(int32) :: cell_dimension
        integer(int32) :: cell_order
    end type type_vtk_constants_local

    type :: type_vtk_constants
        type(type_vtk_constants_local), allocatable :: cell(:)
    contains
        procedure, public :: get_cell_info
        procedure, public :: get_cell_name
        procedure, private :: get_cell_type
    end type type_vtk_constants

    type(type_vtk_constants) :: vtk_constants
    !------------------------------------------------------------------------
    ! Linear cells
    !------------------------------------------------------------------------
    integer(int32), parameter :: VTK_EMPTY_CELL = 0
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
    integer(int32), parameter :: VTK_PENTAGONAL_PRISM = 15
    integer(int32), parameter :: VTK_HEXAGONAL_PRISM = 16
    !------------------------------------------------------------------------
    ! Quadratic, isoparametric cells
    !------------------------------------------------------------------------
    integer(int32), parameter :: VTK_QUADRATIC_EDGE = 21
    integer(int32), parameter :: VTK_QUADRATIC_TRIANGLE = 22
    integer(int32), parameter :: VTK_QUADRATIC_QUAD = 23
    integer(int32), parameter :: VTK_QUADRATIC_POLYGON = 36
    integer(int32), parameter :: VTK_QUADRATIC_TETRA = 24
    integer(int32), parameter :: VTK_QUADRATIC_HEXAHEDRON = 25
    integer(int32), parameter :: VTK_QUADRATIC_WEDGE = 26
    integer(int32), parameter :: VTK_QUADRATIC_PYRAMID = 27
    integer(int32), parameter :: VTK_BIQUADRATIC_QUAD = 28
    integer(int32), parameter :: VTK_TRIQUADRATIC_HEXAHEDRON = 29
    integer(int32), parameter :: VTK_TRIQUADRATIC_PYRAMID = 37
    integer(int32), parameter :: VTK_QUADRATIC_LINEAR_QUAD = 30
    integer(int32), parameter :: VTK_QUADRATIC_LINEAR_WEDGE = 31
    integer(int32), parameter :: VTK_BIQUADRATIC_QUADRATIC_WEDGE = 32
    integer(int32), parameter :: VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON = 33
    integer(int32), parameter :: VTK_BIQUADRATIC_TRIANGLE = 34
    !------------------------------------------------------------------------
    ! Cubic, isoparametric cell
    !------------------------------------------------------------------------
    integer(int32), parameter :: VTK_CUBIC_LINE = 35
    !------------------------------------------------------------------------
    ! Special class of cells formed by convex group of points
    !------------------------------------------------------------------------
    integer(int32), parameter :: VTK_CONVEX_POINT_SET = 41
    !------------------------------------------------------------------------
    ! Polyhedron cell(consisting of polygonal faces)
    !------------------------------------------------------------------------
    integer(int32), parameter :: VTK_POLYHEDRON = 42
    !------------------------------------------------------------------------
    ! Higher order cells in parametric form
    !------------------------------------------------------------------------
    integer(int32), parameter :: VTK_PARAMETRIC_CURVE = 51
    integer(int32), parameter :: VTK_PARAMETRIC_SURFACE = 52
    integer(int32), parameter :: VTK_PARAMETRIC_TRI_SURFACE = 53
    integer(int32), parameter :: VTK_PARAMETRIC_QUAD_SURFACE = 54
    integer(int32), parameter :: VTK_PARAMETRIC_TETRA_REGION = 55
    integer(int32), parameter :: VTK_PARAMETRIC_HEX_REGION = 56
    !------------------------------------------------------------------------
    ! Higher order cells
    !------------------------------------------------------------------------
    integer(int32), parameter :: VTK_HIGHER_ORDER_EDGE = 60
    integer(int32), parameter :: VTK_HIGHER_ORDER_TRIANGLE = 61
    integer(int32), parameter :: VTK_HIGHER_ORDER_QUAD = 62
    integer(int32), parameter :: VTK_HIGHER_ORDER_POLYGON = 63
    integer(int32), parameter :: VTK_HIGHER_ORDER_TETRAHEDRON = 64
    integer(int32), parameter :: VTK_HIGHER_ORDER_WEDGE = 65
    integer(int32), parameter :: VTK_HIGHER_ORDER_PYRAMID = 66
    integer(int32), parameter :: VTK_HIGHER_ORDER_HEXAHEDRON = 67
    !------------------------------------------------------------------------
    ! Arbitrary order Lagrange elements(formulated separated from generic
    ! higher order cells)
    !------------------------------------------------------------------------
    integer(int32), parameter :: VTK_LAGRANGE_CURVE = 68
    integer(int32), parameter :: VTK_LAGRANGE_TRIANGLE = 69
    integer(int32), parameter :: VTK_LAGRANGE_QUADRILATERAL = 70
    integer(int32), parameter :: VTK_LAGRANGE_TETRAHEDRON = 71
    integer(int32), parameter :: VTK_LAGRANGE_HEXAHEDRON = 72
    integer(int32), parameter :: VTK_LAGRANGE_WEDGE = 73
    integer(int32), parameter :: VTK_LAGRANGE_PYRAMID = 74
    !------------------------------------------------------------------------
    ! Arbitrary order Bezier elements(formulated separated from generic
    ! higher order cells)
    !------------------------------------------------------------------------
    integer(int32), parameter :: VTK_BEZIER_CURVE = 75
    integer(int32), parameter :: VTK_BEZIER_TRIANGLE = 76
    integer(int32), parameter :: VTK_BEZIER_QUADRILATERAL = 77
    integer(int32), parameter :: VTK_BEZIER_TETRAHEDRON = 78
    integer(int32), parameter :: VTK_BEZIER_HEXAHEDRON = 79
    integer(int32), parameter :: VTK_BEZIER_WEDGE = 80
    integer(int32), parameter :: VTK_BEZIER_PYRAMID = 81
    !------------------------------------------------------------------------

    integer(int32), parameter :: NUM_VTK_CELL_TYPES = 81

contains
    subroutine initialize_vtk_constants()
        implicit none

        ! Allocate the array to hold all cell type definitions
        if (allocated(vtk_constants%cell)) deallocate (vtk_constants%cell)
        allocate (vtk_constants%cell(0:NUM_VTK_CELL_TYPES))

        !------------------------------------------------------------------------
        ! Initialize each cell type
        !------------------------------------------------------------------------

        ! VTK_EMPTY_CELL
        vtk_constants%cell(VTK_EMPTY_CELL)%cell_name = "Empty"
        vtk_constants%cell(VTK_EMPTY_CELL)%cell_type = VTK_EMPTY_CELL
        vtk_constants%cell(VTK_EMPTY_CELL)%num_nodes_in_cell = 0
        vtk_constants%cell(VTK_EMPTY_CELL)%cell_dimension = -1
        vtk_constants%cell(VTK_EMPTY_CELL)%cell_order = 0

        ! VTK_VERTEX
        vtk_constants%cell(VTK_VERTEX)%cell_name = "Vertex"
        vtk_constants%cell(VTK_VERTEX)%cell_type = VTK_VERTEX
        vtk_constants%cell(VTK_VERTEX)%num_nodes_in_cell = 1
        vtk_constants%cell(VTK_VERTEX)%cell_dimension = 0
        vtk_constants%cell(VTK_VERTEX)%cell_order = 1

        ! VTK_POLY_VERTEX
        vtk_constants%cell(VTK_POLY_VERTEX)%cell_name = "PolyVertex"
        vtk_constants%cell(VTK_POLY_VERTEX)%cell_type = VTK_POLY_VERTEX
        vtk_constants%cell(VTK_POLY_VERTEX)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_POLY_VERTEX)%cell_dimension = 0
        vtk_constants%cell(VTK_POLY_VERTEX)%cell_order = 1

        ! VTK_LINE
        vtk_constants%cell(VTK_LINE)%cell_name = "Line"
        vtk_constants%cell(VTK_LINE)%cell_type = VTK_LINE
        vtk_constants%cell(VTK_LINE)%num_nodes_in_cell = 2
        vtk_constants%cell(VTK_LINE)%cell_dimension = 1
        vtk_constants%cell(VTK_LINE)%cell_order = 1

        ! VTK_POLY_LINE
        vtk_constants%cell(VTK_POLY_LINE)%cell_name = "PolyLine"
        vtk_constants%cell(VTK_POLY_LINE)%cell_type = VTK_POLY_LINE
        vtk_constants%cell(VTK_POLY_LINE)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_POLY_LINE)%cell_dimension = 1
        vtk_constants%cell(VTK_POLY_LINE)%cell_order = -1 ! Variable

        ! VTK_TRIANGLE
        vtk_constants%cell(VTK_TRIANGLE)%cell_name = "Triangle"
        vtk_constants%cell(VTK_TRIANGLE)%cell_type = VTK_TRIANGLE
        vtk_constants%cell(VTK_TRIANGLE)%num_nodes_in_cell = 3
        vtk_constants%cell(VTK_TRIANGLE)%cell_dimension = 2
        vtk_constants%cell(VTK_TRIANGLE)%cell_order = 1

        ! VTK_TRIANGLE_STRIP
        vtk_constants%cell(VTK_TRIANGLE_STRIP)%cell_name = "TriangleStrip"
        vtk_constants%cell(VTK_TRIANGLE_STRIP)%cell_type = VTK_TRIANGLE_STRIP
        vtk_constants%cell(VTK_TRIANGLE_STRIP)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_TRIANGLE_STRIP)%cell_dimension = 2
        vtk_constants%cell(VTK_TRIANGLE_STRIP)%cell_order = 1

        ! VTK_POLYGON
        vtk_constants%cell(VTK_POLYGON)%cell_name = "Polygon"
        vtk_constants%cell(VTK_POLYGON)%cell_type = VTK_POLYGON
        vtk_constants%cell(VTK_POLYGON)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_POLYGON)%cell_dimension = 2
        vtk_constants%cell(VTK_POLYGON)%cell_order = -1 ! Variable

        ! VTK_PIXEL
        vtk_constants%cell(VTK_PIXEL)%cell_name = "Pixel"
        vtk_constants%cell(VTK_PIXEL)%cell_type = VTK_PIXEL
        vtk_constants%cell(VTK_PIXEL)%num_nodes_in_cell = 4
        vtk_constants%cell(VTK_PIXEL)%cell_dimension = 2
        vtk_constants%cell(VTK_PIXEL)%cell_order = 1

        ! VTK_QUAD
        vtk_constants%cell(VTK_QUAD)%cell_name = "Quad"
        vtk_constants%cell(VTK_QUAD)%cell_type = VTK_QUAD
        vtk_constants%cell(VTK_QUAD)%num_nodes_in_cell = 4
        vtk_constants%cell(VTK_QUAD)%cell_dimension = 2
        vtk_constants%cell(VTK_QUAD)%cell_order = 1

        ! VTK_TETRA
        vtk_constants%cell(VTK_TETRA)%cell_name = "Tetra"
        vtk_constants%cell(VTK_TETRA)%cell_type = VTK_TETRA
        vtk_constants%cell(VTK_TETRA)%num_nodes_in_cell = 4
        vtk_constants%cell(VTK_TETRA)%cell_dimension = 3
        vtk_constants%cell(VTK_TETRA)%cell_order = 1

        ! VTK_VOXEL
        vtk_constants%cell(VTK_VOXEL)%cell_name = "Voxel"
        vtk_constants%cell(VTK_VOXEL)%cell_type = VTK_VOXEL
        vtk_constants%cell(VTK_VOXEL)%num_nodes_in_cell = 8
        vtk_constants%cell(VTK_VOXEL)%cell_dimension = 3
        vtk_constants%cell(VTK_VOXEL)%cell_order = 1

        ! VTK_HEXAHEDRON
        vtk_constants%cell(VTK_HEXAHEDRON)%cell_name = "Hexahedron"
        vtk_constants%cell(VTK_HEXAHEDRON)%cell_type = VTK_HEXAHEDRON
        vtk_constants%cell(VTK_HEXAHEDRON)%num_nodes_in_cell = 8
        vtk_constants%cell(VTK_HEXAHEDRON)%cell_dimension = 3
        vtk_constants%cell(VTK_HEXAHEDRON)%cell_order = 1

        ! VTK_WEDGE
        vtk_constants%cell(VTK_WEDGE)%cell_name = "Wedge"
        vtk_constants%cell(VTK_WEDGE)%cell_type = VTK_WEDGE
        vtk_constants%cell(VTK_WEDGE)%num_nodes_in_cell = 6
        vtk_constants%cell(VTK_WEDGE)%cell_dimension = 3
        vtk_constants%cell(VTK_WEDGE)%cell_order = 1

        ! VTK_PYRAMID
        vtk_constants%cell(VTK_PYRAMID)%cell_name = "Pyramid"
        vtk_constants%cell(VTK_PYRAMID)%cell_type = VTK_PYRAMID
        vtk_constants%cell(VTK_PYRAMID)%num_nodes_in_cell = 5
        vtk_constants%cell(VTK_PYRAMID)%cell_dimension = 3
        vtk_constants%cell(VTK_PYRAMID)%cell_order = 1

        ! VTK_PENTAGONAL_PRISM
        vtk_constants%cell(VTK_PENTAGONAL_PRISM)%cell_name = "PentagonalPrism"
        vtk_constants%cell(VTK_PENTAGONAL_PRISM)%cell_type = VTK_PENTAGONAL_PRISM
        vtk_constants%cell(VTK_PENTAGONAL_PRISM)%num_nodes_in_cell = 10
        vtk_constants%cell(VTK_PENTAGONAL_PRISM)%cell_dimension = 3
        vtk_constants%cell(VTK_PENTAGONAL_PRISM)%cell_order = 1

        ! VTK_HEXAGONAL_PRISM
        vtk_constants%cell(VTK_HEXAGONAL_PRISM)%cell_name = "HexagonalPrism"
        vtk_constants%cell(VTK_HEXAGONAL_PRISM)%cell_type = VTK_HEXAGONAL_PRISM
        vtk_constants%cell(VTK_HEXAGONAL_PRISM)%num_nodes_in_cell = 12
        vtk_constants%cell(VTK_HEXAGONAL_PRISM)%cell_dimension = 3
        vtk_constants%cell(VTK_HEXAGONAL_PRISM)%cell_order = 1

        ! VTK_QUADRATIC_EDGE
        vtk_constants%cell(VTK_QUADRATIC_EDGE)%cell_name = "QuadraticEdge"
        vtk_constants%cell(VTK_QUADRATIC_EDGE)%cell_type = VTK_QUADRATIC_EDGE
        vtk_constants%cell(VTK_QUADRATIC_EDGE)%num_nodes_in_cell = 3
        vtk_constants%cell(VTK_QUADRATIC_EDGE)%cell_dimension = 1
        vtk_constants%cell(VTK_QUADRATIC_EDGE)%cell_order = 2

        ! VTK_QUADRATIC_TRIANGLE
        vtk_constants%cell(VTK_QUADRATIC_TRIANGLE)%cell_name = "QuadraticTriangle"
        vtk_constants%cell(VTK_QUADRATIC_TRIANGLE)%cell_type = VTK_QUADRATIC_TRIANGLE
        vtk_constants%cell(VTK_QUADRATIC_TRIANGLE)%num_nodes_in_cell = 6
        vtk_constants%cell(VTK_QUADRATIC_TRIANGLE)%cell_dimension = 2
        vtk_constants%cell(VTK_QUADRATIC_TRIANGLE)%cell_order = 2

        ! VTK_QUADRATIC_QUAD
        vtk_constants%cell(VTK_QUADRATIC_QUAD)%cell_name = "QuadraticQuad"
        vtk_constants%cell(VTK_QUADRATIC_QUAD)%cell_type = VTK_QUADRATIC_QUAD
        vtk_constants%cell(VTK_QUADRATIC_QUAD)%num_nodes_in_cell = 8
        vtk_constants%cell(VTK_QUADRATIC_QUAD)%cell_dimension = 2
        vtk_constants%cell(VTK_QUADRATIC_QUAD)%cell_order = 2

        ! VTK_QUADRATIC_POLYGON
        vtk_constants%cell(VTK_QUADRATIC_POLYGON)%cell_name = "QuadraticPolygon"
        vtk_constants%cell(VTK_QUADRATIC_POLYGON)%cell_type = VTK_QUADRATIC_POLYGON
        vtk_constants%cell(VTK_QUADRATIC_POLYGON)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_QUADRATIC_POLYGON)%cell_dimension = 2
        vtk_constants%cell(VTK_QUADRATIC_POLYGON)%cell_order = 2

        ! VTK_QUADRATIC_TETRA
        vtk_constants%cell(VTK_QUADRATIC_TETRA)%cell_name = "QuadraticTetra"
        vtk_constants%cell(VTK_QUADRATIC_TETRA)%cell_type = VTK_QUADRATIC_TETRA
        vtk_constants%cell(VTK_QUADRATIC_TETRA)%num_nodes_in_cell = 10
        vtk_constants%cell(VTK_QUADRATIC_TETRA)%cell_dimension = 3
        vtk_constants%cell(VTK_QUADRATIC_TETRA)%cell_order = 2

        ! VTK_QUADRATIC_HEXAHEDRON
        vtk_constants%cell(VTK_QUADRATIC_HEXAHEDRON)%cell_name = "QuadraticHexahedron"
        vtk_constants%cell(VTK_QUADRATIC_HEXAHEDRON)%cell_type = VTK_QUADRATIC_HEXAHEDRON
        vtk_constants%cell(VTK_QUADRATIC_HEXAHEDRON)%num_nodes_in_cell = 20
        vtk_constants%cell(VTK_QUADRATIC_HEXAHEDRON)%cell_dimension = 3
        vtk_constants%cell(VTK_QUADRATIC_HEXAHEDRON)%cell_order = 2

        ! VTK_QUADRATIC_WEDGE
        vtk_constants%cell(VTK_QUADRATIC_WEDGE)%cell_name = "QuadraticWedge"
        vtk_constants%cell(VTK_QUADRATIC_WEDGE)%cell_type = VTK_QUADRATIC_WEDGE
        vtk_constants%cell(VTK_QUADRATIC_WEDGE)%num_nodes_in_cell = 15
        vtk_constants%cell(VTK_QUADRATIC_WEDGE)%cell_dimension = 3
        vtk_constants%cell(VTK_QUADRATIC_WEDGE)%cell_order = 2

        ! VTK_QUADRATIC_PYRAMID
        vtk_constants%cell(VTK_QUADRATIC_PYRAMID)%cell_name = "QuadraticPyramid"
        vtk_constants%cell(VTK_QUADRATIC_PYRAMID)%cell_type = VTK_QUADRATIC_PYRAMID
        vtk_constants%cell(VTK_QUADRATIC_PYRAMID)%num_nodes_in_cell = 13
        vtk_constants%cell(VTK_QUADRATIC_PYRAMID)%cell_dimension = 3
        vtk_constants%cell(VTK_QUADRATIC_PYRAMID)%cell_order = 2

        ! VTK_BIQUADRATIC_QUAD
        vtk_constants%cell(VTK_BIQUADRATIC_QUAD)%cell_name = "BiQuadraticQuad"
        vtk_constants%cell(VTK_BIQUADRATIC_QUAD)%cell_type = VTK_BIQUADRATIC_QUAD
        vtk_constants%cell(VTK_BIQUADRATIC_QUAD)%num_nodes_in_cell = 9
        vtk_constants%cell(VTK_BIQUADRATIC_QUAD)%cell_dimension = 2
        vtk_constants%cell(VTK_BIQUADRATIC_QUAD)%cell_order = 2

        ! VTK_TRIQUADRATIC_HEXAHEDRON
        vtk_constants%cell(VTK_TRIQUADRATIC_HEXAHEDRON)%cell_name = "TriQuadraticHexahedron"
        vtk_constants%cell(VTK_TRIQUADRATIC_HEXAHEDRON)%cell_type = VTK_TRIQUADRATIC_HEXAHEDRON
        vtk_constants%cell(VTK_TRIQUADRATIC_HEXAHEDRON)%num_nodes_in_cell = 27
        vtk_constants%cell(VTK_TRIQUADRATIC_HEXAHEDRON)%cell_dimension = 3
        vtk_constants%cell(VTK_TRIQUADRATIC_HEXAHEDRON)%cell_order = 2

        ! VTK_TRIQUADRATIC_PYRAMID
        vtk_constants%cell(VTK_TRIQUADRATIC_PYRAMID)%cell_name = "TriQuadraticPyramid"
        vtk_constants%cell(VTK_TRIQUADRATIC_PYRAMID)%cell_type = VTK_TRIQUADRATIC_PYRAMID
        vtk_constants%cell(VTK_TRIQUADRATIC_PYRAMID)%num_nodes_in_cell = 19
        vtk_constants%cell(VTK_TRIQUADRATIC_PYRAMID)%cell_dimension = 3
        vtk_constants%cell(VTK_TRIQUADRATIC_PYRAMID)%cell_order = 2

        ! VTK_QUADRATIC_LINEAR_QUAD
        vtk_constants%cell(VTK_QUADRATIC_LINEAR_QUAD)%cell_name = "QuadraticLinearQuad"
        vtk_constants%cell(VTK_QUADRATIC_LINEAR_QUAD)%cell_type = VTK_QUADRATIC_LINEAR_QUAD
        vtk_constants%cell(VTK_QUADRATIC_LINEAR_QUAD)%num_nodes_in_cell = 6
        vtk_constants%cell(VTK_QUADRATIC_LINEAR_QUAD)%cell_dimension = 2
        vtk_constants%cell(VTK_QUADRATIC_LINEAR_QUAD)%cell_order = 2

        ! VTK_QUADRATIC_LINEAR_WEDGE
        vtk_constants%cell(VTK_QUADRATIC_LINEAR_WEDGE)%cell_name = "QuadraticLinearWedge"
        vtk_constants%cell(VTK_QUADRATIC_LINEAR_WEDGE)%cell_type = VTK_QUADRATIC_LINEAR_WEDGE
        vtk_constants%cell(VTK_QUADRATIC_LINEAR_WEDGE)%num_nodes_in_cell = 12
        vtk_constants%cell(VTK_QUADRATIC_LINEAR_WEDGE)%cell_dimension = 3
        vtk_constants%cell(VTK_QUADRATIC_LINEAR_WEDGE)%cell_order = 2

        ! VTK_BIQUADRATIC_QUADRATIC_WEDGE
        vtk_constants%cell(VTK_BIQUADRATIC_QUADRATIC_WEDGE)%cell_name = "BiQuadraticQuadraticWedge"
        vtk_constants%cell(VTK_BIQUADRATIC_QUADRATIC_WEDGE)%cell_type = VTK_BIQUADRATIC_QUADRATIC_WEDGE
        vtk_constants%cell(VTK_BIQUADRATIC_QUADRATIC_WEDGE)%num_nodes_in_cell = 18
        vtk_constants%cell(VTK_BIQUADRATIC_QUADRATIC_WEDGE)%cell_dimension = 3
        vtk_constants%cell(VTK_BIQUADRATIC_QUADRATIC_WEDGE)%cell_order = 2

        ! VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON
        vtk_constants%cell(VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON)%cell_name = "BiQuadraticQuadraticHexahedron"
        vtk_constants%cell(VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON)%cell_type = VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON
        vtk_constants%cell(VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON)%num_nodes_in_cell = 24
        vtk_constants%cell(VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON)%cell_dimension = 3
        vtk_constants%cell(VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON)%cell_order = 2

        ! VTK_BIQUADRATIC_TRIANGLE
        vtk_constants%cell(VTK_BIQUADRATIC_TRIANGLE)%cell_name = "BiQuadraticTriangle"
        vtk_constants%cell(VTK_BIQUADRATIC_TRIANGLE)%cell_type = VTK_BIQUADRATIC_TRIANGLE
        vtk_constants%cell(VTK_BIQUADRATIC_TRIANGLE)%num_nodes_in_cell = 7
        vtk_constants%cell(VTK_BIQUADRATIC_TRIANGLE)%cell_dimension = 2
        vtk_constants%cell(VTK_BIQUADRATIC_TRIANGLE)%cell_order = 2

        ! VTK_CUBIC_LINE
        vtk_constants%cell(VTK_CUBIC_LINE)%cell_name = "CubicLine"
        vtk_constants%cell(VTK_CUBIC_LINE)%cell_type = VTK_CUBIC_LINE
        vtk_constants%cell(VTK_CUBIC_LINE)%num_nodes_in_cell = 4
        vtk_constants%cell(VTK_CUBIC_LINE)%cell_dimension = 1
        vtk_constants%cell(VTK_CUBIC_LINE)%cell_order = 3

        ! VTK_CONVEX_POINT_SET
        vtk_constants%cell(VTK_CONVEX_POINT_SET)%cell_name = "ConvexPointSet"
        vtk_constants%cell(VTK_CONVEX_POINT_SET)%cell_type = VTK_CONVEX_POINT_SET
        vtk_constants%cell(VTK_CONVEX_POINT_SET)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_CONVEX_POINT_SET)%cell_dimension = -1 ! Variable
        vtk_constants%cell(VTK_CONVEX_POINT_SET)%cell_order = -1 ! Variable

        ! VTK_POLYHEDRON
        vtk_constants%cell(VTK_POLYHEDRON)%cell_name = "Polyhedron"
        vtk_constants%cell(VTK_POLYHEDRON)%cell_type = VTK_POLYHEDRON
        vtk_constants%cell(VTK_POLYHEDRON)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_POLYHEDRON)%cell_dimension = 3
        vtk_constants%cell(VTK_POLYHEDRON)%cell_order = 1

        ! VTK_PARAMETRIC_CURVE
        vtk_constants%cell(VTK_PARAMETRIC_CURVE)%cell_name = "ParametricCurve"
        vtk_constants%cell(VTK_PARAMETRIC_CURVE)%cell_type = VTK_PARAMETRIC_CURVE
        vtk_constants%cell(VTK_PARAMETRIC_CURVE)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_PARAMETRIC_CURVE)%cell_dimension = 1
        vtk_constants%cell(VTK_PARAMETRIC_CURVE)%cell_order = -1 ! Variable

        ! VTK_PARAMETRIC_SURFACE
        vtk_constants%cell(VTK_PARAMETRIC_SURFACE)%cell_name = "ParametricSurface"
        vtk_constants%cell(VTK_PARAMETRIC_SURFACE)%cell_type = VTK_PARAMETRIC_SURFACE
        vtk_constants%cell(VTK_PARAMETRIC_SURFACE)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_PARAMETRIC_SURFACE)%cell_dimension = 2
        vtk_constants%cell(VTK_PARAMETRIC_SURFACE)%cell_order = -1 ! Variable

        ! VTK_PARAMETRIC_TRI_SURFACE
        vtk_constants%cell(VTK_PARAMETRIC_TRI_SURFACE)%cell_name = "ParametricTriSurface"
        vtk_constants%cell(VTK_PARAMETRIC_TRI_SURFACE)%cell_type = VTK_PARAMETRIC_TRI_SURFACE
        vtk_constants%cell(VTK_PARAMETRIC_TRI_SURFACE)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_PARAMETRIC_TRI_SURFACE)%cell_dimension = 2
        vtk_constants%cell(VTK_PARAMETRIC_TRI_SURFACE)%cell_order = -1 ! Variable

        ! VTK_PARAMETRIC_QUAD_SURFACE
        vtk_constants%cell(VTK_PARAMETRIC_QUAD_SURFACE)%cell_name = "ParametricQuadSurface"
        vtk_constants%cell(VTK_PARAMETRIC_QUAD_SURFACE)%cell_type = VTK_PARAMETRIC_QUAD_SURFACE
        vtk_constants%cell(VTK_PARAMETRIC_QUAD_SURFACE)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_PARAMETRIC_QUAD_SURFACE)%cell_dimension = 2
        vtk_constants%cell(VTK_PARAMETRIC_QUAD_SURFACE)%cell_order = -1 ! Variable

        ! VTK_PARAMETRIC_TETRA_REGION
        vtk_constants%cell(VTK_PARAMETRIC_TETRA_REGION)%cell_name = "ParametricTetraRegion"
        vtk_constants%cell(VTK_PARAMETRIC_TETRA_REGION)%cell_type = VTK_PARAMETRIC_TETRA_REGION
        vtk_constants%cell(VTK_PARAMETRIC_TETRA_REGION)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_PARAMETRIC_TETRA_REGION)%cell_dimension = 3
        vtk_constants%cell(VTK_PARAMETRIC_TETRA_REGION)%cell_order = -1 ! Variable

        ! VTK_PARAMETRIC_HEX_REGION
        vtk_constants%cell(VTK_PARAMETRIC_HEX_REGION)%cell_name = "ParametricHexRegion"
        vtk_constants%cell(VTK_PARAMETRIC_HEX_REGION)%cell_type = VTK_PARAMETRIC_HEX_REGION
        vtk_constants%cell(VTK_PARAMETRIC_HEX_REGION)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_PARAMETRIC_HEX_REGION)%cell_dimension = 3
        vtk_constants%cell(VTK_PARAMETRIC_HEX_REGION)%cell_order = -1 ! Variable

        ! VTK_HIGHER_ORDER_EDGE
        vtk_constants%cell(VTK_HIGHER_ORDER_EDGE)%cell_name = "HigherOrderEdge"
        vtk_constants%cell(VTK_HIGHER_ORDER_EDGE)%cell_type = VTK_HIGHER_ORDER_EDGE
        vtk_constants%cell(VTK_HIGHER_ORDER_EDGE)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_HIGHER_ORDER_EDGE)%cell_dimension = 1
        vtk_constants%cell(VTK_HIGHER_ORDER_EDGE)%cell_order = -1 ! Variable

        ! VTK_HIGHER_ORDER_TRIANGLE
        vtk_constants%cell(VTK_HIGHER_ORDER_TRIANGLE)%cell_name = "HigherOrderTriangle"
        vtk_constants%cell(VTK_HIGHER_ORDER_TRIANGLE)%cell_type = VTK_HIGHER_ORDER_TRIANGLE
        vtk_constants%cell(VTK_HIGHER_ORDER_TRIANGLE)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_HIGHER_ORDER_TRIANGLE)%cell_dimension = 2
        vtk_constants%cell(VTK_HIGHER_ORDER_TRIANGLE)%cell_order = -1 ! Variable

        ! VTK_HIGHER_ORDER_QUAD
        vtk_constants%cell(VTK_HIGHER_ORDER_QUAD)%cell_name = "HigherOrderQuad"
        vtk_constants%cell(VTK_HIGHER_ORDER_QUAD)%cell_type = VTK_HIGHER_ORDER_QUAD
        vtk_constants%cell(VTK_HIGHER_ORDER_QUAD)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_HIGHER_ORDER_QUAD)%cell_dimension = 2
        vtk_constants%cell(VTK_HIGHER_ORDER_QUAD)%cell_order = -1 ! Variable

        ! VTK_HIGHER_ORDER_POLYGON
        vtk_constants%cell(VTK_HIGHER_ORDER_POLYGON)%cell_name = "HigherOrderPolygon"
        vtk_constants%cell(VTK_HIGHER_ORDER_POLYGON)%cell_type = VTK_HIGHER_ORDER_POLYGON
        vtk_constants%cell(VTK_HIGHER_ORDER_POLYGON)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_HIGHER_ORDER_POLYGON)%cell_dimension = 2
        vtk_constants%cell(VTK_HIGHER_ORDER_POLYGON)%cell_order = -1 ! Variable

        ! VTK_HIGHER_ORDER_TETRAHEDRON
        vtk_constants%cell(VTK_HIGHER_ORDER_TETRAHEDRON)%cell_name = "HigherOrderTetrahedron"
        vtk_constants%cell(VTK_HIGHER_ORDER_TETRAHEDRON)%cell_type = VTK_HIGHER_ORDER_TETRAHEDRON
        vtk_constants%cell(VTK_HIGHER_ORDER_TETRAHEDRON)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_HIGHER_ORDER_TETRAHEDRON)%cell_dimension = 3
        vtk_constants%cell(VTK_HIGHER_ORDER_TETRAHEDRON)%cell_order = -1 ! Variable

        ! VTK_HIGHER_ORDER_WEDGE
        vtk_constants%cell(VTK_HIGHER_ORDER_WEDGE)%cell_name = "HigherOrderWedge"
        vtk_constants%cell(VTK_HIGHER_ORDER_WEDGE)%cell_type = VTK_HIGHER_ORDER_WEDGE
        vtk_constants%cell(VTK_HIGHER_ORDER_WEDGE)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_HIGHER_ORDER_WEDGE)%cell_dimension = 3
        vtk_constants%cell(VTK_HIGHER_ORDER_WEDGE)%cell_order = -1 ! Variable

        ! VTK_HIGHER_ORDER_PYRAMID
        vtk_constants%cell(VTK_HIGHER_ORDER_PYRAMID)%cell_name = "HigherOrderPyramid"
        vtk_constants%cell(VTK_HIGHER_ORDER_PYRAMID)%cell_type = VTK_HIGHER_ORDER_PYRAMID
        vtk_constants%cell(VTK_HIGHER_ORDER_PYRAMID)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_HIGHER_ORDER_PYRAMID)%cell_dimension = 3
        vtk_constants%cell(VTK_HIGHER_ORDER_PYRAMID)%cell_order = -1 ! Variable

        ! VTK_HIGHER_ORDER_HEXAHEDRON
        vtk_constants%cell(VTK_HIGHER_ORDER_HEXAHEDRON)%cell_name = "HigherOrderHexahedron"
        vtk_constants%cell(VTK_HIGHER_ORDER_HEXAHEDRON)%cell_type = VTK_HIGHER_ORDER_HEXAHEDRON
        vtk_constants%cell(VTK_HIGHER_ORDER_HEXAHEDRON)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_HIGHER_ORDER_HEXAHEDRON)%cell_dimension = 3
        vtk_constants%cell(VTK_HIGHER_ORDER_HEXAHEDRON)%cell_order = -1 ! Variable

        ! VTK_LAGRANGE_CURVE
        vtk_constants%cell(VTK_LAGRANGE_CURVE)%cell_name = "LagrangeCurve"
        vtk_constants%cell(VTK_LAGRANGE_CURVE)%cell_type = VTK_LAGRANGE_CURVE
        vtk_constants%cell(VTK_LAGRANGE_CURVE)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_LAGRANGE_CURVE)%cell_dimension = 1
        vtk_constants%cell(VTK_LAGRANGE_CURVE)%cell_order = -1 ! Variable

        ! VTK_LAGRANGE_TRIANGLE
        vtk_constants%cell(VTK_LAGRANGE_TRIANGLE)%cell_name = "LagrangeTriangle"
        vtk_constants%cell(VTK_LAGRANGE_TRIANGLE)%cell_type = VTK_LAGRANGE_TRIANGLE
        vtk_constants%cell(VTK_LAGRANGE_TRIANGLE)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_LAGRANGE_TRIANGLE)%cell_dimension = 2
        vtk_constants%cell(VTK_LAGRANGE_TRIANGLE)%cell_order = -1 ! Variable

        ! VTK_LAGRANGE_QUADRILATERAL
        vtk_constants%cell(VTK_LAGRANGE_QUADRILATERAL)%cell_name = "LagrangeQuadrilateral"
        vtk_constants%cell(VTK_LAGRANGE_QUADRILATERAL)%cell_type = VTK_LAGRANGE_QUADRILATERAL
        vtk_constants%cell(VTK_LAGRANGE_QUADRILATERAL)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_LAGRANGE_QUADRILATERAL)%cell_dimension = 2
        vtk_constants%cell(VTK_LAGRANGE_QUADRILATERAL)%cell_order = -1 ! Variable

        ! VTK_LAGRANGE_TETRAHEDRON
        vtk_constants%cell(VTK_LAGRANGE_TETRAHEDRON)%cell_name = "LagrangeTetrahedron"
        vtk_constants%cell(VTK_LAGRANGE_TETRAHEDRON)%cell_type = VTK_LAGRANGE_TETRAHEDRON
        vtk_constants%cell(VTK_LAGRANGE_TETRAHEDRON)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_LAGRANGE_TETRAHEDRON)%cell_dimension = 3
        vtk_constants%cell(VTK_LAGRANGE_TETRAHEDRON)%cell_order = -1 ! Variable

        ! VTK_LAGRANGE_HEXAHEDRON
        vtk_constants%cell(VTK_LAGRANGE_HEXAHEDRON)%cell_name = "LagrangeHexahedron"
        vtk_constants%cell(VTK_LAGRANGE_HEXAHEDRON)%cell_type = VTK_LAGRANGE_HEXAHEDRON
        vtk_constants%cell(VTK_LAGRANGE_HEXAHEDRON)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_LAGRANGE_HEXAHEDRON)%cell_dimension = 3
        vtk_constants%cell(VTK_LAGRANGE_HEXAHEDRON)%cell_order = -1 ! Variable

        ! VTK_LAGRANGE_WEDGE
        vtk_constants%cell(VTK_LAGRANGE_WEDGE)%cell_name = "LagrangeWedge"
        vtk_constants%cell(VTK_LAGRANGE_WEDGE)%cell_type = VTK_LAGRANGE_WEDGE
        vtk_constants%cell(VTK_LAGRANGE_WEDGE)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_LAGRANGE_WEDGE)%cell_dimension = 3
        vtk_constants%cell(VTK_LAGRANGE_WEDGE)%cell_order = -1 ! Variable

        ! VTK_LAGRANGE_PYRAMID
        vtk_constants%cell(VTK_LAGRANGE_PYRAMID)%cell_name = "LagrangePyramid"
        vtk_constants%cell(VTK_LAGRANGE_PYRAMID)%cell_type = VTK_LAGRANGE_PYRAMID
        vtk_constants%cell(VTK_LAGRANGE_PYRAMID)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_LAGRANGE_PYRAMID)%cell_dimension = 3
        vtk_constants%cell(VTK_LAGRANGE_PYRAMID)%cell_order = -1 ! Variable

        ! VTK_BEZIER_CURVE
        vtk_constants%cell(VTK_BEZIER_CURVE)%cell_name = "BezierCurve"
        vtk_constants%cell(VTK_BEZIER_CURVE)%cell_type = VTK_BEZIER_CURVE
        vtk_constants%cell(VTK_BEZIER_CURVE)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_BEZIER_CURVE)%cell_dimension = 1
        vtk_constants%cell(VTK_BEZIER_CURVE)%cell_order = -1 ! Variable

        ! VTK_BEZIER_TRIANGLE
        vtk_constants%cell(VTK_BEZIER_TRIANGLE)%cell_name = "BezierTriangle"
        vtk_constants%cell(VTK_BEZIER_TRIANGLE)%cell_type = VTK_BEZIER_TRIANGLE
        vtk_constants%cell(VTK_BEZIER_TRIANGLE)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_BEZIER_TRIANGLE)%cell_dimension = 2
        vtk_constants%cell(VTK_BEZIER_TRIANGLE)%cell_order = -1 ! Variable

        ! VTK_BEZIER_QUADRILATERAL
        vtk_constants%cell(VTK_BEZIER_QUADRILATERAL)%cell_name = "BezierQuadrilateral"
        vtk_constants%cell(VTK_BEZIER_QUADRILATERAL)%cell_type = VTK_BEZIER_QUADRILATERAL
        vtk_constants%cell(VTK_BEZIER_QUADRILATERAL)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_BEZIER_QUADRILATERAL)%cell_dimension = 2
        vtk_constants%cell(VTK_BEZIER_QUADRILATERAL)%cell_order = -1 ! Variable

        ! VTK_BEZIER_TETRAHEDRON
        vtk_constants%cell(VTK_BEZIER_TETRAHEDRON)%cell_name = "BezierTetrahedron"
        vtk_constants%cell(VTK_BEZIER_TETRAHEDRON)%cell_type = VTK_BEZIER_TETRAHEDRON
        vtk_constants%cell(VTK_BEZIER_TETRAHEDRON)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_BEZIER_TETRAHEDRON)%cell_dimension = 3
        vtk_constants%cell(VTK_BEZIER_TETRAHEDRON)%cell_order = -1 ! Variable

        ! VTK_BEZIER_HEXAHEDRON
        vtk_constants%cell(VTK_BEZIER_HEXAHEDRON)%cell_name = "BezierHexahedron"
        vtk_constants%cell(VTK_BEZIER_HEXAHEDRON)%cell_type = VTK_BEZIER_HEXAHEDRON
        vtk_constants%cell(VTK_BEZIER_HEXAHEDRON)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_BEZIER_HEXAHEDRON)%cell_dimension = 3
        vtk_constants%cell(VTK_BEZIER_HEXAHEDRON)%cell_order = -1 ! Variable

        ! VTK_BEZIER_WEDGE
        vtk_constants%cell(VTK_BEZIER_WEDGE)%cell_name = "BezierWedge"
        vtk_constants%cell(VTK_BEZIER_WEDGE)%cell_type = VTK_BEZIER_WEDGE
        vtk_constants%cell(VTK_BEZIER_WEDGE)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_BEZIER_WEDGE)%cell_dimension = 3
        vtk_constants%cell(VTK_BEZIER_WEDGE)%cell_order = -1 ! Variable

        ! VTK_BEZIER_PYRAMID
        vtk_constants%cell(VTK_BEZIER_PYRAMID)%cell_name = "BezierPyramid"
        vtk_constants%cell(VTK_BEZIER_PYRAMID)%cell_type = VTK_BEZIER_PYRAMID
        vtk_constants%cell(VTK_BEZIER_PYRAMID)%num_nodes_in_cell = -1 ! Variable
        vtk_constants%cell(VTK_BEZIER_PYRAMID)%cell_dimension = 3
        vtk_constants%cell(VTK_BEZIER_PYRAMID)%cell_order = -1 ! Variable

    end subroutine initialize_vtk_constants

    function get_cell_type(self, cell_name) result(cell_type)
        implicit none
        class(type_vtk_constants), intent(in) :: self
        character(len=*), intent(in) :: cell_name
        integer(int32) :: cell_type

        select case (trim(adjustl(cell_name)))
        case ("EMPTY_CELL")
            cell_type = VTK_EMPTY_CELL
        case ("VERTEX")
            cell_type = VTK_VERTEX
        case ("POLY_VERTEX")
            cell_type = VTK_POLY_VERTEX
        case ("LINE")
            cell_type = VTK_LINE
        case ("POLY_LINE")
            cell_type = VTK_POLY_LINE
        case ("TRIANGLE")
            cell_type = VTK_TRIANGLE
        case ("TRIANGLE_STRIP")
            cell_type = VTK_TRIANGLE_STRIP
        case ("POLYGON")
            cell_type = VTK_POLYGON
        case ("PIXEL")
            cell_type = VTK_PIXEL
        case ("QUAD")
            cell_type = VTK_QUAD
        case ("TETRA")
            cell_type = VTK_TETRA
        case ("VOXEL")
            cell_type = VTK_VOXEL
        case ("HEXAHEDRON")
            cell_type = VTK_HEXAHEDRON
        case ("WEDGE")
            cell_type = VTK_WEDGE
        case ("PYRAMID")
            cell_type = VTK_PYRAMID
        case ("PENTAGONAL_PRISM")
            cell_type = VTK_PENTAGONAL_PRISM
        case ("HEXAGONAL_PRISM")
            cell_type = VTK_HEXAGONAL_PRISM
        case ("QUADRATIC_EDGE")
            cell_type = VTK_QUADRATIC_EDGE
        case ("QUADRATIC_TRIANGLE")
            cell_type = VTK_QUADRATIC_TRIANGLE
        case ("QUADRATIC_QUAD")
            cell_type = VTK_QUADRATIC_QUAD
        case ("QUADRATIC_POLYGON")
            cell_type = VTK_QUADRATIC_POLYGON
        case ("QUADRATIC_TETRA")
            cell_type = VTK_QUADRATIC_TETRA
        case ("QUADRATIC_HEXAHEDRON")
            cell_type = VTK_QUADRATIC_HEXAHEDRON
        case ("QUADRATIC_WEDGE")
            cell_type = VTK_QUADRATIC_WEDGE
        case ("QUADRATIC_PYRAMID")
            cell_type = VTK_QUADRATIC_PYRAMID
        case ("BIQUADRATIC_QUAD")
            cell_type = VTK_BIQUADRATIC_QUAD
        case ("TRIQUADRATIC_HEXAHEDRON")
            cell_type = VTK_TRIQUADRATIC_HEXAHEDRON
        case ("TRIQUADRATIC_PYRAMID")
            cell_type = VTK_TRIQUADRATIC_PYRAMID
        case ("QUADRATIC_LINEAR_QUAD")
            cell_type = VTK_QUADRATIC_LINEAR_QUAD
        case ("QUADRATIC_LINEAR_WEDGE")
            cell_type = VTK_QUADRATIC_LINEAR_WEDGE
        case ("BIQUADRATIC_QUADRATIC_WEDGE")
            cell_type = VTK_BIQUADRATIC_QUADRATIC_WEDGE
        case ("BIQUADRATIC_QUADRATIC_HEXAHEDRON")
            cell_type = VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON
        case ("BIQUADRATIC_TRIANGLE")
            cell_type = VTK_BIQUADRATIC_TRIANGLE
        case ("CUBIC_LINE")
            cell_type = VTK_CUBIC_LINE
        case ("CONVEX_POINT_SET")
            cell_type = VTK_CONVEX_POINT_SET
        case ("POLYHEDRON")
            cell_type = VTK_POLYHEDRON
        case ("PARAMETRIC_CURVE")
            cell_type = VTK_PARAMETRIC_CURVE
        case ("PARAMETRIC_SURFACE")
            cell_type = VTK_PARAMETRIC_SURFACE
        case ("PARAMETRIC_TRI_SURFACE")
            cell_type = VTK_PARAMETRIC_TRI_SURFACE
        case ("PARAMETRIC_QUAD_SURFACE")
            cell_type = VTK_PARAMETRIC_QUAD_SURFACE
        case ("PARAMETRIC_TETRA_REGION")
            cell_type = VTK_PARAMETRIC_TETRA_REGION
        case ("PARAMETRIC_HEX_REGION")
            cell_type = VTK_PARAMETRIC_HEX_REGION
        case ("HIGHER_ORDER_EDGE")
            cell_type = VTK_HIGHER_ORDER_EDGE
        case ("HIGHER_ORDER_TRIANGLE")
            cell_type = VTK_HIGHER_ORDER_TRIANGLE
        case ("HIGHER_ORDER_QUAD")
            cell_type = VTK_HIGHER_ORDER_QUAD
        case ("HIGHER_ORDER_POLYGON")
            cell_type = VTK_HIGHER_ORDER_POLYGON
        case ("HIGHER_ORDER_TETRAHEDRON")
            cell_type = VTK_HIGHER_ORDER_TETRAHEDRON
        case ("HIGHER_ORDER_WEDGE")
            cell_type = VTK_HIGHER_ORDER_WEDGE
        case ("HIGHER_ORDER_PYRAMID")
            cell_type = VTK_HIGHER_ORDER_PYRAMID
        case ("HIGHER_ORDER_HEXAHEDRON")
            cell_type = VTK_HIGHER_ORDER_HEXAHEDRON
        case ("LAGRANGE_CURVE")
            cell_type = VTK_LAGRANGE_CURVE
        case ("LAGRANGE_TRIANGLE")
            cell_type = VTK_LAGRANGE_TRIANGLE
        case ("LAGRANGE_QUADRILATERAL")
            cell_type = VTK_LAGRANGE_QUADRILATERAL
        case ("LAGRANGE_TETRAHEDRON")
            cell_type = VTK_LAGRANGE_TETRAHEDRON
        case ("LAGRANGE_HEXAHEDRON")
            cell_type = VTK_LAGRANGE_HEXAHEDRON
        case ("LAGRANGE_WEDGE")
            cell_type = VTK_LAGRANGE_WEDGE
        case ("LAGRANGE_PYRAMID")
            cell_type = VTK_LAGRANGE_PYRAMID
        case ("BEZIER_CURVE")
            cell_type = VTK_BEZIER_CURVE
        case ("BEZIER_TRIANGLE")
            cell_type = VTK_BEZIER_TRIANGLE
        case ("BEZIER_QUADRILATERAL")
            cell_type = VTK_BEZIER_QUADRILATERAL
        case ("BEZIER_TETRAHEDRON")
            cell_type = VTK_BEZIER_TETRAHEDRON
        case ("BEZIER_HEXAHEDRON")
            cell_type = VTK_BEZIER_HEXAHEDRON
        case ("BEZIER_WEDGE")
            cell_type = VTK_BEZIER_WEDGE
        case ("BEZIER_PYRAMID")
            cell_type = VTK_BEZIER_PYRAMID
        case default
            print *, "Error: Unknown VTK cell name ", trim(cell_name)
            stop
        end select
    end function get_cell_type

    subroutine get_cell_info(self, cell_name, cell_type, num_nodes_in_cell, cell_dimension, cell_order)
        class(type_vtk_constants), intent(in) :: self
        character(len=*), intent(in) :: cell_name
        integer(int32), intent(inout) :: cell_type
        integer(int32), intent(inout) :: num_nodes_in_cell
        integer(int32), intent(inout) :: cell_dimension
        integer(int32), intent(inout) :: cell_order

        cell_type = self%get_cell_type(trim(adjustl(cell_name)))
        num_nodes_in_cell = self%cell(cell_type)%num_nodes_in_cell
        cell_dimension = self%cell(cell_type)%cell_dimension
        cell_order = self%cell(cell_type)%cell_order

    end subroutine get_cell_info

    function get_cell_name(self, cell_type) result(cell_name)
        implicit none
        class(type_vtk_constants), intent(in) :: self
        integer(int32), intent(in) :: cell_type
        character(len=30) :: cell_name

        if (cell_type < 0 .or. cell_type > VTK_BEZIER_PYRAMID) then
            print *, "Error: Unknown VTK cell type ", cell_type
            stop
        end if

        cell_name = self%cell(cell_type)%cell_name

    end function get_cell_name
end module core_vtk_vtk_constants

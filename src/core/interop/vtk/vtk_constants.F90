
module core_interop_vtk_constants
    use, intrinsic :: iso_fortran_env, only: int32
    use :: stdlib_strings
    implicit none
    private

    public :: vtk_constants

    type :: type_vtk_constants_local
        character(:), allocatable :: cell_name
        integer(int32) :: cell_type
        integer(int32) :: num_nodes_in_cell
        integer(int32) :: cell_dimension
        integer(int32) :: cell_order
    end type type_vtk_constants_local

    type :: type_vtk_constants
        logical, private :: is_initialized = .false.
        integer(int32) :: max_cell_id
        type(type_vtk_constants_local), allocatable, private :: cell(:)
    contains
        procedure, public :: initialize => initialize_vtk_constants
        procedure, public :: get_cell_info_from_cell_name
        procedure, public :: get_cell_info_from_cell_type
        procedure, public :: get_cell_name
        procedure, public :: get_max_cell_id
        procedure, public :: get_cell_type
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
    subroutine initialize_vtk_constants(self)
        implicit none
        class(type_vtk_constants), intent(inout) :: self

        ! Allocate the array to hold all cell type definitions
        if (allocated(self%cell)) deallocate (self%cell)
        allocate (self%cell(0:NUM_VTK_CELL_TYPES))
        self%max_cell_id = NUM_VTK_CELL_TYPES

        !------------------------------------------------------------------------
        ! Initialize each cell type
        !------------------------------------------------------------------------

        ! VTK_EMPTY_CELL
        self%cell(VTK_EMPTY_CELL)%cell_name = "Empty"
        self%cell(VTK_EMPTY_CELL)%cell_type = VTK_EMPTY_CELL
        self%cell(VTK_EMPTY_CELL)%num_nodes_in_cell = 0
        self%cell(VTK_EMPTY_CELL)%cell_dimension = -1
        self%cell(VTK_EMPTY_CELL)%cell_order = 0

        ! VTK_VERTEX
        self%cell(VTK_VERTEX)%cell_name = "Vertex"
        self%cell(VTK_VERTEX)%cell_type = VTK_VERTEX
        self%cell(VTK_VERTEX)%num_nodes_in_cell = 1
        self%cell(VTK_VERTEX)%cell_dimension = 0
        self%cell(VTK_VERTEX)%cell_order = 1

        ! VTK_POLY_VERTEX
        self%cell(VTK_POLY_VERTEX)%cell_name = "PolyVertex"
        self%cell(VTK_POLY_VERTEX)%cell_type = VTK_POLY_VERTEX
        self%cell(VTK_POLY_VERTEX)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_POLY_VERTEX)%cell_dimension = 0
        self%cell(VTK_POLY_VERTEX)%cell_order = 1

        ! VTK_LINE
        self%cell(VTK_LINE)%cell_name = "Line"
        self%cell(VTK_LINE)%cell_type = VTK_LINE
        self%cell(VTK_LINE)%num_nodes_in_cell = 2
        self%cell(VTK_LINE)%cell_dimension = 1
        self%cell(VTK_LINE)%cell_order = 1

        ! VTK_POLY_LINE
        self%cell(VTK_POLY_LINE)%cell_name = "PolyLine"
        self%cell(VTK_POLY_LINE)%cell_type = VTK_POLY_LINE
        self%cell(VTK_POLY_LINE)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_POLY_LINE)%cell_dimension = 1
        self%cell(VTK_POLY_LINE)%cell_order = -1 ! Variable

        ! VTK_TRIANGLE
        self%cell(VTK_TRIANGLE)%cell_name = "Triangle"
        self%cell(VTK_TRIANGLE)%cell_type = VTK_TRIANGLE
        self%cell(VTK_TRIANGLE)%num_nodes_in_cell = 3
        self%cell(VTK_TRIANGLE)%cell_dimension = 2
        self%cell(VTK_TRIANGLE)%cell_order = 1

        ! VTK_TRIANGLE_STRIP
        self%cell(VTK_TRIANGLE_STRIP)%cell_name = "TriangleStrip"
        self%cell(VTK_TRIANGLE_STRIP)%cell_type = VTK_TRIANGLE_STRIP
        self%cell(VTK_TRIANGLE_STRIP)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_TRIANGLE_STRIP)%cell_dimension = 2
        self%cell(VTK_TRIANGLE_STRIP)%cell_order = 1

        ! VTK_POLYGON
        self%cell(VTK_POLYGON)%cell_name = "Polygon"
        self%cell(VTK_POLYGON)%cell_type = VTK_POLYGON
        self%cell(VTK_POLYGON)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_POLYGON)%cell_dimension = 2
        self%cell(VTK_POLYGON)%cell_order = -1 ! Variable

        ! VTK_PIXEL
        self%cell(VTK_PIXEL)%cell_name = "Pixel"
        self%cell(VTK_PIXEL)%cell_type = VTK_PIXEL
        self%cell(VTK_PIXEL)%num_nodes_in_cell = 4
        self%cell(VTK_PIXEL)%cell_dimension = 2
        self%cell(VTK_PIXEL)%cell_order = 1

        ! VTK_QUAD
        self%cell(VTK_QUAD)%cell_name = "Quad"
        self%cell(VTK_QUAD)%cell_type = VTK_QUAD
        self%cell(VTK_QUAD)%num_nodes_in_cell = 4
        self%cell(VTK_QUAD)%cell_dimension = 2
        self%cell(VTK_QUAD)%cell_order = 1

        ! VTK_TETRA
        self%cell(VTK_TETRA)%cell_name = "Tetra"
        self%cell(VTK_TETRA)%cell_type = VTK_TETRA
        self%cell(VTK_TETRA)%num_nodes_in_cell = 4
        self%cell(VTK_TETRA)%cell_dimension = 3
        self%cell(VTK_TETRA)%cell_order = 1

        ! VTK_VOXEL
        self%cell(VTK_VOXEL)%cell_name = "Voxel"
        self%cell(VTK_VOXEL)%cell_type = VTK_VOXEL
        self%cell(VTK_VOXEL)%num_nodes_in_cell = 8
        self%cell(VTK_VOXEL)%cell_dimension = 3
        self%cell(VTK_VOXEL)%cell_order = 1

        ! VTK_HEXAHEDRON
        self%cell(VTK_HEXAHEDRON)%cell_name = "Hexahedron"
        self%cell(VTK_HEXAHEDRON)%cell_type = VTK_HEXAHEDRON
        self%cell(VTK_HEXAHEDRON)%num_nodes_in_cell = 8
        self%cell(VTK_HEXAHEDRON)%cell_dimension = 3
        self%cell(VTK_HEXAHEDRON)%cell_order = 1

        ! VTK_WEDGE
        self%cell(VTK_WEDGE)%cell_name = "Wedge"
        self%cell(VTK_WEDGE)%cell_type = VTK_WEDGE
        self%cell(VTK_WEDGE)%num_nodes_in_cell = 6
        self%cell(VTK_WEDGE)%cell_dimension = 3
        self%cell(VTK_WEDGE)%cell_order = 1

        ! VTK_PYRAMID
        self%cell(VTK_PYRAMID)%cell_name = "Pyramid"
        self%cell(VTK_PYRAMID)%cell_type = VTK_PYRAMID
        self%cell(VTK_PYRAMID)%num_nodes_in_cell = 5
        self%cell(VTK_PYRAMID)%cell_dimension = 3
        self%cell(VTK_PYRAMID)%cell_order = 1

        ! VTK_PENTAGONAL_PRISM
        self%cell(VTK_PENTAGONAL_PRISM)%cell_name = "PentagonalPrism"
        self%cell(VTK_PENTAGONAL_PRISM)%cell_type = VTK_PENTAGONAL_PRISM
        self%cell(VTK_PENTAGONAL_PRISM)%num_nodes_in_cell = 10
        self%cell(VTK_PENTAGONAL_PRISM)%cell_dimension = 3
        self%cell(VTK_PENTAGONAL_PRISM)%cell_order = 1

        ! VTK_HEXAGONAL_PRISM
        self%cell(VTK_HEXAGONAL_PRISM)%cell_name = "HexagonalPrism"
        self%cell(VTK_HEXAGONAL_PRISM)%cell_type = VTK_HEXAGONAL_PRISM
        self%cell(VTK_HEXAGONAL_PRISM)%num_nodes_in_cell = 12
        self%cell(VTK_HEXAGONAL_PRISM)%cell_dimension = 3
        self%cell(VTK_HEXAGONAL_PRISM)%cell_order = 1

        ! VTK_QUADRATIC_EDGE
        self%cell(VTK_QUADRATIC_EDGE)%cell_name = "QuadraticEdge"
        self%cell(VTK_QUADRATIC_EDGE)%cell_type = VTK_QUADRATIC_EDGE
        self%cell(VTK_QUADRATIC_EDGE)%num_nodes_in_cell = 3
        self%cell(VTK_QUADRATIC_EDGE)%cell_dimension = 1
        self%cell(VTK_QUADRATIC_EDGE)%cell_order = 2

        ! VTK_QUADRATIC_TRIANGLE
        self%cell(VTK_QUADRATIC_TRIANGLE)%cell_name = "QuadraticTriangle"
        self%cell(VTK_QUADRATIC_TRIANGLE)%cell_type = VTK_QUADRATIC_TRIANGLE
        self%cell(VTK_QUADRATIC_TRIANGLE)%num_nodes_in_cell = 6
        self%cell(VTK_QUADRATIC_TRIANGLE)%cell_dimension = 2
        self%cell(VTK_QUADRATIC_TRIANGLE)%cell_order = 2

        ! VTK_QUADRATIC_QUAD
        self%cell(VTK_QUADRATIC_QUAD)%cell_name = "QuadraticQuad"
        self%cell(VTK_QUADRATIC_QUAD)%cell_type = VTK_QUADRATIC_QUAD
        self%cell(VTK_QUADRATIC_QUAD)%num_nodes_in_cell = 8
        self%cell(VTK_QUADRATIC_QUAD)%cell_dimension = 2
        self%cell(VTK_QUADRATIC_QUAD)%cell_order = 2

        ! VTK_QUADRATIC_POLYGON
        self%cell(VTK_QUADRATIC_POLYGON)%cell_name = "QuadraticPolygon"
        self%cell(VTK_QUADRATIC_POLYGON)%cell_type = VTK_QUADRATIC_POLYGON
        self%cell(VTK_QUADRATIC_POLYGON)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_QUADRATIC_POLYGON)%cell_dimension = 2
        self%cell(VTK_QUADRATIC_POLYGON)%cell_order = 2

        ! VTK_QUADRATIC_TETRA
        self%cell(VTK_QUADRATIC_TETRA)%cell_name = "QuadraticTetra"
        self%cell(VTK_QUADRATIC_TETRA)%cell_type = VTK_QUADRATIC_TETRA
        self%cell(VTK_QUADRATIC_TETRA)%num_nodes_in_cell = 10
        self%cell(VTK_QUADRATIC_TETRA)%cell_dimension = 3
        self%cell(VTK_QUADRATIC_TETRA)%cell_order = 2

        ! VTK_QUADRATIC_HEXAHEDRON
        self%cell(VTK_QUADRATIC_HEXAHEDRON)%cell_name = "QuadraticHexahedron"
        self%cell(VTK_QUADRATIC_HEXAHEDRON)%cell_type = VTK_QUADRATIC_HEXAHEDRON
        self%cell(VTK_QUADRATIC_HEXAHEDRON)%num_nodes_in_cell = 20
        self%cell(VTK_QUADRATIC_HEXAHEDRON)%cell_dimension = 3
        self%cell(VTK_QUADRATIC_HEXAHEDRON)%cell_order = 2

        ! VTK_QUADRATIC_WEDGE
        self%cell(VTK_QUADRATIC_WEDGE)%cell_name = "QuadraticWedge"
        self%cell(VTK_QUADRATIC_WEDGE)%cell_type = VTK_QUADRATIC_WEDGE
        self%cell(VTK_QUADRATIC_WEDGE)%num_nodes_in_cell = 15
        self%cell(VTK_QUADRATIC_WEDGE)%cell_dimension = 3
        self%cell(VTK_QUADRATIC_WEDGE)%cell_order = 2

        ! VTK_QUADRATIC_PYRAMID
        self%cell(VTK_QUADRATIC_PYRAMID)%cell_name = "QuadraticPyramid"
        self%cell(VTK_QUADRATIC_PYRAMID)%cell_type = VTK_QUADRATIC_PYRAMID
        self%cell(VTK_QUADRATIC_PYRAMID)%num_nodes_in_cell = 13
        self%cell(VTK_QUADRATIC_PYRAMID)%cell_dimension = 3
        self%cell(VTK_QUADRATIC_PYRAMID)%cell_order = 2

        ! VTK_BIQUADRATIC_QUAD
        self%cell(VTK_BIQUADRATIC_QUAD)%cell_name = "BiQuadraticQuad"
        self%cell(VTK_BIQUADRATIC_QUAD)%cell_type = VTK_BIQUADRATIC_QUAD
        self%cell(VTK_BIQUADRATIC_QUAD)%num_nodes_in_cell = 9
        self%cell(VTK_BIQUADRATIC_QUAD)%cell_dimension = 2
        self%cell(VTK_BIQUADRATIC_QUAD)%cell_order = 2

        ! VTK_TRIQUADRATIC_HEXAHEDRON
        self%cell(VTK_TRIQUADRATIC_HEXAHEDRON)%cell_name = "TriQuadraticHexahedron"
        self%cell(VTK_TRIQUADRATIC_HEXAHEDRON)%cell_type = VTK_TRIQUADRATIC_HEXAHEDRON
        self%cell(VTK_TRIQUADRATIC_HEXAHEDRON)%num_nodes_in_cell = 27
        self%cell(VTK_TRIQUADRATIC_HEXAHEDRON)%cell_dimension = 3
        self%cell(VTK_TRIQUADRATIC_HEXAHEDRON)%cell_order = 2

        ! VTK_TRIQUADRATIC_PYRAMID
        self%cell(VTK_TRIQUADRATIC_PYRAMID)%cell_name = "TriQuadraticPyramid"
        self%cell(VTK_TRIQUADRATIC_PYRAMID)%cell_type = VTK_TRIQUADRATIC_PYRAMID
        self%cell(VTK_TRIQUADRATIC_PYRAMID)%num_nodes_in_cell = 19
        self%cell(VTK_TRIQUADRATIC_PYRAMID)%cell_dimension = 3
        self%cell(VTK_TRIQUADRATIC_PYRAMID)%cell_order = 2

        ! VTK_QUADRATIC_LINEAR_QUAD
        self%cell(VTK_QUADRATIC_LINEAR_QUAD)%cell_name = "QuadraticLinearQuad"
        self%cell(VTK_QUADRATIC_LINEAR_QUAD)%cell_type = VTK_QUADRATIC_LINEAR_QUAD
        self%cell(VTK_QUADRATIC_LINEAR_QUAD)%num_nodes_in_cell = 6
        self%cell(VTK_QUADRATIC_LINEAR_QUAD)%cell_dimension = 2
        self%cell(VTK_QUADRATIC_LINEAR_QUAD)%cell_order = 2

        ! VTK_QUADRATIC_LINEAR_WEDGE
        self%cell(VTK_QUADRATIC_LINEAR_WEDGE)%cell_name = "QuadraticLinearWedge"
        self%cell(VTK_QUADRATIC_LINEAR_WEDGE)%cell_type = VTK_QUADRATIC_LINEAR_WEDGE
        self%cell(VTK_QUADRATIC_LINEAR_WEDGE)%num_nodes_in_cell = 12
        self%cell(VTK_QUADRATIC_LINEAR_WEDGE)%cell_dimension = 3
        self%cell(VTK_QUADRATIC_LINEAR_WEDGE)%cell_order = 2

        ! VTK_BIQUADRATIC_QUADRATIC_WEDGE
        self%cell(VTK_BIQUADRATIC_QUADRATIC_WEDGE)%cell_name = "BiQuadraticQuadraticWedge"
        self%cell(VTK_BIQUADRATIC_QUADRATIC_WEDGE)%cell_type = VTK_BIQUADRATIC_QUADRATIC_WEDGE
        self%cell(VTK_BIQUADRATIC_QUADRATIC_WEDGE)%num_nodes_in_cell = 18
        self%cell(VTK_BIQUADRATIC_QUADRATIC_WEDGE)%cell_dimension = 3
        self%cell(VTK_BIQUADRATIC_QUADRATIC_WEDGE)%cell_order = 2

        ! VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON
        self%cell(VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON)%cell_name = "BiQuadraticQuadraticHexahedron"
        self%cell(VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON)%cell_type = VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON
        self%cell(VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON)%num_nodes_in_cell = 24
        self%cell(VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON)%cell_dimension = 3
        self%cell(VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON)%cell_order = 2

        ! VTK_BIQUADRATIC_TRIANGLE
        self%cell(VTK_BIQUADRATIC_TRIANGLE)%cell_name = "BiQuadraticTriangle"
        self%cell(VTK_BIQUADRATIC_TRIANGLE)%cell_type = VTK_BIQUADRATIC_TRIANGLE
        self%cell(VTK_BIQUADRATIC_TRIANGLE)%num_nodes_in_cell = 7
        self%cell(VTK_BIQUADRATIC_TRIANGLE)%cell_dimension = 2
        self%cell(VTK_BIQUADRATIC_TRIANGLE)%cell_order = 2

        ! VTK_CUBIC_LINE
        self%cell(VTK_CUBIC_LINE)%cell_name = "CubicLine"
        self%cell(VTK_CUBIC_LINE)%cell_type = VTK_CUBIC_LINE
        self%cell(VTK_CUBIC_LINE)%num_nodes_in_cell = 4
        self%cell(VTK_CUBIC_LINE)%cell_dimension = 1
        self%cell(VTK_CUBIC_LINE)%cell_order = 3

        ! VTK_CONVEX_POINT_SET
        self%cell(VTK_CONVEX_POINT_SET)%cell_name = "ConvexPointSet"
        self%cell(VTK_CONVEX_POINT_SET)%cell_type = VTK_CONVEX_POINT_SET
        self%cell(VTK_CONVEX_POINT_SET)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_CONVEX_POINT_SET)%cell_dimension = -1 ! Variable
        self%cell(VTK_CONVEX_POINT_SET)%cell_order = -1 ! Variable

        ! VTK_POLYHEDRON
        self%cell(VTK_POLYHEDRON)%cell_name = "Polyhedron"
        self%cell(VTK_POLYHEDRON)%cell_type = VTK_POLYHEDRON
        self%cell(VTK_POLYHEDRON)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_POLYHEDRON)%cell_dimension = 3
        self%cell(VTK_POLYHEDRON)%cell_order = 1

        ! VTK_PARAMETRIC_CURVE
        self%cell(VTK_PARAMETRIC_CURVE)%cell_name = "ParametricCurve"
        self%cell(VTK_PARAMETRIC_CURVE)%cell_type = VTK_PARAMETRIC_CURVE
        self%cell(VTK_PARAMETRIC_CURVE)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_PARAMETRIC_CURVE)%cell_dimension = 1
        self%cell(VTK_PARAMETRIC_CURVE)%cell_order = -1 ! Variable

        ! VTK_PARAMETRIC_SURFACE
        self%cell(VTK_PARAMETRIC_SURFACE)%cell_name = "ParametricSurface"
        self%cell(VTK_PARAMETRIC_SURFACE)%cell_type = VTK_PARAMETRIC_SURFACE
        self%cell(VTK_PARAMETRIC_SURFACE)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_PARAMETRIC_SURFACE)%cell_dimension = 2
        self%cell(VTK_PARAMETRIC_SURFACE)%cell_order = -1 ! Variable

        ! VTK_PARAMETRIC_TRI_SURFACE
        self%cell(VTK_PARAMETRIC_TRI_SURFACE)%cell_name = "ParametricTriSurface"
        self%cell(VTK_PARAMETRIC_TRI_SURFACE)%cell_type = VTK_PARAMETRIC_TRI_SURFACE
        self%cell(VTK_PARAMETRIC_TRI_SURFACE)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_PARAMETRIC_TRI_SURFACE)%cell_dimension = 2
        self%cell(VTK_PARAMETRIC_TRI_SURFACE)%cell_order = -1 ! Variable

        ! VTK_PARAMETRIC_QUAD_SURFACE
        self%cell(VTK_PARAMETRIC_QUAD_SURFACE)%cell_name = "ParametricQuadSurface"
        self%cell(VTK_PARAMETRIC_QUAD_SURFACE)%cell_type = VTK_PARAMETRIC_QUAD_SURFACE
        self%cell(VTK_PARAMETRIC_QUAD_SURFACE)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_PARAMETRIC_QUAD_SURFACE)%cell_dimension = 2
        self%cell(VTK_PARAMETRIC_QUAD_SURFACE)%cell_order = -1 ! Variable

        ! VTK_PARAMETRIC_TETRA_REGION
        self%cell(VTK_PARAMETRIC_TETRA_REGION)%cell_name = "ParametricTetraRegion"
        self%cell(VTK_PARAMETRIC_TETRA_REGION)%cell_type = VTK_PARAMETRIC_TETRA_REGION
        self%cell(VTK_PARAMETRIC_TETRA_REGION)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_PARAMETRIC_TETRA_REGION)%cell_dimension = 3
        self%cell(VTK_PARAMETRIC_TETRA_REGION)%cell_order = -1 ! Variable

        ! VTK_PARAMETRIC_HEX_REGION
        self%cell(VTK_PARAMETRIC_HEX_REGION)%cell_name = "ParametricHexRegion"
        self%cell(VTK_PARAMETRIC_HEX_REGION)%cell_type = VTK_PARAMETRIC_HEX_REGION
        self%cell(VTK_PARAMETRIC_HEX_REGION)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_PARAMETRIC_HEX_REGION)%cell_dimension = 3
        self%cell(VTK_PARAMETRIC_HEX_REGION)%cell_order = -1 ! Variable

        ! VTK_HIGHER_ORDER_EDGE
        self%cell(VTK_HIGHER_ORDER_EDGE)%cell_name = "HigherOrderEdge"
        self%cell(VTK_HIGHER_ORDER_EDGE)%cell_type = VTK_HIGHER_ORDER_EDGE
        self%cell(VTK_HIGHER_ORDER_EDGE)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_HIGHER_ORDER_EDGE)%cell_dimension = 1
        self%cell(VTK_HIGHER_ORDER_EDGE)%cell_order = -1 ! Variable

        ! VTK_HIGHER_ORDER_TRIANGLE
        self%cell(VTK_HIGHER_ORDER_TRIANGLE)%cell_name = "HigherOrderTriangle"
        self%cell(VTK_HIGHER_ORDER_TRIANGLE)%cell_type = VTK_HIGHER_ORDER_TRIANGLE
        self%cell(VTK_HIGHER_ORDER_TRIANGLE)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_HIGHER_ORDER_TRIANGLE)%cell_dimension = 2
        self%cell(VTK_HIGHER_ORDER_TRIANGLE)%cell_order = -1 ! Variable

        ! VTK_HIGHER_ORDER_QUAD
        self%cell(VTK_HIGHER_ORDER_QUAD)%cell_name = "HigherOrderQuad"
        self%cell(VTK_HIGHER_ORDER_QUAD)%cell_type = VTK_HIGHER_ORDER_QUAD
        self%cell(VTK_HIGHER_ORDER_QUAD)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_HIGHER_ORDER_QUAD)%cell_dimension = 2
        self%cell(VTK_HIGHER_ORDER_QUAD)%cell_order = -1 ! Variable

        ! VTK_HIGHER_ORDER_POLYGON
        self%cell(VTK_HIGHER_ORDER_POLYGON)%cell_name = "HigherOrderPolygon"
        self%cell(VTK_HIGHER_ORDER_POLYGON)%cell_type = VTK_HIGHER_ORDER_POLYGON
        self%cell(VTK_HIGHER_ORDER_POLYGON)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_HIGHER_ORDER_POLYGON)%cell_dimension = 2
        self%cell(VTK_HIGHER_ORDER_POLYGON)%cell_order = -1 ! Variable

        ! VTK_HIGHER_ORDER_TETRAHEDRON
        self%cell(VTK_HIGHER_ORDER_TETRAHEDRON)%cell_name = "HigherOrderTetrahedron"
        self%cell(VTK_HIGHER_ORDER_TETRAHEDRON)%cell_type = VTK_HIGHER_ORDER_TETRAHEDRON
        self%cell(VTK_HIGHER_ORDER_TETRAHEDRON)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_HIGHER_ORDER_TETRAHEDRON)%cell_dimension = 3
        self%cell(VTK_HIGHER_ORDER_TETRAHEDRON)%cell_order = -1 ! Variable

        ! VTK_HIGHER_ORDER_WEDGE
        self%cell(VTK_HIGHER_ORDER_WEDGE)%cell_name = "HigherOrderWedge"
        self%cell(VTK_HIGHER_ORDER_WEDGE)%cell_type = VTK_HIGHER_ORDER_WEDGE
        self%cell(VTK_HIGHER_ORDER_WEDGE)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_HIGHER_ORDER_WEDGE)%cell_dimension = 3
        self%cell(VTK_HIGHER_ORDER_WEDGE)%cell_order = -1 ! Variable

        ! VTK_HIGHER_ORDER_PYRAMID
        self%cell(VTK_HIGHER_ORDER_PYRAMID)%cell_name = "HigherOrderPyramid"
        self%cell(VTK_HIGHER_ORDER_PYRAMID)%cell_type = VTK_HIGHER_ORDER_PYRAMID
        self%cell(VTK_HIGHER_ORDER_PYRAMID)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_HIGHER_ORDER_PYRAMID)%cell_dimension = 3
        self%cell(VTK_HIGHER_ORDER_PYRAMID)%cell_order = -1 ! Variable

        ! VTK_HIGHER_ORDER_HEXAHEDRON
        self%cell(VTK_HIGHER_ORDER_HEXAHEDRON)%cell_name = "HigherOrderHexahedron"
        self%cell(VTK_HIGHER_ORDER_HEXAHEDRON)%cell_type = VTK_HIGHER_ORDER_HEXAHEDRON
        self%cell(VTK_HIGHER_ORDER_HEXAHEDRON)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_HIGHER_ORDER_HEXAHEDRON)%cell_dimension = 3
        self%cell(VTK_HIGHER_ORDER_HEXAHEDRON)%cell_order = -1 ! Variable

        ! VTK_LAGRANGE_CURVE
        self%cell(VTK_LAGRANGE_CURVE)%cell_name = "LagrangeCurve"
        self%cell(VTK_LAGRANGE_CURVE)%cell_type = VTK_LAGRANGE_CURVE
        self%cell(VTK_LAGRANGE_CURVE)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_LAGRANGE_CURVE)%cell_dimension = 1
        self%cell(VTK_LAGRANGE_CURVE)%cell_order = -1 ! Variable

        ! VTK_LAGRANGE_TRIANGLE
        self%cell(VTK_LAGRANGE_TRIANGLE)%cell_name = "LagrangeTriangle"
        self%cell(VTK_LAGRANGE_TRIANGLE)%cell_type = VTK_LAGRANGE_TRIANGLE
        self%cell(VTK_LAGRANGE_TRIANGLE)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_LAGRANGE_TRIANGLE)%cell_dimension = 2
        self%cell(VTK_LAGRANGE_TRIANGLE)%cell_order = -1 ! Variable

        ! VTK_LAGRANGE_QUADRILATERAL
        self%cell(VTK_LAGRANGE_QUADRILATERAL)%cell_name = "LagrangeQuadrilateral"
        self%cell(VTK_LAGRANGE_QUADRILATERAL)%cell_type = VTK_LAGRANGE_QUADRILATERAL
        self%cell(VTK_LAGRANGE_QUADRILATERAL)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_LAGRANGE_QUADRILATERAL)%cell_dimension = 2
        self%cell(VTK_LAGRANGE_QUADRILATERAL)%cell_order = -1 ! Variable

        ! VTK_LAGRANGE_TETRAHEDRON
        self%cell(VTK_LAGRANGE_TETRAHEDRON)%cell_name = "LagrangeTetrahedron"
        self%cell(VTK_LAGRANGE_TETRAHEDRON)%cell_type = VTK_LAGRANGE_TETRAHEDRON
        self%cell(VTK_LAGRANGE_TETRAHEDRON)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_LAGRANGE_TETRAHEDRON)%cell_dimension = 3
        self%cell(VTK_LAGRANGE_TETRAHEDRON)%cell_order = -1 ! Variable

        ! VTK_LAGRANGE_HEXAHEDRON
        self%cell(VTK_LAGRANGE_HEXAHEDRON)%cell_name = "LagrangeHexahedron"
        self%cell(VTK_LAGRANGE_HEXAHEDRON)%cell_type = VTK_LAGRANGE_HEXAHEDRON
        self%cell(VTK_LAGRANGE_HEXAHEDRON)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_LAGRANGE_HEXAHEDRON)%cell_dimension = 3
        self%cell(VTK_LAGRANGE_HEXAHEDRON)%cell_order = -1 ! Variable

        ! VTK_LAGRANGE_WEDGE
        self%cell(VTK_LAGRANGE_WEDGE)%cell_name = "LagrangeWedge"
        self%cell(VTK_LAGRANGE_WEDGE)%cell_type = VTK_LAGRANGE_WEDGE
        self%cell(VTK_LAGRANGE_WEDGE)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_LAGRANGE_WEDGE)%cell_dimension = 3
        self%cell(VTK_LAGRANGE_WEDGE)%cell_order = -1 ! Variable

        ! VTK_LAGRANGE_PYRAMID
        self%cell(VTK_LAGRANGE_PYRAMID)%cell_name = "LagrangePyramid"
        self%cell(VTK_LAGRANGE_PYRAMID)%cell_type = VTK_LAGRANGE_PYRAMID
        self%cell(VTK_LAGRANGE_PYRAMID)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_LAGRANGE_PYRAMID)%cell_dimension = 3
        self%cell(VTK_LAGRANGE_PYRAMID)%cell_order = -1 ! Variable

        ! VTK_BEZIER_CURVE
        self%cell(VTK_BEZIER_CURVE)%cell_name = "BezierCurve"
        self%cell(VTK_BEZIER_CURVE)%cell_type = VTK_BEZIER_CURVE
        self%cell(VTK_BEZIER_CURVE)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_BEZIER_CURVE)%cell_dimension = 1
        self%cell(VTK_BEZIER_CURVE)%cell_order = -1 ! Variable

        ! VTK_BEZIER_TRIANGLE
        self%cell(VTK_BEZIER_TRIANGLE)%cell_name = "BezierTriangle"
        self%cell(VTK_BEZIER_TRIANGLE)%cell_type = VTK_BEZIER_TRIANGLE
        self%cell(VTK_BEZIER_TRIANGLE)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_BEZIER_TRIANGLE)%cell_dimension = 2
        self%cell(VTK_BEZIER_TRIANGLE)%cell_order = -1 ! Variable

        ! VTK_BEZIER_QUADRILATERAL
        self%cell(VTK_BEZIER_QUADRILATERAL)%cell_name = "BezierQuadrilateral"
        self%cell(VTK_BEZIER_QUADRILATERAL)%cell_type = VTK_BEZIER_QUADRILATERAL
        self%cell(VTK_BEZIER_QUADRILATERAL)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_BEZIER_QUADRILATERAL)%cell_dimension = 2
        self%cell(VTK_BEZIER_QUADRILATERAL)%cell_order = -1 ! Variable

        ! VTK_BEZIER_TETRAHEDRON
        self%cell(VTK_BEZIER_TETRAHEDRON)%cell_name = "BezierTetrahedron"
        self%cell(VTK_BEZIER_TETRAHEDRON)%cell_type = VTK_BEZIER_TETRAHEDRON
        self%cell(VTK_BEZIER_TETRAHEDRON)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_BEZIER_TETRAHEDRON)%cell_dimension = 3
        self%cell(VTK_BEZIER_TETRAHEDRON)%cell_order = -1 ! Variable

        ! VTK_BEZIER_HEXAHEDRON
        self%cell(VTK_BEZIER_HEXAHEDRON)%cell_name = "BezierHexahedron"
        self%cell(VTK_BEZIER_HEXAHEDRON)%cell_type = VTK_BEZIER_HEXAHEDRON
        self%cell(VTK_BEZIER_HEXAHEDRON)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_BEZIER_HEXAHEDRON)%cell_dimension = 3
        self%cell(VTK_BEZIER_HEXAHEDRON)%cell_order = -1 ! Variable

        ! VTK_BEZIER_WEDGE
        self%cell(VTK_BEZIER_WEDGE)%cell_name = "BezierWedge"
        self%cell(VTK_BEZIER_WEDGE)%cell_type = VTK_BEZIER_WEDGE
        self%cell(VTK_BEZIER_WEDGE)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_BEZIER_WEDGE)%cell_dimension = 3
        self%cell(VTK_BEZIER_WEDGE)%cell_order = -1 ! Variable

        ! VTK_BEZIER_PYRAMID
        self%cell(VTK_BEZIER_PYRAMID)%cell_name = "BezierPyramid"
        self%cell(VTK_BEZIER_PYRAMID)%cell_type = VTK_BEZIER_PYRAMID
        self%cell(VTK_BEZIER_PYRAMID)%num_nodes_in_cell = -1 ! Variable
        self%cell(VTK_BEZIER_PYRAMID)%cell_dimension = 3
        self%cell(VTK_BEZIER_PYRAMID)%cell_order = -1 ! Variable

        self%is_initialized = .true.

    end subroutine initialize_vtk_constants

    function get_cell_type(self, cell_name) result(cell_type)
        implicit none
        class(type_vtk_constants), intent(in) :: self
        character(len=*), intent(in) :: cell_name
        integer(int32) :: cell_type

        select case (strip(cell_name))
        case ("Empty")
            cell_type = VTK_EMPTY_CELL
        case ("Vertex")
            cell_type = VTK_VERTEX
        case ("PolyVertex")
            cell_type = VTK_POLY_VERTEX
        case ("Line")
            cell_type = VTK_LINE
        case ("PolyLine")
            cell_type = VTK_POLY_LINE
        case ("Triangle")
            cell_type = VTK_TRIANGLE
        case ("TriangleStrip")
            cell_type = VTK_TRIANGLE_STRIP
        case ("Polygon")
            cell_type = VTK_POLYGON
        case ("Pixel")
            cell_type = VTK_PIXEL
        case ("Quad")
            cell_type = VTK_QUAD
        case ("Tetra")
            cell_type = VTK_TETRA
        case ("Voxel")
            cell_type = VTK_VOXEL
        case ("Hexahedron")
            cell_type = VTK_HEXAHEDRON
        case ("Wedge")
            cell_type = VTK_WEDGE
        case ("Pyramid")
            cell_type = VTK_PYRAMID
        case ("PentagonalPrism")
            cell_type = VTK_PENTAGONAL_PRISM
        case ("HexagonalPrism")
            cell_type = VTK_HEXAGONAL_PRISM
        case ("QuadraticEdge")
            cell_type = VTK_QUADRATIC_EDGE
        case ("QuadraticTriangle")
            cell_type = VTK_QUADRATIC_TRIANGLE
        case ("QuadraticQuad")
            cell_type = VTK_QUADRATIC_QUAD
        case ("QuadraticPolygon")
            cell_type = VTK_QUADRATIC_POLYGON
        case ("QuadraticTetra")
            cell_type = VTK_QUADRATIC_TETRA
        case ("QuadraticHexahedron")
            cell_type = VTK_QUADRATIC_HEXAHEDRON
        case ("QuadraticWedge")
            cell_type = VTK_QUADRATIC_WEDGE
        case ("QuadraticPyramid")
            cell_type = VTK_QUADRATIC_PYRAMID
        case ("BiquadraticQuad")
            cell_type = VTK_BIQUADRATIC_QUAD
        case ("TriquadraticHexahedron")
            cell_type = VTK_TRIQUADRATIC_HEXAHEDRON
        case ("TriquadraticPyramid")
            cell_type = VTK_TRIQUADRATIC_PYRAMID
        case ("QuadraticLinearQuad")
            cell_type = VTK_QUADRATIC_LINEAR_QUAD
        case ("QuadraticLinearWedge")
            cell_type = VTK_QUADRATIC_LINEAR_WEDGE
        case ("BiquadraticQuadraticWedge")
            cell_type = VTK_BIQUADRATIC_QUADRATIC_WEDGE
        case ("BiquadraticQuadraticHexahedron")
            cell_type = VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON
        case ("BiquadraticTriangle")
            cell_type = VTK_BIQUADRATIC_TRIANGLE
        case ("CubicLine")
            cell_type = VTK_CUBIC_LINE
        case ("ConvexPointSet")
            cell_type = VTK_CONVEX_POINT_SET
        case ("Polyhedron")
            cell_type = VTK_POLYHEDRON
        case ("ParametricCurve")
            cell_type = VTK_PARAMETRIC_CURVE
        case ("ParametricSurface")
            cell_type = VTK_PARAMETRIC_SURFACE
        case ("ParametricTriSurface")
            cell_type = VTK_PARAMETRIC_TRI_SURFACE
        case ("ParametricQuadSurface")
            cell_type = VTK_PARAMETRIC_QUAD_SURFACE
        case ("ParametricTetraRegion")
            cell_type = VTK_PARAMETRIC_TETRA_REGION
        case ("ParametricHexRegion")
            cell_type = VTK_PARAMETRIC_HEX_REGION
        case ("HigherOrderEdge")
            cell_type = VTK_HIGHER_ORDER_EDGE
        case ("HigherOrderTriangle")
            cell_type = VTK_HIGHER_ORDER_TRIANGLE
        case ("HigherOrderQuad")
            cell_type = VTK_HIGHER_ORDER_QUAD
        case ("HigherOrderPolygon")
            cell_type = VTK_HIGHER_ORDER_POLYGON
        case ("HigherOrderTetrahedron")
            cell_type = VTK_HIGHER_ORDER_TETRAHEDRON
        case ("HigherOrderWedge")
            cell_type = VTK_HIGHER_ORDER_WEDGE
        case ("HigherOrderPyramid")
            cell_type = VTK_HIGHER_ORDER_PYRAMID
        case ("HigherOrderHexahedron")
            cell_type = VTK_HIGHER_ORDER_HEXAHEDRON
        case ("LagrangeCurve")
            cell_type = VTK_LAGRANGE_CURVE
        case ("LagrangeTriangle")
            cell_type = VTK_LAGRANGE_TRIANGLE
        case ("LagrangeQuadrilateral")
            cell_type = VTK_LAGRANGE_QUADRILATERAL
        case ("LagrangeTetrahedron")
            cell_type = VTK_LAGRANGE_TETRAHEDRON
        case ("LagrangeHexahedron")
            cell_type = VTK_LAGRANGE_HEXAHEDRON
        case ("LagrangeWedge")
            cell_type = VTK_LAGRANGE_WEDGE
        case ("LagrangePyramid")
            cell_type = VTK_LAGRANGE_PYRAMID
        case ("BezierCurve")
            cell_type = VTK_BEZIER_CURVE
        case ("BezierTriangle")
            cell_type = VTK_BEZIER_TRIANGLE
        case ("BezierQuadrilateral")
            cell_type = VTK_BEZIER_QUADRILATERAL
        case ("BezierTetrahedron")
            cell_type = VTK_BEZIER_TETRAHEDRON
        case ("BezierHexahedron")
            cell_type = VTK_BEZIER_HEXAHEDRON
        case ("BezierWedge")
            cell_type = VTK_BEZIER_WEDGE
        case ("BezierPyramid")
            cell_type = VTK_BEZIER_PYRAMID
        case default
            print *, "Error: Unknown VTK cell name ", strip(cell_name)
            stop
        end select
    end function get_cell_type

    subroutine get_cell_info_from_cell_name(self, cell_name, cell_type, num_nodes_in_cell, cell_dimension, cell_order)
        implicit none
        class(type_vtk_constants), intent(inout) :: self
        character(len=*), intent(in) :: cell_name
        integer(int32), intent(inout) :: cell_type
        integer(int32), intent(inout) :: num_nodes_in_cell
        integer(int32), intent(inout) :: cell_dimension
        integer(int32), intent(inout) :: cell_order

        if (.not. self%is_initialized) call self%initialize()

        cell_type = self%get_cell_type(strip(cell_name))
        num_nodes_in_cell = self%cell(cell_type)%num_nodes_in_cell
        cell_dimension = self%cell(cell_type)%cell_dimension
        cell_order = self%cell(cell_type)%cell_order

    end subroutine get_cell_info_from_cell_name

    subroutine get_cell_info_from_cell_type(self, cell_type, cell_name, num_nodes_in_cell, cell_dimension, cell_order)
        implicit none
        class(type_vtk_constants), intent(inout) :: self
        integer(int32), intent(in) :: cell_type
        character(:), allocatable, intent(inout) :: cell_name
        integer(int32), intent(inout) :: num_nodes_in_cell
        integer(int32), intent(inout) :: cell_dimension
        integer(int32), intent(inout) :: cell_order

        if (.not. self%is_initialized) call self%initialize()

        if (cell_type < 0 .or. cell_type > VTK_BEZIER_PYRAMID) then
            print *, "Error: Unknown VTK cell type ", cell_type
            stop
        end if

        cell_name = self%cell(cell_type)%cell_name
        num_nodes_in_cell = self%cell(cell_type)%num_nodes_in_cell
        cell_dimension = self%cell(cell_type)%cell_dimension
        cell_order = self%cell(cell_type)%cell_order

    end subroutine get_cell_info_from_cell_type

    function get_cell_name(self, cell_type) result(cell_name)
        implicit none
        class(type_vtk_constants), intent(inout) :: self
        integer(int32), intent(in) :: cell_type
        character(len=30) :: cell_name

        if (.not. self%is_initialized) call self%initialize()

        if (cell_type < 0 .or. cell_type > VTK_BEZIER_PYRAMID) then
            print *, "Error: Unknown VTK cell type ", cell_type
            stop
        end if

        cell_name = self%cell(cell_type)%cell_name

    end function get_cell_name

    function get_max_cell_id(self) result(max_cell_id)
        implicit none
        class(type_vtk_constants), intent(inout) :: self
        integer(int32) :: max_cell_id

        if (.not. self%is_initialized) call self%initialize()

        max_cell_id = self%max_cell_id

    end function get_max_cell_id
end module core_interop_vtk_constants

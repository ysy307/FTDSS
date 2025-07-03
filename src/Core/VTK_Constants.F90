
module Core_VTK_Constants
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    private

    public :: VTK_EMPTY_CELL, VTK_VERTEX, VTK_POLY_VERTEX, VTK_LINE, &
              VTK_POLY_LINE, VTK_TRIANGLE, VTK_TRIANGLE_STRIP, VTK_POLYGON, &
              VTK_PIXEL, VTK_QUAD, VTK_TETRA, VTK_VOXEL, VTK_HEXAHEDRON, &
              VTK_WEDGE, VTK_PYRAMID, VTK_PENTAGONAL_PRISM, &
              VTK_HEXAGONAL_PRISM, VTK_QUADRATIC_EDGE, &
              VTK_QUADRATIC_TRIANGLE, VTK_QUADRATIC_QUAD, &
              VTK_QUADRATIC_POLYGON, VTK_QUADRATIC_TETRA, &
              VTK_QUADRATIC_HEXAHEDRON, VTK_QUADRATIC_WEDGE, &
              VTK_QUADRATIC_PYRAMID, VTK_BIQUADRATIC_QUAD, &
              VTK_TRIQUADRATIC_HEXAHEDRON, VTK_TRIQUADRATIC_PYRAMID, &
              VTK_QUADRATIC_LINEAR_QUAD, VTK_QUADRATIC_LINEAR_WEDGE, &
              VTK_BIQUADRATIC_QUADRATIC_WEDGE, &
              VTK_BIQUADRATIC_QUADRATIC_HEXAHEDRON, &
              VTK_BIQUADRATIC_TRIANGLE, VTK_CUBIC_LINE, &
              VTK_CONVEX_POINT_SET, VTK_POLYHEDRON, &
              VTK_PARAMETRIC_CURVE, VTK_PARAMETRIC_SURFACE, &
              VTK_PARAMETRIC_TRI_SURFACE, &
              VTK_PARAMETRIC_QUAD_SURFACE, &
              VTK_PARAMETRIC_TETRA_REGION, &
              VTK_PARAMETRIC_HEX_REGION, &
              VTK_HIGHER_ORDER_EDGE, &
              VTK_HIGHER_ORDER_TRIANGLE, &
              VTK_HIGHER_ORDER_QUAD, &
              VTK_HIGHER_ORDER_POLYGON, &
              VTK_HIGHER_ORDER_TETRAHEDRON, &
              VTK_HIGHER_ORDER_WEDGE, &
              VTK_HIGHER_ORDER_PYRAMID, &
              VTK_HIGHER_ORDER_HEXAHEDRON, &
              VTK_LAGRANGE_CURVE, &
              VTK_LAGRANGE_TRIANGLE, &
              VTK_LAGRANGE_QUADRILATERAL, &
              VTK_LAGRANGE_TETRAHEDRON, &
              VTK_LAGRANGE_HEXAHEDRON, &
              VTK_LAGRANGE_WEDGE, &
              VTK_LAGRANGE_PYRAMID, &
              VTK_BEZIER_CURVE, &
              VTK_BEZIER_TRIANGLE, &
              VTK_BEZIER_QUADRILATERAL, &
              VTK_BEZIER_TETRAHEDRON, &
              VTK_BEZIER_HEXAHEDRON, &
              VTK_BEZIER_WEDGE, &
              VTK_BEZIER_PYRAMID

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

end module Core_VTK_Constants

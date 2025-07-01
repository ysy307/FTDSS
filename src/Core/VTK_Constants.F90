
module Core_VTK_Constants
    implicit none
    private
    public :: VTK_VERTEX, VTK_LINE, VTK_TRIANGLE, VTK_TETRA, VTK_QUAD, &
              VTK_QUADRATIC_EDGE, VTK_QUADRATIC_TRIANGLE, VTK_QUADRATIC_QUAD, &
              VTK_QUADRATIC_TETRA, VTK_QUADRATIC_HEXAHEDRON, VTK_WEDGE, &
              VTK_PYRAMID, VTK_PIXEL, VTK_HEXAHEDRON

    integer, parameter :: VTK_VERTEX = 1
    integer, parameter :: VTK_LINE = 3
    integer, parameter :: VTK_TRIANGLE = 5
    integer, parameter :: VTK_PIXEL = 8
    integer, parameter :: VTK_QUAD = 9
    integer, parameter :: VTK_TETRA = 10
    integer, parameter :: VTK_HEXAHEDRON = 12
    integer, parameter :: VTK_WEDGE = 13
    integer, parameter :: VTK_PYRAMID = 14
    integer, parameter :: VTK_QUADRATIC_EDGE = 21
    integer, parameter :: VTK_QUADRATIC_TRIANGLE = 22
    integer, parameter :: VTK_QUADRATIC_QUAD = 23
    integer, parameter :: VTK_QUADRATIC_TETRA = 24
    integer, parameter :: VTK_QUADRATIC_HEXAHEDRON = 25

end module Core_VTK_Constants

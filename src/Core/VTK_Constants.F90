
module Core_VTK_Constants
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    private
    public :: VTK_VERTEX, VTK_LINE, VTK_TRIANGLE, VTK_TETRA, VTK_QUAD, &
              VTK_QUADRATIC_EDGE, VTK_QUADRATIC_TRIANGLE, VTK_QUADRATIC_QUAD, &
              VTK_QUADRATIC_TETRA, VTK_QUADRATIC_HEXAHEDRON, VTK_WEDGE, &
              VTK_PYRAMID, VTK_PIXEL, VTK_HEXAHEDRON

    integer(int32), parameter :: VTK_VERTEX = 1
    integer(int32), parameter :: VTK_LINE = 3
    integer(int32), parameter :: VTK_TRIANGLE = 5
    integer(int32), parameter :: VTK_PIXEL = 8
    integer(int32), parameter :: VTK_QUAD = 9
    integer(int32), parameter :: VTK_TETRA = 10
    integer(int32), parameter :: VTK_HEXAHEDRON = 12
    integer(int32), parameter :: VTK_WEDGE = 13
    integer(int32), parameter :: VTK_PYRAMID = 14
    integer(int32), parameter :: VTK_QUADRATIC_EDGE = 21
    integer(int32), parameter :: VTK_QUADRATIC_TRIANGLE = 22
    integer(int32), parameter :: VTK_QUADRATIC_QUAD = 23
    integer(int32), parameter :: VTK_QUADRATIC_TETRA = 24
    integer(int32), parameter :: VTK_QUADRATIC_HEXAHEDRON = 25

end module Core_VTK_Constants

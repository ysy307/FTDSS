module core_constants_physical
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    !> Physics unit in systems
    integer(int32), parameter, public :: PHYSICS_UNIT_M = 1
    integer(int32), parameter, public :: PHYSICS_UNIT_CM = 2
    integer(int32), parameter, public :: PHYSICS_UNIT_PA = 3

    !> SWCC models
    integer(int32), parameter, public :: SWCC_BC = 1
    integer(int32), parameter, public :: SWCC_VG = 2
    integer(int32), parameter, public :: SWCC_KO = 3
    integer(int32), parameter, public :: SWCC_MVG = 4
    integer(int32), parameter, public :: SWCC_DURNER = 5
    integer(int32), parameter, public :: SWCC_DVGCH = 6
    !> Water retention function of Brooks-Corey model
    integer(int32), parameter, public :: WRF_BC = SWCC_BC
    !> Water retention function of van-Genuchten model
    integer(int32), parameter, public :: WRF_VG = SWCC_VG
    !> Water retention function of Kosugi model
    integer(int32), parameter, public :: WRF_KO = SWCC_KO
    !> Water retention function of modified van-Genuchten model
    integer(int32), parameter, public :: WRF_MVG = SWCC_MVG
    !> Water retention function of durner model
    integer(int32), parameter, public :: WRF_DURNER = SWCC_DURNER
    !> Water retention function of dvgch model
    integer(int32), parameter, public :: WRF_DVGCH = SWCC_DVGCH

    integer(int32), parameter, public :: HCF_BASE = 1
    integer(int32), parameter, public :: HCF_IMPEDANCE = 2
    integer(int32), parameter, public :: HCF_VISCOSITY = 3
    integer(int32), parameter, public :: HCF_BASE_IMPEDANCE = 4
    integer(int32), parameter, public :: HCF_BASE_VISCOSITY = 5
    integer(int32), parameter, public :: HCF_IMPEDANCE_VISCOSITY = 6
    integer(int32), parameter, public :: HCF_BASE_IMPEDANCE_VISCOSITY = 7

    integer(int32), parameter, public :: HCF_BC = SWCC_BC
    integer(int32), parameter, public :: HCF_VG = SWCC_VG
    integer(int32), parameter, public :: HCF_KO = SWCC_KO
    integer(int32), parameter, public :: HCF_MVG = SWCC_MVG
    integer(int32), parameter, public :: HCF_DURNER = SWCC_DURNER
    integer(int32), parameter, public :: HCF_DVGCH = SWCC_DVGCH

    integer(int32), parameter, public :: HCF_VISCOSITY_EXPONENTIAL = 1
    integer(int32), parameter, public :: HCF_VISCOSITY_SUPERCOOLED = 2

    integer(int32), parameter, public :: GCC_NON_SEGREGATION = 1
    integer(int32), parameter, public :: GCC_SEGREGATION = 2

    !------------------------------------------------------------------------
    ! Linear cells
    !------------------------------------------------------------------------
    integer(int32), parameter, public :: FE_EMPTY_CELL = 0
    integer(int32), parameter, public :: FE_VERTEX = 1
    integer(int32), parameter, public :: FE_POLY_VERTEX = 2
    integer(int32), parameter, public :: FE_LINE = 3
    integer(int32), parameter, public :: FE_POLY_LINE = 4
    integer(int32), parameter, public :: FE_TRIANGLE = 5
    integer(int32), parameter, public :: FE_TRIANGLE_STRIP = 6
    integer(int32), parameter, public :: FE_POLYGON = 7
    integer(int32), parameter, public :: FE_PIXEL = 8
    integer(int32), parameter, public :: FE_QUAD = 9
    integer(int32), parameter, public :: FE_TETRA = 10
    integer(int32), parameter, public :: FE_VOXEL = 11
    integer(int32), parameter, public :: FE_HEXAHEDRON = 12
    integer(int32), parameter, public :: FE_WEDGE = 13
    integer(int32), parameter, public :: FE_PYRAMID = 14
    integer(int32), parameter, public :: FE_PENTAGONAL_PRISM = 15
    integer(int32), parameter, public :: FE_HEXAGONAL_PRISM = 16
    !------------------------------------------------------------------------
    ! Quadratic, isoparametric cells
    !------------------------------------------------------------------------
    integer(int32), parameter, public :: FE_QUADRATIC_EDGE = 21
    integer(int32), parameter, public :: FE_QUADRATIC_TRIANGLE = 22
    integer(int32), parameter, public :: FE_QUADRATIC_QUAD = 23
    integer(int32), parameter, public :: FE_QUADRATIC_POLYGON = 36
    integer(int32), parameter, public :: FE_QUADRATIC_TETRA = 24
    integer(int32), parameter, public :: FE_QUADRATIC_HEXAHEDRON = 25
    integer(int32), parameter, public :: FE_QUADRATIC_WEDGE = 26
    integer(int32), parameter, public :: FE_QUADRATIC_PYRAMID = 27
    integer(int32), parameter, public :: FE_BIQUADRATIC_QUAD = 28
    integer(int32), parameter, public :: FE_TRIQUADRATIC_HEXAHEDRON = 29
    integer(int32), parameter, public :: FE_TRIQUADRATIC_PYRAMID = 37
    integer(int32), parameter, public :: FE_QUADRATIC_LINEAR_QUAD = 30
    integer(int32), parameter, public :: FE_QUADRATIC_LINEAR_WEDGE = 31
    integer(int32), parameter, public :: FE_BIQUADRATIC_QUADRATIC_WEDGE = 32
    integer(int32), parameter, public :: FE_BIQUADRATIC_QUADRATIC_HEXAHEDRON = 33
    integer(int32), parameter, public :: FE_BIQUADRATIC_TRIANGLE = 34
    !------------------------------------------------------------------------
    ! Cubic, isoparametric cell
    !------------------------------------------------------------------------
    integer(int32), parameter, public :: FE_CUBIC_LINE = 35
    !------------------------------------------------------------------------
    ! Special class of cells formed by convex group of points
    !------------------------------------------------------------------------
    integer(int32), parameter, public :: FE_CONVEX_POINT_SET = 41
    !------------------------------------------------------------------------
    ! Polyhedron cell(consisting of polygonal faces)
    !------------------------------------------------------------------------
    integer(int32), parameter, public :: FE_POLYHEDRON = 42
    !------------------------------------------------------------------------
    ! Higher order cells in parametric form
    !------------------------------------------------------------------------
    integer(int32), parameter, public :: FE_PARAMETRIC_CURVE = 51
    integer(int32), parameter, public :: FE_PARAMETRIC_SURFACE = 52
    integer(int32), parameter, public :: FE_PARAMETRIC_TRI_SURFACE = 53
    integer(int32), parameter, public :: FE_PARAMETRIC_QUAD_SURFACE = 54
    integer(int32), parameter, public :: FE_PARAMETRIC_TETRA_REGION = 55
    integer(int32), parameter, public :: FE_PARAMETRIC_HEX_REGION = 56
    !------------------------------------------------------------------------
    ! Higher order cells
    !------------------------------------------------------------------------
    integer(int32), parameter, public :: FE_HIGHER_ORDER_EDGE = 60
    integer(int32), parameter, public :: FE_HIGHER_ORDER_TRIANGLE = 61
    integer(int32), parameter, public :: FE_HIGHER_ORDER_QUAD = 62
    integer(int32), parameter, public :: FE_HIGHER_ORDER_POLYGON = 63
    integer(int32), parameter, public :: FE_HIGHER_ORDER_TETRAHEDRON = 64
    integer(int32), parameter, public :: FE_HIGHER_ORDER_WEDGE = 65
    integer(int32), parameter, public :: FE_HIGHER_ORDER_PYRAMID = 66
    integer(int32), parameter, public :: FE_HIGHER_ORDER_HEXAHEDRON = 67
    !------------------------------------------------------------------------
    ! Arbitrary order Lagrange elements(formulated separated from generic
    ! higher order cells)
    !------------------------------------------------------------------------
    integer(int32), parameter, public :: FE_LAGRANGE_CURVE = 68
    integer(int32), parameter, public :: FE_LAGRANGE_TRIANGLE = 69
    integer(int32), parameter, public :: FE_LAGRANGE_QUADRILATERAL = 70
    integer(int32), parameter, public :: FE_LAGRANGE_TETRAHEDRON = 71
    integer(int32), parameter, public :: FE_LAGRANGE_HEXAHEDRON = 72
    integer(int32), parameter, public :: FE_LAGRANGE_WEDGE = 73
    integer(int32), parameter, public :: FE_LAGRANGE_PYRAMID = 74
    !------------------------------------------------------------------------
    ! Arbitrary order Bezier elements(formulated separated from generic
    ! higher order cells)
    !------------------------------------------------------------------------
    integer(int32), parameter, public :: FE_BEZIER_CURVE = 75
    integer(int32), parameter, public :: FE_BEZIER_TRIANGLE = 76
    integer(int32), parameter, public :: FE_BEZIER_QUADRILATERAL = 77
    integer(int32), parameter, public :: FE_BEZIER_TETRAHEDRON = 78
    integer(int32), parameter, public :: FE_BEZIER_HEXAHEDRON = 79
    integer(int32), parameter, public :: FE_BEZIER_WEDGE = 80
    integer(int32), parameter, public :: FE_BEZIER_PYRAMID = 81

end module core_constants_physical

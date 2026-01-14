module core_constants_physical
    use, intrinsic :: iso_fortran_env
    use :: core_constants_base, only:type_constant_id
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

    type :: type_constant_ids_fe_type
        type(type_constant_id) :: EMPTY_CELL = type_constant_id("EMPTY_CELL", 0)
        type(type_constant_id) :: VERTEX = type_constant_id("VERTEX", 1)
        type(type_constant_id) :: POLY_VERTEX = type_constant_id("POLY_VERTEX", 2)
        type(type_constant_id) :: LINE = type_constant_id("LINE", 3)
        type(type_constant_id) :: POLY_LINE = type_constant_id("POLY_LINE", 4)
        type(type_constant_id) :: TRIANGLE = type_constant_id("TRIANGLE", 5)
        type(type_constant_id) :: TRIANGLE_STRIP = type_constant_id("TRIANGLE_STRIP", 6)
        type(type_constant_id) :: POLYGON = type_constant_id("POLYGON", 7)
        type(type_constant_id) :: PIXEL = type_constant_id("PIXEL", 8)
        type(type_constant_id) :: QUAD = type_constant_id("QUAD", 9)
        type(type_constant_id) :: TETRA = type_constant_id("TETRA", 10)
        type(type_constant_id) :: VOXEL = type_constant_id("VOXEL", 11)
        type(type_constant_id) :: HEXAHEDRON = type_constant_id("HEXAHEDRON", 12)
        type(type_constant_id) :: WEDGE = type_constant_id("WEDGE", 13)
        type(type_constant_id) :: PYRAMID = type_constant_id("PYRAMID", 14)
        type(type_constant_id) :: PENTAGONAL_PRISM = type_constant_id("PENTAGONAL_PRISM", 15)
        type(type_constant_id) :: HEXAGONAL_PRISM = type_constant_id("HEXAGONAL_PRISM", 16)
        type(type_constant_id) :: QUADRATIC_EDGE = type_constant_id("QUADRATIC_EDGE", 21)
        type(type_constant_id) :: QUADRATIC_TRIANGLE = type_constant_id("QUADRATIC_TRIANGLE", 22)
        type(type_constant_id) :: QUADRATIC_QUAD = type_constant_id("QUADRATIC_QUAD", 23)
        type(type_constant_id) :: QUADRATIC_POLYGON = type_constant_id("QUADRATIC_POLYGON", 36)
        type(type_constant_id) :: QUADRATIC_TETRA = type_constant_id("QUADRATIC_TETRA", 24)
        type(type_constant_id) :: QUADRATIC_HEXAHEDRON = type_constant_id("QUADRATIC_HEXAHEDRON", 25)
        type(type_constant_id) :: QUADRATIC_WEDGE = type_constant_id("QUADRATIC_WEDGE", 26)
        type(type_constant_id) :: QUADRATIC_PYRAMID = type_constant_id("QUADRATIC_PYRAMID", 27)
        type(type_constant_id) :: BIQUADRATIC_QUAD = type_constant_id("BIQUADRATIC_QUAD", 28)
        type(type_constant_id) :: TRIQUADRATIC_HEXAHEDRON = type_constant_id("TRIQUADRATIC_HEXAHEDRON", 29)
        type(type_constant_id) :: TRIQUADRATIC_PYRAMID = type_constant_id("TRIQUADRATIC_PYRAMID", 37)
        type(type_constant_id) :: QUADRATIC_LINEAR_QUAD = type_constant_id("QUADRATIC_LINEAR_QUAD", 30)
        type(type_constant_id) :: QUADRATIC_LINEAR_WEDGE = type_constant_id("QUADRATIC_LINEAR_WEDGE", 31)
        type(type_constant_id) :: BIQUADRATIC_QUADRATIC_WEDGE = type_constant_id("BIQUADRATIC_QUADRATIC_WEDGE", 32)
        type(type_constant_id) :: BIQUADRATIC_QUADRATIC_HEXAHEDRON = type_constant_id("BIQUADRATIC_QUADRATIC_HEXAHEDRON", 33)
        type(type_constant_id) :: BIQUADRATIC_TRIANGLE = type_constant_id("BIQUADRATIC_TRIANGLE", 34)
        type(type_constant_id) :: CUBIC_LINE = type_constant_id("CUBIC_LINE", 35)
        type(type_constant_id) :: CONVEX_POINT_SET = type_constant_id("CONVEX_POINT_SET", 41)
        type(type_constant_id) :: POLYHEDRON = type_constant_id("POLYHEDRON", 42)
        type(type_constant_id) :: PARAMETRIC_CURVE = type_constant_id("PARAMETRIC_CURVE", 51)
        type(type_constant_id) :: PARAMETRIC_SURFACE = type_constant_id("PARAMETRIC_SURFACE", 52)
        type(type_constant_id) :: PARAMETRIC_TRI_SURFACE = type_constant_id("PARAMETRIC_TRI_SURFACE", 53)
        type(type_constant_id) :: PARAMETRIC_QUAD_SURFACE = type_constant_id("PARAMETRIC_QUAD_SURFACE", 54)
        type(type_constant_id) :: PARAMETRIC_TETRA_REGION = type_constant_id("PARAMETRIC_TETRA_REGION", 55)
        type(type_constant_id) :: PARAMETRIC_HEX_REGION = type_constant_id("PARAMETRIC_HEX_REGION", 56)
        type(type_constant_id) :: HIGHER_ORDER_EDGE = type_constant_id("HIGHER_ORDER_EDGE", 60)
        type(type_constant_id) :: HIGHER_ORDER_TRIANGLE = type_constant_id("HIGHER_ORDER_TRIANGLE", 61)
        type(type_constant_id) :: HIGHER_ORDER_QUAD = type_constant_id("HIGHER_ORDER_QUAD", 62)
        type(type_constant_id) :: HIGHER_ORDER_POLYGON = type_constant_id("HIGHER_ORDER_POLYGON", 63)
        type(type_constant_id) :: HIGHER_ORDER_TETRAHEDRON = type_constant_id("HIGHER_ORDER_TETRAHEDRON", 64)
        type(type_constant_id) :: HIGHER_ORDER_WEDGE = type_constant_id("HIGHER_ORDER_WEDGE", 65)
        type(type_constant_id) :: HIGHER_ORDER_PYRAMID = type_constant_id("HIGHER_ORDER_PYRAMID", 66)
        type(type_constant_id) :: HIGHER_ORDER_HEXAHEDRON = type_constant_id("HIGHER_ORDER_HEXAHEDRON", 67)
        type(type_constant_id) :: LAGRANGE_CURVE = type_constant_id("LAGRANGE_CURVE", 68)
        type(type_constant_id) :: LAGRANGE_TRIANGLE = type_constant_id("LAGRANGE_TRIANGLE", 69)
        type(type_constant_id) :: LAGRANGE_QUADRILATERAL = type_constant_id("LAGRANGE_QUADRILATERAL", 70)
        type(type_constant_id) :: LAGRANGE_TETRAHEDRON = type_constant_id("LAGRANGE_TETRAHEDRON", 71)
        type(type_constant_id) :: LAGRANGE_HEXAHEDRON = type_constant_id("LAGRANGE_HEXAHEDRON", 72)
        type(type_constant_id) :: LAGRANGE_WEDGE = type_constant_id("LAGRANGE_WEDGE", 73)
        type(type_constant_id) :: LAGRANGE_PYRAMID = type_constant_id("LAGRANGE_PYRAMID", 74)
        type(type_constant_id) :: BEZIER_CURVE = type_constant_id("BEZIER_CURVE", 75)
        type(type_constant_id) :: BEZIER_TRIANGLE = type_constant_id("BEZIER_TRIANGLE", 76)
        type(type_constant_id) :: BEZIER_QUADRILATERAL = type_constant_id("BEZIER_QUADRILATERAL", 77)
        type(type_constant_id) :: BEZIER_TETRAHEDRON = type_constant_id("BEZIER_TETRAHEDRON", 78)
        type(type_constant_id) :: BEZIER_HEXAHEDRON = type_constant_id("BEZIER_HEXAHEDRON", 79)
        type(type_constant_id) :: BEZIER_WEDGE = type_constant_id("BEZIER_WEDGE", 80)
        type(type_constant_id) :: BEZIER_PYRAMID = type_constant_id("BEZIER_PYRAMID", 81)
        integer(int32) :: max_fe_type = 81
    end type type_constant_ids_fe_type

    type(type_constant_ids_fe_type), public, parameter :: FE_TYPE = type_constant_ids_fe_type()

end module core_constants_physical

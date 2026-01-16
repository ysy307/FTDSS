module core_constants_fe
    use, intrinsic :: iso_fortran_env
    use :: stdlib_strings, only:strip
    use :: stdlib_ascii, only:to_lower
    use :: core_constants_base, only:type_constant_id
    implicit none
    private

    type :: type_constant_ids_fe_type
        type(type_constant_id) :: EMPTY_CELL = type_constant_id("FE_TYPE", "EMPTY_CELL", 0)
        type(type_constant_id) :: VERTEX = type_constant_id("FE_TYPE", "VERTEX", 1)
        type(type_constant_id) :: POLY_VERTEX = type_constant_id("FE_TYPE", "POLY_VERTEX", 2)
        type(type_constant_id) :: LINE = type_constant_id("FE_TYPE", "LINE", 3)
        type(type_constant_id) :: POLY_LINE = type_constant_id("FE_TYPE", "POLY_LINE", 4)
        type(type_constant_id) :: TRIANGLE = type_constant_id("FE_TYPE", "TRIANGLE", 5)
        type(type_constant_id) :: TRIANGLE_STRIP = type_constant_id("FE_TYPE", "TRIANGLE_STRIP", 6)
        type(type_constant_id) :: POLYGON = type_constant_id("FE_TYPE", "POLYGON", 7)
        type(type_constant_id) :: PIXEL = type_constant_id("FE_TYPE", "PIXEL", 8)
        type(type_constant_id) :: QUAD = type_constant_id("FE_TYPE", "QUAD", 9)
        type(type_constant_id) :: TETRA = type_constant_id("FE_TYPE", "TETRA", 10)
        type(type_constant_id) :: VOXEL = type_constant_id("FE_TYPE", "VOXEL", 11)
        type(type_constant_id) :: HEXAHEDRON = type_constant_id("FE_TYPE", "HEXAHEDRON", 12)
        type(type_constant_id) :: WEDGE = type_constant_id("FE_TYPE", "WEDGE", 13)
        type(type_constant_id) :: PYRAMID = type_constant_id("FE_TYPE", "PYRAMID", 14)
        type(type_constant_id) :: PENTAGONAL_PRISM = type_constant_id("FE_TYPE", "PENTAGONAL_PRISM", 15)
        type(type_constant_id) :: HEXAGONAL_PRISM = type_constant_id("FE_TYPE", "HEXAGONAL_PRISM", 16)
        type(type_constant_id) :: QUADRATIC_EDGE = type_constant_id("FE_TYPE", "QUADRATIC_EDGE", 21)
        type(type_constant_id) :: QUADRATIC_TRIANGLE = type_constant_id("FE_TYPE", "QUADRATIC_TRIANGLE", 22)
        type(type_constant_id) :: QUADRATIC_QUAD = type_constant_id("FE_TYPE", "QUADRATIC_QUAD", 23)
        type(type_constant_id) :: QUADRATIC_POLYGON = type_constant_id("FE_TYPE", "QUADRATIC_POLYGON", 36)
        type(type_constant_id) :: QUADRATIC_TETRA = type_constant_id("FE_TYPE", "QUADRATIC_TETRA", 24)
        type(type_constant_id) :: QUADRATIC_HEXAHEDRON = type_constant_id("FE_TYPE", "QUADRATIC_HEXAHEDRON", 25)
        type(type_constant_id) :: QUADRATIC_WEDGE = type_constant_id("FE_TYPE", "QUADRATIC_WEDGE", 26)
        type(type_constant_id) :: QUADRATIC_PYRAMID = type_constant_id("FE_TYPE", "QUADRATIC_PYRAMID", 27)
        type(type_constant_id) :: BIQUADRATIC_QUAD = type_constant_id("FE_TYPE", "BIQUADRATIC_QUAD", 28)
        type(type_constant_id) :: TRIQUADRATIC_HEXAHEDRON = type_constant_id("FE_TYPE", "TRIQUADRATIC_HEXAHEDRON", 29)
        type(type_constant_id) :: TRIQUADRATIC_PYRAMID = type_constant_id("FE_TYPE", "TRIQUADRATIC_PYRAMID", 37)
        type(type_constant_id) :: QUADRATIC_LINEAR_QUAD = type_constant_id("FE_TYPE", "QUADRATIC_LINEAR_QUAD", 30)
        type(type_constant_id) :: QUADRATIC_LINEAR_WEDGE = type_constant_id("FE_TYPE", "QUADRATIC_LINEAR_WEDGE", 31)
        type(type_constant_id) :: BIQUADRATIC_QUADRATIC_WEDGE = type_constant_id("FE_TYPE", "BIQUADRATIC_QUADRATIC_WEDGE", 32)
        type(type_constant_id) :: BIQUADRATIC_QUADRATIC_HEXAHEDRON = type_constant_id("FE_TYPE", "BIQUADRATIC_QUADRATIC_HEXAHEDRON", 33)
        type(type_constant_id) :: BIQUADRATIC_TRIANGLE = type_constant_id("FE_TYPE", "BIQUADRATIC_TRIANGLE", 34)
        type(type_constant_id) :: CUBIC_LINE = type_constant_id("FE_TYPE", "CUBIC_LINE", 35)
        type(type_constant_id) :: CONVEX_POINT_SET = type_constant_id("FE_TYPE", "CONVEX_POINT_SET", 41)
        type(type_constant_id) :: POLYHEDRON = type_constant_id("FE_TYPE", "POLYHEDRON", 42)
        type(type_constant_id) :: PARAMETRIC_CURVE = type_constant_id("FE_TYPE", "PARAMETRIC_CURVE", 51)
        type(type_constant_id) :: PARAMETRIC_SURFACE = type_constant_id("FE_TYPE", "PARAMETRIC_SURFACE", 52)
        type(type_constant_id) :: PARAMETRIC_TRI_SURFACE = type_constant_id("FE_TYPE", "PARAMETRIC_TRI_SURFACE", 53)
        type(type_constant_id) :: PARAMETRIC_QUAD_SURFACE = type_constant_id("FE_TYPE", "PARAMETRIC_QUAD_SURFACE", 54)
        type(type_constant_id) :: PARAMETRIC_TETRA_REGION = type_constant_id("FE_TYPE", "PARAMETRIC_TETRA_REGION", 55)
        type(type_constant_id) :: PARAMETRIC_HEX_REGION = type_constant_id("FE_TYPE", "PARAMETRIC_HEX_REGION", 56)
        type(type_constant_id) :: HIGHER_ORDER_EDGE = type_constant_id("FE_TYPE", "HIGHER_ORDER_EDGE", 60)
        type(type_constant_id) :: HIGHER_ORDER_TRIANGLE = type_constant_id("FE_TYPE", "HIGHER_ORDER_TRIANGLE", 61)
        type(type_constant_id) :: HIGHER_ORDER_QUAD = type_constant_id("FE_TYPE", "HIGHER_ORDER_QUAD", 62)
        type(type_constant_id) :: HIGHER_ORDER_POLYGON = type_constant_id("FE_TYPE", "HIGHER_ORDER_POLYGON", 63)
        type(type_constant_id) :: HIGHER_ORDER_TETRAHEDRON = type_constant_id("FE_TYPE", "HIGHER_ORDER_TETRAHEDRON", 64)
        type(type_constant_id) :: HIGHER_ORDER_WEDGE = type_constant_id("FE_TYPE", "HIGHER_ORDER_WEDGE", 65)
        type(type_constant_id) :: HIGHER_ORDER_PYRAMID = type_constant_id("FE_TYPE", "HIGHER_ORDER_PYRAMID", 66)
        type(type_constant_id) :: HIGHER_ORDER_HEXAHEDRON = type_constant_id("FE_TYPE", "HIGHER_ORDER_HEXAHEDRON", 67)
        type(type_constant_id) :: LAGRANGE_CURVE = type_constant_id("FE_TYPE", "LAGRANGE_CURVE", 68)
        type(type_constant_id) :: LAGRANGE_TRIANGLE = type_constant_id("FE_TYPE", "LAGRANGE_TRIANGLE", 69)
        type(type_constant_id) :: LAGRANGE_QUADRILATERAL = type_constant_id("FE_TYPE", "LAGRANGE_QUADRILATERAL", 70)
        type(type_constant_id) :: LAGRANGE_TETRAHEDRON = type_constant_id("FE_TYPE", "LAGRANGE_TETRAHEDRON", 71)
        type(type_constant_id) :: LAGRANGE_HEXAHEDRON = type_constant_id("FE_TYPE", "LAGRANGE_HEXAHEDRON", 72)
        type(type_constant_id) :: LAGRANGE_WEDGE = type_constant_id("FE_TYPE", "LAGRANGE_WEDGE", 73)
        type(type_constant_id) :: LAGRANGE_PYRAMID = type_constant_id("FE_TYPE", "LAGRANGE_PYRAMID", 74)
        type(type_constant_id) :: BEZIER_CURVE = type_constant_id("FE_TYPE", "BEZIER_CURVE", 75)
        type(type_constant_id) :: BEZIER_TRIANGLE = type_constant_id("FE_TYPE", "BEZIER_TRIANGLE", 76)
        type(type_constant_id) :: BEZIER_QUADRILATERAL = type_constant_id("FE_TYPE", "BEZIER_QUADRILATERAL", 77)
        type(type_constant_id) :: BEZIER_TETRAHEDRON = type_constant_id("FE_TYPE", "BEZIER_TETRAHEDRON", 78)
        type(type_constant_id) :: BEZIER_HEXAHEDRON = type_constant_id("FE_TYPE", "BEZIER_HEXAHEDRON", 79)
        type(type_constant_id) :: BEZIER_WEDGE = type_constant_id("FE_TYPE", "BEZIER_WEDGE", 80)
        type(type_constant_id) :: BEZIER_PYRAMID = type_constant_id("FE_TYPE", "BEZIER_PYRAMID", 81)
        integer(int32) :: max_fe_type = 81
    contains
        procedure, private, pass(self) :: to_id_from_name => to_id_from_name_fe_type
        procedure, private, pass(self) :: to_id_from_object => to_id_from_object_fe_type
        generic, public :: to_id => to_id_from_name, to_id_from_object
        procedure, private, pass(self) :: to_name_from_id => to_name_from_id_fe_type
        procedure, private, pass(self) :: to_name_from_object => to_name_from_object_fe_type
        generic, public :: to_name => to_name_from_id, to_name_from_object
        procedure, private, pass(self) :: to_object_from_id => to_object_from_id_fe_type
        procedure, private, pass(self) :: to_object_from_name => to_object_from_name_fe_type
        generic, public :: to_object => to_object_from_id, to_object_from_name
    end type type_constant_ids_fe_type

    type(type_constant_ids_fe_type), public, parameter :: FE_TYPE = type_constant_ids_fe_type()

contains

    pure function to_id_from_name_fe_type(self, name) result(id)
        implicit none
        class(type_constant_ids_fe_type), intent(in) :: self
        character(len=*), intent(in) :: name
        integer(int32) :: id

        character(len=128) :: lname
        lname = strip(to_lower(name))

        if (trim(lname) == strip(to_lower(self%EMPTY_CELL%name))) then
            id = self%EMPTY_CELL%id
        else if (trim(lname) == strip(to_lower(self%VERTEX%name))) then
            id = self%VERTEX%id
        else if (trim(lname) == strip(to_lower(self%POLY_VERTEX%name))) then
            id = self%POLY_VERTEX%id
        else if (trim(lname) == strip(to_lower(self%LINE%name))) then
            id = self%LINE%id
        else if (trim(lname) == strip(to_lower(self%POLY_LINE%name))) then
            id = self%POLY_LINE%id
        else if (trim(lname) == strip(to_lower(self%TRIANGLE%name))) then
            id = self%TRIANGLE%id
        else if (trim(lname) == strip(to_lower(self%TRIANGLE_STRIP%name))) then
            id = self%TRIANGLE_STRIP%id
        else if (trim(lname) == strip(to_lower(self%POLYGON%name))) then
            id = self%POLYGON%id
        else if (trim(lname) == strip(to_lower(self%PIXEL%name))) then
            id = self%PIXEL%id
        else if (trim(lname) == strip(to_lower(self%QUAD%name))) then
            id = self%QUAD%id
        else if (trim(lname) == strip(to_lower(self%TETRA%name))) then
            id = self%TETRA%id
        else if (trim(lname) == strip(to_lower(self%VOXEL%name))) then
            id = self%VOXEL%id
        else if (trim(lname) == strip(to_lower(self%HEXAHEDRON%name))) then
            id = self%HEXAHEDRON%id
        else if (trim(lname) == strip(to_lower(self%WEDGE%name))) then
            id = self%WEDGE%id
        else if (trim(lname) == strip(to_lower(self%PYRAMID%name))) then
            id = self%PYRAMID%id
        else if (trim(lname) == strip(to_lower(self%PENTAGONAL_PRISM%name))) then
            id = self%PENTAGONAL_PRISM%id
        else if (trim(lname) == strip(to_lower(self%HEXAGONAL_PRISM%name))) then
            id = self%HEXAGONAL_PRISM%id
        else if (trim(lname) == strip(to_lower(self%QUADRATIC_EDGE%name))) then
            id = self%QUADRATIC_EDGE%id
        else if (trim(lname) == strip(to_lower(self%QUADRATIC_TRIANGLE%name))) then
            id = self%QUADRATIC_TRIANGLE%id
        else if (trim(lname) == strip(to_lower(self%QUADRATIC_QUAD%name))) then
            id = self%QUADRATIC_QUAD%id
        else if (trim(lname) == strip(to_lower(self%QUADRATIC_POLYGON%name))) then
            id = self%QUADRATIC_POLYGON%id
        else if (trim(lname) == strip(to_lower(self%QUADRATIC_TETRA%name))) then
            id = self%QUADRATIC_TETRA%id
        else if (trim(lname) == strip(to_lower(self%QUADRATIC_HEXAHEDRON%name))) then
            id = self%QUADRATIC_HEXAHEDRON%id
        else if (trim(lname) == strip(to_lower(self%QUADRATIC_WEDGE%name))) then
            id = self%QUADRATIC_WEDGE%id
        else if (trim(lname) == strip(to_lower(self%QUADRATIC_PYRAMID%name))) then
            id = self%QUADRATIC_PYRAMID%id
        else if (trim(lname) == strip(to_lower(self%BIQUADRATIC_QUAD%name))) then
            id = self%BIQUADRATIC_QUAD%id
        else if (trim(lname) == strip(to_lower(self%TRIQUADRATIC_HEXAHEDRON%name))) then
            id = self%TRIQUADRATIC_HEXAHEDRON%id
        else if (trim(lname) == strip(to_lower(self%TRIQUADRATIC_PYRAMID%name))) then
            id = self%TRIQUADRATIC_PYRAMID%id
        else if (trim(lname) == strip(to_lower(self%QUADRATIC_LINEAR_QUAD%name))) then
            id = self%QUADRATIC_LINEAR_QUAD%id
        else if (trim(lname) == strip(to_lower(self%QUADRATIC_LINEAR_WEDGE%name))) then
            id = self%QUADRATIC_LINEAR_WEDGE%id
        else if (trim(lname) == strip(to_lower(self%BIQUADRATIC_QUADRATIC_WEDGE%name))) then
            id = self%BIQUADRATIC_QUADRATIC_WEDGE%id
        else if (trim(lname) == strip(to_lower(self%BIQUADRATIC_QUADRATIC_HEXAHEDRON%name))) then
            id = self%BIQUADRATIC_QUADRATIC_HEXAHEDRON%id
        else if (trim(lname) == strip(to_lower(self%BIQUADRATIC_TRIANGLE%name))) then
            id = self%BIQUADRATIC_TRIANGLE%id
        else if (trim(lname) == strip(to_lower(self%CUBIC_LINE%name))) then
            id = self%CUBIC_LINE%id
        else if (trim(lname) == strip(to_lower(self%CONVEX_POINT_SET%name))) then
            id = self%CONVEX_POINT_SET%id
        else if (trim(lname) == strip(to_lower(self%POLYHEDRON%name))) then
            id = self%POLYHEDRON%id
        else if (trim(lname) == strip(to_lower(self%PARAMETRIC_CURVE%name))) then
            id = self%PARAMETRIC_CURVE%id
        else if (trim(lname) == strip(to_lower(self%PARAMETRIC_SURFACE%name))) then
            id = self%PARAMETRIC_SURFACE%id
        else if (trim(lname) == strip(to_lower(self%PARAMETRIC_TRI_SURFACE%name))) then
            id = self%PARAMETRIC_TRI_SURFACE%id
        else if (trim(lname) == strip(to_lower(self%PARAMETRIC_QUAD_SURFACE%name))) then
            id = self%PARAMETRIC_QUAD_SURFACE%id
        else if (trim(lname) == strip(to_lower(self%PARAMETRIC_TETRA_REGION%name))) then
            id = self%PARAMETRIC_TETRA_REGION%id
        else if (trim(lname) == strip(to_lower(self%PARAMETRIC_HEX_REGION%name))) then
            id = self%PARAMETRIC_HEX_REGION%id
        else if (trim(lname) == strip(to_lower(self%HIGHER_ORDER_EDGE%name))) then
            id = self%HIGHER_ORDER_EDGE%id
        else if (trim(lname) == strip(to_lower(self%HIGHER_ORDER_TRIANGLE%name))) then
            id = self%HIGHER_ORDER_TRIANGLE%id
        else if (trim(lname) == strip(to_lower(self%HIGHER_ORDER_QUAD%name))) then
            id = self%HIGHER_ORDER_QUAD%id
        else if (trim(lname) == strip(to_lower(self%HIGHER_ORDER_POLYGON%name))) then
            id = self%HIGHER_ORDER_POLYGON%id
        else if (trim(lname) == strip(to_lower(self%HIGHER_ORDER_TETRAHEDRON%name))) then
            id = self%HIGHER_ORDER_TETRAHEDRON%id
        else if (trim(lname) == strip(to_lower(self%HIGHER_ORDER_WEDGE%name))) then
            id = self%HIGHER_ORDER_WEDGE%id
        else if (trim(lname) == strip(to_lower(self%HIGHER_ORDER_PYRAMID%name))) then
            id = self%HIGHER_ORDER_PYRAMID%id
        else if (trim(lname) == strip(to_lower(self%HIGHER_ORDER_HEXAHEDRON%name))) then
            id = self%HIGHER_ORDER_HEXAHEDRON%id
        else if (trim(lname) == strip(to_lower(self%LAGRANGE_CURVE%name))) then
            id = self%LAGRANGE_CURVE%id
        else if (trim(lname) == strip(to_lower(self%LAGRANGE_TRIANGLE%name))) then
            id = self%LAGRANGE_TRIANGLE%id
        else if (trim(lname) == strip(to_lower(self%LAGRANGE_QUADRILATERAL%name))) then
            id = self%LAGRANGE_QUADRILATERAL%id
        else if (trim(lname) == strip(to_lower(self%LAGRANGE_TETRAHEDRON%name))) then
            id = self%LAGRANGE_TETRAHEDRON%id
        else if (trim(lname) == strip(to_lower(self%LAGRANGE_HEXAHEDRON%name))) then
            id = self%LAGRANGE_HEXAHEDRON%id
        else if (trim(lname) == strip(to_lower(self%LAGRANGE_WEDGE%name))) then
            id = self%LAGRANGE_WEDGE%id
        else if (trim(lname) == strip(to_lower(self%LAGRANGE_PYRAMID%name))) then
            id = self%LAGRANGE_PYRAMID%id
        else if (trim(lname) == strip(to_lower(self%BEZIER_CURVE%name))) then
            id = self%BEZIER_CURVE%id
        else if (trim(lname) == strip(to_lower(self%BEZIER_TRIANGLE%name))) then
            id = self%BEZIER_TRIANGLE%id
        else if (trim(lname) == strip(to_lower(self%BEZIER_QUADRILATERAL%name))) then
            id = self%BEZIER_QUADRILATERAL%id
        else if (trim(lname) == strip(to_lower(self%BEZIER_TETRAHEDRON%name))) then
            id = self%BEZIER_TETRAHEDRON%id
        else if (trim(lname) == strip(to_lower(self%BEZIER_HEXAHEDRON%name))) then
            id = self%BEZIER_HEXAHEDRON%id
        else if (trim(lname) == strip(to_lower(self%BEZIER_WEDGE%name))) then
            id = self%BEZIER_WEDGE%id
        else if (trim(lname) == strip(to_lower(self%BEZIER_PYRAMID%name))) then
            id = self%BEZIER_PYRAMID%id
        else
            id = -1
        end if
    end function to_id_from_name_fe_type

    pure function to_id_from_object_fe_type(self, object) result(id)
        implicit none
        class(type_constant_ids_fe_type), intent(in) :: self
        type(type_constant_id), intent(in) :: object
        integer(int32) :: id

        id = object%id
    end function to_id_from_object_fe_type

    pure function to_name_from_id_fe_type(self, id) result(name)
        implicit none
        class(type_constant_ids_fe_type), intent(in) :: self
        integer(int32), intent(in) :: id
        character(len=:), allocatable :: name

        if (id == self%EMPTY_CELL%id) then
            name = self%EMPTY_CELL%name
        else if (id == self%VERTEX%id) then
            name = self%VERTEX%name
        else if (id == self%POLY_VERTEX%id) then
            name = self%POLY_VERTEX%name
        else if (id == self%LINE%id) then
            name = self%LINE%name
        else if (id == self%POLY_LINE%id) then
            name = self%POLY_LINE%name
        else if (id == self%TRIANGLE%id) then
            name = self%TRIANGLE%name
        else if (id == self%TRIANGLE_STRIP%id) then
            name = self%TRIANGLE_STRIP%name
        else if (id == self%POLYGON%id) then
            name = self%POLYGON%name
        else if (id == self%PIXEL%id) then
            name = self%PIXEL%name
        else if (id == self%QUAD%id) then
            name = self%QUAD%name
        else if (id == self%TETRA%id) then
            name = self%TETRA%name
        else if (id == self%VOXEL%id) then
            name = self%VOXEL%name
        else if (id == self%HEXAHEDRON%id) then
            name = self%HEXAHEDRON%name
        else if (id == self%WEDGE%id) then
            name = self%WEDGE%name
        else if (id == self%PYRAMID%id) then
            name = self%PYRAMID%name
        else if (id == self%PENTAGONAL_PRISM%id) then
            name = self%PENTAGONAL_PRISM%name
        else if (id == self%HEXAGONAL_PRISM%id) then
            name = self%HEXAGONAL_PRISM%name
        else if (id == self%QUADRATIC_EDGE%id) then
            name = self%QUADRATIC_EDGE%name
        else if (id == self%QUADRATIC_TRIANGLE%id) then
            name = self%QUADRATIC_TRIANGLE%name
        else if (id == self%QUADRATIC_QUAD%id) then
            name = self%QUADRATIC_QUAD%name
        else if (id == self%QUADRATIC_POLYGON%id) then
            name = self%QUADRATIC_POLYGON%name
        else if (id == self%QUADRATIC_TETRA%id) then
            name = self%QUADRATIC_TETRA%name
        else if (id == self%QUADRATIC_HEXAHEDRON%id) then
            name = self%QUADRATIC_HEXAHEDRON%name
        else if (id == self%QUADRATIC_WEDGE%id) then
            name = self%QUADRATIC_WEDGE%name
        else if (id == self%QUADRATIC_PYRAMID%id) then
            name = self%QUADRATIC_PYRAMID%name
        else if (id == self%BIQUADRATIC_QUAD%id) then
            name = self%BIQUADRATIC_QUAD%name
        else if (id == self%TRIQUADRATIC_HEXAHEDRON%id) then
            name = self%TRIQUADRATIC_HEXAHEDRON%name
        else if (id == self%TRIQUADRATIC_PYRAMID%id) then
            name = self%TRIQUADRATIC_PYRAMID%name
        else if (id == self%QUADRATIC_LINEAR_QUAD%id) then
            name = self%QUADRATIC_LINEAR_QUAD%name
        else if (id == self%QUADRATIC_LINEAR_WEDGE%id) then
            name = self%QUADRATIC_LINEAR_WEDGE%name
        else if (id == self%BIQUADRATIC_QUADRATIC_WEDGE%id) then
            name = self%BIQUADRATIC_QUADRATIC_WEDGE%name
        else if (id == self%BIQUADRATIC_QUADRATIC_HEXAHEDRON%id) then
            name = self%BIQUADRATIC_QUADRATIC_HEXAHEDRON%name
        else if (id == self%BIQUADRATIC_TRIANGLE%id) then
            name = self%BIQUADRATIC_TRIANGLE%name
        else if (id == self%CUBIC_LINE%id) then
            name = self%CUBIC_LINE%name
        else if (id == self%CONVEX_POINT_SET%id) then
            name = self%CONVEX_POINT_SET%name
        else if (id == self%POLYHEDRON%id) then
            name = self%POLYHEDRON%name
        else if (id == self%PARAMETRIC_CURVE%id) then
            name = self%PARAMETRIC_CURVE%name
        else if (id == self%PARAMETRIC_SURFACE%id) then
            name = self%PARAMETRIC_SURFACE%name
        else if (id == self%PARAMETRIC_TRI_SURFACE%id) then
            name = self%PARAMETRIC_TRI_SURFACE%name
        else if (id == self%PARAMETRIC_QUAD_SURFACE%id) then
            name = self%PARAMETRIC_QUAD_SURFACE%name
        else if (id == self%PARAMETRIC_TETRA_REGION%id) then
            name = self%PARAMETRIC_TETRA_REGION%name
        else if (id == self%PARAMETRIC_HEX_REGION%id) then
            name = self%PARAMETRIC_HEX_REGION%name
        else if (id == self%HIGHER_ORDER_EDGE%id) then
            name = self%HIGHER_ORDER_EDGE%name
        else if (id == self%HIGHER_ORDER_TRIANGLE%id) then
            name = self%HIGHER_ORDER_TRIANGLE%name
        else if (id == self%HIGHER_ORDER_QUAD%id) then
            name = self%HIGHER_ORDER_QUAD%name
        else if (id == self%HIGHER_ORDER_POLYGON%id) then
            name = self%HIGHER_ORDER_POLYGON%name
        else if (id == self%HIGHER_ORDER_TETRAHEDRON%id) then
            name = self%HIGHER_ORDER_TETRAHEDRON%name
        else if (id == self%HIGHER_ORDER_WEDGE%id) then
            name = self%HIGHER_ORDER_WEDGE%name
        else if (id == self%HIGHER_ORDER_PYRAMID%id) then
            name = self%HIGHER_ORDER_PYRAMID%name
        else if (id == self%HIGHER_ORDER_HEXAHEDRON%id) then
            name = self%HIGHER_ORDER_HEXAHEDRON%name
        else if (id == self%LAGRANGE_CURVE%id) then
            name = self%LAGRANGE_CURVE%name
        else if (id == self%LAGRANGE_TRIANGLE%id) then
            name = self%LAGRANGE_TRIANGLE%name
        else if (id == self%LAGRANGE_QUADRILATERAL%id) then
            name = self%LAGRANGE_QUADRILATERAL%name
        else if (id == self%LAGRANGE_TETRAHEDRON%id) then
            name = self%LAGRANGE_TETRAHEDRON%name
        else if (id == self%LAGRANGE_HEXAHEDRON%id) then
            name = self%LAGRANGE_HEXAHEDRON%name
        else if (id == self%LAGRANGE_WEDGE%id) then
            name = self%LAGRANGE_WEDGE%name
        else if (id == self%LAGRANGE_PYRAMID%id) then
            name = self%LAGRANGE_PYRAMID%name
        else if (id == self%BEZIER_CURVE%id) then
            name = self%BEZIER_CURVE%name
        else if (id == self%BEZIER_TRIANGLE%id) then
            name = self%BEZIER_TRIANGLE%name
        else if (id == self%BEZIER_QUADRILATERAL%id) then
            name = self%BEZIER_QUADRILATERAL%name
        else if (id == self%BEZIER_TETRAHEDRON%id) then
            name = self%BEZIER_TETRAHEDRON%name
        else if (id == self%BEZIER_HEXAHEDRON%id) then
            name = self%BEZIER_HEXAHEDRON%name
        else if (id == self%BEZIER_WEDGE%id) then
            name = self%BEZIER_WEDGE%name
        else if (id == self%BEZIER_PYRAMID%id) then
            name = self%BEZIER_PYRAMID%name
        else
            name = ""
        end if
    end function to_name_from_id_fe_type

    pure function to_name_from_object_fe_type(self, object) result(name)
        implicit none
        class(type_constant_ids_fe_type), intent(in) :: self
        type(type_constant_id), intent(in) :: object
        character(len=:), allocatable :: name

        name = object%name
    end function to_name_from_object_fe_type

    pure function to_object_from_id_fe_type(self, id) result(object)
        implicit none
        class(type_constant_ids_fe_type), intent(in) :: self
        integer(int32), intent(in) :: id
        type(type_constant_id) :: object

        if (id == self%EMPTY_CELL%id) then
            object = self%EMPTY_CELL
        else if (id == self%VERTEX%id) then
            object = self%VERTEX
        else if (id == self%POLY_VERTEX%id) then
            object = self%POLY_VERTEX
        else if (id == self%LINE%id) then
            object = self%LINE
        else if (id == self%POLY_LINE%id) then
            object = self%POLY_LINE
        else if (id == self%TRIANGLE%id) then
            object = self%TRIANGLE
        else if (id == self%TRIANGLE_STRIP%id) then
            object = self%TRIANGLE_STRIP
        else if (id == self%POLYGON%id) then
            object = self%POLYGON
        else if (id == self%PIXEL%id) then
            object = self%PIXEL
        else if (id == self%QUAD%id) then
            object = self%QUAD
        else if (id == self%TETRA%id) then
            object = self%TETRA
        else if (id == self%VOXEL%id) then
            object = self%VOXEL
        else if (id == self%HEXAHEDRON%id) then
            object = self%HEXAHEDRON
        else if (id == self%WEDGE%id) then
            object = self%WEDGE
        else if (id == self%PYRAMID%id) then
            object = self%PYRAMID
        else if (id == self%PENTAGONAL_PRISM%id) then
            object = self%PENTAGONAL_PRISM
        else if (id == self%HEXAGONAL_PRISM%id) then
            object = self%HEXAGONAL_PRISM
        else if (id == self%QUADRATIC_EDGE%id) then
            object = self%QUADRATIC_EDGE
        else if (id == self%QUADRATIC_TRIANGLE%id) then
            object = self%QUADRATIC_TRIANGLE
        else if (id == self%QUADRATIC_QUAD%id) then
            object = self%QUADRATIC_QUAD
        else if (id == self%QUADRATIC_POLYGON%id) then
            object = self%QUADRATIC_POLYGON
        else if (id == self%QUADRATIC_TETRA%id) then
            object = self%QUADRATIC_TETRA
        else if (id == self%QUADRATIC_HEXAHEDRON%id) then
            object = self%QUADRATIC_HEXAHEDRON
        else if (id == self%QUADRATIC_WEDGE%id) then
            object = self%QUADRATIC_WEDGE
        else if (id == self%QUADRATIC_PYRAMID%id) then
            object = self%QUADRATIC_PYRAMID
        else if (id == self%BIQUADRATIC_QUAD%id) then
            object = self%BIQUADRATIC_QUAD
        else if (id == self%TRIQUADRATIC_HEXAHEDRON%id) then
            object = self%TRIQUADRATIC_HEXAHEDRON
        else if (id == self%TRIQUADRATIC_PYRAMID%id) then
            object = self%TRIQUADRATIC_PYRAMID
        else if (id == self%QUADRATIC_LINEAR_QUAD%id) then
            object = self%QUADRATIC_LINEAR_QUAD
        else if (id == self%QUADRATIC_LINEAR_WEDGE%id) then
            object = self%QUADRATIC_LINEAR_WEDGE
        else if (id == self%BIQUADRATIC_QUADRATIC_WEDGE%id) then
            object = self%BIQUADRATIC_QUADRATIC_WEDGE
        else if (id == self%BIQUADRATIC_QUADRATIC_HEXAHEDRON%id) then
            object = self%BIQUADRATIC_QUADRATIC_HEXAHEDRON
        else if (id == self%BIQUADRATIC_TRIANGLE%id) then
            object = self%BIQUADRATIC_TRIANGLE
        else if (id == self%CUBIC_LINE%id) then
            object = self%CUBIC_LINE
        else if (id == self%CONVEX_POINT_SET%id) then
            object = self%CONVEX_POINT_SET
        else if (id == self%POLYHEDRON%id) then
            object = self%POLYHEDRON
        else if (id == self%PARAMETRIC_CURVE%id) then
            object = self%PARAMETRIC_CURVE
        else if (id == self%PARAMETRIC_SURFACE%id) then
            object = self%PARAMETRIC_SURFACE
        else if (id == self%PARAMETRIC_TRI_SURFACE%id) then
            object = self%PARAMETRIC_TRI_SURFACE
        else if (id == self%PARAMETRIC_QUAD_SURFACE%id) then
            object = self%PARAMETRIC_QUAD_SURFACE
        else if (id == self%PARAMETRIC_TETRA_REGION%id) then
            object = self%PARAMETRIC_TETRA_REGION
        else if (id == self%PARAMETRIC_HEX_REGION%id) then
            object = self%PARAMETRIC_HEX_REGION
        else if (id == self%HIGHER_ORDER_EDGE%id) then
            object = self%HIGHER_ORDER_EDGE
        else if (id == self%HIGHER_ORDER_TRIANGLE%id) then
            object = self%HIGHER_ORDER_TRIANGLE
        else if (id == self%HIGHER_ORDER_QUAD%id) then
            object = self%HIGHER_ORDER_QUAD
        else if (id == self%HIGHER_ORDER_POLYGON%id) then
            object = self%HIGHER_ORDER_POLYGON
        else if (id == self%HIGHER_ORDER_TETRAHEDRON%id) then
            object = self%HIGHER_ORDER_TETRAHEDRON
        else if (id == self%HIGHER_ORDER_WEDGE%id) then
            object = self%HIGHER_ORDER_WEDGE
        else if (id == self%HIGHER_ORDER_PYRAMID%id) then
            object = self%HIGHER_ORDER_PYRAMID
        else if (id == self%HIGHER_ORDER_HEXAHEDRON%id) then
            object = self%HIGHER_ORDER_HEXAHEDRON
        else if (id == self%LAGRANGE_CURVE%id) then
            object = self%LAGRANGE_CURVE
        else if (id == self%LAGRANGE_TRIANGLE%id) then
            object = self%LAGRANGE_TRIANGLE
        else if (id == self%LAGRANGE_QUADRILATERAL%id) then
            object = self%LAGRANGE_QUADRILATERAL
        else if (id == self%LAGRANGE_TETRAHEDRON%id) then
            object = self%LAGRANGE_TETRAHEDRON
        else if (id == self%LAGRANGE_HEXAHEDRON%id) then
            object = self%LAGRANGE_HEXAHEDRON
        else if (id == self%LAGRANGE_WEDGE%id) then
            object = self%LAGRANGE_WEDGE
        else if (id == self%LAGRANGE_PYRAMID%id) then
            object = self%LAGRANGE_PYRAMID
        else if (id == self%BEZIER_CURVE%id) then
            object = self%BEZIER_CURVE
        else if (id == self%BEZIER_TRIANGLE%id) then
            object = self%BEZIER_TRIANGLE
        else if (id == self%BEZIER_QUADRILATERAL%id) then
            object = self%BEZIER_QUADRILATERAL
        else if (id == self%BEZIER_TETRAHEDRON%id) then
            object = self%BEZIER_TETRAHEDRON
        else if (id == self%BEZIER_HEXAHEDRON%id) then
            object = self%BEZIER_HEXAHEDRON
        else if (id == self%BEZIER_WEDGE%id) then
            object = self%BEZIER_WEDGE
        else if (id == self%BEZIER_PYRAMID%id) then
            object = self%BEZIER_PYRAMID
        end if
    end function to_object_from_id_fe_type

    pure function to_object_from_name_fe_type(self, name) result(object)
        implicit none
        class(type_constant_ids_fe_type), intent(in) :: self
        character(len=*), intent(in) :: name
        type(type_constant_id) :: object

        character(len=128) :: lname
        lname = strip(to_lower(name))

        if (trim(lname) == strip(to_lower(self%EMPTY_CELL%name))) then
            object = self%EMPTY_CELL
        else if (trim(lname) == strip(to_lower(self%VERTEX%name))) then
            object = self%VERTEX
        else if (trim(lname) == strip(to_lower(self%POLY_VERTEX%name))) then
            object = self%POLY_VERTEX
        else if (trim(lname) == strip(to_lower(self%LINE%name))) then
            object = self%LINE
        else if (trim(lname) == strip(to_lower(self%POLY_LINE%name))) then
            object = self%POLY_LINE
        else if (trim(lname) == strip(to_lower(self%TRIANGLE%name))) then
            object = self%TRIANGLE
        else if (trim(lname) == strip(to_lower(self%TRIANGLE_STRIP%name))) then
            object = self%TRIANGLE_STRIP
        else if (trim(lname) == strip(to_lower(self%POLYGON%name))) then
            object = self%POLYGON
        else if (trim(lname) == strip(to_lower(self%PIXEL%name))) then
            object = self%PIXEL
        else if (trim(lname) == strip(to_lower(self%QUAD%name))) then
            object = self%QUAD
        else if (trim(lname) == strip(to_lower(self%TETRA%name))) then
            object = self%TETRA
        else if (trim(lname) == strip(to_lower(self%VOXEL%name))) then
            object = self%VOXEL
        else if (trim(lname) == strip(to_lower(self%HEXAHEDRON%name))) then
            object = self%HEXAHEDRON
        else if (trim(lname) == strip(to_lower(self%WEDGE%name))) then
            object = self%WEDGE
        else if (trim(lname) == strip(to_lower(self%PYRAMID%name))) then
            object = self%PYRAMID
        else if (trim(lname) == strip(to_lower(self%PENTAGONAL_PRISM%name))) then
            object = self%PENTAGONAL_PRISM
        else if (trim(lname) == strip(to_lower(self%HEXAGONAL_PRISM%name))) then
            object = self%HEXAGONAL_PRISM
        else if (trim(lname) == strip(to_lower(self%QUADRATIC_EDGE%name))) then
            object = self%QUADRATIC_EDGE
        else if (trim(lname) == strip(to_lower(self%QUADRATIC_TRIANGLE%name))) then
            object = self%QUADRATIC_TRIANGLE
        else if (trim(lname) == strip(to_lower(self%QUADRATIC_QUAD%name))) then
            object = self%QUADRATIC_QUAD
        else if (trim(lname) == strip(to_lower(self%QUADRATIC_POLYGON%name))) then
            object = self%QUADRATIC_POLYGON
        else if (trim(lname) == strip(to_lower(self%QUADRATIC_TETRA%name))) then
            object = self%QUADRATIC_TETRA
        else if (trim(lname) == strip(to_lower(self%QUADRATIC_HEXAHEDRON%name))) then
            object = self%QUADRATIC_HEXAHEDRON
        else if (trim(lname) == strip(to_lower(self%QUADRATIC_WEDGE%name))) then
            object = self%QUADRATIC_WEDGE
        else if (trim(lname) == strip(to_lower(self%QUADRATIC_PYRAMID%name))) then
            object = self%QUADRATIC_PYRAMID
        else if (trim(lname) == strip(to_lower(self%BIQUADRATIC_QUAD%name))) then
            object = self%BIQUADRATIC_QUAD
        else if (trim(lname) == strip(to_lower(self%TRIQUADRATIC_HEXAHEDRON%name))) then
            object = self%TRIQUADRATIC_HEXAHEDRON
        else if (trim(lname) == strip(to_lower(self%TRIQUADRATIC_PYRAMID%name))) then
            object = self%TRIQUADRATIC_PYRAMID
        else if (trim(lname) == strip(to_lower(self%QUADRATIC_LINEAR_QUAD%name))) then
            object = self%QUADRATIC_LINEAR_QUAD
        else if (trim(lname) == strip(to_lower(self%QUADRATIC_LINEAR_WEDGE%name))) then
            object = self%QUADRATIC_LINEAR_WEDGE
        else if (trim(lname) == strip(to_lower(self%BIQUADRATIC_QUADRATIC_WEDGE%name))) then
            object = self%BIQUADRATIC_QUADRATIC_WEDGE
        else if (trim(lname) == strip(to_lower(self%BIQUADRATIC_QUADRATIC_HEXAHEDRON%name))) then
            object = self%BIQUADRATIC_QUADRATIC_HEXAHEDRON
        else if (trim(lname) == strip(to_lower(self%BIQUADRATIC_TRIANGLE%name))) then
            object = self%BIQUADRATIC_TRIANGLE
        else if (trim(lname) == strip(to_lower(self%CUBIC_LINE%name))) then
            object = self%CUBIC_LINE
        else if (trim(lname) == strip(to_lower(self%CONVEX_POINT_SET%name))) then
            object = self%CONVEX_POINT_SET
        else if (trim(lname) == strip(to_lower(self%POLYHEDRON%name))) then
            object = self%POLYHEDRON
        else if (trim(lname) == strip(to_lower(self%PARAMETRIC_CURVE%name))) then
            object = self%PARAMETRIC_CURVE
        else if (trim(lname) == strip(to_lower(self%PARAMETRIC_SURFACE%name))) then
            object = self%PARAMETRIC_SURFACE
        else if (trim(lname) == strip(to_lower(self%PARAMETRIC_TRI_SURFACE%name))) then
            object = self%PARAMETRIC_TRI_SURFACE
        else if (trim(lname) == strip(to_lower(self%PARAMETRIC_QUAD_SURFACE%name))) then
            object = self%PARAMETRIC_QUAD_SURFACE
        else if (trim(lname) == strip(to_lower(self%PARAMETRIC_TETRA_REGION%name))) then
            object = self%PARAMETRIC_TETRA_REGION
        else if (trim(lname) == strip(to_lower(self%PARAMETRIC_HEX_REGION%name))) then
            object = self%PARAMETRIC_HEX_REGION
        else if (trim(lname) == strip(to_lower(self%HIGHER_ORDER_EDGE%name))) then
            object = self%HIGHER_ORDER_EDGE
        else if (trim(lname) == strip(to_lower(self%HIGHER_ORDER_TRIANGLE%name))) then
            object = self%HIGHER_ORDER_TRIANGLE
        else if (trim(lname) == strip(to_lower(self%HIGHER_ORDER_QUAD%name))) then
            object = self%HIGHER_ORDER_QUAD
        else if (trim(lname) == strip(to_lower(self%HIGHER_ORDER_POLYGON%name))) then
            object = self%HIGHER_ORDER_POLYGON
        else if (trim(lname) == strip(to_lower(self%HIGHER_ORDER_TETRAHEDRON%name))) then
            object = self%HIGHER_ORDER_TETRAHEDRON
        else if (trim(lname) == strip(to_lower(self%HIGHER_ORDER_WEDGE%name))) then
            object = self%HIGHER_ORDER_WEDGE
        else if (trim(lname) == strip(to_lower(self%HIGHER_ORDER_PYRAMID%name))) then
            object = self%HIGHER_ORDER_PYRAMID
        else if (trim(lname) == strip(to_lower(self%HIGHER_ORDER_HEXAHEDRON%name))) then
            object = self%HIGHER_ORDER_HEXAHEDRON
        else if (trim(lname) == strip(to_lower(self%LAGRANGE_CURVE%name))) then
            object = self%LAGRANGE_CURVE
        else if (trim(lname) == strip(to_lower(self%LAGRANGE_TRIANGLE%name))) then
            object = self%LAGRANGE_TRIANGLE
        else if (trim(lname) == strip(to_lower(self%LAGRANGE_QUADRILATERAL%name))) then
            object = self%LAGRANGE_QUADRILATERAL
        else if (trim(lname) == strip(to_lower(self%LAGRANGE_TETRAHEDRON%name))) then
            object = self%LAGRANGE_TETRAHEDRON
        else if (trim(lname) == strip(to_lower(self%LAGRANGE_HEXAHEDRON%name))) then
            object = self%LAGRANGE_HEXAHEDRON
        else if (trim(lname) == strip(to_lower(self%LAGRANGE_WEDGE%name))) then
            object = self%LAGRANGE_WEDGE
        else if (trim(lname) == strip(to_lower(self%LAGRANGE_PYRAMID%name))) then
            object = self%LAGRANGE_PYRAMID
        else if (trim(lname) == strip(to_lower(self%BEZIER_CURVE%name))) then
            object = self%BEZIER_CURVE
        else if (trim(lname) == strip(to_lower(self%BEZIER_TRIANGLE%name))) then
            object = self%BEZIER_TRIANGLE
        else if (trim(lname) == strip(to_lower(self%BEZIER_QUADRILATERAL%name))) then
            object = self%BEZIER_QUADRILATERAL
        else if (trim(lname) == strip(to_lower(self%BEZIER_TETRAHEDRON%name))) then
            object = self%BEZIER_TETRAHEDRON
        else if (trim(lname) == strip(to_lower(self%BEZIER_HEXAHEDRON%name))) then
            object = self%BEZIER_HEXAHEDRON
        else if (trim(lname) == strip(to_lower(self%BEZIER_WEDGE%name))) then
            object = self%BEZIER_WEDGE
        else if (trim(lname) == strip(to_lower(self%BEZIER_PYRAMID%name))) then
            object = self%BEZIER_PYRAMID
        end if
    end function to_object_from_name_fe_type

end module core_constants_fe

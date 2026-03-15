!>
!> Defines 2D finite element types (triangles, quadrilaterals) and their
!> associated operations, such as shape functions and Jacobians.
!> Definition part using interfaces for submodule implementation.
!>
module domain_fe_element
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_strings, only:strip
    use :: stdlib_logger
    use :: module_core
    use :: domain_base_fe, only:abst_fe
    use :: domain_fe_integration, only:type_gauss_integration_rule
    implicit none
    private

    public :: type_triangle_first
    public :: type_triangle_second
    public :: type_triangle_third
    public :: type_square_first
    public :: type_square_second
    public :: type_square_second_lagrange
    public :: type_square_third
    public :: type_square_third_serendipity

    public :: construct_triangle_first
    public :: construct_square_first
    public :: construct_triangle_second
    public :: construct_square_second
    public :: construct_square_second_lagrange
    public :: construct_triangle_third
    public :: construct_square_third
    public :: construct_square_third_serendipity

    ! ====================================================================================
    !   Type Definitions
    ! ====================================================================================

    !>
    !> Represents a first-order triangular element (3 nodes).
    !>
    type, extends(abst_fe) :: type_triangle_first
    contains
        procedure, pass(self) :: calc_measure => calc_area_triangle_first
        procedure, pass(self) :: calc_psi => calc_psi_triangle_first
        procedure, pass(self) :: calc_dpsi => calc_dpsi_triangle_first
        procedure, pass(self) :: calc_jacobian => calc_jacobian_triangle_first
        procedure, pass(self) :: is_inside => is_in_triangle_first
    end type type_triangle_first

    !>
    !> Represents a first-order quadrilateral element (4 nodes).
    !>
    type, extends(abst_fe) :: type_square_first
    contains
        procedure, pass(self) :: calc_measure => calc_area_square_first
        procedure, pass(self) :: calc_psi => calc_psi_square_first
        procedure, pass(self) :: calc_dpsi => calc_dpsi_square_first
        procedure, pass(self) :: calc_jacobian => calc_jacobian_square_first
        procedure, pass(self) :: is_inside => is_in_square_first
    end type type_square_first

    !>
    !> Represents a second-order triangular element (6 nodes).
    !>
    type, extends(abst_fe) :: type_triangle_second
    contains
        procedure, pass(self) :: calc_measure => calc_area_triangle_second
        procedure, pass(self) :: calc_psi => calc_psi_triangle_second
        procedure, pass(self) :: calc_dpsi => calc_dpsi_triangle_second
        procedure, pass(self) :: calc_jacobian => calc_jacobian_triangle_second
        procedure, pass(self) :: is_inside => is_in_triangle_second
    end type type_triangle_second

    !>
    !> Represents a second-order quadrilateral element (8 nodes).
    !>
    type, extends(abst_fe) :: type_square_second
    contains
        procedure, pass(self) :: calc_measure => calc_area_square_second
        procedure, pass(self) :: calc_psi => calc_psi_square_second
        procedure, pass(self) :: calc_dpsi => calc_dpsi_square_second
        procedure, pass(self) :: calc_jacobian => calc_jacobian_square_second
        procedure, pass(self) :: is_inside => is_in_square_second
    end type type_square_second

    !>
    !> Represents a second-order quadrilateral Lagrange element (9 nodes).
    !>
    type, extends(abst_fe) :: type_square_second_lagrange
    contains
        procedure, pass(self) :: calc_measure => calc_area_square_second_lagrange
        procedure, pass(self) :: calc_psi => calc_psi_square_second_lagrange
        procedure, pass(self) :: calc_dpsi => calc_dpsi_square_second_lagrange
        procedure, pass(self) :: calc_jacobian => calc_jacobian_square_second_lagrange
        procedure, pass(self) :: is_inside => is_in_square_second_lagrange
    end type type_square_second_lagrange

    !>
    !> Represents a third-order triangular element (10 nodes, Lagrange P3).
    !>
    type, extends(abst_fe) :: type_triangle_third
    contains
        procedure, pass(self) :: calc_measure => calc_area_triangle_third
        procedure, pass(self) :: calc_psi => calc_psi_triangle_third
        procedure, pass(self) :: calc_dpsi => calc_dpsi_triangle_third
        procedure, pass(self) :: calc_jacobian => calc_jacobian_triangle_third
        procedure, pass(self) :: is_inside => is_in_triangle_third
    end type type_triangle_third

    !>
    !> Represents a third-order quadrilateral element (16 nodes, Lagrange Q3).
    !>
    type, extends(abst_fe) :: type_square_third
    contains
        procedure, pass(self) :: calc_measure => calc_area_square_third
        procedure, pass(self) :: calc_psi => calc_psi_square_third
        procedure, pass(self) :: calc_dpsi => calc_dpsi_square_third
        procedure, pass(self) :: calc_jacobian => calc_jacobian_square_third
        procedure, pass(self) :: is_inside => is_in_square_third
    end type type_square_third

    !>
    !> Represents a third-order quadrilateral Serendipity element (12 nodes).
    !>
    type, extends(abst_fe) :: type_square_third_serendipity
    contains
        procedure, pass(self) :: calc_measure => calc_area_square_third_serendipity
        procedure, pass(self) :: calc_psi => calc_psi_square_third_serendipity
        procedure, pass(self) :: calc_dpsi => calc_dpsi_square_third_serendipity
        procedure, pass(self) :: calc_jacobian => calc_jacobian_square_third_serendipity
        procedure, pass(self) :: is_inside => is_in_square_third_serendipity
    end type type_square_third_serendipity

    ! ====================================================================================
    !   Interface Definitions for Submodule Implementation
    ! ====================================================================================
    interface
        ! --- Constructors ---
        !> Constructs an instance of a first-order triangular element.
        module function construct_triangle_first(integration_order) result(fe)
            implicit none
            !> The integration order for the element.
            integer(int32), intent(in) :: integration_order
            !> The newly created and allocated finite element object.
            class(abst_fe), allocatable :: fe
        end function construct_triangle_first

        !> Constructs an instance of a first-order quadrilateral element.
        module function construct_square_first(integration_order) result(fe)
            implicit none
            !> The integration order for the element.
            integer(int32), intent(in) :: integration_order
            !> The newly created and allocated finite element object.
            class(abst_fe), allocatable :: fe
        end function construct_square_first

        !> Constructs an instance of a second-order triangular element.
        module function construct_triangle_second(integration_order) result(fe)
            implicit none
            !> The integration order for the element.
            integer(int32), intent(in) :: integration_order
            !> The newly created and allocated finite element object.
            class(abst_fe), allocatable :: fe
        end function construct_triangle_second

        !> Constructs an instance of a second-order quadrilateral element.
        module function construct_square_second(integration_order) result(fe)
            implicit none
            !> The integration order for the element.
            integer(int32), intent(in) :: integration_order
            !> The newly created and allocated finite element object.
            class(abst_fe), allocatable :: fe
        end function construct_square_second

        ! --- Triangle First Order ---
        !> Calculates the area of a first-order triangular element.
        module subroutine calc_area_triangle_first(self, node_coords, measure)
            implicit none
            !> The element instance.
            class(type_triangle_first), intent(in) :: self
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Calculated measure (area).
            real(real64), intent(inout) :: measure
        end subroutine calc_area_triangle_first

        !> Evaluates the shape function psi.
        pure elemental module subroutine calc_psi_triangle_first(self, i, r, psi_val)
            implicit none
            !> The element instance.
            class(type_triangle_first), intent(in) :: self
            !> Shape function index.
            integer(int32), intent(in) :: i
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Output value.
            real(real64), intent(inout) :: psi_val
        end subroutine calc_psi_triangle_first

        !> Evaluates the derivative of the shape function dpsi.
        pure elemental module subroutine calc_dpsi_triangle_first(self, i, j, r, dpsi_val)
            implicit none
            !> The element instance.
            class(type_triangle_first), intent(in) :: self
            !> Shape function index.
            integer(int32), intent(in) :: i
            !> Derivative direction index.
            integer(int32), intent(in) :: j
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Output value.
            real(real64), intent(inout) :: dpsi_val
        end subroutine calc_dpsi_triangle_first

        !> Calculates the Jacobian matrix.
        pure module subroutine calc_jacobian_triangle_first(self, r, node_coords, jac)
            implicit none
            !> The element instance.
            class(type_triangle_first), intent(in) :: self
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output Jacobian matrix.
            real(real64), intent(inout) :: jac(:, :)
        end subroutine calc_jacobian_triangle_first

        !> Checks if a point is inside the element.
        module subroutine is_in_triangle_first(self, cartesian, normalized, node_coords, is_in)
            implicit none
            !> The element instance.
            class(type_triangle_first), intent(in) :: self
            !> Cartesian coordinate to check.
            type(type_coordinate_dp), intent(in) :: cartesian
            !> Output normalized coordinate.
            type(type_coordinate_dp), intent(inout) :: normalized
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output flag.
            logical, intent(inout) :: is_in
        end subroutine is_in_triangle_first

        ! --- Square First Order ---
        !> Calculates the area of a first-order square element.
        module subroutine calc_area_square_first(self, node_coords, measure)
            implicit none
            !> The element instance.
            class(type_square_first), intent(in) :: self
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output area.
            real(real64), intent(inout) :: measure
        end subroutine calc_area_square_first

        !> Evaluates the shape function psi.
        pure elemental module subroutine calc_psi_square_first(self, i, r, psi_val)
            implicit none
            !> The element instance.
            class(type_square_first), intent(in) :: self
            !> Shape function index.
            integer(int32), intent(in) :: i
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Output value.
            real(real64), intent(inout) :: psi_val
        end subroutine calc_psi_square_first

        !> Evaluates the derivative of the shape function dpsi.
        pure elemental module subroutine calc_dpsi_square_first(self, i, j, r, dpsi_val)
            implicit none
            !> The element instance.
            class(type_square_first), intent(in) :: self
            !> Shape function index.
            integer(int32), intent(in) :: i
            !> Derivative direction index.
            integer(int32), intent(in) :: j
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Output value.
            real(real64), intent(inout) :: dpsi_val
        end subroutine calc_dpsi_square_first

        !> Calculates the Jacobian matrix.
        pure module subroutine calc_jacobian_square_first(self, r, node_coords, jac)
            implicit none
            !> The element instance.
            class(type_square_first), intent(in) :: self
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output Jacobian matrix.
            real(real64), intent(inout) :: jac(:, :)
        end subroutine calc_jacobian_square_first

        !> Checks if a point is inside the element.
        module subroutine is_in_square_first(self, cartesian, normalized, node_coords, is_in)
            implicit none
            !> The element instance.
            class(type_square_first), intent(in) :: self
            !> Cartesian coordinate to check.
            type(type_coordinate_dp), intent(in) :: cartesian
            !> Output normalized coordinate.
            type(type_coordinate_dp), intent(inout) :: normalized
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output flag.
            logical, intent(inout) :: is_in
        end subroutine is_in_square_first

        ! --- Triangle Second Order ---
        !> Calculates the area of a second-order triangular element.
        module subroutine calc_area_triangle_second(self, node_coords, measure)
            implicit none
            !> The element instance.
            class(type_triangle_second), intent(in) :: self
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output area.
            real(real64), intent(inout) :: measure
        end subroutine calc_area_triangle_second

        !> Evaluates the shape function psi.
        pure elemental module subroutine calc_psi_triangle_second(self, i, r, psi_val)
            implicit none
            !> The element instance.
            class(type_triangle_second), intent(in) :: self
            !> Shape function index.
            integer(int32), intent(in) :: i
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Output value.
            real(real64), intent(inout) :: psi_val
        end subroutine calc_psi_triangle_second

        !> Evaluates the derivative of the shape function dpsi.
        pure elemental module subroutine calc_dpsi_triangle_second(self, i, j, r, dpsi_val)
            implicit none
            !> The element instance.
            class(type_triangle_second), intent(in) :: self
            !> Shape function index.
            integer(int32), intent(in) :: i
            !> Derivative direction index.
            integer(int32), intent(in) :: j
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Output value.
            real(real64), intent(inout) :: dpsi_val
        end subroutine calc_dpsi_triangle_second

        !> Calculates the Jacobian matrix.
        pure module subroutine calc_jacobian_triangle_second(self, r, node_coords, jac)
            implicit none
            !> The element instance.
            class(type_triangle_second), intent(in) :: self
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output Jacobian matrix.
            real(real64), intent(inout) :: jac(:, :)
        end subroutine calc_jacobian_triangle_second

        !> Checks if a point is inside the element.
        module subroutine is_in_triangle_second(self, cartesian, normalized, node_coords, is_in)
            implicit none
            !> The element instance.
            class(type_triangle_second), intent(in) :: self
            !> Cartesian coordinate to check.
            type(type_coordinate_dp), intent(in) :: cartesian
            !> Output normalized coordinate.
            type(type_coordinate_dp), intent(inout) :: normalized
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output flag.
            logical, intent(inout) :: is_in
        end subroutine is_in_triangle_second

        ! --- Square Second Order ---
        !> Calculates the area of a second-order square element.
        module subroutine calc_area_square_second(self, node_coords, measure)
            implicit none
            !> The element instance.
            class(type_square_second), intent(in) :: self
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output area.
            real(real64), intent(inout) :: measure
        end subroutine calc_area_square_second

        !> Evaluates the shape function psi.
        pure elemental module subroutine calc_psi_square_second(self, i, r, psi_val)
            implicit none
            !> The element instance.
            class(type_square_second), intent(in) :: self
            !> Shape function index.
            integer(int32), intent(in) :: i
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Output value.
            real(real64), intent(inout) :: psi_val
        end subroutine calc_psi_square_second

        !> Evaluates the derivative of the shape function dpsi.
        pure elemental module subroutine calc_dpsi_square_second(self, i, j, r, dpsi_val)
            implicit none
            !> The element instance.
            class(type_square_second), intent(in) :: self
            !> Shape function index.
            integer(int32), intent(in) :: i
            !> Derivative direction index.
            integer(int32), intent(in) :: j
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Output value.
            real(real64), intent(inout) :: dpsi_val
        end subroutine calc_dpsi_square_second

        !> Calculates the Jacobian matrix.
        pure module subroutine calc_jacobian_square_second(self, r, node_coords, jac)
            implicit none
            !> The element instance.
            class(type_square_second), intent(in) :: self
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output Jacobian matrix.
            real(real64), intent(inout) :: jac(:, :)
        end subroutine calc_jacobian_square_second

        !> Checks if a point is inside the element.
        module subroutine is_in_square_second(self, cartesian, normalized, node_coords, is_in)
            implicit none
            !> The element instance.
            class(type_square_second), intent(in) :: self
            !> Cartesian coordinate to check.
            type(type_coordinate_dp), intent(in) :: cartesian
            !> Output normalized coordinate.
            type(type_coordinate_dp), intent(inout) :: normalized
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output flag.
            logical, intent(inout) :: is_in
        end subroutine is_in_square_second

        ! --- Square Second Order Lagrange (9-node) ---
        module function construct_square_second_lagrange(integration_order) result(fe)
            implicit none
            integer(int32), intent(in) :: integration_order
            class(abst_fe), allocatable :: fe
        end function construct_square_second_lagrange

        module subroutine calc_area_square_second_lagrange(self, node_coords, measure)
            implicit none
            class(type_square_second_lagrange), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: measure
        end subroutine calc_area_square_second_lagrange

        pure elemental module subroutine calc_psi_square_second_lagrange(self, i, r, psi_val)
            implicit none
            class(type_square_second_lagrange), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: psi_val
        end subroutine calc_psi_square_second_lagrange

        pure elemental module subroutine calc_dpsi_square_second_lagrange(self, i, j, r, dpsi_val)
            implicit none
            class(type_square_second_lagrange), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: dpsi_val
        end subroutine calc_dpsi_square_second_lagrange

        pure module subroutine calc_jacobian_square_second_lagrange(self, r, node_coords, jac)
            implicit none
            class(type_square_second_lagrange), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: jac(:, :)
        end subroutine calc_jacobian_square_second_lagrange

        module subroutine is_in_square_second_lagrange(self, cartesian, normalized, node_coords, is_in)
            implicit none
            class(type_square_second_lagrange), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: cartesian
            type(type_coordinate_dp), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            logical, intent(inout) :: is_in
        end subroutine is_in_square_second_lagrange

        ! --- Triangle Third Order ---
        module function construct_triangle_third(integration_order) result(fe)
            implicit none
            integer(int32), intent(in) :: integration_order
            class(abst_fe), allocatable :: fe
        end function construct_triangle_third

        module subroutine calc_area_triangle_third(self, node_coords, measure)
            implicit none
            class(type_triangle_third), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: measure
        end subroutine calc_area_triangle_third

        pure elemental module subroutine calc_psi_triangle_third(self, i, r, psi_val)
            implicit none
            class(type_triangle_third), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: psi_val
        end subroutine calc_psi_triangle_third

        pure elemental module subroutine calc_dpsi_triangle_third(self, i, j, r, dpsi_val)
            implicit none
            class(type_triangle_third), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: dpsi_val
        end subroutine calc_dpsi_triangle_third

        pure module subroutine calc_jacobian_triangle_third(self, r, node_coords, jac)
            implicit none
            class(type_triangle_third), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: jac(:, :)
        end subroutine calc_jacobian_triangle_third

        module subroutine is_in_triangle_third(self, cartesian, normalized, node_coords, is_in)
            implicit none
            class(type_triangle_third), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: cartesian
            type(type_coordinate_dp), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            logical, intent(inout) :: is_in
        end subroutine is_in_triangle_third

        ! --- Square Third Order ---
        module function construct_square_third(integration_order) result(fe)
            implicit none
            integer(int32), intent(in) :: integration_order
            class(abst_fe), allocatable :: fe
        end function construct_square_third

        module subroutine calc_area_square_third(self, node_coords, measure)
            implicit none
            class(type_square_third), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: measure
        end subroutine calc_area_square_third

        pure elemental module subroutine calc_psi_square_third(self, i, r, psi_val)
            implicit none
            class(type_square_third), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: psi_val
        end subroutine calc_psi_square_third

        pure elemental module subroutine calc_dpsi_square_third(self, i, j, r, dpsi_val)
            implicit none
            class(type_square_third), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: dpsi_val
        end subroutine calc_dpsi_square_third

        pure module subroutine calc_jacobian_square_third(self, r, node_coords, jac)
            implicit none
            class(type_square_third), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: jac(:, :)
        end subroutine calc_jacobian_square_third

        module subroutine is_in_square_third(self, cartesian, normalized, node_coords, is_in)
            implicit none
            class(type_square_third), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: cartesian
            type(type_coordinate_dp), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            logical, intent(inout) :: is_in
        end subroutine is_in_square_third

        ! --- Square Third Order Serendipity (12-node) ---
        module function construct_square_third_serendipity(integration_order) result(fe)
            implicit none
            integer(int32), intent(in) :: integration_order
            class(abst_fe), allocatable :: fe
        end function construct_square_third_serendipity

        module subroutine calc_area_square_third_serendipity(self, node_coords, measure)
            implicit none
            class(type_square_third_serendipity), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: measure
        end subroutine calc_area_square_third_serendipity

        pure elemental module subroutine calc_psi_square_third_serendipity(self, i, r, psi_val)
            implicit none
            class(type_square_third_serendipity), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: psi_val
        end subroutine calc_psi_square_third_serendipity

        pure elemental module subroutine calc_dpsi_square_third_serendipity(self, i, j, r, dpsi_val)
            implicit none
            class(type_square_third_serendipity), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: dpsi_val
        end subroutine calc_dpsi_square_third_serendipity

        pure module subroutine calc_jacobian_square_third_serendipity(self, r, node_coords, jac)
            implicit none
            class(type_square_third_serendipity), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: jac(:, :)
        end subroutine calc_jacobian_square_third_serendipity

        module subroutine is_in_square_third_serendipity(self, cartesian, normalized, node_coords, is_in)
            implicit none
            class(type_square_third_serendipity), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: cartesian
            type(type_coordinate_dp), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            logical, intent(inout) :: is_in
        end subroutine is_in_square_third_serendipity

    end interface

end module domain_fe_element

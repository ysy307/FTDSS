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
    use :: module_input, only:type_input
    use :: domain_fe, only:abst_fe
    use :: domain_fe_integration, only:get_integration_rule
    implicit none
    private

    public :: type_triangle_first
    public :: type_triangle_second
    public :: type_square_first
    public :: type_square_second

    public :: construct_triangle_first
    public :: construct_square_first
    public :: construct_triangle_second
    public :: construct_square_second

    ! ====================================================================================
    !   Type Definitions
    ! ====================================================================================

    !>
    !> Represents a first-order triangular element (3 nodes).
    !>
    type, extends(abst_fe) :: type_triangle_first
    contains
        procedure, pass(self) :: get_geometry => get_area_triangle_first
        procedure, pass(self) :: psi => psi_triangle_first
        procedure, pass(self) :: dpsi => dpsi_triangle_first
        procedure, pass(self) :: jacobian => jacobian_triangle_first
        procedure, pass(self) :: jacobian_det => jacobian_det_triangle_first
        procedure, pass(self) :: is_inside => is_in_triangle_first
    end type type_triangle_first

    !>
    !> Represents a first-order quadrilateral element (4 nodes).
    !>
    type, extends(abst_fe) :: type_square_first
    contains
        procedure, pass(self) :: get_geometry => get_area_square_first
        procedure, pass(self) :: psi => psi_square_first
        procedure, pass(self) :: dpsi => dpsi_square_first
        procedure, pass(self) :: jacobian => jacobian_square_first
        procedure, pass(self) :: jacobian_det => jacobian_det_square_first
        procedure, pass(self) :: is_inside => is_in_square_first
    end type type_square_first

    !>
    !> Represents a second-order triangular element (6 nodes).
    !>
    type, extends(abst_fe) :: type_triangle_second
    contains
        procedure, pass(self) :: get_geometry => get_area_triangle_second
        procedure, pass(self) :: psi => psi_triangle_second
        procedure, pass(self) :: dpsi => dpsi_triangle_second
        procedure, pass(self) :: jacobian => jacobian_triangle_second
        procedure, pass(self) :: jacobian_det => jacobian_det_triangle_second
        procedure, pass(self) :: is_inside => is_in_triangle_second
    end type type_triangle_second

    !>
    !> Represents a second-order quadrilateral element (8 nodes).
    !>
    type, extends(abst_fe) :: type_square_second
    contains
        procedure, pass(self) :: get_geometry => get_area_square_second
        procedure, pass(self) :: psi => psi_square_second
        procedure, pass(self) :: dpsi => dpsi_square_second
        procedure, pass(self) :: jacobian => jacobian_square_second
        procedure, pass(self) :: jacobian_det => jacobian_det_square_second
        procedure, pass(self) :: is_inside => is_in_square_second
    end type type_square_second

    ! ====================================================================================
    !   Interface Definitions for Submodule Implementation
    ! ====================================================================================
    interface
        ! --- Constructors ---
        !> Constructs an instance of a first-order triangular element.
        module function construct_triangle_first(input) result(fe)
            implicit none
            !> The main input data structure.
            type(type_input), intent(in) :: input
            !> The newly created and allocated finite element object.
            class(abst_fe), allocatable :: fe
        end function construct_triangle_first

        !> Constructs an instance of a first-order quadrilateral element.
        module function construct_square_first(input) result(fe)
            implicit none
            !> The main input data structure.
            type(type_input), intent(in) :: input
            !> The newly created and allocated finite element object.
            class(abst_fe), allocatable :: fe
        end function construct_square_first

        !> Constructs an instance of a second-order triangular element.
        module function construct_triangle_second(input) result(fe)
            implicit none
            !> The main input data structure.
            type(type_input), intent(in) :: input
            !> The newly created and allocated finite element object.
            class(abst_fe), allocatable :: fe
        end function construct_triangle_second

        !> Constructs an instance of a second-order quadrilateral element.
        module function construct_square_second(input) result(fe)
            implicit none
            !> The main input data structure.
            type(type_input), intent(in) :: input
            !> The newly created and allocated finite element object.
            class(abst_fe), allocatable :: fe
        end function construct_square_second

        ! --- Triangle First Order ---
        !> Calculates the area of a first-order triangular element.
        module subroutine get_area_triangle_first(self, node_coords, geometry)
            implicit none
            !> The element instance.
            class(type_triangle_first), intent(in) :: self
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Calculated geometry (area).
            real(real64), intent(inout) :: geometry
        end subroutine get_area_triangle_first

        !> Evaluates the shape function psi.
        pure elemental module subroutine psi_triangle_first(self, i, r, psi_val)
            implicit none
            !> The element instance.
            class(type_triangle_first), intent(in) :: self
            !> Shape function index.
            integer(int32), intent(in) :: i
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Output value.
            real(real64), intent(inout) :: psi_val
        end subroutine psi_triangle_first

        !> Evaluates the derivative of the shape function dpsi.
        pure elemental module subroutine dpsi_triangle_first(self, i, j, r, dpsi_val)
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
        end subroutine dpsi_triangle_first

        !> Calculates the Jacobian matrix.
        pure module subroutine jacobian_triangle_first(self, r, node_coords, jac)
            implicit none
            !> The element instance.
            class(type_triangle_first), intent(in) :: self
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output Jacobian matrix.
            real(real64), intent(inout) :: jac(:, :)
        end subroutine jacobian_triangle_first

        !> Calculates the Jacobian determinant.
        pure module subroutine jacobian_det_triangle_first(self, r, node_coords, det_j)
            implicit none
            !> The element instance.
            class(type_triangle_first), intent(in) :: self
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output Jacobian determinant.
            real(real64), intent(inout) :: det_j
        end subroutine jacobian_det_triangle_first

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
        module subroutine get_area_square_first(self, node_coords, geometry)
            implicit none
            !> The element instance.
            class(type_square_first), intent(in) :: self
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output area.
            real(real64), intent(inout) :: geometry
        end subroutine get_area_square_first

        !> Evaluates the shape function psi.
        pure elemental module subroutine psi_square_first(self, i, r, psi_val)
            implicit none
            !> The element instance.
            class(type_square_first), intent(in) :: self
            !> Shape function index.
            integer(int32), intent(in) :: i
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Output value.
            real(real64), intent(inout) :: psi_val
        end subroutine psi_square_first

        !> Evaluates the derivative of the shape function dpsi.
        pure elemental module subroutine dpsi_square_first(self, i, j, r, dpsi_val)
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
        end subroutine dpsi_square_first

        !> Calculates the Jacobian matrix.
        pure module subroutine jacobian_square_first(self, r, node_coords, jac)
            implicit none
            !> The element instance.
            class(type_square_first), intent(in) :: self
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output Jacobian matrix.
            real(real64), intent(inout) :: jac(:, :)
        end subroutine jacobian_square_first

        !> Calculates the Jacobian determinant.
        pure module subroutine jacobian_det_square_first(self, r, node_coords, det_j)
            implicit none
            !> The element instance.
            class(type_square_first), intent(in) :: self
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output Jacobian determinant.
            real(real64), intent(inout) :: det_j
        end subroutine jacobian_det_square_first

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
        module subroutine get_area_triangle_second(self, node_coords, geometry)
            implicit none
            !> The element instance.
            class(type_triangle_second), intent(in) :: self
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output area.
            real(real64), intent(inout) :: geometry
        end subroutine get_area_triangle_second

        !> Evaluates the shape function psi.
        pure elemental module subroutine psi_triangle_second(self, i, r, psi_val)
            implicit none
            !> The element instance.
            class(type_triangle_second), intent(in) :: self
            !> Shape function index.
            integer(int32), intent(in) :: i
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Output value.
            real(real64), intent(inout) :: psi_val
        end subroutine psi_triangle_second

        !> Evaluates the derivative of the shape function dpsi.
        pure elemental module subroutine dpsi_triangle_second(self, i, j, r, dpsi_val)
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
        end subroutine dpsi_triangle_second

        !> Calculates the Jacobian matrix.
        pure module subroutine jacobian_triangle_second(self, r, node_coords, jac)
            implicit none
            !> The element instance.
            class(type_triangle_second), intent(in) :: self
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output Jacobian matrix.
            real(real64), intent(inout) :: jac(:, :)
        end subroutine jacobian_triangle_second

        !> Calculates the Jacobian determinant.
        pure module subroutine jacobian_det_triangle_second(self, r, node_coords, det_j)
            implicit none
            !> The element instance.
            class(type_triangle_second), intent(in) :: self
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output Jacobian determinant.
            real(real64), intent(inout) :: det_j
        end subroutine jacobian_det_triangle_second

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
        module subroutine get_area_square_second(self, node_coords, geometry)
            implicit none
            !> The element instance.
            class(type_square_second), intent(in) :: self
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output area.
            real(real64), intent(inout) :: geometry
        end subroutine get_area_square_second

        !> Evaluates the shape function psi.
        pure elemental module subroutine psi_square_second(self, i, r, psi_val)
            implicit none
            !> The element instance.
            class(type_square_second), intent(in) :: self
            !> Shape function index.
            integer(int32), intent(in) :: i
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Output value.
            real(real64), intent(inout) :: psi_val
        end subroutine psi_square_second

        !> Evaluates the derivative of the shape function dpsi.
        pure elemental module subroutine dpsi_square_second(self, i, j, r, dpsi_val)
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
        end subroutine dpsi_square_second

        !> Calculates the Jacobian matrix.
        pure module subroutine jacobian_square_second(self, r, node_coords, jac)
            implicit none
            !> The element instance.
            class(type_square_second), intent(in) :: self
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output Jacobian matrix.
            real(real64), intent(inout) :: jac(:, :)
        end subroutine jacobian_square_second

        !> Calculates the Jacobian determinant.
        pure module subroutine jacobian_det_square_second(self, r, node_coords, det_j)
            implicit none
            !> The element instance.
            class(type_square_second), intent(in) :: self
            !> Local coordinate.
            type(type_coordinate_dp), intent(in) :: r
            !> Global coordinates of the nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> Output Jacobian determinant.
            real(real64), intent(inout) :: det_j
        end subroutine jacobian_det_square_second

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

    end interface

end module domain_fe_element

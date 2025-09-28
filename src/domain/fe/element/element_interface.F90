!>
!> Defines 2D finite element types (triangles, quadrilaterals) and their
!> associated operations, such as shape functions and Jacobians.
!>
module domain_fe_element
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_strings, only:strip
    use :: stdlib_logger
    use :: module_core
    use :: module_input, only:type_input
    use :: domain_fe, only:abst_fe
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
        procedure, pass(self) :: get_geometry => get_area_triangle_first !&
        procedure, pass(self) :: psi          => psi_triangle_first !&
        procedure, pass(self) :: dpsi         => dpsi_triangle_first !&
        procedure, pass(self) :: jacobian     => jacobian_triangle_first !&
        procedure, pass(self) :: jacobian_det => jacobian_det_triangle_first !&
        procedure, pass(self) :: is_inside    => is_in_triangle_first !&
    end type type_triangle_first

    !>
    !> Represents a first-order quadrilateral element (4 nodes).
    !>
    type, extends(abst_fe) :: type_square_first
    contains
        procedure, pass(self) :: get_geometry => get_area_square_first !&
        procedure, pass(self) :: psi          => psi_square_first !&
        procedure, pass(self) :: dpsi         => dpsi_square_first !&
        procedure, pass(self) :: jacobian     => jacobian_square_first !&
        procedure, pass(self) :: jacobian_det => jacobian_det_square_first !&
        procedure, pass(self) :: is_inside    => is_in_square_first !&
    end type type_square_first

    !>
    !> Represents a second-order triangular element (6 nodes).
    !>
    type, extends(abst_fe) :: type_triangle_second
    contains
        procedure, pass(self) :: get_geometry => get_area_triangle_second !&
        procedure, pass(self) :: psi          => psi_triangle_second !&
        procedure, pass(self) :: dpsi         => dpsi_triangle_second !&
        procedure, pass(self) :: jacobian     => jacobian_triangle_second !&
        procedure, pass(self) :: jacobian_det => jacobian_det_triangle_second !&
        procedure, pass(self) :: is_inside    => is_in_triangle_second !&
    end type type_triangle_second

    !>
    !> Represents a second-order quadrilateral element (8 nodes).
    !>
    type, extends(abst_fe) :: type_square_second
    contains
        procedure, pass(self) :: get_geometry => get_area_square_second !&
        procedure, pass(self) :: psi          => psi_square_second !&
        procedure, pass(self) :: dpsi         => dpsi_square_second !&
        procedure, pass(self) :: jacobian     => jacobian_square_second !&
        procedure, pass(self) :: jacobian_det => jacobian_det_square_second !&
        procedure, pass(self) :: is_inside    => is_in_square_second !&
    end type type_square_second

    ! ====================================================================================
    !   Interface Definitions
    ! ====================================================================================

    !--------------------------------------------------------------------------------------
    !  Triangle First Order Element Type procedures interface
    !--------------------------------------------------------------------------------------
    interface
        !>
        !> Constructs an instance of a first-order triangular element.
        !>
        module function construct_triangle_first(input) result(fe)
            implicit none
            !> The main input data structure.
            type(type_input), intent(in) :: input
            !> The newly created and allocated finite element object.
            class(abst_fe), allocatable :: fe
        end function construct_triangle_first

        !>
        !> Calculates the area of a first-order triangular element.
        !>
        module function get_area_triangle_first(self, node_coords, connectivity) result(area)
            implicit none
            !> The first-order triangular element object.
            class(type_triangle_first), intent(in) :: self
            !> The global coordinates of the mesh nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The computed area of the element.
            real(real64) :: area
        end function get_area_triangle_first

        !>
        !> Calculates the value of the shape function \( \psi_i \) for a first-order triangular element.
        !>
        pure elemental module function psi_triangle_first(self, i, r) result(psi)
            implicit none
            !> The first-order triangular element object.
            class(type_triangle_first), intent(in) :: self
            !> The index of the shape function (local node number).
            integer(int32), intent(in) :: i
            !> The local coordinate vector \( r \).
            type(type_coordinate_dp), intent(in) :: r
            !> The value of the shape function.
            real(real64) :: psi
        end function psi_triangle_first

        !>
        !> Calculates the derivative of the shape function with respect to the
        !> normalized coordinates, \( \frac{\partial\psi_i}{\partial r_j} \).
        !>
        pure elemental module function dpsi_triangle_first(self, i, j, r) result(dpsi)
            implicit none
            !> The first-order triangular element object.
            class(type_triangle_first), intent(in) :: self
            !> The index of the shape function (local node number).
            integer(int32), intent(in) :: i
            !> The index of the coordinate to differentiate with respect to (1 for \( \xi \), 2 for \( \eta \)).
            integer(int32), intent(in) :: j
            !> The local coordinate vector \( r \).
            type(type_coordinate_dp), intent(in) :: r
            !> The value of the shape function's derivative.
            real(real64) :: dpsi
        end function dpsi_triangle_first

        !>
        !> Calculates the Jacobian matrix for a first-order triangular element.
        !>
        pure module function jacobian_triangle_first(self, r, node_coords, connectivity) result(jacobian)
            implicit none
            !> The first-order triangular element object.
            class(type_triangle_first), intent(in) :: self
            !> The local coordinate vector.
            type(type_coordinate_dp), intent(in) :: r
            !> The global coordinates of the mesh nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The resulting Jacobian matrix.
            real(real64) :: jacobian(self%get_dimension(), self%get_dimension())
        end function jacobian_triangle_first

        !>
        !> Calculates the Jacobian determinant for a first-order triangular element.
        !>
        pure module function jacobian_det_triangle_first(self, r, node_coords, connectivity) result(jacobian_det)
            implicit none
            !> The first-order triangular element object.
            class(type_triangle_first), intent(in) :: self
            !> The local coordinate vector.
            type(type_coordinate_dp), intent(in) :: r
            !> The global coordinates of the mesh nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The Jacobian determinant.
            real(real64) :: jacobian_det
        end function jacobian_det_triangle_first

        !>
        !> Checks if a given Cartesian coordinate is inside the element.
        !>
        module subroutine is_in_triangle_first(self, cartesian, normalized, node_coords, connectivity, is_in)
            implicit none
            !> The first-order triangular element object.
            class(type_triangle_first), intent(in) :: self
            !> The Cartesian coordinate to check.
            type(type_coordinate_dp), intent(in) :: cartesian
            !> The corresponding normalized coordinate if the point is inside.
            type(type_coordinate_dp), intent(inout) :: normalized
            !> The global coordinates of the mesh nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The result (`.true.` if the point is inside, `.false.` otherwise).
            logical, intent(inout) :: is_in
        end subroutine is_in_triangle_first
    end interface

    !--------------------------------------------------------------------------------------
    !  Square First Order Element Type procedures interface
    !--------------------------------------------------------------------------------------
    interface
        !>
        !> Constructs an instance of a first-order quadrilateral element.
        !>
        module function construct_square_first(input) result(fe)
            implicit none
            !> The main input data structure.
            type(type_input), intent(in) :: input
            !> The newly created and allocated finite element object.
            class(abst_fe), allocatable :: fe
        end function construct_square_first

        !>
        !> Calculates the area of a first-order quadrilateral element using Gauss quadrature.
        !>
        module function get_area_square_first(self, node_coords, connectivity) result(area)
            implicit none
            !> The first-order quadrilateral element object.
            class(type_square_first), intent(in) :: self
            !> The global coordinates of the mesh nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The computed area of the element.
            real(real64) :: area
        end function get_area_square_first

        !>
        !> Calculates the value of the shape function \( \psi_i \) for a first-order quadrilateral element.
        !>
        pure elemental module function psi_square_first(self, i, r) result(psi)
            implicit none
            !> The first-order quadrilateral element object.
            class(type_square_first), intent(in) :: self
            !> The index of the shape function (local node number).
            integer(int32), intent(in) :: i
            !> The local coordinate vector \( r \).
            type(type_coordinate_dp), intent(in) :: r
            !> The value of the shape function.
            real(real64) :: psi
        end function psi_square_first

        !>
        !> Calculates the derivative of the shape function with respect to the
        !> normalized coordinates, \( \frac{\partial\psi_i}{\partial r_j} \).
        !>
        pure elemental module function dpsi_square_first(self, i, j, r) result(dpsi)
            implicit none
            !> The first-order quadrilateral element object.
            class(type_square_first), intent(in) :: self
            !> The index of the shape function (local node number).
            integer(int32), intent(in) :: i
            !> The index of the coordinate to differentiate with respect to (1 for \( \xi \), 2 for \( \eta \)).
            integer(int32), intent(in) :: j
            !> The local coordinate vector \( r \).
            type(type_coordinate_dp), intent(in) :: r
            !> The value of the shape function's derivative.
            real(real64) :: dpsi
        end function dpsi_square_first

        !>
        !> Calculates the Jacobian matrix for a first-order quadrilateral element.
        !>
        pure module function jacobian_square_first(self, r, node_coords, connectivity) result(jacobian)
            implicit none
            !> The first-order quadrilateral element object.
            class(type_square_first), intent(in) :: self
            !> The local coordinate vector.
            type(type_coordinate_dp), intent(in) :: r
            !> The global coordinates of the mesh nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The resulting Jacobian matrix.
            real(real64) :: jacobian(self%get_dimension(), self%get_dimension())
        end function jacobian_square_first

        !>
        !> Calculates the Jacobian determinant for a first-order quadrilateral element.
        !>
        pure module function jacobian_det_square_first(self, r, node_coords, connectivity) result(jacobian_det)
            implicit none
            !> The first-order quadrilateral element object.
            class(type_square_first), intent(in) :: self
            !> The local coordinate vector.
            type(type_coordinate_dp), intent(in) :: r
            !> The global coordinates of the mesh nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The Jacobian determinant.
            real(real64) :: jacobian_det
        end function jacobian_det_square_first

        !>
        !> Checks if a given Cartesian coordinate is inside the element.
        !>
        module subroutine is_in_square_first(self, cartesian, normalized, node_coords, connectivity, is_in)
            implicit none
            !> The first-order quadrilateral element object.
            class(type_square_first), intent(in) :: self
            !> The Cartesian coordinate to check.
            type(type_coordinate_dp), intent(in) :: cartesian
            !> The corresponding normalized coordinate if the point is inside.
            type(type_coordinate_dp), intent(inout) :: normalized
            !> The global coordinates of the mesh nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The result (`.true.` if the point is inside, `.false.` otherwise).
            logical, intent(inout) :: is_in
        end subroutine is_in_square_first
    end interface

    !--------------------------------------------------------------------------------------
    !  Triangle Second Order Element Type procedures interface
    !--------------------------------------------------------------------------------------
    interface
        !>
        !> Constructs an instance of a second-order triangular element.
        !>
        module function construct_triangle_second(input) result(fe)
            implicit none
            !> The main input data structure.
            type(type_input), intent(in) :: input
            !> The newly created and allocated finite element object.
            class(abst_fe), allocatable :: fe
        end function construct_triangle_second

        !>
        !> Calculates the area of a second-order triangular element using Gauss quadrature.
        !>
        module function get_area_triangle_second(self, node_coords, connectivity) result(area)
            implicit none
            !> The second-order triangular element object.
            class(type_triangle_second), intent(in) :: self
            !> The global coordinates of the mesh nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The computed area of the element.
            real(real64) :: area
        end function get_area_triangle_second

        !>
        !> Calculates the value of the shape function \( \psi_i \) for a second-order triangular element.
        !>
        pure elemental module function psi_triangle_second(self, i, r) result(psi)
            implicit none
            !> The second-order triangular element object.
            class(type_triangle_second), intent(in) :: self
            !> The index of the shape function (local node number).
            integer(int32), intent(in) :: i
            !> The local coordinate vector \( r \).
            type(type_coordinate_dp), intent(in) :: r
            !> The value of the shape function.
            real(real64) :: psi
        end function psi_triangle_second

        !>
        !> Calculates the derivative of the shape function with respect to the
        !> normalized coordinates, \( \frac{\partial\psi_i}{\partial r_j} \).
        !>
        pure elemental module function dpsi_triangle_second(self, i, j, r) result(dpsi)
            implicit none
            !> The second-order triangular element object.
            class(type_triangle_second), intent(in) :: self
            !> The index of the shape function (local node number).
            integer(int32), intent(in) :: i
            !> The index of the coordinate to differentiate with respect to (1 for \( \xi \), 2 for \( \eta \)).
            integer(int32), intent(in) :: j
            !> The local coordinate vector \( r \).
            type(type_coordinate_dp), intent(in) :: r
            !> The value of the shape function's derivative.
            real(real64) :: dpsi
        end function dpsi_triangle_second

        !>
        !> Calculates the derivative of the shape function with respect to the
        !> normalized coordinates, \( \frac{\partial\psi_i}{\partial r_j} \).
        !>
        pure elemental module function dpsi_deta_triangle_second(self, i, j, r) result(dpsi)
            implicit none
            !> The second-order triangular element object.
            class(type_triangle_second), intent(in) :: self
            !> The index of the shape function (local node number).
            integer(int32), intent(in) :: i
            !> The index of the coordinate to differentiate with respect to (1 for \( \xi \), 2 for \( \eta \)).
            integer(int32), intent(in) :: j
            !> The local coordinate vector \( r \).
            type(type_coordinate_dp), intent(in) :: r
            !> The value of the shape function's derivative.
            real(real64) :: dpsi
        end function dpsi_deta_triangle_second

        !>
        !> Calculates the Jacobian matrix for a second-order triangular element.
        !>
        pure module function jacobian_triangle_second(self, r, node_coords, connectivity) result(jacobian)
            implicit none
            !> The second-order triangular element object.
            class(type_triangle_second), intent(in) :: self
            !> The local coordinate vector.
            type(type_coordinate_dp), intent(in) :: r
            !> The global coordinates of the mesh nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The resulting Jacobian matrix.
            real(real64) :: jacobian(self%get_dimension(), self%get_dimension())
        end function jacobian_triangle_second

        !>
        !> Calculates the Jacobian determinant for a second-order triangular element.
        !>
        pure module function jacobian_det_triangle_second(self, r, node_coords, connectivity) result(jacobian_det)
            implicit none
            !> The second-order triangular element object.
            class(type_triangle_second), intent(in) :: self
            !> The local coordinate vector.
            type(type_coordinate_dp), intent(in) :: r
            !> The global coordinates of the mesh nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The Jacobian determinant.
            real(real64) :: jacobian_det
        end function jacobian_det_triangle_second

        !>
        !> Checks if a given Cartesian coordinate is inside the element.
        !>
        module subroutine is_in_triangle_second(self, cartesian, normalized, node_coords, connectivity, is_in)
            implicit none
            !> The second-order triangular element object.
            class(type_triangle_second), intent(in) :: self
            !> The Cartesian coordinate to check.
            type(type_coordinate_dp), intent(in) :: cartesian
            !> The corresponding normalized coordinate if the point is inside.
            type(type_coordinate_dp), intent(inout) :: normalized
            !> The global coordinates of the mesh nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The result (`.true.` if the point is inside, `.false.` otherwise).
            logical, intent(inout) :: is_in
        end subroutine is_in_triangle_second

    end interface

    !--------------------------------------------------------------------------------------
    !  Square Second Order Element Type procedures interface
    !--------------------------------------------------------------------------------------
    interface
        !>
        !> Constructs an instance of a second-order quadrilateral element.
        !>
        module function construct_square_second(input) result(fe)
            implicit none
            !> The main input data structure.
            type(type_input), intent(in) :: input
            !> The newly created and allocated finite element object.
            class(abst_fe), allocatable :: fe
        end function construct_square_second

        !>
        !> Calculates the area of a second-order quadrilateral element using Gauss quadrature.
        !>
        module function get_area_square_second(self, node_coords, connectivity) result(area)
            implicit none
            !> The second-order quadrilateral element object.
            class(type_square_second), intent(in) :: self
            !> The global coordinates of the mesh nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The computed area of the element.
            real(real64) :: area
        end function get_area_square_second

        !>
        !> Calculates the value of the shape function \( \psi_i \) for a second-order quadrilateral element.
        !>
        pure elemental module function psi_square_second(self, i, r) result(psi)
            implicit none
            !> The second-order quadrilateral element object.
            class(type_square_second), intent(in) :: self
            !> The index of the shape function (local node number).
            integer(int32), intent(in) :: i
            !> The local coordinate vector \( r \).
            type(type_coordinate_dp), intent(in) :: r
            !> The value of the shape function.
            real(real64) :: psi
        end function psi_square_second

        !>
        !> Calculates the derivative of the shape function with respect to the
        !> normalized coordinates, \( \frac{\partial\psi_i}{\partial r_j} \).
        !>
        pure elemental module function dpsi_square_second(self, i, j, r) result(dpsi)
            implicit none
            !> The second-order quadrilateral element object.
            class(type_square_second), intent(in) :: self
            !> The index of the shape function (local node number).
            integer(int32), intent(in) :: i
            !> The index of the coordinate to differentiate with respect to (1 for \( \xi \), 2 for \( \eta \)).
            integer(int32), intent(in) :: j
            !> The local coordinate vector \( r \).
            type(type_coordinate_dp), intent(in) :: r
            !> The value of the shape function's derivative.
            real(real64) :: dpsi
        end function dpsi_square_second

        !>
        !> Calculates the Jacobian matrix for a second-order quadrilateral element.
        !>
        pure module function jacobian_square_second(self, r, node_coords, connectivity) result(jacobian)
            implicit none
            !> The second-order quadrilateral element object.
            class(type_square_second), intent(in) :: self
            !> The local coordinate vector.
            type(type_coordinate_dp), intent(in) :: r
            !> The global coordinates of the mesh nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The resulting Jacobian matrix.
            real(real64) :: jacobian(self%get_dimension(), self%get_dimension())
        end function jacobian_square_second

        !>
        !> Calculates the Jacobian determinant for a second-order quadrilateral element.
        !>
        pure module function jacobian_det_square_second(self, r, node_coords, connectivity) result(jacobian_det)
            implicit none
            !> The second-order quadrilateral element object.
            class(type_square_second), intent(in) :: self
            !> The local coordinate vector.
            type(type_coordinate_dp), intent(in) :: r
            !> The global coordinates of the mesh nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The Jacobian determinant.
            real(real64) :: jacobian_det
        end function jacobian_det_square_second

        !>
        !> Checks if a given Cartesian coordinate is inside the element.
        !>
        module subroutine is_in_square_second(self, cartesian, normalized, node_coords, connectivity, is_in)
            implicit none
            !> The second-order quadrilateral element object.
            class(type_square_second), intent(in) :: self
            !> The Cartesian coordinate to check.
            type(type_coordinate_dp), intent(in) :: cartesian
            !> The corresponding normalized coordinate if the point is inside.
            type(type_coordinate_dp), intent(inout) :: normalized
            !> The global coordinates of the mesh nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The result (`.true.` if the point is inside, `.false.` otherwise).
            logical, intent(inout) :: is_in
        end subroutine is_in_square_second
    end interface

end module domain_fe_element

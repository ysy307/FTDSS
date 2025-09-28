!>
!> Defines 1D finite element types (side elements) and their associated operations.
!> This module provides types and procedures for first and second-order side (line)
!> elements, typically used for boundary conditions.
!>
module domain_fe_side
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_logger
    use :: stdlib_strings
    use :: module_core
    use :: module_input, only:type_input
    use :: domain_fe, only:abst_fe
    implicit none
    private

    public :: type_side_first
    public :: type_side_second

    public :: construct_side_first
    public :: construct_side_second

    ! ====================================================================================
    !   Type Definitions
    ! ====================================================================================

    !>
    !> Represents a first-order side (line) element.
    !>
    type, extends(abst_fe) :: type_side_first
    contains
        procedure, pass(self) :: get_geometry => get_length_side_first
        procedure, pass(self) :: psi => psi_side_first
        procedure, pass(self) :: dpsi => dpsi_side_first
        procedure, pass(self) :: jacobian => jacobian_side_first
        procedure, pass(self) :: jacobian_det => jacobian_det_side_first
        procedure, pass(self) :: is_inside => is_in_side_first
        procedure, pass(self), private :: compute_tangent_vector => compute_tangent_vector_side_first
    end type type_side_first

    !>
    !> Represents a second-order side (line) element.
    !>
    type, extends(abst_fe) :: type_side_second
    contains
        procedure, pass(self) :: get_geometry => get_length_side_second
        procedure, pass(self) :: psi => psi_side_second
        procedure, pass(self) :: dpsi => dpsi_side_second
        procedure, pass(self) :: jacobian => jacobian_side_second
        procedure, pass(self) :: jacobian_det => jacobian_det_side_second
        procedure, pass(self) :: is_inside => is_in_side_second
        procedure, pass(self), private :: compute_tangent_vector => compute_tangent_vector_side_second
    end type type_side_second

    ! ====================================================================================
    !   Interface Definitions
    ! ====================================================================================

    !--------------------------------------------------------------------------------------
    !  Side first order procedures interface
    !--------------------------------------------------------------------------------------
    interface
        !>
        !> Constructs an instance of a first-order side element.
        !>
        module function construct_side_first(input) result(fe)
            implicit none
            !> The main input data structure.
            type(type_input), intent(in) :: input
            !> The newly created and allocated finite element object.
            class(abst_fe), allocatable :: fe
        end function construct_side_first

        !>
        !> Calculates the length of a first-order side element.
        !>
        module function get_length_side_first(self, node_coords, connectivity) result(length)
            implicit none
            !> The first-order side element object.
            class(type_side_first), intent(in) :: self
            !> The global coordinates of the element's nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The calculated length of the element.
            real(real64) :: length
        end function get_length_side_first

        !>
        !> Calculates the value of the shape function \( \psi_i \) for a first-order side element.
        !>
        module pure elemental function psi_side_first(self, i, r) result(psi)
            implicit none
            !> The first-order side element object.
            class(type_side_first), intent(in) :: self
            !> The index of the shape function (local node number).
            integer(int32), intent(in) :: i
            !> The local coordinate vector \( r \). Only \( r\%x \) is used as \( \xi \).
            type(type_coordinate_dp), intent(in) :: r
            !> The value of the shape function.
            real(real64) :: psi
        end function psi_side_first

        !>
        !> Calculates the derivative of the shape function with respect to the
        !> normalized coordinate, \( \frac{d\psi_i}{d\xi} \).
        !>
        module pure elemental function dpsi_side_first(self, i, j, r) result(dpsi)
            implicit none
            !> The first-order side element object.
            class(type_side_first), intent(in) :: self
            !> The index of the shape function (local node number).
            integer(int32), intent(in) :: i
            !> The index of the coordinate to differentiate with respect to (should be 1 for \( \xi \)).
            integer(int32), intent(in) :: j
            !> The local coordinate vector \( r \).
            type(type_coordinate_dp), intent(in) :: r
            !> The value of the shape function's derivative.
            real(real64) :: dpsi
        end function dpsi_side_first

        !>
        !> Calculates the Jacobian matrix for a first-order side element.
        !>
        pure module function jacobian_side_first(self, r, node_coords, connectivity) result(jacobian)
            implicit none
            !> The first-order side element object.
            class(type_side_first), intent(in) :: self
            !> The local coordinate vector.
            type(type_coordinate_dp), intent(in) :: r
            !> The global coordinates of the element's nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The resulting Jacobian matrix.
            real(real64) :: jacobian(self%get_dimension(), self%get_dimension())
        end function jacobian_side_first

        !>
        !> Calculates the Jacobian determinant (differential length element \( dL \))
        !> for a first-order side element.
        !>
        pure module function jacobian_det_side_first(self, r, node_coords, connectivity) result(jacobian_det)
            implicit none
            !> The first-order side element object.
            class(type_side_first), intent(in) :: self
            !> The local coordinate vector.
            type(type_coordinate_dp), intent(in) :: r
            !> The global coordinates of the element's nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The Jacobian determinant.
            real(real64) :: jacobian_det
        end function jacobian_det_side_first

        !>
        !> Checks if a given Cartesian coordinate is on the element.
        !>
        module subroutine is_in_side_first(self, cartesian, normalized, node_coords, connectivity, is_in)
            implicit none
            !> The first-order side element object.
            class(type_side_first), intent(in) :: self
            !> The Cartesian coordinate to check.
            type(type_coordinate_dp), intent(in) :: cartesian
            !> The corresponding normalized coordinate if the point is on the element.
            type(type_coordinate_dp), intent(inout) :: normalized
            !> The global coordinates of the element's nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The result (true if the point is on the element, false otherwise).
            logical, intent(inout) :: is_in
        end subroutine is_in_side_first

        !>
        !> Computes the tangent vector at a point on the first-order side element.
        !>
        module pure function compute_tangent_vector_side_first(self, r, node_coords, connectivity) result(tangent_vec)
            implicit none
            !> The first-order side element object.
            class(type_side_first), intent(in) :: self
            !> The local coordinate vector.
            type(type_coordinate_dp), intent(in) :: r
            !> The global coordinates of the element's nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The computed tangent vector.
            real(real64) :: tangent_vec(3)
        end function compute_tangent_vector_side_first

    end interface

    !--------------------------------------------------------------------------------------
    !  Side Second order procedures interface
    !--------------------------------------------------------------------------------------
    interface
        !>
        !> Constructs an instance of a second-order side element.
        !>
        module function construct_side_second(input) result(fe)
            implicit none
            !> The main input data structure.
            type(type_input), intent(in) :: input
            !> The newly created and allocated finite element object.
            class(abst_fe), allocatable :: fe
        end function construct_side_second

        !>
        !> Calculates the length of a second-order side element.
        !>
        module function get_length_side_second(self, node_coords, connectivity) result(length)
            implicit none
            !> The second-order side element object.
            class(type_side_second), intent(in) :: self
            !> The global coordinates of the element's nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The calculated length of the element.
            real(real64) :: length
        end function get_length_side_second

        !>
        !> Calculates the value of the shape function \( \psi_i \) for a second-order side element.
        !>
        module pure elemental function psi_side_second(self, i, r) result(psi)
            implicit none
            !> The second-order side element object.
            class(type_side_second), intent(in) :: self
            !> The index of the shape function (local node number).
            integer(int32), intent(in) :: i
            !> The local coordinate vector \( r \). Only \( r\%x \) is used as \( \xi \).
            type(type_coordinate_dp), intent(in) :: r
            !> The value of the shape function.
            real(real64) :: psi
        end function psi_side_second

        !>
        !> Calculates the derivative of the shape function with respect to the
        !> normalized coordinate, \( \frac{d\psi_i}{d\xi} \).
        !>
        module pure elemental function dpsi_side_second(self, i, j, r) result(dpsi)
            implicit none
            !> The second-order side element object.
            class(type_side_second), intent(in) :: self
            !> The index of the shape function (local node number).
            integer(int32), intent(in) :: i
            !> The index of the coordinate to differentiate with respect to (should be 1 for \( \xi \)).
            integer(int32), intent(in) :: j
            !> The local coordinate vector \( r \).
            type(type_coordinate_dp), intent(in) :: r
            !> The value of the shape function's derivative.
            real(real64) :: dpsi
        end function dpsi_side_second

        !>
        !> Calculates the Jacobian matrix for a second-order side element.
        !>
        pure module function jacobian_side_second(self, r, node_coords, connectivity) result(jacobian)
            implicit none
            !> The second-order side element object.
            class(type_side_second), intent(in) :: self
            !> The local coordinate vector.
            type(type_coordinate_dp), intent(in) :: r
            !> The global coordinates of the element's nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The resulting Jacobian matrix.
            real(real64) :: jacobian(self%get_dimension(), self%get_dimension())
        end function jacobian_side_second

        !>
        !> Calculates the Jacobian determinant (differential length element \( dL \))
        !> for a second-order side element.
        !>
        pure module function jacobian_det_side_second(self, r, node_coords, connectivity) result(jacobian_det)
            implicit none
            !> The second-order side element object.
            class(type_side_second), intent(in) :: self
            !> The local coordinate vector.
            type(type_coordinate_dp), intent(in) :: r
            !> The global coordinates of the element's nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The Jacobian determinant.
            real(real64) :: jacobian_det
        end function jacobian_det_side_second

        !>
        !> Checks if a given Cartesian coordinate is on the element.
        !>
        module subroutine is_in_side_second(self, cartesian, normalized, node_coords, connectivity, is_in)
            implicit none
            !> The second-order side element object.
            class(type_side_second), intent(in) :: self
            !> The Cartesian coordinate to check.
            type(type_coordinate_dp), intent(in) :: cartesian
            !> The corresponding normalized coordinate if the point is on the element.
            type(type_coordinate_dp), intent(inout) :: normalized
            !> The global coordinates of the element's nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The result (true if the point is on the element, false otherwise).
            logical, intent(inout) :: is_in
        end subroutine is_in_side_second

        !>
        !> Computes the tangent vector at a point on the second-order side element.
        !>
        module pure function compute_tangent_vector_side_second(self, r, node_coords, connectivity) result(tangent_vec)
            implicit none
            !> The second-order side element object.
            class(type_side_second), intent(in) :: self
            !> The local coordinate vector.
            type(type_coordinate_dp), intent(in) :: r
            !> The global coordinates of the element's nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The computed tangent vector.
            real(real64) :: tangent_vec(3)
        end function compute_tangent_vector_side_second
    end interface

end module domain_fe_side

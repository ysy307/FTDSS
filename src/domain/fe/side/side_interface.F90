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
    use :: domain_base_fe, only:abst_fe
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

    interface
        !----------------------------------------------------------------------------------
        !  Side First Order Procedures
        !----------------------------------------------------------------------------------

        !> Constructs an instance of a first-order side element.
        module function construct_side_first(input) result(fe)
            implicit none
            type(type_input), intent(in) :: input
            class(abst_fe), allocatable :: fe
        end function construct_side_first

        !> Calculates the length of a first-order side element.
        module subroutine get_length_side_first(self, node_coords, geometry)
            implicit none
            class(type_side_first), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: geometry
        end subroutine get_length_side_first

        !> Calculates the value of the shape function psi_i.
        pure elemental module subroutine psi_side_first(self, i, r, psi_val)
            implicit none
            class(type_side_first), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: psi_val
        end subroutine psi_side_first

        !> Calculates the derivative of the shape function dpsi_i/dxi.
        pure elemental module subroutine dpsi_side_first(self, i, j, r, dpsi_val)
            implicit none
            class(type_side_first), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: dpsi_val
        end subroutine dpsi_side_first

        !> Calculates the Jacobian matrix.
        pure module subroutine jacobian_side_first(self, r, node_coords, jac)
            implicit none
            class(type_side_first), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: jac(:, :)
        end subroutine jacobian_side_first

        !> Calculates the Jacobian determinant (dL).
        pure module subroutine jacobian_det_side_first(self, r, node_coords, det_j)
            implicit none
            class(type_side_first), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: det_j
        end subroutine jacobian_det_side_first

        !> Checks if a given Cartesian coordinate is on the element.
        module subroutine is_in_side_first(self, cartesian, normalized, node_coords, is_in)
            implicit none
            class(type_side_first), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: cartesian
            type(type_coordinate_dp), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            logical, intent(inout) :: is_in
        end subroutine is_in_side_first

        !> Computes the tangent vector at a point.
        pure module subroutine compute_tangent_vector_side_first(self, r, node_coords, tangent_vec)
            implicit none
            class(type_side_first), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: tangent_vec(:)
        end subroutine compute_tangent_vector_side_first

        !----------------------------------------------------------------------------------
        !  Side Second Order Procedures
        !----------------------------------------------------------------------------------

        !> Constructs an instance of a second-order side element.
        module function construct_side_second(input) result(fe)
            implicit none
            type(type_input), intent(in) :: input
            class(abst_fe), allocatable :: fe
        end function construct_side_second

        !> Calculates the length of a second-order side element.
        module subroutine get_length_side_second(self, node_coords, geometry)
            implicit none
            class(type_side_second), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: geometry
        end subroutine get_length_side_second

        !> Calculates the value of the shape function psi_i.
        pure elemental module subroutine psi_side_second(self, i, r, psi_val)
            implicit none
            class(type_side_second), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: psi_val
        end subroutine psi_side_second

        !> Calculates the derivative of the shape function dpsi_i/dxi.
        pure elemental module subroutine dpsi_side_second(self, i, j, r, dpsi_val)
            implicit none
            class(type_side_second), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: dpsi_val
        end subroutine dpsi_side_second

        !> Calculates the Jacobian matrix.
        pure module subroutine jacobian_side_second(self, r, node_coords, jac)
            implicit none
            class(type_side_second), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: jac(:, :)
        end subroutine jacobian_side_second

        !> Calculates the Jacobian determinant (dL).
        pure module subroutine jacobian_det_side_second(self, r, node_coords, det_j)
            implicit none
            class(type_side_second), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: det_j
        end subroutine jacobian_det_side_second

        !> Checks if a given Cartesian coordinate is on the element.
        module subroutine is_in_side_second(self, cartesian, normalized, node_coords, is_in)
            implicit none
            class(type_side_second), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: cartesian
            type(type_coordinate_dp), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            logical, intent(inout) :: is_in
        end subroutine is_in_side_second

        !> Computes the tangent vector at a point.
        pure module subroutine compute_tangent_vector_side_second(self, r, node_coords, tangent_vec)
            implicit none
            class(type_side_second), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: tangent_vec(:)
        end subroutine compute_tangent_vector_side_second

    end interface

end module domain_fe_side

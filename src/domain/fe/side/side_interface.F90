module domain_fe_side
    !---------------------------------------------------------------------------------------
    !  Module: domain_fe_side
    !  Purpose: Define 1D finite element types and their associated operations.
    !           This module is refactored to align with the design of domain_fe_element,
    !           decoupling element type definitions from specific geometric instances.
    !--------------------------------------------------------------------------------------
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

    !--------------------------------------------------------------------------------------
    !  Side First Order Element Type
    !--------------------------------------------------------------------------------------
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

    !--------------------------------------------------------------------------------------
    !  Side Second Order Element Type
    !--------------------------------------------------------------------------------------
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

    !--------------------------------------------------------------------------------------
    !  Side first order procedures interface
    !--------------------------------------------------------------------------------------
    interface
        module function construct_side_first(input) result(fe)
            implicit none
            type(type_input), intent(in) :: input
            class(abst_fe), allocatable :: fe
        end function construct_side_first

        module function get_length_side_first(self, node_coords, connectivity) result(length)
            implicit none
            class(type_side_first), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: length
        end function get_length_side_first

        module pure elemental function psi_side_first(self, i, r) result(psi)
            implicit none
            class(type_side_first), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: psi
        end function psi_side_first

        module pure elemental function dpsi_side_first(self, i, j, r) result(dpsi)
            implicit none
            class(type_side_first), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi
        end function dpsi_side_first

        pure module function jacobian_side_first(self, r, node_coords, connectivity) result(jacobian)
            implicit none
            class(type_side_first), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: jacobian(self%get_dimension(), self%get_dimension())
        end function jacobian_side_first

        pure module function jacobian_det_side_first(self, r, node_coords, connectivity) result(jacobian_det)
            implicit none
            class(type_side_first), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: jacobian_det
        end function jacobian_det_side_first

        module subroutine is_in_side_first(self, cartesian, normalized, node_coords, connectivity, is_in)
            implicit none
            class(type_side_first), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: cartesian
            type(type_dp_vector_3d), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            logical, intent(inout) :: is_in
        end subroutine is_in_side_first

        module pure function compute_tangent_vector_side_first(self, r, node_coords, connectivity) result(tangent_vec)
            implicit none
            class(type_side_first), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: tangent_vec(3)

        end function compute_tangent_vector_side_first

    end interface

    !--------------------------------------------------------------------------------------
    !  Side Second order procedures interface
    !--------------------------------------------------------------------------------------
    interface
        module function construct_side_second(input) result(fe)
            implicit none
            type(type_input), intent(in) :: input
            class(abst_fe), allocatable :: fe
        end function construct_side_second

        module function get_length_side_second(self, node_coords, connectivity) result(length)
            implicit none
            class(type_side_second), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: length
        end function get_length_side_second

        module pure elemental function psi_side_second(self, i, r) result(psi)
            implicit none
            class(type_side_second), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: psi
        end function psi_side_second

        module pure elemental function dpsi_side_second(self, i, j, r) result(dpsi)
            implicit none
            class(type_side_second), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi
        end function dpsi_side_second

        pure module function jacobian_side_second(self, r, node_coords, connectivity) result(jacobian)
            implicit none
            class(type_side_second), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: jacobian(self%get_dimension(), self%get_dimension())
        end function jacobian_side_second

        pure module function jacobian_det_side_second(self, r, node_coords, connectivity) result(jacobian_det)
            implicit none
            class(type_side_second), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: jacobian_det
        end function jacobian_det_side_second

        module subroutine is_in_side_second(self, cartesian, normalized, node_coords, connectivity, is_in)
            implicit none
            class(type_side_second), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: cartesian
            type(type_dp_vector_3d), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            logical, intent(inout) :: is_in
        end subroutine is_in_side_second

        module pure function compute_tangent_vector_side_second(self, r, node_coords, connectivity) result(tangent_vec)
            implicit none
            class(type_side_second), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: tangent_vec(3)

        end function compute_tangent_vector_side_second
    end interface

end module domain_fe_side

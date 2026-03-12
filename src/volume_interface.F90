!>
!> Defines 3D finite element types (tetrahedra, hexahedra) and their
!> associated operations, such as shape functions and Jacobians.
!>
module domain_fe_volume
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_logger
    use :: stdlib_strings
    use :: module_core
    use :: domain_base_fe, only:abst_fe
    use :: domain_fe_integration, only:type_gauss_integration_rule
    implicit none
    private

    public :: type_tetra_first
    public :: type_tetra_second
    public :: type_tetra_third
    public :: type_hexa_first
    public :: type_hexa_second
    public :: type_hexa_third

    public :: construct_tetra_first
    public :: construct_tetra_second
    public :: construct_tetra_third
    public :: construct_hexa_first
    public :: construct_hexa_second
    public :: construct_hexa_third

    ! ====================================================================================
    !   Type Definitions
    ! ====================================================================================

    !> 4-node linear tetrahedron (P1)
    type, extends(abst_fe) :: type_tetra_first
    contains
        procedure, pass(self) :: calc_measure => calc_volume_tetra_first
        procedure, pass(self) :: calc_psi => calc_psi_tetra_first
        procedure, pass(self) :: calc_dpsi => calc_dpsi_tetra_first
        procedure, pass(self) :: calc_jacobian => calc_jacobian_tetra_first
        procedure, pass(self) :: is_inside => is_in_tetra_first
    end type type_tetra_first

    !> 10-node quadratic tetrahedron (P2)
    type, extends(abst_fe) :: type_tetra_second
    contains
        procedure, pass(self) :: calc_measure => calc_volume_tetra_second
        procedure, pass(self) :: calc_psi => calc_psi_tetra_second
        procedure, pass(self) :: calc_dpsi => calc_dpsi_tetra_second
        procedure, pass(self) :: calc_jacobian => calc_jacobian_tetra_second
        procedure, pass(self) :: is_inside => is_in_tetra_second
    end type type_tetra_second

    !> 20-node cubic tetrahedron (P3)
    type, extends(abst_fe) :: type_tetra_third
    contains
        procedure, pass(self) :: calc_measure => calc_volume_tetra_third
        procedure, pass(self) :: calc_psi => calc_psi_tetra_third
        procedure, pass(self) :: calc_dpsi => calc_dpsi_tetra_third
        procedure, pass(self) :: calc_jacobian => calc_jacobian_tetra_third
        procedure, pass(self) :: is_inside => is_in_tetra_third
    end type type_tetra_third

    !> 8-node trilinear hexahedron (Q1)
    type, extends(abst_fe) :: type_hexa_first
    contains
        procedure, pass(self) :: calc_measure => calc_volume_hexa_first
        procedure, pass(self) :: calc_psi => calc_psi_hexa_first
        procedure, pass(self) :: calc_dpsi => calc_dpsi_hexa_first
        procedure, pass(self) :: calc_jacobian => calc_jacobian_hexa_first
        procedure, pass(self) :: is_inside => is_in_hexa_first
    end type type_hexa_first

    !> 27-node triquadratic hexahedron (Q2)
    type, extends(abst_fe) :: type_hexa_second
    contains
        procedure, pass(self) :: calc_measure => calc_volume_hexa_second
        procedure, pass(self) :: calc_psi => calc_psi_hexa_second
        procedure, pass(self) :: calc_dpsi => calc_dpsi_hexa_second
        procedure, pass(self) :: calc_jacobian => calc_jacobian_hexa_second
        procedure, pass(self) :: is_inside => is_in_hexa_second
    end type type_hexa_second

    !> 64-node tricubic hexahedron (Q3)
    type, extends(abst_fe) :: type_hexa_third
    contains
        procedure, pass(self) :: calc_measure => calc_volume_hexa_third
        procedure, pass(self) :: calc_psi => calc_psi_hexa_third
        procedure, pass(self) :: calc_dpsi => calc_dpsi_hexa_third
        procedure, pass(self) :: calc_jacobian => calc_jacobian_hexa_third
        procedure, pass(self) :: is_inside => is_in_hexa_third
    end type type_hexa_third

    ! ====================================================================================
    !   Interface Definitions for Submodule Implementation
    ! ====================================================================================
    interface
        ! --- Tetra First Order ---
        module function construct_tetra_first(integration_order) result(fe)
            implicit none
            integer(int32), intent(in) :: integration_order
            class(abst_fe), allocatable :: fe
        end function construct_tetra_first

        module subroutine calc_volume_tetra_first(self, node_coords, measure)
            implicit none
            class(type_tetra_first), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: measure
        end subroutine calc_volume_tetra_first

        pure elemental module subroutine calc_psi_tetra_first(self, i, r, psi_val)
            implicit none
            class(type_tetra_first), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: psi_val
        end subroutine calc_psi_tetra_first

        pure elemental module subroutine calc_dpsi_tetra_first(self, i, j, r, dpsi_val)
            implicit none
            class(type_tetra_first), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: dpsi_val
        end subroutine calc_dpsi_tetra_first

        pure module subroutine calc_jacobian_tetra_first(self, r, node_coords, jac)
            implicit none
            class(type_tetra_first), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: jac(:, :)
        end subroutine calc_jacobian_tetra_first

        module subroutine is_in_tetra_first(self, cartesian, normalized, node_coords, is_in)
            implicit none
            class(type_tetra_first), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: cartesian
            type(type_coordinate_dp), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            logical, intent(inout) :: is_in
        end subroutine is_in_tetra_first

        ! --- Tetra Second Order ---
        module function construct_tetra_second(integration_order) result(fe)
            implicit none
            integer(int32), intent(in) :: integration_order
            class(abst_fe), allocatable :: fe
        end function construct_tetra_second

        module subroutine calc_volume_tetra_second(self, node_coords, measure)
            implicit none
            class(type_tetra_second), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: measure
        end subroutine calc_volume_tetra_second

        pure elemental module subroutine calc_psi_tetra_second(self, i, r, psi_val)
            implicit none
            class(type_tetra_second), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: psi_val
        end subroutine calc_psi_tetra_second

        pure elemental module subroutine calc_dpsi_tetra_second(self, i, j, r, dpsi_val)
            implicit none
            class(type_tetra_second), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: dpsi_val
        end subroutine calc_dpsi_tetra_second

        pure module subroutine calc_jacobian_tetra_second(self, r, node_coords, jac)
            implicit none
            class(type_tetra_second), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: jac(:, :)
        end subroutine calc_jacobian_tetra_second

        module subroutine is_in_tetra_second(self, cartesian, normalized, node_coords, is_in)
            implicit none
            class(type_tetra_second), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: cartesian
            type(type_coordinate_dp), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            logical, intent(inout) :: is_in
        end subroutine is_in_tetra_second

        ! --- Tetra Third Order ---
        module function construct_tetra_third(integration_order) result(fe)
            implicit none
            integer(int32), intent(in) :: integration_order
            class(abst_fe), allocatable :: fe
        end function construct_tetra_third

        module subroutine calc_volume_tetra_third(self, node_coords, measure)
            implicit none
            class(type_tetra_third), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: measure
        end subroutine calc_volume_tetra_third

        pure elemental module subroutine calc_psi_tetra_third(self, i, r, psi_val)
            implicit none
            class(type_tetra_third), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: psi_val
        end subroutine calc_psi_tetra_third

        pure elemental module subroutine calc_dpsi_tetra_third(self, i, j, r, dpsi_val)
            implicit none
            class(type_tetra_third), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: dpsi_val
        end subroutine calc_dpsi_tetra_third

        pure module subroutine calc_jacobian_tetra_third(self, r, node_coords, jac)
            implicit none
            class(type_tetra_third), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: jac(:, :)
        end subroutine calc_jacobian_tetra_third

        module subroutine is_in_tetra_third(self, cartesian, normalized, node_coords, is_in)
            implicit none
            class(type_tetra_third), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: cartesian
            type(type_coordinate_dp), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            logical, intent(inout) :: is_in
        end subroutine is_in_tetra_third

        ! --- Hexa First Order ---
        module function construct_hexa_first(integration_order) result(fe)
            implicit none
            integer(int32), intent(in) :: integration_order
            class(abst_fe), allocatable :: fe
        end function construct_hexa_first

        module subroutine calc_volume_hexa_first(self, node_coords, measure)
            implicit none
            class(type_hexa_first), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: measure
        end subroutine calc_volume_hexa_first

        pure elemental module subroutine calc_psi_hexa_first(self, i, r, psi_val)
            implicit none
            class(type_hexa_first), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: psi_val
        end subroutine calc_psi_hexa_first

        pure elemental module subroutine calc_dpsi_hexa_first(self, i, j, r, dpsi_val)
            implicit none
            class(type_hexa_first), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: dpsi_val
        end subroutine calc_dpsi_hexa_first

        pure module subroutine calc_jacobian_hexa_first(self, r, node_coords, jac)
            implicit none
            class(type_hexa_first), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: jac(:, :)
        end subroutine calc_jacobian_hexa_first

        module subroutine is_in_hexa_first(self, cartesian, normalized, node_coords, is_in)
            implicit none
            class(type_hexa_first), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: cartesian
            type(type_coordinate_dp), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            logical, intent(inout) :: is_in
        end subroutine is_in_hexa_first

        ! --- Hexa Second Order ---
        module function construct_hexa_second(integration_order) result(fe)
            implicit none
            integer(int32), intent(in) :: integration_order
            class(abst_fe), allocatable :: fe
        end function construct_hexa_second

        module subroutine calc_volume_hexa_second(self, node_coords, measure)
            implicit none
            class(type_hexa_second), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: measure
        end subroutine calc_volume_hexa_second

        pure elemental module subroutine calc_psi_hexa_second(self, i, r, psi_val)
            implicit none
            class(type_hexa_second), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: psi_val
        end subroutine calc_psi_hexa_second

        pure elemental module subroutine calc_dpsi_hexa_second(self, i, j, r, dpsi_val)
            implicit none
            class(type_hexa_second), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: dpsi_val
        end subroutine calc_dpsi_hexa_second

        pure module subroutine calc_jacobian_hexa_second(self, r, node_coords, jac)
            implicit none
            class(type_hexa_second), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: jac(:, :)
        end subroutine calc_jacobian_hexa_second

        module subroutine is_in_hexa_second(self, cartesian, normalized, node_coords, is_in)
            implicit none
            class(type_hexa_second), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: cartesian
            type(type_coordinate_dp), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            logical, intent(inout) :: is_in
        end subroutine is_in_hexa_second

        ! --- Hexa Third Order ---
        module function construct_hexa_third(integration_order) result(fe)
            implicit none
            integer(int32), intent(in) :: integration_order
            class(abst_fe), allocatable :: fe
        end function construct_hexa_third

        module subroutine calc_volume_hexa_third(self, node_coords, measure)
            implicit none
            class(type_hexa_third), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: measure
        end subroutine calc_volume_hexa_third

        pure elemental module subroutine calc_psi_hexa_third(self, i, r, psi_val)
            implicit none
            class(type_hexa_third), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: psi_val
        end subroutine calc_psi_hexa_third

        pure elemental module subroutine calc_dpsi_hexa_third(self, i, j, r, dpsi_val)
            implicit none
            class(type_hexa_third), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: dpsi_val
        end subroutine calc_dpsi_hexa_third

        pure module subroutine calc_jacobian_hexa_third(self, r, node_coords, jac)
            implicit none
            class(type_hexa_third), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: jac(:, :)
        end subroutine calc_jacobian_hexa_third

        module subroutine is_in_hexa_third(self, cartesian, normalized, node_coords, is_in)
            implicit none
            class(type_hexa_third), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: cartesian
            type(type_coordinate_dp), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            logical, intent(inout) :: is_in
        end subroutine is_in_hexa_third

    end interface

end module domain_fe_volume

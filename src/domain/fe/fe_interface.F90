!>
!> Defines the abstract interface and common functionalities for finite elements.
!>
module domain_fe
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: mpi_f08
    use :: stdlib_logger
    use :: stdlib_strings
    use :: module_core

    implicit none
    private

    public :: abst_fe
    public :: holder_fes

    !>
    !> An abstract base type for finite element objects.
    !> It provides a unified interface for various element types, defining common
    !> properties like Gauss points and deferred procedures for element-specific
    !> calculations such as shape functions and Jacobians.
    !>
    type, abstract :: abst_fe
        private

        !> The element type identifier, typically corresponding to a VTK cell type ID.
        integer(int32) :: type
        !> The number of nodes defining the element.
        integer(int32) :: num_nodes
        !> The geometric dimension of the element (1 for line, 2 for surface, 3 for volume).
        integer(int32) :: dimension
        !> The interpolation order of the element's shape functions (e.g., 1 for linear, 2 for quadratic).
        integer(int32) :: order
        !> The number of Gauss points used for numerical integration.
        integer(int32) :: num_gauss
        !> An array of weights for Gauss integration points.
        real(real64), allocatable :: weight(:)
        !> An array containing the local coordinates of the Gauss integration points.
        type(type_coordinate_dp), allocatable :: gauss(:)

    contains
        !---------------------------------------------------------------------------------------------------------------------------
        ! Public methods with common implementations
        !---------------------------------------------------------------------------------------------------------------------------
        procedure, pass(self), public :: initialize => initialize_abst_fe
        procedure, pass(self), public :: destroy => destroy_abst_fe
        procedure, pass(self), public :: get_type
        procedure, pass(self), public :: get_num_nodes
        procedure, pass(self), public :: get_dimension
        procedure, pass(self), public :: get_order
        procedure, pass(self), public :: get_num_gauss
        procedure, pass(self), public :: get_weight
        procedure, pass(self), public :: get_gauss
        procedure, pass(self), public :: lerp
        procedure, pass(self), public :: dlerp
        procedure, pass(self), public :: display
        !---------------------------------------------------------------------------------------------------------------------------
        !  Abstract methods to be implemented in derived types
        !---------------------------------------------------------------------------------------------------------------------------
        procedure(abst_get_geometry), pass(self), public, deferred :: get_geometry !&
        procedure(abst_psi),          pass(self), public, deferred :: psi !&
        procedure(abst_dpsi),         pass(self), public, deferred :: dpsi !&
        procedure(abst_jacobian),     pass(self), public, deferred :: jacobian !&
        procedure(abst_jacobian_det), pass(self), public, deferred :: jacobian_det !&
        procedure(abst_is_inside),    pass(self), public, deferred :: is_inside !&
    end type

    abstract interface
        !>
        !> Computes a primary geometric property of the element.
        !> For 1D elements, this returns the length. For 2D, area. For 3D, volume.
        !>
        function abst_get_geometry(self, node_coords, connectivity) result(geometry)
            import :: abst_fe, int32, real64
            implicit none
            !> The finite element object.
            class(abst_fe), intent(in) :: self
            !> The global coordinates of the element's nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The computed geometric property (length, area, or volume).
            real(real64) :: geometry
        end function

        !>
        !> Evaluates the i-th shape function \( \psi_i \) at a given local coordinate
        !> \( r = (\xi, \eta, \zeta) \).
        !>
        pure elemental function abst_psi(self, i, r) result(psi_val)
            import :: abst_fe, type_coordinate_dp, int32, real64
            implicit none
            !> The finite element object.
            class(abst_fe), intent(in) :: self
            !> The index of the shape function (corresponding to a local node).
            integer(int32), intent(in) :: i
            !> The local coordinate vector \( r \).
            type(type_coordinate_dp), intent(in) :: r
            !> The value of the shape function, \( \psi_i (r) \).
            real(real64) :: psi_val
        end function abst_psi

        !>
        !> Evaluates the partial derivative of the i-th shape function \( \psi_i \)
        !> with respect to the j-th local coordinate, \( \frac{\partial \psi_i}{\partial r_j} \).
        !>
        pure elemental function abst_dpsi(self, i, j, r) result(dpsi_val)
            import :: abst_fe, type_coordinate_dp, int32, real64
            implicit none
            !> The finite element object.
            class(abst_fe), intent(in) :: self
            !> The index of the shape function.
            integer(int32), intent(in) :: i
            !> The index of the local coordinate (1=\( \xi \), 2=\( \eta \), 3=\( \zeta \)).
            integer(int32), intent(in) :: j
            !> The local coordinate vector \( r \).
            type(type_coordinate_dp), intent(in) :: r
            !> The value of the derivative, \( \frac{\partial \psi_i}{\partial r_j} \).
            real(real64) :: dpsi_val
        end function abst_dpsi

        !>
        !> Computes the Jacobian matrix \( J \), which maps derivatives from local
        !> coordinates to global coordinates, where \( J_{ij} = \frac{\partial x_i}{\partial \xi_j} \).
        !>
        pure function abst_jacobian(self, r, node_coords, connectivity) result(jac)
            import :: abst_fe, type_coordinate_dp, int32, real64
            implicit none
            !> The finite element object.
            class(abst_fe), intent(in) :: self
            !> The local coordinate vector where the Jacobian is evaluated.
            type(type_coordinate_dp), intent(in) :: r
            !> The global coordinates of the element's nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The Jacobian matrix at the specified local coordinate.
            real(real64) :: jac(self%dimension, self%dimension)
        end function abst_jacobian

        !>
        !> Computes the determinant of the Jacobian matrix, \( |J| \).
        !> This value is used as the scaling factor for transforming integrals
        !> from global to local coordinate systems.
        !>
        pure function abst_jacobian_det(self, r, node_coords, connectivity) result(det_j)
            import :: abst_fe, type_coordinate_dp, int32, real64
            implicit none
            !> The finite element object.
            class(abst_fe), intent(in) :: self
            !> The local coordinate vector where the determinant is evaluated.
            type(type_coordinate_dp), intent(in) :: r
            !> The global coordinates of the element's nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> The determinant of the Jacobian matrix.
            real(real64) :: det_j
        end function abst_jacobian_det

        !>
        !> Checks if a point in global coordinates is inside the element.
        !> If the point is inside, this subroutine also computes its corresponding
        !> local coordinates.
        !>
        subroutine abst_is_inside(self, cartesian, normalized, node_coords, connectivity, is_in)
            import abst_fe, type_coordinate_dp, int32, real64
            implicit none
            !> The finite element object.
            class(abst_fe), intent(in) :: self
            !> The point in global (Cartesian) coordinates to check.
            type(type_coordinate_dp), intent(in) :: cartesian
            !> The resulting local (normalized) coordinates if the point is inside.
            type(type_coordinate_dp), intent(inout) :: normalized
            !> The global coordinates of the element's nodes.
            real(real64), intent(in) :: node_coords(:, :)
            !> The connectivity array for the element.
            integer(int32), intent(in) :: connectivity(:)
            !> A logical flag, set to `.true.` if the point is inside, `.false.` otherwise.
            logical, intent(inout) :: is_in
        end subroutine abst_is_inside
    end interface

    !>
    !> A wrapper type that owns a polymorphic finite element object.
    !> This is used to allocate and store a specific, concrete FE derived type at runtime.
    !>
    type :: holder_fes
        !> An allocatable, polymorphic finite element object.
        class(abst_fe), allocatable :: fe
    end type holder_fes

contains

    !>
    !> Initializes the common properties of a finite element object.
    !>
    subroutine initialize_abst_fe(self, type, dimension, order, num_nodes, num_gauss, weight, gauss)
        implicit none
        !> The finite element object to initialize.
        class(abst_fe), intent(inout) :: self
        !> The element type, dimension, order, node count, and Gauss point count.
        integer(int32), intent(in) :: type, dimension, order, num_nodes, num_gauss
        !> The weights for the Gauss integration points.
        real(real64), intent(in) :: weight(:)
        !> The local coordinates of the Gauss integration points.
        real(real64), intent(in) :: gauss(:, :) ! (3, num_gauss)
        integer(int32) :: i

        self%type = type
        self%dimension = dimension
        self%order = order
        self%num_nodes = num_nodes
        self%num_gauss = num_gauss

        call allocate_array(self%weight, self%num_gauss)
        self%weight(:) = weight(:)

        allocate (self%gauss(self%num_gauss))
        do i = 1, self%num_gauss
            call self%gauss(i)%set(gauss(1, i), gauss(2, i), gauss(3, i))
        end do
    end subroutine initialize_abst_fe

    !>
    !> Returns the VTK cell type ID of the element.
    !>
    pure function get_type(self) result(val)
        implicit none
        !> The finite element object.
        class(abst_fe), intent(in) :: self
        !> The VTK cell type ID.
        integer(int32) :: val
        val = self%type
    end function get_type

    !>
    !> Returns the number of nodes in the element.
    !>
    pure function get_num_nodes(self) result(val)
        implicit none
        !> The finite element object.
        class(abst_fe), intent(in) :: self
        !> The number of nodes.
        integer(int32) :: val
        val = self%num_nodes
    end function get_num_nodes

    !>
    !> Returns the spatial dimension of the element.
    !>
    pure function get_dimension(self) result(val)
        implicit none
        !> The finite element object.
        class(abst_fe), intent(in) :: self
        !> The spatial dimension (1, 2, or 3).
        integer(int32) :: val
        val = self%dimension
    end function get_dimension

    !>
    !> Returns the polynomial order of the element's interpolation.
    !>
    pure function get_order(self) result(val)
        implicit none
        !> The finite element object.
        class(abst_fe), intent(in) :: self
        !> The polynomial order (e.g., 1 for linear, 2 for quadratic).
        integer(int32) :: val
        val = self%order
    end function get_order

    !>
    !> Returns the number of Gauss integration points.
    !>
    pure function get_num_gauss(self) result(val)
        implicit none
        !> The finite element object.
        class(abst_fe), intent(in) :: self
        !> The number of Gauss integration points.
        integer(int32) :: val
        val = self%num_gauss
    end function get_num_gauss

    !>
    !> Returns the weights of the Gauss integration points.
    !>
    function get_weight(self) result(val)
        implicit none
        !> The finite element object.
        class(abst_fe), intent(in) :: self
        !> An allocatable array that will contain the Gauss weights.
        real(real64), allocatable :: val(:)
        val = self%weight
    end function get_weight

    !>
    !> Returns the local coordinates of the Gauss integration points.
    !>
    function get_gauss(self) result(val)
        implicit none
        !> The finite element object.
        class(abst_fe), intent(in) :: self
        !> An allocatable array that will contain the Gauss points.
        type(type_coordinate_dp), allocatable :: val(:)
        val = self%gauss
    end function get_gauss

    !>
    !> Interpolates a field variable at a given local coordinate.
    !> This performs the summation \( \sum_{i=1}^{N} \psi_i(r) u_i \), where \( u_i \) are nodal values.
    !>
    pure function lerp(self, r, global_values, connectivity) result(val)
        implicit none
        !> The finite element object.
        class(abst_fe), intent(in) :: self
        !> The local coordinate vector \( r \) where the interpolation is evaluated.
        type(type_coordinate_dp), intent(in) :: r
        !> The array of values at all nodes in the mesh.
        real(real64), intent(in) :: global_values(:)
        !> The connectivity array for the element.
        integer(int32), intent(in) :: connectivity(:)
        !> The interpolated value at the local coordinate \( r \).
        real(real64) :: val
        integer(int32) :: i

        val = 0.0d0
        do i = 1, self%num_nodes
            val = val + self%psi(i, r) * global_values(connectivity(i))
        end do
    end function lerp

    !>
    !> Computes the gradient of an interpolated field at a given local coordinate.
    !> This performs the summation \( \sum_{i=1}^{N} \nabla\psi_i(r) u_i \).
    !>
    pure function dlerp(self, r, global_values, connectivity) result(val)
        implicit none
        !> The finite element object.
        class(abst_fe), intent(in) :: self
        !> The local coordinate vector \( r \) where the derivative is evaluated.
        type(type_coordinate_dp), intent(in) :: r
        !> The array of values at all nodes in the mesh.
        real(real64), intent(in) :: global_values(:)
        !> The connectivity array for the element.
        integer(int32), intent(in) :: connectivity(:)
        !> The gradient of the interpolated value at the local coordinate \( r \).
        type(type_coordinate_dp) :: val
        integer(int32) :: i

        val%x = 0.0d0
        val%y = 0.0d0
        val%z = 0.0d0
        do i = 1, self%num_nodes
            if (self%dimension >= 1) then
                val%x = val%x + self%dpsi(i, 1, r) * global_values(connectivity(i))
            end if
            if (self%dimension >= 2) then
                val%y = val%y + self%dpsi(i, 2, r) * global_values(connectivity(i))
            end if
            if (self%dimension >= 3) then
                val%z = val%z + self%dpsi(i, 3, r) * global_values(connectivity(i))
            end if
        end do
    end function dlerp

    !>
    !> Displays the properties of the finite element to the global logger.
    !>
    subroutine display(self)
        implicit none
        !> The finite element object to display.
        class(abst_fe), intent(in) :: self

        character(256) :: msg
        integer(int32) :: i

        call global_logger%log_information("========================================================")
        call global_logger%log_information(" Finite Element Information ")
        call global_logger%log_information("========================================================")
        write (msg, '(a, i0)') "Element Type       : ", self%type
        call global_logger%log_information(strip(msg))
        write (msg, '(a, i0)') "Dimension          : ", self%dimension
        call global_logger%log_information(strip(msg))
        write (msg, '(a, i0)') "Order              : ", self%order
        call global_logger%log_information(strip(msg))
        write (msg, '(a, i0)') "Number of Nodes    : ", self%num_nodes
        call global_logger%log_information(strip(msg))
        write (msg, '(a, i0)') "Number of Gauss Pts: ", self%num_gauss
        call global_logger%log_information(strip(msg))

        if (self%num_gauss > 0) then
            call global_logger%log_information("--------------------------------------------------------")
            call global_logger%log_information("Gauss Quadrature Points and Weights:")
            call global_logger%log_information("--------------------------------------------------------")
            do i = 1, self%num_gauss
                write (msg, '(a, i2, a, 3(f12.8, a), f12.8)') "  GP ", i, ": (", &
                    self%gauss(i)%x, ", ", self%gauss(i)%y, ", ", self%gauss(i)%z, &
                    ")  Weight = ", self%weight(i)
                call global_logger%log_information(strip(msg))
            end do
        end if
        call global_logger%log_information("========================================================")
    end subroutine display

    !>
    !> Deallocates resources associated with the finite element object.
    !>
    subroutine destroy_abst_fe(self)
        implicit none
        !> The finite element object to destroy.
        class(abst_fe), intent(inout) :: self
        if (allocated(self%weight)) then
            deallocate (self%weight)
        end if
        if (allocated(self%gauss)) then
            deallocate (self%gauss)
        end if
    end subroutine destroy_abst_fe

end module domain_fe

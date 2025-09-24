!>
!> @brief Module for finite element interface definitions
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
    !> @brief Abstract base type for finite element objects
    !>
    !> Provides unified interface for shape functions and Gauss integration points.
    type, abstract :: abst_fe
        private

        !>
        !> @brief Element type (e.g. VTK cell type ID)
        !>
        integer(int32) :: type
        !>
        !> @brief Number of nodes
        !>
        integer(int32) :: num_nodes
        !>
        !> @brief Geometric dimension (1,2,3)
        !>
        integer(int32) :: dimension
        !>
        !> @brief Interpolation order
        !>
        integer(int32) :: order
        !>
        !> @brief Number of Gauss points
        !>
        integer(int32) :: num_gauss
        !>
        !> @brief Weights for Gauss integration
        !>
        real(real64), allocatable :: weight(:)
        !>
        !> @brief Coordinates of Gauss integration points
        !>
        type(type_dp_vector_3d), allocatable :: gauss(:)

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
        !> @brief Computes a geometric property of the element
        !>
        !>  For 1D elements, this returns the length.
        !>  For 2D elements, this returns the area.
        !>  For 3D elements, this returns the volume.
        !> @param[in] self The fe element object.
        !>
        !> @param[in] node_coords The global coordinates of the element's nodes.
        !> @param[in] connectivity The mapping from local node indices to global node indices.
        !>
        !> @return The geometric property (length, area, or volume).
        !>
        function abst_get_geometry(self, node_coords, connectivity) result(geometry)
            import :: abst_fe, int32, real64
            implicit none
            class(abst_fe), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: geometry
        end function

        !>
        !> @brief Shape function
        !>
        !>  Evaluates the i-th shape function ψ_i at a given local coordinate
        !>  r = (ξ, η, ζ).
        !>
        !> @param[in] self The fe element object.
        !> @param[in] i The index of the shape function (corresponding to a node).
        !> @param[in] r The local coordinate vector.
        !>
        !> @return The value of the shape function, ψ_i(r).
        !>
        pure elemental function abst_psi(self, i, r) result(psi_val)
            import :: abst_fe, type_dp_vector_3d, int32, real64
            implicit none
            class(abst_fe), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: psi_val
        end function abst_psi

        !>
        !> @brief Derivative of the shape function
        !>
        !>  Evaluates the partial derivative of the i-th shape function ψ_i with
        !>  respect to the j-th local coordinate  ( d(ψ_i)/d(x_j)).
        !>
        !> @param[in] self The fe element object.
        !> @param[in] i The index of the shape function (corresponding to a node).
        !> @param[in] j The index of the local coordinate (1=ξ, 2=η, 3=ζ).
        !> @param[in] r The local coordinate vector.
        !>
        !> @return The value of the derivative, d(ψ_i)/d(x_j).
        !>
        pure elemental function abst_dpsi(self, i, j, r) result(dpsi_val)
            import :: abst_fe, type_dp_vector_3d, int32, real64
            implicit none
            class(abst_fe), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi_val
        end function abst_dpsi

        !>
        !> @brief Jacobian matrix
        !>
        !>  Computes the Jacobian matrix J, which maps derivatives from local
        !>  coordinates (ξ, η) to global coordinates (x, y).
        !>  J_ij = d(x_i) / d(xi_j)
        !>
        !> @param[in] self The fe element object.
        !> @param[in] r The local coordinate vector where the Jacobian is evaluated.
        !> @param[in] node_coords The global coordinates of the element's nodes.
        !> @param[in] connectivity The mapping from local node indices to global node indices.
        !>
        !> @return The Jacobian matrix at the specified local coordinate.
        !>
        pure function abst_jacobian(self, r, node_coords, connectivity) result(jac)
            import :: abst_fe, type_dp_vector_3d, int32, real64
            implicit none
            class(abst_fe), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :) ! (dimension, num_nodes)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: jac(self%dimension, self%dimension)
        end function abst_jacobian

        !>
        !> @brief Determinant of the Jacobian matrix
        !>
        !>  Computes the determinant of the Jacobian matrix, |J|. This value is
        !>  used as the scaling factor for integration
        !>
        !>  @param[in] self The fe element object.
        !>  @param[in] r The local coordinate vector where the determinant is evaluated.
        !>  @param[in] node_coords The global coordinates of the element's nodes.
        !>  @param[in] connectivity The mapping from local node indices to global node indices
        !>
        !> @return The determinant of the Jacobian matrix.
        !>
        pure function abst_jacobian_det(self, r, node_coords, connectivity) result(det_j)
            import :: abst_fe, type_dp_vector_3d, int32, real64
            implicit none
            class(abst_fe), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :) ! (dimension, num_nodes)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: det_j
        end function abst_jacobian_det

        !>
        !> @brief Checks if a point in global coordinates is inside the element.
        !>
        !>  Determines if a point given in global coordinates lies within the
        !>  boundaries of the finite element. If the point is inside, the local
        !>  coordinates (xi, eta) are computed.
        !>
        !> @param[in] self The fe element object.
        !> @param[in] cartesian The point in global coordinates to check.
        !> @param[inout] normalized The local coordinates (xi, eta) if the point is inside.
        !> @param[in] node_coords The global coordinates of the element's nodes.
        !> @param[in] connectivity The mapping from local node indices to global node indices.
        !> @param[inout] is_in Logical flag set to true if the point is inside the element.
        !>
        subroutine abst_is_inside(self, cartesian, normalized, node_coords, connectivity, is_in)
            import abst_fe, type_dp_vector_3d, int32, real64
            implicit none
            class(abst_fe), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: cartesian
            type(type_dp_vector_3d), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            logical, intent(inout) :: is_in
        end subroutine abst_is_inside
    end interface

    !> @brief Wrapper type that owns a polymorphic finite element object
    !>
    !> Used to allocate and store a specific FE derived type at runtime.
    type :: holder_fes
        !>  Allocatable polymorphic FE object
        class(abst_fe), allocatable :: fe
    end type holder_fes

contains

    !>
    !> @brief Initializes the finite element object with given parameters.
    !>
    !> @param[inout] self The fe element object to initialize.
    !> @param[in] type The VTK cell type ID.
    !> @param[in] dimension The spatial dimension of the element (1D, 2D, or 3D).
    !> @param[in] order The polynomial order of the element (1 for linear, 2 for quadratic).
    !> @param[in] num_nodes The number of nodes in the element.
    !> @param[in] num_gauss The number of Gauss integration points.
    !> @param[in] weight The weights for the Gauss integration points.
    !> @param[in] gauss The local coordinates of the Gauss integration points.
    !>
    subroutine initialize_abst_fe(self, type, dimension, order, num_nodes, num_gauss, weight, gauss)
        implicit none
        class(abst_fe), intent(inout) :: self
        integer(int32), intent(in) :: type, dimension, order, num_nodes, num_gauss
        real(real64), intent(in) :: weight(:)
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
    !> @brief Returns the VTK cell type ID of the element.
    !> @param[in] self The fe element object.
    !>
    !> @return The VTK cell type ID.
    !>
    pure function get_type(self) result(val)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32) :: val
        val = self%type
    end function get_type

    !>
    !> @brief Returns the number of nodes in the element.
    !> @param[in] self The fe element object.
    !>
    !> @return The number of nodes.
    !>
    pure function get_num_nodes(self) result(val)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32) :: val
        val = self%num_nodes
    end function get_num_nodes

    !>
    !> @brief Returns the spatial dimension of the element.
    !> @param[in] self The fe element object.
    !>
    !> @return The spatial dimension (1, 2, or 3).
    !>
    pure function get_dimension(self) result(val)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32) :: val
        val = self%dimension
    end function get_dimension

    !>
    !> @brief Returns the polynomial order of the element.
    !> @param[in] self The fe element object.
    !>
    !> @return The polynomial order (1 for linear, 2 for quadratic).
    !>
    pure function get_order(self) result(val)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32) :: val
        val = self%order
    end function get_order

    !> @brief Returns the number of Gauss integration points.
    !> @param[in] self The fe element object.
    !>
    !> @return The number of Gauss integration points.
    !>
    pure function get_num_gauss(self) result(val)
        implicit none
        class(abst_fe), intent(in) :: self
        integer(int32) :: val
        val = self%num_gauss
    end function get_num_gauss

    !>
    !> @brief Returns the weights of the Gauss integration points.
    !> @param[in] self The fe element object.
    !>
    !> @return The array of Gauss weights.
    !>
    function get_weight(self) result(val)
        implicit none
        class(abst_fe), intent(in) :: self
        real(real64), allocatable :: val(:)
        val = self%weight
    end function get_weight

    !>
    !> @brief Returns the local coordinates of the Gauss integration points.
    !> @param[in] self The fe element object.
    !>
    !> @return The array of Gauss points in local coordinates.
    !>
    function get_gauss(self) result(val)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_dp_vector_3d), allocatable :: val(:)
        val = self%gauss
    end function get_gauss

    !>
    !> @brief Linear interpolation at a given local coordinate.
    !> @param[in] self The fe element object.
    !> @param[in] r The local coordinate vector where interpolation is evaluated.
    !> @param[in] global_values The array of values at global nodes.
    !> @param[in] connectivity The mapping from local node indices to global node indices.
    !>
    !> @return The interpolated value at the local coordinate r.
    !>
    pure function lerp(self, r, global_values, connectivity) result(val)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64), intent(in) :: global_values(:)
        integer(int32), intent(in) :: connectivity(:)
        real(real64) :: val
        integer(int32) :: i

        val = 0.0d0
        do i = 1, self%num_nodes
            val = val + self%psi(i, r) * global_values(connectivity(i))
        end do
    end function lerp

    !>
    !> @brief Derivative of interpolation at a given local coordinate.
    !> @param[in] self The fe element object.
    !> @param[in] r The local coordinate vector where derivative is evaluated.
    !> @param[in] global_values The array of values at global nodes.
    !> @param[in] connectivity The mapping from local node indices to global node indices.
    !>
    !> @return The derivative of the interpolated value at the local coordinate r.
    !>
    pure function dlerp(self, r, global_values, connectivity) result(val)
        implicit none
        class(abst_fe), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64), intent(in) :: global_values(:)
        integer(int32), intent(in) :: connectivity(:)
        type(type_dp_vector_3d) :: val
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
    !> @brief Displays the properties of the finite element.
    !> @param[in] self The fe element object.
    !>
    subroutine display(self)
        implicit none
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
    !> @brief Deallocates resources associated with the finite element object.
    !> @param[inout] self The fe element object to destroy.
    !>
    subroutine destroy_abst_fe(self)
        implicit none
        class(abst_fe), intent(inout) :: self
        if (allocated(self%weight)) then
            deallocate (self%weight)
        end if
        if (allocated(self%gauss)) then
            deallocate (self%gauss)
        end if
    end subroutine destroy_abst_fe

end module domain_fe

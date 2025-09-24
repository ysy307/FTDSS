!>
!> @brief Module for defining 2-dimensional finite element types.
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
    !> @brief Type representing a first-order triangular element.
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
    !> @brief Type representing a first-order quadrilateral element.
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
    !> @brief Type representing a second-order triangular element.
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
    !> @brief Type representing a second-order quadrilateral element.
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
    !   Triangle First Order Element Type procedures interface
    !--------------------------------------------------------------------------------------
    interface
        !>
        !> @brief Constructs an instance of a first-order triangular element.
        !> @param[in] input Input parameters.
        !>
        !> @return fe The created instance of the first-order triangular element.
        !>
        module function construct_triangle_first(input) result(fe)
            implicit none
            type(type_input), intent(in) :: input
            class(abst_fe), allocatable :: fe
        end function construct_triangle_first

        !>
        !> @brief Calculates the area of a first-order triangular element.
        !> @param[in] self First-order triangular element object.
        !> @param[in] node_coords Nodal coordinate array.
        !> @param[in] connectivity Node numbers that make up the element.
        !>
        !> @return area The area of the element.
        !>
        module function get_area_triangle_first(self, node_coords, connectivity) result(area)
            implicit none
            class(type_triangle_first), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: area
        end function get_area_triangle_first

        !>
        !> @brief Calculates the value of the shape function (psi_i) for a first-order triangular element.
        !> @param[in] self First-order triangular element object.
        !> @param[in] i Index of the shape function (node number).
        !> @param[in] r Normalized coordinates.
        !>
        !> @return psi Value of the shape function.
        !>
        pure elemental module function psi_triangle_first(self, i, r) result(psi)
            implicit none
            class(type_triangle_first), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: psi
        end function psi_triangle_first

        !>
        !> @brief Calculates the derivative of the shape function w.r.t. normalized coordinates (d(psi_i)/dr_j).
        !> @param[in] self First-order triangular element object.
        !> @param[in] i Index of the shape function (node number).
        !> @param[in] j Index of the coordinate to differentiate w.r.t. (1:xi, 2:eta).
        !> @param[in] r Normalized coordinates.
        !>
        !> @return dpsi Value of the shape function's derivative.
        !>
        pure elemental module function dpsi_triangle_first(self, i, j, r) result(dpsi)
            implicit none
            class(type_triangle_first), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi
        end function dpsi_triangle_first

        !>
        !> @brief Calculates the Jacobian matrix for a first-order triangular element.
        !> @param[in] self First-order triangular element object.
        !> @param[in] r Normalized coordinates.
        !> @param[in] node_coords Nodal coordinate array.
        !> @param[in] connectivity Node numbers that make up the element.
        !>
        !> @return jacobian The Jacobian matrix.
        !>
        pure module function jacobian_triangle_first(self, r, node_coords, connectivity) result(jacobian)
            implicit none
            class(type_triangle_first), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: jacobian(self%get_dimension(), self%get_dimension())
        end function jacobian_triangle_first

        !>
        !> @brief Calculates the Jacobian determinant for a first-order triangular element.
        !> @param[in] self First-order triangular element object.
        !> @param[in] r Normalized coordinates.
        !> @param[in] node_coords Nodal coordinate array.
        !> @param[in] connectivity Node numbers that make up the element.
        !>
        !> @return jacobian_det The Jacobian determinant.
        !>
        pure module function jacobian_det_triangle_first(self, r, node_coords, connectivity) result(jacobian_det)
            implicit none
            class(type_triangle_first), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: jacobian_det
        end function jacobian_det_triangle_first

        !>
        !> @brief Checks if a given Cartesian coordinate is inside the element.
        !> @param[in] self First-order triangular element object.
        !> @param[in] cartesian Cartesian coordinate to check.
        !> @param[inout] normalized The corresponding normalized coordinate if inside.
        !> @param[in] node_coords Nodal coordinate array.
        !> @param[in] connectivity Node numbers that make up the element.
        !> @param[inout] is_in The result (true: is inside, false: is not).
        !>
        module subroutine is_in_triangle_first(self, cartesian, normalized, node_coords, connectivity, is_in)
            implicit none
            class(type_triangle_first), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: cartesian
            type(type_dp_vector_3d), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            logical, intent(inout) :: is_in
        end subroutine is_in_triangle_first
    end interface

    !--------------------------------------------------------------------------------------
    !   Square First Order Element Type procedures interface
    !--------------------------------------------------------------------------------------
    interface
        !>
        !> @brief Constructs an instance of a first-order quadrilateral element.
        !> @param[in] input Input parameters.
        !>
        !> @return fe The created instance of the first-order quadrilateral element.
        !>
        module function construct_square_first(input) result(fe)
            implicit none
            type(type_input), intent(in) :: input
            class(abst_fe), allocatable :: fe
        end function construct_square_first

        !>
        !> @brief Calculates the area of a first-order quadrilateral element.
        !> @param[in] self First-order quadrilateral element object.
        !> @param[in] node_coords Nodal coordinate array.
        !> @param[in] connectivity Node numbers that make up the element.
        !>
        !> @return area The area of the element.
        !>
        module function get_area_square_first(self, node_coords, connectivity) result(area)
            implicit none
            class(type_square_first), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: area
        end function get_area_square_first

        !>
        !> @brief Calculates the value of the shape function (psi_i) for a first-order quadrilateral element.
        !> @param[in] self First-order quadrilateral element object.
        !> @param[in] i Index of the shape function (node number).
        !> @param[in] r Normalized coordinates.
        !>
        !> @return psi Value of the shape function.
        !>
        pure elemental module function psi_square_first(self, i, r) result(psi)
            implicit none
            class(type_square_first), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: psi
        end function psi_square_first

        !>
        !> @brief Calculates the derivative of the shape function w.r.t. normalized coordinates (d(psi_i)/dr_j).
        !> @param[in] self First-order quadrilateral element object.
        !> @param[in] i Index of the shape function (node number).
        !> @param[in] j Index of the coordinate to differentiate w.r.t. (1:xi, 2:eta).
        !> @param[in] r Normalized coordinates.
        !>
        !> @return dpsi Value of the shape function's derivative.
        !>
        pure elemental module function dpsi_square_first(self, i, j, r) result(dpsi)
            implicit none
            class(type_square_first), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi
        end function dpsi_square_first

        !>
        !> @brief Calculates the Jacobian matrix for a first-order quadrilateral element.
        !> @param[in] self First-order quadrilateral element object.
        !> @param[in] r Normalized coordinates.
        !> @param[in] node_coords Nodal coordinate array.
        !> @param[in] connectivity Node numbers that make up the element.
        !>
        !> @return jacobian The Jacobian matrix.
        !>
        pure module function jacobian_square_first(self, r, node_coords, connectivity) result(jacobian)
            implicit none
            class(type_square_first), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: jacobian(self%get_dimension(), self%get_dimension())
        end function jacobian_square_first

        !>
        !> @brief Calculates the Jacobian determinant for a first-order quadrilateral element.
        !> @param[in] self First-order quadrilateral element object.
        !> @param[in] r Normalized coordinates.
        !> @param[in] node_coords Nodal coordinate array.
        !> @param[in] connectivity Node numbers that make up the element.
        !>
        !> @return jacobian_det The Jacobian determinant.
        !>
        pure module function jacobian_det_square_first(self, r, node_coords, connectivity) result(jacobian_det)
            implicit none
            class(type_square_first), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: jacobian_det
        end function jacobian_det_square_first

        !>
        !> @brief Checks if a given Cartesian coordinate is inside the element.
        !> @param[in] self First-order quadrilateral element object.
        !> @param[in] cartesian Cartesian coordinate to check.
        !> @param[inout] normalized The corresponding normalized coordinate if inside.
        !> @param[in] node_coords Nodal coordinate array.
        !> @param[in] connectivity Node numbers that make up the element.
        !> @param[inout] is_in The result (true: is inside, false: is not).
        !>
        module subroutine is_in_square_first(self, cartesian, normalized, node_coords, connectivity, is_in)
            implicit none
            class(type_square_first), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: cartesian
            type(type_dp_vector_3d), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            logical, intent(inout) :: is_in
        end subroutine is_in_square_first
    end interface

    !--------------------------------------------------------------------------------------
    !   Triangle Second Order Element Type procedures interface
    !--------------------------------------------------------------------------------------
    interface
        !>
        !> @brief Constructs an instance of a second-order triangular element.
        !> @param[in] input Input parameters.
        !>
        !> @return fe The created instance of the second-order triangular element.
        !>
        module function construct_triangle_second(input) result(fe)
            implicit none
            type(type_input), intent(in) :: input
            class(abst_fe), allocatable :: fe
        end function construct_triangle_second

        !>
        !> @brief Calculates the area of a second-order triangular element.
        !> @param[in] self Second-order triangular element object.
        !> @param[in] node_coords Nodal coordinate array.
        !> @param[in] connectivity Node numbers that make up the element.
        !>
        !> @return area The area of the element.
        !>
        module function get_area_triangle_second(self, node_coords, connectivity) result(area)
            implicit none
            class(type_triangle_second), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: area
        end function get_area_triangle_second

        !>
        !> @brief Calculates the value of the shape function (psi_i) for a second-order triangular element.
        !> @param[in] self Second-order triangular element object.
        !> @param[in] i Index of the shape function (node number).
        !> @param[in] r Normalized coordinates.
        !>
        !> @return psi Value of the shape function.
        !>
        pure elemental module function psi_triangle_second(self, i, r) result(psi)
            implicit none
            class(type_triangle_second), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: psi
        end function psi_triangle_second

        !>
        !> @brief Calculates the derivative of the shape function w.r.t. normalized coordinates (d(psi_i)/dr_j).
        !> @param[in] self Second-order triangular element object.
        !> @param[in] i Index of the shape function (node number).
        !> @param[in] j Index of the coordinate to differentiate w.r.t. (1:xi, 2:eta).
        !> @param[in] r Normalized coordinates.
        !>
        !> @return dpsi Value of the shape function's derivative.
        !>
        pure elemental module function dpsi_triangle_second(self, i, j, r) result(dpsi)
            implicit none
            class(type_triangle_second), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi
        end function dpsi_triangle_second

        !>
        !> @brief Calculates the derivative of the shape function w.r.t. normalized coordinates (d(psi_i)/dr_j).
        !> @param[in] self Second-order triangular element object.
        !> @param[in] i Index of the shape function (node number).
        !> @param[in] j Index of the coordinate to differentiate w.r.t. (1:xi, 2:eta).
        !> @param[in] r Normalized coordinates.
        !>
        !> @return dpsi Value of the shape function's derivative.
        !>
        pure elemental module function dpsi_deta_triangle_second(self, i, j, r) result(dpsi)
            implicit none
            class(type_triangle_second), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi
        end function dpsi_deta_triangle_second

        !>
        !> @brief Calculates the Jacobian matrix for a second-order triangular element.
        !> @param[in] self Second-order triangular element object.
        !> @param[in] r Normalized coordinates.
        !> @param[in] node_coords Nodal coordinate array.
        !> @param[in] connectivity Node numbers that make up the element.
        !>
        !> @return jacobian The Jacobian matrix.
        !>
        pure module function jacobian_triangle_second(self, r, node_coords, connectivity) result(jacobian)
            implicit none
            class(type_triangle_second), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: jacobian(self%get_dimension(), self%get_dimension())
        end function jacobian_triangle_second

        !>
        !> @brief Calculates the Jacobian determinant for a second-order triangular element.
        !> @param[in] self Second-order triangular element object.
        !> @param[in] r Normalized coordinates.
        !> @param[in] node_coords Nodal coordinate array.
        !> @param[in] connectivity Node numbers that make up the element.
        !>
        !> @return jacobian_det The Jacobian determinant.
        !>
        pure module function jacobian_det_triangle_second(self, r, node_coords, connectivity) result(jacobian_det)
            implicit none
            class(type_triangle_second), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: jacobian_det
        end function jacobian_det_triangle_second

        !>
        !> @brief Checks if a given Cartesian coordinate is inside the element.
        !> @param[in] self Second-order triangular element object.
        !> @param[in] cartesian Cartesian coordinate to check.
        !> @param[inout] normalized The corresponding normalized coordinate if inside.
        !> @param[in] node_coords Nodal coordinate array.
        !> @param[in] connectivity Node numbers that make up the element.
        !> @param[inout] is_in The result (true: is inside, false: is not).
        !>
        module subroutine is_in_triangle_second(self, cartesian, normalized, node_coords, connectivity, is_in)
            implicit none
            class(type_triangle_second), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: cartesian
            type(type_dp_vector_3d), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            logical, intent(inout) :: is_in
        end subroutine is_in_triangle_second

    end interface

    !--------------------------------------------------------------------------------------
    !   Square Second Order Element Type procedures interface
    !--------------------------------------------------------------------------------------
    interface
        !>
        !> @brief Constructs an instance of a second-order quadrilateral element.
        !> @param[in] input Input parameters.
        !>
        !> @return fe The created instance of the second-order quadrilateral element.
        !>
        module function construct_square_second(input) result(fe)
            implicit none
            type(type_input), intent(in) :: input
            class(abst_fe), allocatable :: fe
        end function construct_square_second

        !>
        !> @brief Calculates the area of a second-order quadrilateral element.
        !> @param[in] self Second-order quadrilateral element object.
        !> @param[in] node_coords Nodal coordinate array.
        !> @param[in] connectivity Node numbers that make up the element.
        !>
        !> @return area The area of the element.
        !>
        module function get_area_square_second(self, node_coords, connectivity) result(area)
            implicit none
            class(type_square_second), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: area
        end function get_area_square_second

        !>
        !> @brief Calculates the value of the shape function (psi_i) for a second-order quadrilateral element.
        !> @param[in] self Second-order quadrilateral element object.
        !> @param[in] i Index of the shape function (node number).
        !> @param[in] r Normalized coordinates.
        !>
        !> @return psi Value of the shape function.
        !>
        pure elemental module function psi_square_second(self, i, r) result(psi)
            implicit none
            class(type_square_second), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: psi
        end function psi_square_second

        !>
        !> @brief Calculates the derivative of the shape function w.r.t. normalized coordinates (d(psi_i)/dr_j).
        !> @param[in] self Second-order quadrilateral element object.
        !> @param[in] i Index of the shape function (node number).
        !> @param[in] j Index of the coordinate to differentiate w.r.t. (1:xi, 2:eta).
        !> @param[in] r Normalized coordinates.
        !>
        !> @return dpsi Value of the shape function's derivative.
        !>
        pure elemental module function dpsi_square_second(self, i, j, r) result(dpsi)
            implicit none
            class(type_square_second), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_dp_vector_3d), intent(in) :: r
            real(real64) :: dpsi
        end function dpsi_square_second

        !>
        !> @brief Calculates the Jacobian matrix for a second-order quadrilateral element.
        !> @param[in] self Second-order quadrilateral element object.
        !> @param[in] r Normalized coordinates.
        !> @param[in] node_coords Nodal coordinate array.
        !> @param[in] connectivity Node numbers that make up the element.
        !>
        !> @return jacobian The Jacobian matrix.
        !>
        pure module function jacobian_square_second(self, r, node_coords, connectivity) result(jacobian)
            implicit none
            class(type_square_second), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: jacobian(self%get_dimension(), self%get_dimension())
        end function jacobian_square_second

        !>
        !> @brief Calculates the Jacobian determinant for a second-order quadrilateral element.
        !> @param[in] self Second-order quadrilateral element object.
        !> @param[in] r Normalized coordinates.
        !> @param[in] node_coords Nodal coordinate array.
        !> @param[in] connectivity Node numbers that make up the element.
        !>
        !> @return jacobian_det The Jacobian determinant.
        !>
        pure module function jacobian_det_square_second(self, r, node_coords, connectivity) result(jacobian_det)
            implicit none
            class(type_square_second), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            real(real64) :: jacobian_det
        end function jacobian_det_square_second

        !>
        !> @brief Checks if a given Cartesian coordinate is inside the element.
        !> @param[in] self Second-order quadrilateral element object.
        !> @param[in] cartesian Cartesian coordinate to check.
        !> @param[inout] normalized The corresponding normalized coordinate if inside.
        !> @param[in] node_coords Nodal coordinate array.
        !> @param[in] connectivity Node numbers that make up the element.
        !> @param[inout] is_in The result (true: is inside, false: is not).
        !>
        module subroutine is_in_square_second(self, cartesian, normalized, node_coords, connectivity, is_in)
            implicit none
            class(type_square_second), intent(in) :: self
            type(type_dp_vector_3d), intent(in) :: cartesian
            type(type_dp_vector_3d), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            integer(int32), intent(in) :: connectivity(:)
            logical, intent(inout) :: is_in
        end subroutine is_in_square_second
    end interface

end module domain_fe_element

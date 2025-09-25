!>
!> Implements the procedures for the first-order quadrilateral (4-node) finite element.
!>
submodule(domain_fe_element) domain_fe_element_square_first
    implicit none

contains

    !>
    !> Creates and initializes a first-order quadrilateral (4-node) element object.
    !> This function sets up the element properties, including the number of nodes,
    !> dimension, order, and Gauss integration rule based on the input settings.
    !>
    module function construct_square_first(input) result(fe)
        implicit none
        !> The main input data structure containing simulation settings.
        type(type_input), intent(in) :: input
        !> The newly created and allocated first-order quadrilateral element object.
        class(abst_fe), allocatable :: fe

        ! Local variables
        character(len=32), parameter :: cell_name = "Quad"
        integer(int32) :: vtk_type
        integer(int32) :: num_nodes
        integer(int32) :: dimension
        integer(int32) :: order
        integer(int32) :: num_gauss
        real(real64) :: p
        real(real64), allocatable :: weight(:)
        real(real64), allocatable :: gauss(:, :)

        ! Allocate the object with the concrete type
        allocate (type_square_first :: fe)

        ! Get fe properties from the vtk_constants helper using the cell name
        call vtk_constants%get_cell_info_from_cell_name(cell_name, vtk_type, num_nodes, dimension, order)

        ! Prepare Gauss quadrature rule based on input settings
        select case (strip(input%basic%geometry_settings%integration_type))
        case ("full")
            num_gauss = 4
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, 3, num_gauss)

            weight(:) = 1.0d0
            gauss(1:2, 1) = [-sqrt(1.0d0 / 3.0d0), -sqrt(1.0d0 / 3.0d0)]
            gauss(1:2, 2) = [sqrt(1.0d0 / 3.0d0), -sqrt(1.0d0 / 3.0d0)]
            gauss(1:2, 3) = [sqrt(1.0d0 / 3.0d0), sqrt(1.0d0 / 3.0d0)]
            gauss(1:2, 4) = [-sqrt(1.0d0 / 3.0d0), sqrt(1.0d0 / 3.0d0)]
            gauss(3, :) = 0.0d0

        case ("reduced")
            num_gauss = 1
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, 3, num_gauss)

            weight(1) = 4.0d0
            gauss(:, 1) = 0.0d0

        case ("free")
            num_gauss = 4
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, 3, num_gauss)

            p = input%basic%geometry_settings%integration_points
            weight(:) = 1.0d0
            gauss(1:2, 1) = [-p, -p]
            gauss(1:2, 2) = [p, -p]
            gauss(1:2, 3) = [p, p]
            gauss(1:2, 4) = [-p, p]
            gauss(3, :) = 0.0d0
        end select

        ! Initialize the object with properties common to this fe type
        call fe%initialize(type=vtk_type, dimension=dimension, order=order, num_nodes=num_nodes, &
                           num_gauss=num_gauss, weight=weight, gauss=gauss)

        call deallocate_array(weight)
        call deallocate_array(gauss)

    end function construct_square_first

    !>
    !> Computes the area of a specific element instance using Gauss quadrature.
    !> The area is calculated by integrating the Jacobian determinant over the element's
    !> local coordinate domain.
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

        integer(int32) :: i
        type(type_dp_vector_3d), allocatable :: gauss_pts(:)
        real(real64), allocatable :: weights(:)

        area = 0.0d0
        gauss_pts = self%get_gauss()
        weights = self%get_weight()

        ! Area is the integral of |J| over the element domain
        do i = 1, self%get_num_gauss()
            area = area + self%jacobian_det(gauss_pts(i), node_coords, connectivity) * weights(i)
        end do

        deallocate (gauss_pts)
        deallocate (weights)

    end function get_area_square_first

    !>
    !> Evaluates the shape function \( \psi_i \) for a 4-node bilinear quadrilateral element.
    !> For example, \( \psi_1(\xi, \eta) = \frac{1}{4}(1-\xi)(1-\eta) \).
    !>
    pure elemental module function psi_square_first(self, i, r) result(psi)
        implicit none
        !> The first-order quadrilateral element object.
        class(type_square_first), intent(in) :: self
        !> The index of the shape function (1 to 4).
        integer(int32), intent(in) :: i
        !> The local coordinate vector, where \( \xi = r\%x \) and \( \eta = r\%y \).
        type(type_dp_vector_3d), intent(in) :: r
        !> The value of the shape function \( \psi_i(\xi, \eta) \).
        real(real64) :: psi

        select case (i)
        case (1)
            psi = 0.25d0 * (1.0d0 - r%x) * (1.0d0 - r%y)
        case (2)
            psi = 0.25d0 * (1.0d0 + r%x) * (1.0d0 - r%y)
        case (3)
            psi = 0.25d0 * (1.0d0 + r%x) * (1.0d0 + r%y)
        case (4)
            psi = 0.25d0 * (1.0d0 - r%x) * (1.0d0 + r%y)
        case default
            psi = 0.0d0
        end select
    end function psi_square_first

    !>
    !> Evaluates the derivative of the shape function with respect to the local
    !> coordinates, \( \frac{\partial\psi_i}{\partial r_j} \).
    !>
    pure elemental module function dpsi_square_first(self, i, j, r) result(dpsi)
        implicit none
        !> The first-order quadrilateral element object.
        class(type_square_first), intent(in) :: self
        !> The index of the shape function (1 to 4).
        integer(int32), intent(in) :: i
        !> The index of the local coordinate to differentiate with respect to (1 for \( \xi \), 2 for \( \eta \)).
        integer(int32), intent(in) :: j
        !> The local coordinate vector, where \( \xi = r\%x \) and \( \eta = r\%y \).
        type(type_dp_vector_3d), intent(in) :: r
        !> The value of the derivative.
        real(real64) :: dpsi

        dpsi = 0.0d0
        select case (j)
        case (1) ! d/d(xi)
            select case (i)
            case (1)
                dpsi = -0.25d0 * (1.0d0 - r%y)
            case (2)
                dpsi = 0.25d0 * (1.0d0 - r%y)
            case (3)
                dpsi = 0.25d0 * (1.0d0 + r%y)
            case (4)
                dpsi = -0.25d0 * (1.0d0 + r%y)
            end select
        case (2) ! d/d(eta)
            select case (i)
            case (1)
                dpsi = -0.25d0 * (1.0d0 - r%x)
            case (2)
                dpsi = -0.25d0 * (1.0d0 + r%x)
            case (3)
                dpsi = 0.25d0 * (1.0d0 + r%x)
            case (4)
                dpsi = 0.25d0 * (1.0d0 - r%x)
            end select
        end select
    end function dpsi_square_first

    !>
    !> Computes the Jacobian matrix \( J \), which maps derivatives from local
    !> to global coordinates.
    !>
    pure module function jacobian_square_first(self, r, node_coords, connectivity) result(jac)
        implicit none
        !> The first-order quadrilateral element object.
        class(type_square_first), intent(in) :: self
        !> The local coordinate vector where the Jacobian is evaluated.
        type(type_dp_vector_3d), intent(in) :: r
        !> The global coordinates of the mesh nodes.
        real(real64), intent(in) :: node_coords(:, :)
        !> The connectivity array for the element.
        integer(int32), intent(in) :: connectivity(:)
        !> The computed 2x2 Jacobian matrix.
        real(real64) :: jac(self%get_dimension(), self%get_dimension())

        integer(int32) :: i, node_id

        jac = 0.0d0
        do i = 1, self%get_num_nodes()
            node_id = connectivity(i)
            jac(1, 1) = jac(1, 1) + self%dpsi(i, 1, r) * node_coords(1, node_id) ! dx/d_xi
            jac(1, 2) = jac(1, 2) + self%dpsi(i, 2, r) * node_coords(1, node_id) ! dx/d_eta
            jac(2, 1) = jac(2, 1) + self%dpsi(i, 1, r) * node_coords(2, node_id) ! dy/d_xi
            jac(2, 2) = jac(2, 2) + self%dpsi(i, 2, r) * node_coords(2, node_id) ! dy/d_eta
        end do
    end function jacobian_square_first

    !>
    !> Computes the determinant of the Jacobian matrix, \( |J| \).
    !>
    pure module function jacobian_det_square_first(self, r, node_coords, connectivity) result(det_j)
        implicit none
        !> The first-order quadrilateral element object.
        class(type_square_first), intent(in) :: self
        !> The local coordinate vector where the determinant is evaluated.
        type(type_dp_vector_3d), intent(in) :: r
        !> The global coordinates of the mesh nodes.
        real(real64), intent(in) :: node_coords(:, :)
        !> The connectivity array for the element.
        integer(int32), intent(in) :: connectivity(:)
        !> The Jacobian determinant.
        real(real64) :: det_j

        real(real64) :: jac(self%get_dimension(), self%get_dimension())

        jac = self%jacobian(r, node_coords, connectivity)
        det_j = jac(1, 1) * jac(2, 2) - jac(1, 2) * jac(2, 1)
    end function jacobian_det_square_first

    !>
    !> Determines if a point in global coordinates lies inside the element.
    !> This is solved by using the Newton-Raphson method to find the local
    !> coordinates \( (\xi, \eta) \) corresponding to the given global point.
    !>
    module subroutine is_in_square_first(self, cartesian, normalized, node_coords, connectivity, is_in)
        implicit none
        !> The first-order quadrilateral element object.
        class(type_square_first), intent(in) :: self
        !> The point in global (Cartesian) coordinates to check.
        type(type_dp_vector_3d), intent(in) :: cartesian
        !> The resulting local (normalized) coordinate if the point is inside.
        type(type_dp_vector_3d), intent(inout) :: normalized
        !> The global coordinates of the mesh nodes.
        real(real64), intent(in) :: node_coords(:, :)
        !> The connectivity array for the element.
        integer(int32), intent(in) :: connectivity(:)
        !> A logical flag, set to true if the point is inside, false otherwise.
        logical, intent(inout) :: is_in

        type(type_dp_vector_3d) :: r
        type(type_dp_vector_3d) :: interpolated_pos
        real(real64) :: det_j
        real(real64) :: dx
        real(real64) :: dy
        real(real64) :: jac(self%get_dimension(), self%get_dimension())
        real(real64) :: j11, j12, j21, j22
        integer(int32) :: iter
        integer(int32) :: i, node_id
        logical :: converged
        real(real64), parameter :: tol = 1.0e-9
        integer(int32), parameter :: max_iter = 10

        ! Initial guess for local coordinate is the center of the element
        call r%set(0.0d0, 0.0d0, 0.0d0)
        converged = .false.

        ! Newton-Raphson method to find local coordinate 'r' for the given 'cartesian' point
        do iter = 1, max_iter
            call interpolated_pos%set(0.0d0, 0.0d0, 0.0d0)
            do i = 1, self%get_num_nodes()
                node_id = connectivity(i)
                interpolated_pos%x = interpolated_pos%x + self%psi(i, r) * node_coords(1, node_id)
                interpolated_pos%y = interpolated_pos%y + self%psi(i, r) * node_coords(2, node_id)
            end do

            dx = cartesian%x - interpolated_pos%x
            dy = cartesian%y - interpolated_pos%y
            if (sqrt(dx**2 + dy**2) < tol) then
                converged = .true.
                exit
            end if

            det_j = self%jacobian_det(r, node_coords, connectivity)
            if (abs(det_j) < epsilon(det_j)) then
                exit ! Jacobian is singular, cannot continue
            end if

            jac = self%jacobian(r, node_coords, connectivity)

            ! Update local coordinates using the inverse of the Jacobian
            r%x = r%x + (jac(2, 2) * dx - jac(1, 2) * dy) / det_j
            r%y = r%y + (-jac(2, 1) * dx + jac(1, 1) * dy) / det_j
        end do

        ! Check if the converged local coordinate is within the element bounds [-1, 1]
        is_in = converged .and. (abs(r%x) <= 1.0d0 + tol) .and. (abs(r%y) <= 1.0d0 + tol)
        if (is_in) then
            normalized = r
        end if

    end subroutine is_in_square_first

end submodule domain_fe_element_square_first

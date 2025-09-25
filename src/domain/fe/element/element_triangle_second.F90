!>
!> Implements the procedures for the second-order triangular (6-node) finite element.
!>
submodule(domain_fe_element) domain_fe_element_triangle_second
    implicit none
contains

    !>
    !> Creates and initializes a second-order triangular (6-node) element object.
    !> This function sets up the element properties, including the number of nodes,
    !> dimension, order, and a 3-point Gauss integration rule suitable for
    !> quadratic triangles.
    !>
    module function construct_triangle_second(input) result(fe)
        implicit none
        !> The main input data structure (unused, for API consistency).
        type(type_input), intent(in) :: input
        !> The newly created and allocated second-order triangular element object.
        class(abst_fe), allocatable :: fe

        character(len=32), parameter :: cell_name = "QuadraticTriangle"
        integer(int32) :: vtk_type
        integer(int32) :: num_nodes
        integer(int32) :: dimension
        integer(int32) :: order
        integer(int32) :: num_gauss
        real(real64), allocatable :: weight(:)
        real(real64), allocatable :: gauss(:, :)

        allocate (type_triangle_second :: fe)

        call vtk_constants%get_cell_info_from_cell_name(cell_name, vtk_type, num_nodes, dimension, order)

        ! For quadratic triangles, a 3-point rule is standard.
        num_gauss = 3
        allocate (weight(num_gauss))
        allocate (gauss(3, num_gauss))

        weight(:) = 1.0d0 / 6.0d0
        gauss(1:2, 1) = [1.0d0 / 6.0d0, 1.0d0 / 6.0d0]
        gauss(1:2, 2) = [2.0d0 / 3.0d0, 1.0d0 / 6.0d0]
        gauss(1:2, 3) = [1.0d0 / 6.0d0, 2.0d0 / 3.0d0]
        gauss(3, :) = 0.0d0

        call fe%initialize(type=vtk_type, dimension=dimension, order=order, num_nodes=num_nodes, &
                           num_gauss=num_gauss, weight=weight, gauss=gauss)

        deallocate (weight, gauss)

    end function construct_triangle_second

    !>
    !> Computes the area of a specific element instance using Gauss quadrature.
    !> The area is calculated by integrating the Jacobian determinant over the element's
    !> local coordinate domain.
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

        integer(int32) :: i
        type(type_dp_vector_3d), allocatable :: gauss_pts(:)
        real(real64), allocatable :: weights(:)

        area = 0.0d0
        gauss_pts = self%get_gauss()
        weights = self%get_weight()

        do i = 1, self%get_num_gauss()
            area = area + self%jacobian_det(gauss_pts(i), node_coords, connectivity) * weights(i)
        end do

    end function get_area_triangle_second

    !>
    !> Evaluates the shape function \( \psi_i \) for a 6-node quadratic triangle.
    !> The functions are defined in terms of barycentric coordinates \( \xi, \eta \),
    !> and \( \zeta = 1 - \xi - \eta \).
    !>
    pure elemental module function psi_triangle_second(self, i, r) result(psi)
        implicit none
        !> The second-order triangular element object.
        class(type_triangle_second), intent(in) :: self
        !> The index of the shape function (1 to 6).
        integer(int32), intent(in) :: i
        !> The local coordinate vector, where \( \xi = r\%x \) and \( \eta = r\%y \).
        type(type_dp_vector_3d), intent(in) :: r
        !> The value of the shape function \( \psi_i(\xi, \eta) \).
        real(real64) :: psi
        real(real64) :: xi
        real(real64) :: eta
        real(real64) :: zeta

        xi = r%x
        eta = r%y
        zeta = 1.0d0 - xi - eta

        select case (i)
        case (1) ! Corner nodes
            psi = xi * (2.0d0 * xi - 1.0d0)
        case (2)
            psi = eta * (2.0d0 * eta - 1.0d0)
        case (3)
            psi = zeta * (2.0d0 * zeta - 1.0d0)
        case (4) ! Midside nodes
            psi = 4.0d0 * xi * eta
        case (5)
            psi = 4.0d0 * eta * zeta
        case (6)
            psi = 4.0d0 * zeta * xi
        case default
            psi = 0.0d0
        end select
    end function psi_triangle_second

    !>
    !> Evaluates the derivative of the shape function with respect to the local
    !> coordinates, \( \frac{\partial\psi_i}{\partial r_j} \).
    !>
    pure elemental module function dpsi_triangle_second(self, i, j, r) result(dpsi)
        implicit none
        !> The second-order triangular element object.
        class(type_triangle_second), intent(in) :: self
        !> The index of the shape function (1 to 6).
        integer(int32), intent(in) :: i
        !> The index of the local coordinate to differentiate with respect to (1 for \( \xi \), 2 for \( \eta \)).
        integer(int32), intent(in) :: j
        !> The local coordinate vector, where \( \xi = r\%x \) and \( \eta = r\%y \).
        type(type_dp_vector_3d), intent(in) :: r
        !> The value of the derivative.
        real(real64) :: dpsi
        real(real64) :: xi
        real(real64) :: eta

        xi = r%x
        eta = r%y
        dpsi = 0.0d0

        select case (j)
        case (1) ! d/d(xi)
            select case (i)
            case (1)
                dpsi = 4.0d0 * xi - 1.0d0
            case (2)
                dpsi = 0.0d0
            case (3)
                dpsi = 4.0d0 * xi + 4.0d0 * eta - 3.0d0
            case (4)
                dpsi = 4.0d0 * eta
            case (5)
                dpsi = -4.0d0 * eta
            case (6)
                dpsi = 4.0d0 - 8.0d0 * xi - 4.0d0 * eta
            end select
        case (2) ! d/d(eta)
            select case (i)
            case (1)
                dpsi = 0.0d0
            case (2)
                dpsi = 4.0d0 * eta - 1.0d0
            case (3)
                dpsi = 4.0d0 * xi + 4.0d0 * eta - 3.0d0
            case (4)
                dpsi = 4.0d0 * xi
            case (5)
                dpsi = 4.0d0 - 4.0d0 * xi - 8.0d0 * eta
            case (6)
                dpsi = -4.0d0 * xi
            end select
        end select
    end function dpsi_triangle_second

    !>
    !> Computes the Jacobian matrix \( J \), which maps derivatives from local
    !> to global coordinates.
    !>
    pure module function jacobian_triangle_second(self, r, node_coords, connectivity) result(jac)
        implicit none
        !> The second-order triangular element object.
        class(type_triangle_second), intent(in) :: self
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
            jac(1, 1) = jac(1, 1) + self%dpsi(i, 1, r) * node_coords(1, node_id)
            jac(1, 2) = jac(1, 2) + self%dpsi(i, 2, r) * node_coords(1, node_id)
            jac(2, 1) = jac(2, 1) + self%dpsi(i, 1, r) * node_coords(2, node_id)
            jac(2, 2) = jac(2, 2) + self%dpsi(i, 2, r) * node_coords(2, node_id)
        end do
    end function jacobian_triangle_second

    !>
    !> Computes the determinant of the Jacobian matrix, \( |J| \).
    !>
    pure module function jacobian_det_triangle_second(self, r, node_coords, connectivity) result(det_j)
        implicit none
        !> The second-order triangular element object.
        class(type_triangle_second), intent(in) :: self
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
    end function jacobian_det_triangle_second

    !>
    !> Determines if a point in global coordinates lies inside the element.
    !> This is solved by using the Newton-Raphson method to find the local
    !> (barycentric) coordinates corresponding to the given global point.
    !>
    module subroutine is_in_triangle_second(self, cartesian, normalized, node_coords, connectivity, is_in)
        implicit none
        !> The second-order triangular element object.
        class(type_triangle_second), intent(in) :: self
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
        integer(int32) :: iter
        integer(int32) :: i, node_id
        logical :: converged
        real(real64), parameter :: tol = 1.0e-9
        integer(int32), parameter :: max_iter = 10

        ! Initial guess for local coordinate is the centroid of the element
        call r%set(1.0d0 / 3.0d0, 1.0d0 / 3.0d0, 0.0d0)
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

        ! Check if the converged local coordinate is within the element bounds
        is_in = converged .and. (r%x >= -tol) .and. (r%y >= -tol) .and. (r%x + r%y <= 1.0d0 + tol)
        if (is_in) then
            normalized = r
        end if

    end subroutine is_in_triangle_second

end submodule domain_fe_element_triangle_second

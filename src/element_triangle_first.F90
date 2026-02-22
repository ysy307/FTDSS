!>
!> Implements the procedures for the first-order triangular (3-node) finite element.
!> Corrected Jacobian definition.
!>
submodule(domain_fe_element) domain_fe_element_triangle_first
    implicit none

contains

    !>
    !> Creates and initializes a first-order triangular (3-node) element object.
    !>
    module function construct_triangle_first(integration_order) result(fe)
        implicit none
        !> The integration order for the element.
        integer(int32), intent(in) :: integration_order
        !> The newly created element object.
        class(abst_fe), allocatable :: fe

        character(len=*), parameter :: cell_name = "Triangle"
        integer(int32) :: vtk_type
        integer(int32) :: num_nodes
        integer(int32) :: dimension
        integer(int32) :: order
        integer(int32) :: num_gauss

        allocate (type_triangle_first :: fe)

        call vtk_constants%get_cell_info_from_cell_name(cell_name, vtk_type, num_nodes, dimension, order)

        call fe%initialize(type=vtk_type, dimension=dimension, order=order, num_nodes=num_nodes, &
                           integration_order=integration_order)

    end function construct_triangle_first

    !>
    !> Calculates the area of the element.
    !>
    module subroutine calc_area_triangle_first(self, node_coords, measure)
        implicit none
        class(type_triangle_first), intent(in) :: self
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: measure

        real(real64) :: x(3)
        real(real64) :: y(3)
        integer(int32) :: i

        do i = 1, 3
            x(i) = node_coords(1, i)
            y(i) = node_coords(2, i)
        end do

        ! Area = 0.5 * |x1(y2-y3) + x2(y3-y1) + x3(y1-y2)|
        measure = 0.5d0 * abs(x(1) * (y(2) - y(3)) + x(2) * (y(3) - y(1)) + x(3) * (y(1) - y(2)))
    end subroutine calc_area_triangle_first

    !>
    !> Evaluates the shape function psi.
    !>
    pure elemental module subroutine calc_psi_triangle_first(self, i, r, psi_val)
        implicit none
        class(type_triangle_first), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: psi_val

        select case (i)
        case (1)
            psi_val = 1.0d0 - r%x - r%y
        case (2)
            psi_val = r%x
        case (3)
            psi_val = r%y
        case default
            psi_val = 0.0d0
        end select
    end subroutine calc_psi_triangle_first

    !>
    !> Evaluates the derivative of the shape function dpsi.
    !>
    pure elemental module subroutine calc_dpsi_triangle_first(self, i, j, r, dpsi_val)
        implicit none
        class(type_triangle_first), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: dpsi_val

        dpsi_val = 0.0d0
        select case (i)
        case (1)
            if (j == 1) dpsi_val = -1.0d0
            if (j == 2) dpsi_val = -1.0d0
        case (2)
            if (j == 1) dpsi_val = 1.0d0
        case (3)
            if (j == 2) dpsi_val = 1.0d0
        end select
    end subroutine calc_dpsi_triangle_first

    !>
    !> Calculates the Jacobian matrix.
    !> Standard definition: J(i, j) = d(x_i) / d(xi_j)
    !>
    pure module subroutine calc_jacobian_triangle_first(self, r, node_coords, jac)
        implicit none
        class(type_triangle_first), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: jac(:, :)

        real(real64) :: x(3)
        real(real64) :: y(3)
        integer(int32) :: k

        do k = 1, 3
            x(k) = node_coords(1, k)
            y(k) = node_coords(2, k)
        end do

        jac = 0.0d0
        ! Row 1: dx/dxi, dx/deta
        jac(1, 1) = x(2) - x(1)
        jac(1, 2) = x(3) - x(1) ! Fixed: Was y(2)-y(1) in previous/implicit logic

        ! Row 2: dy/dxi, dy/deta
        jac(2, 1) = y(2) - y(1) ! Fixed: Was x(3)-x(1) in previous/implicit logic
        jac(2, 2) = y(3) - y(1)
    end subroutine calc_jacobian_triangle_first

    !>
    !> Checks if point is inside using analytical Barycentric coordinates.
    !>
    module subroutine is_in_triangle_first(self, cartesian, normalized, node_coords, is_in)
        implicit none
        class(type_triangle_first), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: cartesian
        type(type_coordinate_dp), intent(inout) :: normalized
        real(real64), intent(in) :: node_coords(:, :)
        logical, intent(inout) :: is_in

        real(real64) :: x(3)
        real(real64) :: y(3)
        real(real64) :: det_T
        real(real64) :: dx
        real(real64) :: dy
        real(real64) :: xi
        real(real64) :: eta
        integer(int32) :: i
        real(real64), parameter :: tol = 1.0e-9

        do i = 1, 3
            x(i) = node_coords(1, i)
            y(i) = node_coords(2, i)
        end do

        det_T = (x(2) - x(1)) * (y(3) - y(1)) - (x(3) - x(1)) * (y(2) - y(1))

        if (abs(det_T) < epsilon(det_T)) then
            is_in = .false.
            return
        end if

        dx = cartesian%x - x(1)
        dy = cartesian%y - y(1)

        ! Solving linear system for barycentric coords
        xi = ((y(3) - y(1)) * dx - (x(3) - x(1)) * dy) / det_T
        eta = (-(y(2) - y(1)) * dx + (x(2) - x(1)) * dy) / det_T

        is_in = (xi >= -tol) .and. (eta >= -tol) .and. (xi + eta <= 1.0d0 + tol)

        if (is_in) then
            call normalized%set(xi, eta, 0.0d0)
        end if
    end subroutine is_in_triangle_first

end submodule domain_fe_element_triangle_first

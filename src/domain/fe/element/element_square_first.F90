!>
!> Implements the procedures for the first-order quadrilateral (4-node) finite element.
!> Corrected Jacobian definition.
!>
submodule(domain_fe_element) domain_fe_element_square_first
    implicit none

contains

    !>
    !> Creates and initializes a first-order quadrilateral (4-node) element object.
    !>
    module function construct_square_first(integration_order) result(fe)
        implicit none
        !> The integration order for the element.
        integer(int32), intent(in) :: integration_order
        !> The newly created element object.
        class(abst_fe), allocatable :: fe

        character(len=*), parameter :: cell_name = "Quad"
        integer(int32) :: vtk_type
        integer(int32) :: num_nodes
        integer(int32) :: dimension
        integer(int32) :: order

        allocate (type_square_first :: fe)

        call vtk_constants%get_cell_info_from_cell_name(cell_name, vtk_type, num_nodes, dimension, order)

        call fe%initialize(type=vtk_type, dimension=dimension, order=order, num_nodes=num_nodes, &
                           integration_order=integration_order)

    end function construct_square_first

    module subroutine calc_area_square_first(self, node_coords, measure)
        implicit none
        class(type_square_first), intent(in) :: self
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: measure

        integer(int32) :: i
        integer(int32) :: ng
        real(real64) :: det_j
        type(type_coordinate_dp) :: r
        type(type_coordinate_dp), pointer, contiguous, dimension(:) :: gauss_pts
        real(real64), pointer, contiguous, dimension(:) :: weights

        measure = 0.0d0
        call self%get_gauss(gauss_pts)
        call self%get_weight(weights)
        call self%get_num_gauss(ng)

        do i = 1, ng
            r = gauss_pts(i)
            call self%calc_jacobian_determinant(r, node_coords, det_j)
            measure = measure + abs(det_j) * weights(i)
        end do

    end subroutine calc_area_square_first

    pure elemental module subroutine calc_psi_square_first(self, i, r, psi_val)
        implicit none
        class(type_square_first), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: psi_val

        select case (i)
        case (1)
            psi_val = 0.25d0 * (1.0d0 - r%x) * (1.0d0 - r%y)
        case (2)
            psi_val = 0.25d0 * (1.0d0 + r%x) * (1.0d0 - r%y)
        case (3)
            psi_val = 0.25d0 * (1.0d0 + r%x) * (1.0d0 + r%y)
        case (4)
            psi_val = 0.25d0 * (1.0d0 - r%x) * (1.0d0 + r%y)
        case default
            psi_val = 0.0d0
        end select
    end subroutine calc_psi_square_first

    pure elemental module subroutine calc_dpsi_square_first(self, i, j, r, dpsi_val)
        implicit none
        class(type_square_first), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: dpsi_val

        dpsi_val = 0.0d0
        select case (j)
        case (1) ! d/dxi
            select case (i)
            case (1)
                dpsi_val = -0.25d0 * (1.0d0 - r%y)
            case (2)
                dpsi_val = 0.25d0 * (1.0d0 - r%y)
            case (3)
                dpsi_val = 0.25d0 * (1.0d0 + r%y)
            case (4)
                dpsi_val = -0.25d0 * (1.0d0 + r%y)
            end select
        case (2) ! d/deta
            select case (i)
            case (1)
                dpsi_val = -0.25d0 * (1.0d0 - r%x)
            case (2)
                dpsi_val = -0.25d0 * (1.0d0 + r%x)
            case (3)
                dpsi_val = 0.25d0 * (1.0d0 + r%x)
            case (4)
                dpsi_val = 0.25d0 * (1.0d0 - r%x)
            end select
        end select
    end subroutine calc_dpsi_square_first

    !>
    !> Calculates the Jacobian matrix.
    !> Standard definition: J(i, j) = d(x_i) / d(xi_j)
    !>
    pure module subroutine calc_jacobian_square_first(self, r, node_coords, jac)
        implicit none
        class(type_square_first), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: jac(:, :)

        integer(int32) :: k
        real(real64) :: dpsi_xi
        real(real64) :: dpsi_eta
        real(real64) :: xk
        real(real64) :: yk

        jac = 0.0d0
        do k = 1, 4
            xk = node_coords(1, k)
            yk = node_coords(2, k)

            call self%calc_dpsi(k, 1, r, dpsi_xi)
            call self%calc_dpsi(k, 2, r, dpsi_eta)

            ! Row 1: x derivatives
            jac(1, 1) = jac(1, 1) + dpsi_xi * xk ! dx/dxi
            jac(1, 2) = jac(1, 2) + dpsi_eta * xk ! dx/deta (was yk*dpsi_xi)

            ! Row 2: y derivatives
            jac(2, 1) = jac(2, 1) + dpsi_xi * yk ! dy/dxi (was xk*dpsi_eta)
            jac(2, 2) = jac(2, 2) + dpsi_eta * yk ! dy/deta
        end do
    end subroutine calc_jacobian_square_first

    module subroutine is_in_square_first(self, cartesian, normalized, node_coords, is_in)
        implicit none
        class(type_square_first), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: cartesian
        type(type_coordinate_dp), intent(inout) :: normalized
        real(real64), intent(in) :: node_coords(:, :)
        logical, intent(inout) :: is_in

        type(type_coordinate_dp) :: r
        type(type_coordinate_dp) :: interpolated_pos
        real(real64) :: det_j
        real(real64) :: dx
        real(real64) :: dy
        real(real64) :: jac(2, 2)
        real(real64) :: psi_val
        integer(int32) :: iter
        integer(int32) :: i
        integer(int32) :: nn
        logical :: converged
        real(real64), parameter :: tol = 1.0e-9
        integer(int32), parameter :: max_iter = 10

        call r%set(0.0d0, 0.0d0, 0.0d0)
        call self%get_num_nodes(nn)
        converged = .false.

        do iter = 1, max_iter
            call interpolated_pos%set(0.0d0, 0.0d0, 0.0d0)
            do i = 1, nn
                call self%calc_psi(i, r, psi_val)
                interpolated_pos%x = interpolated_pos%x + psi_val * node_coords(1, i)
                interpolated_pos%y = interpolated_pos%y + psi_val * node_coords(2, i)
            end do

            dx = cartesian%x - interpolated_pos%x
            dy = cartesian%y - interpolated_pos%y
            if (sqrt(dx**2 + dy**2) < tol) then
                converged = .true.
                exit
            end if

            call self%calc_jacobian_determinant(r, node_coords, det_j)
            if (abs(det_j) < epsilon(det_j)) exit

            call self%calc_jacobian(r, node_coords, jac)

            ! Inverse Jacobian Update:
            ! dxi = (J22*dx - J12*dy) / det
            ! deta = (-J21*dx + J11*dy) / det
            r%x = r%x + (jac(2, 2) * dx - jac(1, 2) * dy) / det_j
            r%y = r%y + (-jac(2, 1) * dx + jac(1, 1) * dy) / det_j
        end do

        is_in = converged .and. (abs(r%x) <= 1.0d0 + tol) .and. (abs(r%y) <= 1.0d0 + tol)
        if (is_in) normalized = r
    end subroutine is_in_square_first

end submodule domain_fe_element_square_first

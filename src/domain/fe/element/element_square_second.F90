!>
!> Implements the procedures for the second-order quadrilateral (8-node) finite element.
!> Corrected Jacobian definition and Newton-Raphson inverse logic.
!>
submodule(domain_fe_element) domain_fe_element_square_second
    implicit none

contains

    module function construct_square_second(integration_order) result(fe)
        implicit none
        !> The integration order for the element.
        integer(int32), intent(in) :: integration_order
        class(abst_fe), allocatable :: fe

        character(len=*), parameter :: cell_name = "QuadraticQuad"
        integer(int32) :: vtk_type
        integer(int32) :: num_nodes
        integer(int32) :: dimension
        integer(int32) :: order
        integer(int32) :: num_gauss

        allocate (type_square_second :: fe)

        call vtk_constants%get_cell_info_from_cell_name(cell_name, vtk_type, num_nodes, dimension, order)

        call fe%initialize(type=vtk_type, dimension=dimension, order=order, num_nodes=num_nodes, &
                           integration_order=integration_order)

    end function construct_square_second

    module subroutine calc_area_square_second(self, node_coords, measure)
        implicit none
        class(type_square_second), intent(in) :: self
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: measure

        integer(int32) :: i
        integer(int32) :: ng
        type(type_coordinate_dp), pointer, contiguous, dimension(:) :: gauss_pts
        real(real64), pointer, contiguous, dimension(:) :: weights
        real(real64) :: det_j

        measure = 0.0d0
        call self%get_gauss(gauss_pts)
        call self%get_weight(weights)
        call self%get_num_gauss(ng)

        do i = 1, ng
            call self%calc_jacobian_determinant(gauss_pts(i), node_coords, det_j)
            measure = measure + abs(det_j) * weights(i)
        end do

    end subroutine calc_area_square_second

    pure elemental module subroutine calc_psi_square_second(self, i, r, psi_val)
        implicit none
        class(type_square_second), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: psi_val
        real(real64) :: xi
        real(real64) :: eta

        xi = r%x
        eta = r%y

        ! Serendipity element shape functions
        select case (i)
        case (1)
            psi_val = 0.25d0 * (1.0d0 - xi) * (1.0d0 - eta) * (-1.0d0 - xi - eta)
        case (2)
            psi_val = 0.25d0 * (1.0d0 + xi) * (1.0d0 - eta) * (-1.0d0 + xi - eta)
        case (3)
            psi_val = 0.25d0 * (1.0d0 + xi) * (1.0d0 + eta) * (-1.0d0 + xi + eta)
        case (4)
            psi_val = 0.25d0 * (1.0d0 - xi) * (1.0d0 + eta) * (-1.0d0 - xi + eta)
        case (5)
            psi_val = 0.5d0 * (1.0d0 - xi**2) * (1.0d0 - eta)
        case (6)
            psi_val = 0.5d0 * (1.0d0 + xi) * (1.0d0 - eta**2)
        case (7)
            psi_val = 0.5d0 * (1.0d0 - xi**2) * (1.0d0 + eta)
        case (8)
            psi_val = 0.5d0 * (1.0d0 - xi) * (1.0d0 - eta**2)
        case default
            psi_val = 0.0d0
        end select
    end subroutine calc_psi_square_second

    pure elemental module subroutine calc_dpsi_square_second(self, i, j, r, dpsi_val)
        implicit none
        class(type_square_second), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: dpsi_val
        real(real64) :: xi
        real(real64) :: eta

        xi = r%x
        eta = r%y
        dpsi_val = 0.0d0

        if (j == 1) then ! d/dxi
            select case (i)
            case (1)
                dpsi_val = 0.25d0 * (1.0d0 - eta) * (2.0d0 * xi + eta)
            case (2)
                dpsi_val = 0.25d0 * (1.0d0 - eta) * (2.0d0 * xi - eta)
            case (3)
                dpsi_val = 0.25d0 * (1.0d0 + eta) * (2.0d0 * xi + eta)
            case (4)
                dpsi_val = 0.25d0 * (1.0d0 + eta) * (2.0d0 * xi - eta)
            case (5)
                dpsi_val = -xi * (1.0d0 - eta)
            case (6)
                dpsi_val = 0.5d0 * (1.0d0 - eta**2)
            case (7)
                dpsi_val = -xi * (1.0d0 + eta)
            case (8)
                dpsi_val = -0.5d0 * (1.0d0 - eta**2)
            end select
        else if (j == 2) then ! d/deta
            select case (i)
            case (1)
                dpsi_val = 0.25d0 * (1.0d0 - xi) * (xi + 2.0d0 * eta)
            case (2)
                dpsi_val = 0.25d0 * (1.0d0 + xi) * (-xi + 2.0d0 * eta)
            case (3)
                dpsi_val = 0.25d0 * (1.0d0 + xi) * (xi + 2.0d0 * eta)
            case (4)
                dpsi_val = 0.25d0 * (1.0d0 - xi) * (-xi + 2.0d0 * eta)
            case (5)
                dpsi_val = -0.5d0 * (1.0d0 - xi**2)
            case (6)
                dpsi_val = -eta * (1.0d0 + xi)
            case (7)
                dpsi_val = 0.5d0 * (1.0d0 - xi**2)
            case (8)
                dpsi_val = -eta * (1.0d0 - xi)
            end select
        end if
    end subroutine calc_dpsi_square_second

    !>
    !> Calculates the Jacobian matrix.
    !> Standard definition: J(i, j) = d(x_i) / d(xi_j)
    !> Row 1: dx/dxi, dx/deta
    !> Row 2: dy/dxi, dy/deta
    !>
    pure module subroutine calc_jacobian_square_second(self, r, node_coords, jac)
        implicit none
        class(type_square_second), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: jac(:, :)

        integer(int32) :: k
        real(real64) :: dpsi_xi
        real(real64) :: dpsi_eta
        real(real64) :: xk, yk

        jac = 0.0d0
        do k = 1, 8
            call self%calc_dpsi(k, 1, r, dpsi_xi)
            call self%calc_dpsi(k, 2, r, dpsi_eta)
            xk = node_coords(1, k)
            yk = node_coords(2, k)

            ! dpsi_xi contributes to derivatives w.r.t xi (Col 1)
            jac(1, 1) = jac(1, 1) + dpsi_xi * xk ! dx/dxi
            jac(2, 1) = jac(2, 1) + dpsi_xi * yk ! dy/dxi (was mixed in original)

            ! dpsi_eta contributes to derivatives w.r.t eta (Col 2)
            jac(1, 2) = jac(1, 2) + dpsi_eta * xk ! dx/deta (was mixed in original)
            jac(2, 2) = jac(2, 2) + dpsi_eta * yk ! dy/deta
        end do
    end subroutine calc_jacobian_square_second

    module subroutine is_in_square_second(self, cartesian, normalized, node_coords, is_in)
        implicit none
        class(type_square_second), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: cartesian
        type(type_coordinate_dp), intent(inout) :: normalized
        real(real64), intent(in) :: node_coords(:, :)
        logical, intent(inout) :: is_in

        type(type_coordinate_dp) :: r
        type(type_coordinate_dp) :: pos
        real(real64) :: det_j
        real(real64) :: dx
        real(real64) :: dy
        real(real64) :: jac(2, 2)
        integer(int32) :: iter
        integer(int32) :: i
        integer(int32) :: nn
        logical :: converged

        real(real64), parameter :: tol = 1.0e-5
        real(real64), parameter :: bbox_margin = 1.0e-3
        integer(int32), parameter :: max_iter = 100
        real(real64) :: psi_val
        real(real64) :: inv_det
        real(real64) :: dr_x, dr_y
        real(real64) :: min_coord(2), max_coord(2)
        real(real64) :: init_guesses(9, 2)
        integer(int32) :: ig

        ! 1. Bounding box check
        min_coord(1) = minval(node_coords(1, :))
        max_coord(1) = maxval(node_coords(1, :))
        min_coord(2) = minval(node_coords(2, :))
        max_coord(2) = maxval(node_coords(2, :))

        if (cartesian%x < min_coord(1) - bbox_margin .or. cartesian%x > max_coord(1) + bbox_margin .or. &
            cartesian%y < min_coord(2) - bbox_margin .or. cartesian%y > max_coord(2) + bbox_margin) then
            is_in = .false.
            return
        end if

        call self%get_num_nodes(nn)
        is_in = .false.

        ! 2. Newton-Raphson with multiple initial guesses
        init_guesses(1, :) = [0.0d0, 0.0d0]
        init_guesses(2, :) = [0.9d0, 0.9d0]
        init_guesses(3, :) = [-0.9d0, 0.9d0]
        init_guesses(4, :) = [-0.9d0, -0.9d0]
        init_guesses(5, :) = [0.9d0, -0.9d0]
        init_guesses(6, :) = [0.9d0, 0.0d0]
        init_guesses(7, :) = [0.0d0, 0.9d0]
        init_guesses(8, :) = [-0.9d0, 0.0d0]
        init_guesses(9, :) = [0.0d0, -0.9d0]

        guess_loop: do ig = 1, 9
            call r%set(init_guesses(ig, 1), init_guesses(ig, 2), 0.0d0)
            converged = .false.

            newton_loop: do iter = 1, max_iter
                call pos%set(0.0d0, 0.0d0, 0.0d0)
                do i = 1, nn
                    call self%calc_psi(i, r, psi_val)
                    pos%x = pos%x + psi_val * node_coords(1, i)
                    pos%y = pos%y + psi_val * node_coords(2, i)
                end do

                dx = cartesian%x - pos%x
                dy = cartesian%y - pos%y

                ! Convergence check
                if (sqrt(dx**2 + dy**2) < 1.0e-9) then
                    if (abs(r%x) <= 1.0d0 + 1.0e-4 .and. abs(r%y) <= 1.0d0 + 1.0e-4) then
                        is_in = .true.
                        normalized = r
                        return
                    end if
                    exit newton_loop
                end if

                call self%calc_jacobian_determinant(r, node_coords, det_j)
                if (abs(det_j) < 1.0e-12) exit newton_loop

                call self%calc_jacobian(r, node_coords, jac)
                inv_det = 1.0d0 / det_j

                ! Calculate inverse Jacobian * residual
                ! [dxi ]   1   [ J22  -J12 ] [ dx ]
                ! [deta] = det [ -J21  J11 ] [ dy ]
                ! Note: J12 is jac(1,2), J21 is jac(2,1)

                dr_x = (jac(2, 2) * dx - jac(1, 2) * dy) * inv_det
                dr_y = (-jac(2, 1) * dx + jac(1, 1) * dy) * inv_det

                r%x = r%x + dr_x
                r%y = r%y + dr_y

                ! Divergence check
                if (abs(r%x) > 3.0d0 .or. abs(r%y) > 3.0d0) exit newton_loop
            end do newton_loop

            ! Check if residual is small enough even if max_iter reached
            if (.not. is_in .and. sqrt(dx**2 + dy**2) < tol) then
                if (abs(r%x) <= 1.0d0 + 1.0e-4 .and. abs(r%y) <= 1.0d0 + 1.0e-4) then
                    is_in = .true.
                    normalized = r
                    return
                end if
            end if
        end do guess_loop

    end subroutine is_in_square_second

end submodule domain_fe_element_square_second

!>
!> Implements the procedures for the second-order quadrilateral (8-node) finite element.
!>
submodule(domain_fe_element) domain_fe_element_square_second
    implicit none

contains

    module function construct_square_second(input) result(fe)
        implicit none
        type(type_input), intent(in) :: input
        class(abst_fe), allocatable :: fe

        character(len=*), parameter :: cell_name = "QuadraticQuad"
        integer(int32) :: vtk_type
        integer(int32) :: num_nodes
        integer(int32) :: dimension
        integer(int32) :: order
        integer(int32) :: num_gauss
        integer(int32) :: integration_order

        allocate (type_square_second :: fe)

        call vtk_constants%get_cell_info_from_cell_name(cell_name, vtk_type, num_nodes, dimension, order)

        select case (strip(input%basic%geometry_settings%integration_type))
        case ("full")
            integration_order = 3
        case ("reduced")
            integration_order = 2
        case default
            integration_order = 3
        end select

        call fe%initialize(type=vtk_type, dimension=dimension, order=order, num_nodes=num_nodes, &
                           integration_order=integration_order)

    end function construct_square_second

    module subroutine get_area_square_second(self, node_coords, geometry)
        implicit none
        class(type_square_second), intent(in) :: self
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: geometry

        integer(int32) :: i
        integer(int32) :: ng
        type(type_coordinate_dp), pointer, contiguous, dimension(:) :: gauss_pts
        real(real64), pointer, contiguous, dimension(:) :: weights
        real(real64) :: det_j

        geometry = 0.0d0
        call self%get_gauss(gauss_pts)
        call self%get_weight(weights)
        call self%get_num_gauss(ng)

        do i = 1, ng
            call self%jacobian_det(gauss_pts(i), node_coords, det_j)
            geometry = geometry + det_j * weights(i)
        end do

    end subroutine get_area_square_second

    pure elemental module subroutine psi_square_second(self, i, r, psi_val)
        implicit none
        class(type_square_second), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: psi_val
        real(real64) :: xi
        real(real64) :: eta

        xi = r%x
        eta = r%y

        select case (i)
        case (1); psi_val = 0.25d0 * (1.0d0 - xi) * (1.0d0 - eta) * (-1.0d0 - xi - eta)
        case (2); psi_val = 0.25d0 * (1.0d0 + xi) * (1.0d0 - eta) * (-1.0d0 + xi - eta)
        case (3); psi_val = 0.25d0 * (1.0d0 + xi) * (1.0d0 + eta) * (-1.0d0 + xi + eta)
        case (4); psi_val = 0.25d0 * (1.0d0 - xi) * (1.0d0 + eta) * (-1.0d0 - xi + eta)
        case (5); psi_val = 0.5d0 * (1.0d0 - xi**2) * (1.0d0 - eta)
        case (6); psi_val = 0.5d0 * (1.0d0 + xi) * (1.0d0 - eta**2)
        case (7); psi_val = 0.5d0 * (1.0d0 - xi**2) * (1.0d0 + eta)
        case (8); psi_val = 0.5d0 * (1.0d0 - xi) * (1.0d0 - eta**2)
        case default; psi_val = 0.0d0
        end select
    end subroutine psi_square_second

    pure elemental module subroutine dpsi_square_second(self, i, j, r, dpsi_val)
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
            case (1); dpsi_val = 0.25d0 * (1.0d0 - eta) * (2.0d0 * xi + eta)
            case (2); dpsi_val = 0.25d0 * (1.0d0 - eta) * (2.0d0 * xi - eta)
            case (3); dpsi_val = 0.25d0 * (1.0d0 + eta) * (2.0d0 * xi + eta)
            case (4); dpsi_val = 0.25d0 * (1.0d0 + eta) * (2.0d0 * xi - eta)
            case (5); dpsi_val = -xi * (1.0d0 - eta)
            case (6); dpsi_val = 0.5d0 * (1.0d0 - eta**2)
            case (7); dpsi_val = -xi * (1.0d0 + eta)
            case (8); dpsi_val = -0.5d0 * (1.0d0 - eta**2)
            end select
        else if (j == 2) then ! d/deta
            select case (i)
            case (1); dpsi_val = 0.25d0 * (1.0d0 - xi) * (xi + 2.0d0 * eta)
            case (2); dpsi_val = 0.25d0 * (1.0d0 + xi) * (-xi + 2.0d0 * eta)
            case (3); dpsi_val = 0.25d0 * (1.0d0 + xi) * (xi + 2.0d0 * eta)
            case (4); dpsi_val = 0.25d0 * (1.0d0 - xi) * (-xi + 2.0d0 * eta)
            case (5); dpsi_val = -0.5d0 * (1.0d0 - xi**2)
            case (6); dpsi_val = -eta * (1.0d0 + xi)
            case (7); dpsi_val = 0.5d0 * (1.0d0 - xi**2)
            case (8); dpsi_val = -eta * (1.0d0 - xi)
            end select
        end if
    end subroutine dpsi_square_second

    pure module subroutine jacobian_square_second(self, r, node_coords, jac)
        implicit none
        class(type_square_second), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: jac(:, :)

        integer(int32) :: k
        real(real64) :: dpsi_xi
        real(real64) :: dpsi_eta

        jac = 0.0d0
        do k = 1, 8
            call self%dpsi(k, 1, r, dpsi_xi)
            call self%dpsi(k, 2, r, dpsi_eta)
            jac(1, 1) = jac(1, 1) + dpsi_xi * node_coords(1, k)
            jac(1, 2) = jac(1, 2) + dpsi_xi * node_coords(2, k)
            jac(2, 1) = jac(2, 1) + dpsi_eta * node_coords(1, k)
            jac(2, 2) = jac(2, 2) + dpsi_eta * node_coords(2, k)
        end do
    end subroutine jacobian_square_second

    pure module subroutine jacobian_det_square_second(self, r, node_coords, det_j)
        implicit none
        class(type_square_second), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: det_j

        real(real64) :: jac(2, 2)
        call self%jacobian(r, node_coords, jac)
        det_j = jac(1, 1) * jac(2, 2) - jac(1, 2) * jac(2, 1)
    end subroutine jacobian_det_square_second

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
        integer(int32), parameter :: max_iter = 100 ! 反復回数を増加
        real(real64) :: psi_val
        real(real64) :: inv_det
        real(real64) :: dr_x, dr_y
        real(real64) :: min_coord(2), max_coord(2)
        real(real64) :: init_guesses(9, 2) ! 9点試行
        integer(int32) :: ig

        ! 1. バウンディングボックスによる早期判定
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

        ! 2. 複数の初期値でニュートン法を試行 (中心 + 四隅 + 辺中点)
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
                    call self%psi(i, r, psi_val)
                    pos%x = pos%x + psi_val * node_coords(1, i)
                    pos%y = pos%y + psi_val * node_coords(2, i)
                end do

                dx = cartesian%x - pos%x
                dy = cartesian%y - pos%y

                ! 残差チェック
                if (sqrt(dx**2 + dy**2) < 1.0e-9) then
                    if (abs(r%x) <= 1.0d0 + 1.0e-4 .and. abs(r%y) <= 1.0d0 + 1.0e-4) then
                        is_in = .true.
                        normalized = r
                        return
                    end if
                    exit newton_loop
                end if

                call self%jacobian_det(r, node_coords, det_j)
                ! 特異点の場合は次の初期値へ(cycleでなくexit newton_loopで次のguessへ)
                if (abs(det_j) < 1.0e-12) exit newton_loop

                call self%jacobian(r, node_coords, jac)

                inv_det = 1.0d0 / det_j
                dr_x = (jac(2, 2) * dx - jac(2, 1) * dy) * inv_det
                dr_y = (-jac(1, 2) * dx + jac(1, 1) * dy) * inv_det

                r%x = r%x + dr_x
                r%y = r%y + dr_y

                ! 発散チェック (範囲外に大きく飛び出したらこの初期値は諦める)
                if (abs(r%x) > 3.0d0 .or. abs(r%y) > 3.0d0) exit newton_loop
            end do newton_loop

            ! 反復回数切れでも残差が十分小さければ採用
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

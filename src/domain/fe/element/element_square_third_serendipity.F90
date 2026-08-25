!>
!> Implements the procedures for the third-order quadrilateral Serendipity (12-node) finite element.
!> Cubic Serendipity shape functions on reference quadrilateral [-1,1]^2.
!> No interior nodes — only vertex and edge nodes.
!>
!> Node numbering:
!>   Vertices: 1(-1,-1), 2(1,-1), 3(1,1), 4(-1,1)
!>   Edge 1-2: 5(-1/3,-1), 6(1/3,-1)
!>   Edge 2-3: 7(1,-1/3), 8(1,1/3)
!>   Edge 3-4: 9(1/3,1), 10(-1/3,1)
!>   Edge 4-1: 11(-1,1/3), 12(-1,-1/3)
!>
submodule(domain_fe_element) domain_fe_element_square_third_serendipity
    implicit none

contains

    module function construct_square_third_serendipity(integration_order) result(fe)
        implicit none
        integer(int32), intent(in) :: integration_order
        class(abst_fe), allocatable :: fe

        allocate (type_square_third_serendipity :: fe)
        call fe%initialize(type=FE_TYPE%HIGHER_ORDER_QUAD%ID, dimension=2, order=3, num_nodes=12, &
                           integration_order=integration_order)
    end function construct_square_third_serendipity

    module subroutine calc_area_square_third_serendipity(self, node_coords, measure)
        implicit none
        class(type_square_third_serendipity), intent(in) :: self
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: measure

        integer(int32) :: i, ng
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
    end subroutine calc_area_square_third_serendipity

    !>
    !> Cubic Serendipity shape functions for 12-node quad.
    !> Vertex nodes: N_i = (1/32)(1+xi_i*xi)(1+eta_i*eta)(-10+9(xi^2+eta^2))
    !> Edge nodes on xi-constant edges: N_i = (9/32)(1-xi^2)(1+9*xi_i*xi)(1+eta_i*eta)
    !> Edge nodes on eta-constant edges: N_i = (9/32)(1-eta^2)(1+9*eta_i*eta)(1+xi_i*xi)
    !>
    pure elemental module subroutine calc_psi_square_third_serendipity(self, i, r, psi_val)
        implicit none
        class(type_square_third_serendipity), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: psi_val

        real(real64) :: xi, eta, xi0, eta0

        xi  = r%x
        eta = r%y

        select case (i)
        ! Vertex nodes
        case (1)  ! (-1,-1)
            xi0 = -1.0d0; eta0 = -1.0d0
            psi_val = (1.0d0/32.0d0) * (1.0d0 + xi0*xi) * (1.0d0 + eta0*eta) &
                    * (-10.0d0 + 9.0d0*(xi**2 + eta**2))
        case (2)  ! (1,-1)
            xi0 = 1.0d0; eta0 = -1.0d0
            psi_val = (1.0d0/32.0d0) * (1.0d0 + xi0*xi) * (1.0d0 + eta0*eta) &
                    * (-10.0d0 + 9.0d0*(xi**2 + eta**2))
        case (3)  ! (1,1)
            xi0 = 1.0d0; eta0 = 1.0d0
            psi_val = (1.0d0/32.0d0) * (1.0d0 + xi0*xi) * (1.0d0 + eta0*eta) &
                    * (-10.0d0 + 9.0d0*(xi**2 + eta**2))
        case (4)  ! (-1,1)
            xi0 = -1.0d0; eta0 = 1.0d0
            psi_val = (1.0d0/32.0d0) * (1.0d0 + xi0*xi) * (1.0d0 + eta0*eta) &
                    * (-10.0d0 + 9.0d0*(xi**2 + eta**2))
        ! Edge 1-2 (eta=-1): nodes at xi=-1/3, 1/3
        case (5)  ! (-1/3,-1)
            psi_val = (9.0d0/32.0d0) * (1.0d0 - xi**2) * (1.0d0 + 9.0d0*(-1.0d0/3.0d0)*xi) &
                    * (1.0d0 + (-1.0d0)*eta)
        case (6)  ! (1/3,-1)
            psi_val = (9.0d0/32.0d0) * (1.0d0 - xi**2) * (1.0d0 + 9.0d0*(1.0d0/3.0d0)*xi) &
                    * (1.0d0 + (-1.0d0)*eta)
        ! Edge 2-3 (xi=1): nodes at eta=-1/3, 1/3
        case (7)  ! (1,-1/3)
            psi_val = (9.0d0/32.0d0) * (1.0d0 - eta**2) * (1.0d0 + 9.0d0*(-1.0d0/3.0d0)*eta) &
                    * (1.0d0 + 1.0d0*xi)
        case (8)  ! (1,1/3)
            psi_val = (9.0d0/32.0d0) * (1.0d0 - eta**2) * (1.0d0 + 9.0d0*(1.0d0/3.0d0)*eta) &
                    * (1.0d0 + 1.0d0*xi)
        ! Edge 3-4 (eta=1): nodes at xi=1/3, -1/3
        case (9)  ! (1/3,1)
            psi_val = (9.0d0/32.0d0) * (1.0d0 - xi**2) * (1.0d0 + 9.0d0*(1.0d0/3.0d0)*xi) &
                    * (1.0d0 + 1.0d0*eta)
        case (10) ! (-1/3,1)
            psi_val = (9.0d0/32.0d0) * (1.0d0 - xi**2) * (1.0d0 + 9.0d0*(-1.0d0/3.0d0)*xi) &
                    * (1.0d0 + 1.0d0*eta)
        ! Edge 4-1 (xi=-1): nodes at eta=1/3, -1/3
        case (11) ! (-1,1/3)
            psi_val = (9.0d0/32.0d0) * (1.0d0 - eta**2) * (1.0d0 + 9.0d0*(1.0d0/3.0d0)*eta) &
                    * (1.0d0 + (-1.0d0)*xi)
        case (12) ! (-1,-1/3)
            psi_val = (9.0d0/32.0d0) * (1.0d0 - eta**2) * (1.0d0 + 9.0d0*(-1.0d0/3.0d0)*eta) &
                    * (1.0d0 + (-1.0d0)*xi)
        case default
            psi_val = 0.0d0
        end select
    end subroutine calc_psi_square_third_serendipity

    !>
    !> Derivatives of cubic Serendipity shape functions.
    !> j=1: d/dxi, j=2: d/deta
    !>
    pure elemental module subroutine calc_dpsi_square_third_serendipity(self, i, j, r, dpsi_val)
        implicit none
        class(type_square_third_serendipity), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: dpsi_val

        real(real64) :: xi, eta, xi0, eta0, s2

        xi  = r%x
        eta = r%y
        s2 = xi**2 + eta**2

        select case (i)
        ! Vertex nodes: N = (1/32)(1+xi0*xi)(1+eta0*eta)(-10+9*s2)
        case (1); xi0 = -1.0d0; eta0 = -1.0d0
        case (2); xi0 =  1.0d0; eta0 = -1.0d0
        case (3); xi0 =  1.0d0; eta0 =  1.0d0
        case (4); xi0 = -1.0d0; eta0 =  1.0d0
        case default
            xi0 = 0.0d0; eta0 = 0.0d0
        end select

        if (i >= 1 .and. i <= 4) then
            if (j == 1) then
                dpsi_val = (1.0d0/32.0d0) * ( &
                    xi0 * (1.0d0 + eta0*eta) * (-10.0d0 + 9.0d0*s2) &
                    + (1.0d0 + xi0*xi) * (1.0d0 + eta0*eta) * 18.0d0*xi)
            else
                dpsi_val = (1.0d0/32.0d0) * ( &
                    eta0 * (1.0d0 + xi0*xi) * (-10.0d0 + 9.0d0*s2) &
                    + (1.0d0 + xi0*xi) * (1.0d0 + eta0*eta) * 18.0d0*eta)
            end if
            return
        end if

        select case (i)
        ! Edge 1-2 (eta=-1): N = (9/32)(1-xi^2)(1+9*xi_e*xi)(1-eta) where xi_e=-1/3 or 1/3
        case (5)  ! xi_e = -1/3
            if (j == 1) then
                dpsi_val = (9.0d0/32.0d0) * (1.0d0 - eta) * &
                    ((-2.0d0*xi) * (1.0d0 - 3.0d0*xi) + (1.0d0 - xi**2) * (-3.0d0))
            else
                dpsi_val = (9.0d0/32.0d0) * (1.0d0 - xi**2) * (1.0d0 - 3.0d0*xi) * (-1.0d0)
            end if
        case (6)  ! xi_e = 1/3
            if (j == 1) then
                dpsi_val = (9.0d0/32.0d0) * (1.0d0 - eta) * &
                    ((-2.0d0*xi) * (1.0d0 + 3.0d0*xi) + (1.0d0 - xi**2) * 3.0d0)
            else
                dpsi_val = (9.0d0/32.0d0) * (1.0d0 - xi**2) * (1.0d0 + 3.0d0*xi) * (-1.0d0)
            end if
        ! Edge 2-3 (xi=1): N = (9/32)(1-eta^2)(1+9*eta_e*eta)(1+xi) where eta_e=-1/3 or 1/3
        case (7)  ! eta_e = -1/3
            if (j == 1) then
                dpsi_val = (9.0d0/32.0d0) * (1.0d0 - eta**2) * (1.0d0 - 3.0d0*eta) * 1.0d0
            else
                dpsi_val = (9.0d0/32.0d0) * (1.0d0 + xi) * &
                    ((-2.0d0*eta) * (1.0d0 - 3.0d0*eta) + (1.0d0 - eta**2) * (-3.0d0))
            end if
        case (8)  ! eta_e = 1/3
            if (j == 1) then
                dpsi_val = (9.0d0/32.0d0) * (1.0d0 - eta**2) * (1.0d0 + 3.0d0*eta) * 1.0d0
            else
                dpsi_val = (9.0d0/32.0d0) * (1.0d0 + xi) * &
                    ((-2.0d0*eta) * (1.0d0 + 3.0d0*eta) + (1.0d0 - eta**2) * 3.0d0)
            end if
        ! Edge 3-4 (eta=1): N = (9/32)(1-xi^2)(1+9*xi_e*xi)(1+eta)
        case (9)  ! xi_e = 1/3
            if (j == 1) then
                dpsi_val = (9.0d0/32.0d0) * (1.0d0 + eta) * &
                    ((-2.0d0*xi) * (1.0d0 + 3.0d0*xi) + (1.0d0 - xi**2) * 3.0d0)
            else
                dpsi_val = (9.0d0/32.0d0) * (1.0d0 - xi**2) * (1.0d0 + 3.0d0*xi) * 1.0d0
            end if
        case (10) ! xi_e = -1/3
            if (j == 1) then
                dpsi_val = (9.0d0/32.0d0) * (1.0d0 + eta) * &
                    ((-2.0d0*xi) * (1.0d0 - 3.0d0*xi) + (1.0d0 - xi**2) * (-3.0d0))
            else
                dpsi_val = (9.0d0/32.0d0) * (1.0d0 - xi**2) * (1.0d0 - 3.0d0*xi) * 1.0d0
            end if
        ! Edge 4-1 (xi=-1): N = (9/32)(1-eta^2)(1+9*eta_e*eta)(1-xi)
        case (11) ! eta_e = 1/3
            if (j == 1) then
                dpsi_val = (9.0d0/32.0d0) * (1.0d0 - eta**2) * (1.0d0 + 3.0d0*eta) * (-1.0d0)
            else
                dpsi_val = (9.0d0/32.0d0) * (1.0d0 - xi) * &
                    ((-2.0d0*eta) * (1.0d0 + 3.0d0*eta) + (1.0d0 - eta**2) * 3.0d0)
            end if
        case (12) ! eta_e = -1/3
            if (j == 1) then
                dpsi_val = (9.0d0/32.0d0) * (1.0d0 - eta**2) * (1.0d0 - 3.0d0*eta) * (-1.0d0)
            else
                dpsi_val = (9.0d0/32.0d0) * (1.0d0 - xi) * &
                    ((-2.0d0*eta) * (1.0d0 - 3.0d0*eta) + (1.0d0 - eta**2) * (-3.0d0))
            end if
        case default
            dpsi_val = 0.0d0
        end select
    end subroutine calc_dpsi_square_third_serendipity

    pure module subroutine calc_jacobian_square_third_serendipity(self, r, node_coords, jac)
        implicit none
        class(type_square_third_serendipity), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: jac(:, :)

        integer(int32) :: k
        real(real64) :: dpsi_xi, dpsi_eta, xk, yk

        jac = 0.0d0
        do k = 1, 12
            call self%calc_dpsi(k, 1, r, dpsi_xi)
            call self%calc_dpsi(k, 2, r, dpsi_eta)
            xk = node_coords(1, k)
            yk = node_coords(2, k)

            jac(1, 1) = jac(1, 1) + dpsi_xi * xk
            jac(1, 2) = jac(1, 2) + dpsi_eta * xk
            jac(2, 1) = jac(2, 1) + dpsi_xi * yk
            jac(2, 2) = jac(2, 2) + dpsi_eta * yk
        end do
    end subroutine calc_jacobian_square_third_serendipity

    module subroutine is_in_square_third_serendipity(self, cartesian, normalized, node_coords, is_in)
        implicit none
        class(type_square_third_serendipity), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: cartesian
        type(type_coordinate_dp), intent(inout) :: normalized
        real(real64), intent(in) :: node_coords(:, :)
        logical, intent(inout) :: is_in

        type(type_coordinate_dp) :: r_loc, pos
        real(real64) :: det_j, dx, dy, jac(2, 2), psi_val, inv_det
        integer(int32) :: iter, k, nn
        real(real64), parameter :: tol = 1.0e-9
        real(real64), parameter :: inside_tol = 1.0e-4
        integer(int32), parameter :: max_iter = 20

        call r_loc%set(0.0d0, 0.0d0, 0.0d0)
        call self%get_num_nodes(nn)
        is_in = .false.

        do iter = 1, max_iter
            call pos%set(0.0d0, 0.0d0, 0.0d0)
            do k = 1, nn
                call self%calc_psi(k, r_loc, psi_val)
                pos%x = pos%x + psi_val * node_coords(1, k)
                pos%y = pos%y + psi_val * node_coords(2, k)
            end do

            dx = cartesian%x - pos%x
            dy = cartesian%y - pos%y

            if (sqrt(dx**2 + dy**2) < tol) then
                if (abs(r_loc%x) <= 1.0d0 + inside_tol .and. abs(r_loc%y) <= 1.0d0 + inside_tol) then
                    is_in = .true.
                    normalized = r_loc
                end if
                return
            end if

            call self%calc_jacobian(r_loc, node_coords, jac)
            det_j = jac(1, 1) * jac(2, 2) - jac(1, 2) * jac(2, 1)
            if (abs(det_j) < 1.0e-12) return
            inv_det = 1.0d0 / det_j

            r_loc%x = r_loc%x + (jac(2, 2) * dx - jac(1, 2) * dy) * inv_det
            r_loc%y = r_loc%y + (-jac(2, 1) * dx + jac(1, 1) * dy) * inv_det

            if (abs(r_loc%x) > 3.0d0 .or. abs(r_loc%y) > 3.0d0) return
        end do
    end subroutine is_in_square_third_serendipity

end submodule domain_fe_element_square_third_serendipity

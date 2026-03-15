!>
!> Implements the procedures for the third-order triangular (10-node) finite element.
!> Uses Lagrange P3 shape functions on the standard triangle with
!> barycentric coordinate nodes at L = {0, 1/3, 2/3, 1}.
!>
!> Node numbering (VTK LagrangeTriangle convention):
!>   Vertices: 1(0,0), 2(1,0), 3(0,1)
!>   Edge 1-2: 4(1/3,0), 5(2/3,0)
!>   Edge 2-3: 6(2/3,1/3), 7(1/3,2/3)
!>   Edge 3-1: 8(0,2/3), 9(0,1/3)
!>   Interior: 10(1/3,1/3)
!>
submodule(domain_fe_element) domain_fe_element_triangle_third
    implicit none

contains

    module function construct_triangle_third(integration_order) result(fe)
        implicit none
        integer(int32), intent(in) :: integration_order
        class(abst_fe), allocatable :: fe

        allocate (type_triangle_third :: fe)

        call fe%initialize(type=FE_TYPE%LAGRANGE_TRIANGLE%ID, dimension=2, order=3, num_nodes=10, &
                           integration_order=integration_order)

    end function construct_triangle_third

    module subroutine calc_area_triangle_third(self, node_coords, measure)
        implicit none
        class(type_triangle_third), intent(in) :: self
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
    end subroutine calc_area_triangle_third

    pure elemental module subroutine calc_psi_triangle_third(self, i, r, psi_val)
        implicit none
        class(type_triangle_third), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: psi_val

        real(real64) :: L1, L2, L3

        L1 = 1.0d0 - r%x - r%y
        L2 = r%x
        L3 = r%y

        ! P3 Lagrange shape functions: N_i = (9/2) * product of (L_k - c)
        select case (i)
        case (1)
            psi_val = 0.5d0 * L1 * (3.0d0 * L1 - 1.0d0) * (3.0d0 * L1 - 2.0d0)
        case (2)
            psi_val = 0.5d0 * L2 * (3.0d0 * L2 - 1.0d0) * (3.0d0 * L2 - 2.0d0)
        case (3)
            psi_val = 0.5d0 * L3 * (3.0d0 * L3 - 1.0d0) * (3.0d0 * L3 - 2.0d0)
        case (4) ! Edge 1-2, at (1/3, 0)
            psi_val = 4.5d0 * L1 * L2 * (3.0d0 * L1 - 1.0d0)
        case (5) ! Edge 1-2, at (2/3, 0)
            psi_val = 4.5d0 * L1 * L2 * (3.0d0 * L2 - 1.0d0)
        case (6) ! Edge 2-3, at (2/3, 1/3)
            psi_val = 4.5d0 * L2 * L3 * (3.0d0 * L2 - 1.0d0)
        case (7) ! Edge 2-3, at (1/3, 2/3)
            psi_val = 4.5d0 * L2 * L3 * (3.0d0 * L3 - 1.0d0)
        case (8) ! Edge 3-1, at (0, 2/3)
            psi_val = 4.5d0 * L3 * L1 * (3.0d0 * L3 - 1.0d0)
        case (9) ! Edge 3-1, at (0, 1/3)
            psi_val = 4.5d0 * L3 * L1 * (3.0d0 * L1 - 1.0d0)
        case (10) ! Interior, at (1/3, 1/3)
            psi_val = 27.0d0 * L1 * L2 * L3
        case default
            psi_val = 0.0d0
        end select
    end subroutine calc_psi_triangle_third

    pure elemental module subroutine calc_dpsi_triangle_third(self, i, j, r, dpsi_val)
        implicit none
        class(type_triangle_third), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: dpsi_val

        real(real64) :: L1, L2, L3
        real(real64) :: dL1, dL2, dL3

        L1 = 1.0d0 - r%x - r%y
        L2 = r%x
        L3 = r%y

        ! Derivatives of area coordinates w.r.t. xi (j=1) or eta (j=2)
        if (j == 1) then
            dL1 = -1.0d0; dL2 = 1.0d0; dL3 = 0.0d0
        else
            dL1 = -1.0d0; dL2 = 0.0d0; dL3 = 1.0d0
        end if

        dpsi_val = 0.0d0

        select case (i)
        case (1)
            dpsi_val = 0.5d0 * dL1 * ((3.0d0*L1 - 1.0d0)*(3.0d0*L1 - 2.0d0) &
                     + L1 * 3.0d0 * (3.0d0*L1 - 2.0d0) &
                     + L1 * (3.0d0*L1 - 1.0d0) * 3.0d0)
        case (2)
            dpsi_val = 0.5d0 * dL2 * ((3.0d0*L2 - 1.0d0)*(3.0d0*L2 - 2.0d0) &
                     + L2 * 3.0d0 * (3.0d0*L2 - 2.0d0) &
                     + L2 * (3.0d0*L2 - 1.0d0) * 3.0d0)
        case (3)
            dpsi_val = 0.5d0 * dL3 * ((3.0d0*L3 - 1.0d0)*(3.0d0*L3 - 2.0d0) &
                     + L3 * 3.0d0 * (3.0d0*L3 - 2.0d0) &
                     + L3 * (3.0d0*L3 - 1.0d0) * 3.0d0)
        case (4)
            dpsi_val = 4.5d0 * (dL1*L2*(3.0d0*L1 - 1.0d0) + L1*dL2*(3.0d0*L1 - 1.0d0) &
                     + L1*L2*3.0d0*dL1)
        case (5)
            dpsi_val = 4.5d0 * (dL1*L2*(3.0d0*L2 - 1.0d0) + L1*dL2*(3.0d0*L2 - 1.0d0) &
                     + L1*L2*3.0d0*dL2)
        case (6)
            dpsi_val = 4.5d0 * (dL2*L3*(3.0d0*L2 - 1.0d0) + L2*dL3*(3.0d0*L2 - 1.0d0) &
                     + L2*L3*3.0d0*dL2)
        case (7)
            dpsi_val = 4.5d0 * (dL2*L3*(3.0d0*L3 - 1.0d0) + L2*dL3*(3.0d0*L3 - 1.0d0) &
                     + L2*L3*3.0d0*dL3)
        case (8)
            dpsi_val = 4.5d0 * (dL3*L1*(3.0d0*L3 - 1.0d0) + L3*dL1*(3.0d0*L3 - 1.0d0) &
                     + L3*L1*3.0d0*dL3)
        case (9)
            dpsi_val = 4.5d0 * (dL3*L1*(3.0d0*L1 - 1.0d0) + L3*dL1*(3.0d0*L1 - 1.0d0) &
                     + L3*L1*3.0d0*dL1)
        case (10)
            dpsi_val = 27.0d0 * (dL1*L2*L3 + L1*dL2*L3 + L1*L2*dL3)
        end select
    end subroutine calc_dpsi_triangle_third

    pure module subroutine calc_jacobian_triangle_third(self, r, node_coords, jac)
        implicit none
        class(type_triangle_third), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: jac(:, :)

        integer(int32) :: k
        real(real64) :: dpsi_xi, dpsi_eta, xk, yk

        jac = 0.0d0
        do k = 1, 10
            call self%calc_dpsi(k, 1, r, dpsi_xi)
            call self%calc_dpsi(k, 2, r, dpsi_eta)
            xk = node_coords(1, k)
            yk = node_coords(2, k)

            jac(1, 1) = jac(1, 1) + dpsi_xi * xk
            jac(1, 2) = jac(1, 2) + dpsi_eta * xk
            jac(2, 1) = jac(2, 1) + dpsi_xi * yk
            jac(2, 2) = jac(2, 2) + dpsi_eta * yk
        end do
    end subroutine calc_jacobian_triangle_third

    module subroutine is_in_triangle_third(self, cartesian, normalized, node_coords, is_in)
        implicit none
        class(type_triangle_third), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: cartesian
        type(type_coordinate_dp), intent(inout) :: normalized
        real(real64), intent(in) :: node_coords(:, :)
        logical, intent(inout) :: is_in

        type(type_coordinate_dp) :: r_loc, pos
        real(real64) :: det_j, dx, dy, jac(2, 2), psi_val
        integer(int32) :: iter, k, nn
        logical :: converged
        real(real64), parameter :: tol = 1.0e-9
        integer(int32), parameter :: max_iter = 20

        call r_loc%set(1.0d0 / 3.0d0, 1.0d0 / 3.0d0, 0.0d0)
        call self%get_num_nodes(nn)
        converged = .false.

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
                converged = .true.
                exit
            end if

            call self%calc_jacobian_determinant(r_loc, node_coords, det_j)
            if (abs(det_j) < epsilon(det_j)) exit

            call self%calc_jacobian(r_loc, node_coords, jac)

            r_loc%x = r_loc%x + (jac(2, 2) * dx - jac(1, 2) * dy) / det_j
            r_loc%y = r_loc%y + (-jac(2, 1) * dx + jac(1, 1) * dy) / det_j
        end do

        is_in = converged .and. (r_loc%x >= -tol) .and. (r_loc%y >= -tol) &
                .and. (r_loc%x + r_loc%y <= 1.0d0 + tol)
        if (is_in) normalized = r_loc
    end subroutine is_in_triangle_third

end submodule domain_fe_element_triangle_third

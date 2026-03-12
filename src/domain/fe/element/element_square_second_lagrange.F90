!>
!> Implements the procedures for the second-order quadrilateral Lagrange (9-node) finite element.
!> Biquadratic Lagrange shape functions on reference quadrilateral [-1,1]^2.
!>
!> Node numbering (VTK BiQuadraticQuad convention):
!>   Vertices: 1(-1,-1), 2(1,-1), 3(1,1), 4(-1,1)
!>   Edge midpoints: 5(0,-1), 6(1,0), 7(0,1), 8(-1,0)
!>   Center: 9(0,0)
!>
submodule(domain_fe_element) domain_fe_element_square_second_lagrange
    implicit none

    ! 1D quadratic Lagrange node positions: {-1, 0, 1}
    real(real64), parameter :: nd(3) = [-1.0d0, 0.0d0, 1.0d0]

contains

    ! 1D quadratic Lagrange basis function
    pure function lagrange_1d_q2(a, t) result(val)
        implicit none
        integer(int32), intent(in) :: a
        real(real64), intent(in) :: t
        real(real64) :: val
        integer(int32) :: b

        val = 1.0d0
        do b = 1, 3
            if (b /= a) val = val * (t - nd(b)) / (nd(a) - nd(b))
        end do
    end function lagrange_1d_q2

    ! Derivative of 1D quadratic Lagrange basis function
    pure function dlagrange_1d_q2(a, t) result(val)
        implicit none
        integer(int32), intent(in) :: a
        real(real64), intent(in) :: t
        real(real64) :: val
        integer(int32) :: b, c
        real(real64) :: prod

        val = 0.0d0
        do b = 1, 3
            if (b /= a) then
                prod = 1.0d0 / (nd(a) - nd(b))
                do c = 1, 3
                    if (c /= a .and. c /= b) prod = prod * (t - nd(c)) / (nd(a) - nd(c))
                end do
                val = val + prod
            end if
        end do
    end function dlagrange_1d_q2

    ! Map VTK BiQuadraticQuad node (1..9) to tensor-product indices (ix, iy) in {1,2,3}
    pure subroutine node_to_ij_q9(node, ix, iy)
        implicit none
        integer(int32), intent(in) :: node
        integer(int32), intent(inout) :: ix, iy

        select case (node)
        ! Vertices
        case (1); ix = 1; iy = 1
        case (2); ix = 3; iy = 1
        case (3); ix = 3; iy = 3
        case (4); ix = 1; iy = 3
        ! Edge midpoints
        case (5); ix = 2; iy = 1   ! Edge 1-2
        case (6); ix = 3; iy = 2   ! Edge 2-3
        case (7); ix = 2; iy = 3   ! Edge 3-4
        case (8); ix = 1; iy = 2   ! Edge 4-1
        ! Center
        case (9); ix = 2; iy = 2
        case default
            ix = 1; iy = 1
        end select
    end subroutine node_to_ij_q9

    module function construct_square_second_lagrange(integration_order) result(fe)
        implicit none
        integer(int32), intent(in) :: integration_order
        class(abst_fe), allocatable :: fe

        allocate (type_square_second_lagrange :: fe)
        call fe%initialize(type=FE_TYPE%BIQUADRATIC_QUAD%ID, dimension=2, order=2, num_nodes=9, &
                           integration_order=integration_order)
    end function construct_square_second_lagrange

    module subroutine calc_area_square_second_lagrange(self, node_coords, measure)
        implicit none
        class(type_square_second_lagrange), intent(in) :: self
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
    end subroutine calc_area_square_second_lagrange

    pure elemental module subroutine calc_psi_square_second_lagrange(self, i, r, psi_val)
        implicit none
        class(type_square_second_lagrange), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: psi_val

        integer(int32) :: ix, iy

        ix = 0; iy = 0
        call node_to_ij_q9(i, ix, iy)
        psi_val = lagrange_1d_q2(ix, r%x) * lagrange_1d_q2(iy, r%y)
    end subroutine calc_psi_square_second_lagrange

    pure elemental module subroutine calc_dpsi_square_second_lagrange(self, i, j, r, dpsi_val)
        implicit none
        class(type_square_second_lagrange), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: dpsi_val

        integer(int32) :: ix, iy

        ix = 0; iy = 0
        call node_to_ij_q9(i, ix, iy)

        if (j == 1) then
            dpsi_val = dlagrange_1d_q2(ix, r%x) * lagrange_1d_q2(iy, r%y)
        else
            dpsi_val = lagrange_1d_q2(ix, r%x) * dlagrange_1d_q2(iy, r%y)
        end if
    end subroutine calc_dpsi_square_second_lagrange

    pure module subroutine calc_jacobian_square_second_lagrange(self, r, node_coords, jac)
        implicit none
        class(type_square_second_lagrange), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: jac(:, :)

        integer(int32) :: k
        real(real64) :: dpsi_xi, dpsi_eta, xk, yk

        jac = 0.0d0
        do k = 1, 9
            call self%calc_dpsi(k, 1, r, dpsi_xi)
            call self%calc_dpsi(k, 2, r, dpsi_eta)
            xk = node_coords(1, k)
            yk = node_coords(2, k)

            jac(1, 1) = jac(1, 1) + dpsi_xi * xk
            jac(1, 2) = jac(1, 2) + dpsi_eta * xk
            jac(2, 1) = jac(2, 1) + dpsi_xi * yk
            jac(2, 2) = jac(2, 2) + dpsi_eta * yk
        end do
    end subroutine calc_jacobian_square_second_lagrange

    module subroutine is_in_square_second_lagrange(self, cartesian, normalized, node_coords, is_in)
        implicit none
        class(type_square_second_lagrange), intent(in) :: self
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

            call self%calc_jacobian_determinant(r_loc, node_coords, det_j)
            if (abs(det_j) < 1.0e-12) return

            call self%calc_jacobian(r_loc, node_coords, jac)
            inv_det = 1.0d0 / det_j

            r_loc%x = r_loc%x + (jac(2, 2) * dx - jac(1, 2) * dy) * inv_det
            r_loc%y = r_loc%y + (-jac(2, 1) * dx + jac(1, 1) * dy) * inv_det

            if (abs(r_loc%x) > 3.0d0 .or. abs(r_loc%y) > 3.0d0) return
        end do
    end subroutine is_in_square_second_lagrange

end submodule domain_fe_element_square_second_lagrange

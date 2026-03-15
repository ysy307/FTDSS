!>
!> Implements the procedures for the second-order hexahedral (27-node) finite element.
!> Triquadratic Lagrange shape functions on reference hexahedron [-1,1]^3.
!>
!> Node numbering (VTK TriQuadraticHexahedron convention):
!>   Vertices (1-8): standard hex ordering
!>   Edge midpoints (9-20): 1 midpoint per edge, 12 edges
!>   Face centers (21-26): 1 center per face, 6 faces
!>   Body center (27): 1 interior node
!>
submodule(domain_fe_volume) domain_fe_volume_hexa_second
    implicit none

    ! 1D quadratic Lagrange node positions: {-1, 0, 1}
    real(real64), parameter :: nd(3) = [-1.0d0, 0.0d0, 1.0d0]

contains

    ! 1D quadratic Lagrange basis functions
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

    ! Map VTK TriQuadraticHexahedron node (1..27) to tensor-product indices (ix,iy,iz) in {1,2,3}
    pure subroutine node_to_ijk_hexa27(node, ix, iy, iz)
        implicit none
        integer(int32), intent(in) :: node
        integer(int32), intent(inout) :: ix, iy, iz

        select case (node)
        ! Vertices
        case (1);  ix = 1; iy = 1; iz = 1
        case (2);  ix = 3; iy = 1; iz = 1
        case (3);  ix = 3; iy = 3; iz = 1
        case (4);  ix = 1; iy = 3; iz = 1
        case (5);  ix = 1; iy = 1; iz = 3
        case (6);  ix = 3; iy = 1; iz = 3
        case (7);  ix = 3; iy = 3; iz = 3
        case (8);  ix = 1; iy = 3; iz = 3
        ! Edge midpoints (bottom face z=-1)
        case (9);  ix = 2; iy = 1; iz = 1   ! Edge 1-2
        case (10); ix = 3; iy = 2; iz = 1   ! Edge 2-3
        case (11); ix = 2; iy = 3; iz = 1   ! Edge 3-4
        case (12); ix = 1; iy = 2; iz = 1   ! Edge 4-1
        ! Edge midpoints (top face z=+1)
        case (13); ix = 2; iy = 1; iz = 3   ! Edge 5-6
        case (14); ix = 3; iy = 2; iz = 3   ! Edge 6-7
        case (15); ix = 2; iy = 3; iz = 3   ! Edge 7-8
        case (16); ix = 1; iy = 2; iz = 3   ! Edge 8-5
        ! Vertical edge midpoints
        case (17); ix = 1; iy = 1; iz = 2   ! Edge 1-5
        case (18); ix = 3; iy = 1; iz = 2   ! Edge 2-6
        case (19); ix = 3; iy = 3; iz = 2   ! Edge 3-7
        case (20); ix = 1; iy = 3; iz = 2   ! Edge 4-8
        ! Face centers
        case (21); ix = 2; iy = 2; iz = 1   ! Bottom (z=-1)
        case (22); ix = 2; iy = 2; iz = 3   ! Top (z=+1)
        case (23); ix = 2; iy = 1; iz = 2   ! Front (y=-1)
        case (24); ix = 3; iy = 2; iz = 2   ! Right (x=+1)
        case (25); ix = 2; iy = 3; iz = 2   ! Back (y=+1)
        case (26); ix = 1; iy = 2; iz = 2   ! Left (x=-1)
        ! Body center
        case (27); ix = 2; iy = 2; iz = 2
        case default
            ix = 1; iy = 1; iz = 1
        end select
    end subroutine node_to_ijk_hexa27

    module function construct_hexa_second(integration_order) result(fe)
        implicit none
        integer(int32), intent(in) :: integration_order
        class(abst_fe), allocatable :: fe

        allocate (type_hexa_second :: fe)
        call fe%initialize(type=FE_TYPE%TRIQUADRATIC_HEXAHEDRON%ID, dimension=3, order=2, num_nodes=27, &
                           integration_order=integration_order)
    end function construct_hexa_second

    module subroutine calc_volume_hexa_second(self, node_coords, measure)
        implicit none
        class(type_hexa_second), intent(in) :: self
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
    end subroutine calc_volume_hexa_second

    pure elemental module subroutine calc_psi_hexa_second(self, i, r, psi_val)
        implicit none
        class(type_hexa_second), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: psi_val

        integer(int32) :: ix, iy, iz

        ix = 0; iy = 0; iz = 0
        call node_to_ijk_hexa27(i, ix, iy, iz)
        psi_val = lagrange_1d_q2(ix, r%x) * lagrange_1d_q2(iy, r%y) * lagrange_1d_q2(iz, r%z)
    end subroutine calc_psi_hexa_second

    pure elemental module subroutine calc_dpsi_hexa_second(self, i, j, r, dpsi_val)
        implicit none
        class(type_hexa_second), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: dpsi_val

        integer(int32) :: ix, iy, iz

        ix = 0; iy = 0; iz = 0
        call node_to_ijk_hexa27(i, ix, iy, iz)

        select case (j)
        case (1)
            dpsi_val = dlagrange_1d_q2(ix, r%x) * lagrange_1d_q2(iy, r%y) * lagrange_1d_q2(iz, r%z)
        case (2)
            dpsi_val = lagrange_1d_q2(ix, r%x) * dlagrange_1d_q2(iy, r%y) * lagrange_1d_q2(iz, r%z)
        case (3)
            dpsi_val = lagrange_1d_q2(ix, r%x) * lagrange_1d_q2(iy, r%y) * dlagrange_1d_q2(iz, r%z)
        case default
            dpsi_val = 0.0d0
        end select
    end subroutine calc_dpsi_hexa_second

    pure module subroutine calc_jacobian_hexa_second(self, r, node_coords, jac)
        implicit none
        class(type_hexa_second), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: jac(:, :)

        integer(int32) :: k
        real(real64) :: dpsi_xi, dpsi_eta, dpsi_zeta
        real(real64) :: xk, yk, zk

        jac = 0.0d0
        do k = 1, 27
            call self%calc_dpsi(k, 1, r, dpsi_xi)
            call self%calc_dpsi(k, 2, r, dpsi_eta)
            call self%calc_dpsi(k, 3, r, dpsi_zeta)
            xk = node_coords(1, k)
            yk = node_coords(2, k)
            zk = node_coords(3, k)

            jac(1, 1) = jac(1, 1) + dpsi_xi * xk
            jac(1, 2) = jac(1, 2) + dpsi_eta * xk
            jac(1, 3) = jac(1, 3) + dpsi_zeta * xk

            jac(2, 1) = jac(2, 1) + dpsi_xi * yk
            jac(2, 2) = jac(2, 2) + dpsi_eta * yk
            jac(2, 3) = jac(2, 3) + dpsi_zeta * yk

            jac(3, 1) = jac(3, 1) + dpsi_xi * zk
            jac(3, 2) = jac(3, 2) + dpsi_eta * zk
            jac(3, 3) = jac(3, 3) + dpsi_zeta * zk
        end do
    end subroutine calc_jacobian_hexa_second

    module subroutine is_in_hexa_second(self, cartesian, normalized, node_coords, is_in)
        implicit none
        class(type_hexa_second), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: cartesian
        type(type_coordinate_dp), intent(inout) :: normalized
        real(real64), intent(in) :: node_coords(:, :)
        logical, intent(inout) :: is_in

        type(type_coordinate_dp) :: r_loc, pos
        real(real64) :: jac(3,3), dx, dy, dz, det_j, psi_val
        real(real64) :: c11, c12, c13, c21, c22, c23, c31, c32, c33, inv_det
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
                pos%z = pos%z + psi_val * node_coords(3, k)
            end do

            dx = cartesian%x - pos%x
            dy = cartesian%y - pos%y
            dz = cartesian%z - pos%z
            if (sqrt(dx**2 + dy**2 + dz**2) < tol) then
                if (abs(r_loc%x) <= 1.0d0 + inside_tol .and. &
                    abs(r_loc%y) <= 1.0d0 + inside_tol .and. &
                    abs(r_loc%z) <= 1.0d0 + inside_tol) then
                    is_in = .true.
                    normalized = r_loc
                end if
                return
            end if

            call self%calc_jacobian(r_loc, node_coords, jac)
            det_j = jac(1,1)*(jac(2,2)*jac(3,3) - jac(2,3)*jac(3,2)) &
                  - jac(1,2)*(jac(2,1)*jac(3,3) - jac(2,3)*jac(3,1)) &
                  + jac(1,3)*(jac(2,1)*jac(3,2) - jac(2,2)*jac(3,1))
            if (abs(det_j) < 1.0e-12) return

            inv_det = 1.0d0 / det_j
            c11 =  (jac(2,2)*jac(3,3) - jac(2,3)*jac(3,2))
            c12 = -(jac(2,1)*jac(3,3) - jac(2,3)*jac(3,1))
            c13 =  (jac(2,1)*jac(3,2) - jac(2,2)*jac(3,1))
            c21 = -(jac(1,2)*jac(3,3) - jac(1,3)*jac(3,2))
            c22 =  (jac(1,1)*jac(3,3) - jac(1,3)*jac(3,1))
            c23 = -(jac(1,1)*jac(3,2) - jac(1,2)*jac(3,1))
            c31 =  (jac(1,2)*jac(2,3) - jac(1,3)*jac(2,2))
            c32 = -(jac(1,1)*jac(2,3) - jac(1,3)*jac(2,1))
            c33 =  (jac(1,1)*jac(2,2) - jac(1,2)*jac(2,1))

            r_loc%x = r_loc%x + (c11*dx + c21*dy + c31*dz) * inv_det
            r_loc%y = r_loc%y + (c12*dx + c22*dy + c32*dz) * inv_det
            r_loc%z = r_loc%z + (c13*dx + c23*dy + c33*dz) * inv_det

            if (abs(r_loc%x) > 3.0d0 .or. abs(r_loc%y) > 3.0d0 .or. abs(r_loc%z) > 3.0d0) return
        end do
    end subroutine is_in_hexa_second

end submodule domain_fe_volume_hexa_second

!>
!> Implements the procedures for the third-order hexahedral Serendipity (32-node) finite element.
!> Cubic Serendipity shape functions on reference hexahedron [-1,1]^3.
!> 8 vertex nodes + 24 edge nodes (2 per edge, 12 edges). No face or interior nodes.
!>
!> Node numbering:
!>   Vertices (1-8): standard hex ordering
!>     1(-1,-1,-1), 2(1,-1,-1), 3(1,1,-1), 4(-1,1,-1)
!>     5(-1,-1,1), 6(1,-1,1), 7(1,1,1), 8(-1,1,1)
!>   Edge nodes (9-32): 2 nodes per edge at ±1/3
!>     Bottom edges (z=-1):
!>       Edge 1-2: 9(-1/3,-1,-1), 10(1/3,-1,-1)
!>       Edge 2-3: 11(1,-1/3,-1), 12(1,1/3,-1)
!>       Edge 3-4: 13(1/3,1,-1), 14(-1/3,1,-1)
!>       Edge 4-1: 15(-1,1/3,-1), 16(-1,-1/3,-1)
!>     Top edges (z=1):
!>       Edge 5-6: 17(-1/3,-1,1), 18(1/3,-1,1)
!>       Edge 6-7: 19(1,-1/3,1), 20(1,1/3,1)
!>       Edge 7-8: 21(1/3,1,1), 22(-1/3,1,1)
!>       Edge 8-5: 23(-1,1/3,1), 24(-1,-1/3,1)
!>     Vertical edges:
!>       Edge 1-5: 25(-1,-1,-1/3), 26(-1,-1,1/3)
!>       Edge 2-6: 27(1,-1,-1/3), 28(1,-1,1/3)
!>       Edge 3-7: 29(1,1,-1/3), 30(1,1,1/3)
!>       Edge 4-8: 31(-1,1,-1/3), 32(-1,1,1/3)
!>
submodule(domain_fe_volume) domain_fe_volume_hexa_third_serendipity
    implicit none

    ! Vertex reference coordinates
    real(real64), parameter :: vx(8) = [-1.0d0,  1.0d0,  1.0d0, -1.0d0, -1.0d0,  1.0d0,  1.0d0, -1.0d0]
    real(real64), parameter :: vy(8) = [-1.0d0, -1.0d0,  1.0d0,  1.0d0, -1.0d0, -1.0d0,  1.0d0,  1.0d0]
    real(real64), parameter :: vz(8) = [-1.0d0, -1.0d0, -1.0d0, -1.0d0,  1.0d0,  1.0d0,  1.0d0,  1.0d0]

    ! Edge node reference coordinates (24 nodes, indices 1..24 for nodes 9..32)
    ! Free direction: 1=xi, 2=eta, 3=zeta
    ! Edge parameter value at each node: -1/3 or +1/3
    real(real64), parameter :: third = 1.0d0 / 3.0d0

contains

    ! Get reference coordinates and free direction for edge node (local index 1..24)
    pure subroutine get_edge_node_info(idx, xi0, eta0, zeta0, free_dir, t_val)
        implicit none
        integer(int32), intent(in) :: idx
        real(real64), intent(out) :: xi0, eta0, zeta0, t_val
        integer(int32), intent(out) :: free_dir

        select case (idx)
        ! Bottom edges (z=-1)
        ! Edge 1-2: xi free, eta=-1, zeta=-1
        case (1);  xi0 = -third; eta0 = -1.0d0; zeta0 = -1.0d0; free_dir = 1; t_val = -third
        case (2);  xi0 =  third; eta0 = -1.0d0; zeta0 = -1.0d0; free_dir = 1; t_val =  third
        ! Edge 2-3: eta free, xi=1, zeta=-1
        case (3);  xi0 = 1.0d0;  eta0 = -third; zeta0 = -1.0d0; free_dir = 2; t_val = -third
        case (4);  xi0 = 1.0d0;  eta0 =  third; zeta0 = -1.0d0; free_dir = 2; t_val =  third
        ! Edge 3-4: xi free, eta=1, zeta=-1
        case (5);  xi0 =  third; eta0 = 1.0d0;  zeta0 = -1.0d0; free_dir = 1; t_val =  third
        case (6);  xi0 = -third; eta0 = 1.0d0;  zeta0 = -1.0d0; free_dir = 1; t_val = -third
        ! Edge 4-1: eta free, xi=-1, zeta=-1
        case (7);  xi0 = -1.0d0; eta0 =  third; zeta0 = -1.0d0; free_dir = 2; t_val =  third
        case (8);  xi0 = -1.0d0; eta0 = -third; zeta0 = -1.0d0; free_dir = 2; t_val = -third
        ! Top edges (z=1)
        ! Edge 5-6: xi free, eta=-1, zeta=1
        case (9);  xi0 = -third; eta0 = -1.0d0; zeta0 = 1.0d0; free_dir = 1; t_val = -third
        case (10); xi0 =  third; eta0 = -1.0d0; zeta0 = 1.0d0; free_dir = 1; t_val =  third
        ! Edge 6-7: eta free, xi=1, zeta=1
        case (11); xi0 = 1.0d0;  eta0 = -third; zeta0 = 1.0d0; free_dir = 2; t_val = -third
        case (12); xi0 = 1.0d0;  eta0 =  third; zeta0 = 1.0d0; free_dir = 2; t_val =  third
        ! Edge 7-8: xi free, eta=1, zeta=1
        case (13); xi0 =  third; eta0 = 1.0d0;  zeta0 = 1.0d0; free_dir = 1; t_val =  third
        case (14); xi0 = -third; eta0 = 1.0d0;  zeta0 = 1.0d0; free_dir = 1; t_val = -third
        ! Edge 8-5: eta free, xi=-1, zeta=1
        case (15); xi0 = -1.0d0; eta0 =  third; zeta0 = 1.0d0; free_dir = 2; t_val =  third
        case (16); xi0 = -1.0d0; eta0 = -third; zeta0 = 1.0d0; free_dir = 2; t_val = -third
        ! Vertical edges
        ! Edge 1-5: zeta free, xi=-1, eta=-1
        case (17); xi0 = -1.0d0; eta0 = -1.0d0; zeta0 = -third; free_dir = 3; t_val = -third
        case (18); xi0 = -1.0d0; eta0 = -1.0d0; zeta0 =  third; free_dir = 3; t_val =  third
        ! Edge 2-6: zeta free, xi=1, eta=-1
        case (19); xi0 = 1.0d0;  eta0 = -1.0d0; zeta0 = -third; free_dir = 3; t_val = -third
        case (20); xi0 = 1.0d0;  eta0 = -1.0d0; zeta0 =  third; free_dir = 3; t_val =  third
        ! Edge 3-7: zeta free, xi=1, eta=1
        case (21); xi0 = 1.0d0;  eta0 = 1.0d0;  zeta0 = -third; free_dir = 3; t_val = -third
        case (22); xi0 = 1.0d0;  eta0 = 1.0d0;  zeta0 =  third; free_dir = 3; t_val =  third
        ! Edge 4-8: zeta free, xi=-1, eta=1
        case (23); xi0 = -1.0d0; eta0 = 1.0d0;  zeta0 = -third; free_dir = 3; t_val = -third
        case (24); xi0 = -1.0d0; eta0 = 1.0d0;  zeta0 =  third; free_dir = 3; t_val =  third
        case default
            xi0 = 0.0d0; eta0 = 0.0d0; zeta0 = 0.0d0; free_dir = 1; t_val = 0.0d0
        end select
    end subroutine get_edge_node_info

    module function construct_hexa_third_serendipity(integration_order) result(fe)
        implicit none
        integer(int32), intent(in) :: integration_order
        class(abst_fe), allocatable :: fe

        allocate (type_hexa_third_serendipity :: fe)
        call fe%initialize(type=FE_TYPE%HIGHER_ORDER_HEXAHEDRON%ID, dimension=3, order=3, num_nodes=32, &
                           integration_order=integration_order)
    end function construct_hexa_third_serendipity

    module subroutine calc_volume_hexa_third_serendipity(self, node_coords, measure)
        implicit none
        class(type_hexa_third_serendipity), intent(in) :: self
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
    end subroutine calc_volume_hexa_third_serendipity

    !>
    !> 32-node cubic Serendipity shape functions for hexahedron.
    !>
    !> Vertex nodes (i=1..8):
    !>   N_i = (1/64)(1+xi_i*xi)(1+eta_i*eta)(1+zeta_i*zeta)
    !>         * (9(xi^2+eta^2+zeta^2) - 19)
    !>
    !> Edge nodes (i=9..32):
    !>   For edges where xi is free:
    !>     N = (9/64)(1-xi^2)(1+9*t_val*xi)(1+eta0*eta)(1+zeta0*zeta)
    !>   For edges where eta is free:
    !>     N = (9/64)(1+xi0*xi)(1-eta^2)(1+9*t_val*eta)(1+zeta0*zeta)
    !>   For edges where zeta is free:
    !>     N = (9/64)(1+xi0*xi)(1+eta0*eta)(1-zeta^2)(1+9*t_val*zeta)
    !>
    pure elemental module subroutine calc_psi_hexa_third_serendipity(self, i, r, psi_val)
        implicit none
        class(type_hexa_third_serendipity), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: psi_val

        real(real64) :: xi, eta, zeta, xi0, eta0, zeta0, t_val
        integer(int32) :: free_dir

        xi   = r%x
        eta  = r%y
        zeta = r%z

        if (i >= 1 .and. i <= 8) then
            ! Vertex node
            xi0   = vx(i)
            eta0  = vy(i)
            zeta0 = vz(i)
            psi_val = (1.0d0/64.0d0) * (1.0d0 + xi0*xi) * (1.0d0 + eta0*eta) * (1.0d0 + zeta0*zeta) &
                    * (9.0d0*(xi**2 + eta**2 + zeta**2) - 19.0d0)
        else if (i >= 9 .and. i <= 32) then
            call get_edge_node_info(i - 8, xi0, eta0, zeta0, free_dir, t_val)

            select case (free_dir)
            case (1) ! xi is free
                psi_val = (9.0d0/64.0d0) * (1.0d0 - xi**2) * (1.0d0 + 9.0d0*t_val*xi) &
                        * (1.0d0 + eta0*eta) * (1.0d0 + zeta0*zeta)
            case (2) ! eta is free
                psi_val = (9.0d0/64.0d0) * (1.0d0 + xi0*xi) * (1.0d0 - eta**2) * (1.0d0 + 9.0d0*t_val*eta) &
                        * (1.0d0 + zeta0*zeta)
            case (3) ! zeta is free
                psi_val = (9.0d0/64.0d0) * (1.0d0 + xi0*xi) * (1.0d0 + eta0*eta) &
                        * (1.0d0 - zeta**2) * (1.0d0 + 9.0d0*t_val*zeta)
            case default
                psi_val = 0.0d0
            end select
        else
            psi_val = 0.0d0
        end if
    end subroutine calc_psi_hexa_third_serendipity

    !>
    !> Derivatives of 32-node cubic Serendipity shape functions.
    !> j=1: d/dxi, j=2: d/deta, j=3: d/dzeta
    !>
    pure elemental module subroutine calc_dpsi_hexa_third_serendipity(self, i, j, r, dpsi_val)
        implicit none
        class(type_hexa_third_serendipity), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: dpsi_val

        real(real64) :: xi, eta, zeta, xi0, eta0, zeta0, t_val
        real(real64) :: a, b, c, s
        integer(int32) :: free_dir

        xi   = r%x
        eta  = r%y
        zeta = r%z

        if (i >= 1 .and. i <= 8) then
            ! Vertex: N = (1/64)(1+xi0*xi)(1+eta0*eta)(1+zeta0*zeta)(9(xi^2+eta^2+zeta^2)-19)
            xi0   = vx(i)
            eta0  = vy(i)
            zeta0 = vz(i)
            a = 1.0d0 + xi0*xi
            b = 1.0d0 + eta0*eta
            c = 1.0d0 + zeta0*zeta
            s = 9.0d0*(xi**2 + eta**2 + zeta**2) - 19.0d0

            select case (j)
            case (1)
                dpsi_val = (1.0d0/64.0d0) * (xi0 * b * c * s + a * b * c * 18.0d0*xi)
            case (2)
                dpsi_val = (1.0d0/64.0d0) * (a * eta0 * c * s + a * b * c * 18.0d0*eta)
            case (3)
                dpsi_val = (1.0d0/64.0d0) * (a * b * zeta0 * s + a * b * c * 18.0d0*zeta)
            case default
                dpsi_val = 0.0d0
            end select
        else if (i >= 9 .and. i <= 32) then
            call get_edge_node_info(i - 8, xi0, eta0, zeta0, free_dir, t_val)

            select case (free_dir)
            case (1) ! xi free: N = (9/64)(1-xi^2)(1+9*t*xi)(1+eta0*eta)(1+zeta0*zeta)
                a = 1.0d0 - xi**2
                b = 1.0d0 + 9.0d0*t_val*xi
                c = (1.0d0 + eta0*eta) * (1.0d0 + zeta0*zeta)
                select case (j)
                case (1)
                    dpsi_val = (9.0d0/64.0d0) * ((-2.0d0*xi)*b + a*9.0d0*t_val) * c
                case (2)
                    dpsi_val = (9.0d0/64.0d0) * a * b * eta0 * (1.0d0 + zeta0*zeta)
                case (3)
                    dpsi_val = (9.0d0/64.0d0) * a * b * (1.0d0 + eta0*eta) * zeta0
                case default
                    dpsi_val = 0.0d0
                end select
            case (2) ! eta free: N = (9/64)(1+xi0*xi)(1-eta^2)(1+9*t*eta)(1+zeta0*zeta)
                a = 1.0d0 - eta**2
                b = 1.0d0 + 9.0d0*t_val*eta
                c = (1.0d0 + xi0*xi) * (1.0d0 + zeta0*zeta)
                select case (j)
                case (1)
                    dpsi_val = (9.0d0/64.0d0) * xi0 * a * b * (1.0d0 + zeta0*zeta)
                case (2)
                    dpsi_val = (9.0d0/64.0d0) * (1.0d0 + xi0*xi) * ((-2.0d0*eta)*b + a*9.0d0*t_val) * (1.0d0 + zeta0*zeta)
                case (3)
                    dpsi_val = (9.0d0/64.0d0) * (1.0d0 + xi0*xi) * a * b * zeta0
                case default
                    dpsi_val = 0.0d0
                end select
            case (3) ! zeta free: N = (9/64)(1+xi0*xi)(1+eta0*eta)(1-zeta^2)(1+9*t*zeta)
                a = 1.0d0 - zeta**2
                b = 1.0d0 + 9.0d0*t_val*zeta
                c = (1.0d0 + xi0*xi) * (1.0d0 + eta0*eta)
                select case (j)
                case (1)
                    dpsi_val = (9.0d0/64.0d0) * xi0 * (1.0d0 + eta0*eta) * a * b
                case (2)
                    dpsi_val = (9.0d0/64.0d0) * (1.0d0 + xi0*xi) * eta0 * a * b
                case (3)
                    dpsi_val = (9.0d0/64.0d0) * c * ((-2.0d0*zeta)*b + a*9.0d0*t_val)
                case default
                    dpsi_val = 0.0d0
                end select
            case default
                dpsi_val = 0.0d0
            end select
        else
            dpsi_val = 0.0d0
        end if
    end subroutine calc_dpsi_hexa_third_serendipity

    pure module subroutine calc_jacobian_hexa_third_serendipity(self, r, node_coords, jac)
        implicit none
        class(type_hexa_third_serendipity), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: jac(:, :)

        integer(int32) :: k
        real(real64) :: dpsi_xi, dpsi_eta, dpsi_zeta
        real(real64) :: xk, yk, zk

        jac = 0.0d0
        do k = 1, 32
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
    end subroutine calc_jacobian_hexa_third_serendipity

    module subroutine is_in_hexa_third_serendipity(self, cartesian, normalized, node_coords, is_in)
        implicit none
        class(type_hexa_third_serendipity), intent(in) :: self
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
        integer(int32), parameter :: max_iter = 30

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
    end subroutine is_in_hexa_third_serendipity

end submodule domain_fe_volume_hexa_third_serendipity

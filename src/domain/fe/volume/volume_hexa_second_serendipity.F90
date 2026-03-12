!>
!> Implements the procedures for the second-order hexahedral Serendipity (20-node) finite element.
!> Quadratic Serendipity shape functions on reference hexahedron [-1,1]^3.
!> 8 vertex nodes + 12 edge midpoint nodes. No face or interior nodes.
!>
!> Node numbering (VTK QuadraticHexahedron convention):
!>   Vertices (1-8): standard hex ordering
!>     1(-1,-1,-1), 2(1,-1,-1), 3(1,1,-1), 4(-1,1,-1)
!>     5(-1,-1,1), 6(1,-1,1), 7(1,1,1), 8(-1,1,1)
!>   Edge midpoints (9-20):
!>     Bottom: 9(0,-1,-1), 10(1,0,-1), 11(0,1,-1), 12(-1,0,-1)
!>     Top:    13(0,-1,1), 14(1,0,1), 15(0,1,1), 16(-1,0,1)
!>     Vertical: 17(-1,-1,0), 18(1,-1,0), 19(1,1,0), 20(-1,1,0)
!>
submodule(domain_fe_volume) domain_fe_volume_hexa_second_serendipity
    implicit none

    ! Vertex reference coordinates (xi, eta, zeta) for nodes 1-8
    real(real64), parameter :: vx(8) = [-1.0d0,  1.0d0,  1.0d0, -1.0d0, -1.0d0,  1.0d0,  1.0d0, -1.0d0]
    real(real64), parameter :: vy(8) = [-1.0d0, -1.0d0,  1.0d0,  1.0d0, -1.0d0, -1.0d0,  1.0d0,  1.0d0]
    real(real64), parameter :: vz(8) = [-1.0d0, -1.0d0, -1.0d0, -1.0d0,  1.0d0,  1.0d0,  1.0d0,  1.0d0]

    ! Edge midpoint reference coordinates for nodes 9-20
    real(real64), parameter :: ex(12) = [ &
        0.0d0,  1.0d0, 0.0d0, -1.0d0,  0.0d0,  1.0d0, &
        0.0d0, -1.0d0, -1.0d0,  1.0d0,  1.0d0, -1.0d0]
    real(real64), parameter :: ey(12) = [ &
        -1.0d0, 0.0d0, 1.0d0,  0.0d0, -1.0d0,  0.0d0, &
        1.0d0,  0.0d0, -1.0d0, -1.0d0,  1.0d0,  1.0d0]
    real(real64), parameter :: ez(12) = [-1.0d0, -1.0d0, -1.0d0, -1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0]

    ! Which directions are "free" for each edge midpoint (1=xi varies, 2=eta varies, 3=zeta varies)
    ! For edge node e, the free direction index (1, 2, or 3)
    integer(int32), parameter :: efree(12) = [1, 2, 1, 2, 1, 2, 1, 2, 3, 3, 3, 3]

contains

    module function construct_hexa_second_serendipity(integration_order) result(fe)
        implicit none
        integer(int32), intent(in) :: integration_order
        class(abst_fe), allocatable :: fe

        character(len=*), parameter :: cell_name = "QuadraticHexahedron"
        integer(int32) :: vtk_type, num_nodes_info, dim_info, order_info

        allocate (type_hexa_second_serendipity :: fe)
        call vtk_constants%get_cell_info_from_cell_name(cell_name, vtk_type, num_nodes_info, dim_info, order_info)
        call fe%initialize(type=vtk_type, dimension=3, order=2, num_nodes=20, &
                           integration_order=integration_order)
    end function construct_hexa_second_serendipity

    module subroutine calc_volume_hexa_second_serendipity(self, node_coords, measure)
        implicit none
        class(type_hexa_second_serendipity), intent(in) :: self
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
    end subroutine calc_volume_hexa_second_serendipity

    !>
    !> 20-node Serendipity shape functions.
    !> Vertex nodes (i=1..8):
    !>   N_i = (1/8)(1+xi_i*xi)(1+eta_i*eta)(1+zeta_i*zeta)(xi_i*xi+eta_i*eta+zeta_i*zeta-2)
    !> Edge midpoint nodes (i=9..20):
    !>   N_i = (1/4)(1-t^2) * product of (1+coord_j*val_j) for fixed coords
    !>   where t is the free coordinate direction
    !>
    pure elemental module subroutine calc_psi_hexa_second_serendipity(self, i, r, psi_val)
        implicit none
        class(type_hexa_second_serendipity), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: psi_val

        real(real64) :: xi, eta, zeta, xi0, eta0, zeta0

        xi   = r%x
        eta  = r%y
        zeta = r%z

        if (i >= 1 .and. i <= 8) then
            ! Vertex node
            xi0   = vx(i)
            eta0  = vy(i)
            zeta0 = vz(i)
            psi_val = 0.125d0 * (1.0d0 + xi0*xi) * (1.0d0 + eta0*eta) * (1.0d0 + zeta0*zeta) &
                    * (xi0*xi + eta0*eta + zeta0*zeta - 2.0d0)
        else if (i >= 9 .and. i <= 20) then
            ! Edge midpoint node
            xi0   = ex(i - 8)
            eta0  = ey(i - 8)
            zeta0 = ez(i - 8)

            select case (efree(i - 8))
            case (1) ! xi is free (xi0=0)
                psi_val = 0.25d0 * (1.0d0 - xi**2) * (1.0d0 + eta0*eta) * (1.0d0 + zeta0*zeta)
            case (2) ! eta is free (eta0=0)
                psi_val = 0.25d0 * (1.0d0 + xi0*xi) * (1.0d0 - eta**2) * (1.0d0 + zeta0*zeta)
            case (3) ! zeta is free (zeta0=0)
                psi_val = 0.25d0 * (1.0d0 + xi0*xi) * (1.0d0 + eta0*eta) * (1.0d0 - zeta**2)
            case default
                psi_val = 0.0d0
            end select
        else
            psi_val = 0.0d0
        end if
    end subroutine calc_psi_hexa_second_serendipity

    !>
    !> Derivatives of 20-node Serendipity shape functions.
    !> j=1: d/dxi, j=2: d/deta, j=3: d/dzeta
    !>
    pure elemental module subroutine calc_dpsi_hexa_second_serendipity(self, i, j, r, dpsi_val)
        implicit none
        class(type_hexa_second_serendipity), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: dpsi_val

        real(real64) :: xi, eta, zeta, xi0, eta0, zeta0
        real(real64) :: a, b, c, s

        xi   = r%x
        eta  = r%y
        zeta = r%z

        if (i >= 1 .and. i <= 8) then
            ! Vertex node: N = (1/8)(1+xi0*xi)(1+eta0*eta)(1+zeta0*zeta)(xi0*xi+eta0*eta+zeta0*zeta-2)
            xi0   = vx(i)
            eta0  = vy(i)
            zeta0 = vz(i)
            a = 1.0d0 + xi0*xi
            b = 1.0d0 + eta0*eta
            c = 1.0d0 + zeta0*zeta
            s = xi0*xi + eta0*eta + zeta0*zeta - 2.0d0

            select case (j)
            case (1)
                dpsi_val = 0.125d0 * (xi0 * b * c * s + a * b * c * xi0)
            case (2)
                dpsi_val = 0.125d0 * (a * eta0 * c * s + a * b * c * eta0)
            case (3)
                dpsi_val = 0.125d0 * (a * b * zeta0 * s + a * b * c * zeta0)
            case default
                dpsi_val = 0.0d0
            end select
        else if (i >= 9 .and. i <= 20) then
            xi0   = ex(i - 8)
            eta0  = ey(i - 8)
            zeta0 = ez(i - 8)

            select case (efree(i - 8))
            case (1) ! xi is free: N = (1/4)(1-xi^2)(1+eta0*eta)(1+zeta0*zeta)
                select case (j)
                case (1)
                    dpsi_val = 0.25d0 * (-2.0d0*xi) * (1.0d0 + eta0*eta) * (1.0d0 + zeta0*zeta)
                case (2)
                    dpsi_val = 0.25d0 * (1.0d0 - xi**2) * eta0 * (1.0d0 + zeta0*zeta)
                case (3)
                    dpsi_val = 0.25d0 * (1.0d0 - xi**2) * (1.0d0 + eta0*eta) * zeta0
                case default
                    dpsi_val = 0.0d0
                end select
            case (2) ! eta is free: N = (1/4)(1+xi0*xi)(1-eta^2)(1+zeta0*zeta)
                select case (j)
                case (1)
                    dpsi_val = 0.25d0 * xi0 * (1.0d0 - eta**2) * (1.0d0 + zeta0*zeta)
                case (2)
                    dpsi_val = 0.25d0 * (1.0d0 + xi0*xi) * (-2.0d0*eta) * (1.0d0 + zeta0*zeta)
                case (3)
                    dpsi_val = 0.25d0 * (1.0d0 + xi0*xi) * (1.0d0 - eta**2) * zeta0
                case default
                    dpsi_val = 0.0d0
                end select
            case (3) ! zeta is free: N = (1/4)(1+xi0*xi)(1+eta0*eta)(1-zeta^2)
                select case (j)
                case (1)
                    dpsi_val = 0.25d0 * xi0 * (1.0d0 + eta0*eta) * (1.0d0 - zeta**2)
                case (2)
                    dpsi_val = 0.25d0 * (1.0d0 + xi0*xi) * eta0 * (1.0d0 - zeta**2)
                case (3)
                    dpsi_val = 0.25d0 * (1.0d0 + xi0*xi) * (1.0d0 + eta0*eta) * (-2.0d0*zeta)
                case default
                    dpsi_val = 0.0d0
                end select
            case default
                dpsi_val = 0.0d0
            end select
        else
            dpsi_val = 0.0d0
        end if
    end subroutine calc_dpsi_hexa_second_serendipity

    pure module subroutine calc_jacobian_hexa_second_serendipity(self, r, node_coords, jac)
        implicit none
        class(type_hexa_second_serendipity), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: jac(:, :)

        integer(int32) :: k
        real(real64) :: dpsi_xi, dpsi_eta, dpsi_zeta
        real(real64) :: xk, yk, zk

        jac = 0.0d0
        do k = 1, 20
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
    end subroutine calc_jacobian_hexa_second_serendipity

    module subroutine is_in_hexa_second_serendipity(self, cartesian, normalized, node_coords, is_in)
        implicit none
        class(type_hexa_second_serendipity), intent(in) :: self
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
    end subroutine is_in_hexa_second_serendipity

end submodule domain_fe_volume_hexa_second_serendipity

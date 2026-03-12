!>
!> Implements the procedures for the third-order hexahedron (64-node) finite element.
!> Uses tensor-product Lagrange Q3 shape functions on [-1,1]^3.
!>
!> Node numbering (VTK LagrangeHexahedron convention):
!>   Vertices (1-8): standard hexahedron ordering
!>   Edge nodes (9-32): 2 interior nodes per edge, 12 edges
!>   Face nodes (33-56): 4 interior nodes per face, 6 faces
!>   Interior nodes (57-64): 8 interior nodes
!>
!> Tensor-product indices (ix, iy, iz) in {1,2,3,4} correspond to
!> 1D equispaced positions {-1, -1/3, 1/3, 1} on the reference domain [-1,1].
!>
submodule(domain_fe_volume) domain_fe_volume_hexa_third
    implicit none

    ! 1D equispaced node positions for cubic Lagrange on [-1,1]
    real(real64), parameter :: nd(4) = [-1.0d0, -1.0d0/3.0d0, 1.0d0/3.0d0, 1.0d0]

contains

    ! ==================================================================================
    !   1D Lagrange helpers
    ! ==================================================================================

    !> 1D cubic Lagrange basis function L_a(t) through nodes nd(1..4)
    pure function lagrange_1d_q3(a, t) result(val)
        implicit none
        integer(int32), intent(in) :: a
        real(real64), intent(in) :: t
        real(real64) :: val
        integer(int32) :: b

        val = 1.0d0
        do b = 1, 4
            if (b /= a) then
                val = val * (t - nd(b)) / (nd(a) - nd(b))
            end if
        end do
    end function lagrange_1d_q3

    !> Derivative of 1D cubic Lagrange basis function dL_a/dt
    pure function dlagrange_1d_q3(a, t) result(val)
        implicit none
        integer(int32), intent(in) :: a
        real(real64), intent(in) :: t
        real(real64) :: val
        integer(int32) :: b, c
        real(real64) :: prod

        val = 0.0d0
        do b = 1, 4
            if (b /= a) then
                prod = 1.0d0 / (nd(a) - nd(b))
                do c = 1, 4
                    if (c /= a .and. c /= b) then
                        prod = prod * (t - nd(c)) / (nd(a) - nd(c))
                    end if
                end do
                val = val + prod
            end if
        end do
    end function dlagrange_1d_q3

    ! ==================================================================================
    !   VTK node index to tensor-product index mapping
    ! ==================================================================================

    !> Map VTK LagrangeHexahedron node index (1..64) to tensor-product indices
    !> (ix, iy, iz) each in {1,2,3,4}, corresponding to positions {-1, -1/3, 1/3, 1}.
    pure subroutine node_to_ijk_hexa64(node, ix, iy, iz)
        implicit none
        integer(int32), intent(in) :: node
        integer(int32), intent(inout) :: ix, iy, iz

        select case (node)
        ! ------------------------------------------------------------------
        ! Vertices (1-8)
        ! ------------------------------------------------------------------
        case (1);  ix = 1; iy = 1; iz = 1   ! (-1,-1,-1)
        case (2);  ix = 4; iy = 1; iz = 1   ! ( 1,-1,-1)
        case (3);  ix = 4; iy = 4; iz = 1   ! ( 1, 1,-1)
        case (4);  ix = 1; iy = 4; iz = 1   ! (-1, 1,-1)
        case (5);  ix = 1; iy = 1; iz = 4   ! (-1,-1, 1)
        case (6);  ix = 4; iy = 1; iz = 4   ! ( 1,-1, 1)
        case (7);  ix = 4; iy = 4; iz = 4   ! ( 1, 1, 1)
        case (8);  ix = 1; iy = 4; iz = 4   ! (-1, 1, 1)

        ! ------------------------------------------------------------------
        ! Edge nodes (9-32): 2 interior nodes per edge, 12 edges
        ! ------------------------------------------------------------------
        ! Edge 1-2 (y=-1, z=-1, x varies)
        case (9);  ix = 2; iy = 1; iz = 1
        case (10); ix = 3; iy = 1; iz = 1
        ! Edge 2-3 (x=+1, z=-1, y varies)
        case (11); ix = 4; iy = 2; iz = 1
        case (12); ix = 4; iy = 3; iz = 1
        ! Edge 3-4 (y=+1, z=-1, x varies, reversed)
        case (13); ix = 3; iy = 4; iz = 1
        case (14); ix = 2; iy = 4; iz = 1
        ! Edge 1-4 (x=-1, z=-1, y varies)
        case (15); ix = 1; iy = 2; iz = 1
        case (16); ix = 1; iy = 3; iz = 1
        ! Edge 5-6 (y=-1, z=+1, x varies)
        case (17); ix = 2; iy = 1; iz = 4
        case (18); ix = 3; iy = 1; iz = 4
        ! Edge 6-7 (x=+1, z=+1, y varies)
        case (19); ix = 4; iy = 2; iz = 4
        case (20); ix = 4; iy = 3; iz = 4
        ! Edge 7-8 (y=+1, z=+1, x varies, reversed)
        case (21); ix = 3; iy = 4; iz = 4
        case (22); ix = 2; iy = 4; iz = 4
        ! Edge 5-8 (x=-1, z=+1, y varies)
        case (23); ix = 1; iy = 2; iz = 4
        case (24); ix = 1; iy = 3; iz = 4
        ! Edge 1-5 (x=-1, y=-1, z varies)
        case (25); ix = 1; iy = 1; iz = 2
        case (26); ix = 1; iy = 1; iz = 3
        ! Edge 2-6 (x=+1, y=-1, z varies)
        case (27); ix = 4; iy = 1; iz = 2
        case (28); ix = 4; iy = 1; iz = 3
        ! Edge 3-7 (x=+1, y=+1, z varies)
        case (29); ix = 4; iy = 4; iz = 2
        case (30); ix = 4; iy = 4; iz = 3
        ! Edge 4-8 (x=-1, y=+1, z varies)
        case (31); ix = 1; iy = 4; iz = 2
        case (32); ix = 1; iy = 4; iz = 3

        ! ------------------------------------------------------------------
        ! Face nodes (33-56): 4 interior nodes per face, 6 faces
        ! ------------------------------------------------------------------
        ! Face z=-1 (bottom face)
        case (33); ix = 2; iy = 2; iz = 1
        case (34); ix = 3; iy = 2; iz = 1
        case (35); ix = 3; iy = 3; iz = 1
        case (36); ix = 2; iy = 3; iz = 1
        ! Face z=+1 (top face)
        case (37); ix = 2; iy = 2; iz = 4
        case (38); ix = 3; iy = 2; iz = 4
        case (39); ix = 3; iy = 3; iz = 4
        case (40); ix = 2; iy = 3; iz = 4
        ! Face y=-1 (front face)
        case (41); ix = 2; iy = 1; iz = 2
        case (42); ix = 3; iy = 1; iz = 2
        case (43); ix = 3; iy = 1; iz = 3
        case (44); ix = 2; iy = 1; iz = 3
        ! Face x=+1 (right face)
        case (45); ix = 4; iy = 2; iz = 2
        case (46); ix = 4; iy = 3; iz = 2
        case (47); ix = 4; iy = 3; iz = 3
        case (48); ix = 4; iy = 2; iz = 3
        ! Face y=+1 (back face)
        case (49); ix = 3; iy = 4; iz = 2
        case (50); ix = 2; iy = 4; iz = 2
        case (51); ix = 2; iy = 4; iz = 3
        case (52); ix = 3; iy = 4; iz = 3
        ! Face x=-1 (left face)
        case (53); ix = 1; iy = 3; iz = 2
        case (54); ix = 1; iy = 2; iz = 2
        case (55); ix = 1; iy = 2; iz = 3
        case (56); ix = 1; iy = 3; iz = 3

        ! ------------------------------------------------------------------
        ! Interior nodes (57-64)
        ! ------------------------------------------------------------------
        case (57); ix = 2; iy = 2; iz = 2
        case (58); ix = 3; iy = 2; iz = 2
        case (59); ix = 3; iy = 3; iz = 2
        case (60); ix = 2; iy = 3; iz = 2
        case (61); ix = 2; iy = 2; iz = 3
        case (62); ix = 3; iy = 2; iz = 3
        case (63); ix = 3; iy = 3; iz = 3
        case (64); ix = 2; iy = 3; iz = 3

        case default
            ix = 1; iy = 1; iz = 1
        end select
    end subroutine node_to_ijk_hexa64

    ! ==================================================================================
    !   Constructor
    ! ==================================================================================

    module function construct_hexa_third(integration_order) result(fe)
        implicit none
        integer(int32), intent(in) :: integration_order
        class(abst_fe), allocatable :: fe

        character(len=*), parameter :: cell_name = "LagrangeHexahedron"
        integer(int32) :: vtk_type
        integer(int32) :: num_nodes_info, dim_info, order_info

        allocate (type_hexa_third :: fe)

        call vtk_constants%get_cell_info_from_cell_name(cell_name, vtk_type, num_nodes_info, dim_info, order_info)

        call fe%initialize(type=vtk_type, dimension=3, order=3, num_nodes=64, &
                           integration_order=integration_order)

    end function construct_hexa_third

    ! ==================================================================================
    !   Volume (measure) via Gauss quadrature
    ! ==================================================================================

    module subroutine calc_volume_hexa_third(self, node_coords, measure)
        implicit none
        class(type_hexa_third), intent(in) :: self
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
    end subroutine calc_volume_hexa_third

    ! ==================================================================================
    !   Shape functions
    ! ==================================================================================

    !> Shape function N_i(r) = L_ix(xi) * L_iy(eta) * L_iz(zeta)
    pure elemental module subroutine calc_psi_hexa_third(self, i, r, psi_val)
        implicit none
        class(type_hexa_third), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: psi_val

        integer(int32) :: ix, iy, iz

        ix = 0; iy = 0; iz = 0
        call node_to_ijk_hexa64(i, ix, iy, iz)
        psi_val = lagrange_1d_q3(ix, r%x) * lagrange_1d_q3(iy, r%y) * lagrange_1d_q3(iz, r%z)
    end subroutine calc_psi_hexa_third

    ! ==================================================================================
    !   Shape function derivatives
    ! ==================================================================================

    !> Derivative of shape function: dN_i/dxi_j
    !> j=1: d/dxi, j=2: d/deta, j=3: d/dzeta
    !> Uses product rule on the tensor-product form.
    pure elemental module subroutine calc_dpsi_hexa_third(self, i, j, r, dpsi_val)
        implicit none
        class(type_hexa_third), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: dpsi_val

        integer(int32) :: ix, iy, iz

        ix = 0; iy = 0; iz = 0
        call node_to_ijk_hexa64(i, ix, iy, iz)

        select case (j)
        case (1) ! d/dxi: differentiate in x, keep y and z
            dpsi_val = dlagrange_1d_q3(ix, r%x) * lagrange_1d_q3(iy, r%y) * lagrange_1d_q3(iz, r%z)
        case (2) ! d/deta: differentiate in y, keep x and z
            dpsi_val = lagrange_1d_q3(ix, r%x) * dlagrange_1d_q3(iy, r%y) * lagrange_1d_q3(iz, r%z)
        case (3) ! d/dzeta: differentiate in z, keep x and y
            dpsi_val = lagrange_1d_q3(ix, r%x) * lagrange_1d_q3(iy, r%y) * dlagrange_1d_q3(iz, r%z)
        case default
            dpsi_val = 0.0d0
        end select
    end subroutine calc_dpsi_hexa_third

    ! ==================================================================================
    !   Jacobian matrix (3x3)
    ! ==================================================================================

    !> Jacobian matrix J(i,j) = dx_i / dxi_j
    !> Row 1: dx/dxi,   dx/deta,   dx/dzeta
    !> Row 2: dy/dxi,   dy/deta,   dy/dzeta
    !> Row 3: dz/dxi,   dz/deta,   dz/dzeta
    pure module subroutine calc_jacobian_hexa_third(self, r, node_coords, jac)
        implicit none
        class(type_hexa_third), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: jac(:, :)

        integer(int32) :: k
        real(real64) :: dpsi_xi, dpsi_eta, dpsi_zeta
        real(real64) :: xk, yk, zk

        jac = 0.0d0
        do k = 1, 64
            call self%calc_dpsi(k, 1, r, dpsi_xi)
            call self%calc_dpsi(k, 2, r, dpsi_eta)
            call self%calc_dpsi(k, 3, r, dpsi_zeta)
            xk = node_coords(1, k)
            yk = node_coords(2, k)
            zk = node_coords(3, k)

            ! Row 1: x-coordinate derivatives
            jac(1, 1) = jac(1, 1) + dpsi_xi * xk    ! dx/dxi
            jac(1, 2) = jac(1, 2) + dpsi_eta * xk   ! dx/deta
            jac(1, 3) = jac(1, 3) + dpsi_zeta * xk  ! dx/dzeta

            ! Row 2: y-coordinate derivatives
            jac(2, 1) = jac(2, 1) + dpsi_xi * yk    ! dy/dxi
            jac(2, 2) = jac(2, 2) + dpsi_eta * yk   ! dy/deta
            jac(2, 3) = jac(2, 3) + dpsi_zeta * yk  ! dy/dzeta

            ! Row 3: z-coordinate derivatives
            jac(3, 1) = jac(3, 1) + dpsi_xi * zk    ! dz/dxi
            jac(3, 2) = jac(3, 2) + dpsi_eta * zk   ! dz/deta
            jac(3, 3) = jac(3, 3) + dpsi_zeta * zk  ! dz/dzeta
        end do
    end subroutine calc_jacobian_hexa_third

    ! ==================================================================================
    !   Point inclusion test via Newton-Raphson
    ! ==================================================================================

    !> Determines whether a physical point lies inside the element by inverting
    !> the isoparametric mapping x(xi) using Newton-Raphson iteration with
    !> analytical 3x3 Jacobian inversion (cofactor/adjugate method).
    module subroutine is_in_hexa_third(self, cartesian, normalized, node_coords, is_in)
        implicit none
        class(type_hexa_third), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: cartesian
        type(type_coordinate_dp), intent(inout) :: normalized
        real(real64), intent(in) :: node_coords(:, :)
        logical, intent(inout) :: is_in

        type(type_coordinate_dp) :: r_loc, pos
        real(real64) :: jac(3, 3)
        real(real64) :: dx, dy, dz, det_j, inv_det
        real(real64) :: c11, c12, c13, c21, c22, c23, c31, c32, c33
        real(real64) :: psi_val, res
        integer(int32) :: iter, k, nn
        real(real64), parameter :: tol = 1.0d-9
        real(real64), parameter :: inside_tol = 1.0d-4
        integer(int32), parameter :: max_iter = 30

        call r_loc%set(0.0d0, 0.0d0, 0.0d0)
        call self%get_num_nodes(nn)
        is_in = .false.

        do iter = 1, max_iter
            ! Evaluate the isoparametric mapping x(xi)
            call pos%set(0.0d0, 0.0d0, 0.0d0)
            do k = 1, nn
                call self%calc_psi(k, r_loc, psi_val)
                pos%x = pos%x + psi_val * node_coords(1, k)
                pos%y = pos%y + psi_val * node_coords(2, k)
                pos%z = pos%z + psi_val * node_coords(3, k)
            end do

            ! Residual
            dx = cartesian%x - pos%x
            dy = cartesian%y - pos%y
            dz = cartesian%z - pos%z
            res = sqrt(dx**2 + dy**2 + dz**2)

            ! Check convergence
            if (res < tol) then
                if (abs(r_loc%x) <= 1.0d0 + inside_tol .and. &
                    abs(r_loc%y) <= 1.0d0 + inside_tol .and. &
                    abs(r_loc%z) <= 1.0d0 + inside_tol) then
                    is_in = .true.
                    normalized = r_loc
                end if
                return
            end if

            ! Compute Jacobian at current reference point
            call self%calc_jacobian(r_loc, node_coords, jac)

            ! Determinant of 3x3 Jacobian
            det_j = jac(1,1) * (jac(2,2)*jac(3,3) - jac(2,3)*jac(3,2)) &
                  - jac(1,2) * (jac(2,1)*jac(3,3) - jac(2,3)*jac(3,1)) &
                  + jac(1,3) * (jac(2,1)*jac(3,2) - jac(2,2)*jac(3,1))

            if (abs(det_j) < 1.0d-12) return

            inv_det = 1.0d0 / det_j

            ! Cofactors of J (used to form adjugate = transpose of cofactor matrix)
            c11 =  (jac(2,2)*jac(3,3) - jac(2,3)*jac(3,2))
            c12 = -(jac(2,1)*jac(3,3) - jac(2,3)*jac(3,1))
            c13 =  (jac(2,1)*jac(3,2) - jac(2,2)*jac(3,1))
            c21 = -(jac(1,2)*jac(3,3) - jac(1,3)*jac(3,2))
            c22 =  (jac(1,1)*jac(3,3) - jac(1,3)*jac(3,1))
            c23 = -(jac(1,1)*jac(3,2) - jac(1,2)*jac(3,1))
            c31 =  (jac(1,2)*jac(2,3) - jac(1,3)*jac(2,2))
            c32 = -(jac(1,1)*jac(2,3) - jac(1,3)*jac(2,1))
            c33 =  (jac(1,1)*jac(2,2) - jac(1,2)*jac(2,1))

            ! Newton update: delta_xi = J^{-1} * delta_x
            ! J^{-1}(j,i) = adj(J)(j,i) / det = C(i,j) / det
            ! adj(J) = transpose of cofactor matrix
            r_loc%x = r_loc%x + (c11 * dx + c21 * dy + c31 * dz) * inv_det
            r_loc%y = r_loc%y + (c12 * dx + c22 * dy + c32 * dz) * inv_det
            r_loc%z = r_loc%z + (c13 * dx + c23 * dy + c33 * dz) * inv_det

            ! Divergence check
            if (abs(r_loc%x) > 3.0d0 .or. abs(r_loc%y) > 3.0d0 .or. abs(r_loc%z) > 3.0d0) return
        end do
    end subroutine is_in_hexa_third

end submodule domain_fe_volume_hexa_third

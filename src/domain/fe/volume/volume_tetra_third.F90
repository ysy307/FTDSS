!>
!> Implements the procedures for the third-order tetrahedral (20-node) finite element.
!> P3 Lagrange shape functions on reference tetrahedron (0,0,0)-(1,0,0)-(0,1,0)-(0,0,1).
!>
!> 20 nodes: 4 vertices, 12 edge nodes (2 per edge), 4 face nodes (1 per face)
!>
submodule(domain_fe_volume) domain_fe_volume_tetra_third
    implicit none

contains

    module function construct_tetra_third(integration_order) result(fe)
        implicit none
        integer(int32), intent(in) :: integration_order
        class(abst_fe), allocatable :: fe

        allocate (type_tetra_third :: fe)
        call fe%initialize(type=FE_TYPE%LAGRANGE_TETRAHEDRON%ID, dimension=3, order=3, num_nodes=20, &
                           integration_order=integration_order)
    end function construct_tetra_third

    module subroutine calc_volume_tetra_third(self, node_coords, measure)
        implicit none
        class(type_tetra_third), intent(in) :: self
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
    end subroutine calc_volume_tetra_third

    ! L1=1-xi-eta-zeta, L2=xi, L3=eta, L4=zeta
    ! Vertex: N = (1/2)*L*(3L-1)*(3L-2)
    ! Edge (near a): N = (9/2)*La*Lb*(3La-1)
    ! Edge (near b): N = (9/2)*La*Lb*(3Lb-1)
    ! Face centroid: N = 27*La*Lb*Lc
    pure elemental module subroutine calc_psi_tetra_third(self, i, r, psi_val)
        implicit none
        class(type_tetra_third), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: psi_val

        real(real64) :: L1, L2, L3, L4

        L1 = 1.0d0 - r%x - r%y - r%z
        L2 = r%x
        L3 = r%y
        L4 = r%z

        select case (i)
        ! Vertices
        case (1); psi_val = 0.5d0*L1*(3.0d0*L1 - 1.0d0)*(3.0d0*L1 - 2.0d0)
        case (2); psi_val = 0.5d0*L2*(3.0d0*L2 - 1.0d0)*(3.0d0*L2 - 2.0d0)
        case (3); psi_val = 0.5d0*L3*(3.0d0*L3 - 1.0d0)*(3.0d0*L3 - 2.0d0)
        case (4); psi_val = 0.5d0*L4*(3.0d0*L4 - 1.0d0)*(3.0d0*L4 - 2.0d0)
        ! Edge 1-2
        case (5);  psi_val = 4.5d0*L1*L2*(3.0d0*L1 - 1.0d0)
        case (6);  psi_val = 4.5d0*L1*L2*(3.0d0*L2 - 1.0d0)
        ! Edge 2-3
        case (7);  psi_val = 4.5d0*L2*L3*(3.0d0*L2 - 1.0d0)
        case (8);  psi_val = 4.5d0*L2*L3*(3.0d0*L3 - 1.0d0)
        ! Edge 1-3
        case (9);  psi_val = 4.5d0*L1*L3*(3.0d0*L1 - 1.0d0)
        case (10); psi_val = 4.5d0*L1*L3*(3.0d0*L3 - 1.0d0)
        ! Edge 1-4
        case (11); psi_val = 4.5d0*L1*L4*(3.0d0*L1 - 1.0d0)
        case (12); psi_val = 4.5d0*L1*L4*(3.0d0*L4 - 1.0d0)
        ! Edge 2-4
        case (13); psi_val = 4.5d0*L2*L4*(3.0d0*L2 - 1.0d0)
        case (14); psi_val = 4.5d0*L2*L4*(3.0d0*L4 - 1.0d0)
        ! Edge 3-4
        case (15); psi_val = 4.5d0*L3*L4*(3.0d0*L3 - 1.0d0)
        case (16); psi_val = 4.5d0*L3*L4*(3.0d0*L4 - 1.0d0)
        ! Face nodes
        case (17); psi_val = 27.0d0*L1*L2*L3  ! Face 1-2-3
        case (18); psi_val = 27.0d0*L1*L2*L4  ! Face 1-2-4
        case (19); psi_val = 27.0d0*L2*L3*L4  ! Face 2-3-4
        case (20); psi_val = 27.0d0*L1*L3*L4  ! Face 1-3-4
        case default; psi_val = 0.0d0
        end select
    end subroutine calc_psi_tetra_third

    pure elemental module subroutine calc_dpsi_tetra_third(self, i, j, r, dpsi_val)
        implicit none
        class(type_tetra_third), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: dpsi_val

        real(real64) :: L1, L2, L3, L4
        real(real64) :: dL1, dL2, dL3, dL4

        L1 = 1.0d0 - r%x - r%y - r%z
        L2 = r%x; L3 = r%y; L4 = r%z

        dL1 = -1.0d0; dL2 = 0.0d0; dL3 = 0.0d0; dL4 = 0.0d0
        if (j == 1) dL2 = 1.0d0
        if (j == 2) dL3 = 1.0d0
        if (j == 3) dL4 = 1.0d0

        dpsi_val = 0.0d0

        select case (i)
        ! Vertices: d/dt [0.5*L*(3L-1)*(3L-2)]
        case (1)
            dpsi_val = 0.5d0*dL1*((3.0d0*L1-1.0d0)*(3.0d0*L1-2.0d0) &
                     + L1*3.0d0*(3.0d0*L1-2.0d0) + L1*(3.0d0*L1-1.0d0)*3.0d0)
        case (2)
            dpsi_val = 0.5d0*dL2*((3.0d0*L2-1.0d0)*(3.0d0*L2-2.0d0) &
                     + L2*3.0d0*(3.0d0*L2-2.0d0) + L2*(3.0d0*L2-1.0d0)*3.0d0)
        case (3)
            dpsi_val = 0.5d0*dL3*((3.0d0*L3-1.0d0)*(3.0d0*L3-2.0d0) &
                     + L3*3.0d0*(3.0d0*L3-2.0d0) + L3*(3.0d0*L3-1.0d0)*3.0d0)
        case (4)
            dpsi_val = 0.5d0*dL4*((3.0d0*L4-1.0d0)*(3.0d0*L4-2.0d0) &
                     + L4*3.0d0*(3.0d0*L4-2.0d0) + L4*(3.0d0*L4-1.0d0)*3.0d0)
        ! Edge nodes: d/dt [4.5*La*Lb*(3La-1)]
        case (5)
            dpsi_val = 4.5d0*(dL1*L2*(3.0d0*L1-1.0d0) + L1*dL2*(3.0d0*L1-1.0d0) + L1*L2*3.0d0*dL1)
        case (6)
            dpsi_val = 4.5d0*(dL1*L2*(3.0d0*L2-1.0d0) + L1*dL2*(3.0d0*L2-1.0d0) + L1*L2*3.0d0*dL2)
        case (7)
            dpsi_val = 4.5d0*(dL2*L3*(3.0d0*L2-1.0d0) + L2*dL3*(3.0d0*L2-1.0d0) + L2*L3*3.0d0*dL2)
        case (8)
            dpsi_val = 4.5d0*(dL2*L3*(3.0d0*L3-1.0d0) + L2*dL3*(3.0d0*L3-1.0d0) + L2*L3*3.0d0*dL3)
        case (9)
            dpsi_val = 4.5d0*(dL1*L3*(3.0d0*L1-1.0d0) + L1*dL3*(3.0d0*L1-1.0d0) + L1*L3*3.0d0*dL1)
        case (10)
            dpsi_val = 4.5d0*(dL1*L3*(3.0d0*L3-1.0d0) + L1*dL3*(3.0d0*L3-1.0d0) + L1*L3*3.0d0*dL3)
        case (11)
            dpsi_val = 4.5d0*(dL1*L4*(3.0d0*L1-1.0d0) + L1*dL4*(3.0d0*L1-1.0d0) + L1*L4*3.0d0*dL1)
        case (12)
            dpsi_val = 4.5d0*(dL1*L4*(3.0d0*L4-1.0d0) + L1*dL4*(3.0d0*L4-1.0d0) + L1*L4*3.0d0*dL4)
        case (13)
            dpsi_val = 4.5d0*(dL2*L4*(3.0d0*L2-1.0d0) + L2*dL4*(3.0d0*L2-1.0d0) + L2*L4*3.0d0*dL2)
        case (14)
            dpsi_val = 4.5d0*(dL2*L4*(3.0d0*L4-1.0d0) + L2*dL4*(3.0d0*L4-1.0d0) + L2*L4*3.0d0*dL4)
        case (15)
            dpsi_val = 4.5d0*(dL3*L4*(3.0d0*L3-1.0d0) + L3*dL4*(3.0d0*L3-1.0d0) + L3*L4*3.0d0*dL3)
        case (16)
            dpsi_val = 4.5d0*(dL3*L4*(3.0d0*L4-1.0d0) + L3*dL4*(3.0d0*L4-1.0d0) + L3*L4*3.0d0*dL4)
        ! Face nodes: d/dt [27*La*Lb*Lc]
        case (17)
            dpsi_val = 27.0d0*(dL1*L2*L3 + L1*dL2*L3 + L1*L2*dL3)
        case (18)
            dpsi_val = 27.0d0*(dL1*L2*L4 + L1*dL2*L4 + L1*L2*dL4)
        case (19)
            dpsi_val = 27.0d0*(dL2*L3*L4 + L2*dL3*L4 + L2*L3*dL4)
        case (20)
            dpsi_val = 27.0d0*(dL1*L3*L4 + L1*dL3*L4 + L1*L3*dL4)
        end select
    end subroutine calc_dpsi_tetra_third

    pure module subroutine calc_jacobian_tetra_third(self, r, node_coords, jac)
        implicit none
        class(type_tetra_third), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: jac(:, :)

        integer(int32) :: k, d
        real(real64) :: dpsi_val

        jac = 0.0d0
        do k = 1, 20
            do d = 1, 3
                call self%calc_dpsi(k, d, r, dpsi_val)
                jac(1, d) = jac(1, d) + dpsi_val * node_coords(1, k)
                jac(2, d) = jac(2, d) + dpsi_val * node_coords(2, k)
                jac(3, d) = jac(3, d) + dpsi_val * node_coords(3, k)
            end do
        end do
    end subroutine calc_jacobian_tetra_third

    module subroutine is_in_tetra_third(self, cartesian, normalized, node_coords, is_in)
        implicit none
        class(type_tetra_third), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: cartesian
        type(type_coordinate_dp), intent(inout) :: normalized
        real(real64), intent(in) :: node_coords(:, :)
        logical, intent(inout) :: is_in

        type(type_coordinate_dp) :: r_loc, pos
        real(real64) :: jac(3,3), dx, dy, dz, det_j, psi_val
        real(real64) :: c11, c12, c13, c21, c22, c23, c31, c32, c33, inv_det
        integer(int32) :: iter, k, nn
        real(real64), parameter :: tol = 1.0e-9
        integer(int32), parameter :: max_iter = 30

        call r_loc%set(0.25d0, 0.25d0, 0.25d0)
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
                if ((r_loc%x >= -tol) .and. (r_loc%y >= -tol) .and. (r_loc%z >= -tol) &
                    .and. (r_loc%x + r_loc%y + r_loc%z <= 1.0d0 + tol)) then
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
            c11 = (jac(2,2)*jac(3,3) - jac(2,3)*jac(3,2)) * inv_det
            c12 = (jac(1,3)*jac(3,2) - jac(1,2)*jac(3,3)) * inv_det
            c13 = (jac(1,2)*jac(2,3) - jac(1,3)*jac(2,2)) * inv_det
            c21 = (jac(2,3)*jac(3,1) - jac(2,1)*jac(3,3)) * inv_det
            c22 = (jac(1,1)*jac(3,3) - jac(1,3)*jac(3,1)) * inv_det
            c23 = (jac(1,3)*jac(2,1) - jac(1,1)*jac(2,3)) * inv_det
            c31 = (jac(2,1)*jac(3,2) - jac(2,2)*jac(3,1)) * inv_det
            c32 = (jac(1,2)*jac(3,1) - jac(1,1)*jac(3,2)) * inv_det
            c33 = (jac(1,1)*jac(2,2) - jac(1,2)*jac(2,1)) * inv_det

            r_loc%x = r_loc%x + c11*dx + c12*dy + c13*dz
            r_loc%y = r_loc%y + c21*dx + c22*dy + c23*dz
            r_loc%z = r_loc%z + c31*dx + c32*dy + c33*dz

            if (abs(r_loc%x) > 3.0d0 .or. abs(r_loc%y) > 3.0d0 .or. abs(r_loc%z) > 3.0d0) return
        end do
    end subroutine is_in_tetra_third

end submodule domain_fe_volume_tetra_third

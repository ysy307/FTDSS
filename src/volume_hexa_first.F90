!>
!> Implements the procedures for the first-order hexahedral (8-node) finite element.
!> Trilinear shape functions on reference hexahedron [-1,1]^3.
!>
!> Node numbering (VTK Hexahedron convention):
!>   1(-1,-1,-1), 2(1,-1,-1), 3(1,1,-1), 4(-1,1,-1),
!>   5(-1,-1,1),  6(1,-1,1),  7(1,1,1),  8(-1,1,1)
!>
submodule(domain_fe_volume) domain_fe_volume_hexa_first
    implicit none

    ! Vertex coordinates for each of the 8 nodes
    real(real64), parameter :: xi_n(8)  = [-1, 1, 1,-1,-1, 1, 1,-1]
    real(real64), parameter :: eta_n(8) = [-1,-1, 1, 1,-1,-1, 1, 1]
    real(real64), parameter :: ze_n(8)  = [-1,-1,-1,-1, 1, 1, 1, 1]

contains

    module function construct_hexa_first(integration_order) result(fe)
        implicit none
        integer(int32), intent(in) :: integration_order
        class(abst_fe), allocatable :: fe

        allocate (type_hexa_first :: fe)
        call fe%initialize(type=FE_TYPE%HEXAHEDRON%ID, dimension=3, order=1, num_nodes=8, &
                           integration_order=integration_order)
    end function construct_hexa_first

    module subroutine calc_volume_hexa_first(self, node_coords, measure)
        implicit none
        class(type_hexa_first), intent(in) :: self
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
    end subroutine calc_volume_hexa_first

    ! N_i = (1/8)(1 + xi_i*xi)(1 + eta_i*eta)(1 + zeta_i*zeta)
    pure elemental module subroutine calc_psi_hexa_first(self, i, r, psi_val)
        implicit none
        class(type_hexa_first), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: psi_val

        psi_val = 0.125d0 * (1.0d0 + xi_n(i)*r%x) * (1.0d0 + eta_n(i)*r%y) * (1.0d0 + ze_n(i)*r%z)
    end subroutine calc_psi_hexa_first

    pure elemental module subroutine calc_dpsi_hexa_first(self, i, j, r, dpsi_val)
        implicit none
        class(type_hexa_first), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: dpsi_val

        select case (j)
        case (1) ! d/dxi
            dpsi_val = 0.125d0 * xi_n(i) * (1.0d0 + eta_n(i)*r%y) * (1.0d0 + ze_n(i)*r%z)
        case (2) ! d/deta
            dpsi_val = 0.125d0 * (1.0d0 + xi_n(i)*r%x) * eta_n(i) * (1.0d0 + ze_n(i)*r%z)
        case (3) ! d/dzeta
            dpsi_val = 0.125d0 * (1.0d0 + xi_n(i)*r%x) * (1.0d0 + eta_n(i)*r%y) * ze_n(i)
        case default
            dpsi_val = 0.0d0
        end select
    end subroutine calc_dpsi_hexa_first

    pure module subroutine calc_jacobian_hexa_first(self, r, node_coords, jac)
        implicit none
        class(type_hexa_first), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: jac(:, :)

        integer(int32) :: k
        real(real64) :: dpsi_xi, dpsi_eta, dpsi_zeta

        jac = 0.0d0
        do k = 1, 8
            call self%calc_dpsi(k, 1, r, dpsi_xi)
            call self%calc_dpsi(k, 2, r, dpsi_eta)
            call self%calc_dpsi(k, 3, r, dpsi_zeta)

            jac(1, 1) = jac(1, 1) + dpsi_xi * node_coords(1, k)
            jac(1, 2) = jac(1, 2) + dpsi_eta * node_coords(1, k)
            jac(1, 3) = jac(1, 3) + dpsi_zeta * node_coords(1, k)

            jac(2, 1) = jac(2, 1) + dpsi_xi * node_coords(2, k)
            jac(2, 2) = jac(2, 2) + dpsi_eta * node_coords(2, k)
            jac(2, 3) = jac(2, 3) + dpsi_zeta * node_coords(2, k)

            jac(3, 1) = jac(3, 1) + dpsi_xi * node_coords(3, k)
            jac(3, 2) = jac(3, 2) + dpsi_eta * node_coords(3, k)
            jac(3, 3) = jac(3, 3) + dpsi_zeta * node_coords(3, k)
        end do
    end subroutine calc_jacobian_hexa_first

    module subroutine is_in_hexa_first(self, cartesian, normalized, node_coords, is_in)
        implicit none
        class(type_hexa_first), intent(in) :: self
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
    end subroutine is_in_hexa_first

end submodule domain_fe_volume_hexa_first

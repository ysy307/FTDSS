!>
!> Implements the procedures for the first-order tetrahedral (4-node) finite element.
!>
submodule(domain_fe_volume) domain_fe_volume_tetra_first
    implicit none

contains

    module function construct_tetra_first(integration_order) result(fe)
        implicit none
        integer(int32), intent(in) :: integration_order
        class(abst_fe), allocatable :: fe

        allocate (type_tetra_first :: fe)
        call fe%initialize(type=FE_TYPE%TETRA%ID, dimension=3, order=1, num_nodes=4, &
                           integration_order=integration_order)
    end function construct_tetra_first

    module subroutine calc_volume_tetra_first(self, node_coords, measure)
        implicit none
        class(type_tetra_first), intent(in) :: self
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: measure

        real(real64) :: a(3), b(3), c(3), det_j
        integer(int32) :: d

        do d = 1, 3
            a(d) = node_coords(d, 2) - node_coords(d, 1)
            b(d) = node_coords(d, 3) - node_coords(d, 1)
            c(d) = node_coords(d, 4) - node_coords(d, 1)
        end do

        det_j = a(1)*(b(2)*c(3) - b(3)*c(2)) &
              - a(2)*(b(1)*c(3) - b(3)*c(1)) &
              + a(3)*(b(1)*c(2) - b(2)*c(1))

        measure = abs(det_j) / 6.0d0
    end subroutine calc_volume_tetra_first

    pure elemental module subroutine calc_psi_tetra_first(self, i, r, psi_val)
        implicit none
        class(type_tetra_first), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: psi_val

        select case (i)
        case (1); psi_val = 1.0d0 - r%x - r%y - r%z
        case (2); psi_val = r%x
        case (3); psi_val = r%y
        case (4); psi_val = r%z
        case default; psi_val = 0.0d0
        end select
    end subroutine calc_psi_tetra_first

    pure elemental module subroutine calc_dpsi_tetra_first(self, i, j, r, dpsi_val)
        implicit none
        class(type_tetra_first), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: dpsi_val

        dpsi_val = 0.0d0
        select case (i)
        case (1)
            if (j == 1 .or. j == 2 .or. j == 3) dpsi_val = -1.0d0
        case (2)
            if (j == 1) dpsi_val = 1.0d0
        case (3)
            if (j == 2) dpsi_val = 1.0d0
        case (4)
            if (j == 3) dpsi_val = 1.0d0
        end select
    end subroutine calc_dpsi_tetra_first

    pure module subroutine calc_jacobian_tetra_first(self, r, node_coords, jac)
        implicit none
        class(type_tetra_first), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: jac(:, :)

        integer(int32) :: d

        jac = 0.0d0
        do d = 1, 3
            jac(d, 1) = node_coords(d, 2) - node_coords(d, 1)
            jac(d, 2) = node_coords(d, 3) - node_coords(d, 1)
            jac(d, 3) = node_coords(d, 4) - node_coords(d, 1)
        end do
    end subroutine calc_jacobian_tetra_first

    module subroutine is_in_tetra_first(self, cartesian, normalized, node_coords, is_in)
        implicit none
        class(type_tetra_first), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: cartesian
        type(type_coordinate_dp), intent(inout) :: normalized
        real(real64), intent(in) :: node_coords(:, :)
        logical, intent(inout) :: is_in

        real(real64) :: a11, a12, a13, a21, a22, a23, a31, a32, a33
        real(real64) :: dx, dy, dz, det_j, xi, eta, zeta
        real(real64), parameter :: tol = 1.0e-9

        a11 = node_coords(1,2) - node_coords(1,1)
        a12 = node_coords(1,3) - node_coords(1,1)
        a13 = node_coords(1,4) - node_coords(1,1)
        a21 = node_coords(2,2) - node_coords(2,1)
        a22 = node_coords(2,3) - node_coords(2,1)
        a23 = node_coords(2,4) - node_coords(2,1)
        a31 = node_coords(3,2) - node_coords(3,1)
        a32 = node_coords(3,3) - node_coords(3,1)
        a33 = node_coords(3,4) - node_coords(3,1)

        det_j = a11*(a22*a33 - a23*a32) - a12*(a21*a33 - a23*a31) + a13*(a21*a32 - a22*a31)

        if (abs(det_j) < epsilon(det_j)) then
            is_in = .false.
            return
        end if

        dx = cartesian%x - node_coords(1,1)
        dy = cartesian%y - node_coords(2,1)
        dz = cartesian%z - node_coords(3,1)

        xi   = (dx*(a22*a33 - a23*a32) - a12*(dy*a33 - a23*dz) + a13*(dy*a32 - a22*dz)) / det_j
        eta  = (a11*(dy*a33 - a23*dz) - dx*(a21*a33 - a23*a31) + a13*(a21*dz - dy*a31)) / det_j
        zeta = (a11*(a22*dz - dy*a32) - a12*(a21*dz - dy*a31) + dx*(a21*a32 - a22*a31)) / det_j

        is_in = (xi >= -tol) .and. (eta >= -tol) .and. (zeta >= -tol) &
                .and. (xi + eta + zeta <= 1.0d0 + tol)

        if (is_in) call normalized%set(xi, eta, zeta)
    end subroutine is_in_tetra_first

end submodule domain_fe_volume_tetra_first

    !>
    !> Implements the procedures for the second-order triangular (6-node) finite element.
    !> Corrected to use subroutine calls.
    !>
    submodule(domain_fe_element) domain_fe_element_triangle_second
        implicit none

    contains

        module function construct_triangle_second(integration_order) result(fe)
            implicit none
            !> The integration order for the element.
            integer(int32), intent(in) :: integration_order
            class(abst_fe), allocatable :: fe

            character(len=*), parameter :: cell_name = "QuadraticTriangle"
            integer(int32) :: vtk_type
            integer(int32) :: num_nodes
            integer(int32) :: dimension
            integer(int32) :: order
            integer(int32) :: num_gauss

            allocate (type_triangle_second :: fe)

            call vtk_constants%get_cell_info_from_cell_name(cell_name, vtk_type, num_nodes, dimension, order)

            call fe%initialize(type=vtk_type, dimension=dimension, order=order, num_nodes=num_nodes, &
                               integration_order=integration_order)

        end function construct_triangle_second

        module subroutine get_area_triangle_second(self, node_coords, geometry)
            implicit none
            class(type_triangle_second), intent(in) :: self
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: geometry

            integer(int32) :: i
            integer(int32) :: ng
            type(type_coordinate_dp), pointer, contiguous, dimension(:) :: gauss_pts
            real(real64), pointer, contiguous, dimension(:) :: weights
            real(real64) :: det_j

            geometry = 0.0d0
            call self%get_gauss(gauss_pts)
            call self%get_weight(weights)
            call self%get_num_gauss(ng)

            do i = 1, ng
                call self%jacobian_det(gauss_pts(i), node_coords, det_j)
                geometry = geometry + det_j * weights(i)
            end do

        end subroutine get_area_triangle_second

        pure elemental module subroutine psi_triangle_second(self, i, r, psi_val)
            implicit none
            class(type_triangle_second), intent(in) :: self
            integer(int32), intent(in) :: i
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: psi_val
            real(real64) :: L1
            real(real64) :: L2
            real(real64) :: L3

            L1 = 1.0d0 - r%x - r%y
            L2 = r%x
            L3 = r%y

            select case (i)
            case (1) ! Corner 1
                psi_val = L1 * (2.0d0 * L1 - 1.0d0)
            case (2) ! Corner 2
                psi_val = L2 * (2.0d0 * L2 - 1.0d0)
            case (3) ! Corner 3
                psi_val = L3 * (2.0d0 * L3 - 1.0d0)
            case (4) ! Mid 1-2
                psi_val = 4.0d0 * L1 * L2
            case (5) ! Mid 2-3
                psi_val = 4.0d0 * L2 * L3
            case (6) ! Mid 3-1
                psi_val = 4.0d0 * L3 * L1
            case default
                psi_val = 0.0d0
            end select
        end subroutine psi_triangle_second

        pure elemental module subroutine dpsi_triangle_second(self, i, j, r, dpsi_val)
            implicit none
            class(type_triangle_second), intent(in) :: self
            integer(int32), intent(in) :: i
            integer(int32), intent(in) :: j
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(inout) :: dpsi_val
            real(real64) :: xi
            real(real64) :: eta

            xi = r%x
            eta = r%y

            dpsi_val = 0.0d0
            select case (i)
            case (1)
                if (j == 1) dpsi_val = -3.0d0 + 4.0d0 * xi + 4.0d0 * eta
                if (j == 2) dpsi_val = -3.0d0 + 4.0d0 * xi + 4.0d0 * eta
            case (2)
                if (j == 1) dpsi_val = 4.0d0 * xi - 1.0d0
            case (3)
                if (j == 2) dpsi_val = 4.0d0 * eta - 1.0d0
            case (4)
                if (j == 1) dpsi_val = 4.0d0 - 8.0d0 * xi - 4.0d0 * eta
                if (j == 2) dpsi_val = -4.0d0 * xi
            case (5)
                if (j == 1) dpsi_val = 4.0d0 * eta
                if (j == 2) dpsi_val = 4.0d0 * xi
            case (6)
                if (j == 1) dpsi_val = -4.0d0 * eta
                if (j == 2) dpsi_val = 4.0d0 - 4.0d0 * xi - 8.0d0 * eta
            end select
        end subroutine dpsi_triangle_second

        pure module subroutine jacobian_triangle_second(self, r, node_coords, jac)
            implicit none
            class(type_triangle_second), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: jac(:, :)

            integer(int32) :: k
            integer(int32) :: nid
            real(real64) :: dpsi_xi
            real(real64) :: dpsi_eta

            jac = 0.0d0
            do k = 1, 6
                call self%dpsi(k, 1, r, dpsi_xi)
                call self%dpsi(k, 2, r, dpsi_eta)
                jac(1, 1) = jac(1, 1) + dpsi_xi * node_coords(1, k)
                jac(1, 2) = jac(1, 2) + dpsi_xi * node_coords(2, k)
                jac(2, 1) = jac(2, 1) + dpsi_eta * node_coords(1, k)
                jac(2, 2) = jac(2, 2) + dpsi_eta * node_coords(2, k)
            end do
        end subroutine jacobian_triangle_second

        pure module subroutine jacobian_det_triangle_second(self, r, node_coords, det_j)
            implicit none
            class(type_triangle_second), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: r
            real(real64), intent(in) :: node_coords(:, :)
            real(real64), intent(inout) :: det_j

            real(real64) :: jac(2, 2)
            call self%jacobian(r, node_coords, jac)
            det_j = jac(1, 1) * jac(2, 2) - jac(1, 2) * jac(2, 1)
        end subroutine jacobian_det_triangle_second

        module subroutine is_in_triangle_second(self, cartesian, normalized, node_coords, is_in)
            implicit none
            class(type_triangle_second), intent(in) :: self
            type(type_coordinate_dp), intent(in) :: cartesian
            type(type_coordinate_dp), intent(inout) :: normalized
            real(real64), intent(in) :: node_coords(:, :)
            logical, intent(inout) :: is_in

            type(type_coordinate_dp) :: r
            type(type_coordinate_dp) :: pos
            real(real64) :: det_j
            real(real64) :: dx
            real(real64) :: dy
            real(real64) :: jac(2, 2)
            real(real64) :: val
            integer(int32) :: iter
            integer(int32) :: i
            integer(int32) :: nid
            integer(int32) :: nn
            logical :: converged
            real(real64), parameter :: tol = 1.0e-9
            integer(int32), parameter :: max_iter = 10

            call r%set(1.0d0 / 3.0d0, 1.0d0 / 3.0d0, 0.0d0)
            call self%get_num_nodes(nn)
            converged = .false.

            do iter = 1, max_iter
                call pos%set(0.0d0, 0.0d0, 0.0d0)
                do i = 1, nn
                    call self%psi(i, r, val)
                    pos%x = pos%x + val * node_coords(1, i)
                    pos%y = pos%y + val * node_coords(2, i)
                end do

                dx = cartesian%x - pos%x
                dy = cartesian%y - pos%y
                if (sqrt(dx**2 + dy**2) < tol) then
                    converged = .true.
                    exit
                end if

                call self%jacobian_det(r, node_coords, det_j)
                if (abs(det_j) < epsilon(det_j)) exit

                call self%jacobian(r, node_coords, jac)

                r%x = r%x + (jac(2, 2) * dx - jac(1, 2) * dy) / det_j
                r%y = r%y + (-jac(2, 1) * dx + jac(1, 1) * dy) / det_j
            end do

            is_in = converged .and. (r%x >= -tol) .and. (r%y >= -tol) .and. (r%x + r%y <= 1.0d0 + tol)
            if (is_in) normalized = r
        end subroutine is_in_triangle_second

    end submodule domain_fe_element_triangle_second

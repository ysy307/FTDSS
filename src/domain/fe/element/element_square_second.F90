!>
!> Implements the procedures for the second-order quadrilateral (8-node) finite element.
!> Corrected to use subroutine calls.
!>
submodule(domain_fe_element) domain_fe_element_square_second
    use :: domain_fe_integration, only:get_integration_rule
    implicit none

contains

    module function construct_square_second(input) result(fe)
        implicit none
        type(type_input), intent(in) :: input
        class(abst_fe), allocatable :: fe

        character(len=*), parameter :: cell_name = "QuadraticQuad"
        integer(int32) :: vtk_type
        integer(int32) :: num_nodes
        integer(int32) :: dimension
        integer(int32) :: order
        integer(int32) :: num_gauss
        integer(int32) :: integration_order
        real(real64), allocatable :: weight(:)
        real(real64), allocatable :: gauss(:, :)

        allocate (type_square_second :: fe)

        call vtk_constants%get_cell_info_from_cell_name(cell_name, vtk_type, num_nodes, dimension, order)

        select case (strip(input%basic%geometry_settings%integration_type))
        case ("full")
            integration_order = 3
        case ("reduced")
            integration_order = 2
        case default
            integration_order = 3
        end select

        call get_integration_rule(cell_name, integration_order, num_gauss, weight, gauss)

        call fe%initialize(type=vtk_type, dimension=dimension, order=order, num_nodes=num_nodes, &
                           num_gauss=num_gauss, weight=weight, gauss=gauss)

        if (allocated(weight)) call deallocate_array(weight)
        if (allocated(gauss)) call deallocate_array(gauss)
    end function construct_square_second

    module subroutine get_area_square_second(self, node_coords, connectivity, geometry)
        implicit none
        class(type_square_second), intent(in) :: self
        real(real64), intent(in) :: node_coords(:, :)
        integer(int32), intent(in) :: connectivity(:)
        real(real64), intent(inout) :: geometry

        integer(int32) :: i
        integer(int32) :: ng
        type(type_coordinate_dp), allocatable :: gauss_pts(:)
        real(real64), allocatable :: weights(:)
        real(real64) :: det_j

        geometry = 0.0d0
        call self%get_gauss(gauss_pts)
        call self%get_weight(weights)
        call self%get_num_gauss(ng)

        do i = 1, ng
            call self%jacobian_det(gauss_pts(i), node_coords, connectivity, det_j)
            geometry = geometry + det_j * weights(i)
        end do

        if (allocated(gauss_pts)) deallocate (gauss_pts)
        if (allocated(weights)) deallocate (weights)
    end subroutine get_area_square_second

    pure elemental module subroutine psi_square_second(self, i, r, psi_val)
        implicit none
        class(type_square_second), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: psi_val
        real(real64) :: xi
        real(real64) :: eta

        xi = r%x
        eta = r%y

        select case (i)
        case (1); psi_val = 0.25d0 * (1.0d0 - xi) * (1.0d0 - eta) * (-1.0d0 - xi - eta)
        case (2); psi_val = 0.25d0 * (1.0d0 + xi) * (1.0d0 - eta) * (-1.0d0 + xi - eta)
        case (3); psi_val = 0.25d0 * (1.0d0 + xi) * (1.0d0 + eta) * (-1.0d0 + xi + eta)
        case (4); psi_val = 0.25d0 * (1.0d0 - xi) * (1.0d0 + eta) * (-1.0d0 - xi + eta)
        case (5); psi_val = 0.5d0 * (1.0d0 - xi**2) * (1.0d0 - eta)
        case (6); psi_val = 0.5d0 * (1.0d0 + xi) * (1.0d0 - eta**2)
        case (7); psi_val = 0.5d0 * (1.0d0 - xi**2) * (1.0d0 + eta)
        case (8); psi_val = 0.5d0 * (1.0d0 - xi) * (1.0d0 - eta**2)
        case default; psi_val = 0.0d0
        end select
    end subroutine psi_square_second

    pure elemental module subroutine dpsi_square_second(self, i, j, r, dpsi_val)
        implicit none
        class(type_square_second), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: dpsi_val
        real(real64) :: xi
        real(real64) :: eta

        xi = r%x
        eta = r%y
        dpsi_val = 0.0d0

        if (j == 1) then ! d/dxi
            select case (i)
            case (1); dpsi_val = 0.25d0 * (1.0d0 - eta) * (2.0d0 * xi + eta)
            case (2); dpsi_val = 0.25d0 * (1.0d0 - eta) * (2.0d0 * xi - eta)
            case (3); dpsi_val = 0.25d0 * (1.0d0 + eta) * (2.0d0 * xi + eta)
            case (4); dpsi_val = 0.25d0 * (1.0d0 + eta) * (2.0d0 * xi - eta)
            case (5); dpsi_val = -xi * (1.0d0 - eta)
            case (6); dpsi_val = 0.5d0 * (1.0d0 - eta**2)
            case (7); dpsi_val = -xi * (1.0d0 + eta)
            case (8); dpsi_val = -0.5d0 * (1.0d0 - eta**2)
            end select
        else if (j == 2) then ! d/deta
            select case (i)
            case (1); dpsi_val = 0.25d0 * (1.0d0 - xi) * (xi + 2.0d0 * eta)
            case (2); dpsi_val = 0.25d0 * (1.0d0 + xi) * (-xi + 2.0d0 * eta)
            case (3); dpsi_val = 0.25d0 * (1.0d0 + xi) * (xi + 2.0d0 * eta)
            case (4); dpsi_val = 0.25d0 * (1.0d0 - xi) * (-xi + 2.0d0 * eta)
            case (5); dpsi_val = -0.5d0 * (1.0d0 - xi**2)
            case (6); dpsi_val = -eta * (1.0d0 + xi)
            case (7); dpsi_val = 0.5d0 * (1.0d0 - xi**2)
            case (8); dpsi_val = -eta * (1.0d0 - xi)
            end select
        end if
    end subroutine dpsi_square_second

    pure module subroutine jacobian_square_second(self, r, node_coords, connectivity, jac)
        implicit none
        class(type_square_second), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        integer(int32), intent(in) :: connectivity(:)
        real(real64), intent(inout) :: jac(:, :)

        integer(int32) :: k
        integer(int32) :: nid
        real(real64) :: dpsi_xi
        real(real64) :: dpsi_eta

        jac = 0.0d0
        do k = 1, 8
            nid = connectivity(k)
            call self%dpsi(k, 1, r, dpsi_xi)
            call self%dpsi(k, 2, r, dpsi_eta)
            jac(1, 1) = jac(1, 1) + dpsi_xi * node_coords(1, nid)
            jac(1, 2) = jac(1, 2) + dpsi_xi * node_coords(2, nid)
            jac(2, 1) = jac(2, 1) + dpsi_eta * node_coords(1, nid)
            jac(2, 2) = jac(2, 2) + dpsi_eta * node_coords(2, nid)
        end do
    end subroutine jacobian_square_second

    pure module subroutine jacobian_det_square_second(self, r, node_coords, connectivity, det_j)
        implicit none
        class(type_square_second), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        integer(int32), intent(in) :: connectivity(:)
        real(real64), intent(inout) :: det_j

        real(real64) :: jac(2, 2)
        call self%jacobian(r, node_coords, connectivity, jac)
        det_j = jac(1, 1) * jac(2, 2) - jac(1, 2) * jac(2, 1)
    end subroutine jacobian_det_square_second

    module subroutine is_in_square_second(self, cartesian, normalized, node_coords, connectivity, is_in)
        implicit none
        class(type_square_second), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: cartesian
        type(type_coordinate_dp), intent(inout) :: normalized
        real(real64), intent(in) :: node_coords(:, :)
        integer(int32), intent(in) :: connectivity(:)
        logical, intent(inout) :: is_in

        type(type_coordinate_dp) :: r
        type(type_coordinate_dp) :: pos
        real(real64) :: det_j
        real(real64) :: dx
        real(real64) :: dy
        real(real64) :: jac(2, 2)
        integer(int32) :: iter
        integer(int32) :: i
        integer(int32) :: nid
        integer(int32) :: nn
        logical :: converged
        real(real64), parameter :: tol = 1.0e-9
        integer(int32), parameter :: max_iter = 10
        real(real64) :: psi_val

        call r%set(0.0d0, 0.0d0, 0.0d0)
        call self%get_num_nodes(nn)
        converged = .false.

        do iter = 1, max_iter
            call pos%set(0.0d0, 0.0d0, 0.0d0)
            do i = 1, nn
                nid = connectivity(i)
                call self%psi(i, r, psi_val)
                pos%x = pos%x + psi_val * node_coords(1, nid)
                pos%y = pos%y + psi_val * node_coords(2, nid)
            end do

            dx = cartesian%x - pos%x
            dy = cartesian%y - pos%y
            if (sqrt(dx**2 + dy**2) < tol) then
                converged = .true.
                exit
            end if

            call self%jacobian_det(r, node_coords, connectivity, det_j)
            if (abs(det_j) < epsilon(det_j)) exit

            call self%jacobian(r, node_coords, connectivity, jac)

            r%x = r%x + (jac(2, 2) * dx - jac(1, 2) * dy) / det_j
            r%y = r%y + (-jac(2, 1) * dx + jac(1, 1) * dy) / det_j
        end do

        is_in = converged .and. (abs(r%x) <= 1.0d0 + tol) .and. (abs(r%y) <= 1.0d0 + tol)
        if (is_in) normalized = r
    end subroutine is_in_square_second

end submodule domain_fe_element_square_second

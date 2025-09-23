submodule(domain_mesh_element) domain_mesh_element_triangle_first
    implicit none
contains

    !----------------------------------------------------------------------
    ! CONSTRUCTOR for a first-order triangle element calculator.
    !----------------------------------------------------------------------
    ! This function constructs a generic computational object for a 3-node
    ! linear triangle element. It creates the object based on global
    ! settings found in the 'input' object.
    !----------------------------------------------------------------------
    module function construct_triangle_first(input) result(element)
        implicit none
        type(type_input), intent(in) :: input
        class(abst_element), allocatable :: element

        character(len=32), parameter :: cell_name = "TRIANGLE"
        integer(int32) :: vtk_type
        integer(int32) :: num_nodes
        integer(int32) :: dimension
        integer(int32) :: order
        integer(int32) :: num_gauss
        real(real64), allocatable :: weight(:)
        real(real64), allocatable :: gauss(:, :)

        allocate (type_triangle_first :: element)

        call vtk_constants%get_cell_info(cell_name, vtk_type, num_nodes, dimension, order)

        ! For linear triangles, full, reduced, and free integration
        ! typically use the same single-point rule at the centroid.
        num_gauss = 1
        allocate (weight(num_gauss))
        allocate (gauss(3, num_gauss))
        weight(1) = 0.5d0
        gauss(1:2, 1) = [1.0d0 / 3.0d0, 1.0d0 / 3.0d0]
        gauss(3, 1) = 0.0d0

        call element%initialize(type=vtk_type, dimension=dimension, order=order, num_nodes=num_nodes, &
                                num_gauss=num_gauss, weight=weight, gauss=gauss)

        deallocate (weight, gauss)

    end function construct_triangle_first

    !----------------------------------------------------------------------
    ! get_area: Computes the area of a specific element instance.
    !----------------------------------------------------------------------
    module function get_area_triangle_first(self, node_coords) result(area)
        implicit none
        class(type_triangle_first), intent(in) :: self
        real(real64), intent(in) :: node_coords(:, :)
        real(real64) :: area

        integer(int32) :: i
        type(type_dp_vector_3d), allocatable :: gauss_pts(:)
        real(real64), allocatable :: weights(:)

        area = 0.0d0
        gauss_pts = self%get_gauss()
        weights = self%get_weight()

        do i = 1, self%get_num_gauss()
            area = area + self%jacobian_det(gauss_pts(i), node_coords) * weights(i)
        end do

    end function get_area_triangle_first

    !----------------------------------------------------------------------
    ! psi: Shape function N_i at local coordinate r = (xi, eta)
    !----------------------------------------------------------------------
    pure elemental module function psi_triangle_first(self, i, r) result(psi)
        implicit none
        class(type_triangle_first), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_dp_vector_3d), intent(in) :: r
        real(real64) :: psi

        select case (i)
        case (1)
            psi = r%x
        case (2)
            psi = r%y
        case (3)
            psi = 1.0d0 - r%x - r%y
        case default
            psi = 0.0d0
        end select
    end function psi_triangle_first

    !----------------------------------------------------------------------
    ! dpsi: Derivative of shape function w.r.t. local coordinates
    !----------------------------------------------------------------------
    pure elemental module function dpsi_triangle_first(self, i, j, r) result(dpsi)
        implicit none
        class(type_triangle_first), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_dp_vector_3d), intent(in) :: r
        real(real64) :: dpsi

        dpsi = 0.0d0
        select case (j)
        case (1) ! d/d(xi)
            select case (i)
            case (1)
                dpsi = 1.0d0
            case (3)
                dpsi = -1.0d0
            end select
        case (2) ! d/d(eta)
            select case (i)
            case (2)
                dpsi = 1.0d0
            case (3)
                dpsi = -1.0d0
            end select
        end select
    end function dpsi_triangle_first

    !----------------------------------------------------------------------
    ! jacobian: Computes the Jacobian matrix J.
    !----------------------------------------------------------------------
    pure module function jacobian_triangle_first(self, r, node_coords) result(jac)
        implicit none
        class(type_triangle_first), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64) :: jac(self%get_dimension(), self%get_dimension())

        integer(int32) :: i

        jac = 0.0d0
        do i = 1, self%get_num_nodes()
            jac(1, 1) = jac(1, 1) + self%dpsi(i, 1, r) * node_coords(1, i) ! dx/d_xi
            jac(1, 2) = jac(1, 2) + self%dpsi(i, 2, r) * node_coords(1, i) ! dx/d_eta
            jac(2, 1) = jac(2, 1) + self%dpsi(i, 1, r) * node_coords(2, i) ! dy/d_xi
            jac(2, 2) = jac(2, 2) + self%dpsi(i, 2, r) * node_coords(2, i) ! dy/d_eta
        end do
    end function jacobian_triangle_first

    !----------------------------------------------------------------------
    ! jacobian_det: Computes the determinant of the Jacobian matrix |J|.
    !----------------------------------------------------------------------
    pure module function jacobian_det_triangle_first(self, r, node_coords) result(det_j)
        implicit none
        class(type_triangle_first), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64) :: det_j

        real(real64) :: jac(self%get_dimension(), self%get_dimension())

        jac = self%jacobian(r, node_coords)
        det_j = jac(1, 1) * jac(2, 2) - jac(1, 2) * jac(2, 1)
    end function jacobian_det_triangle_first

    !----------------------------------------------------------------------
    ! is_in: Checks if a point in global coordinates is inside the element.
    !----------------------------------------------------------------------
    module subroutine is_in_triangle_first(self, cartesian, normalized, node_coords, is_in)
        implicit none
        class(type_triangle_first), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: cartesian
        type(type_dp_vector_3d), intent(inout) :: normalized
        real(real64), intent(in) :: node_coords(:, :)
        logical, intent(inout) :: is_in

        type(type_dp_vector_3d) :: r
        type(type_dp_vector_3d) :: interpolated_pos
        real(real64) :: det_j
        real(real64) :: dx
        real(real64) :: dy
        real(real64) :: jac(self%get_dimension(), self%get_dimension())
        integer(int32) :: iter
        integer(int32) :: i
        logical :: converged
        real(real64), parameter :: tol = 1.0e-9
        integer(int32), parameter :: max_iter = 10

        call r%set(1.0d0 / 3.0d0, 1.0d0 / 3.0d0, 0.0d0)
        converged = .false.

        do iter = 1, max_iter
            call interpolated_pos%set(0.0d0, 0.0d0, 0.0d0)
            do i = 1, self%get_num_nodes()
                interpolated_pos%x = interpolated_pos%x + self%psi(i, r) * node_coords(1, i)
                interpolated_pos%y = interpolated_pos%y + self%psi(i, r) * node_coords(2, i)
            end do

            dx = cartesian%x - interpolated_pos%x
            dy = cartesian%y - interpolated_pos%y
            if (sqrt(dx**2 + dy**2) < tol) then
                converged = .true.
                exit
            end if

            det_j = self%jacobian_det(r, node_coords)
            if (abs(det_j) < epsilon(det_j)) then
                exit
            end if

            jac = self%jacobian(r, node_coords)
            r%x = r%x + (jac(2, 2) * dx - jac(1, 2) * dy) / det_j
            r%y = r%y + (-jac(2, 1) * dx + jac(1, 1) * dy) / det_j
        end do

        is_in = converged .and. (r%x >= -tol) .and. (r%y >= -tol) .and. (r%x + r%y <= 1.0d0 + tol)
        if (is_in) then
            normalized = r
        end if

    end subroutine is_in_triangle_first

end submodule domain_mesh_element_triangle_first

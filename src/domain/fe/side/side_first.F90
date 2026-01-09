!>
!> Implements the procedures for the first-order side (line) finite element.
!> Refactored to use subroutines for all interface methods and strict variable declarations.
!>
submodule(domain_fe_side) domain_fe_side_first
    implicit none

contains

    !>
    !> Creates and initializes a first-order side (2-node line) element object.
    !>
    module function construct_side_first(input) result(fe)
        implicit none
        !> The main input data structure.
        type(type_input), intent(in) :: input
        !> The newly created and allocated first-order side element object.
        class(abst_fe), allocatable :: fe

        character(len=*), parameter :: cell_name = "Line"
        integer(int32) :: vtk_type
        integer(int32) :: num_nodes
        integer(int32) :: dimension
        integer(int32) :: order
        integer(int32) :: num_gauss
        real(real64), allocatable :: weight(:)
        real(real64), allocatable :: gauss(:, :)

        allocate (type_side_first :: fe)

        call vtk_constants%get_cell_info_from_cell_name(cell_name, vtk_type, num_nodes, dimension, order)

        ! Integration rule for a 1st-order element (1-point Gauss quadrature)
        num_gauss = 1
        call allocate_array(weight, num_gauss)
        call allocate_array(gauss, 3, num_gauss)

        weight(1) = 2.0d0
        gauss(:, 1) = 0.0d0

        call fe%initialize(type=vtk_type, dimension=dimension, order=order, num_nodes=num_nodes, &
                           num_gauss=num_gauss, weight=weight, gauss=gauss)

        call deallocate_array(weight)
        call deallocate_array(gauss)
    end function construct_side_first

    !>
    !> Calculates the straight-line length of the element.
    !>
    module subroutine get_length_side_first(self, node_coords, geometry)
        implicit none
        class(type_side_first), intent(in) :: self
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: geometry

        integer(int32) :: node1_id
        integer(int32) :: node2_id
        real(real64) :: dx
        real(real64) :: dy
        real(real64) :: dz

        dx = node_coords(1, 2) - node_coords(1, 1)
        dy = node_coords(2, 2) - node_coords(2, 1)
        dz = node_coords(3, 2) - node_coords(3, 1)

        geometry = sqrt(dx**2 + dy**2 + dz**2)
    end subroutine get_length_side_first

    !>
    !> Evaluates the shape function psi.
    !>
    pure elemental module subroutine psi_side_first(self, i, r, psi_val)
        implicit none
        class(type_side_first), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: psi_val

        select case (i)
        case (1)
            psi_val = 0.5d0 * (1.0d0 - r%x)
        case (2)
            psi_val = 0.5d0 * (1.0d0 + r%x)
        case default
            psi_val = 0.0d0
        end select
    end subroutine psi_side_first

    !>
    !> Evaluates the derivative of the shape function dpsi.
    !>
    pure elemental module subroutine dpsi_side_first(self, i, j, r, dpsi_val)
        implicit none
        class(type_side_first), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: dpsi_val

        dpsi_val = 0.0d0
        if (j == 1) then
            select case (i)
            case (1)
                dpsi_val = -0.5d0
            case (2)
                dpsi_val = 0.5d0
            end select
        end if
    end subroutine dpsi_side_first

    !>
    !> Computes the tangent vector at a specified local coordinate.
    !>
    pure module subroutine compute_tangent_vector_side_first(self, r, node_coords, tangent_vec)
        implicit none
        class(type_side_first), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: tangent_vec(:)

        integer(int32) :: i
        integer(int32) :: node_id
        integer(int32) :: nn
        real(real64) :: dpsi_val

        tangent_vec = 0.0d0
        call self%get_num_nodes(nn)

        do i = 1, nn
            call self%dpsi(i, 1, r, dpsi_val)
            tangent_vec(1) = tangent_vec(1) + dpsi_val * node_coords(1, i)
            tangent_vec(2) = tangent_vec(2) + dpsi_val * node_coords(2, i)
            tangent_vec(3) = tangent_vec(3) + dpsi_val * node_coords(3, i)
        end do
    end subroutine compute_tangent_vector_side_first

    !>
    !> Calculates the Jacobian matrix.
    !>
    pure module subroutine jacobian_side_first(self, r, node_coords, jac)
        implicit none
        class(type_side_first), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: jac(:, :)

        real(real64) :: tangent_vec(3)

        call self%compute_tangent_vector(r, node_coords, tangent_vec)
        jac(1, 1) = sqrt(sum(tangent_vec**2))
    end subroutine jacobian_side_first

    !>
    !> Calculates the Jacobian determinant.
    !>
    pure module subroutine jacobian_det_side_first(self, r, node_coords, det_j)
        implicit none
        class(type_side_first), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: det_j

        real(real64) :: jac(1, 1)

        call self%jacobian(r, node_coords, jac)
        det_j = jac(1, 1)
    end subroutine jacobian_det_side_first

    !>
    !> Checks if a point is on the element.
    !>
    module subroutine is_in_side_first(self, cartesian, normalized, node_coords, is_in)
        implicit none
        class(type_side_first), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: cartesian
        type(type_coordinate_dp), intent(inout) :: normalized
        real(real64), intent(in) :: node_coords(:, :)
        logical, intent(inout) :: is_in

        real(real64) :: v(3)
        real(real64) :: w(3)
        real(real64) :: t
        real(real64) :: v_dot_v
        integer(int32) :: node1_id
        integer(int32) :: node2_id
        real(real64), parameter :: tol = 1.0e-9

        ! Vector from node 1 to node 2
        v(1) = node_coords(1, 2) - node_coords(1, 1)
        v(2) = node_coords(2, 2) - node_coords(2, 1)
        v(3) = node_coords(3, 2) - node_coords(3, 1)

        ! Vector from node 1 to the point to check
        w(1) = cartesian%x - node_coords(1, 1)
        w(2) = cartesian%y - node_coords(2, 1)
        w(3) = cartesian%z - node_coords(3, 1)

        v_dot_v = v(1)**2 + v(2)**2 + v(3)**2

        if (v_dot_v < tol**2) then
            is_in = (abs(w(1)) < tol .and. abs(w(2)) < tol .and. abs(w(3)) < tol)
        else
            t = (w(1) * v(1) + w(2) * v(2) + w(3) * v(3)) / v_dot_v
            is_in = (t >= 0.0d0 - tol .and. t <= 1.0d0 + tol)
        end if

        if (is_in) then
            call normalized%set(2.0d0 * t - 1.0d0, 0.0d0, 0.0d0)
        end if
    end subroutine is_in_side_first

end submodule domain_fe_side_first


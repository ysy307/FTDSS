!>
!> Implements the procedures for the first-order side (line) finite element.
!>
submodule(domain_fe_side) domain_fe_side_first
    implicit none
contains

    !>
    !> Creates and initializes a first-order side (2-node line) element object.
    !> This function sets up the element properties, including the number of nodes,
    !> dimension, order, and a 1-point Gauss integration rule suitable for linear elements.
    !>
    module function construct_side_first(input) result(fe)
        implicit none
        !> The main input data structure (currently unused but kept for API consistency).
        type(type_input), intent(in) :: input
        !> The newly created and allocated first-order side element object.
        class(abst_fe), allocatable :: fe

        character(len=32), parameter :: cell_name = "Line"
        integer(int32) :: vtk_type, num_nodes, dimension, order, num_gauss
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
    !> This is computed as the Euclidean distance between the element's two nodes.
    !>
    module function get_length_side_first(self, node_coords, connectivity) result(length)
        implicit none
        !> The first-order side element object.
        class(type_side_first), intent(in) :: self
        !> The global coordinates of the mesh nodes.
        real(real64), intent(in) :: node_coords(:, :)
        !> The connectivity array for the element.
        integer(int32), intent(in) :: connectivity(:)
        !> The computed length of the element.
        real(real64) :: length
        integer(int32) :: node1_id, node2_id
        real(real64) :: dx, dy, dz

        node1_id = connectivity(1)
        node2_id = connectivity(2)

        dx = node_coords(1, node2_id) - node_coords(1, node1_id)
        dy = node_coords(2, node2_id) - node_coords(2, node1_id)
        dz = node_coords(3, node2_id) - node_coords(3, node1_id)

        length = sqrt(dx**2 + dy**2 + dz**2)
    end function get_length_side_first

    !>
    !> Evaluates the shape function \( \psi_i \) for a 2-node linear line element.
    !> The functions are \( \psi_1(\xi) = \frac{1}{2}(1-\xi) \) and \( \psi_2(\xi) = \frac{1}{2}(1+\xi) \).
    !>
    module pure elemental function psi_side_first(self, i, r) result(psi)
        implicit none
        !> The first-order side element object.
        class(type_side_first), intent(in) :: self
        !> The index of the shape function (1 or 2).
        integer(int32), intent(in) :: i
        !> The local coordinate vector, where \( \xi = r\%x \).
        type(type_dp_vector_3d), intent(in) :: r
        !> The value of the shape function \( \psi_i(\xi) \).
        real(real64) :: psi

        select case (i)
        case (1)
            psi = 0.5d0 * (1.0d0 - r%x)
        case (2)
            psi = 0.5d0 * (1.0d0 + r%x)
        case default
            psi = 0.0d0
        end select
    end function psi_side_first

    !>
    !> Evaluates the derivative of the shape function with respect to the local
    !> coordinate, \( \frac{d\psi_i}{d\xi} \).
    !>
    module pure elemental function dpsi_side_first(self, i, j, r) result(dpsi)
        implicit none
        !> The first-order side element object.
        class(type_side_first), intent(in) :: self
        !> The index of the shape function (1 or 2).
        integer(int32), intent(in) :: i
        !> The index of the local coordinate to differentiate with respect to (must be 1 for \( \xi \)).
        integer(int32), intent(in) :: j
        !> The local coordinate vector (unused, as derivatives are constant).
        type(type_dp_vector_3d), intent(in) :: r
        !> The value of the derivative, which is a constant (\( \mp 0.5 \)).
        real(real64) :: dpsi

        dpsi = 0.0d0
        if (j == 1) then
            select case (i)
            case (1)
                dpsi = -0.5d0
            case (2)
                dpsi = 0.5d0
            end select
        end if
    end function dpsi_side_first

    !>
    !> Computes the tangent vector at a specified local coordinate on the element.
    !> For a linear element, this vector is constant along the element.
    !>
    module pure function compute_tangent_vector_side_first(self, r, node_coords, connectivity) result(tangent_vec)
        implicit none
        !> The first-order side element object.
        class(type_side_first), intent(in) :: self
        !> The local coordinate vector (unused, as the tangent is constant).
        type(type_dp_vector_3d), intent(in) :: r
        !> The global coordinates of the mesh nodes.
        real(real64), intent(in) :: node_coords(:, :)
        !> The connectivity array for the element.
        integer(int32), intent(in) :: connectivity(:)
        !> The computed 3D tangent vector.
        real(real64) :: tangent_vec(3)
        integer(int32) :: i, node_id

        tangent_vec = 0.0d0
        do i = 1, self%get_num_nodes()
            node_id = connectivity(i)
            tangent_vec(1) = tangent_vec(1) + self%dpsi(i, 1, r) * node_coords(1, node_id)
            tangent_vec(2) = tangent_vec(2) + self%dpsi(i, 1, r) * node_coords(2, node_id)
            tangent_vec(3) = tangent_vec(3) + self%dpsi(i, 1, r) * node_coords(3, node_id)
        end do
    end function compute_tangent_vector_side_first

    !>
    !> Calculates the Jacobian matrix for the 1D element.
    !> For a 1D element, this is a 1x1 matrix whose single component is the
    !> magnitude (norm) of the tangent vector, which is constant for a linear element.
    !>
    module pure function jacobian_side_first(self, r, node_coords, connectivity) result(jacobian)
        implicit none
        !> The first-order side element object.
        class(type_side_first), intent(in) :: self
        !> The local coordinate vector (unused).
        type(type_dp_vector_3d), intent(in) :: r
        !> The global coordinates of the mesh nodes.
        real(real64), intent(in) :: node_coords(:, :)
        !> The connectivity array for the element.
        integer(int32), intent(in) :: connectivity(:)
        !> The computed 1x1 Jacobian matrix.
        real(real64) :: jacobian(self%get_dimension(), self%get_dimension())
        real(real64) :: tangent_vec(3)

        tangent_vec = self%compute_tangent_vector(r, node_coords, connectivity)

        jacobian(1, 1) = sqrt(sum(tangent_vec**2))
    end function jacobian_side_first

    !>
    !> Calculates the determinant of the Jacobian matrix.
    !> For a 1D element, this is simply the value of the (1,1) component of the
    !> Jacobian matrix, which is half the element length.
    !>
    module pure function jacobian_det_side_first(self, r, node_coords, connectivity) result(jacobian_det)
        implicit none
        !> The first-order side element object.
        class(type_side_first), intent(in) :: self
        !> The local coordinate vector (unused).
        type(type_dp_vector_3d), intent(in) :: r
        !> The global coordinates of the mesh nodes.
        real(real64), intent(in) :: node_coords(:, :)
        !> The connectivity array for the element.
        integer(int32), intent(in) :: connectivity(:)
        !> The Jacobian determinant.
        real(real64) :: jacobian_det
        real(real64) :: jacobian(self%get_dimension(), self%get_dimension())

        jacobian = self%jacobian(r, node_coords, connectivity)
        jacobian_det = jacobian(1, 1)
    end function jacobian_det_side_first

    !>
    !> Determines if a point in global coordinates lies on the line segment element.
    !> This is solved by projecting the vector from the first node to the point onto
    !> the element's direction vector.
    !>
    module subroutine is_in_side_first(self, cartesian, normalized, node_coords, connectivity, is_in)
        implicit none
        !> The first-order side element object.
        class(type_side_first), intent(in) :: self
        !> The point in global (Cartesian) coordinates to check.
        type(type_dp_vector_3d), intent(in) :: cartesian
        !> The resulting local (normalized) coordinate if the point is on the element.
        type(type_dp_vector_3d), intent(inout) :: normalized
        !> The global coordinates of the mesh nodes.
        real(real64), intent(in) :: node_coords(:, :)
        !> The connectivity array for the element.
        integer(int32), intent(in) :: connectivity(:)
        !> A logical flag, set to true if the point is on the element, false otherwise.
        logical, intent(inout) :: is_in

        real(real64) :: v(3), w(3)
        real(real64) :: t, v_dot_v
        integer(int32) :: node1_id, node2_id
        real(real64), parameter :: tol = 1.0e-9

        node1_id = connectivity(1)
        node2_id = connectivity(2)

        ! Vector from node 1 to node 2
        v(1) = node_coords(1, node2_id) - node_coords(1, node1_id)
        v(2) = node_coords(2, node2_id) - node_coords(2, node1_id)
        v(3) = node_coords(3, node2_id) - node_coords(3, node1_id)

        ! Vector from node 1 to the point to check
        w(1) = cartesian%x - node_coords(1, node1_id)
        w(2) = cartesian%y - node_coords(2, node1_id)
        w(3) = cartesian%z - node_coords(3, node1_id)

        v_dot_v = v(1)**2 + v(2)**2 + v(3)**2

        if (v_dot_v < tol**2) then
            ! The element has zero length; check if the point is at the same location.
            is_in = (abs(w(1)) < tol .and. abs(w(2)) < tol .and. abs(w(3)) < tol)
        else
            ! Project w onto v to find the parametric coordinate t.
            t = (w(1) * v(1) + w(2) * v(2) + w(3) * v(3)) / v_dot_v
            ! The point is on the line segment if t is between 0 and 1.
            is_in = (t >= 0.0d0 - tol .and. t <= 1.0d0 + tol)
        end if

        if (is_in) then
            ! Convert parametric coordinate t [0, 1] to local coordinate xi [-1, 1].
            call normalized%set(2.0d0 * t - 1.0d0, 0.0d0, 0.0d0)
        end if
    end subroutine is_in_side_first

end submodule domain_fe_side_first

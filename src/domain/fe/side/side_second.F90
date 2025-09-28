!>
!> Implements the procedures for the second-order side (line) finite element.
!>
submodule(domain_fe_side) domain_fe_side_second
    implicit none

contains

    !>
    !> Creates and initializes a second-order side (3-node line) element object.
    !> This function sets up the element properties, including the number of nodes,
    !> dimension, order, and Gauss integration rule based on the input settings.
    !>
    module function construct_side_second(input) result(fe)
        implicit none
        !> The main input data structure containing simulation settings.
        type(type_input), intent(in) :: input
        !> The newly created and allocated second-order side element object.
        class(abst_fe), allocatable :: fe

        character(len=32), parameter :: cell_name = "QuadraticEdge"
        integer(int32) :: vtk_type, num_nodes, dimension, order, num_gauss
        real(real64), allocatable :: weight(:), gauss(:, :)

        allocate (type_side_second :: fe)

        call vtk_constants%get_cell_info_from_cell_name(cell_name, vtk_type, num_nodes, dimension, order)

        ! Set integration rule based on input settings
        select case (strip(input%basic%geometry_settings%integration_type))
        case ("full")
            num_gauss = 2
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, 3, num_gauss)

            weight(:) = [1.0d0, 1.0d0]
            gauss(1, 1) = -1.0d0 / sqrt(3.0d0)
            gauss(1, 2) = 1.0d0 / sqrt(3.0d0)
            gauss(2:3, :) = 0.0d0
        case ("reduced")
            num_gauss = 1
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, 3, num_gauss)

            weight(1) = 2.0d0
            gauss(:, 1) = 0.0d0
        case ("free") ! Default to full integration for "free"
            num_gauss = 2
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, 3, num_gauss)

            weight(:) = [1.0d0, 1.0d0]
            gauss(1, 1) = -1.0d0 / sqrt(3.0d0)
            gauss(1, 2) = 1.0d0 / sqrt(3.0d0)
            gauss(2:3, :) = 0.0d0
        end select

        call fe%initialize(type=vtk_type, dimension=dimension, order=order, num_nodes=num_nodes, &
                           num_gauss=num_gauss, weight=weight, gauss=gauss)

        call deallocate_array(weight)
        call deallocate_array(gauss)

    end function construct_side_second

    !>
    !> Computes the tangent vector at a specified local coordinate on the element.
    !> The tangent vector is calculated as \( \sum_{i=1}^{N} \frac{d\psi_i}{d\xi} \mathbf{x}_i \).
    !>
    module pure function compute_tangent_vector_side_second(self, r, node_coords, connectivity) result(tangent_vec)
        implicit none
        !> The second-order side element object.
        class(type_side_second), intent(in) :: self
        !> The local coordinate vector \( r \).
        type(type_coordinate_dp), intent(in) :: r
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
    end function compute_tangent_vector_side_second

    !>
    !> Calculates the curved length of the element using Gauss quadrature.
    !> The length is computed by integrating the Jacobian determinant (magnitude of the
    !> tangent vector) over the element domain.
    !>
    module function get_length_side_second(self, node_coords, connectivity) result(length)
        implicit none
        !> The second-order side element object.
        class(type_side_second), intent(in) :: self
        !> The global coordinates of the mesh nodes.
        real(real64), intent(in) :: node_coords(:, :)
        !> The connectivity array for the element.
        integer(int32), intent(in) :: connectivity(:)
        !> The computed length of the element.
        real(real64) :: length
        integer(int32) :: i
        type(type_coordinate_dp), allocatable :: gauss_pts(:)
        real(real64), allocatable :: weights(:)

        length = 0.0d0
        gauss_pts = self%get_gauss()
        weights = self%get_weight()

        do i = 1, self%get_num_gauss()
            length = length + self%jacobian_det(gauss_pts(i), node_coords, connectivity) * weights(i)
        end do
    end function get_length_side_second

    !>
    !> Evaluates the shape function \( \psi_i \) for a 3-node quadratic line element.
    !>
    module pure elemental function psi_side_second(self, i, r) result(psi)
        implicit none
        !> The second-order side element object.
        class(type_side_second), intent(in) :: self
        !> The index of the shape function (1, 2, or 3).
        integer(int32), intent(in) :: i
        !> The local coordinate vector, where \( \xi = r\%x \).
        type(type_coordinate_dp), intent(in) :: r
        !> The value of the shape function \( \psi_i(\xi) \).
        real(real64) :: psi
        real(real64) :: xi

        xi = r%x
        select case (i)
        case (1)
            psi = 0.5d0 * xi * (xi - 1.0d0)
        case (2)
            psi = 0.5d0 * xi * (xi + 1.0d0)
        case (3)
            psi = 1.0d0 - xi**2
        case default
            psi = 0.0d0
        end select
    end function psi_side_second

    !>
    !> Evaluates the derivative of the shape function with respect to the local
    !> coordinate, \( \frac{d\psi_i}{d\xi} \).
    !>
    module pure elemental function dpsi_side_second(self, i, j, r) result(dpsi)
        implicit none
        !> The second-order side element object.
        class(type_side_second), intent(in) :: self
        !> The index of the shape function (1, 2, or 3).
        integer(int32), intent(in) :: i
        !> The index of the local coordinate to differentiate with respect to (must be 1 for \( \xi \)).
        integer(int32), intent(in) :: j
        !> The local coordinate vector, where \( \xi = r\%x \).
        type(type_coordinate_dp), intent(in) :: r
        !> The value of the derivative \( \frac{d\psi_i}{d\xi} \).
        real(real64) :: dpsi
        real(real64) :: xi

        dpsi = 0.0d0
        if (j == 1) then
            xi = r%x
            select case (i)
            case (1)
                dpsi = xi - 0.5d0
            case (2)
                dpsi = xi + 0.5d0
            case (3)
                dpsi = -2.0d0 * xi
            end select
        end if
    end function dpsi_side_second

    !>
    !> Calculates the Jacobian matrix for the 1D element.
    !> For a 1D element, this is a 1x1 matrix whose single component is the
    !> magnitude (norm) of the tangent vector.
    !>
    module pure function jacobian_side_second(self, r, node_coords, connectivity) result(jacobian)
        implicit none
        !> The second-order side element object.
        class(type_side_second), intent(in) :: self
        !> The local coordinate vector \( r \).
        type(type_coordinate_dp), intent(in) :: r
        !> The global coordinates of the mesh nodes.
        real(real64), intent(in) :: node_coords(:, :)
        !> The connectivity array for the element.
        integer(int32), intent(in) :: connectivity(:)
        !> The computed 1x1 Jacobian matrix.
        real(real64) :: jacobian(self%get_dimension(), self%get_dimension())

        real(real64) :: tangent_vec(3)

        tangent_vec = compute_tangent_vector_side_second(self, r, node_coords, connectivity)
        jacobian(1, 1) = sqrt(sum(tangent_vec**2))
    end function jacobian_side_second

    !>
    !> Calculates the determinant of the Jacobian matrix.
    !> For a 1D element, this is simply the value of the (1,1) component of the
    !> Jacobian matrix, which represents the differential length element \( dL \).
    !>
    module pure function jacobian_det_side_second(self, r, node_coords, connectivity) result(jacobian_det)
        implicit none
        !> The second-order side element object.
        class(type_side_second), intent(in) :: self
        !> The local coordinate vector \( r \).
        type(type_coordinate_dp), intent(in) :: r
        !> The global coordinates of the mesh nodes.
        real(real64), intent(in) :: node_coords(:, :)
        !> The connectivity array for the element.
        integer(int32), intent(in) :: connectivity(:)
        !> The Jacobian determinant.
        real(real64) :: jacobian_det

        real(real64) :: jacobian_matrix(self%get_dimension(), self%get_dimension())

        jacobian_matrix = self%jacobian(r, node_coords, connectivity)
        jacobian_det = jacobian_matrix(1, 1)
    end function jacobian_det_side_second

    !>
    !> Determines if a point in global coordinates lies on the element.
    !> This is solved by using the Newton-Raphson method to find the local
    !> coordinate \( \xi \) corresponding to the given global point.
    !>
    module subroutine is_in_side_second(self, cartesian, normalized, node_coords, connectivity, is_in)
        implicit none
        !> The second-order side element object.
        class(type_side_second), intent(in) :: self
        !> The point in global (Cartesian) coordinates to check.
        type(type_coordinate_dp), intent(in) :: cartesian
        !> The resulting local (normalized) coordinate if the point is on the element.
        type(type_coordinate_dp), intent(inout) :: normalized
        !> The global coordinates of the mesh nodes.
        real(real64), intent(in) :: node_coords(:, :)
        !> The connectivity array for the element.
        integer(int32), intent(in) :: connectivity(:)
        !> A logical flag, set to true if the point is on the element, false otherwise.
        logical, intent(inout) :: is_in

        type(type_coordinate_dp) :: r_local
        type(type_coordinate_dp) :: pos_guess
        type(type_coordinate_dp) :: residual_vec
        real(real64) :: tangent_vec(3)
        real(real64) :: tangent_dot_tangent
        real(real64) :: residual_norm
        real(real64) :: psi_val
        integer(int32) :: iter
        integer(int32) :: i
        integer(int32) :: node_id
        logical :: converged
        real(real64), parameter :: tol = 1.0e-9
        integer(int32), parameter :: max_iter = 10

        ! Start Newton-Raphson iteration from the center of the element (xi=0)
        call r_local%set(0.0d0, 0.0d0, 0.0d0)
        converged = .false.

        do iter = 1, max_iter
            ! 1. Calculate the global position for the current local coordinate guess
            call pos_guess%set(0.0d0, 0.0d0, 0.0d0)
            do i = 1, self%get_num_nodes()
                node_id = connectivity(i)
                psi_val = self%psi(i, r_local)
                pos_guess%x = pos_guess%x + psi_val * node_coords(1, node_id)
                pos_guess%y = pos_guess%y + psi_val * node_coords(2, node_id)
                pos_guess%z = pos_guess%z + psi_val * node_coords(3, node_id)
            end do

            ! 2. Calculate the residual vector (difference from target point)
            residual_vec%x = cartesian%x - pos_guess%x
            residual_vec%y = cartesian%y - pos_guess%y
            residual_vec%z = cartesian%z - pos_guess%z
            residual_norm = sqrt(residual_vec%x**2 + residual_vec%y**2 + residual_vec%z**2)

            if (residual_norm < tol) then
                converged = .true.
                exit
            end if

            ! 3. Calculate the tangent vector at the current local coordinate
            tangent_vec = compute_tangent_vector_side_second(self, r_local, node_coords, connectivity)
            tangent_dot_tangent = tangent_vec(1)**2 + tangent_vec(2)**2 + tangent_vec(3)**2

            if (tangent_dot_tangent < epsilon(tangent_dot_tangent)) then
                is_in = .false.
                return
            end if

            ! 4. Update the local coordinate using the Newton-Raphson formula
            r_local%x = r_local%x + (tangent_vec(1) * residual_vec%x + &
                                     tangent_vec(2) * residual_vec%y + &
                                     tangent_vec(3) * residual_vec%z) / tangent_dot_tangent
        end do

        ! 5. After converging, check if the local coordinate is within the valid range [-1, 1]
        is_in = converged .and. (abs(r_local%x) <= 1.0d0 + tol)
        if (is_in) then
            normalized = r_local
        end if

    end subroutine is_in_side_second

end submodule domain_fe_side_second

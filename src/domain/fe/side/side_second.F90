!>
!> Implements the procedures for the second-order side (line) finite element.
!> Refactored to use subroutines for all interface methods and strict variable declarations.
!>
submodule(domain_fe_side) domain_fe_side_second
    implicit none
contains

    !>
    !> Creates and initializes a second-order side (3-node line) element object.
    !>
    module function construct_side_second(integration_order) result(fe)
        implicit none
        !> The integration order for the element.
        integer(int32), intent(in) :: integration_order
        class(abst_fe), allocatable :: fe

        character(len=*), parameter :: cell_name = "QuadraticEdge"
        integer(int32) :: vtk_type
        integer(int32) :: num_nodes
        integer(int32) :: dimension
        integer(int32) :: order
        integer(int32) :: num_gauss

        allocate (type_side_second :: fe)

        call vtk_constants%get_cell_info_from_cell_name(cell_name, vtk_type, num_nodes, dimension, order)

        call fe%initialize(type=vtk_type, dimension=dimension, order=order, num_nodes=num_nodes, &
                           integration_order=integration_order)

    end function construct_side_second

    !>
    !> Computes the tangent vector at a specified local coordinate.
    !>
    pure module subroutine compute_tangent_vector_side_second(self, r, node_coords, tangent_vec)
        implicit none
        class(type_side_second), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: tangent_vec(:)

        integer(int32) :: i
        integer(int32) :: nn
        integer(int32) :: n_dim ! <--- 追加: 座標の次元数
        real(real64) :: dpsi_val

        tangent_vec = 0.0d0
        call self%get_num_nodes(nn)

        ! node_coords の第1次元（空間次元）を取得
        n_dim = size(node_coords, 1)

        do i = 1, nn
            call self%calc_dpsi(i, 1, r, dpsi_val)

            ! tangent_vec のサイズだけでなく、node_coords の次元数もチェックする
            if (size(tangent_vec) >= 1 .and. n_dim >= 1) &
                tangent_vec(1) = tangent_vec(1) + dpsi_val * node_coords(1, i)

            if (size(tangent_vec) >= 2 .and. n_dim >= 2) &
                tangent_vec(2) = tangent_vec(2) + dpsi_val * node_coords(2, i)

            if (size(tangent_vec) >= 3 .and. n_dim >= 3) &
                tangent_vec(3) = tangent_vec(3) + dpsi_val * node_coords(3, i)
        end do
    end subroutine compute_tangent_vector_side_second

    !>
    !> Calculates the curved length of the element using Gauss quadrature.
    !>
    module subroutine calc_length_side_second(self, node_coords, measure)
        implicit none
        class(type_side_second), intent(in) :: self
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: measure

        integer(int32) :: i
        type(type_gauss_integration_rule), pointer :: gauss_rule
        real(real64) :: det_j

        measure = 0.0d0
        call self%get_integration_rule(gauss_rule)

        do i = 1, gauss_rule%num_gauss
            call self%calc_jacobian_determinant(gauss_rule%gauss(i), node_coords, det_j)
            measure = measure + det_j * gauss_rule%weight(i)
        end do

    end subroutine calc_length_side_second

    !>
    !> Evaluates the shape function psi.
    !>
    pure elemental module subroutine calc_psi_side_second(self, i, r, psi_val)
        implicit none
        class(type_side_second), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: psi_val
        real(real64) :: xi

        xi = r%x
        select case (i)
        case (1)
            psi_val = 0.5d0 * xi * (xi - 1.0d0)
        case (2)
            psi_val = 0.5d0 * xi * (xi + 1.0d0)
        case (3)
            psi_val = 1.0d0 - xi**2
        case default
            psi_val = 0.0d0
        end select
    end subroutine calc_psi_side_second

    !>
    !> Evaluates the derivative dpsi.
    !>
    pure elemental module subroutine calc_dpsi_side_second(self, i, j, r, dpsi_val)
        implicit none
        class(type_side_second), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(inout) :: dpsi_val
        real(real64) :: xi

        dpsi_val = 0.0d0
        if (j == 1) then
            xi = r%x
            select case (i)
            case (1)
                dpsi_val = xi - 0.5d0
            case (2)
                dpsi_val = xi + 0.5d0
            case (3)
                dpsi_val = -2.0d0 * xi
            end select
        end if
    end subroutine calc_dpsi_side_second

    !>
    !> Calculates the Jacobian matrix.
    !>
    pure module subroutine calc_jacobian_side_second(self, r, node_coords, jac)
        implicit none
        class(type_side_second), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: r
        real(real64), intent(in) :: node_coords(:, :)
        real(real64), intent(inout) :: jac(:, :)

        real(real64) :: tangent_vec(3)

        call self%compute_tangent_vector(r, node_coords, tangent_vec)
        jac(1, 1) = sqrt(sum(tangent_vec**2))
    end subroutine calc_jacobian_side_second

    !>
    !> Checks if point is inside using Newton-Raphson.
    !>
    module subroutine is_in_side_second(self, cartesian, normalized, node_coords, is_in)
        implicit none
        class(type_side_second), intent(in) :: self
        type(type_coordinate_dp), intent(in) :: cartesian
        type(type_coordinate_dp), intent(inout) :: normalized
        real(real64), intent(in) :: node_coords(:, :)
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
        integer(int32) :: nn
        logical :: converged
        real(real64), parameter :: tol = 1.0e-9
        integer(int32), parameter :: max_iter = 10

        call r_local%set(0.0d0, 0.0d0, 0.0d0)
        call self%get_num_nodes(nn)
        converged = .false.

        do iter = 1, max_iter
            call pos_guess%set(0.0d0, 0.0d0, 0.0d0)
            do i = 1, nn
                call self%calc_psi(i, r_local, psi_val)
                pos_guess%x = pos_guess%x + psi_val * node_coords(1, i)
                pos_guess%y = pos_guess%y + psi_val * node_coords(2, i)
                pos_guess%z = pos_guess%z + psi_val * node_coords(3, i)
            end do

            residual_vec%x = cartesian%x - pos_guess%x
            residual_vec%y = cartesian%y - pos_guess%y
            residual_vec%z = cartesian%z - pos_guess%z
            residual_norm = sqrt(residual_vec%x**2 + residual_vec%y**2 + residual_vec%z**2)

            if (residual_norm < tol) then
                converged = .true.
                exit
            end if

            call self%compute_tangent_vector(r_local, node_coords, tangent_vec)
            tangent_dot_tangent = tangent_vec(1)**2 + tangent_vec(2)**2 + tangent_vec(3)**2

            if (tangent_dot_tangent < epsilon(tangent_dot_tangent)) then
                is_in = .false.
                return
            end if

            r_local%x = r_local%x + (tangent_vec(1) * residual_vec%x + &
                                     tangent_vec(2) * residual_vec%y + &
                                     tangent_vec(3) * residual_vec%z) / tangent_dot_tangent
        end do

        is_in = converged .and. (abs(r_local%x) <= 1.0d0 + tol)
        if (is_in) then
            normalized = r_local
        end if

    end subroutine is_in_side_second

end submodule domain_fe_side_second

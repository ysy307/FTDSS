submodule(Domain_Element) Domain_Element_triangle_second
    implicit none
contains
    !----------------------------------------------------------------------!
    ! construct_triangle_second:
    !----------------------------------------------------------------------!
    ! This function constructs a type_triangle_second element object based on the
    ! given element index, global nodal coordinates, connectivity, and
    ! spatial dimension type.
    !
    ! Arguments:
    !   iElem             : Element index (int32).
    !                       Identifies the target element.
    !
    !   Global_Coordinate : type_dp_3d type pointer containing the global coordinates
    !                       of all nodes in the mesh.
    !
    !   connectivity      : Integer array (size 6) specifying the indices of
    !                       nodes that form the triangular element.
    !
    ! Return Value:
    !   element         : Allocated polymorphic object of type
    !                       type_triangle_second (extends Abstract_ElementType).
    !
    ! Function Details:
    !   - Allocates a new type_triangle_second element object.
    !   - Stores element ID and connectivity information.
    !   - Links to the corresponding global coordinates for each node.
    !   - Initializes Gauss point and weight for integration.
    !
    !----------------------------------------------------------------------!
    module function construct_triangle_second(id, global_coordinate, cell_info) result(element)
        implicit none
        integer(int32), intent(in) :: id
        type(type_dp_3d), pointer, intent(in) :: global_coordinate
        type(type_vtk_cell), intent(in) :: cell_info
        class(abst_element), allocatable :: element

        integer(int32) :: i

        if (allocated(element)) deallocate (element)
        allocate (type_triangle_second :: element)

        element%id = id
        element%type = cell_info%cell_type
        element%group = cell_info%cell_entity_id
        element%dimension = cell_info%get_dimension()
        element%order = cell_info%get_order()

        element%num_nodes = cell_info%num_nodes_in_cell
        allocate (element%connectivity(element%num_nodes))
        element%connectivity(:) = cell_info%connectivity(1:element%num_nodes)

        allocate (element%x(element%num_nodes))
        allocate (element%y(element%num_nodes))
        allocate (element%z(element%num_nodes))
        do i = 1, element%num_nodes
            nullify (element%x(i)%val)
            nullify (element%y(i)%val)
            nullify (element%z(i)%val)
            element%x(i)%val => global_coordinate%x(element%connectivity(i))
            element%y(i)%val => global_coordinate%y(element%connectivity(i))
            element%z(i)%val => global_coordinate%z(element%connectivity(i))
        end do

        element%num_gauss = 3_int32
        call allocate_array(element%weight, element%num_gauss)
        call allocate_array(element%gauss, element%dimension, element%num_gauss)

        element%weight(:) = [1.0d0 / 6.0d0, 1.0d0 / 6.0d0, 1.0d0 / 6.0d0]
        element%gauss(:, 1) = [1.0d0 / 6.0d0, 1.0d0 / 6.0d0]
        element%gauss(:, 2) = [2.0d0 / 3.0d0, 1.0d0 / 6.0d0]
        element%gauss(:, 3) = [1.0d0 / 6.0d0, 2.0d0 / 3.0d0]

    end function construct_triangle_second

    !----------------------------------------------------------------------!
    ! getNumNodes_triangle_second:
    !----------------------------------------------------------------------!
    ! This function returns the number of nodes associated with a
    ! type_triangle_second element.
    !
    ! Arguments:
    !   self : type_triangle_second type object.
    !          Represents the current triangular element instance.
    !
    ! Return Value:
    !   n    : Integer (int32) indicating the number of nodes used by the
    !          element. This is typically 6 for a linear triangle.
    !
    ! Function Details:
    !   - Retrieves the value stored in `self%num_nodes`, which represents
    !     the number of nodes for the element.
    !
    !----------------------------------------------------------------------!

    module function get_id_triangle_second(self) result(id)
        implicit none
        class(type_triangle_second), intent(in) :: self
        integer(int32) :: id

        id = self%id
    end function get_id_triangle_second

    module function get_type_triangle_second(self) result(type)
        implicit none
        class(type_triangle_second), intent(in) :: self
        integer(int32) :: type

        type = self%type
    end function get_type_triangle_second

    module function get_num_nodes_triangle_second(self) result(num_nodes)
        implicit none
        class(type_triangle_second), intent(in) :: self
        integer(int32) :: num_nodes

        num_nodes = self%num_nodes
    end function get_num_nodes_triangle_second

    module function get_group_triangle_second(self) result(group)
        implicit none
        class(type_triangle_second), intent(in) :: self
        integer(int32) :: group

        group = self%group
    end function get_group_triangle_second

    module function get_dimension_triangle_second(self) result(dimension)
        implicit none
        class(type_triangle_second), intent(in) :: self
        integer(int32) :: dimension

        dimension = self%dimension
    end function get_dimension_triangle_second

    module function get_order_triangle_second(self) result(order)
        implicit none
        class(type_triangle_second), intent(in) :: self
        integer(int32) :: order

        order = self%order
    end function get_order_triangle_second

    module function get_num_gauss_triangle_second(self) result(num_gauss)
        implicit none
        class(type_triangle_second), intent(in) :: self
        integer(int32) :: num_gauss

        num_gauss = self%num_gauss
    end function get_num_gauss_triangle_second

    !----------------------------------------------------------------------!
    ! psi_triangle_second:
    !----------------------------------------------------------------------!
    ! This function evaluates the shape function ψ_i(ξ, η) for a linear
    ! triangular element at the given natural coordinates (ξ, η).
    !
    ! Arguments:
    !   self : type_triangle_second type object.
    !          Represents the triangular element for which the shape
    !          function is evaluated.
    !
    !   i    : Integer (int32), index of the shape function (i = 1 ~ 6).
    !          Each index corresponds to a vertex of the triangle.
    !
    !   xi   : Real(real64), the ξ coordinate in the natural coordinate
    !          system (barycentric or reference triangle).
    !
    !   eta  : Real(real64), the η coordinate in the natural coordinate system.
    !
    ! Return Value:
    !   psi  : Real(real64), value of the i-th shape function ψ_i at (ξ, η).
    !
    ! Function Details:
    !   - For a bilenar triangular element, the shape functions are:
    !       ψ₁(ξ, η) = ξ * (2 * ξ -1)
    !       ψ₂(ξ, η) = η
    !       ψ₃(ξ, η) = 1 - ξ - η
    !       ψ₄(ξ, η) = 4 * ξ * η
    !       ψ₅(ξ, η) = 4 * (1 - ξ - η) * η
    !       ψ₆(ξ, η) = 4 * ξ * (1 - ξ - η)
    !   - Returns 0.0d0 for indices outside the range [1, 6].
    !
    !----------------------------------------------------------------------!
    module function psi_triangle_second(self, i, xi, eta) result(psi)
        implicit none
        class(type_triangle_second), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: xi, eta
        real(real64) :: psi
        select case (i)
        case (1)
            psi = xi * (2.0d0 * xi - 1.0d0)
        case (2)
            psi = eta * (2.0d0 * eta - 1.0d0)
        case (3)
            psi = (1.0d0 - xi - eta) * (1.0d0 - 2.0d0 * xi - 2.0d0 * eta)
        case (4)
            psi = 4.0d0 * xi * eta
        case (5)
            psi = 4.0d0 * (1.0d0 - xi - eta) * eta
        case (6)
            psi = 4.0d0 * xi * (1.0d0 - xi - eta)
        case default
            psi = 0.0d0
        end select
    end function psi_triangle_second

    !----------------------------------------------------------------------!
    ! dpsi_dxi_triangle_second:
    !----------------------------------------------------------------------!
    ! This function evaluates the partial derivative ∂ψ_i/∂ξ of the i-th
    ! shape function for a linear triangular element with respect to ξ
    ! at a given η coordinate.
    !
    ! Arguments:
    !   self : type_triangle_second type object.
    !          Represents the triangular element for which the derivative
    !          is being evaluated.
    !
    !   i    : Integer (int32), index of the shape function (i = 1 ~ 6).
    !
    !   eta  : Real(real64), the η coordinate in the natural coordinate
    !          system.
    !
    ! Return Value:
    !   dpsi : Real(real64), value of ∂ψ_i/∂ξ evaluated at (ξ, η).
    !
    ! Function Details:
    !   - For a linear triangle element:
    !       ∂ψ₁/∂ξ =  4 * ξ - 1
    !       ∂ψ₂/∂ξ =  0
    !       ∂ψ₃/∂ξ =  4 * ξ + 4 * η - 3
    !       ∂ψ₄/∂ξ =  4 * η
    !       ∂ψ₅/∂ξ = -4 * η
    !       ∂ψ₆/∂ξ =  4 - 8 * ξ - 4 * η
    !   - Returns 0.0 for indices outside [1, 6].
    !
    !----------------------------------------------------------------------!
    module function dpsi_dxi_triangle_second(self, i, xi, eta) result(dpsi)
        implicit none
        class(type_triangle_second), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: xi, eta
        real(real64) :: dpsi

        select case (i)
        case (1)
            dpsi = 4.0d0 * xi - 1.0d0
        case (2)
            dpsi = 0.0d0
        case (3)
            dpsi = -3.0d0 + 4.0d0 * xi + 4.0d0 * eta
        case (4)
            dpsi = 4.0d0 * eta
        case (5)
            dpsi = -4.0d0 * eta
        case (6)
            dpsi = 4.0d0 - 8.0d0 * xi - 4.0d0 * eta
        case default
            dpsi = 0.0d0
        end select
    end function dpsi_dxi_triangle_second

    !----------------------------------------------------------------------!
    ! dpsi_deta_triangle_second:
    !----------------------------------------------------------------------!
    ! This function evaluates the partial derivative ∂ψ_i/∂η of the i-th
    ! shape function for a linear triangular element with respect to η
    ! at a given ξ coordinate.
    !
    ! Arguments:
    !   self : type_triangle_second type object.
    !          Represents the triangular element for which the derivative
    !          is being evaluated.
    !
    !   i    : Integer (int32), index of the shape function (i = 1 ~ 6).
    !
    !   xi   : Real(real64), the ξ coordinate in the natural coordinate
    !          system.
    !
    !   eta  : Real(real64), the η coordinate in the natural coordinate
    !          system.
    !
    ! Return Value:
    !   dpsi : Real(real64), value of ∂ψ_i/∂η evaluated at (ξ, η).
    !
    ! Function Details:
    !   - For a linear triangle element:
    !       ∂ψ₁/∂η = 0
    !       ∂ψ₂/∂η = 4 * η - 1
    !       ∂ψ₃/∂η = 4 * η + 4 * ξ - 3
    !       ∂ψ₄/∂η = 4 * ξ
    !       ∂ψ₅/∂η = 4 * (1 - ξ - η)
    !       ∂ψ₆/∂η = -4 * ξ
    !   - Returns 0.0d0 for indices outside [1, 6].
    !
    !----------------------------------------------------------------------!
    module function dpsi_deta_triangle_second(self, i, xi, eta) result(dpsi)
        implicit none
        class(type_triangle_second), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: xi, eta
        real(real64) :: dpsi

        select case (i)
        case (1)
            dpsi = 0.0d0
        case (2)
            dpsi = 4.0d0 * eta - 1.0d0
        case (3)
            dpsi = -3.0d0 + 4.0d0 * eta + 4.0d0 * xi
        case (4)
            dpsi = 4.0d0 * xi
        case (5)
            dpsi = 4.0d0 - 4.0d0 * xi - 8.0d0 * eta
        case (6)
            dpsi = -4.0d0 * xi
        case default
            dpsi = 0.0d0
        end select
    end function dpsi_deta_triangle_second

    !----------------------------------------------------------------------!
    ! jacobian_triangle_second:
    !----------------------------------------------------------------------!
    ! This function computes the (i,j) component of the jacobian matrix J
    ! for a linear triangular finite element at a given natural coordinate
    ! (ξ, η). The jacobian maps natural coordinates (ξ, η) to physical
    ! coordinates (x, y).
    !
    ! Arguments:
    !   self : type_triangle_second type object.
    !          Represents the element whose jacobian is being evaluated.
    !
    !   i    : Integer (int32), the row index of the jacobian component.
    !          i = 1 → corresponds to x-component (dx/dξ or dx/dη),
    !          i = 2 → corresponds to y-component (dy/dξ or dy/dη).
    !
    !   j    : Integer (int32), the column index of the jacobian component.
    !          j = 1 → partial derivative w.r.t ξ,
    !          j = 2 → partial derivative w.r.t η.
    !
    !   xi   : Real(real64), ξ coordinate in natural coordinate system.
    !
    !   eta  : Real(real64), η coordinate in natural coordinate system.
    !
    ! Return Value:
    !   Jval : Real(real64), the (i,j) component of the jacobian matrix.
    !
    ! Function Details:
    !   - The jacobian matrix J is a 2×2 matrix defined as:
    !         [ ∂x/∂ξ  ∂x/∂η ]
    !         [ ∂y/∂ξ  ∂y/∂η ]
    !
    !   - Each entry is computed as a weighted sum over the shape function
    !     derivatives with respect to ξ or η, multiplied by the physical
    !     coordinates (X or Y) of the element's nodes.
    !
    !   - The derivatives of shape functions are accessed via:
    !         self%dpsi_dxi(ii, eta)
    !         self%dpsi_deta(ii, xi)
    !
    !   - For example:
    !       ∂x/∂ξ = Σ (∂ψ_i/∂ξ) * x_i
    !       ∂y/∂η = Σ (∂ψ_i/∂η) * y_i
    !
    !   - This function supports 2D problems.
    !
    !----------------------------------------------------------------------!
    module function jacobian_triangle_second(self, i, j, xi, eta) result(Jval)
        implicit none
        class(type_triangle_second), intent(in) :: self
        integer(int32), intent(in) :: i, j
        real(real64), intent(in) :: xi, eta

        real(real64) :: Jval
        integer(int32) :: ii, jlocal

        Jval = 0
        !! dx
        select case (i)
        case (1)
            select case (j)
            case (1)
                !! dx_dxi
                do ii = 1, self%num_nodes
                    Jval = Jval + self%dpsi_dxi(ii, xi, eta) * self%x(ii)%val
                end do
            case (2)
                !! dx_deta
                do ii = 1, self%num_nodes
                    Jval = Jval + self%dpsi_deta(ii, xi, eta) * self%x(ii)%val
                end do
            end select

        !! dy
        case (2)
            select case (j)
            case (1)
                !! dy_dxi
                do ii = 1, self%num_nodes
                    Jval = Jval + self%dpsi_dxi(ii, xi, eta) * self%y(ii)%val
                end do
            case (2)
                !! dy_deta
                do ii = 1, self%num_nodes
                    Jval = Jval + self%dpsi_deta(ii, xi, eta) * self%y(ii)%val
                end do
            end select
        end select

    end function jacobian_triangle_second

    !----------------------------------------------------------------------!
    ! jacobian_det_triangle_second:
    !----------------------------------------------------------------------!
    ! This function computes the determinant of the jacobian matrix J
    ! for a linear triangular element at a specified point (ξ, η) in
    ! the natural coordinate system.
    !
    ! Arguments:
    !   self : type_triangle_second type object.
    !          Represents the finite element whose jacobian is evaluated.
    !
    !   xi   : Real(real64), ξ coordinate in the natural coordinate system.
    !
    !   eta  : Real(real64), η coordinate in the natural coordinate system.
    !
    ! Return Value:
    !   J_Det : Real(real64), the determinant of the jacobian matrix J.
    !
    ! Function Details:
    !   - The jacobian matrix J is a 2×2 matrix defined as:
    !         [ ∂x/∂ξ  ∂x/∂η ]
    !         [ ∂y/∂ξ  ∂y/∂η ]
    !
    !   - The determinant is calculated using:
    !         det(J) = (∂x/∂ξ)(∂y/∂η) - (∂x/∂η)(∂y/∂ξ)
    !
    !   - This determinant gives the area scaling factor for transformation
    !     from natural to physical coordinates and is used in numerical
    !     integration (e.g., Gauss quadrature) on the element.
    !
    !   - A zero or negative determinant typically indicates a problem
    !     with the element geometry (e.g., inverted element).
    !
    !----------------------------------------------------------------------!
    module function jacobian_det_triangle_second(self, xi, eta) result(J_Det)
        implicit none
        class(type_triangle_second), intent(in) :: self
        real(real64), intent(in) :: xi, eta
        real(real64) :: J_Det

        real(real64) :: dx_xi, dx_eta
        real(real64) :: dy_xi, dy_eta

        integer(int32) :: i

        dx_xi = self%jacobian(1, 1, xi, eta)
        dx_eta = self%jacobian(1, 2, xi, eta)
        dy_xi = self%jacobian(2, 1, xi, eta)
        dy_eta = self%jacobian(2, 2, xi, eta)

        J_Det = dx_xi * dy_eta - dx_eta * dy_xi
    end function jacobian_det_triangle_second

    !----------------------------------------------------------------------!
    ! is_in_triangle_second:
    !----------------------------------------------------------------------!
    ! This function checks if the given physical coordinates (px, py) lie
    ! within the boundaries of a square element.
    ! The function uses a reverse mapping (Newton-Raphson method) to map
    ! the physical coordinates to natural coordinates (ξ, η) and then
    ! checks if the point lies within the square element.
    !
    ! Arguments:
    !   self  : type_triangle_second type object.
    !
    !   px    : x-coordinate (real64 type) in the physical coordinate system.
    !           This coordinate is checked to see if it lies inside the square element.
    !
    !   py    : y-coordinate (real64 type) in the physical coordinate system.
    !           This coordinate is checked to see if it lies inside the square element.
    !
    ! Return Value:
    !   is_in : .true. if the point lies within the square element,
    !           .false. otherwise.
    !           The function also returns .false. if the Newton-Raphson method
    !           does not converge or if the natural coordinates fall outside
    !           the square element's domain.
    !
    ! Algorithm:
    !   - The function uses the Newton-Raphson method to map the physical
    !     coordinates (px, py) to the natural coordinates (ξ, η).
    !   - The function then checks if the natural coordinates (ξ, η) are
    !     within the valid range [-1, 1]. If they are, the point is inside
    !     the square element.
    !   - If the method does not converge, or the natural coordinates fall
    !     outside the valid range, the function returns .false.
    !
    !----------------------------------------------------------------------!
    module subroutine is_in_triangle_second(self, px, py, pxi, peta, is_in)
        class(type_triangle_second), intent(in) :: self
        real(real64), intent(in) :: px, py
        real(real64), intent(inout) :: pxi, peta
        logical, intent(inout) :: is_in

        real(real64) :: xi, eta
        real(real64) :: x0, y0
        real(real64) :: dx_xi, dx_eta, dy_xi, dy_eta
        real(real64) :: detJ
        real(real64) :: dx, dy
        integer(int32) :: iter, max_iter
        real(real64) :: tol
        integer(int32) :: i
        logical :: converged

        ! 初期化
        xi = 0.0d0
        eta = 0.0d0
        tol = 1.0d-15
        max_iter = 100
        converged = .false.

        ! Newton-Raphson 法による逆写像
        do iter = 1, max_iter
            x0 = 0.0d0
            y0 = 0.0d0

            do i = 1, self%num_nodes
                x0 = x0 + self%psi(i, xi, eta) * self%x(i)%val
                y0 = y0 + self%psi(i, xi, eta) * self%y(i)%val
            end do

            dx = px - x0
            dy = py - y0

            if (sqrt(dx * dx + dy * dy) < tol) then
                converged = .true.
                exit
            end if

            dx_xi = self%jacobian(1, 1, xi, eta)
            dx_eta = self%jacobian(1, 2, xi, eta)
            dy_xi = self%jacobian(2, 1, xi, eta)
            dy_eta = self%jacobian(2, 2, xi, eta)

            detJ = self%jacobian_Det(xi, eta)
            if (abs(detJ) < 1.0d-20) exit ! ヤコビ行列の特異性チェック

            ! Newton-Raphson 更新
            xi = xi + (dy_eta * dx - dx_eta * dy) / detJ
            eta = eta + (-dy_xi * dx + dx_xi * dy) / detJ
        end do

        ! 最終判定：収束かつ自然座標が範囲内
        is_in = converged .and. (xi >= 0.0d0) .and. (eta >= 0.0d0) .and. (xi + eta <= 1.0d0)

        if (is_in) then
            pxi = xi
            peta = eta
        end if
    end subroutine is_in_triangle_second

    module function Interpolate_triangle_second(self, xi, eta, value) result(interpolated_value)
        implicit none
        class(type_triangle_second), intent(in) :: self
        real(real64), intent(in) :: xi, eta
        real(real64), intent(in) :: value(:)
        real(real64) :: interpolated_value
        integer(int32) :: i

        interpolated_value = 0.0d0
        do i = 1, self%num_nodes
            interpolated_value = interpolated_value + self%psi(i, xi, eta) * value(self%connectivity(i))
        end do

    end function Interpolate_triangle_second

end submodule Domain_Element_triangle_second

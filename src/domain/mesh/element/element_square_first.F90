submodule(domain_mesh_element) domain_mesh_element_square_first
    implicit none
contains

    !----------------------------------------------------------------------!
    ! construct_square_second:
    !----------------------------------------------------------------------!
    ! This function constructs a square_first element object based on the
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
    !   Connectivity      : Integer array (size 4) specifying the indices of
    !                       nodes that form the square element.
    !
    ! Return Value:
    !   element         : Allocated polymorphic object of type
    !                       square_first (extends Abstract_ElementType).
    !
    ! Function Details:
    !   - Allocates a new square_first element object.
    !   - Stores element ID and connectivity information.
    !   - Links to the corresponding global coordinates for each node.
    !   - Initializes Gauss point and weight for integration.
    !
    !----------------------------------------------------------------------!
    module function construct_square_first(id, global_coordinate, input) result(element)
        implicit none
        integer(int32), intent(in) :: id
        type(type_dp_3d), pointer, intent(in) :: global_coordinate
        type(type_input), intent(in) :: input
        class(abst_element), allocatable :: element

        integer(int32) :: i
        integer(int32) :: num_nodes, num_gauss
        real(real64), allocatable :: weight(:)
        real(real64), allocatable :: gauss(:, :)

        allocate (type_square_first :: element)

        num_nodes = input%geometry%vtk%cells(id)%num_nodes_in_cell

        ! Gauss点情報の準備
        select case (input%basic%geometry_settings%integration_type)
        case ("full")
            num_gauss = 4_int32
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, 3_int32, num_gauss)

            weight(:) = [1.0d0, 1.0d0, 1.0d0, 1.0d0]
            gauss(:, 1) = [-sqrt(1.0d0 / 3.0d0), -sqrt(1.0d0 / 3.0d0), 0.0d0]
            gauss(:, 2) = [-sqrt(1.0d0 / 3.0d0), sqrt(1.0d0 / 3.0d0), 0.0d0]
            gauss(:, 3) = [sqrt(1.0d0 / 3.0d0), sqrt(1.0d0 / 3.0d0), 0.0d0]
            gauss(:, 4) = [sqrt(1.0d0 / 3.0d0), -sqrt(1.0d0 / 3.0d0), 0.0d0]
        case ("reduced")
            num_gauss = 1_int32
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, 3_int32, num_gauss)

            weight(:) = [4.0d0]
            gauss(:, 1) = [0.0d0, 0.0d0, 0.0d0]
        case ("free")
            num_gauss = 4_int32
            call allocate_array(weight, num_gauss)
            call allocate_array(gauss, 3_int32, num_gauss)

            weight(:) = [1.0d0, 1.0d0, 1.0d0, 1.0d0]
            gauss(:, 1) = [-input%basic%geometry_settings%integration_points, -input%basic%geometry_settings%integration_points, 0.0d0]
            gauss(:, 2) = [-input%basic%geometry_settings%integration_points, input%basic%geometry_settings%integration_points, 0.0d0]
            gauss(:, 3) = [input%basic%geometry_settings%integration_points, input%basic%geometry_settings%integration_points, 0.0d0]
            gauss(:, 4) = [input%basic%geometry_settings%integration_points, -input%basic%geometry_settings%integration_points, 0.0d0]
        end select

        call element%initialize(id=id, &
                                type=input%geometry%vtk%cells(id)%cell_type, &
                                group=input%geometry%vtk%cells(id)%cell_entity_id, &
                                dimension=input%geometry%vtk%cells(id)%get_dimension(), &
                                order=input%geometry%vtk%cells(id)%get_order(), &
                                num_nodes=num_nodes, &
                                connectivity=input%geometry%vtk%cells(id)%connectivity(1:num_nodes), &
                                num_gauss=num_gauss, &
                                weight=weight, &
                                gauss=gauss, &
                                global_coordinate=global_coordinate)

    end function construct_square_first

    !----------------------------------------------------------------------!
    ! get_area_square_first:
    !----------------------------------------------------------------------!
    pure module function get_area_square_first(self) result(area)
        implicit none
        class(type_square_first), intent(in) :: self
        real(real64) :: area

        type(type_dp_vector_3d) :: r
        real(real64), parameter :: gauss(2) = [-sqrt(1.0d0 / 3.0d0), sqrt(1.0d0 / 3.0d0)]
        integer(int32) :: i, j

        area = 0.0d0
        r%z = 0.0d0

        do i = 1, 2
            r%x = gauss(i)
            do j = 1, 2
                r%y = gauss(j)
                area = area + self%jacobian_det(r)
            end do
        end do

    end function get_area_square_first

    !----------------------------------------------------------------------!
    ! psi_square_first:
    !----------------------------------------------------------------------!
    ! This function evaluates the shape function ψ_i(ξ, η) for a linear
    ! square element at the given natural coordinates (ξ, η).
    !
    ! Arguments:
    !   self : square_first type object.
    !          Represents the square element for which the shape
    !          function is evaluated.
    !
    !   i    : Integer (int32), index of the shape function (i = 1 ~ 4).
    !          Each index corresponds to a vertex of the square.
    !
    !   xi   : Real(real64), the ξ coordinate in the natural coordinate
    !          system.
    !
    !   eta  : Real(real64), the η coordinate in the natural coordinate
    !          system.
    !
    ! Return Value:
    !   psi  : Real(real64), value of the i-th shape function ψ_i at (ξ, η).
    !
    ! Function Details:
    !   - For a linear square element, the shape functions are:
    !       ψ₁(ξ, η) = 0.25 * (1 - ξ) * (1 - η)
    !       ψ₂(ξ, η) = 0.25 * (1 + ξ) * (1 - η)
    !       ψ₃(ξ, η) = 0.25 * (1 + ξ) * (1 + η)
    !       ψ₄(ξ, η) = 0.25 * (1 - ξ) * (1 + η)
    !   - Returns 0.0d0 for indices outside the range [1, 4].
    !
    !----------------------------------------------------------------------!
    pure elemental module function psi_square_first(self, i, r) result(psi)
        implicit none
        class(type_square_first), intent(in) :: self
        integer(int32), intent(in) :: i
        type(type_dp_vector_3d), intent(in) :: r
        real(real64) :: psi

        select case (i)
        case (1)
            psi = 0.25d0 * (1.0d0 - r%x) * (1.0d0 - r%y)
        case (2)
            psi = 0.25d0 * (1.0d0 + r%x) * (1.0d0 - r%y)
        case (3)
            psi = 0.25d0 * (1.0d0 + r%x) * (1.0d0 + r%y)
        case (4)
            psi = 0.25d0 * (1.0d0 - r%x) * (1.0d0 + r%y)
        case default
            psi = 0.0d0
        end select
    end function psi_square_first

    !----------------------------------------------------------------------!
    ! dpsi_square_first:
    !----------------------------------------------------------------------!
    ! This function evaluates the partial derivative ∂ψ_i/∂ξ of the i-th
    ! shape function for a linear square element with respect to ξ
    ! at a given η coordinate.
    !
    ! Arguments:
    !   self : square_first type object.
    !          Represents the square element for which the derivative
    !          is being evaluated.
    !
    !   i    : Integer (int32), index of the shape function (i = 1 ~ 4).
    !
    !   xi   : Real(real64), the ξ coordinate in the natural coordinate
    !          system (not used in linear case, but included for interface).
    !
    !   eta  : Real(real64), the η coordinate in the natural coordinate
    !          system.
    !
    ! Return Value:
    !   dpsi : Real(real64), value of ∂ψ_i/∂ξ evaluated at (ξ, η).
    !
    ! Function Details:
    !   - For a linear square element:
    !       ∂ψ₁/∂ξ = -0.25 * (1 - η)
    !       ∂ψ₂/∂ξ =  0.25 * (1 - η)
    !       ∂ψ₃/∂ξ =  0.25 * (1 + η)
    !       ∂ψ₄/∂ξ = -0.25 * (1 + η)
    !       ∂ψ₁/∂η = -0.25 * (1 - ξ)
    !       ∂ψ₂/∂η = -0.25 * (1 + ξ)
    !       ∂ψ₃/∂η =  0.25 * (1 + ξ)
    !       ∂ψ₄/∂η =  0.25 * (1 - ξ)
    !   - Returns 0.0d0 for indices outside [1, 4].
    !
    !----------------------------------------------------------------------!
    pure elemental module function dpsi_square_first(self, i, j, r) result(dpsi)
        implicit none
        class(type_square_first), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_dp_vector_3d), intent(in) :: r
        real(real64) :: dpsi

        select case (j)
        case (1)
            select case (i)
            case (1)
                dpsi = -0.25d0 * (1.0d0 - r%y)
            case (2)
                dpsi = 0.25d0 * (1.0d0 - r%y)
            case (3)
                dpsi = 0.25d0 * (1.0d0 + r%y)
            case (4)
                dpsi = -0.25d0 * (1.0d0 + r%y)
            case default
                dpsi = 0.0d0
            end select
        case (2)
            select case (i)
            case (1)
                dpsi = -0.25d0 * (1.0d0 - r%x)
            case (2)
                dpsi = -0.25d0 * (1.0d0 + r%x)
            case (3)
                dpsi = 0.25d0 * (1.0d0 + r%x)
            case (4)
                dpsi = 0.25d0 * (1.0d0 - r%x)
            case default
                dpsi = 0.0d0
            end select
        end select
    end function dpsi_square_first

    !----------------------------------------------------------------------!
    ! jacobian_square_first:
    !----------------------------------------------------------------------!
    ! This function computes the (i,j) component of the jacobian matrix J
    ! for a linear square finite element at a given natural coordinate
    ! (ξ, η). The jacobian maps natural coordinates (ξ, η) to physical
    ! coordinates (x, y).
    !
    ! Arguments:
    !   self : square_first type object.
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
    !   jacobian : Real(real64), the (i,j) component of the jacobian matrix.
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
    !         self%dpsi(ii,1, eta)
    !         self%dpsi(ii,2, xi)
    !
    !   - For example:
    !       ∂x/∂ξ = Σ (∂ψ_i/∂ξ) * x_i
    !       ∂y/∂η = Σ (∂ψ_i/∂η) * y_i
    !
    !   - This function supports 2D problems.
    !
    !----------------------------------------------------------------------!
    pure elemental module function jacobian_square_first(self, i, j, r) result(jacobian)
        implicit none
        class(type_square_first), intent(in) :: self
        integer(int32), intent(in) :: i
        integer(int32), intent(in) :: j
        type(type_dp_vector_3d), intent(in) :: r
        real(real64) :: jacobian

        integer(int32) :: ii
        type(type_dp_vector_3d) :: coordinate

        jacobian = 0
        !! dx
        select case (i)
        case (1)
            select case (j)
            case (1)
                !! dx_dxi
                do ii = 1, self%get_num_nodes()
                    coordinate = self%get_coordinate(ii)
                    jacobian = jacobian + self%dpsi(ii, 1, r) * coordinate%x
                end do
            case (2)
                !! dx_deta
                do ii = 1, self%get_num_nodes()
                    coordinate = self%get_coordinate(ii)
                    jacobian = jacobian + self%dpsi(ii, 2, r) * coordinate%x
                end do
            end select

        !! dy
        case (2)
            select case (j)
            case (1)
                !! dy_dxi
                do ii = 1, self%get_num_nodes()
                    coordinate = self%get_coordinate(ii)
                    jacobian = jacobian + self%dpsi(ii, 1, r) * coordinate%y
                end do
            case (2)
                !! dy_deta
                do ii = 1, self%get_num_nodes()
                    coordinate = self%get_coordinate(ii)
                    jacobian = jacobian + self%dpsi(ii, 2, r) * coordinate%y
                end do
            end select
        end select

    end function jacobian_square_first

    !----------------------------------------------------------------------!
    ! jacobian_det_square_first:
    !----------------------------------------------------------------------!
    ! This function computes the determinant of the jacobian matrix J
    ! for a linear square element at a specified point (ξ, η) in
    ! the natural coordinate system.
    !
    ! Arguments:
    !   self : square_first type object.
    !          Represents the finite element whose jacobian is evaluated.
    !
    !   xi   : Real(real64), ξ coordinate in the natural coordinate system.
    !
    !   eta  : Real(real64), η coordinate in the natural coordinate system.
    !
    ! Return Value:
    !   jacobian_det : Real(real64), the determinant of the jacobian matrix J.
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
    pure elemental module function jacobian_det_square_first(self, r) result(jacobian_det)
        implicit none
        class(type_square_first), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: r
        real(real64) :: jacobian_det

        real(real64) :: dx_xi, dx_eta
        real(real64) :: dy_xi, dy_eta

        dx_xi  = self%jacobian(1, 1, r) !&
        dx_eta = self%jacobian(1, 2, r) !&
        dy_xi  = self%jacobian(2, 1, r) !&
        dy_eta = self%jacobian(2, 2, r) !&

        jacobian_det = dx_xi * dy_eta - dx_eta * dy_xi

    end function jacobian_det_square_first

    !--------------------------------------------------------------------------------------
    ! is_in_square_first:
    !--------------------------------------------------------------------------------------
    ! This subroutine checks if the given physical coordinates (px, py) lie
    ! within the boundaries of a square element.
    ! The subroutine uses a reverse mapping (Newton-Raphson method) to map
    ! the physical coordinates to natural coordinates (ξ, η) and then
    ! checks if the point lies within the square element.
    !
    ! Arguments:
    !   self  : square_first type object. Represents a square element.
    !           It contains the coordinates (X, Y, Z) and connectivity
    !           information (conn) of the element.
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
    !           The subroutine also returns .false. if the Newton-Raphson method
    !           does not converge or if the natural coordinates fall outside
    !           the square element's domain.
    !
    ! Algorithm:
    !   - The subroutine uses the Newton-Raphson method to map the physical
    !     coordinates (px, py) to the natural coordinates (ξ, η).
    !   - The subroutine then checks if the natural coordinates (ξ, η) are
    !     within the valid range [-1, 1]. If they are, the point is inside
    !     the square element.
    !   - If the method does not converge, or the natural coordinates fall
    !     outside the valid range, the subroutine returns .false.
    !
    !--------------------------------------------------------------------------------------
    module subroutine is_in_square_first(self, cartesian, normalized, is_in)
        class(type_square_first), intent(in) :: self
        type(type_dp_vector_3d), intent(in) :: cartesian
        type(type_dp_vector_3d), intent(inout) :: normalized
        logical, intent(inout) :: is_in

        type(type_dp_vector_3d) :: r
        type(type_dp_vector_3d) :: coordinate
        real(real64) :: x0, y0
        real(real64) :: dx_xi, dx_eta, dy_xi, dy_eta
        real(real64) :: detJ
        real(real64) :: dx, dy
        integer(int32) :: iter, max_iter
        real(real64) :: tol
        integer(int32) :: i
        logical :: converged

        ! 初期化
        call r%set(0.0d0, 0.0d0, 0.0d0)
        tol = 1.0d-15
        max_iter = 100
        converged = .false.

        ! Newton-Raphson 法による逆写像
        do iter = 1, max_iter
            x0 = 0.0d0
            y0 = 0.0d0

            do i = 1, self%get_num_nodes()
                coordinate = self%get_coordinate(i)
                x0 = x0 + self%psi(i, r) * coordinate%x
                y0 = y0 + self%psi(i, r) * coordinate%y
            end do

            dx = cartesian%x - x0
            dy = cartesian%y - y0

            if (sqrt(dx * dx + dy * dy) < tol) then
                converged = .true.
                exit
            end if

            dx_xi = self%jacobian(1, 1, r)
            dx_eta = self%jacobian(1, 2, r)
            dy_xi = self%jacobian(2, 1, r)
            dy_eta = self%jacobian(2, 2, r)

            detJ = self%jacobian_det(r)
            if (abs(detJ) < 1.0d-20) exit ! ヤコビ行列の特異性チェック

            ! Newton-Raphson 更新
            r%x = r%x + (dy_eta * dx - dx_eta * dy) / detJ
            r%y = r%y + (-dy_xi * dx + dx_xi * dy) / detJ
        end do

        ! 最終判定：収束かつ自然座標が範囲内
        is_in = converged .and. (abs(r%x) <= 1.0d0) .and. (abs(r%y) <= 1.0d0)
        if (is_in) normalized = r

    end subroutine is_in_square_first

end submodule domain_mesh_element_square_first


submodule(Core_Element) Core_Element_TriangleFirst
    implicit none
contains
    !----------------------------------------------------------------------!
    ! TriangleFirst_Construct:
    !----------------------------------------------------------------------!
    ! This function constructs a TriangleFirst element object based on the
    ! given element index, global nodal coordinates, connectivity, and
    ! spatial dimension type.
    !
    ! Arguments:
    !   iElem             : Element index (int32).
    !                       Identifies the target element.
    !
    !   Global_Coordinate : DP3d type pointer containing the global coordinates
    !                       of all nodes in the mesh.
    !
    !   Connectivity      : Integer array (size 3) specifying the indices of
    !                       nodes that form the triangular element.
    !
    ! Return Value:
    !   Structure         : Allocated polymorphic object of type
    !                       TriangleFirst (extends Abstract_ElementType).
    !
    ! Function Details:
    !   - Allocates a new TriangleFirst element object.
    !   - Stores element ID and connectivity information.
    !   - Links to the corresponding global coordinates for each node.
    !   - Initializes Gauss point and weight for integration.
    !
    !----------------------------------------------------------------------!
    module function TriangleFirst_Construct(iElem, Global_Coordinate, Connectivity) result(Structure)
        implicit none
        integer(int32), intent(in) :: iElem
        type(DP3d), pointer, intent(in) :: Global_Coordinate
        integer(int32), intent(in) :: Connectivity(3)
        class(Abstract_ElementType), allocatable :: Structure
        integer(int32), parameter :: ndim = 3
        integer(int32) :: i

        allocate (TriangleFirst :: Structure)
        Structure%ElementID = iElem
        Structure%ElementType = 5

        Structure%size = ndim
        allocate (Structure%conn(ndim))
        Structure%conn(:) = Connectivity(1:ndim)

        allocate (Structure%X(ndim))
        allocate (Structure%Y(ndim))
        allocate (Structure%Z(ndim))
        do i = 1, ndim
            nullify (Structure%X(i)%val)
            nullify (Structure%Y(i)%val)
            nullify (Structure%Z(i)%val)
            Structure%X(i)%val => Global_Coordinate%x(Structure%conn(i))
            Structure%Y(i)%val => Global_Coordinate%y(Structure%conn(i))
            Structure%Z(i)%val => Global_Coordinate%z(Structure%conn(i))
        end do

        Structure%nGauss = 1
        call Allocate_Array(Structure%weight, Structure%nGauss)
        call Allocate_Array(Structure%gauss, 2_int32, Structure%nGauss)
        Structure%weight(:) = [0.5d0]
        Structure%gauss(:, 1) = [1.0d0 / 3.0d0, 1.0d0 / 3.0d0]
    end function TriangleFirst_Construct

    !----------------------------------------------------------------------!
    ! getNumNodes_TriangleFirst:
    !----------------------------------------------------------------------!
    ! This function returns the number of nodes associated with a
    ! TriangleFirst element.
    !
    ! Arguments:
    !   self : TriangleFirst type object.
    !          Represents the current triangular element instance.
    !
    ! Return Value:
    !   n    : Integer (int32) indicating the number of nodes used by the
    !          element. This is typically 3 for a linear triangle.
    !
    ! Function Details:
    !   - Retrieves the value stored in `self%size`, which represents
    !     the number of nodes for the element.
    !
    !----------------------------------------------------------------------!
    module function getNumNodes_TriangleFirst(self) result(n)
        implicit none
        class(TriangleFirst), intent(in) :: self
        integer(int32) :: n

        n = self%size
    end function getNumNodes_TriangleFirst

    !----------------------------------------------------------------------!
    ! psi_TriangleFirst:
    !----------------------------------------------------------------------!
    ! This function evaluates the shape function ψ_i(ξ, η) for a linear
    ! triangular element at the given natural coordinates (ξ, η).
    !
    ! Arguments:
    !   self : TriangleFirst type object.
    !          Represents the triangular element for which the shape
    !          function is evaluated.
    !
    !   i    : Integer (int32), index of the shape function (i = 1 ~ 3).
    !          Each index corresponds to a vertex of the triangle.
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
    !   - For a linear triangular element, the shape functions are:
    !       ψ₁(ξ, η) = ξ
    !       ψ₂(ξ, η) = η
    !       ψ₃(ξ, η) = 1 - ξ - η
    !   - Returns 0.0d0 for indices outside the range [1, 3].
    !
    !----------------------------------------------------------------------!
    module function psi_TriangleFirst(self, i, xi, eta) result(psi)
        implicit none
        class(TriangleFirst), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: xi, eta
        real(real64) :: psi
        select case (i)
        case (1)
            psi = xi
        case (2)
            psi = eta
        case (3)
            psi = 1.0d0 - xi - eta
        case default
            psi = 0.0d0
        end select
    end function psi_TriangleFirst

    !----------------------------------------------------------------------!
    ! dpsi_dxi_TriangleFirst:
    !----------------------------------------------------------------------!
    ! This function evaluates the partial derivative ∂ψ_i/∂ξ of the i-th
    ! shape function for a linear triangular element with respect to ξ
    ! at a given η coordinate.
    !
    ! Arguments:
    !   self : TriangleFirst type object.
    !          Represents the triangular element for which the derivative
    !          is being evaluated.
    !
    !   i    : Integer (int32), index of the shape function (i = 1 ~ 3).
    !
    !
    !   xi   : Real(real64), the ξ coordinate in the natural coordinate
    !          system (not used in linear case, but included for interface).
    !
    !   eta  : Real(real64), the η coordinate in the natural coordinate
    !          system (not used in linear case, but included for interface).
    !
    ! Return Value:
    !   dpsi : Real(real64), value of ∂ψ_i/∂ξ evaluated at (ξ, η).
    !
    ! Function Details:
    !   - For a linear triangle element:
    !       ∂ψ₁/∂ξ =  1.0
    !       ∂ψ₂/∂ξ =  0.0
    !       ∂ψ₃/∂ξ = -1.0
    !   - Returns 0.0d0 for indices outside [1, 3].
    !
    !----------------------------------------------------------------------!
    module function dpsi_dxi_TriangleFirst(self, i, xi, eta) result(dpsi)
        implicit none
        class(TriangleFirst), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: xi, eta
        real(real64) :: dpsi
        select case (i)
        case (1)
            dpsi = 1.0d0
        case (2)
            dpsi = 0.0d0
        case (3)
            dpsi = -1.0d0
        case default
            dpsi = 0.0d0
        end select
    end function dpsi_dxi_TriangleFirst

    !----------------------------------------------------------------------!
    ! dpsi_deta_TriangleFirst:
    !----------------------------------------------------------------------!
    ! This function evaluates the partial derivative ∂ψ_i/∂η of the i-th
    ! shape function for a linear triangular element with respect to η
    ! at a given ξ coordinate.
    !
    ! Arguments:
    !   self : TriangleFirst type object.
    !          Represents the triangular element for which the derivative
    !          is being evaluated.
    !
    !   i    : Integer (int32), index of the shape function (i = 1, 2, 3).
    !
    !   xi   : Real(real64), the ξ coordinate in the natural coordinate
    !          system (not used in linear case, but included for interface).
    !
    !   eta  : Real(real64), the η coordinate in the natural coordinate
    !          system (not used in linear case, but included for interface).
    !
    ! Return Value:
    !   dpsi : Real(real64), value of ∂ψ_i/∂η evaluated at (ξ, η).
    !
    ! Function Details:
    !   - For a linear triangle element:
    !       ∂ψ₁/∂η =  0.0
    !       ∂ψ₂/∂η =  1.0
    !       ∂ψ₃/∂η = -1.0
    !   - Returns 0.0d0 for indices outside [1, 3].
    !
    !----------------------------------------------------------------------!
    module function dpsi_deta_TriangleFirst(self, i, xi, eta) result(dpsi)
        implicit none
        class(TriangleFirst), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: xi, eta
        real(real64) :: dpsi
        select case (i)
        case (1)
            dpsi = 0.0d0
        case (2)
            dpsi = 1.0d0
        case (3)
            dpsi = -1.0d0
        case default
            dpsi = 0.0d0
        end select
    end function dpsi_deta_TriangleFirst

    !----------------------------------------------------------------------!
    ! Jac_TriangleFirst:
    !----------------------------------------------------------------------!
    ! This function computes the (i,j) component of the Jacobian matrix J
    ! for a linear triangular finite element at a given natural coordinate
    ! (ξ, η). The Jacobian maps natural coordinates (ξ, η) to physical
    ! coordinates (x, y).
    !
    ! Arguments:
    !   self : TriangleFirst type object.
    !          Represents the element whose Jacobian is being evaluated.
    !
    !   i    : Integer (int32), the row index of the Jacobian component.
    !          i = 1 → corresponds to x-component (dx/dξ or dx/dη),
    !          i = 2 → corresponds to y-component (dy/dξ or dy/dη).
    !
    !   j    : Integer (int32), the column index of the Jacobian component.
    !          j = 1 → partial derivative w.r.t ξ,
    !          j = 2 → partial derivative w.r.t η.
    !
    !   xi   : Real(real64), ξ coordinate in natural coordinate system.
    !
    !   eta  : Real(real64), η coordinate in natural coordinate system.
    !
    ! Return Value:
    !   Jval : Real(real64), the (i,j) component of the Jacobian matrix.
    !
    ! Function Details:
    !   - The Jacobian matrix J is a 2×2 matrix defined as:
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
    module function Jac_TriangleFirst(self, i, j, xi, eta) result(Jval)
        implicit none
        class(TriangleFirst), intent(in) :: self
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
                do ii = 1, self%size
                    Jval = Jval + self%dpsi_dxi(ii, xi, eta) * self%X(ii)%val
                end do
            case (2)
                !! dx_deta
                do ii = 1, self%size
                    Jval = Jval + self%dpsi_deta(ii, xi, eta) * self%X(ii)%val
                end do
            end select

        !! dy
        case (2)
            select case (j)
            case (1)
                !! dy_dxi
                do ii = 1, self%size
                    Jval = Jval + self%dpsi_dxi(ii, xi, eta) * self%Y(ii)%val
                end do
            case (2)
                !! dy_deta
                do ii = 1, self%size
                    Jval = Jval + self%dpsi_deta(ii, xi, eta) * self%Y(ii)%val
                end do
            end select
        end select

    end function Jac_TriangleFirst

    !----------------------------------------------------------------------!
    ! Jac_Det_TriangleFirst:
    !----------------------------------------------------------------------!
    ! This function computes the determinant of the Jacobian matrix J
    ! for a linear triangular element at a specified point (ξ, η) in
    ! the natural coordinate system.
    !
    ! Arguments:
    !   self : TriangleFirst type object.
    !          Represents the finite element whose Jacobian is evaluated.
    !
    !   xi   : Real(real64), ξ coordinate in the natural coordinate system.
    !
    !   eta  : Real(real64), η coordinate in the natural coordinate system.
    !
    ! Return Value:
    !   J_Det : Real(real64), the determinant of the Jacobian matrix J.
    !
    ! Function Details:
    !   - The Jacobian matrix J is a 2×2 matrix defined as:
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
    module function Jac_Det_TriangleFirst(self, xi, eta) result(J_Det)
        implicit none
        class(TriangleFirst), intent(in) :: self
        real(real64), intent(in) :: xi, eta
        real(real64) :: J_Det

        real(real64) :: dx_xi, dx_eta
        real(real64) :: dy_xi, dy_eta

        integer(int32) :: i

        dx_xi = self%Jac(1, 1, xi, eta)
        dx_eta = self%Jac(1, 2, xi, eta)
        dy_xi = self%Jac(2, 1, xi, eta)
        dy_eta = self%Jac(2, 2, xi, eta)

        J_Det = dx_xi * dy_eta - dx_eta * dy_xi
    end function Jac_Det_TriangleFirst

    !----------------------------------------------------------------------!
    ! is_in_TriangleFirst:
    !----------------------------------------------------------------------!
    ! This function checks if the given physical coordinates (px, py) lie
    ! within the boundaries of a square element.
    ! The function uses a reverse mapping (Newton-Raphson method) to map
    ! the physical coordinates to natural coordinates (ξ, η) and then
    ! checks if the point lies within the square element.
    !
    ! Arguments:
    !   self  : TriangleFirst type object.
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
    module subroutine is_in_TriangleFirst(self, px, py, pxi, peta, is_in)
        class(TriangleFirst), intent(in) :: self
        real(real64), intent(in) :: px, py
        real(real64), intent(inout) :: pxi, peta
        logical(4) :: is_in

        real(real64) :: xi, eta
        real(real64) :: x0, y0
        real(real64) :: dx_xi, dx_eta, dy_xi, dy_eta
        real(real64) :: detJ
        real(real64) :: dx, dy
        integer(int32) :: iter, max_iter
        real(real64) :: tol
        integer(int32) :: i
        logical(4) :: converged

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

            do i = 1, self%size
                x0 = x0 + self%psi(i, xi, eta) * self%X(i)%val
                y0 = y0 + self%psi(i, xi, eta) * self%Y(i)%val
            end do

            dx = px - x0
            dy = py - y0

            if (sqrt(dx * dx + dy * dy) < tol) then
                converged = .true.
                exit
            end if

            dx_xi = self%Jac(1, 1, xi, eta)
            dx_eta = self%Jac(1, 2, xi, eta)
            dy_xi = self%Jac(2, 1, xi, eta)
            dy_eta = self%Jac(2, 2, xi, eta)

            detJ = self%Jac_Det(xi, eta)
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
    end subroutine is_in_TriangleFirst

end submodule Core_Element_TriangleFirst

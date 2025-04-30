submodule(Solver_Element) Solver_Element_SquareFirst
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
contains

    !----------------------------------------------------------------
    ! SquareFirst のコンストラクタ
    ! iElem: 要素インデックス
    ! Global_Coordinate: 全節点座標を保持する DP3d
    ! Connectivity(4): 要素–節点接続
    ! DimensionType: 1: 2次元水平, 2: 2次元鉛直, 3: 3次元
    !----------------------------------------------------------------
    module function SquareFirst_Construct(iElem, Global_Coordinate, Connectivity, DimensionType) result(Structure)
        implicit none
        integer(int32), intent(in) :: iElem
        type(DP3d), intent(in), pointer :: Global_Coordinate
        integer(int32), intent(in) :: Connectivity(4)
        integer(int32), intent(in) :: DimensionType
        class(Abstract_ElementType), allocatable :: Structure
        integer(int32), parameter :: ndim = 4
        integer(int32) :: i

        allocate (SquareFirst :: Structure)
        Structure%ElementID = iElem
        Structure%ElementType = 9
        Structure%size = ndim
        allocate (Structure%conn(ndim))
        Structure%conn(1:ndim) = Connectivity(1:ndim)

        ! if (DimensionType == 1) then
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
        ! end if

        Structure%nGauss = 4
        call Allocate_Array(Structure%weight, Structure%nGauss)
        call Allocate_Array(Structure%gauss, 2_int32, Structure%nGauss)

        Structure%weight(:) = [1.0d0, 1.0d0, 1.0d0, 1.0d0]
        Structure%gauss(:, 1) = [-sqrt(1.0d0 / 3.0d0), -sqrt(1.0d0 / 3.0d0)]
        Structure%gauss(:, 2) = [-sqrt(1.0d0 / 3.0d0), sqrt(1.0d0 / 3.0d0)]
        Structure%gauss(:, 3) = [sqrt(1.0d0 / 3.0d0), sqrt(1.0d0 / 3.0d0)]
        Structure%gauss(:, 4) = [sqrt(1.0d0 / 3.0d0), -sqrt(1.0d0 / 3.0d0)]

    end function SquareFirst_Construct

    !---------------------------------------------------
    ! 四角形一次要素の節点数を返す
    !---------------------------------------------------
    module function getNumNodes_SquareFirst(self) result(n)
        implicit none
        class(SquareFirst), intent(in) :: self
        integer(int32) :: n

        n = self%size
    end function getNumNodes_SquareFirst

    !---------------------------------------------------
    ! 四角形線形要素の形状関数 N_i(ξ,η)
    !---------------------------------------------------
    module function psi_SquareFirst(self, i, xi, eta) result(psi)
        implicit none
        class(SquareFirst), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: xi, eta
        real(real64) :: psi

        select case (i)
        case (1)
            psi = 0.25d0 * (1.0d0 - xi) * (1.0d0 - eta)
        case (2)
            psi = 0.25d0 * (1.0d0 + xi) * (1.0d0 - eta)
        case (3)
            psi = 0.25d0 * (1.0d0 + xi) * (1.0d0 + eta)
        case (4)
            psi = 0.25d0 * (1.0d0 - xi) * (1.0d0 + eta)
        case default
            psi = 0.0d0
        end select
    end function psi_SquareFirst

    !---------------------------------------------------
    ! ∂N_i/∂ξ (四角形)
    !---------------------------------------------------
    module function dpsi_dxi_SquareFirst(self, i, eta) result(dpsi)
        implicit none
        class(SquareFirst), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: eta
        real(real64) :: dpsi

        select case (i)
        case (1)
            dpsi = -0.25d0 * (1.0d0 - eta)
        case (2)
            dpsi = 0.25d0 * (1.0d0 - eta)
        case (3)
            dpsi = 0.25d0 * (1.0d0 + eta)
        case (4)
            dpsi = -0.25d0 * (1.0d0 + eta)
        case default
            dpsi = 0.0d0
        end select
    end function dpsi_dxi_SquareFirst

    !---------------------------------------------------
    ! ∂N_i/∂η (四角形)
    !---------------------------------------------------
    module function dpsi_deta_SquareFirst(self, i, xi) result(dpsi)
        implicit none
        class(SquareFirst), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: xi
        real(real64) :: dpsi

        select case (i)
        case (1)
            dpsi = -0.25d0 * (1.0d0 - xi)
        case (2)
            dpsi = -0.25d0 * (1.0d0 + xi)
        case (3)
            dpsi = 0.25d0 * (1.0d0 + xi)
        case (4)
            dpsi = 0.25d0 * (1.0d0 - xi)
        case default
            dpsi = 0.0d0
        end select
    end function dpsi_deta_SquareFirst

    !---------------------------------------------------
    ! ∂x_i/∂ξ (四角形)
    !---------------------------------------------------
    module function Jac_SquareFirst(self, i, j, xi, eta) result(Jval)
        implicit none
        class(SquareFirst), intent(in) :: self
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
                    Jval = Jval + self%dpsi_dxi(ii, eta) * self%X(ii)%val
                end do
            case (2)
                !! dx_deta
                do ii = 1, self%size
                    Jval = Jval + self%dpsi_deta(ii, xi) * self%X(ii)%val
                end do
            end select

        !! dy
        case (2)
            select case (j)
            case (1)
                !! dy_dxi
                do ii = 1, self%size
                    Jval = Jval + self%dpsi_dxi(ii, eta) * self%Y(ii)%val
                end do
            case (2)
                !! dy_deta
                do ii = 1, self%size
                    Jval = Jval + self%dpsi_deta(ii, xi) * self%Y(ii)%val
                end do
            end select
        end select

    end function Jac_SquareFirst

    !---------------------------------------------------
    ! Jacobian Determinant (四角形)
    !---------------------------------------------------
    module function Jac_Det_SquareFirst(self, xi, eta) result(J_Det)
        implicit none
        class(SquareFirst), intent(in) :: self
        real(real64), intent(in) :: xi, eta
        real(real64) :: J_Det

        real(real64) :: dx_xi, dx_eta
        real(real64) :: dy_xi, dy_eta

        integer(int32) :: i

        dx_xi = 0.0d0
        dx_eta = 0.0d0
        dy_xi = 0.0d0
        dy_eta = 0.0d0

        dx_xi = self%Jac(1, 1, xi, eta)
        dx_eta = self%Jac(1, 2, xi, eta)
        dy_xi = self%Jac(2, 1, xi, eta)
        dy_eta = self%Jac(2, 2, xi, eta)

        J_Det = dx_xi * dy_eta - dx_eta * dy_xi

    end function Jac_Det_SquareFirst

    !--------------------------------------------------------------------------------------
    ! is_in_SquareFirst:
    !--------------------------------------------------------------------------------------
    ! This subroutine checks if the given physical coordinates (px, py) lie
    ! within the boundaries of a square element.
    ! The subroutine uses a reverse mapping (Newton-Raphson method) to map
    ! the physical coordinates to natural coordinates (ξ, η) and then
    ! checks if the point lies within the square element.
    !
    ! Arguments:
    !   self  : SquareFirst type object. Represents a square element.
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

    module subroutine is_in_SquareFirst(self, px, py, pxi, peta, is_in)
        class(SquareFirst), intent(in) :: self
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
        is_in = converged .and. (abs(xi) <= 1.0d0) .and. (abs(eta) <= 1.0d0)
        if (is_in) then
            pxi = xi
            peta = eta
        end if
    end subroutine is_in_SquareFirst

end submodule Solver_Element_SquareFirst


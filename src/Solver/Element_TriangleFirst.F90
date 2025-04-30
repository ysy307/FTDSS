submodule(Solver_Element) Solver_Element_TriangleFirst
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
contains
    !----------------------------------------------------------------
    ! TriangleFirst のコンストラクタ
    ! iElem: 要素インデックス
    ! Global_Coordinate: 全節点座標を保持する DP3d
    ! Connectivity(3): 要素–節点接続
    ! DimensionType: 1: 2次元水平, 2: 2次元鉛直, 3: 3次元
    !----------------------------------------------------------------
    module function TriangleFirst_Construct(iElem, Global_Coordinate, Connectivity, DimensionType) result(Structure)
        implicit none
        integer(int32), intent(in) :: iElem
        type(DP3d), pointer, intent(in) :: Global_Coordinate
        integer(int32), intent(in) :: Connectivity(3)
        integer(int32), intent(in) :: DimensionType
        class(Abstract_ElementType), allocatable :: Structure
        integer(int32), parameter :: ndim = 3
        integer(int32) :: i

        allocate (TriangleFirst :: Structure)
        Structure%ElementID = iElem
        Structure%ElementType = 5

        Structure%size = ndim
        allocate (Structure%conn(ndim))
        Structure%conn(:) = Connectivity(1:ndim)

        if (DimensionType == 1) then
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
        end if

        Structure%nGauss = 1
        call Allocate_Array(Structure%weight, Structure%nGauss)
        call Allocate_Array(Structure%gauss, 2_int32, Structure%nGauss)
        Structure%weight(:) = [0.5d0]
        Structure%gauss(:, 1) = [1.0d0 / 3.0d0, 1.0d0 / 3.0d0]
    end function TriangleFirst_Construct

    !---------------------------------------------------
    ! 三角形線形要素の節点数を返す
    !---------------------------------------------------
    module function getNumNodes_TriangleFirst(self) result(n)
        implicit none
        class(TriangleFirst), intent(in) :: self
        integer(int32) :: n

        n = self%size
    end function getNumNodes_TriangleFirst

    !---------------------------------------------------
    ! 三角形線形要素の形状関数 N_i(ξ,η)
    !---------------------------------------------------
    module function psi_TriangleFirst(self, i, xi, eta) result(N)
        implicit none
        class(TriangleFirst), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: xi, eta
        real(real64) :: N
        select case (i)
        case (1)
            N = xi
        case (2)
            N = eta
        case (3)
            N = 1.0d0 - xi - eta
        case default
            N = 0.0d0
        end select
    end function psi_TriangleFirst

    !---------------------------------------------------
    ! ∂N_i/∂ξ (三角形)
    !---------------------------------------------------
    module function dpsi_dxi_TriangleFirst(self, i, eta) result(dpsi)
        implicit none
        class(TriangleFirst), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: eta
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

    !---------------------------------------------------
    ! ∂N_i/∂η (三角形)
    !---------------------------------------------------
    module function dpsi_deta_TriangleFirst(self, i, xi) result(dpsi)
        implicit none
        class(TriangleFirst), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: xi
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

    !---------------------------------------------------
    ! J_{i, (三角形)
    !---------------------------------------------------
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

    end function Jac_TriangleFirst

    !---------------------------------------------------
    ! Jacobian Determinant (三角形)
    !---------------------------------------------------
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

    !--------------------------------------------------------------------
    !> 任意の多角形内判定（Ray-casting 法）
    !! pX, pY    : 判定したい点の座標
    !! 戻り値     : .true. = 内部, .false. = 外部
    !--------------------------------------------------------------------
    module function is_in_TriangleFirst(self, px, py) result(is_in)
        implicit none
        class(TriangleFirst), intent(in) :: self
        real(real64), intent(in) :: px, py
        logical(4) :: is_in

        integer(int32) :: i, j
        integer(int32) :: n

        is_in = .false.
        n = self%getNumNodes()
        j = n
        do i = 1, n
            if (((self%Y(i)%val > py) /= (self%Y(j)%val > py)) .and. &
                (px < (self%X(j)%val - self%X(i)%val) * (py - self%Y(i)%val) / (self%Y(j)%val - self%Y(i)%val) + self%X(i)%val)) then
                is_in = .not. is_in
            end if
            j = i
        end do

    end function is_in_TriangleFirst

end submodule Solver_Element_TriangleFirst

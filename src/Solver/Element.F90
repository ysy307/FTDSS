module Solver_Element
    !------------------------------------------------------------------------------
    !  Module: Solver_Element
    !  Purpose: Define 2D finite element types (square and triangle) and their
    !           associated operations (shape functions, Jacobian, Gauss points).
    !  Ford Coding Standard:
    !    - Use ISO_FORTRAN_ENV for portable kinds
    !    - Maintain explicit interfaces and consistent indentation
    !    - Preserve original function and type names
    !------------------------------------------------------------------------------
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    use :: Allocate_Allocate, only:Allocate_Array
    implicit none
    private
    public :: Abstract_ElementType_2D, SquareFirst, TriangleFirst, &
              ElementHolder, RealPointer

    !------------------------------------------------------------------------------
    ! Holder for polymorphic element objects
    !------------------------------------------------------------------------------
    type :: ElementHolder
        class(Abstract_ElementType_2D), allocatable :: p
    end type ElementHolder

    type :: RealPointer
        real(real64), pointer :: val => null()
    end type RealPointer

    !
    !----- 抽象基底型 -----
    !
    type, abstract :: Abstract_ElementType_2D
        integer(int32) :: ElementType ! 要素型
        integer(int32) :: size ! 節点数
        integer(int32), allocatable :: conn(:) ! 接続情報
        real(real64) :: det ! ヤコビアンの行列式
        type(RealPointer), allocatable :: coords_x(:)
        type(RealPointer), allocatable :: coords_y(:)
        integer(int32) :: nGauss ! ガウス積分点数
        real(real64), allocatable :: weight(:)
        real(real64), allocatable :: gauss(:, :)
    contains
        procedure(nNodes_if), deferred, pass :: getNumNodes
        procedure(shape_if), deferred :: shape
        procedure(shape_dxi_if), deferred :: shape_dxi
        procedure(shape_deta_if), deferred :: shape_deta
        procedure(Jacobian_Det_if), deferred :: Jacobian_Det
        procedure(Jacobian_Components_if), deferred :: Jacobian_Components
    end type Abstract_ElementType_2D

    !
    !----- 四角形要素型 -----
    !
    type, extends(Abstract_ElementType_2D) :: SquareFirst
    contains
        ! Type-Bound Procedure のバインド
        procedure, pass(self) :: getNumNodes => getNumNodesQuad
        procedure, pass(self) :: shape => shapeQuad
        procedure, pass(self) :: shape_dxi => shapeQuad_dxi
        procedure, pass(self) :: shape_deta => shapeQuad_deta
        procedure, pass(self) :: Jacobian_Det => Quad_Jacobian_Det
        procedure, pass(self) :: Jacobian_Components => QuadFirst_Jacobian_Components
    end type SquareFirst

    !
    !----- 三角形線形要素型 -----
    !
    type, extends(Abstract_ElementType_2D) :: TriangleFirst
    contains
        procedure, pass(self) :: getNumNodes => getNumNodesTri
        procedure, pass(self) :: shape => shapeTri
        procedure, pass(self) :: shape_dxi => shapeTri_dxi
        procedure, pass(self) :: shape_deta => shapeTri_deta
        procedure, pass(self) :: Jacobian_Det => Tri_Jacobian_Det
        procedure, pass(self) :: Jacobian_Components => TriFirst_Jacobian_Components
    end type TriangleFirst

    !
    !----- 抽象インターフェース定義 -----
    !
    abstract interface
        function nNodes_if(self) result(n)
            import :: Abstract_ElementType_2D, int32
            class(Abstract_ElementType_2D), intent(in) :: self
            integer(int32) :: n
        end function nNodes_if

        function shape_if(self, i, xi, eta) result(N)
            import :: Abstract_ElementType_2D, int32, real64
            class(Abstract_ElementType_2D), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi, eta
            real(real64) :: N
        end function shape_if

        function shape_dxi_if(self, i, eta) result(dN)
            import :: Abstract_ElementType_2D, int32, real64
            class(Abstract_ElementType_2D), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: eta
            real(real64) :: dN
        end function shape_dxi_if

        function shape_deta_if(self, i, xi) result(dN)
            import :: Abstract_ElementType_2D, int32, real64
            class(Abstract_ElementType_2D), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi
            real(real64) :: dN
        end function shape_deta_if

        function Jacobian_Det_if(self, xi, eta) result(det)
            import :: Abstract_ElementType_2D, int32, real64
            class(Abstract_ElementType_2D), intent(in) :: self
            real(real64), intent(in) :: xi, eta
            real(real64) :: det
        end function Jacobian_Det_if

        function Jacobian_Components_if(self, i, j, xi, eta) result(Jval)
            import :: Abstract_ElementType_2D, int32, real64
            class(Abstract_ElementType_2D), intent(in) :: self
            integer(int32), intent(in) :: i, j
            real(real64), intent(in) :: xi, eta

            real(real64) :: Jval
        end function Jacobian_Components_if
    end interface

    interface SquareFirst
        procedure :: SquareFirst_Construct
    end interface
    interface TriangleFirst
        procedure :: TriangleFirst_Construct
    end interface

contains
    !----------------------------------------------------------------
    ! SquareFirst のコンストラクタ
    ! iElem: 要素インデックス
    ! Global_Coordinate: 全節点座標を保持する DP3d
    ! Connectivity(4): 要素–節点接続
    ! DimensionType: 1: 2次元水平, 2: 2次元鉛直, 3: 3次元
    !----------------------------------------------------------------
    function SquareFirst_Construct(iElem, Global_Coordinate, Connectivity, DimensionType) result(Structure)
        integer(int32), intent(in) :: iElem
        type(DP3d), intent(in), pointer :: Global_Coordinate
        integer(int32), intent(in) :: Connectivity(4)
        integer(int32), intent(in) :: DimensionType
        class(Abstract_ElementType_2D), allocatable :: Structure
        integer(int32), parameter :: ndim = 4
        integer(int32) :: i

        allocate (SquareFirst :: Structure)
        Structure%ElementType = 9
        Structure%size = ndim
        allocate (Structure%conn(ndim))
        Structure%conn(1:ndim) = Connectivity(1:ndim)

        if (DimensionType == 1) then
            allocate (Structure%coords_x(ndim))
            allocate (Structure%coords_y(ndim))
            do i = 1, ndim
                nullify (Structure%coords_x(i)%val)
                nullify (Structure%coords_y(i)%val)
                Structure%coords_x(i)%val => Global_Coordinate%x(Structure%conn(i))
                Structure%coords_y(i)%val => Global_Coordinate%y(Structure%conn(i))
            end do
        end if

        Structure%det = 1.0_real64

        Structure%nGauss = 4
        call Allocate_Array(Structure%weight, Structure%nGauss)
        call Allocate_Array(Structure%gauss, 2_int32, Structure%nGauss)

        Structure%weight(:) = [1.0d0, 1.0d0, 1.0d0, 1.0d0]
        Structure%gauss(:, 1) = [-sqrt(1.0d0 / 3.0d0), -sqrt(1.0d0 / 3.0d0)]
        Structure%gauss(:, 2) = [-sqrt(1.0d0 / 3.0d0), sqrt(1.0d0 / 3.0d0)]
        Structure%gauss(:, 3) = [sqrt(1.0d0 / 3.0d0), sqrt(1.0d0 / 3.0d0)]
        Structure%gauss(:, 4) = [sqrt(1.0d0 / 3.0d0), -sqrt(1.0d0 / 3.0d0)]

    end function SquareFirst_Construct

    !----------------------------------------------------------------
    ! TriangleFirst のコンストラクタ
    ! iElem: 要素インデックス
    ! Global_Coordinate: 全節点座標を保持する DP3d
    ! Connectivity(3): 要素–節点接続
    ! DimensionType: 1: 2次元水平, 2: 2次元鉛直, 3: 3次元
    !----------------------------------------------------------------
    function TriangleFirst_Construct(iElem, Global_Coordinate, Connectivity, DimensionType) result(Structure)
        integer(int32), intent(in) :: iElem
        type(DP3d), pointer, intent(in) :: Global_Coordinate
        integer(int32), intent(in) :: Connectivity(3)
        integer(int32), intent(in) :: DimensionType
        class(Abstract_ElementType_2D), allocatable :: Structure
        integer(int32), parameter :: ndim = 3
        integer(int32) :: i

        allocate (TriangleFirst :: Structure)
        Structure%ElementType = 5

        Structure%size = ndim
        allocate (Structure%conn(ndim))
        Structure%conn(:) = Connectivity(1:ndim)

        if (DimensionType == 1) then
            allocate (Structure%coords_x(ndim))
            allocate (Structure%coords_y(ndim))
            do i = 1, ndim
                nullify (Structure%coords_x(i)%val)
                nullify (Structure%coords_y(i)%val)
                Structure%coords_x(i)%val => Global_Coordinate%x(Structure%conn(i))
                Structure%coords_y(i)%val => Global_Coordinate%y(Structure%conn(i))
            end do
        end if

        Structure%det = 1.0_real64

        Structure%nGauss = 1
        call Allocate_Array(Structure%weight, Structure%nGauss)
        call Allocate_Array(Structure%gauss, 2_int32, Structure%nGauss)
        Structure%weight(:) = [0.5d0]
        Structure%gauss(:, 1) = [1.0d0 / 3.0d0, 1.0d0 / 3.0d0]
    end function TriangleFirst_Construct

    !---------------------------------------------------
    ! 四角形要素の節点数を返す
    !---------------------------------------------------
    function getNumNodesQuad(self) result(n)
        class(SquareFirst), intent(in) :: self
        integer(int32) :: n
        n = self%size
    end function getNumNodesQuad

    !---------------------------------------------------
    ! 四角形線形要素の形状関数 N_i(ξ,η)
    !---------------------------------------------------
    function shapeQuad(self, i, xi, eta) result(N)
        class(SquareFirst), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: xi, eta
        real(real64) :: N
        select case (i)
        case (1)
            N = 0.25d0 * (1.0d0 - xi) * (1.0d0 - eta)
        case (2)
            N = 0.25d0 * (1.0d0 + xi) * (1.0d0 - eta)
        case (3)
            N = 0.25d0 * (1.0d0 + xi) * (1.0d0 + eta)
        case (4)
            N = 0.25d0 * (1.0d0 - xi) * (1.0d0 + eta)
        case default
            N = 0.0d0
        end select
    end function shapeQuad

    !---------------------------------------------------
    ! ∂N_i/∂ξ (四角形)
    !---------------------------------------------------
    function shapeQuad_dxi(self, i, eta) result(dN)
        class(SquareFirst), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: eta
        real(real64) :: dN
        select case (i)
        case (1)
            dN = -0.25d0 * (1.0d0 - eta)
        case (2)
            dN = 0.25d0 * (1.0d0 - eta)
        case (3)
            dN = 0.25d0 * (1.0d0 + eta)
        case (4)
            dN = -0.25d0 * (1.0d0 + eta)
        case default
            dN = 0.0d0
        end select
    end function shapeQuad_dxi

    !---------------------------------------------------
    ! ∂N_i/∂η (四角形)
    !---------------------------------------------------
    function shapeQuad_deta(self, i, xi) result(dN)
        class(SquareFirst), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: xi
        real(real64) :: dN
        select case (i)
        case (1)
            dN = -0.25d0 * (1.0d0 - xi)
        case (2)
            dN = -0.25d0 * (1.0d0 + xi)
        case (3)
            dN = 0.25d0 * (1.0d0 + xi)
        case (4)
            dN = 0.25d0 * (1.0d0 - xi)
        case default
            dN = 0.0d0
        end select
    end function shapeQuad_deta

    !---------------------------------------------------
    ! 三角形線形要素の節点数を返す
    !---------------------------------------------------
    function getNumNodesTri(self) result(n)
        class(TriangleFirst), intent(in) :: self
        integer(int32) :: n
        n = self%size
    end function getNumNodesTri

    !---------------------------------------------------
    ! 三角形線形要素の形状関数 N_i(ξ,η)
    !---------------------------------------------------
    function shapeTri(self, i, xi, eta) result(N)
        class(TriangleFirst), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: xi, eta
        real(real64) :: N
        select case (i)
        case (1)
            N = 1.0d0 - xi - eta
        case (2)
            N = xi
        case (3)
            N = eta
        case default
            N = 0.0d0
        end select
    end function shapeTri

    !---------------------------------------------------
    ! ∂N_i/∂ξ (三角形)
    !---------------------------------------------------
    function shapeTri_dxi(self, i, eta) result(dN)
        class(TriangleFirst), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: eta
        real(real64) :: dN
        select case (i)
        case (1)
            dN = -1.0d0
        case (2)
            dN = 1.0d0
        case (3)
            dN = 0.0d0
        case default
            dN = 0.0d0
        end select
    end function shapeTri_dxi

    !---------------------------------------------------
    ! ∂N_i/∂η (三角形)
    !---------------------------------------------------
    function shapeTri_deta(self, i, xi) result(dN)
        class(TriangleFirst), intent(in) :: self
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: xi
        real(real64) :: dN
        select case (i)
        case (1)
            dN = -1.0d0
        case (2)
            dN = 0.0d0
        case (3)
            dN = 1.0d0
        case default
            dN = 0.0d0
        end select
    end function shapeTri_deta

    !---------------------------------------------------
    ! Jacobian Determinant (三角形)
    !---------------------------------------------------
    function Tri_Jacobian_Det(self, xi, eta) result(Jacobian_Det)
        implicit none
        class(TriangleFirst), intent(in) :: self
        real(real64), intent(in) :: xi, eta
        real(real64) :: Jacobian_Det

        real(real64) :: dx_xi, dx_eta
        real(real64) :: dy_xi, dy_eta

        integer(int32) :: i

        dx_xi = 0.0d0
        dx_eta = 0.0d0
        dy_xi = 0.0d0
        dy_eta = 0.0d0

        do i = 1, self%size
            dx_xi = dx_xi + self%shape_dxi(i, eta) * self%coords_x(i)%val
            dx_eta = dx_eta + self%shape_deta(i, xi) * self%coords_x(i)%val
            dy_xi = dy_xi + self%shape_dxi(i, eta) * self%coords_y(i)%val
            dy_eta = dy_eta + self%shape_deta(i, xi) * self%coords_y(i)%val
        end do

        Jacobian_Det = dx_xi * dy_eta - dx_eta * dy_xi

    end function Tri_Jacobian_Det
    !---------------------------------------------------
    ! J_{i, (三角形)
    !---------------------------------------------------
    function TriFirst_Jacobian_Components(self, i, j, xi, eta) result(Jval)
        class(TriangleFirst), intent(in) :: self
        integer(int32), intent(in) :: i, j
        real(real64), intent(in) :: xi, eta

        real(real64) :: Jval
        integer(int32) :: isize, jlocal

        Jval = 0
        select case (i)
        case (1)
            select case (j)
            case (1)
                do isize = 1, self%size
                    Jval = Jval + self%shape_dxi(isize, eta) * self%coords_x(isize)%val
                end do
            case (2)
                do isize = 1, self%size
                    Jval = Jval + self%shape_deta(isize, xi) * self%coords_x(isize)%val
                end do
            end select
        case (2)
            select case (j)
            case (1)
                do isize = 1, self%size
                    Jval = Jval + self%shape_dxi(isize, eta) * self%coords_y(isize)%val
                end do
            case (2)
                do isize = 1, self%size
                    Jval = Jval + self%shape_deta(isize, xi) * self%coords_y(isize)%val
                end do
            end select
        end select

    end function TriFirst_Jacobian_Components
    !---------------------------------------------------
    ! ∂x_i/∂ξ (四角形)
    !---------------------------------------------------
    function QuadFirst_Jacobian_Components(self, i, j, xi, eta) result(Jval)
        class(SquareFirst), intent(in) :: self
        integer(int32), intent(in) :: i, j
        real(real64), intent(in) :: xi, eta

        real(real64) :: Jval
        integer(int32) :: isize, jlocal

        Jval = 0
        select case (i)
        case (1)
            select case (j)
            case (1)
                do isize = 1, self%size
                    Jval = Jval + self%shape_dxi(isize, eta) * self%coords_x(isize)%val
                end do
            case (2)
                do isize = 1, self%size
                    Jval = Jval + self%shape_deta(isize, xi) * self%coords_x(isize)%val
                end do
            end select
        case (2)
            select case (j)
            case (1)
                do isize = 1, self%size
                    Jval = Jval + self%shape_dxi(isize, eta) * self%coords_y(isize)%val
                end do
            case (2)
                do isize = 1, self%size
                    Jval = Jval + self%shape_deta(isize, xi) * self%coords_y(isize)%val
                end do
            end select
        end select

    end function QuadFirst_Jacobian_Components

    !---------------------------------------------------
    ! Jacobian Determinant (四角形)
    !---------------------------------------------------
    function Quad_Jacobian_Det(self, xi, eta) result(Jacobian_Det)
        implicit none
        class(SquareFirst), intent(in) :: self
        real(real64), intent(in) :: xi, eta
        real(real64) :: Jacobian_Det

        real(real64) :: dx_xi, dx_eta
        real(real64) :: dy_xi, dy_eta

        integer(int32) :: i

        dx_xi = 0.0d0
        dx_eta = 0.0d0
        dy_xi = 0.0d0
        dy_eta = 0.0d0

        do i = 1, self%size
            dx_xi = dx_xi + self%shape_dxi(i, eta) * self%coords_x(i)%val
            dx_eta = dx_eta + self%shape_deta(i, xi) * self%coords_x(i)%val
            dy_xi = dy_xi + self%shape_dxi(i, eta) * self%coords_y(i)%val
            dy_eta = dy_eta + self%shape_deta(i, xi) * self%coords_y(i)%val
        end do

        Jacobian_Det = dx_xi * dy_eta - dx_eta * dy_xi

    end function Quad_Jacobian_Det
    !---------------------------------------------------

end module Solver_Element

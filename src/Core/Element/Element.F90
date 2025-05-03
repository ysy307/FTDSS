module Core_Element
    !---------------------------------------------------------------------------------------
    !  Module: Core_Element
    !  Purpose: Define 2D finite element types (square and triangle) and their
    !           associated operations (shape functions, Jacobian, Gauss points).
    !  Ford Coding Standard:
    !    - Use ISO_FORTRAN_ENV for portable kinds
    !    - Maintain explicit interfaces and consistent indentation
    !    - Preserve original function and type names
    !--------------------------------------------------------------------------------------
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Core_BaseTypes, only:DP3d, RealPointer
    use :: Core_Allocate, only:Allocate_Array
    implicit none
    private

    public :: Abstract_ElementType
    public :: SquareFirst
    public :: TriangleFirst
    public :: ElementHolder

    !--------------------------------------------------------------------------------------
    ! Holder for polymorphic element objects
    !--------------------------------------------------------------------------------------
    type :: ElementHolder
        class(Abstract_ElementType), allocatable :: e
    end type ElementHolder

    !--------------------------------------------------------------------------------------
    !   Abstract base type for 2D elements
    !--------------------------------------------------------------------------------------
    type, abstract :: Abstract_ElementType
        integer(int32) :: ElementID
        integer(int32) :: ElementType ! Element type (5: triangle 1st, 9: square 1st)
        integer(int32) :: size ! Number of nodes in the element
        integer(int32), allocatable :: conn(:) !! connectivity information
        type(RealPointer), allocatable :: X(:) !! X coordinate
        type(RealPointer), allocatable :: Y(:) !! Y coordinate
        type(RealPointer), allocatable :: Z(:) !! Z coordinate

        !----------------------------------------------------------------------------------
        ! Gauss Quadrature points and weights
        !  - Gauss Quadrature points are defined in the local coordinate system
        !  - The number of Gauss points is determined by the element type
        !  - The weights are used for numerical integration over the element
        !  - The Gauss points are used to evaluate the shape functions and their derivatives
        !----------------------------------------------------------------------------------
        integer(int32) :: nGauss !! Number of Gauss Quadrature points
        real(real64), allocatable :: weight(:) !! Gauss weight
        real(real64), allocatable :: gauss(:, :) !! Gauss Quadrature points Coordinate
    contains
        procedure(Abstract_getNmNodes), pass(self), deferred :: getNumNodes !&
        procedure(Abstract_psi),        pass(self), deferred :: psi !&
        procedure(Abstract_dpsi_dxi),   pass(self), deferred :: dpsi_dxi !&
        procedure(Abstract_dpsi_deta),  pass(self), deferred :: dpsi_deta !&
        procedure(Abstract_Jac),        pass(self), deferred :: Jac !&
        procedure(Abstract_Jac_Det),    pass(self), deferred :: Jac_Det !&
        procedure(Abstract_is_inside),  pass(self), deferred :: is_inside !&
    end type Abstract_ElementType

    !--------------------------------------------------------------------------------------
    !   Triangle First Order Element Type
    !--------------------------------------------------------------------------------------
    type, extends(Abstract_ElementType) :: TriangleFirst
    contains
        procedure, pass(self) :: getNumNodes => getNumNodes_TriangleFirst !&
        procedure, pass(self) :: psi         => psi_TriangleFirst !&
        procedure, pass(self) :: dpsi_dxi    => dpsi_dxi_TriangleFirst !&
        procedure, pass(self) :: dpsi_deta   => dpsi_deta_TriangleFirst !&
        procedure, pass(self) :: Jac         => Jac_TriangleFirst !&
        procedure, pass(self) :: Jac_Det     => Jac_Det_TriangleFirst !&
        procedure, pass(self) :: is_inside   => is_in_TriangleFirst !&
    end type TriangleFirst

    !--------------------------------------------------------------------------------------
    !   Square First Order Element Type
    !--------------------------------------------------------------------------------------
    type, extends(Abstract_ElementType) :: SquareFirst
    contains
        procedure, pass(self) :: getNumNodes => getNumNodes_SquareFirst !&
        procedure, pass(self) :: psi         => psi_SquareFirst !&
        procedure, pass(self) :: dpsi_dxi    => dpsi_dxi_SquareFirst !&
        procedure, pass(self) :: dpsi_deta   => dpsi_deta_SquareFirst !&
        procedure, pass(self) :: Jac         => Jac_SquareFirst !&
        procedure, pass(self) :: Jac_Det     => Jac_Det_SquareFirst !&
        procedure, pass(self) :: is_inside   => is_in_SquareFirst !&
    end type SquareFirst

    !--------------------------------------------------------------------------------------
    !   Triangle Second Order Element Type
    !--------------------------------------------------------------------------------------
    type, extends(Abstract_ElementType) :: TriangleSecond
    contains
        procedure, pass(self) :: getNumNodes => getNumNodes_TriangleSecond !&
        procedure, pass(self) :: psi         => psi_TriangleSecond !&
        procedure, pass(self) :: dpsi_dxi    => dpsi_dxi_TriangleSecond !&
        procedure, pass(self) :: dpsi_deta   => dpsi_deta_TriangleSecond !&
        procedure, pass(self) :: Jac         => Jac_TriangleSecond !&
        procedure, pass(self) :: Jac_Det     => Jac_Det_TriangleSecond !&
        procedure, pass(self) :: is_inside   => is_in_TriangleSecond !&
    end type TriangleSecond

    !--------------------------------------------------------------------------------------
    !   Square Second Order Element Type
    !--------------------------------------------------------------------------------------
    type, extends(Abstract_ElementType) :: SquareSecond
    contains
        procedure, pass(self) :: getNumNodes => getNumNodes_SquareSecond !&
        procedure, pass(self) :: psi         => psi_SquareSecond !&
        procedure, pass(self) :: dpsi_dxi    => dpsi_dxi_SquareSecond !&
        procedure, pass(self) :: dpsi_deta   => dpsi_deta_SquareSecond !&
        procedure, pass(self) :: Jac         => Jac_SquareSecond !&
        procedure, pass(self) :: Jac_Det     => Jac_Det_SquareSecond !&
        procedure, pass(self) :: is_inside   => is_in_SquareSecond !&
    end type SquareSecond

    !
    !----- 抽象インターフェース定義 -----
    !
    abstract interface
        function Abstract_getNmNodes(self) result(n)
            import :: Abstract_ElementType, int32
            implicit none
            class(Abstract_ElementType), intent(in) :: self
            integer(int32) :: n
        end function Abstract_getNmNodes

        function Abstract_psi(self, i, xi, eta) result(psi)
            import :: Abstract_ElementType, int32, real64
            implicit none
            class(Abstract_ElementType), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi, eta
            real(real64) :: psi
        end function Abstract_psi

        function Abstract_dpsi_dxi(self, i, xi, eta) result(dpsi)
            import :: Abstract_ElementType, int32, real64
            implicit none
            class(Abstract_ElementType), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi, eta
            real(real64) :: dpsi
        end function Abstract_dpsi_dxi

        function Abstract_dpsi_deta(self, i, xi, eta) result(dpsi)
            import :: Abstract_ElementType, int32, real64
            implicit none
            class(Abstract_ElementType), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi, eta
            real(real64) :: dpsi
        end function Abstract_dpsi_deta

        function Abstract_Jac(self, i, j, xi, eta) result(Jval)
            import :: Abstract_ElementType, int32, real64
            implicit none
            class(Abstract_ElementType), intent(in) :: self
            integer(int32), intent(in) :: i, j
            real(real64), intent(in) :: xi, eta
            real(real64) :: Jval
        end function Abstract_Jac

        function Abstract_Jac_Det(self, xi, eta) result(J_Det)
            import :: Abstract_ElementType, int32, real64
            implicit none
            class(Abstract_ElementType), intent(in) :: self
            real(real64), intent(in) :: xi, eta
            real(real64) :: J_Det
        end function Abstract_Jac_Det

        subroutine Abstract_is_inside(self, px, py, pxi, peta, is_in)
            import Abstract_ElementType, real64
            implicit none
            class(Abstract_ElementType), intent(in) :: self
            real(real64), intent(in) :: px, py
            real(real64), intent(inout) :: pxi, peta
            logical(4) :: is_in
        end subroutine Abstract_is_inside

    end interface

    !--------------------------------------------------------------------------------------
    !   三角形一次要素型 procedures interface
    !--------------------------------------------------------------------------------------
    interface
        module function TriangleFirst_Construct(iElem, Global_Coordinate, Connectivity) result(Structure)
            implicit none
            integer(int32), intent(in) :: iElem
            type(DP3d), pointer, intent(in) :: Global_Coordinate
            integer(int32), intent(in) :: Connectivity(3)
            class(Abstract_ElementType), allocatable :: Structure

        end function TriangleFirst_Construct

        module function getNumNodes_TriangleFirst(self) result(n)
            implicit none
            class(TriangleFirst), intent(in) :: self
            integer(int32) :: n

        end function getNumNodes_TriangleFirst

        module function psi_TriangleFirst(self, i, xi, eta) result(N)
            implicit none
            class(TriangleFirst), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi, eta
            real(real64) :: N

        end function psi_TriangleFirst

        module function dpsi_dxi_TriangleFirst(self, i, xi, eta) result(dpsi)
            implicit none
            class(TriangleFirst), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi, eta
            real(real64) :: dpsi

        end function dpsi_dxi_TriangleFirst

        module function dpsi_deta_TriangleFirst(self, i, xi, eta) result(dpsi)
            implicit none
            class(TriangleFirst), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi, eta
            real(real64) :: dpsi

        end function dpsi_deta_TriangleFirst

        module function Jac_TriangleFirst(self, i, j, xi, eta) result(Jval)
            implicit none
            class(TriangleFirst), intent(in) :: self
            integer(int32), intent(in) :: i, j
            real(real64), intent(in) :: xi, eta
            real(real64) :: Jval

        end function Jac_TriangleFirst

        module function Jac_Det_TriangleFirst(self, xi, eta) result(J_Det)
            implicit none
            class(TriangleFirst), intent(in) :: self
            real(real64), intent(in) :: xi, eta
            real(real64) :: J_Det

        end function Jac_Det_TriangleFirst

        module subroutine is_in_TriangleFirst(self, px, py, pxi, peta, is_in)
            implicit none
            class(TriangleFirst), intent(in) :: self
            real(real64), intent(in) :: px, py
            real(real64), intent(inout) :: pxi, peta
            logical(4) :: is_in

        end subroutine is_in_TriangleFirst
    end interface

    !--------------------------------------------------------------------------------------
    !   四角形一次要素型 procedures interface
    !--------------------------------------------------------------------------------------
    interface
        module function SquareFirst_Construct(iElem, Global_Coordinate, Connectivity) result(Structure)
            implicit none
            integer(int32), intent(in) :: iElem
            type(DP3d), intent(in), pointer :: Global_Coordinate
            integer(int32), intent(in) :: Connectivity(4)
            class(Abstract_ElementType), allocatable :: Structure

        end function SquareFirst_Construct

        module function getNumNodes_SquareFirst(self) result(n)
            implicit none
            class(SquareFirst), intent(in) :: self
            integer(int32) :: n

        end function getNumNodes_SquareFirst

        module function psi_SquareFirst(self, i, xi, eta) result(psi)
            implicit none
            class(SquareFirst), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi, eta
            real(real64) :: psi

        end function psi_SquareFirst

        module function dpsi_dxi_SquareFirst(self, i, xi, eta) result(dpsi)
            implicit none
            class(SquareFirst), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi, eta
            real(real64) :: dpsi

        end function dpsi_dxi_SquareFirst

        module function dpsi_deta_SquareFirst(self, i, xi, eta) result(dpsi)
            implicit none
            class(SquareFirst), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi, eta
            real(real64) :: dpsi

        end function dpsi_deta_SquareFirst

        module function Jac_SquareFirst(self, i, j, xi, eta) result(Jval)
            implicit none
            class(SquareFirst), intent(in) :: self
            integer(int32), intent(in) :: i, j
            real(real64), intent(in) :: xi, eta
            real(real64) :: Jval

        end function Jac_SquareFirst

        module function Jac_Det_SquareFirst(self, xi, eta) result(J_Det)
            implicit none
            class(SquareFirst), intent(in) :: self
            real(real64), intent(in) :: xi, eta
            real(real64) :: J_Det

        end function Jac_Det_SquareFirst

        module subroutine is_in_SquareFirst(self, px, py, pxi, peta, is_in)
            implicit none
            class(SquareFirst), intent(in) :: self
            real(real64), intent(in) :: px, py
            real(real64), intent(inout) :: pxi, peta
            logical(4) :: is_in

        end subroutine is_in_SquareFirst
    end interface

    !--------------------------------------------------------------------------------------
    !   三角形二次要素型 procedures interface
    !--------------------------------------------------------------------------------------
    interface
        module function TriangleSecond_Construct(iElem, Global_Coordinate, Connectivity) result(Structure)
            implicit none
            integer(int32), intent(in) :: iElem
            type(DP3d), pointer, intent(in) :: Global_Coordinate
            integer(int32), intent(in) :: Connectivity(6)
            class(Abstract_ElementType), allocatable :: Structure

        end function TriangleSecond_Construct

        module function getNumNodes_TriangleSecond(self) result(n)
            implicit none
            class(TriangleSecond), intent(in) :: self
            integer(int32) :: n

        end function getNumNodes_TriangleSecond

        module function psi_TriangleSecond(self, i, xi, eta) result(N)
            implicit none
            class(TriangleSecond), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi, eta
            real(real64) :: N

        end function psi_TriangleSecond

        module function dpsi_dxi_TriangleSecond(self, i, xi, eta) result(dpsi)
            implicit none
            class(TriangleSecond), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi, eta
            real(real64) :: dpsi

        end function dpsi_dxi_TriangleSecond

        module function dpsi_deta_TriangleSecond(self, i, xi, eta) result(dpsi)
            implicit none
            class(TriangleSecond), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi, eta
            real(real64) :: dpsi

        end function dpsi_deta_TriangleSecond

        module function Jac_TriangleSecond(self, i, j, xi, eta) result(Jval)
            implicit none
            class(TriangleSecond), intent(in) :: self
            integer(int32), intent(in) :: i, j
            real(real64), intent(in) :: xi, eta
            real(real64) :: Jval

        end function Jac_TriangleSecond

        module function Jac_Det_TriangleSecond(self, xi, eta) result(J_Det)
            implicit none
            class(TriangleSecond), intent(in) :: self
            real(real64), intent(in) :: xi, eta
            real(real64) :: J_Det

        end function Jac_Det_TriangleSecond

        module subroutine is_in_TriangleSecond(self, px, py, pxi, peta, is_in)
            implicit none
            class(TriangleSecond), intent(in) :: self
            real(real64), intent(in) :: px, py
            real(real64), intent(inout) :: pxi, peta
            logical(4) :: is_in

        end subroutine is_in_TriangleSecond
    end interface

    !--------------------------------------------------------------------------------------
    !   四角形二次要素型 procedures interface
    !--------------------------------------------------------------------------------------
    interface
        module function SquareSecond_Construct(iElem, Global_Coordinate, Connectivity) result(Structure)
            implicit none
            integer(int32), intent(in) :: iElem
            type(DP3d), intent(in), pointer :: Global_Coordinate
            integer(int32), intent(in) :: Connectivity(8)
            class(Abstract_ElementType), allocatable :: Structure

        end function SquareSecond_Construct

        module function getNumNodes_SquareSecond(self) result(n)
            implicit none
            class(SquareSecond), intent(in) :: self
            integer(int32) :: n

        end function getNumNodes_SquareSecond

        module function psi_SquareSecond(self, i, xi, eta) result(psi)
            implicit none
            class(SquareSecond), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi, eta
            real(real64) :: psi

        end function psi_SquareSecond

        module function dpsi_dxi_SquareSecond(self, i, xi, eta) result(dpsi)
            implicit none
            class(SquareSecond), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi, eta
            real(real64) :: dpsi

        end function dpsi_dxi_SquareSecond

        module function dpsi_deta_SquareSecond(self, i, xi, eta) result(dpsi)
            implicit none
            class(SquareSecond), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi, eta
            real(real64) :: dpsi

        end function dpsi_deta_SquareSecond

        module function Jac_SquareSecond(self, i, j, xi, eta) result(Jval)
            implicit none
            class(SquareSecond), intent(in) :: self
            integer(int32), intent(in) :: i, j
            real(real64), intent(in) :: xi, eta
            real(real64) :: Jval

        end function Jac_SquareSecond

        module function Jac_Det_SquareSecond(self, xi, eta) result(J_Det)
            implicit none
            class(SquareSecond), intent(in) :: self
            real(real64), intent(in) :: xi, eta
            real(real64) :: J_Det

        end function Jac_Det_SquareSecond

        module subroutine is_in_SquareSecond(self, px, py, pxi, peta, is_in)
            implicit none
            class(SquareSecond), intent(in) :: self
            real(real64), intent(in) :: px, py
            real(real64), intent(inout) :: pxi, peta
            logical(4) :: is_in

        end subroutine is_in_SquareSecond
    end interface

    interface TriangleFirst
        module procedure :: TriangleFirst_Construct
    end interface

    interface SquareFirst
        module procedure :: SquareFirst_Construct
    end interface

    interface TriangleSecond
        module procedure :: TriangleSecond_Construct
    end interface

    interface SquareSecond
        module procedure :: SquareSecond_Construct
    end interface

end module Core_Element

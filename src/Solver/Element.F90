module Solver_Element
    !---------------------------------------------------------------------------------------
    !  Module: Solver_Element
    !  Purpose: Define 2D finite element types (square and triangle) and their
    !           associated operations (shape functions, Jacobian, Gauss points).
    !  Ford Coding Standard:
    !    - Use ISO_FORTRAN_ENV for portable kinds
    !    - Maintain explicit interfaces and consistent indentation
    !    - Preserve original function and type names
    !--------------------------------------------------------------------------------------
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    use :: Allocate_Allocate, only:Allocate_Array
    implicit none
    private
    public :: Abstract_ElementType
    public :: SquareFirst
    public :: TriangleFirst
    public :: ElementHolder
    public :: RealPointer

    !--------------------------------------------------------------------------------------
    ! Holder for polymorphic element objects
    !--------------------------------------------------------------------------------------
    type :: ElementHolder
        class(Abstract_ElementType), allocatable :: e
    end type ElementHolder

    !--------------------------------------------------------------------------------------
    ! Pointer type for real numbers
    !  - This is used to manage the memory of coorinate values in a polymorphic way
    !  - The pointer is initialized to null and can be associated with coorinate values
    !--------------------------------------------------------------------------------------
    type :: RealPointer
        real(real64), pointer :: val => null()
    end type RealPointer

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
        procedure(nNodes_if), pass(self), deferred :: getNumNodes
        procedure(shape_if), pass(self), deferred :: psi
        procedure(shape_dxi_if), pass(self), deferred :: dpsi_dxi
        procedure(shape_deta_if), pass(self), deferred :: dpsi_deta
        procedure(Jacobian_Components_if), pass(self), deferred :: Jac
        procedure(Jacobian_Det_if), pass(self), deferred :: Jac_Det
        procedure(is_in_if), pass(self), deferred :: is_inside
    end type Abstract_ElementType

    !--------------------------------------------------------------------------------------
    !   Triangle First Order Element Type
    !--------------------------------------------------------------------------------------
    type, extends(Abstract_ElementType) :: TriangleFirst
    contains
        procedure, pass(self) :: getNumNodes => getNumNodes_TriangleFirst
        procedure, pass(self) :: psi => psi_TriangleFirst
        procedure, pass(self) :: dpsi_dxi => dpsi_dxi_TriangleFirst
        procedure, pass(self) :: dpsi_deta => dpsi_deta_TriangleFirst
        procedure, pass(self) :: Jac => Jac_TriangleFirst
        procedure, pass(self) :: Jac_Det => Jac_Det_TriangleFirst
        procedure, pass(self) :: is_inside => is_in_TriangleFirst
    end type TriangleFirst

    !--------------------------------------------------------------------------------------
    !   Square First Order Element Type
    !--------------------------------------------------------------------------------------
    type, extends(Abstract_ElementType) :: SquareFirst
    contains
        procedure, pass(self) :: getNumNodes => getNumNodes_SquareFirst
        procedure, pass(self) :: psi => psi_SquareFirst
        procedure, pass(self) :: dpsi_dxi => dpsi_dxi_SquareFirst
        procedure, pass(self) :: dpsi_deta => dpsi_deta_SquareFirst
        procedure, pass(self) :: Jac => Jac_SquareFirst
        procedure, pass(self) :: Jac_Det => Jac_Det_SquareFirst
        procedure, pass(self) :: is_inside => is_in_SquareFirst
    end type SquareFirst

    !
    !----- 抽象インターフェース定義 -----
    !
    abstract interface
        function nNodes_if(self) result(n)
            import :: Abstract_ElementType, int32
            class(Abstract_ElementType), intent(in) :: self
            integer(int32) :: n
        end function nNodes_if

        function shape_if(self, i, xi, eta) result(psi)
            import :: Abstract_ElementType, int32, real64
            class(Abstract_ElementType), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi, eta
            real(real64) :: psi
        end function shape_if

        function shape_dxi_if(self, i, eta) result(dpsi)
            import :: Abstract_ElementType, int32, real64
            class(Abstract_ElementType), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: eta
            real(real64) :: dpsi
        end function shape_dxi_if

        function shape_deta_if(self, i, xi) result(dpsi)
            import :: Abstract_ElementType, int32, real64
            class(Abstract_ElementType), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi
            real(real64) :: dpsi
        end function shape_deta_if

        function Jacobian_Components_if(self, i, j, xi, eta) result(Jval)
            import :: Abstract_ElementType, int32, real64
            class(Abstract_ElementType), intent(in) :: self
            integer(int32), intent(in) :: i, j
            real(real64), intent(in) :: xi, eta

            real(real64) :: Jval
        end function Jacobian_Components_if

        function Jacobian_Det_if(self, xi, eta) result(J_Det)
            import :: Abstract_ElementType, int32, real64
            class(Abstract_ElementType), intent(in) :: self
            real(real64), intent(in) :: xi, eta
            real(real64) :: J_Det
        end function Jacobian_Det_if

        function is_in_if(self, px, py) result(is_in)
            import Abstract_ElementType, real64
            class(Abstract_ElementType), intent(in) :: self
            real(real64), intent(in) :: px, py
            logical(4) :: is_in
        end function is_in_if
    end interface

    !--------------------------------------------------------------------------------------
    !   三角形一次要素型 procedures interface
    !--------------------------------------------------------------------------------------
    interface
        module function TriangleFirst_Construct(iElem, Global_Coordinate, Connectivity, DimensionType) result(Structure)
            implicit none
            integer(int32), intent(in) :: iElem
            type(DP3d), pointer, intent(in) :: Global_Coordinate
            integer(int32), intent(in) :: Connectivity(3)
            integer(int32), intent(in) :: DimensionType
            class(Abstract_ElementType), allocatable :: Structure
            integer(int32), parameter :: ndim = 3
            integer(int32) :: i

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

        module function dpsi_dxi_TriangleFirst(self, i, eta) result(dpsi)
            implicit none
            class(TriangleFirst), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: eta
            real(real64) :: dpsi

        end function dpsi_dxi_TriangleFirst

        module function dpsi_deta_TriangleFirst(self, i, xi) result(dpsi)
            implicit none
            class(TriangleFirst), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi
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

        module function is_in_TriangleFirst(self, px, py) result(is_in)
            implicit none
            class(TriangleFirst), intent(in) :: self
            real(real64), intent(in) :: px, py
            logical(4) :: is_in

        end function is_in_TriangleFirst
    end interface

    !--------------------------------------------------------------------------------------
    !   四角形一次要素型 procedures interface
    !--------------------------------------------------------------------------------------
    interface
        module function SquareFirst_Construct(iElem, Global_Coordinate, Connectivity, DimensionType) result(Structure)
            implicit none
            integer(int32), intent(in) :: iElem
            type(DP3d), intent(in), pointer :: Global_Coordinate
            integer(int32), intent(in) :: Connectivity(4)
            integer(int32), intent(in) :: DimensionType
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

        module function dpsi_dxi_SquareFirst(self, i, eta) result(dpsi)
            implicit none
            class(SquareFirst), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: eta
            real(real64) :: dpsi

        end function dpsi_dxi_SquareFirst

        module function dpsi_deta_SquareFirst(self, i, xi) result(dpsi)
            implicit none
            class(SquareFirst), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi
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

        module function is_in_SquareFirst(self, px, py) result(is_in)
            implicit none
            class(SquareFirst), intent(in) :: self
            real(real64), intent(in) :: px, py
            logical(4) :: is_in

        end function is_in_SquareFirst
    end interface

    interface TriangleFirst
        procedure :: TriangleFirst_Construct
    end interface

    interface SquareFirst
        procedure :: SquareFirst_Construct
    end interface

contains

end module Solver_Element

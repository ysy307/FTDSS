module Core_Side
    !---------------------------------------------------------------------------------------
    !  Module: Core_Side
    !  Purpose: Define 1D finite element types (square and triangle) and their
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

    public :: SideHolder
    public :: Abstract_SideType
    public :: SideFirst
    public :: SideSecond

    !--------------------------------------------------------------------------------------
    ! Holder for polymorphic element objects
    !--------------------------------------------------------------------------------------
    type :: SideHolder
        class(Abstract_SideType), allocatable :: s
    end type SideHolder

    !--------------------------------------------------------------------------------------
    !   Abstract base type for 1D elements
    !--------------------------------------------------------------------------------------
    type, abstract :: Abstract_SideType
        integer(int32) :: SideID
        integer(int32) :: SideType ! Edge type
        integer(int32) :: SideGroup ! Group ID
        integer(int32) :: size ! Number of nodes in the Edge
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
        real(real64), allocatable :: gauss(:) !! Gauss Quadrature points Coordinate
    contains
        procedure(Abstract_getNmNodes), pass(self), deferred :: getNumNodes !&
        procedure(Abstract_psi),        pass(self), deferred :: psi !&
        procedure(Abstract_dpsi_dxi),   pass(self), deferred :: dpsi_dxi !&
        ! procedure(Jacobian_Components_if), pass(self), deferred :: Jac
        ! procedure(Jacobian_Det_if), pass(self), deferred :: Jac_Det
    end type Abstract_SideType

    !--------------------------------------------------------------------------------------
    !   Triangle First Order Element Type
    !--------------------------------------------------------------------------------------
    type, extends(Abstract_SideType) :: SideFirst
    contains
        procedure, pass(self) :: getNumNodes => getNumNodes_SideFirst !&
        procedure, pass(self) :: psi         => psi_SideFirst !&
        procedure, pass(self) :: dpsi_dxi    => dpsi_dxi_SideFirst !&
    end type SideFirst

    !--------------------------------------------------------------------------------------
    !   Triangle Second Order Element Type
    !--------------------------------------------------------------------------------------
    type, extends(Abstract_SideType) :: SideSecond
    contains
        procedure, pass(self) :: getNumNodes => getNumNodes_SideSecond !&
        procedure, pass(self) :: psi         => psi_SideSecond !&
        procedure, pass(self) :: dpsi_dxi    => dpsi_dxi_SideSecond !&
    end type SideSecond

    !--------------------------------------------------------------------------------------
    !  Abstract interface for the 1D element
    !--------------------------------------------------------------------------------------
    abstract interface
        function Abstract_getNmNodes(self) result(n)
            import :: Abstract_SideType, int32
            implicit none
            class(Abstract_SideType), intent(in) :: self
            integer(int32) :: n
        end function Abstract_getNmNodes

        function Abstract_psi(self, i, xi) result(psi)
            import :: Abstract_SideType, int32, real64
            implicit none
            class(Abstract_SideType), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi
            real(real64) :: psi
        end function Abstract_psi

        function Abstract_dpsi_dxi(self, i) result(dpsi)
            import :: Abstract_SideType, int32, real64
            implicit none
            class(Abstract_SideType), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64) :: dpsi
        end function Abstract_dpsi_dxi
    end interface

    !--------------------------------------------------------------------------------------
    !   Edge first order procedures interface
    !--------------------------------------------------------------------------------------
    interface
        module function SideFirst_Construct(iSide, Global_Coordinate, Connectivity, GroupID) result(Structure)
            implicit none
            integer(int32), intent(in) :: iSide
            type(DP3d), pointer, intent(in) :: Global_Coordinate
            integer(int32), intent(in) :: Connectivity(2)
            integer(int32), intent(in) :: GroupID
            class(Abstract_SideType), allocatable :: Structure

        end function SideFirst_Construct

        module function getNumNodes_SideFirst(self) result(n)
            implicit none
            class(SideFirst), intent(in) :: self
            integer(int32) :: n
        end function getNumNodes_SideFirst

        module function psi_SideFirst(self, i, xi) result(psi)
            implicit none
            class(SideFirst), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi
            real(real64) :: psi
        end function psi_SideFirst

        module function dpsi_dxi_SideFirst(self, i) result(dpsi)
            implicit none
            class(SideFirst), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64) :: dpsi
        end function dpsi_dxi_SideFirst
    end interface

    !--------------------------------------------------------------------------------------
    !   Edge Second order procedures interface
    !--------------------------------------------------------------------------------------
    interface
        module function SideSecond_Construct(iSide, Global_Coordinate, Connectivity, GroupID) result(Structure)
            implicit none
            integer(int32), intent(in) :: iSide
            type(DP3d), pointer, intent(in) :: Global_Coordinate
            integer(int32), intent(in) :: Connectivity(3)
            integer(int32), intent(in) :: GroupID
            class(Abstract_SideType), allocatable :: Structure

        end function SideSecond_Construct

        module function getNumNodes_SideSecond(self) result(n)
            implicit none
            class(SideSecond), intent(in) :: self
            integer(int32) :: n
        end function getNumNodes_SideSecond

        module function psi_SideSecond(self, i, xi) result(psi)
            implicit none
            class(SideSecond), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi
            real(real64) :: psi
        end function psi_SideSecond

        module function dpsi_dxi_SideSecond(self, i) result(dpsi)
            implicit none
            class(SideSecond), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64) :: dpsi
        end function dpsi_dxi_SideSecond
    end interface

    interface SideFirst
        procedure :: SideFirst_Construct
    end interface

    interface SideSecond
        procedure :: SideSecond_Construct
    end interface

end module Core_Side

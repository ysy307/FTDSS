module Domain_Side
    !---------------------------------------------------------------------------------------
    !  Module: Domain_Side
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

    public :: Abst_SideType
    public :: SideFirst
    public :: SideSecond
    public :: SideHolder

    !--------------------------------------------------------------------------------------
    ! Holder for polymorphic element objects
    !--------------------------------------------------------------------------------------
    type :: SideHolder
        class(Abst_SideType), allocatable :: s
    end type SideHolder

    !--------------------------------------------------------------------------------------
    !   Abstract base type for 1D elements
    !--------------------------------------------------------------------------------------
    type, abstract :: Abst_SideType
        integer(int32), private :: id
        integer(int32), private :: type ! Edge type
        integer(int32), private :: size ! Number of nodes in the Edge
        integer(int32), private :: group ! Group ID
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
        procedure(Abst_get_id),    pass(self), deferred :: get_id !&
        procedure(Abst_get_type),  pass(self), deferred :: get_type !&
        procedure(Abst_get_size),  pass(self), deferred :: get_size !&
        procedure(Abst_get_group), pass(self), deferred :: get_group !&
        !----------------------------------------------------------------------------------
        procedure(Abst_psi),       pass(self), deferred :: psi !&
        procedure(Abst_dpsi_dxi),  pass(self), deferred :: dpsi_dxi !&
    end type Abst_SideType

    !--------------------------------------------------------------------------------------
    !   Triangle First Order Element Type
    !--------------------------------------------------------------------------------------
    type, extends(Abst_SideType) :: SideFirst
    contains
        procedure, pass(self) :: get_id    => get_id_SideFirst !&
        procedure, pass(self) :: get_type  => get_type_SideFirst !&
        procedure, pass(self) :: get_size  => get_size_SideFirst !&
        procedure, pass(self) :: get_group => get_group_SideFirst !&
        !----------------------------------------------------------------------------------
        procedure, pass(self) :: psi       => psi_SideFirst !&
        procedure, pass(self) :: dpsi_dxi  => dpsi_dxi_SideFirst !&
    end type SideFirst

    !--------------------------------------------------------------------------------------
    !   Triangle Second Order Element Type
    !--------------------------------------------------------------------------------------
    type, extends(Abst_SideType) :: SideSecond
    contains
        procedure, pass(self) :: get_id    => get_id_SideSecond !&
        procedure, pass(self) :: get_type  => get_type_SideSecond !&
        procedure, pass(self) :: get_size  => get_size_SideSecond !&
        procedure, pass(self) :: get_group => get_group_SideSecond !&
        !----------------------------------------------------------------------------------
        procedure, pass(self) :: psi       => psi_SideSecond !&
        procedure, pass(self) :: dpsi_dxi  => dpsi_dxi_SideSecond !&
    end type SideSecond

    !--------------------------------------------------------------------------------------
    !  Abstract interface for the 1D element
    !--------------------------------------------------------------------------------------
    abstract interface
        function Abst_get_id(self) result(id)
            import :: Abst_SideType, int32
            implicit none
            class(Abst_SideType), intent(in) :: self
            integer(int32) :: id
        end function Abst_get_id

        function Abst_get_type(self) result(type)
            import :: Abst_SideType, int32
            implicit none
            class(Abst_SideType), intent(in) :: self
            integer(int32) :: type
        end function Abst_get_type

        function Abst_get_size(self) result(n)
            import :: Abst_SideType, int32
            implicit none
            class(Abst_SideType), intent(in) :: self
            integer(int32) :: n
        end function Abst_get_size

        function Abst_get_group(self) result(group)
            import :: Abst_SideType, int32
            implicit none
            class(Abst_SideType), intent(in) :: self
            integer(int32) :: group
        end function Abst_get_group

        function Abst_psi(self, i, xi) result(psi)
            import :: Abst_SideType, int32, real64
            implicit none
            class(Abst_SideType), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64), intent(in) :: xi
            real(real64) :: psi
        end function Abst_psi

        function Abst_dpsi_dxi(self, i) result(dpsi)
            import :: Abst_SideType, int32, real64
            implicit none
            class(Abst_SideType), intent(in) :: self
            integer(int32), intent(in) :: i
            real(real64) :: dpsi
        end function Abst_dpsi_dxi
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
            class(Abst_SideType), allocatable :: Structure

        end function SideFirst_Construct

        module function get_id_SideFirst(self) result(id)
            implicit none
            class(SideFirst), intent(in) :: self
            integer(int32) :: id
        end function get_id_SideFirst

        module function get_type_SideFirst(self) result(type)
            implicit none
            class(SideFirst), intent(in) :: self
            integer(int32) :: type
        end function get_type_SideFirst

        module function get_size_SideFirst(self) result(n)
            implicit none
            class(SideFirst), intent(in) :: self
            integer(int32) :: n
        end function get_size_SideFirst

        module function get_group_SideFirst(self) result(group)
            implicit none
            class(SideFirst), intent(in) :: self
            integer(int32) :: group
        end function get_group_SideFirst

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
            class(Abst_SideType), allocatable :: Structure

        end function SideSecond_Construct

        module function get_id_SideSecond(self) result(id)
            implicit none
            class(SideSecond), intent(in) :: self
            integer(int32) :: id
        end function get_id_SideSecond

        module function get_type_SideSecond(self) result(type)
            implicit none
            class(SideSecond), intent(in) :: self
            integer(int32) :: type
        end function get_type_SideSecond

        module function get_size_SideSecond(self) result(n)
            implicit none
            class(SideSecond), intent(in) :: self
            integer(int32) :: n
        end function get_size_SideSecond

        module function get_group_SideSecond(self) result(group)
            implicit none
            class(SideSecond), intent(in) :: self
            integer(int32) :: group
        end function get_group_SideSecond

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

end module Domain_Side

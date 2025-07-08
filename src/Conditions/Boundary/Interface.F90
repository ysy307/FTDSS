module Condition_Boundary
    use, intrinsic :: iso_fortran_env
    use :: Core_BaseTypes
    use :: Core_Allocate
    use :: Domain_Side, only:SideHolder
    use :: Domain_Module, only:Domain_t
    use :: core_core
    use :: Matrix_CRS
    use :: Inout_Input
    implicit none
    ! private

    character(*), parameter :: Dirichlet = "Dirichlet"
    character(*), parameter :: Neumann = "Neumann"
    character(*), parameter :: Adiabatic = "Adiabatic"
    character(*), parameter :: FreeHeatDischarge = "FreeHeatDischarge"
    character(*), parameter :: Robin = "Robin"
    character(*), parameter :: HeatTransfer = "HeatTransfer"
    character(*), parameter :: HeatRadiation = "HeatRadiation"
    character(*), parameter :: WaterFlux = "WaterFlux"
    character(*), parameter :: HeatFlux = "HeatFlux"

    public :: Abst_BC_Thermal
    public :: BC_Thermal_Dirichlet
    public :: BC_Thermal_Adiabatic

    !
    ! 熱境界条件の抽象基底クラス
    !
    type, abstract :: Abst_BC_Thermal
        real(real64), allocatable, private :: time_points(:) ! 時間ステップ
        real(real64), allocatable, private :: values(:) ! 対応する値
        integer(int32), private :: num_target_edges ! 対象エッジの数
        integer(int32), allocatable, private :: target_edges(:, :) ! 対象エッジの接続情報
    contains
        procedure(Abst_Setup_BC),            pass(self), deferred :: setup !&

        procedure(Abst_Apply_BC_Dence),      pass(self), deferred :: apply_Dense !&
        procedure(Abst_Apply_BC_Sparse_CRS), pass(self), deferred :: apply_CRS !&
    end type Abst_BC_Thermal

    ! 手続きのインターフェース定義
    abstract interface
        subroutine Abst_Setup_BC(self, Input_BC, time_conv, iGroup, Domain)
            import :: Abst_BC_Thermal, Input_Boundary, int32, Domain_t, real64
            implicit none
            class(Abst_BC_Thermal), intent(inout) :: self
            type(Input_Boundary), intent(in) :: Input_BC
            real(real64), intent(in) :: time_conv
            integer(int32), intent(in) :: iGroup
            type(Domain_t), intent(in) :: Domain
        end subroutine Abst_Setup_BC

        subroutine Abst_Apply_BC_Dence(self, current_time, A, b, Domain, mode)
            import :: Abst_BC_Thermal, real64, Domain_t, int32
            implicit none
            class(Abst_BC_Thermal), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(inout), optional :: A(:, :)
            real(real64), intent(inout) :: b(:)
            type(Domain_t), intent(in) :: Domain
            integer(int32), intent(in), optional :: mode
        end subroutine Abst_Apply_BC_Dence

        subroutine Abst_Apply_BC_Sparse_CRS(self, current_time, A, b, Domain, mode)
            import :: Abst_BC_Thermal, real64, Type_CRS, Domain_t, int32
            implicit none
            class(Abst_BC_Thermal), intent(in) :: self
            real(real64), intent(in) :: current_time
            type(Type_CRS), intent(inout), optional :: A
            real(real64), intent(inout) :: b(:)
            type(Domain_t), intent(in) :: Domain
            integer(int32), intent(in), optional :: mode
        end subroutine Abst_Apply_BC_Sparse_CRS

    end interface

    type, extends(Abst_BC_Thermal) :: BC_Thermal_Dirichlet
        logical(logical32), private :: is_uniform
    contains
        procedure, pass(self) :: setup => setup_Thermal_Dirichlet
        procedure, pass(self) :: apply_Dense => apply_Dense_Thermal_Dirichlet
        procedure, pass(self) :: apply_CRS => apply_CRS_Thermal_Dirichlet
    end type BC_Thermal_Dirichlet

    interface
        module subroutine setup_Thermal_Dirichlet(self, Input_BC, time_conv, iGroup, Domain)
            implicit none
            class(BC_Thermal_Dirichlet), intent(inout) :: self
            type(Input_Boundary), intent(in) :: Input_BC
            real(real64), intent(in) :: time_conv
            integer(int32), intent(in) :: iGroup
            type(Domain_t), intent(in) :: Domain

        end subroutine setup_Thermal_Dirichlet

        module subroutine apply_Dense_Thermal_Dirichlet(self, current_time, A, b, Domain, mode)
            implicit none
            class(BC_Thermal_Dirichlet), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(inout), optional :: A(:, :)
            real(real64), intent(inout) :: b(:)
            type(Domain_t), intent(in) :: Domain
            integer(int32), intent(in), optional :: mode

        end subroutine apply_Dense_Thermal_Dirichlet

        module subroutine apply_CRS_Thermal_Dirichlet(self, current_time, A, b, Domain, mode)
            implicit none
            class(BC_Thermal_Dirichlet), intent(in) :: self
            real(real64), intent(in) :: current_time
            type(Type_CRS), intent(inout), optional :: A
            real(real64), intent(inout) :: b(:)
            type(Domain_t), intent(in) :: Domain
            integer(int32), intent(in), optional :: mode

        end subroutine apply_CRS_Thermal_Dirichlet
    end interface

    !
    ! 断熱 (Adiabatic) 条件
    !
    type, extends(BC_Thermal_Dirichlet) :: BC_Thermal_Adiabatic
        ! データメンバは不要
    contains
        procedure, pass(self) :: setup => setup_Thermal_Adiabatic
        procedure, pass(self) :: apply_Dense => apply_Dense_Thermal_Adiabatic
        procedure, pass(self) :: apply_CRS => apply_CRS_Thermal_Adiabatic
    end type BC_Thermal_Adiabatic

    interface
        module subroutine setup_Thermal_Adiabatic(self, Input_BC, time_conv, iGroup, Domain)
            implicit none
            class(BC_Thermal_Adiabatic), intent(inout) :: self
            type(Input_Boundary), intent(in) :: Input_BC
            real(real64), intent(in) :: time_conv
            integer(int32), intent(in) :: iGroup
            type(Domain_t), intent(in) :: Domain

        end subroutine setup_Thermal_Adiabatic

        module subroutine apply_Dense_Thermal_Adiabatic(self, current_time, A, b, Domain, mode)
            implicit none
            class(BC_Thermal_Adiabatic), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(inout), optional :: A(:, :)
            real(real64), intent(inout) :: b(:)
            type(Domain_t), intent(in) :: Domain
            integer(int32), intent(in), optional :: mode

        end subroutine apply_Dense_Thermal_Adiabatic

        module subroutine apply_CRS_Thermal_Adiabatic(self, current_time, A, b, Domain, mode)
            implicit none
            class(BC_Thermal_Adiabatic), intent(in) :: self
            real(real64), intent(in) :: current_time
            type(Type_CRS), intent(inout), optional :: A
            real(real64), intent(inout) :: b(:)
            type(Domain_t), intent(in) :: Domain
            integer(int32), intent(in), optional :: mode

        end subroutine apply_CRS_Thermal_Adiabatic
    end interface

!     public :: Condition_BC_Local
!     public :: Abstract_Condition_BC
!     public :: Type_BC_Thermal

!     type :: Type_Edge
!         integer(int32) :: EdgeType
!         integer(int32) :: EdgeGroup
!         integer(int32), allocatable :: Conn(:)
!         real(real64), allocatable :: Distance(:)
!         type(Vector3D), allocatable :: UnitNormal(:)
!     end type Type_Edge

!     type :: Condition_BC_Local
!         character(:), allocatable :: type
!         logical(4) :: isUniform
!         real(real64), allocatable :: value(:)
!     end type Condition_BC_Local

!     type, abstract :: Abstract_Condition_BC
!         !! Boundary conditions information
!         integer(int32) :: numBCGroup
!         real(real64), allocatable :: Time(:)
!         integer(int32), allocatable :: BCGroup(:)
!         type(Condition_BC_Local), allocatable :: BCInfo(:)

!     contains
!         procedure(Abstract_BC_Thermal_CRS_Dirichlet),    pass(self), deferred :: Fix_BC_Dirichlet !&
!         procedure(Abstract_BC_Thermal_CRS_Dirichlet_NR), pass(self), deferred :: Fix_BC_Dirichlet_NR !&
!         procedure(Abstract_BC_Thermal_CRS_Fix_All),      pass(self), deferred :: Fix_BC_All !&
!         procedure(Abstract_BC_Thermal_CRS_Fix_RHS),      pass(self), deferred :: Fix_BC_RHS !&
!         generic, public :: Fix_BC => Fix_BC_All, & !&
!                                      Fix_BC_RHS !&
!     end type Abstract_Condition_BC

!     type :: Type_BC_Thermal
!         !! Boundary conditions information
!         integer(int32) :: numBCGroup
!         real(real64), allocatable :: Time(:)
!         integer(int32), allocatable :: BCGroup(:)
!         type(Condition_BC_Local), allocatable :: BC_Info(:)
!         ! type(Condition_BC_Local), allocatable :: BCInfo(:)
!         !! Boundary conditions information of the Edges
!         integer(int32) :: numEdges
!         type(Type_Edge), allocatable :: EdgeInfo(:)

!     contains
!         procedure, private, pass(self) :: Fix_BoundaryConditions_CRS
!         procedure, private, pass(self) :: Fix_BoundaryConditions_Full
!         generic, public :: Fix_BoundaryConditions => Fix_BoundaryConditions_CRS, & !&
!                                                      Fix_BoundaryConditions_Full
!         procedure, private, pass(self) :: Fix_Bounday_Values_CRS
!         procedure, private, pass(self) :: Fix_Bounday_Values_Full
!         procedure, private, pass(self) :: Fix_Bounday_Values_RHS
!         generic, public :: Fix_Bounday_Values => Fix_Bounday_Values_CRS, & !&
!                                                  Fix_Bounday_Values_Full, & !&
!                                                  Fix_Bounday_Values_RHS
!     end type

!     type, extends(Abstract_Condition_BC) :: Type_BC_Thermal_CRS
!     contains
!         procedure :: Fix_BC_Dirichlet    => Type_BC_Thermal_CRS_Dirichlet !&
!         procedure :: Fix_BC_Dirichlet_NR => Type_BC_Thermal_CRS_Dirichlet_NR !&
!         procedure :: Fix_BC_All          => Type_BC_Thermal_CRS_Fix_BC_All !&
!         procedure :: Fix_BC_RHS          => Type_BC_Thermal_CRS_Fix_BC_RHS !&
!     end type
!     ! public :: Fix_BoundaryConditions

!     abstract interface
!         subroutine Abstract_BC_Thermal_CRS_Fix_All(self, A, b, Sides, time)
!             import :: Abstract_Condition_BC, Type_CRS, SideHolder, real64
!             implicit none
!             class(Abstract_Condition_BC), intent(in) :: self
!             type(Type_CRS), intent(inout) :: A
!             real(real64), intent(inout) :: b(:)
!             type(SideHolder), intent(in) :: Sides(:)
!             real(real64), intent(in), optional :: time

!         end subroutine Abstract_BC_Thermal_CRS_Fix_All

!         subroutine Abstract_BC_Thermal_CRS_Fix_RHS(self, b, Sides, time)
!             import :: Abstract_Condition_BC, Type_CRS, SideHolder, real64
!             implicit none
!             class(Abstract_Condition_BC), intent(in) :: self
!             real(real64), intent(inout) :: b(:)
!             type(SideHolder), intent(in) :: Sides(:)
!             real(real64), intent(in), optional :: time

!         end subroutine Abstract_BC_Thermal_CRS_Fix_RHS

!         subroutine Abstract_BC_Thermal_CRS_Dirichlet(self, A, b, Info, Edge, Dval)
!             import :: Abstract_Condition_BC, Condition_BC_Local, Type_CRS, real64, int32
!             implicit none
!             class(Abstract_Condition_BC), intent(in) :: self
!             type(Type_CRS), intent(inout), optional :: A
!             real(real64), intent(inout) :: b(:)
!             type(Condition_BC_Local), intent(in) :: Info
!             integer(int32), intent(in) :: Edge(2)
!             real(real64), intent(in) :: Dval

!         end subroutine Abstract_BC_Thermal_CRS_Dirichlet

!         subroutine Abstract_BC_Thermal_CRS_Dirichlet_NR(self, A, b, Info, Edge)
!             import :: Abstract_Condition_BC, Condition_BC_Local, Type_CRS, real64, int32
!             implicit none
!             class(Abstract_Condition_BC), intent(in) :: self
!             type(Type_CRS), intent(inout), optional :: A
!             real(real64), intent(inout) :: b(:)
!             type(Condition_BC_Local), intent(in) :: Info
!             integer(int32), intent(in) :: Edge(2)

!         end subroutine Abstract_BC_Thermal_CRS_Dirichlet_NR
!     end interface

!     interface
!         module function Type_BC_Thermal_CRS_Construct(Input) result(Structure)
!             implicit none
!             type(Type_Input), intent(in) :: Input
!             class(Abstract_Condition_BC), allocatable :: Structure

!         end function Type_BC_Thermal_CRS_Construct

!         module subroutine Type_BC_Thermal_CRS_Fix_BC_All(self, A, b, Sides, time)
!             implicit none
!             class(Type_BC_Thermal_CRS), intent(in) :: self
!             type(Type_CRS), intent(inout) :: A
!             real(real64), intent(inout) :: b(:)
!             type(SideHolder), intent(in) :: Sides(:)
!             real(real64), intent(in), optional :: time

!         end subroutine Type_BC_Thermal_CRS_Fix_BC_All

!         module subroutine Type_BC_Thermal_CRS_Fix_BC_RHS(self, b, Sides, time)
!             implicit none
!             class(Type_BC_Thermal_CRS), intent(in) :: self
!             real(real64), intent(inout) :: b(:)
!             type(SideHolder), intent(in) :: Sides(:)
!             real(real64), intent(in), optional :: time

!         end subroutine Type_BC_Thermal_CRS_Fix_BC_RHS

!         module subroutine Type_BC_Thermal_CRS_Dirichlet(self, A, b, Info, Edge, Dval)
!             implicit none
!             class(Type_BC_Thermal_CRS), intent(in) :: self
!             type(Type_CRS), intent(inout), optional :: A
!             real(real64), intent(inout) :: b(:)
!             type(Condition_BC_Local), intent(in) :: Info
!             integer(int32), intent(in) :: Edge(2)
!             real(real64), intent(in) :: Dval

!         end subroutine Type_BC_Thermal_CRS_Dirichlet

!         module subroutine Type_BC_Thermal_CRS_Dirichlet_NR(self, A, b, Info, Edge)
!             implicit none
!             class(Type_BC_Thermal_CRS), intent(in) :: self
!             type(Type_CRS), intent(inout), optional :: A
!             real(real64), intent(inout) :: b(:)
!             type(Condition_BC_Local), intent(in) :: Info
!             integer(int32), intent(in) :: Edge(2)

!         end subroutine Type_BC_Thermal_CRS_Dirichlet_NR
!     end interface

!     interface
!         module subroutine Fix_BC_CRS_Dirichlet(A, b, Info, Edge, Dval)
!             implicit none
!             type(Type_CRS), intent(inout), optional :: A
!             real(real64), intent(inout) :: b(:)
!             type(Condition_BC_Local), intent(in) :: Info
!             integer(int32), intent(in) :: Edge(2)
!             real(real64), intent(in) :: Dval

!         end subroutine Fix_BC_CRS_Dirichlet

!     end interface

    interface
        module subroutine Calc_Time_Coefficients(time, arr_time, timeCoe, idx)
            implicit none
            real(real64), intent(in) :: Time
            real(real64), intent(in) :: arr_time(:)
            real(real64), intent(inout) :: timeCoe
            integer(int32), intent(inout) :: idx

        end subroutine Calc_Time_Coefficients

        module subroutine Find_Target_Edges_By_Group(Domain, Input_BC, iGroup, target_edges)
            implicit none
            type(Domain_t), intent(in) :: Domain
            type(Input_Boundary), intent(in) :: Input_BC
            integer(int32), intent(in) :: iGroup
            integer(int32), allocatable, intent(inout) :: target_edges(:, :)

        end subroutine Find_Target_Edges_By_Group
    end interface

!     interface Type_BC_Thermal_CRS
!         module procedure :: Type_BC_Thermal_CRS_Construct
!     end interface

!     interface assignment(=)
!         module procedure Condition_BC_Local_Assignment
!     end interface

! contains
!     subroutine Condition_BC_Local_Assignment(A, B)
!         implicit none
!         type(Condition_BC_Local), intent(inout) :: A
!         type(Condition_BC_Local), intent(in) :: B

!         A%type = B%type
!         A%isUniform = B%isUniform
!         if (allocated(A%value)) deallocate (A%value)
!         if (allocated(B%value)) allocate (A%value, source=B%value)
!     end subroutine

!     function Calculate_Edge_Dinstance(Edge, Coordinate) result(Edge_Distance)
!         implicit none
!         integer(int32), intent(in) :: Edge(:)
!         type(DP3d), intent(in) :: Coordinate
!         real(real64) :: Edge_Distance
!         real(real64) :: x1, y1, x2, y2

!         x1 = Coordinate%x(Edge(1))
!         y1 = Coordinate%y(Edge(1))
!         x2 = Coordinate%x(Edge(2))
!         y2 = Coordinate%y(Edge(2))
!         Edge_Distance = sqrt((x2 - x1)**2.0d0 + (y2 - y1)**2.0d0)
!     end function

!     function Calculate_Edge_UnitNormalVector(Edge, Coordinate, Distance) result(UnitNormalVector)
!         integer(int32), intent(in) :: Edge(:)
!         type(DP3d), intent(in) :: Coordinate
!         real(real64), intent(in) :: Distance
!         type(Vector3D) :: UnitNormalVector

!         real(real64) :: x1, y1, x2, y2

!         x1 = Coordinate%x(Edge(1))
!         y1 = Coordinate%y(Edge(1))
!         x2 = Coordinate%x(Edge(2))
!         y2 = Coordinate%y(Edge(2))

!         UnitNormalVector%x = (y2 - y1) / Distance
!         UnitNormalVector%y = -(x2 - x1) / Distance

!     end function Calculate_Edge_UnitNormalVector

!     function Type_BC_Thermal_Constructor(Structure_Input, Input_VTK) result(Structure)
!         implicit none
!         type(Type_BC_Thermal), intent(in) :: Structure_Input
!         type(Type_VTK), intent(in) :: Input_VTK
!         type(Type_BC_Thermal) :: Structure

!         integer(int32) :: i, iCell, idx
!         integer(int32) :: minimum, maximum
!         integer(int32) :: CounterEdge
!         integer(int32) :: tmpEdge(2)

!         allocate (Structure%BCGroup, source=Structure_Input%BCGroup)
!         minimum = minval(Structure%BCGroup)
!         maximum = maxval(Structure%BCGroup)
!         allocate (Structure%BC_Info(minimum:maximum))
!         do i = 1, size(Structure_Input%BC_Info)
!             Structure%BC_Info(Structure%BCGroup(i)) = Structure_Input%BC_Info(Structure%BCGroup(i))
!         end do

!         CounterEdge = 0
!         do iCell = 1, Input_VTK%numTotalCells
!             if (Input_VTK%CELLS(iCell)%CellType == VTK_LINE .or. &
!                 Input_VTK%CELLS(iCell)%CellType == VTK_QUADRATIC_EDGE) then
!                 CounterEdge = CounterEdge + 1
!             end if
!         end do
!         Structure%numEdges = CounterEdge
!         allocate (Structure%EdgeInfo(Structure%numEdges))

!         idx = 0
!         do iCell = 1, Input_VTK%numTotalCells
!             if (Input_VTK%CELLS(iCell)%CellType == VTK_LINE) then
!                 idx = idx + 1
!                 Structure%EdgeInfo(idx)%EdgeType = VTK_LINE
!                 Structure%EdgeInfo(idx)%EdgeGroup = Input_VTK%CELLS(iCell)%CellEntityId
!                 allocate (Structure%EdgeInfo(idx)%Conn, source=Input_VTK%CELLS(iCell)%Connectivity)
!                 allocate (Structure%EdgeInfo(idx)%Distance(1))
!                 allocate (Structure%EdgeInfo(idx)%UnitNormal(1))

!                 tmpEdge = [Structure%EdgeInfo(idx)%Conn(1), Structure%EdgeInfo(idx)%Conn(2)]
!                 Structure%EdgeInfo(idx)%Distance(1) = Calculate_Edge_Dinstance(tmpEdge, Input_VTK%POINTS)
!                 Structure%EdgeInfo(idx)%UnitNormal(1) = Calculate_Edge_UnitNormalVector(tmpEdge, Input_VTK%POINTS, Structure%EdgeInfo(idx)%Distance(1))
!             else if (Input_VTK%CELLS(iCell)%CellType == VTK_QUADRATIC_EDGE) then
!                 idx = idx + 1
!                 Structure%EdgeInfo(idx)%EdgeType = VTK_QUADRATIC_EDGE
!                 Structure%EdgeInfo(idx)%EdgeGroup = Input_VTK%CELLS(iCell)%CellEntityId
!                 allocate (Structure%EdgeInfo(idx)%Conn, source=Input_VTK%CELLS(iCell)%Connectivity)
!                 allocate (Structure%EdgeInfo(idx)%Distance(2))
!                 allocate (Structure%EdgeInfo(idx)%UnitNormal(2))

!                 tmpEdge = [Structure%EdgeInfo(idx)%Conn(1), Structure%EdgeInfo(idx)%Conn(3)]
!                 Structure%EdgeInfo(idx)%Distance(1) = Calculate_Edge_Dinstance(tmpEdge, Input_VTK%POINTS)
!                 Structure%EdgeInfo(idx)%UnitNormal(1) = Calculate_Edge_UnitNormalVector(tmpEdge, Input_VTK%POINTS, Structure%EdgeInfo(idx)%Distance(1))
!                 tmpEdge = [Structure%EdgeInfo(idx)%Conn(3), Structure%EdgeInfo(idx)%Conn(2)]
!                 Structure%EdgeInfo(idx)%Distance(2) = Calculate_Edge_Dinstance(tmpEdge, Input_VTK%POINTS)
!                 Structure%EdgeInfo(idx)%UnitNormal(2) = Calculate_Edge_UnitNormalVector(tmpEdge, Input_VTK%POINTS, Structure%EdgeInfo(idx)%Distance(2))
!             end if
!         end do

!     end function Type_BC_Thermal_Constructor

!     subroutine Fix_BoundaryConditions_CRS(self, A, b, lambda, Cw, Structure_HeatFlux)
!         implicit none
!         class(Type_BC_Thermal), intent(inout) :: self
!         type(Type_CRS), intent(inout) :: A
!         real(real64), intent(inout) :: b(:)
!         real(real64), optional, intent(in) :: lambda(:)
!         real(real64), optional, intent(in) :: Cw
!         type(DP3d), optional, intent(in) :: Structure_HeatFlux

!         integer(int32) :: i

!         do i = 1, self%numEdges
!             if (self%EdgeInfo(i)%EdgeType == 3) then !! VTK_LINE
!                 select case (self%BC_Info(self%EdgeInfo(i)%EdgeGroup)%type)
!                 case (Neumann)
!                     call Fix_BoundaryCondition_Neumann(b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:), self%EdgeInfo(i)%Distance(1), lambda)
!                 case (HeatFlux)
!                     call Fix_BoundaryCondition_Flux(b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:), self%EdgeInfo(i)%Distance(1))
!                 case (Robin)
!                     call Fix_BoundaryCondition_Robin_CRS(A, b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:), self%EdgeInfo(i)%Distance(1), self%EdgeInfo(i)%UnitNormal(1), Cw, Structure_HeatFlux)
!                 case (HeatTransfer)
!                     call Fix_BoundaryCondition_HeatTransfer_CRS(A, b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:), self%EdgeInfo(i)%Distance(1))
!                 case (HeatRadiation)
!                     call Fix_BoundaryCondition_HeatRadiation_CRS(A, b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:), self%EdgeInfo(i)%Distance(1))
!                 end select
!             end if
!         end do
!         do i = 1, self%numEdges
!             select case (self%BC_Info(self%EdgeInfo(i)%EdgeGroup)%type)
!             case (Dirichlet)
!                 call Fix_BoundaryCondition_Dirichlet_CRS(A, b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:))
!             end select
!         end do

!     end subroutine Fix_BoundaryConditions_CRS

!     subroutine Fix_BoundaryConditions_Full(self, A, b, lambda, Cw, Structure_HeatFlux)
!         implicit none
!         class(Type_BC_Thermal), intent(inout) :: self
!         real(real64), intent(inout) :: A(:, :)
!         real(real64), intent(inout) :: b(:)
!         real(real64), intent(in) :: lambda(:)
!         real(real64), intent(in) :: Cw
!         type(DP3d), intent(in) :: Structure_HeatFlux

!         integer(int32) :: i

!         do i = 1, self%numEdges
!             select case (self%BC_Info(self%EdgeInfo(i)%EdgeGroup)%type)
!             case (Neumann)
!                 call Fix_BoundaryCondition_Neumann(b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:), self%EdgeInfo(i)%Distance(1), lambda)
!             case (HeatFlux)
!                 call Fix_BoundaryCondition_Flux(b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:), self%EdgeInfo(i)%Distance(1))
!             case (Robin)
!                 call Fix_BoundaryCondition_Robin_Full(A, b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:), self%EdgeInfo(i)%Distance(1), self%EdgeInfo(i)%UnitNormal(1), Cw, Structure_HeatFlux)
!             case (HeatTransfer)
!                 call Fix_BoundaryCondition_HeatTransfer_Full(A, b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:), self%EdgeInfo(i)%Distance(1))
!             case (HeatRadiation)
!                 call Fix_BoundaryCondition_HeatRadiation_Full(A, b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:), self%EdgeInfo(i)%Distance(1))
!             end select
!         end do
!         do i = 1, self%numEdges
!             select case (self%BC_Info(self%EdgeInfo(i)%EdgeGroup)%type)
!             case (Dirichlet)
!                 call Fix_BoundaryCondition_Dirichlet_Full(A, b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:))
!             end select
!         end do
!     end subroutine Fix_BoundaryConditions_Full

!     subroutine Fix_Bounday_Values_CRS(self, A, b)
!         implicit none
!         class(Type_BC_Thermal), intent(inout) :: self
!         type(Type_CRS), intent(inout) :: A
!         real(real64), intent(inout) :: b(:)
!         integer(int32) :: i

!         do i = 1, self%numEdges
!             select case (self%BC_Info(self%EdgeInfo(i)%EdgeGroup)%type)
!             case (Dirichlet)
!                 call Fix_BoundaryCondition_Dirichlet_CRS_Value(A, b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:))
!             end select
!         end do
!     end subroutine Fix_Bounday_Values_CRS

!     subroutine Fix_Bounday_Values_Full(self, A, b)
!         implicit none
!         class(Type_BC_Thermal), intent(inout) :: self
!         real(real64), intent(inout) :: A(:, :)
!         real(real64), intent(inout) :: b(:)
!         integer(int32) :: i

!         do i = 1, self%numEdges
!             select case (self%BC_Info(self%EdgeInfo(i)%EdgeGroup)%type)
!             case (Dirichlet)
!                 call Fix_BoundaryCondition_Dirichlet_Full(A, b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:))
!             end select
!         end do
!     end subroutine Fix_Bounday_Values_Full

!     subroutine Fix_Bounday_Values_RHS(self, b)
!         implicit none
!         class(Type_BC_Thermal), intent(inout) :: self
!         real(real64), intent(inout) :: b(:)
!         integer(int32) :: i

!         do i = 1, self%numEdges
!             select case (self%BC_Info(self%EdgeInfo(i)%EdgeGroup)%type)
!             case (Dirichlet)
!                 if (self%BC_Info(self%EdgeInfo(i)%EdgeGroup)%isUniform) then
!                     b(self%EdgeInfo(i)%Conn(1)) = self%BC_Info(self%EdgeInfo(i)%EdgeGroup)%value(1)
!                     b(self%EdgeInfo(i)%Conn(2)) = self%BC_Info(self%EdgeInfo(i)%EdgeGroup)%value(1)
!                 end if
!             end select
!         end do
!     end subroutine Fix_Bounday_Values_RHS

!     subroutine Fix_BoundaryCondition_Dirichlet_CRS(A, b, BC_Info, Edge)
!         implicit none
!         type(Type_CRS), intent(inout) :: A
!         real(real64), intent(inout) :: b(:)
!         type(Condition_BC_Local), intent(inout) :: BC_Info
!         integer(int32), intent(in) :: Edge(:)
!         integer(int32) :: i, ind, ps, pe
!         integer(int32) :: p1, p2

!         if (BC_Info%isUniform) then
!             p1 = Edge(1)
!             p2 = Edge(2)

!             call A%Find(p1, p1, ind)
!             ps = A%Ptr(p1)
!             pe = A%Ptr(p1 + 1) - 1
!             A%val(ps:pe) = 0.0d0
!             A%val(ind) = 1.0d0
!             b(p1) = 0.0d0

!             call A%Find(p2, p2, ind)
!             ps = A%Ptr(p2)
!             pe = A%Ptr(p2 + 1) - 1
!             A%val(ps:pe) = 0.0d0
!             A%val(ind) = 1.0d0
!             b(p2) = 0.0d0
!         end if
!     end subroutine Fix_BoundaryCondition_Dirichlet_CRS

!     subroutine Fix_BoundaryCondition_Dirichlet_CRS_Value(A, b, BC_Info, Edge)
!         implicit none
!         type(Type_CRS), intent(inout) :: A
!         real(real64), intent(inout) :: b(:)
!         type(Condition_BC_Local), intent(inout) :: BC_Info
!         integer(int32), intent(in) :: Edge(:)
!         integer(int32) :: i, ind, ps, pe
!         integer(int32) :: p1, p2

!         if (BC_Info%isUniform) then
!             p1 = Edge(1)
!             p2 = Edge(2)

!             call A%Find(p1, p1, ind)
!             ps = A%Ptr(p1)
!             pe = A%Ptr(p1 + 1) - 1
!             A%val(ps:pe) = 0.0d0
!             A%val(ind) = 1.0d0
!             b(p1) = BC_Info%value(1)

!             call A%Find(p2, p2, ind)
!             ps = A%Ptr(p2)
!             pe = A%Ptr(p2 + 1) - 1
!             A%val(ps:pe) = 0.0d0
!             A%val(ind) = 1.0d0
!             b(p2) = BC_Info%value(1)
!         end if
!     end subroutine Fix_BoundaryCondition_Dirichlet_CRS_Value

!     subroutine Fix_BoundaryCondition_Dirichlet_CRS_Initial(A, b, BC_Info, Edge)
!         implicit none
!         type(Type_CRS), intent(inout) :: A
!         real(real64), intent(inout) :: b(:)
!         type(Condition_BC_Local), intent(inout) :: BC_Info
!         integer(int32), intent(in) :: Edge(:)
!         integer(int32) :: i, ind, ps, pe
!         integer(int32) :: p1, p2

!         if (BC_Info%isUniform) then
!             p1 = Edge(1)
!             p2 = Edge(2)

!             call A%Find(p1, p1, ind)
!             ps = A%Ptr(p1)
!             pe = A%Ptr(p1 + 1) - 1
!             A%val(ps:pe) = 0.0d0
!             A%val(ind) = 1.0d0
!             b(p1) = BC_Info%value(1)

!             call A%Find(p2, p2, ind)
!             ps = A%Ptr(p2)
!             pe = A%Ptr(p2 + 1) - 1
!             A%val(ps:pe) = 0.0d0
!             A%val(ind) = 1.0d0
!             b(p2) = BC_Info%value(1)

!         end if
!     end subroutine Fix_BoundaryCondition_Dirichlet_CRS_Initial

!     subroutine Fix_BoundaryCondition_Dirichlet_Full(A, b, BC_Info, Edge)
!         implicit none
!         real(real64), intent(inout) :: A(:, :)
!         real(real64), intent(inout) :: b(:)
!         type(Condition_BC_Local), intent(inout) :: BC_Info
!         integer(int32), intent(in) :: Edge(:)
!         integer(int32) :: i, ind, ps, pe
!         integer(int32) :: p1, p2

!         if (BC_Info%isUniform) then
!             p1 = Edge(1)
!             p2 = Edge(2)

!             A(p1, :) = 0.0d0
!             A(p1, p1) = 1.0d0
!             b(p1) = 0.0d0

!             A(p2, :) = 0.0d0
!             A(p2, p2) = 1.0d0
!             b(p2) = 0.0d0
!         end if
!     end subroutine Fix_BoundaryCondition_Dirichlet_Full

!     subroutine Fix_BoundaryCondition_Neumann(b, BC_Info, Edge, Edge_Distance, c)
!         implicit none
!         type(Condition_BC_Local), intent(inout) :: BC_Info
!         integer(int32), intent(in) :: Edge(:)
!         real(real64), intent(in) :: Edge_Distance
!         real(real64), intent(inout) :: b(:)
!         real(real64), intent(in) :: c(:)

!         integer(int32) :: p1, p2

!         p1 = Edge(1)
!         p2 = Edge(2)
!         if (BC_Info%isUniform) then
!             b(p1) = b(p1) + (2.0d0 * BC_Info%value(1) * c(p1) + BC_Info%value(1) * c(p2)) * Edge_Distance / 6.0d0
!             b(p2) = b(p2) + (BC_Info%value(1) * c(p1) + 2.0d0 * BC_Info%value(1) * c(p2)) * Edge_Distance / 6.0d0
!         end if

!     end subroutine Fix_BoundaryCondition_Neumann

!     subroutine Fix_BoundaryCondition_Flux(b, BC_Info, Edge, Edge_Distance)
!         implicit none
!         real(real64), intent(inout) :: b(:)
!         type(Condition_BC_Local), intent(inout) :: BC_Info
!         integer(int32), intent(in) :: Edge(:)
!         real(real64), intent(in) :: Edge_Distance

!         integer(int32) :: p1, p2
!         real(real64) :: val

!         p1 = Edge(1)
!         p2 = Edge(2)
!         if (BC_Info%isUniform) then
!             val = 0.5d0 * BC_Info%value(1) * Edge_Distance
!             b(p1) = b(p1) - val
!             b(p2) = b(p2) - val
!         end if

!     end subroutine Fix_BoundaryCondition_Flux

!     subroutine Fix_BoundaryCondition_Robin_CRS(A, b, BC_Info, Edge, Edge_Distance, Edge_UnitNormal, Cw, Flux)
!         implicit none
!         type(Type_CRS), intent(inout) :: A
!         real(real64), intent(inout) :: b(:)
!         type(Condition_BC_Local), intent(inout) :: BC_Info
!         integer(int32), intent(in) :: Edge(:)
!         real(real64), intent(in) :: Edge_Distance
!         type(Vector3D), intent(in) :: Edge_UnitNormal
!         real(real64), intent(in) :: Cw
!         type(DP3d), intent(in) :: Flux

!         integer(int32) :: p1, p2, ind
!         real(real64) :: Q1, Q2

!         p1 = Edge(1)
!         p2 = Edge(2)

!         Q1 = Flux%x(p1) * Edge_UnitNormal%x + Flux%y(p1) * Edge_UnitNormal%y
!         Q2 = Flux%x(p2) * Edge_UnitNormal%x + Flux%y(p2) * Edge_UnitNormal%y
!         if (BC_Info%isUniform) then
!             b(p1) = b(p1) - (2.0d0 * Q1 + Q2) * Edge_Distance / 6.0d0
!             b(p2) = b(p2) - (Q1 + 2.0d0 * Q2) * Edge_Distance / 6.0d0
!         end if

!         !! Assemble Robin (Cauthy) Boundary to the Global Matrix
!         call A%Find(p1, p1, ind)
!         A%val(ind) = A%val(ind) + 2.0d0 * Q1 * Edge_Distance / 6.0d0
!         call A%Find(p1, p2, ind)
!         A%val(ind) = A%val(ind) + Q1 * Edge_Distance / 6.0d0
!         call A%Find(p2, p1, ind)
!         A%val(ind) = A%val(ind) + Q2 * Edge_Distance / 6.0d0
!         call A%Find(p2, p2, ind)
!         A%val(ind) = A%val(ind) + 2.0d0 * Q2 * Edge_Distance / 6.0d0

!     end subroutine Fix_BoundaryCondition_Robin_CRS

!     subroutine Fix_BoundaryCondition_Robin_Full(A, b, BC_Info, Edge, Edge_Distance, Edge_UnitNormal, Cw, Flux)
!         implicit none
!         real(real64), intent(inout) :: A(:, :)
!         real(real64), intent(inout) :: b(:)
!         type(Condition_BC_Local), intent(inout) :: BC_Info
!         integer(int32), intent(in) :: Edge(:)
!         real(real64), intent(in) :: Edge_Distance
!         type(Vector3D), intent(in) :: Edge_UnitNormal
!         real(real64), intent(in) :: Cw
!         type(DP3d), intent(in) :: Flux

!         integer(int32) :: p1, p2, ind
!         real(real64) :: Q1, Q2

!         p1 = Edge(1)
!         p2 = Edge(2)

!         Q1 = Flux%x(p1) * Edge_UnitNormal%x + Flux%y(p1) * Edge_UnitNormal%y
!         Q2 = Flux%x(p2) * Edge_UnitNormal%x + Flux%y(p2) * Edge_UnitNormal%y
!         if (BC_Info%isUniform) then
!             b(p1) = b(p1) - (2.0d0 * Q1 + Q2) * Edge_Distance / 6.0d0
!             b(p2) = b(p2) - (Q1 + 2.0d0 * Q2) * Edge_Distance / 6.0d0
!         end if

!         !! Assemble Robin (Cauthy) Boundary to the Global Matrix
!         A(p1, p1) = A(p1, p1) + 2.0d0 * Q1 * Edge_Distance / 6.0d0
!         A(p1, p2) = A(p1, p2) + Q1 * Edge_Distance / 6.0d0
!         A(p2, p1) = A(p2, p1) + Q2 * Edge_Distance / 6.0d0
!         A(p2, p2) = A(p2, p2) + 2.0d0 * Q2 * Edge_Distance / 6.0d0
!     end subroutine Fix_BoundaryCondition_Robin_Full

!     subroutine Fix_BoundaryCondition_HeatTransfer_CRS(A, b, BC_Info, Edge, Edge_Distance)
!         implicit none
!         type(Type_CRS), intent(inout) :: A
!         real(real64), intent(inout) :: b(:)
!         type(Condition_BC_Local), intent(inout) :: BC_Info
!         integer(int32), intent(in) :: Edge(:)
!         real(real64), intent(in) :: Edge_Distance

!         integer(int32) :: p1, p2, ind
!         real(real64) :: val1, val2

!         p1 = Edge(1)
!         p2 = Edge(2)
!         val1 = BC_Info%value(1)
!         val2 = BC_Info%value(2)

!         if (BC_Info%isUniform) then
!             b(p1) = b(p1) - 0.5d0 * val1 * val2 * Edge_Distance
!             b(p2) = b(p2) - 0.5d0 * val1 * val2 * Edge_Distance
!         end if

!         !! Assemble Heat Transfer Boundary to the Global Matrix
!         call A%Find(p1, p1, ind)
!         A%val(ind) = A%val(ind) + 2.0d0 * val1 * Edge_Distance / 6.0d0
!         call A%Find(p1, p2, ind)
!         A%val(ind) = A%val(ind) + val1 * Edge_Distance / 6.0d0
!         call A%Find(p2, p1, ind)
!         A%val(ind) = A%val(ind) + val1 * Edge_Distance / 6.0d0
!         call A%Find(p2, p2, ind)
!         A%val(ind) = A%val(ind) + 2.0d0 * val1 * Edge_Distance / 6.0d0

!     end subroutine Fix_BoundaryCondition_HeatTransfer_CRS

!     subroutine Fix_BoundaryCondition_HeatTransfer_Full(A, b, BC_Info, Edge, Edge_Distance)
!         implicit none
!         real(real64), intent(inout) :: A(:, :)
!         real(real64), intent(inout) :: b(:)
!         type(Condition_BC_Local), intent(inout) :: BC_Info
!         integer(int32), intent(in) :: Edge(:)
!         real(real64), intent(in) :: Edge_Distance

!         integer(int32) :: p1, p2, ind
!         real(real64) :: val1, val2

!         p1 = Edge(1)
!         p2 = Edge(2)
!         val1 = BC_Info%value(1)
!         val2 = val2

!         if (BC_Info%isUniform) then
!             b(p1) = b(p1) - 0.5d0 * val1 * val2 * Edge_Distance
!             b(p2) = b(p2) - 0.5d0 * val1 * val2 * Edge_Distance
!         end if

!         !! Assemble Heat Transfer Boundary to the Global Matrix
!         A(p1, p1) = A(p1, p1) + 2.0d0 * val1 * Edge_Distance / 6.0d0
!         A(p1, p2) = A(p1, p2) + val1 * Edge_Distance / 6.0d0
!         A(p2, p1) = A(p2, p1) + val1 * Edge_Distance / 6.0d0
!         A(p2, p2) = A(p2, p2) + 2.0d0 * val1 * Edge_Distance / 6.0d0

!     end subroutine Fix_BoundaryCondition_HeatTransfer_Full

!     subroutine Fix_BoundaryCondition_HeatRadiation_CRS(A, b, BC_Info, Edge, Edge_Distance)
!         implicit none
!         type(Type_CRS), intent(inout) :: A
!         real(real64), intent(inout) :: b(:)
!         type(Condition_BC_Local), intent(inout) :: BC_Info
!         integer(int32), intent(in) :: Edge(:)
!         real(real64), intent(in) :: Edge_Distance

!         integer(int32) :: p1, p2, ind
!         real(real64) :: val1, val2

!         val1 = BC_Info%value(1)
!         val2 = BC_Info%value(2)

!         if (BC_Info%isUniform) then
!             b(p1) = b(p1) - 0.5d0 * val1 * val2 * Edge_Distance
!             b(p2) = b(p2) - 0.5d0 * val1 * val2 * Edge_Distance
!         end if
!         !! Assemble Heat Radiation Boundary to the Global Matrix
!         call A%Find(p1, p1, ind)
!         A%val(ind) = A%val(ind) + 2.0d0 * val1 * Edge_Distance / 6.0d0
!         call A%Find(p1, p2, ind)
!         A%val(ind) = A%val(ind) + val1 * Edge_Distance / 6.0d0
!         call A%Find(p2, p1, ind)
!         A%val(ind) = A%val(ind) + val1 * Edge_Distance / 6.0d0
!         call A%Find(p2, p2, ind)
!         A%val(ind) = A%val(ind) + 2.0d0 * val1 * Edge_Distance / 6.0d0

!     end subroutine Fix_BoundaryCondition_HeatRadiation_CRS

!     subroutine Fix_BoundaryCondition_HeatRadiation_Full(A, b, BC_Info, Edge, Edge_Distance)
!         implicit none
!         real(real64), intent(inout) :: A(:, :)
!         real(real64), intent(inout) :: b(:)
!         type(Condition_BC_Local), intent(inout) :: BC_Info
!         integer(int32), intent(in) :: Edge(:)
!         real(real64), intent(in) :: Edge_Distance

!         integer(int32) :: p1, p2, ind
!         real(real64) :: val1, val2

!         p1 = Edge(1)
!         p2 = Edge(2)
!         val1 = BC_Info%value(1)
!         val2 = BC_Info%value(2)

!         if (BC_Info%isUniform) then
!             b(p1) = b(p1) - 0.5d0 * val1 * val2 * Edge_Distance
!             b(p2) = b(p2) - 0.5d0 * val1 * val2 * Edge_Distance
!         end if

!         !! Assemble Heat Radiation Boundary to the Global Matrix
!         A(p1, p1) = A(p1, p1) + 2.0d0 * val1 * Edge_Distance / 6.0d0
!         A(p1, p2) = A(p1, p2) + val1 * Edge_Distance / 6.0d0
!         A(p2, p1) = A(p2, p1) + val1 * Edge_Distance / 6.0d0
!         A(p2, p2) = A(p2, p2) + 2.0d0 * val1 * Edge_Distance / 6.0d0

!     end subroutine Fix_BoundaryCondition_HeatRadiation_Full
end module Condition_Boundary

module Condition_Fix_Boundary_Conditions
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    use :: Allocate_Allocate
    use :: Allocate_Structure
    use :: Matrix_CRS
    use :: Inout_VTK
    implicit none
    private

    character(*), parameter :: Dirichlet = "Dirichlet"
    character(*), parameter :: Neumann = "Neumann"
    character(*), parameter :: Adiabatic = "Adiabatic"
    character(*), parameter :: FreeHeatDischarge = "FreeHeatDischarge"
    character(*), parameter :: Robin = "Robin"
    character(*), parameter :: HeatTransfer = "HeatTransfer"
    character(*), parameter :: HeatRadiation = "HeatRadiation"
    character(*), parameter :: WaterFlux = "WaterFlux"
    character(*), parameter :: HeatFlux = "HeatFlux"

    public :: Condition_BC_Local
    public :: Abstract_Condition_BC
    public :: Type_BC_Thermal

    type :: Type_Edge
        integer(int32) :: EdgeType
        integer(int32) :: EdgeGroup
        integer(int32), allocatable :: Conn(:)
        real(real64), allocatable :: Distance(:)
        type(Vector3D), allocatable :: UnitNormal(:)
    end type Type_Edge

    type :: Condition_BC_Local
        character(:), allocatable :: type
        logical(4) :: isUniform
        real(real64), allocatable :: value(:)
    end type Condition_BC_Local

    type, abstract :: Abstract_Condition_BC
        !! Boundary conditions information
        integer(int32) :: numBCGroup
        integer(int32), allocatable :: BCGroup(:)
        type(Condition_BC_Local), allocatable :: BC_Info(:)
        !! Boundary conditions information of the Edges
        integer(int32) :: numEdges
        type(Type_Edge), allocatable :: EdgeInfo(:)

        ! integer(int32), allocatable :: Edge(:, :)
        ! integer(int32), allocatable :: EdgeType(:)
        ! real(real64), allocatable :: Edge_Distances(:, :)
        ! type(DP3d) :: Edge_UnitNormal
    end type Abstract_Condition_BC

    type, extends(Abstract_Condition_BC) :: Type_BC_Thermal
    contains
        procedure, private, pass(self) :: Fix_BoundaryConditions_CRS
        procedure, private, pass(self) :: Fix_BoundaryConditions_Full
        generic, public :: Fix_BoundaryConditions => Fix_BoundaryConditions_CRS, & !&
                                                     Fix_BoundaryConditions_Full
        procedure, private, pass(self) :: Fix_Bounday_Values_CRS
        procedure, private, pass(self) :: Fix_Bounday_Values_Full
        procedure, private, pass(self) :: Fix_Bounday_Values_RHS
        generic, public :: Fix_Bounday_Values => Fix_Bounday_Values_CRS, & !&
                                                 Fix_Bounday_Values_Full, & !&
                                                 Fix_Bounday_Values_RHS
    end type
    ! public :: Fix_BoundaryConditions

    interface Type_BC_Thermal
        module procedure Type_BC_Thermal_Constructor
    end interface

    interface assignment(=)
        module procedure Condition_BC_Local_Assignment
    end interface

contains
    subroutine Condition_BC_Local_Assignment(A, B)
        implicit none
        type(Condition_BC_Local), intent(inout) :: A
        type(Condition_BC_Local), intent(in) :: B

        A%type = B%type
        A%isUniform = B%isUniform
        if (allocated(A%value)) deallocate (A%value)
        if (allocated(B%value)) allocate (A%value, source=B%value)
    end subroutine

    function Calculate_Edge_Dinstance(Edge, Coordinate) result(Edge_Distance)
        implicit none
        integer(int32), intent(in) :: Edge(:)
        type(DP3d), intent(in) :: Coordinate
        real(real64) :: Edge_Distance
        real(real64) :: x1, y1, x2, y2

        x1 = Coordinate%x(Edge(1))
        y1 = Coordinate%y(Edge(1))
        x2 = Coordinate%x(Edge(2))
        y2 = Coordinate%y(Edge(2))
        Edge_Distance = sqrt((x2 - x1)**2.0d0 + (y2 - y1)**2.0d0)
    end function

    function Calculate_Edge_UnitNormalVector(Edge, Coordinate, Distance) result(UnitNormalVector)
        integer(int32), intent(in) :: Edge(:)
        type(DP3d), intent(in) :: Coordinate
        real(real64), intent(in) :: Distance
        type(Vector3D) :: UnitNormalVector

        real(real64) :: x1, y1, x2, y2

        x1 = Coordinate%x(Edge(1))
        y1 = Coordinate%y(Edge(1))
        x2 = Coordinate%x(Edge(2))
        y2 = Coordinate%y(Edge(2))

        UnitNormalVector%x = (y2 - y1) / Distance
        UnitNormalVector%y = -(x2 - x1) / Distance

    end function Calculate_Edge_UnitNormalVector

    function Type_BC_Thermal_Constructor(Structure_Input, Input_VTK) result(Structure)
        implicit none
        type(Type_BC_Thermal), intent(in) :: Structure_Input
        type(Type_VTK), intent(in) :: Input_VTK
        type(Type_BC_Thermal) :: Structure

        integer(int32) :: i, iCell, idx
        integer(int32) :: minimum, maximum
        integer(int32) :: CounterEdge
        integer(int32) :: tmpEdge(2)

        allocate (Structure%BCGroup, source=Structure_Input%BCGroup)
        minimum = minval(Structure%BCGroup)
        maximum = maxval(Structure%BCGroup)
        allocate (Structure%BC_Info(minimum:maximum))
        do i = 1, size(Structure_Input%BC_Info)
            Structure%BC_Info(Structure%BCGroup(i)) = Structure_Input%BC_Info(Structure%BCGroup(i))
        end do

        CounterEdge = 0
        do iCell = 1, Input_VTK%numTotalCells
            if (Input_VTK%CELLS(iCell)%CellType == Input_VTK%Names%VTK_LINE .or. &
                Input_VTK%CELLS(iCell)%CellType == Input_VTK%Names%VTK_QUADRATIC_EDGE) then
                CounterEdge = CounterEdge + 1
            end if
        end do
        Structure%numEdges = CounterEdge
        allocate (Structure%EdgeInfo(Structure%numEdges))

        idx = 0
        do iCell = 1, Input_VTK%numTotalCells
            if (Input_VTK%CELLS(iCell)%CellType == Input_VTK%Names%VTK_LINE) then
                idx = idx + 1
                Structure%EdgeInfo(idx)%EdgeType = Input_VTK%Names%VTK_LINE
                Structure%EdgeInfo(idx)%EdgeGroup = Input_VTK%CELLS(iCell)%CellEntityId
                allocate (Structure%EdgeInfo(idx)%Conn, source=Input_VTK%CELLS(iCell)%Connectivity)
                allocate (Structure%EdgeInfo(idx)%Distance(1))
                allocate (Structure%EdgeInfo(idx)%UnitNormal(1))

                tmpEdge = [Structure%EdgeInfo(idx)%Conn(1), Structure%EdgeInfo(idx)%Conn(2)]
                Structure%EdgeInfo(idx)%Distance(1) = Calculate_Edge_Dinstance(tmpEdge, Input_VTK%POINTS)
                Structure%EdgeInfo(idx)%UnitNormal(1) = Calculate_Edge_UnitNormalVector(tmpEdge, Input_VTK%POINTS, Structure%EdgeInfo(idx)%Distance(1))
            else if (Input_VTK%CELLS(iCell)%CellType == Input_VTK%Names%VTK_QUADRATIC_EDGE) then
                idx = idx + 1
                Structure%EdgeInfo(idx)%EdgeType = Input_VTK%Names%VTK_QUADRATIC_EDGE
                Structure%EdgeInfo(idx)%EdgeGroup = Input_VTK%CELLS(iCell)%CellEntityId
                allocate (Structure%EdgeInfo(idx)%Conn, source=Input_VTK%CELLS(iCell)%Connectivity)
                allocate (Structure%EdgeInfo(idx)%Distance(2))
                allocate (Structure%EdgeInfo(idx)%UnitNormal(2))

                tmpEdge = [Structure%EdgeInfo(idx)%Conn(1), Structure%EdgeInfo(idx)%Conn(3)]
                Structure%EdgeInfo(idx)%Distance(1) = Calculate_Edge_Dinstance(tmpEdge, Input_VTK%POINTS)
                Structure%EdgeInfo(idx)%UnitNormal(1) = Calculate_Edge_UnitNormalVector(tmpEdge, Input_VTK%POINTS, Structure%EdgeInfo(idx)%Distance(1))
                tmpEdge = [Structure%EdgeInfo(idx)%Conn(3), Structure%EdgeInfo(idx)%Conn(2)]
                Structure%EdgeInfo(idx)%Distance(2) = Calculate_Edge_Dinstance(tmpEdge, Input_VTK%POINTS)
                Structure%EdgeInfo(idx)%UnitNormal(2) = Calculate_Edge_UnitNormalVector(tmpEdge, Input_VTK%POINTS, Structure%EdgeInfo(idx)%Distance(2))
            end if
        end do

    end function Type_BC_Thermal_Constructor

    subroutine Fix_BoundaryConditions_CRS(self, A, b, lambda, Cw, Structure_HeatFlux)
        implicit none
        class(Type_BC_Thermal), intent(inout) :: self
        type(Type_CRS), intent(inout) :: A
        real(real64), intent(inout) :: b(:)
        real(real64), optional, intent(in) :: lambda(:)
        real(real64), optional, intent(in) :: Cw
        type(DP3d), optional, intent(in) :: Structure_HeatFlux

        integer(int32) :: i

        do i = 1, self%numEdges
            if (self%EdgeInfo(i)%EdgeType == 3) then !! VTK_LINE
                select case (self%BC_Info(self%EdgeInfo(i)%EdgeGroup)%type)
                case (Neumann)
                    call Fix_BoundaryCondition_Neumann(b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:), self%EdgeInfo(i)%Distance(1), lambda)
                case (HeatFlux)
                    call Fix_BoundaryCondition_Flux(b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:), self%EdgeInfo(i)%Distance(1))
                case (Robin)
                    call Fix_BoundaryCondition_Robin_CRS(A, b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:), self%EdgeInfo(i)%Distance(1), self%EdgeInfo(i)%UnitNormal(1), Cw, Structure_HeatFlux)
                case (HeatTransfer)
                    call Fix_BoundaryCondition_HeatTransfer_CRS(A, b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:), self%EdgeInfo(i)%Distance(1))
                case (HeatRadiation)
                    call Fix_BoundaryCondition_HeatRadiation_CRS(A, b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:), self%EdgeInfo(i)%Distance(1))
                end select
            end if
        end do
        do i = 1, self%numEdges
            select case (self%BC_Info(self%EdgeInfo(i)%EdgeGroup)%type)
            case (Dirichlet)
                call Fix_BoundaryCondition_Dirichlet_CRS(A, b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:))
            end select
        end do

    end subroutine Fix_BoundaryConditions_CRS

    subroutine Fix_BoundaryConditions_Full(self, A, b, lambda, Cw, Structure_HeatFlux)
        implicit none
        class(Type_BC_Thermal), intent(inout) :: self
        real(real64), intent(inout) :: A(:, :)
        real(real64), intent(inout) :: b(:)
        real(real64), intent(in) :: lambda(:)
        real(real64), intent(in) :: Cw
        type(DP3d), intent(in) :: Structure_HeatFlux

        integer(int32) :: i

        do i = 1, self%numEdges
            select case (self%BC_Info(self%EdgeInfo(i)%EdgeGroup)%type)
            case (Neumann)
                call Fix_BoundaryCondition_Neumann(b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:), self%EdgeInfo(i)%Distance(1), lambda)
            case (HeatFlux)
                call Fix_BoundaryCondition_Flux(b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:), self%EdgeInfo(i)%Distance(1))
            case (Robin)
                call Fix_BoundaryCondition_Robin_Full(A, b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:), self%EdgeInfo(i)%Distance(1), self%EdgeInfo(i)%UnitNormal(1), Cw, Structure_HeatFlux)
            case (HeatTransfer)
                call Fix_BoundaryCondition_HeatTransfer_Full(A, b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:), self%EdgeInfo(i)%Distance(1))
            case (HeatRadiation)
                call Fix_BoundaryCondition_HeatRadiation_Full(A, b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:), self%EdgeInfo(i)%Distance(1))
            end select
        end do
        do i = 1, self%numEdges
            select case (self%BC_Info(self%EdgeInfo(i)%EdgeGroup)%type)
            case (Dirichlet)
                call Fix_BoundaryCondition_Dirichlet_Full(A, b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:))
            end select
        end do
    end subroutine Fix_BoundaryConditions_Full

    subroutine Fix_Bounday_Values_CRS(self, A, b)
        implicit none
        class(Type_BC_Thermal), intent(inout) :: self
        type(Type_CRS), intent(inout) :: A
        real(real64), intent(inout) :: b(:)
        integer(int32) :: i

        do i = 1, self%numEdges
            select case (self%BC_Info(self%EdgeInfo(i)%EdgeGroup)%type)
            case (Dirichlet)
                call Fix_BoundaryCondition_Dirichlet_CRS_Value(A, b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:))
            end select
        end do
    end subroutine Fix_Bounday_Values_CRS

    subroutine Fix_Bounday_Values_Full(self, A, b)
        implicit none
        class(Type_BC_Thermal), intent(inout) :: self
        real(real64), intent(inout) :: A(:, :)
        real(real64), intent(inout) :: b(:)
        integer(int32) :: i

        do i = 1, self%numEdges
            select case (self%BC_Info(self%EdgeInfo(i)%EdgeGroup)%type)
            case (Dirichlet)
                call Fix_BoundaryCondition_Dirichlet_Full(A, b, self%BC_Info(self%EdgeInfo(i)%EdgeGroup), self%EdgeInfo(i)%Conn(:))
            end select
        end do
    end subroutine Fix_Bounday_Values_Full

    subroutine Fix_Bounday_Values_RHS(self, b)
        implicit none
        class(Type_BC_Thermal), intent(inout) :: self
        real(real64), intent(inout) :: b(:)
        integer(int32) :: i

        do i = 1, self%numEdges
            select case (self%BC_Info(self%EdgeInfo(i)%EdgeGroup)%type)
            case (Dirichlet)
                if (self%BC_Info(self%EdgeInfo(i)%EdgeGroup)%isUniform) then
                    b(self%EdgeInfo(i)%Conn(1)) = self%BC_Info(self%EdgeInfo(i)%EdgeGroup)%value(1)
                    b(self%EdgeInfo(i)%Conn(2)) = self%BC_Info(self%EdgeInfo(i)%EdgeGroup)%value(1)
                end if
            end select
        end do
    end subroutine Fix_Bounday_Values_RHS

    subroutine Fix_BoundaryCondition_Dirichlet_CRS(A, b, BC_Info, Edge)
        implicit none
        type(Type_CRS), intent(inout) :: A
        real(real64), intent(inout) :: b(:)
        type(Condition_BC_Local), intent(inout) :: BC_Info
        integer(int32), intent(in) :: Edge(:)
        integer(int32) :: i, ind, ps, pe
        integer(int32) :: p1, p2

        if (BC_Info%isUniform) then
            p1 = Edge(1)
            p2 = Edge(2)

            call A%Find(p1, p1, ind)
            ps = A%Ptr(p1)
            pe = A%Ptr(p1 + 1) - 1
            A%val(ps:pe) = 0.0d0
            A%val(ind) = 1.0d0
            b(p1) = 0.0d0

            call A%Find(p2, p2, ind)
            ps = A%Ptr(p2)
            pe = A%Ptr(p2 + 1) - 1
            A%val(ps:pe) = 0.0d0
            A%val(ind) = 1.0d0
            b(p2) = 0.0d0
        end if
    end subroutine Fix_BoundaryCondition_Dirichlet_CRS

    subroutine Fix_BoundaryCondition_Dirichlet_CRS_Value(A, b, BC_Info, Edge)
        implicit none
        type(Type_CRS), intent(inout) :: A
        real(real64), intent(inout) :: b(:)
        type(Condition_BC_Local), intent(inout) :: BC_Info
        integer(int32), intent(in) :: Edge(:)
        integer(int32) :: i, ind, ps, pe
        integer(int32) :: p1, p2

        if (BC_Info%isUniform) then
            p1 = Edge(1)
            p2 = Edge(2)

            call A%Find(p1, p1, ind)
            ps = A%Ptr(p1)
            pe = A%Ptr(p1 + 1) - 1
            A%val(ps:pe) = 0.0d0
            A%val(ind) = 1.0d0
            b(p1) = BC_Info%value(1)

            call A%Find(p2, p2, ind)
            ps = A%Ptr(p2)
            pe = A%Ptr(p2 + 1) - 1
            A%val(ps:pe) = 0.0d0
            A%val(ind) = 1.0d0
            b(p2) = BC_Info%value(1)
        end if
    end subroutine Fix_BoundaryCondition_Dirichlet_CRS_Value

    subroutine Fix_BoundaryCondition_Dirichlet_CRS_Initial(A, b, BC_Info, Edge)
        implicit none
        type(Type_CRS), intent(inout) :: A
        real(real64), intent(inout) :: b(:)
        type(Condition_BC_Local), intent(inout) :: BC_Info
        integer(int32), intent(in) :: Edge(:)
        integer(int32) :: i, ind, ps, pe
        integer(int32) :: p1, p2

        if (BC_Info%isUniform) then
            p1 = Edge(1)
            p2 = Edge(2)

            call A%Find(p1, p1, ind)
            ps = A%Ptr(p1)
            pe = A%Ptr(p1 + 1) - 1
            A%val(ps:pe) = 0.0d0
            A%val(ind) = 1.0d0
            b(p1) = BC_Info%value(1)

            call A%Find(p2, p2, ind)
            ps = A%Ptr(p2)
            pe = A%Ptr(p2 + 1) - 1
            A%val(ps:pe) = 0.0d0
            A%val(ind) = 1.0d0
            b(p2) = BC_Info%value(1)

        end if
    end subroutine Fix_BoundaryCondition_Dirichlet_CRS_Initial

    subroutine Fix_BoundaryCondition_Dirichlet_Full(A, b, BC_Info, Edge)
        implicit none
        real(real64), intent(inout) :: A(:, :)
        real(real64), intent(inout) :: b(:)
        type(Condition_BC_Local), intent(inout) :: BC_Info
        integer(int32), intent(in) :: Edge(:)
        integer(int32) :: i, ind, ps, pe
        integer(int32) :: p1, p2

        if (BC_Info%isUniform) then
            p1 = Edge(1)
            p2 = Edge(2)

            A(p1, :) = 0.0d0
            A(p1, p1) = 1.0d0
            b(p1) = 0.0d0

            A(p2, :) = 0.0d0
            A(p2, p2) = 1.0d0
            b(p2) = 0.0d0
        end if
    end subroutine Fix_BoundaryCondition_Dirichlet_Full

    subroutine Fix_BoundaryCondition_Neumann(b, BC_Info, Edge, Edge_Distance, c)
        implicit none
        type(Condition_BC_Local), intent(inout) :: BC_Info
        integer(int32), intent(in) :: Edge(:)
        real(real64), intent(in) :: Edge_Distance
        real(real64), intent(inout) :: b(:)
        real(real64), intent(in) :: c(:)

        integer(int32) :: p1, p2

        p1 = Edge(1)
        p2 = Edge(2)
        if (BC_Info%isUniform) then
            b(p1) = b(p1) + (2.0d0 * BC_Info%value(1) * c(p1) + BC_Info%value(1) * c(p2)) * Edge_Distance / 6.0d0
            b(p2) = b(p2) + (BC_Info%value(1) * c(p1) + 2.0d0 * BC_Info%value(1) * c(p2)) * Edge_Distance / 6.0d0
        end if

    end subroutine Fix_BoundaryCondition_Neumann

    subroutine Fix_BoundaryCondition_Flux(b, BC_Info, Edge, Edge_Distance)
        implicit none
        real(real64), intent(inout) :: b(:)
        type(Condition_BC_Local), intent(inout) :: BC_Info
        integer(int32), intent(in) :: Edge(:)
        real(real64), intent(in) :: Edge_Distance

        integer(int32) :: p1, p2
        real(real64) :: val

        p1 = Edge(1)
        p2 = Edge(2)
        if (BC_Info%isUniform) then
            val = 0.5d0 * BC_Info%value(1) * Edge_Distance
            b(p1) = b(p1) - val
            b(p2) = b(p2) - val
        end if

    end subroutine Fix_BoundaryCondition_Flux

    subroutine Fix_BoundaryCondition_Robin_CRS(A, b, BC_Info, Edge, Edge_Distance, Edge_UnitNormal, Cw, Flux)
        implicit none
        type(Type_CRS), intent(inout) :: A
        real(real64), intent(inout) :: b(:)
        type(Condition_BC_Local), intent(inout) :: BC_Info
        integer(int32), intent(in) :: Edge(:)
        real(real64), intent(in) :: Edge_Distance
        type(Vector3D), intent(in) :: Edge_UnitNormal
        real(real64), intent(in) :: Cw
        type(DP3d), intent(in) :: Flux

        integer(int32) :: p1, p2, ind
        real(real64) :: Q1, Q2

        p1 = Edge(1)
        p2 = Edge(2)

        Q1 = Flux%x(p1) * Edge_UnitNormal%x + Flux%y(p1) * Edge_UnitNormal%y
        Q2 = Flux%x(p2) * Edge_UnitNormal%x + Flux%y(p2) * Edge_UnitNormal%y
        if (BC_Info%isUniform) then
            b(p1) = b(p1) - (2.0d0 * Q1 + Q2) * Edge_Distance / 6.0d0
            b(p2) = b(p2) - (Q1 + 2.0d0 * Q2) * Edge_Distance / 6.0d0
        end if

        !! Assemble Robin (Cauthy) Boundary to the Global Matrix
        call A%Find(p1, p1, ind)
        A%val(ind) = A%val(ind) + 2.0d0 * Q1 * Edge_Distance / 6.0d0
        call A%Find(p1, p2, ind)
        A%val(ind) = A%val(ind) + Q1 * Edge_Distance / 6.0d0
        call A%Find(p2, p1, ind)
        A%val(ind) = A%val(ind) + Q2 * Edge_Distance / 6.0d0
        call A%Find(p2, p2, ind)
        A%val(ind) = A%val(ind) + 2.0d0 * Q2 * Edge_Distance / 6.0d0

    end subroutine Fix_BoundaryCondition_Robin_CRS

    subroutine Fix_BoundaryCondition_Robin_Full(A, b, BC_Info, Edge, Edge_Distance, Edge_UnitNormal, Cw, Flux)
        implicit none
        real(real64), intent(inout) :: A(:, :)
        real(real64), intent(inout) :: b(:)
        type(Condition_BC_Local), intent(inout) :: BC_Info
        integer(int32), intent(in) :: Edge(:)
        real(real64), intent(in) :: Edge_Distance
        type(Vector3D), intent(in) :: Edge_UnitNormal
        real(real64), intent(in) :: Cw
        type(DP3d), intent(in) :: Flux

        integer(int32) :: p1, p2, ind
        real(real64) :: Q1, Q2

        p1 = Edge(1)
        p2 = Edge(2)

        Q1 = Flux%x(p1) * Edge_UnitNormal%x + Flux%y(p1) * Edge_UnitNormal%y
        Q2 = Flux%x(p2) * Edge_UnitNormal%x + Flux%y(p2) * Edge_UnitNormal%y
        if (BC_Info%isUniform) then
            b(p1) = b(p1) - (2.0d0 * Q1 + Q2) * Edge_Distance / 6.0d0
            b(p2) = b(p2) - (Q1 + 2.0d0 * Q2) * Edge_Distance / 6.0d0
        end if

        !! Assemble Robin (Cauthy) Boundary to the Global Matrix
        A(p1, p1) = A(p1, p1) + 2.0d0 * Q1 * Edge_Distance / 6.0d0
        A(p1, p2) = A(p1, p2) + Q1 * Edge_Distance / 6.0d0
        A(p2, p1) = A(p2, p1) + Q2 * Edge_Distance / 6.0d0
        A(p2, p2) = A(p2, p2) + 2.0d0 * Q2 * Edge_Distance / 6.0d0
    end subroutine Fix_BoundaryCondition_Robin_Full

    subroutine Fix_BoundaryCondition_HeatTransfer_CRS(A, b, BC_Info, Edge, Edge_Distance)
        implicit none
        type(Type_CRS), intent(inout) :: A
        real(real64), intent(inout) :: b(:)
        type(Condition_BC_Local), intent(inout) :: BC_Info
        integer(int32), intent(in) :: Edge(:)
        real(real64), intent(in) :: Edge_Distance

        integer(int32) :: p1, p2, ind
        real(real64) :: val1, val2

        p1 = Edge(1)
        p2 = Edge(2)
        val1 = BC_Info%value(1)
        val2 = BC_Info%value(2)

        if (BC_Info%isUniform) then
            b(p1) = b(p1) - 0.5d0 * val1 * val2 * Edge_Distance
            b(p2) = b(p2) - 0.5d0 * val1 * val2 * Edge_Distance
        end if

        !! Assemble Heat Transfer Boundary to the Global Matrix
        call A%Find(p1, p1, ind)
        A%val(ind) = A%val(ind) + 2.0d0 * val1 * Edge_Distance / 6.0d0
        call A%Find(p1, p2, ind)
        A%val(ind) = A%val(ind) + val1 * Edge_Distance / 6.0d0
        call A%Find(p2, p1, ind)
        A%val(ind) = A%val(ind) + val1 * Edge_Distance / 6.0d0
        call A%Find(p2, p2, ind)
        A%val(ind) = A%val(ind) + 2.0d0 * val1 * Edge_Distance / 6.0d0

    end subroutine Fix_BoundaryCondition_HeatTransfer_CRS

    subroutine Fix_BoundaryCondition_HeatTransfer_Full(A, b, BC_Info, Edge, Edge_Distance)
        implicit none
        real(real64), intent(inout) :: A(:, :)
        real(real64), intent(inout) :: b(:)
        type(Condition_BC_Local), intent(inout) :: BC_Info
        integer(int32), intent(in) :: Edge(:)
        real(real64), intent(in) :: Edge_Distance

        integer(int32) :: p1, p2, ind
        real(real64) :: val1, val2

        p1 = Edge(1)
        p2 = Edge(2)
        val1 = BC_Info%value(1)
        val2 = val2

        if (BC_Info%isUniform) then
            b(p1) = b(p1) - 0.5d0 * val1 * val2 * Edge_Distance
            b(p2) = b(p2) - 0.5d0 * val1 * val2 * Edge_Distance
        end if

        !! Assemble Heat Transfer Boundary to the Global Matrix
        A(p1, p1) = A(p1, p1) + 2.0d0 * val1 * Edge_Distance / 6.0d0
        A(p1, p2) = A(p1, p2) + val1 * Edge_Distance / 6.0d0
        A(p2, p1) = A(p2, p1) + val1 * Edge_Distance / 6.0d0
        A(p2, p2) = A(p2, p2) + 2.0d0 * val1 * Edge_Distance / 6.0d0

    end subroutine Fix_BoundaryCondition_HeatTransfer_Full

    subroutine Fix_BoundaryCondition_HeatRadiation_CRS(A, b, BC_Info, Edge, Edge_Distance)
        implicit none
        type(Type_CRS), intent(inout) :: A
        real(real64), intent(inout) :: b(:)
        type(Condition_BC_Local), intent(inout) :: BC_Info
        integer(int32), intent(in) :: Edge(:)
        real(real64), intent(in) :: Edge_Distance

        integer(int32) :: p1, p2, ind
        real(real64) :: val1, val2

        val1 = BC_Info%value(1)
        val2 = BC_Info%value(2)

        if (BC_Info%isUniform) then
            b(p1) = b(p1) - 0.5d0 * val1 * val2 * Edge_Distance
            b(p2) = b(p2) - 0.5d0 * val1 * val2 * Edge_Distance
        end if
        !! Assemble Heat Radiation Boundary to the Global Matrix
        call A%Find(p1, p1, ind)
        A%val(ind) = A%val(ind) + 2.0d0 * val1 * Edge_Distance / 6.0d0
        call A%Find(p1, p2, ind)
        A%val(ind) = A%val(ind) + val1 * Edge_Distance / 6.0d0
        call A%Find(p2, p1, ind)
        A%val(ind) = A%val(ind) + val1 * Edge_Distance / 6.0d0
        call A%Find(p2, p2, ind)
        A%val(ind) = A%val(ind) + 2.0d0 * val1 * Edge_Distance / 6.0d0

    end subroutine Fix_BoundaryCondition_HeatRadiation_CRS

    subroutine Fix_BoundaryCondition_HeatRadiation_Full(A, b, BC_Info, Edge, Edge_Distance)
        implicit none
        real(real64), intent(inout) :: A(:, :)
        real(real64), intent(inout) :: b(:)
        type(Condition_BC_Local), intent(inout) :: BC_Info
        integer(int32), intent(in) :: Edge(:)
        real(real64), intent(in) :: Edge_Distance

        integer(int32) :: p1, p2, ind
        real(real64) :: val1, val2

        p1 = Edge(1)
        p2 = Edge(2)
        val1 = BC_Info%value(1)
        val2 = BC_Info%value(2)

        if (BC_Info%isUniform) then
            b(p1) = b(p1) - 0.5d0 * val1 * val2 * Edge_Distance
            b(p2) = b(p2) - 0.5d0 * val1 * val2 * Edge_Distance
        end if

        !! Assemble Heat Radiation Boundary to the Global Matrix
        A(p1, p1) = A(p1, p1) + 2.0d0 * val1 * Edge_Distance / 6.0d0
        A(p1, p2) = A(p1, p2) + val1 * Edge_Distance / 6.0d0
        A(p2, p1) = A(p2, p1) + val1 * Edge_Distance / 6.0d0
        A(p2, p2) = A(p2, p2) + 2.0d0 * val1 * Edge_Distance / 6.0d0

    end subroutine Fix_BoundaryCondition_HeatRadiation_Full
end module Condition_Fix_Boundary_Conditions

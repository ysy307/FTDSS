module conditions_boundary
    use, intrinsic :: iso_fortran_env
    use :: stdlib_strings, only:to_string
    use :: stdlib_logger
    use :: module_core
    use :: module_domain, only:type_domain, holder_sides
    use :: module_input
    implicit none
    private

    public :: abst_bc_thermal
    public :: type_bc_thermal_dirichlet
    public :: type_bc_thermal_adiabatic

    public :: mode_value
    public :: mode_nr
    public :: mode_ic

    integer(int32), parameter :: mode_value = 1
    integer(int32), parameter :: mode_nr = 0
    integer(int32), parameter :: mode_ic = -1

    type, abstract :: abst_bc_thermal
        integer(int32) :: material_id
        character(:), allocatable :: boundary_name
        real(real64), allocatable, private :: time_points(:) ! 時間ステップ
        real(real64), allocatable, private :: values(:) ! 対応する値
        integer(int32), private :: num_target_edges ! 対象エッジの数
        integer(int32), allocatable, private :: target_edges(:, :) ! 対象エッジの接続情報
    contains
        procedure(abst_initialize_bc),       pass(self), deferred :: initialize !&
        procedure(abst_apply_bc_dence),      pass(self), deferred :: apply_dense !&
        procedure(abst_apply_bc_sparse_crs), pass(self), deferred :: apply_crs !&
    end type abst_bc_thermal

    type, extends(abst_bc_thermal) :: type_bc_thermal_dirichlet
        logical, private :: is_uniform
    contains
        procedure, pass(self) :: initialize  => initialize_type_bc_thermal_dirichlet !&
        procedure, pass(self) :: apply_dense => apply_dense_thermal_dirichlet !&
        procedure, pass(self) :: apply_crs   => apply_crs_thermal_dirichlet !&
    end type type_bc_thermal_dirichlet

    type, extends(abst_bc_thermal) :: type_bc_thermal_adiabatic
    contains
        procedure, pass(self) :: initialize  => initialize_type_bc_thermal_adiabatic !&
        procedure, pass(self) :: apply_dense => apply_dense_thermal_adiabatic !&
        procedure, pass(self) :: apply_crs   => apply_crs_thermal_adiabatic !&
    end type type_bc_thermal_adiabatic

    abstract interface
        subroutine abst_initialize_bc(self, input, domain, id, i_material, time_conv)
            import :: abst_bc_thermal, type_input, int32, type_domain, real64
            implicit none
            class(abst_bc_thermal), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_domain), intent(inout) :: domain
            integer(int32), intent(in) :: id
            integer(int32), intent(in) :: i_material
            real(real64), intent(in) :: time_conv
        end subroutine abst_initialize_bc

        subroutine abst_apply_bc_dence(self, current_time, A, b, domain, mode)
            import :: abst_bc_thermal, real64, type_domain, int32
            implicit none
            class(abst_bc_thermal), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(inout), optional :: A(:, :)
            real(real64), intent(inout) :: b(:)
            type(type_domain), intent(inout) :: domain
            integer(int32), intent(in), optional :: mode
        end subroutine abst_apply_bc_dence

        subroutine abst_apply_bc_sparse_crs(self, current_time, A, b, domain, mode)
            import :: abst_bc_thermal, real64, type_crs, type_domain, int32
            implicit none
            class(abst_bc_thermal), intent(in) :: self
            real(real64), intent(in) :: current_time
            type(type_crs), intent(inout), optional :: A
            real(real64), intent(inout) :: b(:)
            type(type_domain), intent(inout) :: domain
            integer(int32), intent(in), optional :: mode
        end subroutine abst_apply_bc_sparse_crs
    end interface

    interface
        module subroutine initialize_type_bc_thermal_dirichlet(self, input, domain, id, i_material, time_conv)
            implicit none
            class(type_bc_thermal_dirichlet), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_domain), intent(inout) :: domain
            integer(int32), intent(in) :: id
            integer(int32), intent(in) :: i_material
            real(real64), intent(in) :: time_conv

        end subroutine initialize_type_bc_thermal_dirichlet

        module subroutine apply_dense_thermal_dirichlet(self, current_time, A, b, domain, mode)
            implicit none
            class(type_bc_thermal_dirichlet), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(inout), optional :: A(:, :)
            real(real64), intent(inout) :: b(:)
            type(type_domain), intent(inout) :: domain
            integer(int32), intent(in), optional :: mode

        end subroutine apply_dense_thermal_dirichlet

        module subroutine apply_crs_thermal_dirichlet(self, current_time, A, b, domain, mode)
            implicit none
            class(type_bc_thermal_dirichlet), intent(in) :: self
            real(real64), intent(in) :: current_time
            type(type_crs), intent(inout), optional :: A
            real(real64), intent(inout) :: b(:)
            type(type_domain), intent(inout) :: domain
            integer(int32), intent(in), optional :: mode

        end subroutine apply_crs_thermal_dirichlet
    end interface

    interface
        module subroutine initialize_type_bc_thermal_adiabatic(self, input, domain, id, i_material, time_conv)
            implicit none
            class(type_bc_thermal_adiabatic), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_domain), intent(inout) :: domain
            integer(int32), intent(in) :: id
            integer(int32), intent(in) :: i_material
            real(real64), intent(in) :: time_conv

        end subroutine initialize_type_bc_thermal_adiabatic

        module subroutine apply_dense_thermal_adiabatic(self, current_time, A, b, domain, mode)
            implicit none
            class(type_bc_thermal_adiabatic), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(inout), optional :: A(:, :)
            real(real64), intent(inout) :: b(:)
            type(type_domain), intent(inout) :: domain
            integer(int32), intent(in), optional :: mode

        end subroutine apply_dense_thermal_adiabatic

        module subroutine apply_crs_thermal_adiabatic(self, current_time, A, b, domain, mode)
            implicit none
            class(type_bc_thermal_adiabatic), intent(in) :: self
            real(real64), intent(in) :: current_time
            type(type_crs), intent(inout), optional :: A
            real(real64), intent(inout) :: b(:)
            type(type_domain), intent(inout) :: domain
            integer(int32), intent(in), optional :: mode

        end subroutine apply_crs_thermal_adiabatic
    end interface

    interface
        module subroutine calculate_time_coefficient(time, arr_time, time_coefficient, idx)
            implicit none
            real(real64), intent(in) :: time
            real(real64), intent(in) :: arr_time(:)
            real(real64), intent(inout) :: time_coefficient
            integer(int32), intent(inout) :: idx

        end subroutine calculate_time_coefficient

        module subroutine find_target_edges_by_group(domain, i_material, target_edges)
            implicit none
            type(type_domain), intent(inout) :: domain
            integer(int32), intent(in) :: i_material
            integer(int32), allocatable, intent(inout) :: target_edges(:, :)

        end subroutine find_target_edges_by_group
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
!         type(type_crs), intent(inout) :: A
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
!         type(type_crs), intent(inout) :: A
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
!         type(type_crs), intent(inout) :: A
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
!         type(type_crs), intent(inout) :: A
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
!         type(type_crs), intent(inout) :: A
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
!         type(type_crs), intent(inout) :: A
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
!         type(type_crs), intent(inout) :: A
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
!         type(type_crs), intent(inout) :: A
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
end module conditions_boundary

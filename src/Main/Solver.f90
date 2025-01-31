module Main_Solver
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    use :: Inout_Input
    use :: calculate_count
    use :: Allocate_Allocate, only:Allocate_Array
    use :: Calculate_Unique, only:Unique
    use :: Allocate_Structure, only:Allocate_DP, Allocate_Structure_Thermal_Type
    use :: Calculate_Area, only:Calc_Area
    use :: Matrix_ConvertCRS, only:Convert_CRS

    implicit none
    private
    integer(int32), parameter :: Calc_Heat = 1

    public :: Class_Solver
    ! public :: Class_Solver_Heat

    type Class_Solver
        type(Type_Geometry) :: Geometry
        type(CRS) :: Matrix_CRS

        integer(int32), allocatable :: BCGroup(:)
        type(BC_Condition), allocatable :: BC(:)
        type(IC_Condition) :: IC
        class(Base_Parameters), allocatable :: Parms(:)

    contains

    end type Class_Solver

    interface Class_Solver
        module procedure Constructor
    end interface

contains

    type(Class_Solver) function Constructor(Structure_Input, Construct_target)
        type(Input), intent(in) :: Structure_Input
        character(*), intent(in) :: Construct_target

        select case (Construct_target)
        case ("Thermal")
            call Set_Geometory_Infomation(Constructor, Structure_Input, Construct_target)
            call Set_Condition_Infomations(Constructor, Structure_Input, Construct_target)
            print *, "Set_Heat_Variables"
            call Set_Heat_Variables(Constructor, Structure_Input)
            ! allocate (Heat_Variables :: Constructor%Variables)
            ! call Set_Calculate_GCC_Segregation
        case ("Hydraulic")
            call Set_Geometory_Infomation(Constructor, Structure_Input, Construct_target)
            call Set_Condition_Infomations(Constructor, Structure_Input, Construct_target)
            ! allocate (Water_Variables :: Constructor%Variables)
        case default
            print *, "Error: Invalid Construct_target"
            stop
        end select

    end function Constructor

    subroutine Set_Geometory_Infomation(self, Structure_Input, Construct_target)
        implicit none
        type(Class_Solver), intent(inout) :: self
        type(Input), intent(in) :: Structure_Input
        character(*), intent(in) :: Construct_target
        procedure(condition_function), pointer :: condition_ptr => null()

        type(Type_Region), allocatable :: Work_Regions(:)
        integer(int32), allocatable :: Work_CellEntityIds(:), Work_CellEntityIdUnique(:)
        integer(int32), allocatable :: Work_CellCounts(:)
        integer(int32), allocatable :: Work_Elements(:, :)
        integer(int32) :: numCellTypes, numUniqueCellEntityIds

        integer(int32) :: i, j, tmp, sums, count
        integer(int32) :: numElements

        character(:), allocatable :: key

        call Structure_Input%Get(self%Geometry%Basic)
        allocate (Work_Regions(self%Geometry%Basic%Region))
        call Structure_Input%Get("CellEntityIds", Work_CellEntityIds)
        call Structure_Input%Get("numCellTypes", numCellTypes)

        call Unique(Work_CellEntityIds, Work_CellEntityIdUnique)
        if (associated(condition_ptr)) nullify (condition_ptr)
        condition_ptr => Condition_BelongingGroup
        numUniqueCellEntityIds = size(Work_CellEntityIdUnique)
        call Allocate_Array(Work_CellCounts, numUniqueCellEntityIds)
        do i = 1, numUniqueCellEntityIds
            Work_CellCounts(i) = Count_if(Work_CellEntityIds, condition_ptr, Work_CellEntityIdUnique(i))
        end do

        select case (Construct_target)
        case ("Thermal")
            key = "Thermal"
        case ("Hydraulic")
            key = "Hydraulic"
        end select

        numElements = 0
        do i = 1, self%Geometry%Basic%Region
            call Structure_Input%Get(Work_Regions(i), i, key)
            if (Work_Regions(i)%Flags%isHeat) then
                numElements = numElements + Work_CellCounts(Work_Regions(i)%BelongingSurface)
            end if
        end do

        !TODO: 三角形要素のみにしか適用していないので，今後修正が必要
        call Allocate_Array(self%Geometry%Element, 3_int32, numElements)
        call Allocate_Array(self%Geometry%Element_Region, numElements)
        ! 5 => VTK_TRIANGLE
        sums = 0
        do i = 1, 4
            call Structure_Input%Get("nCell", tmp, i)
            sums = sums + tmp
        end do

        count = 0
        call Structure_Input%Get("CellNodes", Work_Elements, 5_int32)
        elem: do i = 1, size(Work_Elements(:, :), 2)
            reg: do j = 1, self%Geometry%Basic%Region
            if (Work_Regions(j)%Flags%isHeat) then
                if (Work_Regions(j)%BelongingSurface == Work_CellEntityIds(sums + i)) then
                    count = count + 1
                    self%Geometry%Element(:, count) = Work_Elements(:, i)
                    self%Geometry%Element_Region(count) = Work_CellEntityIds(sums + i)
                    exit reg
                end if
            end if
            end do reg
        end do elem

        call Structure_Input%Get("POINTS", self%Geometry%Nodes)
        call Allocate_Array(self%Geometry%Area, numElements)
        self%Geometry%Basic%Element = numElements
        self%Geometry%Basic%Node = size(self%Geometry%Nodes%x, 1)

        call Calc_Area(self%Geometry)

        call Convert_CRS(self%Geometry, self%Matrix_CRS)

        if (allocated(Work_Regions)) deallocate (Work_Regions)
        if (allocated(Work_CellEntityIds)) deallocate (Work_CellEntityIds)
        if (allocated(Work_CellEntityIdUnique)) deallocate (Work_CellEntityIdUnique)
        if (allocated(Work_CellCounts)) deallocate (Work_CellCounts)
        if (allocated(Work_Elements)) deallocate (Work_Elements)

    end subroutine Set_Geometory_Infomation

    subroutine Set_Condition_Infomations(self, Structure_Input, Construct_target)
        implicit none
        type(Class_Solver), intent(inout) :: self
        type(Input), intent(in) :: Structure_Input
        character(*), intent(in) :: Construct_target

        integer(int32) :: i
        integer(int32) :: numBC, numIC
        character(:), allocatable :: key

        select case (Construct_target)
        case ("Thermal")
            key = "Thermal"
        case ("Hydraulic")
            key = "Hydraulic"
        end select

        call Structure_Input%Get(key, self%BCGroup, self%BC)
        call Structure_Input%Get(key, self%IC)

    end subroutine Set_Condition_Infomations

    subroutine Set_Heat_Variables(self, Structure_Input)
        !> Set the heat variables
        implicit none
        type(Class_Solver), intent(inout) :: self
        type(Input), intent(in) :: Structure_Input

        type(Type_Region), allocatable :: Work_Regions(:)

        integer(int32) :: i

        allocate (Work_Regions(self%Geometry%Basic%Region))
        allocate (Heat_Parameters :: self%Parms(self%Geometry%Basic%Region))

        do i = 1, self%Geometry%Basic%Region
            call Structure_Input%Get(Work_Regions(i), i, "Thermal")
            if (Work_Regions(i)%Flags%isHeat) then
                select type (Parms => self%Parms(i))
                type is (Heat_Parameters)
                    call Allocate_Structure_Thermal_Type(Parms%Constants, Work_Regions(i)%Flags)
                    call Structure_Input%Get(Parms%Constants, i)
                end select
            end if
        end do

        if (allocated(Work_Regions)) deallocate (Work_Regions)

    end subroutine Set_Heat_Variables

    ! subroutine Set_

    logical(4) function Condition_BelongingGroup(num, Group)
        implicit none
        integer(int32), intent(in) :: num
        integer(int32), intent(in) :: Group

        if (num == Group) then
            Condition_BelongingGroup = .true.
        else
            Condition_BelongingGroup = .false.
        end if
    end function Condition_BelongingGroup

end module Main_Solver

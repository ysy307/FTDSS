module Main_Water
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    use :: Inout_Input
    use :: calculate_count, only:Count_if
    use :: allocate, only:Allocate_Vector, Allocate_Matrix
    use :: Calculate_Unique, only:Unique
    use :: Allocate_Structure, only:Allocate_DP
    use :: Calculate_Area, only:Calc_Area
    use :: Matrix_ConvertCRS, only:Convert_CRS

    implicit none
    private
    integer(int32), parameter :: Calc_Water = 1

    public :: Water

    type Water
        type(Type_Geometry) :: Geometry
        type(Boudary_Condition) :: BC
        type(CRS) :: Water_CRS
    contains

    end type Water

    interface Water
        module procedure Water_Constructor
    end interface

contains

    type(Water) function Water_Constructor(Structure_Input)
        type(Input), intent(in) :: Structure_Input

        call Set_Geometory_Infomation(Water_Constructor, Structure_Input)
    end function Water_Constructor

    subroutine Set_Geometory_Infomation(self, Structure_Input)
        implicit none
        type(Water), intent(inout) :: self
        type(Input), intent(in) :: Structure_Input
        procedure(condition_function), pointer :: condition_ptr => null()

        type(Type_Region), allocatable :: Work_Regions(:)
        integer(int32), allocatable :: Work_CellEntityIds(:), Work_CellEntityIdUnique(:)
        integer(int32), allocatable :: Work_CellCounts(:)
        integer(int32), allocatable :: Work_Elements(:, :)
        integer(int32) :: numCellTypes

        integer(int32) :: i, j, tmp, sums, count
        integer(int32) :: numElements

        call Structure_Input%Get(self%Geometry%Basic)
        allocate (Work_Regions(self%Geometry%Basic%Region))
        call Structure_Input%Get("CellEntityIds", Work_CellEntityIds)
        call Structure_Input%Get("numCellTypes", numCellTypes)

        call Unique(Work_CellEntityIds, Work_CellEntityIdUnique)
        condition_ptr => Condition_BelongingGroup
        call Allocate_Vector(Work_CellCounts, size(Work_CellEntityIdUnique))
        do i = 1, size(Work_CellEntityIdUnique)
            Work_CellCounts(i) = Count_if(Work_CellEntityIds, condition_ptr, Work_CellEntityIdUnique(i))
        end do

        numElements = 0
        do i = 1, self%Geometry%Basic%Region
            call Structure_Input%Get(Work_Regions(i), i, "Hydraulic")
            if (Work_Regions(i)%Flags%isWater) then
                numElements = numElements + Work_CellCounts(Work_Regions(i)%BelongingSurface)
            end if
        end do
        !TODO: 三角形要素のみにしか適用していないので，今後修正が必要
        call Allocate_Matrix(self%Geometry%Element, 3, numElements)
        call Allocate_Vector(self%Geometry%Element_Region, numElements)
        ! 5 => VTK_TRIANGLE
        sums = 0
        do i = 1, 4
            call Structure_Input%Get("nCell", tmp, i)
            sums = sums + tmp
        end do

        ! print *, sums, Work_CellEntityIds(sums:sums + 1)
        count = 0
        call Structure_Input%Get("CellNodes", Work_Elements, 5)
        elem: do i = 1, size(Work_Elements(:, :), 2)
            reg: do j = 1, self%Geometry%Basic%Region
            if (Work_Regions(j)%Flags%isWater) then
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
        call Allocate_Vector(self%Geometry%Area, numElements)
        self%Geometry%Basic%Element = numElements
        self%Geometry%Basic%Node = size(self%Geometry%Nodes%x, 1)

        call Calc_Area(self%Geometry)

        call Convert_CRS(self%Geometry, self%Water_CRS)

        if (allocated(Work_Regions)) deallocate (Work_Regions)
        if (allocated(Work_CellEntityIds)) deallocate (Work_CellEntityIds)
        if (allocated(Work_CellEntityIdUnique)) deallocate (Work_CellEntityIdUnique)
        if (allocated(Work_CellCounts)) deallocate (Work_CellCounts)
        if (allocated(Work_Elements)) deallocate (Work_Elements)

    end subroutine Set_Geometory_Infomation

    logical function Condition_BelongingGroup(num, Group)
        implicit none
        integer(int32), intent(in) :: num
        integer(int32), intent(in) :: Group

        if (num == Group) then
            Condition_BelongingGroup = .true.
        else
            Condition_BelongingGroup = .false.
        end if
    end function Condition_BelongingGroup

end module Main_Water

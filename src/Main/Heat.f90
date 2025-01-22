module Main_Heat
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    use :: Inout_Input
    use :: calculate_count, only:Count_if
    use :: allocate, only:Allocate_Vector
    use :: Calculate_Unique, only:Unique
    ! use :: Allocate_Structure, only:
    ! use :: Calculate_HCF

    implicit none
    private
    integer(int32), parameter :: Calc_Heat = 1

    public :: Heat

    type Heat
        type(Type_Geometry) :: Geometry
        type(Boudary_Condition) :: BC
        ! type(HCF) :: HCFs
        ! integer(int32)              :: Num_Elements, Num_Nodes, Num_Shape, Num_Dimention, Num_Shape_Type, Num_Region
        ! integer(int32), allocatable :: Element(:,:)
        ! integer(int32), allocatable :: Element_Region(:), COO_Region(:)
        ! type(DP2d)                  :: Nodes_2D
        ! real(real64),   allocatable :: Area(:)
        ! type(Shape)                 :: Shape_Function

    contains

    end type Heat

    interface Heat
        module procedure Heat_Constructor
    end interface

contains

    type(Heat) function Heat_Constructor(Structure_Input)
        type(Input), intent(in) :: Structure_Input

        call Set_Geometory_Infomation(Heat_Constructor, Structure_Input)
        call Set_Boundary_Condition_Infomations(Heat_Constructor, Structure_Input)

        ! block
        !     real(real64) :: array_HCF(9)
        !     array_HCF = [1, 2, 3, 4, 5, 6, 7, 8, 9]
        !     print *, array_HCF
        !     Heat_Constructor%HCFs = HCF(array_HCF, 5, 2)
        ! end block

    end function Heat_Constructor

    subroutine Set_Geometory_Infomation(self, Structure_Input)
        implicit none
        type(Heat), intent(inout) :: self
        type(Input), intent(in) :: Structure_Input
        procedure(condition_function), pointer :: condition_ptr => null()

        integer(int32), allocatable :: Work_CellEntityIds(:), Work_CellEntityIdUnique(:)
        integer(int32), allocatable :: Work_CellCounts(:)
        integer(int32) :: numCellTypes

        call Structure_Input%Get(self%Geometry%Basic)
        call Structure_Input%Get("CellEntityIds", Work_CellEntityIds)
        call Structure_Input%Get("numCellTypes", numCellTypes)
        ! print *, numCellTypes
        ! allocate (Work_CellEntityIdss, source=Work_CellEntityIds)

        ! print *, Work_CellEntityIds(:)
        call Unique(Work_CellEntityIds, Work_CellEntityIdUnique)
        print *, Work_CellEntityIdUnique(:)
        condition_ptr => Condition_BelongingGroup
        call Allocate_Vector(Work_CellCounts, numCellTypes)
        write (*, *) Count_if(Work_CellEntityIds, condition_ptr, 2)

        stop
    end subroutine Set_Geometory_Infomation

    ! subroutine Set_Boundary_Condition_Infomations(self, Structure_Input)
    !     use :: Calculate_Count, only:Count_if
    !     implicit none
    !     type(Heat), intent(inout) :: self
    !     type(Input), intent(in) :: Structure_Input

    !     integer(int32) :: Count_Heat
    !     integer(int32) :: iBC, Counter

    !     integer(int32), allocatable :: Work_Node(:), Work_Edge(:, :), Work_Type(:), Work_Value_Info(:, :)
    !     real(real64), allocatable :: Work_Value(:)

    !     Work_Node = Structure_Input%Input_Get_BC_Node()
    !     Work_Type = Structure_Input%Input_Get_BC_Node_Type()
    !     Work_Value_Info = Structure_Input%Input_Get_BC_Node_Value_Info()
    !     Work_Value = Structure_Input%Input_Get_BC_Node_Value(Calc_Heat)

    !     self%BC%Dirichlet%Num_Type = Count_if(Work_Value_Info(:, 2), Condition_Heat)
    !     call Allocate_Vector(self%BC%Dirichlet%Value_Info, self%BC%Dirichlet%Num_Type)
    !     call Allocate_Vector(self%BC%Dirichlet%value, self%BC%Dirichlet%Num_Type)

    !     Counter = 0
    !     do iBC = 1, size(Work_Value_Info(:, 2))
    !         if (Condition_Heat(Work_Value_Info(iBC, 2))) then
    !             Counter = Counter + 1
    !             self%BC%Dirichlet%Value_Info(Counter) = Work_Value_Info(iBC, 1)
    !             self%BC%Dirichlet%value(Counter) = Work_Value(iBC)
    !         end if
    !     end do
    !     self%BC%Dirichlet%Num_Node = Count_if(Work_Type(:), Condition_In_BoundaryCondition, self%BC%Dirichlet%Value_Info(:))

    !     call Allocate_Vector(self%BC%Dirichlet%Node, self%BC%Dirichlet%Num_Node)
    !     call Allocate_Vector(self%BC%Dirichlet%Node_Type, self%BC%Dirichlet%Num_Node)

    !     Counter = 0
    !     do iBC = 1, size(Work_Type)
    !         if (Condition_In_BoundaryCondition(Work_Type(iBC), self%BC%Dirichlet%Value_Info(:))) then
    !             Counter = Counter + 1
    !             self%BC%Dirichlet%Node(Counter) = Work_Node(iBC)
    !             self%BC%Dirichlet%Node_Type(Counter) = Work_Type(iBC)
    !         end if
    !     end do

    !     if (allocated(Work_Node)) deallocate (Work_Node)
    !     if (allocated(Work_Type)) deallocate (Work_Type)
    !     if (allocated(Work_Value_Info)) deallocate (Work_Value_Info)
    !     if (allocated(Work_Value)) deallocate (Work_Value)

    !     self%BC%Neumann%Edge = Structure_Input%Input_Get_BC_Edge()
    !     self%BC%Neumann%Edge_Type = Structure_Input%Input_Get_BC_Edge_Type()

    ! end subroutine Set_Boundary_Condition_Infomations

    ! logical function Condition_Heat(num)
    !     implicit none
    !     integer(int32), intent(in) :: num

    !     if (mod(num, 8) >= 4) then
    !         Condition_Heat = .true.
    !     else
    !         Condition_Heat = .false.
    !     end if
    ! end function Condition_Heat

    ! logical function Condition_In_BoundaryCondition(num, Value_Info)
    !     implicit none
    !     integer(int32), intent(in) :: num
    !     integer(int32), intent(in) :: Value_Info(:) ! 配列を引数として受け取る
    !     integer :: i
    !     logical :: found

    !     found = .false.
    !     do i = 1, size(Value_Info)
    !         if (num == Value_Info(i)) then
    !             found = .true.
    !             exit
    !         end if
    !     end do

    !     Condition_In_BoundaryCondition = found
    ! end function Condition_In_BoundaryCondition

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

end module Main_Heat

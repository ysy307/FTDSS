module Domain_Module
    use, intrinsic :: iso_fortran_env, only: int32
    use :: Domain_Element, only:ElementHolder
    use :: Domain_Side, only:SideHolder
    use :: Domain_Element_Factory, only:Create_Element
    use :: Domain_Side_Factory, only:Create_Side
    use :: Core_BaseTypes, only:DP3d
    use :: Inout_Input
    implicit none
    private
    public :: Domain_t

    type :: Domain_t
        ! private
        integer(int32) :: nElement
        integer(int32) :: nSide
        integer(int32) :: nNode
        integer(int32) :: nRegion
        type(ElementHolder), allocatable :: Elements(:)
        type(SideHolder), allocatable :: Sides(:)
        ! ...
    contains
        procedure :: initialize
    end type Domain_t

contains
    subroutine initialize(self, Input, Coordinate, ierr)
        class(Domain_t), intent(inout) :: self
        type(Type_Input), intent(in) :: Input ! Inputモジュールからデータを受け取る
        type(DP3d), intent(inout), pointer :: Coordinate
        integer, intent(out) :: ierr

        integer :: CountElements, CountSides
        integer :: iCell, iElem, iSide
        integer :: factory_ierr

        ierr = 0
        CountElements = 0
        CountSides = 0

        do iCell = 1, Input%VTK%numTotalCells
            if (Input%VTK%Is_In(Input%VTK%CELLS(iCell)%CellType, 1)) then
                CountSides = CountSides + 1
            end if
            if (Input%VTK%Is_In(Input%VTK%CELLS(iCell)%CellType, 2)) then
                CountElements = CountElements + 1
            end if
        end do

        self%nElement = CountElements
        self%nSide = CountSides
        self%nNode = Input%VTK%numPoints
        self%nRegion = Input%Basic%numRegion

        allocate (self%Elements(self%nElement))
        allocate (self%Sides(self%nSide))

        iElem = 1
        iSide = 1
        do iCell = 1, Input%VTK%numTotalCells
            if (Input%VTK%Is_In(Input%VTK%CELLS(iCell)%CellType, 1)) then
                call Create_Side( &
                    new_side=self%Sides(iSide)%s, &
                    shape_type=Input%VTK%CELLS(iCell)%CellType, &
                    ierr=factory_ierr, &
                    iSide=iSide, &
                    Global_Coordinate=Coordinate, &
                    Connectivity=Input%VTK%CELLS(iCell)%CONNECTIVITY, &
                    GroupID=Input%VTK%CELLS(iCell)%CellEntityId &
                    )
                if (factory_ierr /= 0) then
                    ierr = -1
                    return
                end if
                iSide = iSide + 1
            end if
            if (Input%VTK%Is_In(Input%VTK%CELLS(iCell)%CellType, 2)) then
                call create_element( &
                    new_element=self%Elements(iElem)%e, &
                    shape_type=Input%VTK%CELLS(iCell)%CellType, &
                    ierr=factory_ierr, &
                    iElem=iCell, &
                    Global_Coordinate=Coordinate, &
                    Connectivity=Input%VTK%CELLS(iCell)%CONNECTIVITY, &
                    GroupID=Input%VTK%CELLS(iCell)%CellEntityId &
                    )
                if (factory_ierr /= 0) then
                    ierr = -1
                    return
                end if
                iElem = iElem + 1
            end if
        end do
    end subroutine initialize

end module Domain_Module

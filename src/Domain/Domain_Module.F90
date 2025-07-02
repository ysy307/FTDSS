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
        integer(int32), private :: nElement
        integer(int32), private :: nSide
        integer(int32), private :: nNode
        integer(int32), private :: nRegion
        type(ElementHolder), allocatable :: Elements(:)
        type(SideHolder), allocatable :: Sides(:)
        ! ...
    contains
        procedure, pass(self) :: initialize

        procedure, pass(self) :: get_numElement
        procedure, pass(self) :: get_numSide
        procedure, pass(self) :: get_numNode
        procedure, pass(self) :: get_numRegion
    end type Domain_t

contains
    subroutine initialize(self, Input, Coordinate, ierr)
        implicit none
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

    function get_numElement(self) result(numElement)
        implicit none
        class(Domain_t), intent(in) :: self
        integer(int32) :: numElement

        numElement = self%nElement

    end function get_numElement

    function get_numSide(self) result(numSide)
        implicit none
        class(Domain_t), intent(in) :: self
        integer(int32) :: numSide

        numSide = self%nSide

    end function get_numSide

    function get_numNode(self) result(numNode)
        implicit none
        class(Domain_t), intent(in) :: self
        integer(int32) :: numNode

        numNode = self%nNode

    end function get_numNode

    function get_numRegion(self) result(numRegion)
        implicit none
        class(Domain_t), intent(in) :: self
        integer(int32) :: numRegion

        numRegion = self%nRegion

    end function get_numRegion

end module Domain_Module

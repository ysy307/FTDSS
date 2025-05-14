submodule(Core_Element) Core_Element_Base
    implicit none
    integer(int32), parameter :: SHAPE_TRIANGLE = 5
    integer(int32), parameter :: SHAPE_PIXEL = 8
    integer(int32), parameter :: SHAPE_QUAD = 9
    integer(int32), parameter :: SHAPE_QUADRATIC_TRIANGLE = 22
    integer(int32), parameter :: SHAPE_QUADRATIC_QUAD = 23
contains
    module subroutine ElementHolder_allocate(self, iShape_Type, iElem, Global_Coordinate, Connectivity, GroupID)
        implicit none
        class(ElementHolder), intent(inout) :: self
        integer(int32), intent(in) :: iShape_Type
        integer(int32), intent(in) :: iElem
        type(DP3d), pointer, intent(in) :: Global_Coordinate
        integer(int32), intent(in) :: Connectivity(:)
        integer(int32), intent(in) :: GroupID

        if (allocated(self%e)) deallocate (self%e)

        select case (iShape_Type)
        case (SHAPE_TRIANGLE)
            self%e = TriangleFirst(iElem, Global_Coordinate, Connectivity, GroupID)
        case (SHAPE_QUAD)
            self%e = SquareFirst(iElem, Global_Coordinate, Connectivity, GroupID)
        case (SHAPE_QUADRATIC_TRIANGLE)
            self%e = TriangleSecond(iElem, Global_Coordinate, Connectivity, GroupID)
        case (SHAPE_QUADRATIC_QUAD)
            self%e = SquareSecond(iElem, Global_Coordinate, Connectivity, GroupID)
        end select

    end subroutine ElementHolder_allocate

end submodule Core_Element_Base

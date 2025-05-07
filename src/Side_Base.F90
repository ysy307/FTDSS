submodule(Core_Side) Core_Side_Base
    implicit none
    integer(int32), parameter :: SHAPE_LINE = 3
    integer(int32), parameter :: SHAPE_QUADRATIC_EDGE = 21

contains
    module subroutine SideHolder_Allocate(self, iShape_Type, iSide, Global_Coordinate, Connectivity, GroupID)
        implicit none
        class(SideHolder), intent(inout) :: self
        integer(int32), intent(in) :: iShape_Type
        integer(int32), intent(in) :: iSide
        type(DP3d), pointer, intent(in) :: Global_Coordinate
        integer(int32), intent(in) :: Connectivity(:)
        integer(int32), intent(in) :: GroupID
        class(Abstract_SideType), allocatable :: Structure

        select case (iShape_Type)
        case (SHAPE_LINE)
            self%s = SideFirst(iSide, Global_Coordinate, Connectivity, GroupID)
        case (SHAPE_QUADRATIC_EDGE)
            self%s = SideSecond(iSide, Global_Coordinate, Connectivity, GroupID)
        end select

    end subroutine SideHolder_Allocate

end submodule Core_Side_Base

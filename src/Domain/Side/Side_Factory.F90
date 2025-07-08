module Domain_Side_Factory
    use, intrinsic :: iso_fortran_env, only: int32
    use :: Domain_Side, only:Abst_SideType, SideFirst, SideSecond
    use :: core_core, only:type_dp_3d
    implicit none
    integer(int32), parameter :: SHAPE_LINE = 3
    integer(int32), parameter :: SHAPE_QUADRATIC_EDGE = 21

    public :: Create_Side

contains
    ! 要素オブジェクトを生成して返す、独立したファクトリサブルーチン
    subroutine Create_Side(new_side, shape_type, ierr, iSide, Global_Coordinate, Connectivity, GroupID)
        class(Abst_SideType), allocatable, intent(inout) :: new_side
        integer(int32), intent(in) :: shape_type
        integer, intent(inout) :: ierr
        ! --- 各コンストラクタに渡す引数 ---
        integer(int32), intent(in) :: iSide
        type(type_dp_3d), pointer, intent(in) :: Global_Coordinate
        integer(int32), intent(in) :: Connectivity(:)
        integer(int32), intent(in) :: GroupID

        ierr = 0
        if (allocated(new_side)) deallocate (new_side)

        select case (shape_type)
        case (SHAPE_LINE)
            new_side = SideFirst(iSide, Global_Coordinate, Connectivity, GroupID)
        case (SHAPE_QUADRATIC_EDGE)
            new_side = SideSecond(iSide, Global_Coordinate, Connectivity, GroupID)
        case default
            write (*, '(a,i0)') "Error: Unknown element shape type = ", shape_type
            ierr = -1
        end select

    end subroutine Create_Side

end module Domain_Side_Factory

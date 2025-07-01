module Domain_Element_Factory
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Domain_Element, only:Abst_ElementType, & !&
                                TriangleFirst, SquareFirst, & !&
                                TriangleSecond, SquareSecond !&
    use :: Core_BaseTypes, only:DP3d
    implicit none
    private
    integer(int32), parameter :: SHAPE_TRIANGLE = 5
    integer(int32), parameter :: SHAPE_PIXEL = 8
    integer(int32), parameter :: SHAPE_QUAD = 9
    integer(int32), parameter :: SHAPE_QUADRATIC_TRIANGLE = 22
    integer(int32), parameter :: SHAPE_QUADRATIC_QUAD = 23

    public :: Create_Element
contains
    ! 要素オブジェクトを生成して返す、独立したファクトリサブルーチン
    subroutine create_element(new_element, shape_type, ierr, iElem, Global_Coordinate, Connectivity, GroupID)
        class(Abst_ElementType), allocatable, intent(inout) :: new_element
        integer(int32), intent(in) :: shape_type
        integer, intent(inout) :: ierr
        ! --- 各コンストラクタに渡す引数 ---
        integer(int32), intent(in) :: iElem
        type(DP3d), pointer, intent(in) :: Global_Coordinate
        integer(int32), intent(in) :: Connectivity(:)
        integer(int32), intent(in) :: GroupID

        ierr = 0
        if (allocated(new_element)) deallocate (new_element)

        select case (shape_type)
        case (SHAPE_TRIANGLE)
            new_element = TriangleFirst(iElem, Global_Coordinate, Connectivity, GroupID)
        case (SHAPE_QUAD)
            new_element = SquareFirst(iElem, Global_Coordinate, Connectivity, GroupID)
        case (SHAPE_QUADRATIC_TRIANGLE)
            new_element = TriangleSecond(iElem, Global_Coordinate, Connectivity, GroupID)
        case (SHAPE_QUADRATIC_QUAD)
            new_element = SquareSecond(iElem, Global_Coordinate, Connectivity, GroupID)
        case default
            write (*, '(a,i0)') "Error: Unknown element shape type = ", shape_type
            ierr = -1
        end select

    end subroutine Create_Element

end module Domain_Element_Factory

module Calculate_Area
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    use :: error
    use :: Calculate_Points, only:Set_Point
    implicit none

    interface Calc_Area
        procedure :: Calc_Area_31
    end interface

contains

    subroutine Calc_Area_31(Geometry)
        implicit none
        type(Type_Geometry), intent(inout) :: Geometry
        type(Vector2d) :: p1, p2, p3
        integer(int32) :: elem

        do elem = 1, Geometry%Basic%Element
            call set_point(Geometry, elem, p1, p2, p3)
            if (elem < 1 .or. elem > Geometry%Basic%Element) then
                call error_message(932)
            else
                Geometry%Area(elem) = abs((p2%x - p1%x) * (p3%y - p1%y) - (p2%y - p1%y) * (p3%x - p1%x)) / 2.d0
            end if
        end do

    end subroutine Calc_Area_31
end module Calculate_Area

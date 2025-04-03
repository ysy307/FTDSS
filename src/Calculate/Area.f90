module Calculate_Area
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    implicit none
    private

    public :: Update_Area

    interface Update_Area
        procedure :: Update_Area_3
    end interface

contains

    subroutine Update_Area_3(Elements, Coordinate, Area)
        implicit none
        integer(int32), intent(in) :: Elements(:, :)
        type(DP3d), intent(in) :: Coordinate
        real(real64), intent(inout) :: Area(:)
        type(Vector2d) :: p1, p2, p3
        integer(int32) :: iE

        do iE = 1, size(Elements, 2)
            p1%x = Coordinate%x(Elements(1, iE))
            p1%y = Coordinate%y(Elements(1, iE))
            p2%x = Coordinate%x(Elements(2, iE))
            p2%y = Coordinate%y(Elements(2, iE))
            p3%x = Coordinate%x(Elements(3, iE))
            p3%y = Coordinate%y(Elements(3, iE))

            Area(iE) = abs((p2%x - p1%x) * (p3%y - p1%y) - (p2%y - p1%y) * (p3%x - p1%x)) / 2.d0
        end do

    end subroutine Update_Area_3
end module Calculate_Area

module Calculate_Area
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    implicit none

    interface Calc_Area
        procedure :: Calc_Area_3
    end interface

contains

    subroutine Calc_Area_3(Elements, Nodes, Area)
        implicit none
        real(real64), intent(in) :: Elements(:, :)
        type(DP3d), intent(in) :: Nodes
        real(real64), intent(inout) :: Area(:)
        type(Vector2d) :: p1, p2, p3
        integer(int32) :: elem

        do elem = 1, size(Elements, 2)
            p1%x = Nodes%x(Elements(1, elem))
            p1%y = Nodes%y(Elements(1, elem))
            p2%x = Nodes%x(Elements(2, elem))
            p2%y = Nodes%y(Elements(2, elem))
            p3%x = Nodes%x(Elements(3, elem))
            p3%y = Nodes%y(Elements(3, elem))

            Area(elem) = abs((p2%x - p1%x) * (p3%y - p1%y) - (p2%y - p1%y) * (p3%x - p1%x)) / 2.d0
        end do

    end subroutine Calc_Area_3
end module Calculate_Area

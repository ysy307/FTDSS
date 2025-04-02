module Calculate_Shape
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    implicit none

    interface Calculate_Basis
        procedure :: Calculate_Basis_31
    end interface

contains

    subroutine Calculate_Basis_31(Elements, Coordinates, Basis)
        implicit none
        ! type(Type_Geometry), intent(inout) :: Geometry?
        real(real64), intent(in) :: Elements(:, :)
        type(DP3d), intent(in) :: Coordinates
        type(Shape), intent(inout) :: Basis
        type(Vector2d) :: p1, p2, p3
        integer(int32) :: iE

        do iE = 1, size(Elements, 2)
            p1%x = Coordinates%x(Elements(1, iE))
            p1%y = Coordinates%y(Elements(1, iE))
            p2%x = Coordinates%x(Elements(2, iE))
            p2%y = Coordinates%y(Elements(2, iE))
            p3%x = Coordinates%x(Elements(3, iE))
            p3%y = Coordinates%y(Elements(3, iE))

            Basis%a(1, iE) = p2%x * p3%y - p3%x * p2%y
            Basis%a(2, iE) = p3%x * p1%y - p1%x * p3%y
            Basis%a(3, iE) = p1%x * p2%y - p2%x * p1%y
            Basis%b(1, iE) = p2%y - p3%y
            Basis%b(2, iE) = p3%y - p1%y
            Basis%b(3, iE) = p1%y - p2%y
            Basis%c(1, iE) = p3%x - p2%x
            Basis%c(2, iE) = p1%x - p3%x
            Basis%c(3, iE) = p2%x - p1%x
        end do
    end subroutine Calculate_Basis_31
end module Calculate_Shape

module Calculate_Shape
    use, intrinsic :: iso_fortran_env, only : int32, real64
    use :: Types
    use :: error
    use :: Calculate_Points, only : Set_Point
    implicit none

    interface Calc_Shape
		procedure :: Calc_Shape_31
	end interface

    contains

    subroutine Calc_Shape_31(Geometry)
		implicit none
        type(Geometry2d), intent(inout) :: Geometry
		type(Vector2d)                  :: p1, p2, p3
		integer(int32)                  :: iE

		do iE = 1, Geometry%Element
			call set_point(Geometry, iE, p1, p2, p3)
			if (iE < 1 .or. iE > Geometry%Element) then
				call error_message(932)
			else
				Geometry%Basis%a(1,iE) = p2%x * p3%y - p3%x * p2%y
				Geometry%Basis%a(2,iE) = p3%x * p1%y - p1%x * p3%y
				Geometry%Basis%a(3,iE) = p1%x * p2%y - p2%x * p1%y
				Geometry%Basis%b(1,iE) = p2%y - p3%y
				Geometry%Basis%b(2,iE) = p3%y - p1%y
				Geometry%Basis%b(3,iE) = p1%y - p2%y
				Geometry%Basis%c(1,iE) = p3%x - p2%x
				Geometry%Basis%c(2,iE) = p1%x - p3%x
				Geometry%Basis%c(3,iE) = p2%x - p1%x
			end if
		end do
	end subroutine Calc_Shape_31
end module Calculate_Shape
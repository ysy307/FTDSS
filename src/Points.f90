module Calculate_Points
    use, intrinsic :: iso_fortran_env, only : int32, real64
    use :: Types
    use :: error
    implicit none
    private

    interface Set_Point
        procedure :: Set_Point_31
    end interface

    public :: Set_Point

    contains

    subroutine Set_Point_31(Geometry, triangle_index, p1, p2, p3)
		implicit none
		type(Geometry2d), intent(in)    :: Geometry
        integer(int32),   intent(in)    :: triangle_index
		type(Vector2d),   intent(inout) :: p1, p2, p3


		p1%x = Geometry%vCood%x(Geometry%pElement(1, triangle_index))
		p2%x = Geometry%vCood%x(Geometry%pElement(2, triangle_index))
		p3%x = Geometry%vCood%x(Geometry%pElement(3, triangle_index))
		p1%y = Geometry%vCood%y(Geometry%pElement(1, triangle_index))
		p2%y = Geometry%vCood%y(Geometry%pElement(2, triangle_index))
		p3%y = Geometry%vCood%y(Geometry%pElement(3, triangle_index))

		if (p1%x == p2%x .and. p1%y == p2%y .or. &
		  & p1%x == p3%x .and. p1%y == p3%y .or. &
		  & p2%x == p3%x .and. p2%y == p3%y) then
			call error_message(928)
		end if

	end subroutine Set_Point_31

end module Calculate_Points
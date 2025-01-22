module Calculate_Observation
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    use :: allocate
    use :: Calculate_Points, only:Set_Point
    implicit none
    private

    interface Set_Obs_COO
        procedure :: Set_Obs_COO_31
    end interface

    public :: Set_Obs_COO

contains

    subroutine Set_Obs_COO_31(Solver)
        implicit none
        type(SolverInfo), intent(inout) :: Solver
        type(Vector2d) :: p1, p2, p3
        integer(int32) :: iE, iObs, iS
        real(real64) :: x, y, s

        call Allocate_Vector(Solver%Obs%nAreaObs, Solver%Obs%nObs)
        call Allocate_Matrix(Solver%Obs%vAreaObs, Solver%N%ShCoe, Solver%Obs%nObs)

        ! do iObs = 1, Solver%Obs%nObs
        !         x = Solver%Obs%obsCOO%x(iObs)
        !         y = Solver%Obs%obsCOO%y(iObs)
        !         do iE = 1, Solver%N%Element
        !                 call Set_Point(Solver%N, iE, p1, p2, p3)
        !                 if (is_in_triangle(x, y, p1, p2, p3)) then
        !                         s = Solver%N%eArea(iE)
        !                         do iS = 1, Solver%N%ShCoe
        !                                 Solver%Obs%vAreaObs(iS, iObs) = (Solver%N%Basis%a(iS, iE) + Solver%N%Basis%b(iS, iE) * x &
        !                                             & +  Solver%N%Basis%c(iS, iE) * y) / (2.0d0 * s)
        !                         end do
        !                         Solver%Obs%nAreaObs(iObs)         = iE
        !                         exit
        !                 end if
        !         end do
        ! end do
    end subroutine Set_Obs_COO_31

    logical function is_in_triangle(x, y, p1, p2, p3)
        implicit none
        type(Vector2d), intent(in) :: p1, p2, p3
        real(real64), intent(in) :: x, y

        is_in_triangle = .false.
        if (0.0d0 <= (p2%y - p3%y) * (x - p3%x) + (p3%x - p2%x) * (y - p3%y) .and. &
          & 0.0d0 <= (p3%y - p1%y) * (x - p1%x) + (p1%x - p3%x) * (y - p1%y) .and. &
          & 0.0d0 <= (p1%y - p2%y) * (x - p2%x) + (p2%x - p1%x) * (y - p2%y)) then
            is_in_triangle = .true.
        end if
    end function is_in_triangle

end module Calculate_Observation

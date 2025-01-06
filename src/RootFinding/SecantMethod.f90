module RootFinding_SecantMethod
    use, intrinsic :: iso_fortran_env, only: real64, int32
    use, intrinsic :: IEEE_ARITHMETIC, only : ieee_is_nan
	use :: Types
	use :: Calculate_LatentHeat, only : Calc_LatentHeatTerm
    implicit none
    private
    real(real64),   parameter :: eps      = 1.0d-10
    integer(int32), parameter :: max_iter = 100

    public :: Secant_method

    contains

    subroutine Secant_method(Solver, ind)
		implicit none
		type(SolverInfo), intent(inout) :: Solver
		integer(int32), intent(in) :: ind

		real(real64) :: Tnew
		real(real64) :: x0, x1, x2, f0, f1, err
		integer(int32) :: i

		x0   = 0.0d0
		x1   = Solver%T%new(ind)
		Tnew = Solver%T%new(ind)
		do i = 1, max_iter
			f0 = Calc_LatentHeatTerm(x0, Tnew, Solver%Heat%Latent)
			f1 = Calc_LatentHeatTerm(x1, Tnew, Solver%Heat%Latent)
			if (abs(f1 - f0) < epsilon(1.0d0)) then
				stop
			else
				err = f1 * (x1 - x0) / (f1 - f0)
				x2 = x1 - err
				if (abs(err) < eps) exit
				x0 = x1
				x1 = x2
			end if
		end do
		if (i >= max_iter) then
			if (Solver%Flags%isGCC) then
				write(*,"(a)"), "Secant_method_GCC: iteration limit exceeded"
			else if (Solver%Flags%isPower) then
				write(*,"(a)"), "Secant_method_Power: iteration limit exceeded"
			end if
			stop
		end if
		if (ieee_is_nan(x2)) x2 = 0.0d0

		Solver%T%new(ind) = x2
	end subroutine Secant_method
end module RootFinding_SecantMethod
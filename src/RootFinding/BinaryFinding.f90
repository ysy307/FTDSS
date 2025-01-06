module RootFinding_BinaryFinding
    use, intrinsic :: iso_fortran_env, only: real64, int32
    use, intrinsic :: IEEE_ARITHMETIC, only : ieee_is_nan
	use :: Types
	use :: Calculate_LatentHeat, only : Calc_LatentHeatTerm
    implicit none
    private
    real(real64),   parameter :: eps      = 1.0d-10
    integer(int32), parameter :: max_iter = 100

    public :: Binary_finding

    contains

    subroutine Binary_finding(Solver, ind)
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
			if (f0 * f1 > 0.0d0) then
				write(*,"(a)"), "Binary_finding_Power: f0 * f1 > 0"
				stop
			end if
			x2 = (x0 + x1) / 2.0d0
			if (f0 * Calc_LatentHeatTerm(x2, Tnew, Solver%Heat%Latent) < 0.0d0) then
				x1 = x2
			else
				x0 = x2
			end if
			if (abs(x1 - x0) < eps) exit
		end do
		if (i >= max_iter) then
			if (Solver%Flags%isGCC) then
				write(*,"(a)"), "Binary_finding_GCC: iteration limit exceeded"
			else if (Solver%Flags%isPower) then
				write(*,"(a)"), "Binary_finding_Power: iteration limit exceeded"
			end if
			stop
		end if

		Solver%T%new(ind) = x2
	end subroutine Binary_finding
end module RootFinding_BinaryFinding
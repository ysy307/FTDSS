module error
	use, intrinsic :: iso_fortran_env, only : int32, real64
	use, intrinsic :: IEEE_ARITHMETIC, only : ieee_is_nan
	#ifdef _MPI
		use mpi
	#endif
	implicit none
	private

	public :: error_message
	public :: has_nan
	public :: value_in_range

    interface has_nan
		procedure :: has_nan_real64_scalar
		procedure :: has_nan_real64_rank1
		procedure :: has_nan_real64_rank2
	end interface

	interface value_in_range
		procedure :: value_in_range_int32
		procedure :: value_in_range_real64
	end interface

    contains

    !* Check whether the array has NaN or not
	function has_nan_real64_scalar(scalar) result(has_nan)
		implicit none
		real(real64), intent(in) :: scalar
		logical :: has_nan

		has_nan = ieee_is_nan(scalar)
	end function has_nan_real64_scalar

	function has_nan_real64_rank1(array) result(has_nan)
		implicit none
		real(real64), intent(in) :: array(:)
		logical :: has_nan

		has_nan = any(ieee_is_nan(array))
	end function has_nan_real64_rank1

	function has_nan_real64_rank2(array) result(has_nan)
		implicit none
		real(real64), intent(in) :: array(:, :)
		logical :: has_nan

		has_nan = any(ieee_is_nan(array))
	end function has_nan_real64_rank2

    subroutine error_message(err_number, myrank, opt, opt_file_name, copt1, copt2)
		implicit none
		integer(int32), intent(in) :: err_number
		real(real64), optional, intent(in)  :: opt
		integer(int32), optional, intent(in)  :: myrank
		character(256) :: msg
		character(*), optional, intent(in) :: opt_file_name, copt1, copt2
		integer(int32) :: ierr

		if (err_number == 901) then
			write(msg,'(3a)') "Error: Does not exit file '", trim(adjustl(opt_file_name)), "'."
		else if (err_number == 902) then
			write(msg,'(3a)') "Error: Can not open file '", trim(adjustl(opt_file_name)), "'."
		else if (err_number == 903) then
			write(msg,'(3a)') "Error: Selected ", trim(adjustl(copt1)), " number is invalid."
		else if (err_number == 904) then
		msg = "Error: Opening file 'coordinate.in'"
		else if (err_number == 905) then
		msg = "Error: Opening file 'top.in'"
		else if (err_number == 906) then
		msg = "Error: Opening file 'coordinate.in'"
		else if (err_number == 911) then
		msg = "Error: The number of elements must be positive."
		else if (err_number == 912) then
		msg = "Error: The number of nodal must be positive."
		else if (err_number == 913) then
		msg = "Error: The number of shape must be positive."
		else if (err_number == 914) then
		msg = "Error: The number of dimention must be positive."
		else if (err_number == 915) then
		msg = "Error: Dirichlet boundary conditions for water transport must be positive."
		else if (err_number == 916) then
		msg = "Error: Dirichlet boundary conditions for heat transport must be positive."
		else if (err_number == 917) then
		msg = "Error: The value of porosity must be positive."
		else if (err_number == 918) then
		msg = "Error: The value of density must be positive."
		else if (err_number == 919) then
		msg = "Error: The value of hydrulic conductivity must be positive."
		else if (err_number == 920) then
		msg = "Error: The value of thermal conductivity must be positive."
		else if (err_number == 921) then
		msg = "Error: The value of specific heat must be positive."
		else if (err_number == 922) then
		msg = "Error: The value of latent heat must be positive."
		else if (err_number == 923) then
		msg = "Error: The number of concering time information must be positive."
		else if (err_number == 924) then
		msg = "Error: The initial and coolant temperature are same."
		else if (err_number == 928) then
		msg = "Error: Two or more points are the same."
		else if (err_number == 929) then
		msg = "Error: The number of array elements must be positive."
		else if (err_number == 930) then
		msg = "Error: The number of matrix elements must be positive."
		else if (err_number == 931) then
		msg = "Error: opening output file"
		else if (err_number == 932) then
		msg = "Error: Invalid element index"
		else if (err_number == 933) then
		msg = "Error: Solver type is not selected."
		else if (err_number == 934) then
		msg = "Error: Freezing calculation is not selected."
		else if (err_number == 941) then
		msg = "Error: The solution to the simultaneous linear equations could not be found."
		else if (err_number == 942) then
		msg ="Error: LU decomposition could not be successed."
		else if (err_number == 943) then
		msg ="Error: The inverse matrix could not be found."
		else if (err_number == 944) then
		msg ="Error: The target result is too high."
		else if (err_number == 945) then
		msg ="Error: The target result is too low."
		else if (err_number == 946) then
		msg ="Error: The solution has been diverged."
		else if (err_number == 951) then
			write(msg, '(a)') "Error: The number of vector elements must be positive."
		else if (err_number == 952) then
			write(msg, '(a)') "Error: The number of matrix elements must be positive."
		else if (err_number == 953) then
			write(msg, '(a)') "Error: Vector has already allocated."
		else if (err_number == 954) then
			write(msg, '(a)') "Error: Matrix has already allocated."
		else if (err_number == 955) then
			write(msg, '(a)') "Error: Pointer has already allocated."
		else
		msg = "Error: Unknown error"
		end if

		#ifdef _MPI
			if (myrank == 0) then
				call MPI_Finalize(ierr)
		#endif

		write(*,'(a)') msg
		stop
		
		#ifdef _MPI
			end if
		#endif
	end subroutine error_message

	function value_in_range_int32(value, min, max) result(in_range)
		implicit none
		integer(int32),   intent(in) :: value, min, max
		logical :: in_range

		in_range = .true.
		if (value < min .or. value > max) then
			in_range = .false.
		end if
	end function value_in_range_int32

	function value_in_range_real64(value, min, max) result(in_range)
		implicit none
		real(real64), intent(in) :: value, min, max
		logical :: in_range

		in_range = .true.
		if (value < min .or. value > max) then
			in_range = .false.
		end if
	end function value_in_range_real64
end module error
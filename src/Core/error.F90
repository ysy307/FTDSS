module core_error
    use, intrinsic :: iso_fortran_env, only: int32, real32, real64, real128
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    use :: stdlib_logger
    implicit none
    private

    public :: error_message

contains

    subroutine error_message(err_number, myrank, opt, opt_file_name, copt1, copt2)
        implicit none
        integer(int32), intent(in) :: err_number
        real(real64), optional, intent(in) :: opt
        integer(int32), optional, intent(in) :: myrank
        character(256) :: msg
        character(*), optional, intent(in) :: opt_file_name, copt1, copt2
        integer(int32) :: ierr
        character(8) :: fmt = '(a,i3,a)'

        if (err_number == 901) then
            write (msg, '(3a)') "Does not exit file '", trim(adjustl(opt_file_name)), "'."
        else if (err_number == 902) then
            write (msg, '(3a)') "Can not open file '", trim(adjustl(opt_file_name)), "'."
        else if (err_number == 903) then
            write (msg, '(3a)') "Selected ", trim(adjustl(copt1)), " number is invalid."
        else if (err_number == 904) then
            msg = "Opening file 'coordinate.in'"
        else if (err_number == 905) then
            msg = "Opening file 'top.in'"
        else if (err_number == 906) then
            msg = "Opening file 'coordinate.in'"
        else if (err_number == 911) then
            msg = "The number of elements must be positive."
        else if (err_number == 912) then
            msg = "The number of nodal must be positive."
        else if (err_number == 913) then
            msg = "The number of shape must be positive."
        else if (err_number == 914) then
            msg = "The number of dimention must be positive."
        else if (err_number == 915) then
            msg = "Dirichlet boundary conditions for water transport must be positive."
        else if (err_number == 916) then
            msg = "Dirichlet boundary conditions for heat transport must be positive."
        else if (err_number == 917) then
            msg = "The value of porosity must be positive."
        else if (err_number == 918) then
            msg = "The value of density must be positive."
        else if (err_number == 919) then
            msg = "The value of hydrulic conductivity must be positive."
        else if (err_number == 920) then
            msg = "The value of thermal conductivity must be positive."
        else if (err_number == 921) then
            msg = "The value of specific heat must be positive."
        else if (err_number == 922) then
            msg = "The value of latent heat must be positive."
        else if (err_number == 923) then
            msg = "The number of concering time information must be positive."
        else if (err_number == 924) then
            msg = "The initial and coolant temperature are same."
        else if (err_number == 928) then
            msg = "Two or more points are the same."
        else if (err_number == 929) then
            msg = "The number of array elements must be positive."
        else if (err_number == 930) then
            msg = "The number of matrix elements must be positive."
        else if (err_number == 931) then
            msg = "opening output file"
        else if (err_number == 932) then
            msg = "Invalid element index"
        else if (err_number == 933) then
            msg = "Solver type is not selected."
        else if (err_number == 934) then
            msg = "Freezing calculation is not selected."
        else if (err_number == 941) then
            msg = "The solution to the simultaneous linear equations could not be found."
        else if (err_number == 942) then
            msg = "LU decomposition could not be successed."
        else if (err_number == 943) then
            msg = "The inverse matrix could not be found."
        else if (err_number == 944) then
            msg = "The target result is too high."
        else if (err_number == 945) then
            msg = "The target result is too low."
        else if (err_number == 946) then
            msg = "The solution has been diverged."
        else if (err_number == 951) then
            write (msg, fmt) "#", err_number, ": Array is already allocated."
        else if (err_number == 952) then
            write (msg, fmt) "#", err_number, ": Invalid array length."
        else if (err_number == 953) then
            write (msg, fmt) "#", err_number, ": Requested index range is too large."
        else if (err_number == 954) then
            write (msg, fmt) "#", err_number, ": Invalid range - first index is greater than last."
        else if (err_number == 955) then
            write (msg, fmt) "#", err_number, ": Memory allocation failed."
        else if (err_number == 956) then
            write (msg, fmt) "#", err_number, ": Cannot specify both length and bounds."
        else if (err_number == 957) then
            write (msg, fmt) "#", err_number, ": Either length or bounds must be specified."
        else if (err_number == 958) then
            write (msg, fmt) "#", err_number, ": bounds array must have exactly 2 elements."
        else if (err_number == 961) then
            write (msg, '(a)') "Pointer has already allocated."
        else if (err_number == 971) then
            write (msg, fmt) "#", err_number, ": Memory deallocation failed."
        else
            msg = "Unknown error"
        end if

#ifdef _MPI
        if (myrank == 0) then
            call MPI_Finalize(ierr)
#endif

            call global_logger%log_error(message=msg)
            stop

#ifdef _MPI
        end if
#endif
    end subroutine error_message

end module core_error

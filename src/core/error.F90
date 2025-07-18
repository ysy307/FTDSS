module core_error
    use, intrinsic :: iso_fortran_env, only: int32, real32, real64, real128
    use :: stdlib_logger
    implicit none
    private

    public :: error_message

contains

    subroutine error_message(err_number, myrank, opt, c_opt)
        implicit none
        integer(int32), intent(in) :: err_number
        integer(int32), optional, intent(in) :: myrank
        real(real64), optional, intent(in) :: opt
        character(*), optional, intent(in) :: c_opt

        character(256) :: msg
        integer(int32) :: ierr
        character(8) :: fmt = '(a,i3,a)'

        if (err_number == 901) then
            write (msg, '(a)') "Don't exist Input folder."
        else if (err_number == 902) then
            if (present(c_opt)) then
                write (msg, '(3a)') "Don't exist file '", trim(adjustl(c_opt)), "'."
            else
                write (msg, '(a)') "Don't exist file."
            end if
        else if (err_number == 903) then
            if (present(c_opt)) then
                write (msg, '(3a)') "Can't open file '", trim(adjustl(c_opt)), "'."
            else
                write (msg, '(a)') "Can't open file."
            end if
        else if (err_number == 904) then
            if (present(c_opt)) then
                write (msg, '(3a)') "Selected variable '", trim(adjustl(c_opt)), "' is not found in JSON file."
            else
                write (msg, '(a)') "Selected variable is not found in JSON file."
            end if
        else if (err_number == 905) then
            if (present(c_opt)) then
                write (msg, '(3a)') "Selected variable '", trim(adjustl(c_opt)), "' is invalid."
            else
                write (msg, '(a)') "Selected variable is invalid."
            end if
        else if (err_number == 906) then
            if (present(c_opt)) then
                write (msg, '(3a)') "Selected file '", trim(adjustl(c_opt)), "' is invalid"
            else
                write (msg, '(a)') "Selected file is invalid"
            end if
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

            ! reordering error message
        else if (err_number == 931) then
            if (present(c_opt)) then
                write (msg, fmt) "#", err_number, ": Could not find a starting node in "//trim(adjustl(c_opt))//"."
            else
                write (msg, fmt) "#", err_number, ": Could not find a starting node."
            end if
        else if (err_number == 932) then
            if (present(c_opt)) then
                write (msg, fmt) "#", err_number, ": Permutation is not ready in"//trim(adjustl(c_opt))//". Call reordering first."
            else
                write (msg, fmt) "#", err_number, ": Permutation is not ready. Call reordering first."
            end if

            ! solver error message
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

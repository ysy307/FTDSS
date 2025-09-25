!>
!> Provides a routine to read environment variables in an MPI-safe way.
!>
module core_system_env
    use :: mpi_f08
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    private
    public :: get_env_string

contains

    !>
    !> Gets the value of a specified environment variable as a string.
    !> In an MPI environment, rank 0 reads the variable, which is then broadcast
    !> to all other ranks to ensure consistency.
    !>
    subroutine get_env_string(env_var_name, value)
        implicit none
        !> The name of the environment variable to retrieve.
        character(len=*), intent(in) :: env_var_name
        !> The retrieved value of the environment variable. An empty string is
        !> returned if the variable is not set or is empty.
        character(len=:), allocatable, intent(inout) :: value

        character(len=2048) :: buffer
        integer(int32) :: nulpos
        integer(int32) :: status
        integer(int32) :: my_rank, ierr

        call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)

        ! Rank 0 reads the environment variable
        if (my_rank == 0) then
            call get_environment_variable(env_var_name, buffer, status=status)
            if (status /= 0) buffer = ''
        end if

        ! Broadcast the result to all ranks
        call MPI_Bcast(buffer, len(buffer), MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)

        ! Trim the result and allocate the output string to the correct length
        if (len_trim(buffer) > 0) then
            nulpos = scan(buffer, achar(0))
            if (nulpos > 0) then
                allocate (character(len=nulpos - 1) :: value)
                value = buffer(1:nulpos - 1)
            else
                allocate (character(len=len_trim(buffer)) :: value)
                value = buffer(1:len_trim(buffer))
            end if
        else
            value = ''
        end if

    end subroutine get_env_string

end module core_system_env

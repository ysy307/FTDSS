module core_system_env
#ifdef _MPI
    use :: mpi_f08
#endif
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    private
    public :: get_env_string

contains

    subroutine get_env_string(env_var_name, value, status)
        implicit none
        character(len=*), intent(in) :: env_var_name
        character(len=:), allocatable, intent(inout) :: value
        integer(int32), intent(inout), optional :: status

        character(len=2048) :: buffer
        integer(int32) :: nulpos
        integer(int32) :: stat
#ifdef _MPI
        integer(int32) :: my_rank, ierr

        call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
#endif
        if (allocated(value)) deallocate (value)

        ! Initialize variables
        buffer = ''
        stat = 0

        ! Rank 0 reads the environment variable
#ifdef _MPI
        if (my_rank == 0) then
#endif
            call get_environment_variable(env_var_name, buffer, status=stat)
            if (stat /= 0) buffer = ''
#ifdef _MPI
        end if

        ! Broadcast status and buffer to all ranks
        call MPI_Bcast(stat, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
        call MPI_Bcast(buffer, len(buffer), MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)
#endif

        ! Set optional status uniformly across all ranks
        if (present(status)) status = stat

        ! Trim the result and allocate the output string
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

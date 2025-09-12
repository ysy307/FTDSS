module inout_project_settings
#ifdef _MPI
    use mpi_f08
#endif
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    private

    character(len=:), allocatable :: ProjectPath
    logical :: is_initialize_project_path = .false.

    public :: get_project_path

contains

    subroutine inout_project_path_initialize()
        implicit none
        character(len=*), parameter :: ENV_VAR_NAME = "FTDSS_PROJECT_PATH"
        character(len=2048) :: path_buffer
        integer(int32) :: stat, i, null_pos

#ifdef _MPI
        integer(int32) :: my_rank, ierr
#endif

#ifdef _MPI
        call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
        if (my_rank == 0) then
            call get_environment_variable(ENV_VAR_NAME, path_buffer, status=stat)
            if (stat /= 0) path_buffer = ''
        end if
        call MPI_Bcast(path_buffer, len(path_buffer), MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)
        if (len_trim(path_buffer) == 0) then
            if (my_rank == 0) print *, "FATAL ERROR: Could not get project path. Check if '", trim(ENV_VAR_NAME), "' is set."
            call MPI_Abort(MPI_COMM_WORLD, 1, ierr)
        end if
#else
        call get_environment_variable(ENV_VAR_NAME, path_buffer, status=stat)
        if (stat /= 0) then
            print *, "FATAL ERROR: Environment variable '", trim(ENV_VAR_NAME), "' is not set."
            call exit(1)
        end if
#endif

        ProjectPath = trim(path_buffer)

        ! Delete any trailing null characters
        null_pos = index(ProjectPath, char(0))
        if (null_pos > 0) then
            ProjectPath = ProjectPath(:null_pos - 1)
        end if

        ! Replace backslashes with forward slashes for cross-platform compatibility
        do i = 1, len(ProjectPath)
            if (ProjectPath(i:i) == '\') then
                ProjectPath(i:i) = '/'
            end if
        end do

        ! Ensure the path ends with a slash
        if (len_trim(ProjectPath) > 0 .and. ProjectPath(len_trim(ProjectPath):len_trim(ProjectPath)) /= "/") then
            ProjectPath = trim(ProjectPath)//"/"
        end if

        is_initialize_project_path = .true.

    end subroutine inout_project_path_initialize

    function get_project_path() result(res)
        implicit none
        character(len=:), allocatable :: res

        if (.not. is_initialize_project_path) call inout_project_path_initialize

        res = ProjectPath

    end function get_project_path

end module inout_project_settings

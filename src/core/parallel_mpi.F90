!> Provides the MPI API used by FTCMS or a single-process implementation.
module core_parallel_mpi
    use, intrinsic :: iso_fortran_env, only: error_unit, int32
#ifdef _MPI
    use :: mpi_f08
#endif

    implicit none

#ifdef _MPI
    public
#else
    private

    integer(int32), parameter, public :: MPI_SUCCESS = 0
    integer(int32), parameter, public :: MPI_CHARACTER = 1
    integer(int32), parameter, public :: MPI_INTEGER = 2
    integer(int32), parameter, public :: MPI_INTEGER4 = 3

    integer(int32), parameter, public :: MPI_MAX = 1
    integer(int32), parameter, public :: MPI_SUM = 2

    type, public :: MPI_Comm
        integer(int32) :: value = 0
    end type MPI_Comm

    type(MPI_Comm), parameter, public :: MPI_COMM_WORLD = MPI_Comm(1)

    public :: MPI_Abort
    public :: MPI_Allreduce
    public :: MPI_Bcast
    public :: MPI_Comm_rank
    public :: MPI_Comm_size

    interface MPI_Allreduce
        module procedure mpi_allreduce_int32
    end interface MPI_Allreduce

    interface MPI_Bcast
        module procedure mpi_bcast_character
        module procedure mpi_bcast_int32
    end interface MPI_Bcast

contains

    subroutine MPI_Comm_rank(comm, rank, ierror)
        type(MPI_Comm), intent(in) :: comm
        integer(int32), intent(inout) :: rank
        integer(int32), intent(inout) :: ierror

        rank = 0
        ierror = MPI_SUCCESS
    end subroutine MPI_Comm_rank

    subroutine MPI_Comm_size(comm, size, ierror)
        type(MPI_Comm), intent(in) :: comm
        integer(int32), intent(inout) :: size
        integer(int32), intent(inout) :: ierror

        size = 1
        ierror = MPI_SUCCESS
    end subroutine MPI_Comm_size

    subroutine mpi_bcast_int32(buffer, count, datatype, root, comm, ierror)
        integer(int32), intent(inout) :: buffer
        integer(int32), intent(in) :: count
        integer(int32), intent(in) :: datatype
        integer(int32), intent(in) :: root
        type(MPI_Comm), intent(in) :: comm
        integer(int32), intent(inout) :: ierror

        ierror = MPI_SUCCESS
    end subroutine mpi_bcast_int32

    subroutine mpi_bcast_character(buffer, count, datatype, root, comm, ierror)
        character(*), intent(inout) :: buffer
        integer(int32), intent(in) :: count
        integer(int32), intent(in) :: datatype
        integer(int32), intent(in) :: root
        type(MPI_Comm), intent(in) :: comm
        integer(int32), intent(inout) :: ierror

        ierror = MPI_SUCCESS
    end subroutine mpi_bcast_character

    subroutine mpi_allreduce_int32(send_buffer, receive_buffer, count, datatype, operation, comm, ierror)
        integer(int32), intent(in) :: send_buffer
        integer(int32), intent(inout) :: receive_buffer
        integer(int32), intent(in) :: count
        integer(int32), intent(in) :: datatype
        integer(int32), intent(in) :: operation
        type(MPI_Comm), intent(in) :: comm
        integer(int32), intent(inout) :: ierror

        receive_buffer = send_buffer
        ierror = MPI_SUCCESS
    end subroutine mpi_allreduce_int32

    subroutine MPI_Abort(comm, error_code, ierror)
        type(MPI_Comm), intent(in) :: comm
        integer(int32), intent(in) :: error_code
        integer(int32), intent(inout), optional :: ierror

        if (present(ierror)) ierror = error_code
        write (error_unit, '(A,I0)') "FTCMS aborted with error code ", error_code
        error stop
    end subroutine MPI_Abort
#endif

end module core_parallel_mpi

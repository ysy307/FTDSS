program test_domain
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: mpi_f08
    use :: module_core
    use :: module_input
    use :: module_domain
    implicit none
    type(type_input) :: input
    type(type_domain) :: domain
    integer(int32) :: ierr
    integer(int32) :: myrank
    integer(int32) :: nsize
    type(type_dp_3d), pointer :: coordinate

    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)

    call input%initialize()

    call domain%initialize(input)

    call MPI_Finalize(ierr)
end program

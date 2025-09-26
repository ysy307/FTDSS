program test_communicator
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: mpi_f08
    use :: module_core
    use :: module_input
    use :: module_parallel
    implicit none
    type(type_input) :: input
    ! type(type_domain) :: domain
    type(type_halo_communicator) :: communicator
    integer(int32) :: ierr

    call MPI_Init(ierr)

    call input%initialize()

    ! call domain%initialize(input)
    call communicator%initialize(input)

    call MPI_Finalize(ierr)
end program

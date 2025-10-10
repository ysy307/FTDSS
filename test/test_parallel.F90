program test_parallel
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: mpi_f08
    use :: module_core
    use :: module_input
    use :: module_parallel
    implicit none
    type(type_input) :: input
    ! type(type_domain) :: domain
    type(type_halo_communicator) :: communicator
    integer(int32) :: ierr, my_rank, i

    call MPI_Init(ierr)

    call input%initialize()

    ! print *, input%geometry%vtk%communication_partners

    ! call domain%initialize(input)
    call communicator%initialize(input)
    call communicator%display()

    call MPI_Finalize(ierr)
end program test_parallel

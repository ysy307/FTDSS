program test_control
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: mpi_f08
    use :: module_core
    use :: module_input
    use :: module_control
    implicit none
    type(type_input) :: input
    type(type_controls) :: controls
    integer(int32) :: ierr
    integer(int32) :: myrank
    integer(int32) :: nsize

    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)

    call controls%initialize()
    call input%initialize()
    call controls%initialize(input)
    if (myrank == 0) then
        call controls%display()
    end if
    call MPI_Finalize(ierr)
end program

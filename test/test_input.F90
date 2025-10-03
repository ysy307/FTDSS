program test_input
    use, intrinsic :: iso_fortran_env, only: int32, real64
#ifdef _MPI
    use :: mpi_f08
#endif
    use :: module_input
    implicit none
    type(type_input) :: input
    integer(int32) :: ierr
    integer(int32) :: myrank

#ifdef _MPI
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)
    print *, 'ierr=', ierr, ' rank=', myrank
#endif

    call input%initialize()
    call input%display()

#ifdef _MPI
    call MPI_Finalize(ierr)
#endif
end program

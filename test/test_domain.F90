program test_domain
    use, intrinsic :: iso_fortran_env, only: int32, real64
#ifdef _MPI
    use :: mpi_f08
#endif
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

#ifdef _MPI
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)
    print *, 'ierr=', ierr, ' rank=', myrank
#endif

    call input%initialize()
    nsize = input%geometry%vtk%num_points

    ! Initialize the Structure
    allocate (coordinate)
    call coordinate%initialize(nsize)
    call domain%initialize(input, coordinate)

#ifdef _MPI
    call MPI_Finalize(ierr)
#endif
end program

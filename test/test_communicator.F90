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
    integer(int32) :: ierr, my_rank, i

    call MPI_Init(ierr)

    call input%initialize()

    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
    if (my_rank == 0) then
        print *, "my_rank = ", my_rank
        do i = 1, size(input%geometry%vtk%cells(:))
            print '(a,i0)', "cell= ", i
            print '(10(x,i0))', input%geometry%vtk%cells(i)%connectivity(:)
        end do
    end if

    ! call domain%initialize(input)
    call communicator%initialize(input)

    call MPI_Finalize(ierr)
end program

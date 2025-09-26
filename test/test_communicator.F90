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

    if (my_rank == 0) then
        print *, "my_rank = ", my_rank
        do i = 1, input%geometry%vtk%num_points
            print '(8i6)', i, input%geometry%vtk%cells(i)%connectivity(:)
        end do
    end if

    ! call domain%initialize(input)
    call communicator%initialize(input)

    call MPI_Finalize(ierr)
end program

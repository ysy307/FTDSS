program main
    use, intrinsic :: iso_fortran_env, only: int32, real64
#ifdef _MPI
    use :: core_parallel_mpi
#endif
    use :: module_ftcms

    implicit none

    integer(int32) :: ierr
    integer(int32) :: myrank

#ifdef _MPI
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)
#endif

    ! Call test function
    call run_test_ftcms()

#ifdef _MPI
    call MPI_Finalize(ierr)
#endif

contains

    subroutine run_test_ftcms()
        implicit none
        type(type_ftcms) :: ftcms

        call ftcms%initialize()
        call ftcms%run()
        call ftcms%destroy()

    end subroutine run_test_ftcms

end program main

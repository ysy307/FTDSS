program test_main
    use, intrinsic :: iso_fortran_env, only: int32, real64
#ifdef _MPI
    use :: mpi_f08
#endif
    use :: module_ftdss

    implicit none

    integer(int32) :: ierr
    integer(int32) :: myrank
#ifdef _MPI
    logical :: mpi_is_initialized
    logical :: mpi_is_finalized
#endif

#ifdef _MPI
    call MPI_Initialized(mpi_is_initialized, ierr)
    if (.not. mpi_is_initialized) call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)
#endif

    ! テスト関数の呼び出し
    call run_test_ftdss()

#ifdef _MPI
    call MPI_Initialized(mpi_is_initialized, ierr)
    call MPI_Finalized(mpi_is_finalized, ierr)
    if (mpi_is_initialized .and. (.not. mpi_is_finalized)) call MPI_Finalize(ierr)
#endif

contains

    subroutine run_test_ftdss()
        implicit none
        type(type_ftdss) :: ftdss

        call ftdss%initialize()
        call ftdss%run()
        call ftdss%destroy()

    end subroutine run_test_ftdss

end program test_main

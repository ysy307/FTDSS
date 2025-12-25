program test_main
    use, intrinsic :: iso_fortran_env, only: int32, real64
#ifdef _MPI
    use :: mpi_f08
#endif
    use :: module_ftdss
    use :: module_domain

    implicit none

    integer(int32) :: ierr
    integer(int32) :: myrank

#ifdef _MPI
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)
#endif

    ! テスト関数の呼び出し
    call run_test_ftdss()

#ifdef _MPI
    call MPI_Finalize(ierr)
#endif

contains

    subroutine run_test_ftdss()
        implicit none
        type(type_ftdss) :: ftdss

        call ftdss%initialize()
        call ftdss%calc_gradient(ftdss%temperature%pre, ftdss%temperature%grad)

        block
            integer(int32) :: i
            print *, "Gradient of temperature variable:"
            do i = 1, 100
                print '(a,i3,a,3(es16.8,a),es16.8)', "Node ", i, ": (", &
                    ftdss%temperature%grad%x(i), ", ", &
                    ftdss%temperature%grad%y(i), ", ", &
                    ftdss%temperature%grad%z(i), ") at temperature ", &
                    ftdss%temperature%pre(i)
            end do
        end block

    end subroutine run_test_ftdss

end program test_main

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

        ! call ftdss%thermal

        block
            integer(int32), parameter :: num_steps = 100
            integer(int32) :: i
            real(real64) :: h_val, log_h
            real(real64) :: val_theta
            real(real64) :: h_values(num_steps)
            real(real64) :: results_K(4, num_steps)
            type(type_state) :: state
            real(real64) :: res

            integer(int32) :: unit

            ! 計算ループ (h: -0.01 -> -10000)
            open (newunit=unit, file="log/test/main_hcf.log", status="replace", action="write", iostat=ierr)
            write (unit, '(A)') "# h,K_flh"
            do i = 1, num_steps
                ! ログスケールで h を生成 (-0.01 ~ -10000)
                log_h = -2.0d0 + (6.0d0 * real(i - 1, real64) / real(num_steps - 1, real64))
                h_val = -(10.0d0**log_h)

                call state%reset()
                call state%set(temperature=15.0d0, pressure=h_val)
                ! WRF計算実行
                call ftdss%thermal%physics%calc_Kflh(1, state, results_K(1, i))
                write (unit, '(es24.15,",",es24.15)') h_val, results_K(1, i)
                ! call wrf%p%calc(h_val, val_theta)
            end do

        end block

    end subroutine run_test_ftdss

end program test_main

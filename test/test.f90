program test
    use, intrinsic :: iso_fortran_env, only: int32, real64
!$  use :: omp_lib
    use :: stdlib_logger
    use :: Main_FTDSS
    implicit none
    type(Type_FTDSS) :: FTDSS
    real(real64) :: norm_old, norm_new
    integer(int32) :: stat, count
    integer(int32) :: i, j

    call FTDSS%initialize()
    if (was_interrupted()) stop
    call FTDSS%time%profile_start("Setup")
    call FTDSS%IC%apply("thermal", FTDSS%Domain, FTDSS%Thermal%T)
    call FTDSS%BC%apply_CRS(boundary_target='thermal', &
                            current_time=0.0d0, &
                            b=FTDSS%Thermal%T%new, &
                            Domain=FTDSS%Domain, &
                            mode=-1)

    call FTDSS%Thermal%T%Shift()
    count = 0
    call FTDSS%time%profile_stop("Setup")

    call FTDSS%time%profile_start("IO")
    call FTDSS%output%output_fields(file_counts=0, &
                                    domain=FTDSS%Domain, &
                                    porosity=FTDSS%phi%pre, &
                                    temperature=FTDSS%Thermal%T%pre, &
                                    si=FTDSS%Thermal%Qice%pre)
    call FTDSS%output%output_history(time=0.0d0, &
                                     temperature=FTDSS%Thermal%T%pre, &
                                     porosity=FTDSS%phi%pre, &
                                     Propeties=FTDSS%Property, &
                                     Domain=FTDSS%Domain)
    call FTDSS%time%profile_stop("IO")

    ! stop

    call FTDSS%iteration%reset_timestep()
    call global_logger%log_information(message="Starting time loop")
    TIME_LOOP: do while (FTDSS%time%time < FTDSS%time%end_time)
        ! exit TIME_LOOP
        call FTDSS%time%shift()
        call FTDSS%iteration%increment_iter()
        call FTDSS%iteration%reset_step()
        NR_LOOP_THERMAL: do while (FTDSS%iteration%should_continue())
            call FTDSS%time%profile_start("Assemble")
            call FTDSS%iteration%increment_step()
            call FTDSS%Thermal%Assemble(FTDSS%Domain, FTDSS%Property, FTDSS%phi%pre, FTDSS%time, FTDSS%iteration)
            call FTDSS%time%profile_stop("Assemble")
            call FTDSS%time%profile_start("Setup")
            call FTDSS%BC%apply_CRS(boundary_target='thermal', &
                                    current_time=FTDSS%time%get_time(), &
                                    A=FTDSS%Thermal%KT_star_0, &
                                    b=FTDSS%Thermal%PHIT, &
                                    Domain=FTDSS%Domain, &
                                    mode=1)
            if (FTDSS%iteration%get_step() == 1) call FTDSS%iteration%set_initial_norms(res_vec=FTDSS%Thermal%PHIT)

            ! open (unit=10, file='log/debug4.txt', status='replace')
            ! do i = 1, FTDSS%Thermal%KT_star_0%num_row
            !     do j = FTDSS%Thermal%KT_star_0%Ptr(i), FTDSS%Thermal%KT_star_0%Ptr(i + 1) - 1
            !         write (10, '(i0, 2x, i0,2x,f16.7)') i, FTDSS%Thermal%KT_star_0%Ind(j), FTDSS%Thermal%KT_star_0%Val(j)
            !     end do
            ! end do
            ! close (10)
            ! open (unit=20, file='log/debug5.txt', status='replace')
            ! do i = 1, size(FTDSS%Thermal%PHIT(:))
            !     write (20, '( i0,2x,f16.7)') i, FTDSS%Thermal%PHIT(i)
            ! end do
            ! close (20)
            ! stop

            call FTDSS%time%profile_stop("Setup")

            call FTDSS%time%profile_start("Solve")
            call FTDSS%Thermal%solve(FTDSS%time, FTDSS%iteration)
            ! call FTDSS%Thermal%Solver%Solve(FTDSS%Thermal%KT_star_0, FTDSS%Thermal%PHIT, FTDSS%Thermal%T%new(:), stat)
            if (FTDSS%iteration%get_step() == 1) call FTDSS%iteration%set_initial_norms(upd_vec=FTDSS%Thermal%T%dif(:))
            call FTDSS%iteration%check_convergence(FTDSS%Thermal%PHIT, FTDSS%Thermal%T%dif(:))

            call FTDSS%time%profile_stop("Solve")
            ! open (unit=30, file='log/debug3.txt', status='replace')
            ! do i = 1, size(FTDSS%Thermal%T%new(:))
            !     write (30, '( i0,2x,f16.7)') i, FTDSS%Thermal%T%new(i)
            ! end do
            ! close (30)
            ! stop

            ! call Thermal%Solver%Solve(FTDSS%Thermal%KT_star_0, FTDSS%Thermal%PHIT, FTDSS%Thermal%T%new(:), stat)

            ! Thermal%T%new(:) = Thermal%T%pre(:) + Thermal%T%dif(:)

            ! norm_new = norm_2(Thermal%nsize, Thermal%T%dif)
            norm_new = maxval(abs(FTDSS%Thermal%T%dif))

            ! print *, FTDSS%iteration%iter, FTDSS%iteration%iter >= 2
            !! Convergence check
            ! if (FTDSS%iteration%iter >= 1) then
            !     ! if (norm_new < 1.0d-5) then
            !     print *, FTDSS%iteration%step, FTDSS%iteration%iter, norm_new
            !     FTDSS%iteration%isConverged = .true.
            !     call FTDSS%Thermal%T%Shift()
            !     exit NR_LOOP_THERMAL
            ! end if

            FTDSS%Thermal%T%pre(:) = FTDSS%Thermal%T%new(:)
            ! call FTDSS%Thermal%Update(FTDSS%NodeBelonging, FTDSS%phi%pre(:))
        end do NR_LOOP_THERMAL

        ! if (FTDSS%iteration%iter >= FTDSS%iteration%max_iter) then
        !     FTDSS%time%time = FTDSS%time%time_old
        !     FTDSS%time%dt = FTDSS%time%dt * 0.5d0
        !     call FTDSS%Thermal%T%Shift(reverse=.true.)
        ! end if

        print *, "Time:", FTDSS%time%get_time(), &
            "Iter:", FTDSS%iteration%get_iter(), &
            "Step:", FTDSS%iteration%get_step()

        call FTDSS%time%profile_start("IO")
        call FTDSS%output%output_history(time=FTDSS%time%get_time(), &
                                         temperature=FTDSS%Thermal%T%pre, &
                                         porosity=FTDSS%phi%pre, &
                                         Propeties=FTDSS%Property, &
                                         Domain=FTDSS%Domain)
        ! print *, mod(FTDSS%iteration%step, 100)
        if (mod(FTDSS%iteration%get_step(), 10) == 0) then
            count = count + 1
            call FTDSS%output%output_fields(file_counts=count, &
                                            domain=FTDSS%Domain, &
                                            porosity=FTDSS%phi%pre, &
                                            temperature=FTDSS%Thermal%T%pre, &
                                            si=FTDSS%Thermal%Qice%pre)
        end if
        call FTDSS%time%profile_stop("IO")

        call FTDSS%Thermal%T%Shift()

        if (was_interrupted()) stop

    end do TIME_LOOP

    call FTDSS%time%profile_stop("Total")
    call FTDSS%time%record("End")
    call FTDSS%output%output_system_log(FTDSS%time, FTDSS%Thermal%KT_star_0, FTDSS%Domain)

    stop

end program test

program test
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_logger
    use :: Main_FTDSS

#ifdef _OPENMP
    use :: omp_lib
#endif

    implicit none
    type(Type_FTDSS) :: FTDSS
    real(real64) :: norm_old, norm_new
    integer(int32) :: stat, count
    integer(int32) :: i, j
    character(1) :: BC_Type

    call FTDSS%initialize()
    ! if (was_interrupted()) then
    !     call global_logger%log_warning(message="Program interrupted by user.")
    !     stop
    ! end if
    call FTDSS%time%Profile_Start("Setup")
    call FTDSS%IC%apply(physics="Thermal", &
                        domain=FTDSS%Domain, &
                        var=FTDSS%Thermal%T)
    call FTDSS%BC%apply_CRS(BC_Type='T', &
                            current_time=0.0d0, &
                            b=FTDSS%Thermal%T%new, &
                            Domain=FTDSS%Domain, &
                            mode=2) ! mode=2 for initial conditions
    ! FTDSS%phi%pre(:)

    ! call FTDSS%Thermal%HTC% phi, Temperature, Pw, Ice, Density
    ! print *, FTDSS%Thermal%T%pre(1)
    ! print *, FTDSS%Thermal%HTC%Calc(NodeBelonging=FTDSS%NodeBelonging(1), phi=FTDSS%phi%pre(1), Temperature=FTDSS%Thermal%T%pre(1), Ice=FTDSS%Thermal%ICE(1)%f, Density=FTDSS%Thermal%DEN)
    ! call FTDSS%Thermal%Update(FTDSS%NodeBelonging, FTDSS%phi%pre(:))
    call FTDSS%Thermal%T%Shift()
    count = 0
    call FTDSS%time%Profile_Stop("Setup")

    call FTDSS%time%Profile_Start("IO")
    call FTDSS%Output%Overall%Output(fc=count, &
                                     rcm=FTDSS%Domain%rcm, &
                                     Temp=FTDSS%Thermal%T%pre, &
                                     Si=FTDSS%Thermal%Qice%pre)
    call FTDSS%Output%Output_Observation(time=0.0d0, Temp=FTDSS%Thermal%T%pre, Si=FTDSS%Thermal%Qice%pre, Thermal=FTDSS%Thermal, phi=FTDSS%phi%pre, Propeties=FTDSS%Property, Domain=FTDSS%Domain)
    call FTDSS%time%Profile_Stop("IO")
    FTDSS%Iteration%step = 0
    FTDSS%Iteration%max_iter = 100

    ! stop

    FTDSS%Iteration%isConverged = .true.
    print *, "Starting time loop"
    TIME_LOOP: do while (FTDSS%time%time < FTDSS%time%end_time)
        ! exit TIME_LOOP
        FTDSS%time%time_old = FTDSS%time%time
        FTDSS%time%time = FTDSS%time%time + FTDSS%time%dt
        FTDSS%time%dt_old(1) = FTDSS%time%dt

        FTDSS%Iteration%iter = 0

        !! Thermal Newton-Raphson FTDSS%Iteration

        NR_LOOP_THERMAL: do while (FTDSS%Iteration%iter <= FTDSS%Iteration%max_iter)
            call FTDSS%time%Profile_Start("Assemble")
            ! print *, FTDSS%Iteration%iter
            if (FTDSS%Iteration%isConverged) then
                FTDSS%Iteration%step = FTDSS%Iteration%step + 1
                FTDSS%Iteration%isConverged = .false.
            end if
            FTDSS%Iteration%iter = FTDSS%Iteration%iter + 1
            ! print *, FTDSS%Iteration%iter
            ! if (FTDSS%Iteration%iter == 1) then
            !     if (FTDSS%Iteration%step >= 2) then
            !         Thermal%T%pre(:) = Thermal%T%old(:, 1) + (Thermal%T%old(:, 1) - Thermal%T%old(:, 2)) * (FTDSS%time%dt / FTDSS%time%dt_old(1))
            !         ! Thermal%T%pre(:) = 2.0d0 * Thermal%T%old(:, 1) - Thermal%T%old(:, 2)
            !         call Thermal%Update(Input%Regions(1)%Thermal%Porosity, Input%Regions(1)%Thermal%rho(3), FTDSS%Iteration%iter)
            !     end if
            ! ! end if
            ! print *, Thermal%KT_star_0%nnz
            ! print *, Thermal%KT_star_0%ptr(:)
            ! print *, Thermal%KT_star_0%ind(:)
            ! stop

            call FTDSS%Thermal%Assemble(FTDSS%Domain, FTDSS%Property, FTDSS%phi%pre, FTDSS%time%dt, FTDSS%Iteration%step, FTDSS%Iteration%iter)
            ! call Thermal%BC%Fix_BoundaryConditions(Thermal%KT_star_0, Thermal%PHIT)
            ! Thermal%PHIT(:) = -Thermal%PHIT(:)
            ! call FTDSS%Thermal%BC%
            call FTDSS%time%Profile_Stop("Assemble")
            call FTDSS%time%Profile_Start("Setup")
            BC_Type = 'T'
            call FTDSS%BC%apply_CRS(BC_Type=BC_Type, &
                                    current_time=FTDSS%time%time, &
                                    A=FTDSS%Thermal%KT_star_0, &
                                    b=FTDSS%Thermal%PHIT, &
                                    Domain=FTDSS%Domain, &
                                    mode=0)
            ! call FTDSS%Thermal%BC%Fix_BC(A=FTDSS%Thermal%KT_star_0, &
            !                              b=FTDSS%Thermal%PHIT, &
            !                              Sides=FTDSS%Thermal%Domain%Sides, &
            !                              time=FTDSS%time%time)

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
            ! call FTDSS%Thermal%BC%Fix_Bounday_Values(FTDSS%Thermal%KT_star_0, FTDSS%Thermal%PHIT)

            call FTDSS%time%Profile_Stop("Setup")

            call FTDSS%time%Profile_Start("Solve")
            call FTDSS%Thermal%Solver%Solve(FTDSS%Thermal%KT_star_0, FTDSS%Thermal%PHIT, FTDSS%Thermal%T%new(:), stat)

            call FTDSS%time%Profile_Stop("Solve")
            ! open (unit=30, file='log/debug3.txt', status='replace')
            ! do i = 1, size(FTDSS%Thermal%T%new(:))
            !     write (30, '( i0,2x,f16.7)') i, FTDSS%Thermal%T%new(i)
            ! end do
            ! close (30)
            ! call FTDSS%Thermal%Solver%Check(stat, FTDSS%time%time)
            ! stop

            ! call Thermal%Solver%Solve(FTDSS%Thermal%KT_star_0, FTDSS%Thermal%PHIT, FTDSS%Thermal%T%new(:), stat)

            ! Thermal%T%new(:) = Thermal%T%pre(:) + Thermal%T%dif(:)

            ! norm_new = norm_2(Thermal%nsize, Thermal%T%dif)
            norm_new = maxval(abs(FTDSS%Thermal%T%dif))

            ! print *, FTDSS%Iteration%iter, FTDSS%Iteration%iter >= 2
            !! Convergence check
            if (FTDSS%Iteration%iter >= 1) then
                ! if (norm_new < 1.0d-5) then
                print *, FTDSS%Iteration%step, FTDSS%Iteration%iter, norm_new
                FTDSS%Iteration%isConverged = .true.
                call FTDSS%Thermal%T%Shift()
                exit NR_LOOP_THERMAL
            end if

            FTDSS%Thermal%T%pre(:) = FTDSS%Thermal%T%new(:)
            ! call FTDSS%Thermal%Update(FTDSS%NodeBelonging, FTDSS%phi%pre(:))
        end do NR_LOOP_THERMAL

        ! if (FTDSS%Iteration%iter >= FTDSS%Iteration%max_iter) then
        !     FTDSS%time%time = FTDSS%time%time_old
        !     FTDSS%time%dt = FTDSS%time%dt * 0.5d0
        !     call FTDSS%Thermal%T%Shift(reverse=.true.)
        ! end if

        call FTDSS%time%Profile_Start("IO")
        call FTDSS%Output%Output_Observation(time=FTDSS%time%time / 86400.0d0, Temp=FTDSS%Thermal%T%pre, Si=FTDSS%Thermal%Qice%pre, Thermal=FTDSS%Thermal, phi=FTDSS%phi%pre, Propeties=FTDSS%Property, Domain=FTDSS%Domain)
        ! print *, mod(FTDSS%Iteration%step, 100)
        if (mod(FTDSS%Iteration%step, 10) == 0) then
            count = count + 1
            call FTDSS%Output%Overall%Output(fc=count, &
                                             rcm=FTDSS%Domain%rcm, &
                                             Temp=FTDSS%Thermal%T%pre, &
                                             Si=FTDSS%Thermal%Qice%pre)
        end if
        call FTDSS%time%Profile_Stop("IO")

        ! if (was_interrupted()) then
        !     call global_logger%log_warning(message="Program interrupted by user.")
        !     stop
        ! end if

    end do TIME_LOOP

    call FTDSS%time%Profile_Stop("Total")
    call FTDSS%time%Record("End")
    call FTDSS%Output%Output_SystemLog(FTDSS%time, FTDSS%Thermal%KT_star_0, FTDSS%Domain)

    stop

end program test

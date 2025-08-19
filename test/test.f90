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
    character(256) :: out_char

    call FTDSS%initialize()
    if (was_interrupted()) stop
    call FTDSS%time%profile_start("Setup")
    call FTDSS%IC%apply("thermal", FTDSS%domain, FTDSS%T)
    call FTDSS%BC%apply_CRS(boundary_target='thermal', &
                            current_time=0.0d0, &
                            b=FTDSS%T%new, &
                            Domain=FTDSS%domain, &
                            mode=-1)
    call FTDSS%shift()
    call FTDSS%thermal%update(FTDSS%domain, FTDSS%property, FTDSS%T%pre, FTDSS%phi%pre)

    count = 0
    call FTDSS%time%profile_stop("Setup")

    call FTDSS%time%profile_start("IO")
    call FTDSS%output%output_fields(file_counts=0, &
                                    domain=FTDSS%domain, &
                                    porosity=FTDSS%phi%pre, &
                                    temperature=FTDSS%T%pre, &
                                    si=FTDSS%thermal%Si%pre)
    call FTDSS%output%output_history(time=0.0d0, &
                                     temperature=FTDSS%T%pre, &
                                     porosity=FTDSS%phi%pre, &
                                     Propeties=FTDSS%Property, &
                                     Domain=FTDSS%domain)
    call FTDSS%time%profile_stop("IO")

    ! stop

    call FTDSS%iteration%reset_timestep()
    call global_logger%log_information(message="Starting time loop")
    TIME_LOOP: do while (FTDSS%time%time < FTDSS%time%end_time)
        ! exit TIME_LOOP
        call FTDSS%time%shift()
        call FTDSS%iteration%increment_iter()
        call FTDSS%iteration%reset_step()
        call FTDSS%thermal%compute(FTDSS%domain, FTDSS%Property, FTDSS%T, FTDSS%phi, &
                                   FTDSS%time, FTDSS%iteration, FTDSS%BC)

        call FTDSS%time%profile_start("Setup")
        call FTDSS%thermal%update(FTDSS%domain, FTDSS%property, FTDSS%T%pre, FTDSS%phi%pre)
        call FTDSS%time%profile_stop("Setup")

        write (out_char, "(A, F10.6, A, I4, A, I3)") &
            'Time: ', FTDSS%time%get_time(), &
            ' Iter: ', FTDSS%iteration%get_iter(), &
            ' Step: ', FTDSS%iteration%get_step()

        call global_logger%log_information(message=trim(out_char))

        call FTDSS%time%profile_start("IO")
        call FTDSS%output%output_history(time=FTDSS%time%get_time(), &
                                         temperature=FTDSS%T%pre, &
                                         porosity=FTDSS%phi%pre, &
                                         Propeties=FTDSS%Property, &
                                         Domain=FTDSS%domain)
        ! print *, mod(FTDSS%iteration%step, 100)
        if (mod(FTDSS%iteration%get_iter(), 10) == 0) then
            count = count + 1
            call FTDSS%output%output_fields(file_counts=count, &
                                            domain=FTDSS%domain, &
                                            porosity=FTDSS%phi%pre, &
                                            temperature=FTDSS%T%pre, &
                                            si=FTDSS%thermal%Qice%pre)
        end if
        call FTDSS%time%profile_stop("IO")

        call FTDSS%shift()

        if (was_interrupted()) stop

    end do TIME_LOOP

    call FTDSS%time%profile_stop("Total")
    call FTDSS%time%record("End")
    call FTDSS%output%output_system_log(FTDSS%time, FTDSS%thermal%KT_star, FTDSS%domain)

    stop

end program test

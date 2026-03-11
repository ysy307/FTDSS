program test
    use, intrinsic :: iso_fortran_env, only: int32, real64
!$  use :: omp_lib
    use :: stdlib_logger
    use :: Main_FTCMS
    implicit none
    type(Type_FTCMS) :: FTCMS
    integer(int32) :: count
    character(256) :: out_char

    call FTCMS%initialize()
    if (was_interrupted()) stop
    call FTCMS%controls%time%profile_start("Setup")
    call FTCMS%IC%apply("thermal", FTCMS%domain, FTCMS%T)
    call FTCMS%BC%apply_CRS(boundary_target='thermal', &
                            current_time=0.0d0, &
                            b=FTCMS%T%new, &
                            Domain=FTCMS%domain, &
                            mode=-1)
    call FTCMS%shift()
    call FTCMS%thermal%update(FTCMS%domain, FTCMS%property, FTCMS%T%pre, FTCMS%phi%pre, FTCMS%controls)

    count = 0
    call FTCMS%controls%time%profile_stop("Setup")

    call FTCMS%controls%time%profile_start("IO")
    call FTCMS%output%output_fields(file_counts=0, &
                                    domain=FTCMS%domain, &
                                    porosity=FTCMS%phi%pre, &
                                    temperature=FTCMS%T%pre, &
                                    si=FTCMS%thermal%Si%pre)
    call FTCMS%output%output_history(time=0.0d0, &
                                     temperature=FTCMS%T%pre, &
                                     porosity=FTCMS%phi%pre, &
                                     Propeties=FTCMS%Property, &
                                     Domain=FTCMS%domain)
    call FTCMS%controls%time%profile_stop("IO")

    ! stop

    call FTCMS%controls%iteration%reset_timestep()
    call global_logger%log_information(message="Starting time loop")
    TIME_LOOP: do while (FTCMS%controls%time%time < FTCMS%controls%time%end_time)
        ! exit TIME_LOOP
        call FTCMS%controls%time%shift()
        call FTCMS%controls%iteration%increment_iter()
        call FTCMS%controls%iteration%reset_step()
        call FTCMS%thermal%compute(FTCMS%domain, FTCMS%Property, FTCMS%T, FTCMS%phi, &
                                   FTCMS%controls, FTCMS%BC)

        call FTCMS%controls%time%profile_start("Setup")
        call FTCMS%thermal%update(FTCMS%domain, FTCMS%property, FTCMS%T%pre, FTCMS%phi%pre, FTCMS%controls)
        call FTCMS%controls%time%profile_stop("Setup")

        write (out_char, "(A, F10.6, A, I4, A, I3)") &
            'Time: ', FTCMS%controls%time%get_time(), &
            ' Iter: ', FTCMS%controls%iteration%get_iter(), &
            ' Step: ', FTCMS%controls%iteration%get_step()

        call global_logger%log_information(message=trim(out_char))

        call FTCMS%controls%time%profile_start("IO")
        call FTCMS%output%output_history(time=FTCMS%controls%time%get_time(), &
                                         temperature=FTCMS%T%pre, &
                                         porosity=FTCMS%phi%pre, &
                                         Propeties=FTCMS%Property, &
                                         Domain=FTCMS%domain)
        ! print *, mod(FTCMS%controls%iteration%step, 100)
        if (mod(FTCMS%controls%iteration%get_iter(), 10) == 0) then
            count = count + 1
            call FTCMS%output%output_fields(file_counts=count, &
                                            domain=FTCMS%domain, &
                                            porosity=FTCMS%phi%pre, &
                                            temperature=FTCMS%T%pre, &
                                            si=FTCMS%thermal%Qice%pre)
        end if
        call FTCMS%controls%time%profile_stop("IO")

        call FTCMS%shift()

        if (was_interrupted()) stop

    end do TIME_LOOP

    call FTCMS%controls%time%profile_stop("Total")
    call FTCMS%controls%time%record("End")
    call FTCMS%output%output_system_log(FTCMS%controls%time, FTCMS%thermal%KT_star, FTCMS%domain)

    stop

end program test

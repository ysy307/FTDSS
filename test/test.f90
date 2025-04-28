program test
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    use :: Solver_Time
    use :: Inout_Input
    use :: Inout_Output
    use :: Calculate_BLAS
    use :: Main_Thermal
    use :: stdlib_strings, only:to_string

#ifdef _OPENMP
    use omp_lib
#endif
    implicit none

    type(Type_Input) :: Input
    ! type(Input_Ice) :: Ice
    ! type(Input_Thermal) :: Input%Regions(1)%Thermal%Porosity, ThermalInput
    type(Type_Thermal_3Phase_2D) :: Thermal
    type(Type_Time) :: Time
    type(Type_Iteration) :: Iteration
    type(Type_Output) :: Output
    integer(int32) :: i, j, k
    integer(int32), allocatable :: Elements(:, :)
    ! real(real64) :: Lf, Tf, Input%Regions(1)%Thermal%Porosity
    integer(int32) :: meshType
    real(real64) :: norm_old, norm_new
    integer(int32) :: stat, count

    character(:), allocatable :: filename

    Iteration%max_iter = 100

    Input = Type_Input()

    time = Type_Time(Input)

    Thermal = Type_Thermal_3Phase_2D(Input)
    Thermal%T%new(:) = 18.0d0
    call Thermal%BC%Fix_Bounday_Values(Thermal%T%new(:))
    Thermal%T%pre(:) = Thermal%T%new(:)

    call Thermal%Update(Input%Regions(1)%Thermal%Porosity, Input%Regions(1)%Thermal%rho(3), 0)
    call Thermal%T%Shift()
    count = 0
    filename = '/workspaces/FTDSS/tmp/output_'//to_string(count, '(i0)')//'.vtu'
    ! print *, count, filename
    call Output%Output_All(filename, Input%VTK%numPoints, Input%VTK%numTotalCells, Elements, Input%VTK%POINTS, Thermal%T%pre(:), Thermal%Ice%Si%pre)
    Iteration%step = 0

    ! print *, Thermal%T%old(:, 1)

    Iteration%isConverged = .true.
    print *, "Starting time loop"
    ! stop
    TIME_LOOP: do while (time%time < time%end_time)
        time%time_old = time%time
        time%time = time%time + time%dt
        time%dt_old(1) = time%dt

        Iteration%iter = 0

        !! Thermal Newton-Raphson Iteration
        NR_LOOP_THERMAL: do while (Iteration%iter <= Iteration%max_iter)
            ! print *, Iteration%iter
            if (Iteration%isConverged) then
                Iteration%step = Iteration%step + 1
                Iteration%isConverged = .false.
            end if
            Iteration%iter = Iteration%iter + 1
            ! if (Iteration%iter == 1) then
            !     if (Iteration%step >= 2) then
            !         Thermal%T%pre(:) = Thermal%T%old(:, 1) + (Thermal%T%old(:, 1) - Thermal%T%old(:, 2)) * (time%dt / time%dt_old(1))
            !         ! Thermal%T%pre(:) = 2.0d0 * Thermal%T%old(:, 1) - Thermal%T%old(:, 2)
            !         call Thermal%Update(Input%Regions(1)%Thermal%Porosity, Input%Regions(1)%Thermal%rho(3), Iteration%iter)
            !     end if
            ! ! end if
            ! print *, Thermal%KT_star_0%nnz
            ! print *, Thermal%KT_star_0%ptr(:)
            ! print *, Thermal%KT_star_0%ind(:)
            ! stop

            call Thermal%Assemble(time%dt, Iteration%step, Iteration%iter)
            ! call Thermal%BC%Fix_BoundaryConditions(Thermal%KT_star_0, Thermal%PHIT)
            Thermal%PHIT(:) = -Thermal%PHIT(:)
            call Thermal%BC%Fix_Bounday_Values(Thermal%KT_star_0, Thermal%PHIT)

            open (unit=10, file='debug.txt', status='replace')
            do i = 1, Thermal%nsize
                ! print *, Thermal%KT_star_0%Ptr(i), Thermal%KT_star_0%Ptr(i + 1) - 1
                do j = Thermal%KT_star_0%Ptr(i), Thermal%KT_star_0%Ptr(i + 1) - 1
                    write (10, '(i0, 2x, i0,2x,f16.7)') i, Thermal%KT_star_0%Ind(j), Thermal%KT_star_0%Val(j)
                end do
            end do
            close (10)
            open (unit=20, file='debug2.txt', status='replace')
            do i = 1, size(Thermal%PHIT)
                write (20, '( i0,2x,f16.7)') i, Thermal%PHIT(i)
            end do
            close (20)
            ! stop
            ! call Thermal%BC%Fix_Bounday_Values(Thermal%KT_star_0, Thermal%PHIT)
            call Thermal%Solver%Solve(Thermal%KT_star_0, Thermal%PHIT, Thermal%T%new(:), stat)

            open (unit=20, file='debug3.txt', status='replace')
            do i = 1, size(Thermal%PHIT)
                write (20, '( i0,2x,f16.7)') i, Thermal%T%new(i)
            end do
            close (20)
            stop
            ! call Thermal%Solver%Solve(Thermal%KT_star_0, Thermal%PHIT, Thermal%T%dif, stat)

            ! Thermal%T%new(:) = Thermal%T%pre(:) + Thermal%T%dif(:)

            ! norm_new = norm_2(Thermal%nsize, Thermal%T%dif)
            norm_new = maxval(abs(Thermal%T%dif))

            !! Convergence check
            if (Iteration%iter >= 5) then
                ! if (norm_new < 1.0d-5) then
                print *, Iteration%step, Iteration%iter, norm_new
                Iteration%isConverged = .true.
                call Thermal%T%Shift()
                exit NR_LOOP_THERMAL
            end if

            Thermal%T%pre(:) = Thermal%T%new(:)
            call Thermal%Update(Input%Regions(1)%Thermal%Porosity, Input%Regions(1)%Thermal%rho(3), Iteration%iter)
        end do NR_LOOP_THERMAL

        if (Iteration%iter >= Iteration%max_iter) then
            time%time = time%time_old
            time%dt = time%dt * 0.5d0
            call Thermal%T%Shift(reverse=.true.)
        end if

        ! print *, mod(Iteration%step, 100)
        if (mod(Iteration%step, 10) == 0) then
            count = count + 1
            filename = '/workspaces/FTDSS/tmp/output_'//to_string(count, '(i0)')//'.vtu'
            ! print *, count, filename
            call Output%Output_All(filename, Input%VTK%numPoints, Input%VTK%numTotalCells, Elements, Input%VTK%POINTS, Thermal%T%pre, Thermal%Ice%Qice%pre)
            ! if (count == 100) stop
        end if

    end do TIME_LOOP

    stop

end program test

program test
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
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
    type(Input_Ice) :: Ice
    type(Input_Thermal) :: ThermalInput
    type(Type_Thermal_3Phase) :: Thermal
    type(Type_Time) :: Time
    type(Type_Iteration) :: Iteration
    type(Type_Output) :: Output
    integer(int32) :: i, j, k
    integer(int32), allocatable :: Elements(:, :)
    real(real64) :: Lf, Tf, phi_soil
    integer(int32) :: meshType
    real(real64) :: norm_old, norm_new
    integer(int32) :: stat, count

    character(:), allocatable :: filename

    time%start_time = 0.0d0
    time%end_time = 86400.0d0
    time%dt = 1.0d0
    allocate (time%dt_old(1))

    Iteration%max_iter = 20

    meshType = 3

    Lf = 334560.d0
    Tf = 0.0d0
    phi_soil = 0.3d0

    Input = Type_Input()
    allocate (Elements(3, Input%VTK%CELLS(5)%nCells))

    do i = 1, Input%VTK%CELLS(5)%nCells
        do j = 1, 3
            Elements(j, i) = Input%VTK%CELLS(5)%Nodes(j, i)
        end do
    end do

    Ice%QiceType = 2
    Ice%ModelType = 2
    Ice%isSegregation = .false.
    Ice%c_unit = "m"
    Ice%thetaS = 0.30d0
    Ice%thetaR = 0.0d0
    Ice%alpha1 = 0.2d0
    Ice%n1 = 1.8d0

    allocate (ThermalInput%c(3))
    ThermalInput%c(1) = 921.0d0
    ThermalInput%c(2) = 4180.d0
    ThermalInput%c(3) = 2100.d0
    allocate (ThermalInput%rho(3))
    ThermalInput%rho(1) = 2800.0d0
    ThermalInput%rho(2) = 1000.0d0
    ThermalInput%rho(3) = 917.0d0
    allocate (ThermalInput%lambda(3))
    ThermalInput%lambda(1) = 3.78d0
    ThermalInput%lambda(2) = 0.6d0
    ThermalInput%lambda(3) = 2.2d0
    allocate (ThermalInput%Cp(3))
    ThermalInput%Cp(1) = ThermalInput%c(1) * ThermalInput%rho(1)
    ThermalInput%Cp(2) = ThermalInput%c(2) * ThermalInput%rho(2)
    ThermalInput%Cp(3) = ThermalInput%c(3) * ThermalInput%rho(3)

    Thermal = Type_Thermal_3Phase(Elements, Input%VTK%POINTS, meshType, Lf, Tf, Ice, ThermalInput, Input%Conditions, Input%VTK%numPoints, Input%VTK)
    Thermal%T%new(:) = 18.0d0
    call Thermal%BC%Fix_Bounday_Values(Thermal%T%new(:))
    Thermal%T%pre(:) = Thermal%T%new(:)

    call Thermal%Update(phi_soil, ThermalInput%rho(3), 0)
    call Thermal%T%Shift()
    count = 0
    filename = '/workspaces/FTDSS/tmp/output_'//to_string(count, '(i0)')//'.vtu'
    ! print *, count, filename
    call Output%Output_All(filename, Input%VTK%numPoints, Input%VTK%numCells, Elements, Input%VTK%POINTS, Thermal%T%pre(:), Thermal%Ice%Si%pre)
    Iteration%step = 0

    ! print *, Thermal%T%old(:, 1)

    Iteration%isConverged = .true.
    ! stop
    TIME_LOOP: do while (time%time < time%end_time)
        time%time_old = time%time
        time%time = time%time + time%dt
        time%dt_old(1) = time%dt

        Iteration%iter = 0

        !! Thermal Newton-Raphson Iteration
        NR_LOOP_THERMAL: do while (Iteration%iter <= Iteration%max_iter)
            if (Iteration%isConverged) then
                Iteration%step = Iteration%step + 1
                Iteration%isConverged = .false.
            end if
            Iteration%iter = Iteration%iter + 1
            ! print *, Iteration%iter, Iteration%step, Iteration%isConverged

            call Thermal%Assemble(time%dt, Iteration%step, Iteration%iter)
            call Thermal%BC%Fix_BoundaryConditions(Thermal%KT_star_0, Thermal%PHIT)
            Thermal%PHIT(:) = -Thermal%PHIT(:)
            call Thermal%Solver%Solve(Thermal%KT_star_0, Thermal%PHIT, Thermal%T%dif, stat)

            Thermal%T%new(:) = Thermal%T%pre(:) + Thermal%T%dif(:)

            ! norm_new = norm_2(Thermal%nsize, Thermal%T%dif)
            norm_new = maxval(abs(Thermal%T%dif))

            !! Convergence check
            ! if (Iteration%iter >= 3) then
            if (norm_new < 1.0d-5) then
                print *, Iteration%iter, norm_new
                Iteration%isConverged = .true.
                call Thermal%T%Shift()
                exit NR_LOOP_THERMAL
            end if

            Thermal%T%pre(:) = Thermal%T%new(:)
            call Thermal%Update(phi_soil, ThermalInput%rho(3), Iteration%iter)
        end do NR_LOOP_THERMAL

        if (Iteration%iter >= Iteration%max_iter) then
            time%time = time%time_old
            time%dt = time%dt * 0.5d0
            call Thermal%T%Shift(reverse=.true.)
        end if

        ! print *, mod(Iteration%step, 100)
        if (mod(Iteration%step, 100) == 0) then
            count = count + 1
            filename = '/workspaces/FTDSS/tmp/output_'//to_string(count, '(i0)')//'.vtu'
            ! print *, count, filename
            call Output%Output_All(filename, Input%VTK%numPoints, Input%VTK%numCells, Elements, Input%VTK%POINTS, Thermal%T%pre, Thermal%Ice%Si%pre)
            ! if (count == 100) stop
        end if

    end do TIME_LOOP

    stop

end program test

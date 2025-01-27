program test
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    ! use :: Inout_Inout
    use :: allocate
    use :: Allocate_Structure
    use :: Solver_Initialize
    use :: Solver_InitCopy
    use :: Calculate_Area
    use :: Calculate_Shape
    use :: Calculate_Observation
    use :: Condition_FixInitialCondition
    use :: Condition_FixBoundaryCondition
    use :: Calculate_Update
    use :: Matrix_Assemble
    use :: error
    use :: Matrix_ConvertCRS
    use :: Calculate_TRM, only:TRMethod
    use :: Solver_Solve
    use :: Inout_Stdout
    use :: Inout_Input
    use :: Inout_Output
    use :: Main_Solver

    ! use :: tomlf

#ifdef _OPENMP
    use omp_lib
#endif
    implicit none

    type(SolverInfo) :: Solver
    ! type(IO) :: Inout

    type(CRS) :: CTop
    type(ILS) :: ILEQ
    type(DLS) :: DLEQ
    type(Input) :: Inputs
    ! type(Output) :: Outputs
    type(Class_Solver) :: Heat

    real(real64), pointer :: ptst, pdt, podt
    integer(int32), pointer :: piter, ptiter, piNL

    integer(int32) :: i, j, ig
    integer(int32) :: ierr, max_thread, idamax, iNI

    ! Time variables
    real(real64) :: pts, pte, ts, te, dt, tst, tst_old, its, otst, conv_time_out, dt_max, dt_min, outtst
    ! real(real64) :: lis_sum_time, lis_sum_itime, lis_sum_ptime
    real(real64) :: sdts, sdte

    ! type(toml_table), allocatable :: table
    ! character(len=:), allocatable :: title
    ! real, allocatable :: spectrum(:)

    ! character(256) :: Tsolver_name, Psolver_name

#ifdef _MPI
    call MPI_Init(ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, Solver%MPI%size, ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, Solver%MPI%rank, ierr)

    write (*, *) "MPI size: ", Solver%MPI%size
    write (*, *) "MPI rank: ", Solver%MPI%rank
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
#endif

#ifdef _OPENMP
    pts = omp_get_wtime()
    call init_omp_config(Solver)
#endif

    Inputs = Input()
    ! Outputs = Output(Inputs)
    Heat = Class_Solver(Inputs, "Thermal")
    print *, Heat%BCGroup
    ! tHeat = Heat(Inputs)
    ! call Inputs%Input_Parameters()
    ! call Inputs%Input_Coodinates()
    ! call Inputs%Input_Vertices()

    ! call Inout%Input_Coodinates()

    ! block
    !     integer :: io
    !     type(toml_error), allocatable :: ferror

    !     open (file="/workspaces/FTDSS/Inout/new-toml/Basic.toml", newunit=io, status="old")
    !     call toml_parse(table, io, ferror)
    !     close (io)
    !     if (allocated(ferror)) then
    !         print '(a)', "fError: "//ferror%message
    !         stop 1
    !     end if
    ! end block

    ! call read_data(table, title, spectrum)

    ! if (allocated(title)) then
    !     print '(a)', "Title: '"//title//"'"
    ! end if

    ! print '(*(g0, 1x))', "Entries:", size(spectrum)
    ! if (size(spectrum) > 0) then
    !     print '(*(g0, 1x))', "Spectrum:", spectrum
    ! end if

    stop
    ! call init_omp_config(Solver)
    ! Inout = IO()

    ! !* Input basic data
    ! call Inout%Input_Parameters(Solver)
    ! !* Input initial and boundary condition
    ! call Inout%Input_IC(Solver)
    ! call Inout%Input_BC(Solver)
    ! call Inout%Input_Flags(Solver)

    call Allocate_Solver(Solver)

    !* Input coordinate and vertex data
    ! call Inout%Input_Vertices(Solver)
    ! call Inout%Input_Coodinates(Solver)

    call Initialize_Solver(Solver)

    ! print*, Solver%Heat%Variables%TFlux%x(1:10)
    ! call Calc_Area(Solver%N)
    ! call Calc_Shape(Solver%N)
    ! call Convert_CRS(Solver, CTop)
    ! if (Solver%isHeat) call Duplicate_CRS(CTop, Solver%Heat%LHS_A)
    ! if (Solver%isWater) call Duplicate_CRS(CTop, Solver%Water%LHS_A)
    ! call Init_Assemble(CTop)

    ! call Inout%Input_Observation(Solver)
!     if (Solver%Obs%nObsType == 2) call Set_Obs_Coo(Solver)

!     call Fix_InitialCondition(Solver)
!     call Update_Parameters_Water(Solver)
!     call Update_Parameters_Heat(Solver)

!     ! if (Solver%Flags%isOutputAll) call Inout%Output_All(Solver, 0)
!     ! call Inout%Output_Observation(Solver, 0.0d0)

!     ptst => Solver%Time%tst
!     pdt => Solver%Time%dt
!     podt => Solver%Time%odt

!     piter => Solver%Iter%iter
!     ptiter => Solver%Iter%titer
!     piNL => Solver%Iter%iNL

!     ! Construct BiCGSTAB Solver object
!     if (Solver%isHeat) ILEQ = ILS(Solver, CTop)
!     if (Solver%isWater) DLEQ = DLS(Solver)

!     !* Main loop section
!     do while (.true.)
!     if (Solver%Flags%isOutput) then
! #ifdef _OPENMP
!         its = omp_get_wtime()
!         Solver%Flags%isOutput = .false.
! #endif
!     end if
!     otst = ptst
!     podt = pdt
!     ptst = ptst + pdt
!     outtst = ptst * Solver%Time%tconv
!     ig = 1
!     Local_loop: do while (.true.)
!         piNL = 1
!         if (Solver%isHeat) then
!         non_linear_heat: do while (piNL <= Solver%Iter%iNLmax)
!             if (ig == 1 .and. piNL == 1) call Init_Copy_Temperature(Solver)

!             call Update_Parameters_Heat(Solver)

!             if (has_nan(Solver%Water%Variables%wFlux%x(:)) .or. has_nan(Solver%Water%Variables%wFlux%x(:))) then
!                 write (*, '(es13.4,a)'), outtst, " Day: Water flux has NaN."
!                 stop
!             end if

!             call Assemble_GM_Heat(Solver)
!             call Fix_BoundaryConditions(Solver, Temperature)

!             call ILEQ%BiCGStab(Solver, Solver%Heat%LHS_A, Solver%Heat%Rhs, Solver%T%new, ierr)
!             call ILEQ%Chkerr(ierr, outtst)

!             if (Solver%Flags%isTRM) then
!                 call TRMethod(Solver)
!                 Solver%T%pre(:) = Solver%T%new(:)
!                 Solver%Si%pre(:) = Solver%Si%new(:)
!                 call Update_theta(Solver)
!                 exit non_linear_heat
!             else if (Solver%Flags%isGCC .or. Solver%Flags%isPower) then
!                 Solver%T%pre(:) = Solver%T%new(:)
!                 if (piNL == 1) call Update_Phase_Revise(Solver)
!                 call Update_Si(Solver)
!                 call Update_theta(Solver)
!                 if (piNL >= 2) exit non_linear_heat
!             end if

!             piNL = piNL + 1
!         end do non_linear_heat
!         end if
!         if (has_nan(Solver%T%pre(:))) then
!             write (*, '(es13.4,a)'), outtst, " Day: Temperature has NaN."
!             stop
!         end if
!         if (ig >= 2) exit Local_loop

!         if (Solver%isWater) then
!             piNL = 1
!             non_linear_water: do while (piNL <= Solver%Iter%titer)
!                 if (ig == 1 .and. piNL == 1) call Init_Copy_Pressure(Solver)

!                 call Update_Parameters_Water(Solver)
!                 call Assemble_GM_Water(Solver)
!                 call Fix_BoundaryConditions(Solver, Pressure)

!                 call DLEQ%LU(Solver%Water%RA, Solver%Water%Rhs, Solver%P%pre)

!                 if (has_nan(Solver%P%pre(:))) then
!                     write (*, '(es13.4,a)'), outtst, " Day: Pressure has NaN."
!                     stop
!                 end if
!                 call Update_Gradient(Solver, Solver%P%pre(:), Solver%Water%Variables%hGrad)
!                 if (has_nan(Solver%Water%Variables%hGrad%x(:)) .or. has_nan(Solver%Water%Variables%hGrad%x(:))) then
!                     write (*, '(es13.4,a)'), outtst, " Day: Water gradient has NaN."
!                     stop
!                 end if

!                 if (piNL == 1) exit non_linear_water

!                 piNL = piNL + 1

!             end do non_linear_water
!         end if
!         if (.not. Solver%isWater) exit Local_loop
!         exit Local_loop
!         Solver%T%pre(:) = Solver%T%old(:)
!         Solver%Si%pre(:) = Solver%Si%old(:)

!         ig = ig + 1

!     end do Local_loop

!     if (ptst - otst > epsilon(otst)) then
!         call Update_Gradient(Solver, Solver%T%pre(:), Solver%Heat%Variables%Tgrad)
!         ! call Inout%Output_Observation(Solver, outtst)
!         piNL = 1
!     end if

!     if (ptst >= Solver%Time%cinterval * piter) then
! #ifdef _OPENMP
!         pte = omp_get_wtime()
! #endif
!         if (Solver%Flags%isStdOut) write (*, Solver%fmt_Stdout), "Progress:", piter, "/", Solver%Iter%itermax, "; Elapsed time:", pte - its, "/", pte - pts, " sec"
!         ! if (Solver%Flags%isOutputAll) call Inout%Output_All(Solver, piter)
!         piter = piter + 1
!         Solver%Flags%isOutput = .true.
!     end if
!     if (ptst >= Solver%Time%te) exit
!     ptiter = ptiter + 1

!     end do
! #ifdef _OPENMP
!     pte = omp_get_wtime()
! #endif
!     if (Solver%Flags%isStdOut) write (*, '(a)') ""
!     if (Solver%Flags%isStdOut) write (*, '(a,i0)') "Total iteration: ", piter - 1
!     if (Solver%Flags%isStdOut) write (*, '(a)') ""
!     if (Solver%Flags%isStdOut) write (*, '(a)') "-----------------------------------------------------------------"
! ! if (Solver%isHeat .and. Solver%isWater) then
! !         if (Solver%Flags%isStdOut) write(*,'(4a,f17.10,a)') Tsolver_name, ":",Psolver_name, ": Total time         = ", pte - pts,     " s"
! !         if (Solver%Flags%isStdOut) write(*,'(4a,f17.10,a)') Tsolver_name, ":",Psolver_name, "-Lis: Elapsed time   = ", lis_sum_time,  " s"
! !         if (Solver%Flags%isStdOut) write(*,'(4a,f17.10,a)') Tsolver_name, ":",Psolver_name, "-Lis: Linear solver  = ", lis_sum_itime, " s"
! !         if (Solver%Flags%isStdOut) write(*,'(4a,f17.10,a)') Tsolver_name, ":",Psolver_name, "-Lis: Preconditioner = ", lis_sum_ptime, " s"
! ! else
! ! if (Solver%isHeat) then
! !         if (Solver%Flags%isStdOut) write(*,'(2a,f17.10,a)') Tsolver_name, ": Total time         = ", pte - pts,     " s"
! !         if (Solver%Flags%isStdOut) write(*,'(2a,f17.10,a)') Tsolver_name, "-Lis: Elapsed time   = ", lis_sum_time,  " s"
! !         if (Solver%Flags%isStdOut) write(*,'(2a,f17.10,a)') Tsolver_name, "-Lis: Linear solver  = ", lis_sum_itime, " s"
! !         if (Solver%Flags%isStdOut) write(*,'(2a,f17.10,a)') Tsolver_name, "-Lis: Preconditioner = ", lis_sum_ptime, " s"
! ! else if (Solver%isWater) then
! !         if (Solver%Flags%isStdOut) write(*,'(2a,f17.10,a)') Psolver_name, ": Total time         = ", pte - pts,     " s"
! !         if (Solver%Flags%isStdOut) write(*,'(2a,f17.10,a)') Psolver_name, "-Lis: Elapsed time   = ", lis_sum_time,  " s"
! !         if (Solver%Flags%isStdOut) write(*,'(2a,f17.10,a)') Psolver_name, "-Lis: Linear solver  = ", lis_sum_itime, " s"
! !         if (Solver%Flags%isStdOut) write(*,'(2a,f17.10,a)') Psolver_name, "-Lis: Preconditioner = ", lis_sum_ptime, " s"
! ! end if
!     if (Solver%Flags%isStdOut) write (*, '(a)') "-----------------------------------------------------------------"
!     if (Solver%Flags%isStdOut) write (*, '(a)') ""
!     if (Solver%Flags%isStdOut) write (*, '(a)') "End of programsd"

! #ifdef _MPI
!     call MPI_Finalize(ierr)
! #endif
! ! end program main

! ! program test

! ! contains

end program test

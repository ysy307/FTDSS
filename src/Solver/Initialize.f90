module Solver_Initialize
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    use :: allocate
    use :: Calculate_LatentHeat, only:Find_Ca_max
    implicit none
    private

    public :: Initialize_Solver

contains

    subroutine Initialize_Solver(Solver)
        implicit none
        type(SolverInfo), intent(inout) :: Solver
        integer(int32) :: i

        call Allocate_Pointer(Solver%Time%tst)
        call Allocate_Pointer(Solver%Time%dt)
        call Allocate_Pointer(Solver%Time%odt)
        ! Time settings converting to seconds
        Solver%Time%ts = 0.0d0
        Solver%Time%tst = 0.0d0
        Solver%Time%te = Solver%Time%cTime * Convert_TimeUnit(Solver%Time, 1)
        Solver%Time%dt = Solver%Time%cdt * Convert_TimeUnit(Solver%Time, 2)
        Solver%Time%odt = Solver%Time%dt
        Solver%Time%max_dt = 1.0d-8
        Solver%Time%min_dt = 300.0d0
        Solver%Time%tconv = 1.0d0 / Convert_TimeUnit(Solver%Time, 1)

        call Allocate_Pointer(Solver%Iter%iter)
        call Allocate_Pointer(Solver%Iter%titer)
        call Allocate_Pointer(Solver%Iter%iNL)
        ! Iteration settings
        Solver%Iter%iter = 1
        Solver%Iter%itermax = nint(Solver%Time%te / Solver%Time%cinterval)
        Solver%Iter%titer = 1
        Solver%Iter%iNL = 1
        Solver%Iter%iNLmax = 50
        Solver%Iter%iNI = 1
        Solver%Iter%digits_itermax = int(log10(dble(Solver%Iter%itermax))) + 1

        ! Output format settings
        write (Solver%fmt_Stdout, '(a,i0,a,i0,a)'), &
            '(a,i', Solver%Iter%digits_itermax, ',a,i', Solver%Iter%digits_itermax, ',a,f9.4,a,f11.4,a)'
        write (Solver%fmt_Fileout, '(a,i0,a,i0,a)'), &
            '(2a,i', Solver%Iter%digits_itermax, '.', Solver%Iter%digits_itermax, ',a)'

        Solver%Heat%Latent%Cp_unf = Solver%Heat%Constants%HeatCapacity%soil * (1.0d0 - Solver%Heat%Constants%Porosity) &
                                    + Solver%Heat%Constants%HeatCapacity%water * Solver%Heat%Constants%Porosity

        ! Find the maximum value of Ca
        call Find_Ca_max(Solver%Heat)

        if (Solver%BC%numEdges > 0) then
            do i = 1, Solver%BC%numEdges
                ! print*,(Solver%N%vCood%x(Solver%BC%Heat%Edges%y(i)) - Solver%N%vCood%x(Solver%BC%Heat%Edges%x(i))),(Solver%N%vCood%y(Solver%BC%Heat%Edges%y(i)) - Solver%N%vCood%y(Solver%BC%Heat%Edges%x(i)))
                Solver%BC%Heat%EdgesDistance(i) = sqrt((Solver%N%vCood%x(Solver%BC%Heat%Edges%y(i)) - Solver%N%vCood%x(Solver%BC%Heat%Edges%x(i)))**2.0d0 + (Solver%N%vCood%y(Solver%BC%Heat%Edges%y(i)) - Solver%N%vCood%y(Solver%BC%Heat%Edges%x(i)))**2.0d0)
            end do
        end if

    end subroutine Initialize_Solver

    function Convert_TimeUnit(Time, num) result(conv_time)
        implicit none
        type(TimeInfo), intent(in) :: Time
        integer(int32), intent(in) :: num
        real(real64) :: conv_time

        if (Time%tUnit(num:num) == "1") then
            conv_time = 1.0d0
        else if (Time%tUnit(num:num) == "2") then
            conv_time = 60.d0
        else if (Time%tUnit(num:num) == "3") then
            conv_time = 60.d0 * 60.d0
        else if (Time%tUnit(num:num) == "4") then
            conv_time = 60.d0 * 60.d0 * 24.0d0
        else if (Time%tUnit(num:num) == "5") then
            conv_time = 60.d0 * 60.d0 * 24.0d0 * 30.d0
        else if (Time%tUnit(num:num) == "6") then
            conv_time = 60.d0 * 60.d0 * 24.0d0 * 365.0d0
        end if
    end function Convert_TimeUnit
end module Solver_Initialize

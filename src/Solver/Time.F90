module Solver_Time
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Inout_Input
    use :: Core_Allocate, only:Allocate_Array
    implicit none

    type :: Type_Time
        real(real64) :: start_time
        real(real64) :: end_time
        real(real64) :: time
        real(real64) :: time_old
        real(real64) :: dt
        real(real64), allocatable :: dt_old(:)
        real(real64) :: dt_max
        real(real64) :: dt_min

        character(10) :: start_Rdate, start_Rtime, start_Rzone
        character(10) :: end_Rdate, end_Rtime, end_Rzone

    contains
        procedure, public, pass(self) :: Get_RealTime
    end type Type_Time

    interface Type_Time
        module procedure Time_Construct
    end interface Type_Time

contains

    function Time_Construct(Structure_Input) result(time)
        implicit none
        type(Type_Input), intent(in) :: Structure_Input
        type(Type_Time) :: time

        select case (Structure_Input%Basic%Calculation_TimeUnit)
        case ("Second")
            time%dt = Structure_Input%Basic%Calculation_Step
            time%dt_max = Structure_Input%Basic%Calculation_StepMaximum
            time%dt_min = Structure_Input%Basic%Calculation_StepMinimum
        case ("Minute")
            time%dt = Structure_Input%Basic%Calculation_Step * 60.0d0
            time%dt_max = Structure_Input%Basic%Calculation_StepMaximum * 60.0d0
            time%dt_min = Structure_Input%Basic%Calculation_StepMinimum * 60.0d0
        case ("Hour")
            time%dt = Structure_Input%Basic%Calculation_Step * 3600.0d0
            time%dt_max = Structure_Input%Basic%Calculation_StepMaximum * 3600.0d0
            time%dt_min = Structure_Input%Basic%Calculation_StepMinimum * 3600.0d0
        case ("Day")
            time%dt = Structure_Input%Basic%Calculation_Step * 86400.0d0
            time%dt_max = Structure_Input%Basic%Calculation_StepMaximum * 86400.0d0
            time%dt_min = Structure_Input%Basic%Calculation_StepMinimum * 86400.0d0
        case ("Year")
            time%dt = Structure_Input%Basic%Calculation_Step * 31557600.0d0
            time%dt_max = Structure_Input%Basic%Calculation_StepMaximum * 31557600.0d0
            time%dt_min = Structure_Input%Basic%Calculation_StepMinimum * 31557600.0d0
        case default
            write (*, *) "Error: Unknown time unit"
            stop
        end select

        select case (Structure_Input%Basic%Input_TimeUnit)
        case ("Second")
            time%start_time = Structure_Input%Basic%StartCalculation
            time%end_time = Structure_Input%Basic%EndCalculation
        case ("Minute")
            time%start_time = Structure_Input%Basic%StartCalculation * 60.0d0
            time%end_time = Structure_Input%Basic%EndCalculation * 60.0d0
        case ("Hour")
            time%start_time = Structure_Input%Basic%StartCalculation * 3600.0d0
            time%end_time = Structure_Input%Basic%EndCalculation * 3600.0d0
        case ("Day")
            time%start_time = Structure_Input%Basic%StartCalculation * 86400.0d0
            time%end_time = Structure_Input%Basic%EndCalculation * 86400.0d0
        case ("Year")
            time%start_time = Structure_Input%Basic%StartCalculation * 31557600.0d0
            time%end_time = Structure_Input%Basic%EndCalculation * 31557600.0d0
        case default
            write (*, *) "Error: Unknown time unit"
            stop
        end select

        call Allocate_Array(time%dt_old, Structure_Input%Basic%Order)
    end function Time_Construct

    subroutine Get_RealTime(self, date, time, zone)
        implicit none
        class(Type_Time), intent(inout) :: self
        character(10) :: date, time, zone

        call date_and_time(date=date, time=time, zone=zone)

    end subroutine Get_RealTime

end module Solver_Time

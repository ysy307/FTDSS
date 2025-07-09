module control_time
    use, intrinsic :: iso_fortran_env, only: int32, real64, int64
!$  use omp_lib
    use :: module_core, only:allocate_array
    use :: Inout_Input

    implicit none
    private

    public :: type_time

    type :: type_time_Record
        character(len=10) :: label
        character(len=10) :: date
        character(len=10) :: time
        character(len=10) :: zone
    end type type_time_Record

    type :: type_profiler_section
        character(len=20) :: label
        real(real64) :: total_time = 0.0d0
#ifdef _OPENMP
        real(real64) :: start_time_wtime = 0.0d0
#else
        integer(kind=int64) :: start_tick = 0
#endif
    end type type_profiler_section

    type :: type_time
        real(real64) :: start_time, end_time, time, time_old, dt
        real(real64), allocatable :: dt_old(:)
        real(real64) :: dt_max, dt_min
        type(type_time_Record) :: start, end
        type(type_profiler_section), allocatable :: sections(:)
#ifndef _OPENMP
        integer(int32) :: tick_rate = 0
#endif
    contains
        procedure, public, pass(self) :: Record => Record_Timestamp
        procedure, public, pass(self) :: Profile_Start => Profile_Start_Timer
        procedure, public, pass(self) :: Profile_Stop => Profile_Stop_Timer
    end type type_time

    interface type_time
        module procedure construct_type_time
    end interface type_time

contains

    function construct_type_time(Structure_Input, profiler_sections) result(time)
        type(type_Input), intent(in) :: Structure_Input
        character(len=*), intent(in), optional :: profiler_sections(:)
        type(type_time) :: time
        integer :: i
        integer :: dummy

        select case (trim(Structure_Input%Basic%Calculation_TimeUnit))
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
            write (*, *) "Error: Unknown time unit in Calculation_TimeUnit"
            stop
        end select

        select case (trim(Structure_Input%Basic%Input_TimeUnit))
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
            write (*, *) "Error: Unknown time unit in Input_TimeUnit"
            stop
        end select

        call Allocate_Array(time%dt_old, Structure_Input%Basic%Order)

        if (present(profiler_sections)) then
            if (size(profiler_sections) > 0) then
#ifndef _OPENMP
                call system_clock(dummy, time%tick_rate)
#endif
                allocate (time%sections(size(profiler_sections)))
                do i = 1, size(profiler_sections)
                    time%sections(i)%label = trim(profiler_sections(i))
                end do
            end if
        end if
    end function construct_type_time

    subroutine Record_Timestamp(self, label)
        class(type_time), intent(inout) :: self
        character(len=*), intent(in) :: label

        select case (trim(label))
        case ("Start")
            call date_and_time(date=self%start%date, time=self%start%time, zone=self%start%zone)
            self%start%label = label
        case ("End")
            call date_and_time(date=self%end%date, time=self%end%time, zone=self%end%zone)
            self%end%label = label
        case default
            write (*, *) "Error: Unknown time label"
            stop
        end select
    end subroutine Record_Timestamp

    subroutine Profile_Start_Timer(self, label)
        class(type_time), intent(inout) :: self
        character(len=*), intent(in) :: label
        integer :: i
        do i = 1, size(self%sections)
            if (trim(self%sections(i)%label) == trim(label)) then
#ifdef _OPENMP
                self%sections(i)%start_time_wtime = omp_get_wtime()
#else
                call system_clock(count=self%sections(i)%start_tick)
#endif
                return
            end if
        end do
        write (*, '(A,A,A)') "Error: Profiling section '", trim(label), "' not found. Timer not started."
    end subroutine Profile_Start_Timer

    subroutine Profile_Stop_Timer(self, label)
        class(type_time), intent(inout) :: self
        character(len=*), intent(in) :: label
        integer :: i
        real(real64) :: duration
#ifdef _OPENMP
        real(real64) :: end_time_wtime
        end_time_wtime = omp_get_wtime()
#else
        integer(kind=int64) :: end_tick
        call system_clock(count=end_tick)
#endif
        do i = 1, size(self%sections)
            if (trim(self%sections(i)%label) == trim(label)) then
#ifdef _OPENMP
                duration = end_time_wtime - self%sections(i)%start_time_wtime
#else
                if (self%tick_rate > 0) then
                    duration = real(end_tick - self%sections(i)%start_tick, real64) / real(self%tick_rate, real64)
                else
                    duration = 0.0d0
                end if
#endif
                self%sections(i)%total_time = self%sections(i)%total_time + duration
                return
            end if
        end do
        write (*, '(A,A,A)') "Error: Profiling section '", trim(label), "' not found. Timer not stopped."
    end subroutine Profile_Stop_Timer

end module control_time

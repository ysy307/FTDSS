module control_time
    use, intrinsic :: iso_fortran_env, only: int32, real64, int64
!$  use omp_lib
    use :: module_core, only:allocate_array
    use :: module_input, only:type_Input

    implicit none
    private

    public :: type_time

    type :: type_time_record
        character(len=10) :: label
        character(len=10) :: date
        character(len=10) :: time
        character(len=10) :: zone
    end type type_time_record

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
        real(real64) :: start_time
        real(real64) :: end_time
        real(real64) :: time
        real(real64) :: time_old
        real(real64) :: dt
        real(real64), allocatable :: dt_old(:)
        real(real64) :: dt_min
        real(real64) :: dt_max
        type(type_time_record) :: start
        type(type_time_record) :: end
        type(type_profiler_section), allocatable :: sections(:)
#ifndef _OPENMP
        integer(int32) :: tick_rate = 0
#endif
    contains
        procedure, public, pass(self) :: initialize    => initialize_type_time !&
        procedure, public, pass(self) :: record        => record_timestamp !&
        procedure, public, pass(self) :: profile_start => profile_start_timer !&
        procedure, public, pass(self) :: profile_stop  => profile_stop_timer !&
    end type type_time

contains

    subroutine initialize_type_time(self, input, profiler_sections)
        implicit none
        class(type_time), intent(inout) :: self
        type(type_Input), intent(in), optional :: input
        character(*), intent(in), optional :: profiler_sections(:)

        integer(int32) :: i
        integer(int32) :: dummy

        if (present(input)) then
            select case (trim(input%conditions%time_control%time_stepping%unit))
            case ("second")
                self%dt     = input%conditions%time_control%time_stepping%initial_step !&
                self%dt_max = input%conditions%time_control%time_stepping%max_step !&
                self%dt_min = input%conditions%time_control%time_stepping%min_step !&
            case ("minute")
                self%dt     = input%conditions%time_control%time_stepping%initial_step * 60.0d0 !&
                self%dt_max = input%conditions%time_control%time_stepping%max_step * 60.0d0 !&
                self%dt_min = input%conditions%time_control%time_stepping%min_step * 60.0d0 !&
            case ("hour")
                self%dt     = input%conditions%time_control%time_stepping%initial_step * 3600.0d0 !&
                self%dt_max = input%conditions%time_control%time_stepping%max_step * 3600.0d0 !&
                self%dt_min = input%conditions%time_control%time_stepping%min_step * 3600.0d0 !&
            case ("day")
                self%dt     = input%conditions%time_control%time_stepping%initial_step * 86400.0d0 !&
                self%dt_max = input%conditions%time_control%time_stepping%max_step * 86400.0d0 !&
                self%dt_min = input%conditions%time_control%time_stepping%min_step * 86400.0d0 !&
            case ("year")
                self%dt     = input%conditions%time_control%time_stepping%initial_step * 31557600.0d0 !&
                self%dt_max = input%conditions%time_control%time_stepping%max_step * 31557600.0d0 !&
                self%dt_min = input%conditions%time_control%time_stepping%min_step * 31557600.0d0 !&
            case default
                write (*, *) "Error: Unknown time unit in Calculation_TimeUnit"
                stop
            end select

            select case (trim(input%conditions%time_control%simulation_period%unit))
            case ("second")
                self%start_time = input%conditions%time_control%simulation_period%start !&
                self%end_time   = input%conditions%time_control%simulation_period%end !&
            case ("minute")
                self%start_time = input%conditions%time_control%simulation_period%start * 60.0d0 !&
                self%end_time   = input%conditions%time_control%simulation_period%end * 60.0d0 !&
            case ("hour")
                self%start_time = input%conditions%time_control%simulation_period%start * 3600.0d0 !&
                self%end_time   = input%conditions%time_control%simulation_period%end * 3600.0d0 !&
            case ("day")
                self%start_time = input%conditions%time_control%simulation_period%start * 86400.0d0 !&
                self%end_time   = input%conditions%time_control%simulation_period%end * 86400.0d0 !&
            case ("year")
                self%start_time = input%conditions%time_control%simulation_period%start * 31557600.0d0 !&
                self%end_time   = input%conditions%time_control%simulation_period%end * 31557600.0d0 !&
            case default
                write (*, *) "Error: Unknown time unit in Input_TimeUnit"
                stop
            end select

            call Allocate_Array(self%dt_old, input%basic%solver_settings%bdf_order)
        end if

        if (present(profiler_sections)) then
            if (size(profiler_sections) > 0) then
#ifndef _OPENMP
                call system_clock(dummy, self%tick_rate)
#endif
                allocate (self%sections(size(profiler_sections)))
                do i = 1, size(profiler_sections)
                    self%sections(i)%label = trim(profiler_sections(i))
                end do
            end if
        end if
    end subroutine initialize_type_time

    subroutine record_timestamp(self, label)
        implicit none
        class(type_time), intent(inout) :: self
        character(*), intent(in) :: label

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
    end subroutine record_timestamp

    subroutine profile_start_timer(self, label)
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
        write (*, '(3a)') "Error: Profiling section '", trim(label), "' not found. Timer not started."
    end subroutine profile_start_timer

    subroutine profile_stop_timer(self, label)
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
        write (*, '(3a)') "Error: Profiling section '", trim(label), "' not found. Timer not stopped."
    end subroutine profile_stop_timer

end module control_time

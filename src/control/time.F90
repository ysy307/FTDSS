module control_time
    use, intrinsic :: iso_fortran_env, only: int32, real64, int64
!$  use omp_lib
    use :: module_core, only:allocate_array, error_message
    use :: module_input, only:type_input

    implicit none
    private

    public :: type_time

    type :: type_time_record
        character(10) :: label
        character(10) :: date
        character(10) :: time
        character(10) :: zone
    end type type_time_record

    type :: type_profiler_section
        character(20) :: label
        real(real64) :: total_time = 0.0d0
        real(real64) :: start_time = 0.0d0
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
        real(real64) :: time_conversion = 1.0d0
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
        procedure, public, pass(self) :: get_record
        procedure, public, pass(self) :: get_time
    end type type_time

contains

    subroutine initialize_type_time(self, input, profiler_sections)
        implicit none
        class(type_time), intent(inout) :: self
        type(type_input), intent(in), optional :: input
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
                self%dt_max = input%conditions%time_control%time_stepping%max_step     * 60.0d0 !&
                self%dt_min = input%conditions%time_control%time_stepping%min_step     * 60.0d0 !&
            case ("hour")
                self%dt     = input%conditions%time_control%time_stepping%initial_step * 3600.0d0 !&
                self%dt_max = input%conditions%time_control%time_stepping%max_step     * 3600.0d0 !&
                self%dt_min = input%conditions%time_control%time_stepping%min_step     * 3600.0d0 !&
            case ("day")
                self%dt     = input%conditions%time_control%time_stepping%initial_step * 86400.0d0 !&
                self%dt_max = input%conditions%time_control%time_stepping%max_step     * 86400.0d0 !&
                self%dt_min = input%conditions%time_control%time_stepping%min_step     * 86400.0d0 !&
            case ("year")
                self%dt     = input%conditions%time_control%time_stepping%initial_step * 31557600.0d0 !&
                self%dt_max = input%conditions%time_control%time_stepping%max_step     * 31557600.0d0 !&
                self%dt_min = input%conditions%time_control%time_stepping%min_step     * 31557600.0d0 !&
            case default
                call error_message(981, c_opt="calculation time unit")
            end select

            select case (trim(input%conditions%time_control%simulation_period%unit))
            case ("second")
                self%start_time = input%conditions%time_control%simulation_period%start !&
                self%end_time   = input%conditions%time_control%simulation_period%end !&
            case ("minute")
                self%start_time = input%conditions%time_control%simulation_period%start * 60.0d0 !&
                self%end_time   = input%conditions%time_control%simulation_period%end   * 60.0d0 !&
            case ("hour")
                self%start_time = input%conditions%time_control%simulation_period%start * 3600.0d0 !&
                self%end_time   = input%conditions%time_control%simulation_period%end   * 3600.0d0 !&
            case ("day")
                self%start_time = input%conditions%time_control%simulation_period%start * 86400.0d0 !&
                self%end_time   = input%conditions%time_control%simulation_period%end   * 86400.0d0 !&
            case ("year")
                self%start_time = input%conditions%time_control%simulation_period%start * 31557600.0d0 !&
                self%end_time   = input%conditions%time_control%simulation_period%end   * 31557600.0d0 !&
            case default
                call error_message(981, c_opt="simulation period time unit")
            end select

            call Allocate_Array(self%dt_old, input%basic%solver_settings%bdf_order)
        end if

        if (present(profiler_sections)) then
            if (size(profiler_sections) > 0) then
                allocate (self%sections(size(profiler_sections)))
                do i = 1, size(profiler_sections)
                    self%sections(i)%label = trim(profiler_sections(i))
                end do
            end if
        end if

        select case (trim(input%output_settings%field_output%output_interval_unit))
        case ("second")
            select case (trim(input%conditions%time_control%simulation_period%unit))
            case ("second")
                self%time_conversion = 1.0d0
            case ("minute")
                self%time_conversion = 1.0d0 / 60.0d0
            case ("hour")
                self%time_conversion = 1.0d0 / 3600.0d0
            case ("day")
                self%time_conversion = 1.0d0 / 86400.0d0
            case ("year")
                self%time_conversion = 1.0d0 / 31557600.0d0
            case default
                call error_message(981, c_opt="output interval time unit")
            end select
        case ("minute")
            select case (trim(input%conditions%time_control%simulation_period%unit))
            case ("second")
                self%time_conversion = 60.0d0
            case ("minute")
                self%time_conversion = 1.0d0
            case ("hour")
                self%time_conversion = 1.0d0 / 60.0d0
            case ("day")
                self%time_conversion = 1.0d0 / 1440.0d0
            case ("year")
                self%time_conversion = 1.0d0 / 525600.0d0
            case default
                call error_message(981, c_opt="output interval time unit")
            end select
        case ("hour")
            select case (trim(input%conditions%time_control%simulation_period%unit))
            case ("second")
                self%time_conversion = 3600.0d0
            case ("minute")
                self%time_conversion = 60.0d0
            case ("hour")
                self%time_conversion = 1.0d0
            case ("day")
                self%time_conversion = 1.0d0 / 24.0d0
            case ("year")
                self%time_conversion = 1.0d0 / 8760.0d0
            case default
                call error_message(981, c_opt="output interval time unit")
            end select
        case ("day")
            select case (trim(input%conditions%time_control%simulation_period%unit))
            case ("second")
                self%time_conversion = 86400.0d0
            case ("minute")
                self%time_conversion = 1440.0d0
            case ("hour")
                self%time_conversion = 24.0d0
            case ("day")
                self%time_conversion = 1.0d0
            case ("year")
                self%time_conversion = 1.0d0 / 365.0d0
            case default
                call error_message(981, c_opt="output interval time unit")
            end select
        case ("year")
            select case (trim(input%conditions%time_control%simulation_period%unit))
            case ("second")
                self%time_conversion = 31557600.0d0
            case ("minute")
                self%time_conversion = 525600.0d0
            case ("hour")
                self%time_conversion = 8760.0d0
            case ("day")
                self%time_conversion = 365.0d0
            case ("year")
                self%time_conversion = 1.0d0
            case default
                call error_message(981, c_opt="output interval time unit")
            end select
        case default
            call error_message(981, c_opt="output interval time unit")
        end select
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
            call error_message(982, c_opt=label)
        end select
    end subroutine record_timestamp

    subroutine profile_start_timer(self, label)
        implicit none
        class(type_time), intent(inout) :: self
        character(len=*), intent(in) :: label
        integer(int32) :: i

        do i = 1, size(self%sections)
            if (trim(self%sections(i)%label) == trim(label)) then
                self%sections(i)%start_time = get_current_time()
                return
            end if
        end do

        call error_message(982, c_opt=label)
    end subroutine profile_start_timer

    subroutine profile_stop_timer(self, label)
        implicit none
        class(type_time), intent(inout) :: self
        character(*), intent(in) :: label
        integer(int32) :: i
        real(real64) :: end_time

        end_time = get_current_time()

        do i = 1, size(self%sections)
            if (trim(self%sections(i)%label) == trim(label)) then
                self%sections(i)%total_time = self%sections(i)%total_time + &
                                              (end_time - self%sections(i)%start_time)
                return
            end if
        end do

        call error_message(982, c_opt=label)
    end subroutine profile_stop_timer

    function get_current_time() result(current_time)
        implicit none
        real(real64) :: current_time
#ifdef _OPENMP
        current_time = omp_get_wtime()
#else
        integer(int32) :: count
        integer(int32) :: rate
        call system_clock(count=count, count_rate=rate)
        current_time = real(count, kind=real64) / real(rate, kind=real64)
#endif
    end function get_current_time

    function get_record(self, label) result(record)
        implicit none
        class(type_time), intent(in) :: self
        character(*), intent(in) :: label
        character(:), allocatable :: record

        select case (trim(label))
        case ("Start")
            record = trim(self%start%label)//" Time : "// &
                     self%start%date(1:4)//"-"//self%start%date(5:6)//"-"//self%start%date(7:8)//"T"// &
                     self%start%time(1:2)//":"//self%start%time(3:4)//":"//self%start%time(5:6)//trim(self%start%zone)
        case ("End")
            record = trim(self%end%label)//" Time   : "// &
                     self%end%date(1:4)//"-"//self%end%date(5:6)//"-"//self%end%date(7:8)//"T"// &
                     self%end%time(1:2)//":"//self%end%time(3:4)//":"//self%end%time(5:6)//trim(self%end%zone)
        case default
            call error_message(982, c_opt=label)
        end select

    end function get_record

    function get_time(self) result(time)
        implicit none
        class(type_time), intent(in) :: self
        real(real64) :: time

        time = self%time * self%time_conversion

    end function get_time

end module control_time

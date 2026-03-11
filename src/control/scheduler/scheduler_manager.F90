module control_scheduler
    use, intrinsic :: iso_fortran_env
    use :: stdlib_optval, only:optval
    use :: stdlib_strings, only:strip
    use :: module_core
    use :: control_time
    implicit none
    private

    public :: type_scheduler_manager

    type :: type_scheduler_manager
        logical, private :: active = .false.
        real(real64), private :: interval_seconds = 0.0d0 ! Always stored in seconds
        real(real64), private :: next_output_seconds = 0.0d0 ! Always stored in seconds
        integer(int32), private :: current_step = 0
        type(type_constant_value), private :: output_time_unit ! For output time conversion
    contains
        procedure, pass(self), public :: initialize => initialize_manager
        procedure, pass(self), public :: is_output_triggered => check_output_timing
        procedure, pass(self), public :: update => update_state
        procedure, pass(self), public :: get_step
        procedure, pass(self), public :: get_output_time => get_output_time_scheduler_manager
        procedure, pass(self), public :: is_active => is_active_scheduler_manager
        procedure, pass(self), public :: get_next_time
        procedure, pass(self), public :: get_next_target_time
    end type type_scheduler_manager

contains

    subroutine initialize_manager(self, config, current_time_seconds)
        implicit none
        class(type_scheduler_manager), intent(inout) :: self
        type(type_config_output_manager), intent(in) :: config
        real(real64), intent(in) :: current_time_seconds

        self%active = .false.
        self%interval_seconds = 0.0d0
        self%next_output_seconds = 0.0d0
        self%current_step = 0
        self%output_time_unit = type_constant_value("", "", -1, "", 0.0d0)

        if (config%file_format%ID <= 0) then
            return
        end if

        if (config%file_format == FILE_FORMATS%NONE) then
            return
        end if

        self%active = .true.

        ! 1. Convert interval to seconds and store
        self%interval_seconds = config%interval_val * config%interval_unit%value

        ! 2. Store output unit object
        self%output_time_unit = config%output_unit

        ! 3. Set next output time in seconds
        self%next_output_seconds = current_time_seconds

        self%current_step = 0
    end subroutine initialize_manager

    ! ----------------------------------------------------------------------
    ! Check: compare current_time (seconds) against scheduled output time
    ! ----------------------------------------------------------------------
    pure function check_output_timing(self, current_time_seconds) result(is_ready)
        implicit none
        class(type_scheduler_manager), intent(in) :: self
        real(real64), intent(in) :: current_time_seconds
        logical :: is_ready
        real(real64), parameter :: tolerance = 1.0d-9

        if (.not. self%active) then
            is_ready = .false.
            return
        end if

        if (current_time_seconds >= self%next_output_seconds - tolerance) then
            is_ready = .true.
        else
            is_ready = .false.
        end if
    end function check_output_timing

    ! ----------------------------------------------------------------------
    ! Update: advance next output time (seconds)
    ! ----------------------------------------------------------------------
    subroutine update_state(self, current_time_seconds)
        implicit none
        class(type_scheduler_manager), intent(inout) :: self
        real(real64), intent(in) :: current_time_seconds
        real(real64), parameter :: tolerance = 1.0d-9
        real(real64) :: diff, steps_to_add

        if (.not. self%active) return

        if (self%next_output_seconds <= current_time_seconds + tolerance) then
            diff = (current_time_seconds + tolerance) - self%next_output_seconds

            if (self%interval_seconds > 0.0d0) then
                steps_to_add = floor(diff / self%interval_seconds) + 1.0d0
                self%next_output_seconds = self%next_output_seconds + steps_to_add * self%interval_seconds
            end if
        end if

        self%current_step = self%current_step + 1
    end subroutine update_state

    pure subroutine get_output_time_scheduler_manager(self, current_time_seconds,converted_time)
        implicit none
        class(type_scheduler_manager), intent(in) :: self
        real(real64), intent(in) :: current_time_seconds
        real(real64), intent(inout) :: converted_time

        if (self%output_time_unit%value > 0.0d0) then
            converted_time = current_time_seconds / self%output_time_unit%value
        else
            converted_time = current_time_seconds
        end if
    end subroutine get_output_time_scheduler_manager

    pure subroutine get_step(self, step)
        implicit none
        class(type_scheduler_manager), intent(in) :: self
        integer(int32), intent(inout) :: step
        step = self%current_step
    end subroutine get_step

    !> Check whether output is active
    pure function is_active_scheduler_manager(self) result(is_active)
        implicit none
        class(type_scheduler_manager), intent(in) :: self
        logical :: is_active

        is_active = self%active
    end function is_active_scheduler_manager

    !> Get the next scheduled output time (seconds)
    pure function get_next_time(self) result(next_output_seconds)
        implicit none
        class(type_scheduler_manager), intent(in) :: self
        real(real64) :: next_output_seconds

        next_output_seconds = self%next_output_seconds
    end function get_next_time

    !> Return the next target time to synchronize with, based on current time
    subroutine get_next_target_time(self, current_time, target_time)
        implicit none
        class(type_scheduler_manager), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(inout) :: target_time

        real(real64), parameter :: tolerance = 1.0d-9

        if (.not. self%active) then
            target_time = 1.0d+30 ! If inactive, return far-future time
            return
        end if

        ! If current time has already reached (or passed) the scheduled output time,
        ! use the next interval as target
        if (current_time >= self%next_output_seconds - tolerance) then
            target_time = self%next_output_seconds + self%interval_seconds
        else
            target_time = self%next_output_seconds
        end if
    end subroutine get_next_target_time

end module control_scheduler

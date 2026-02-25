module control_ats
    use, intrinsic :: iso_fortran_env
    use :: stdlib_optval, only:optval
    use :: module_core
    implicit none
    private

    public :: type_ats

    ! ==========================================================================
    !> Type for Adaptive Time Stepping Strategy
    !> Manages thresholds and scaling factors for automatic time step adjustment.
    ! ==========================================================================
    type :: type_ats
        logical :: active = .false.

        !> [Config] Thresholds for iteration counts
        integer(int32) :: iter_min = 4 ! If iter < min, increase dt
        integer(int32) :: iter_max = 8 ! If iter > max, decrease dt

        !> [Config] Scaling factors
        real(real64) :: scale_up = 1.2d0 ! Success (easy)
        real(real64) :: scale_down = 0.8d0 ! Success (hard)
        real(real64) :: scale_retry = 0.5d0 ! Failure (diverged)

        real(real64) :: safety_factor = 0.9d0
        real(real64) :: max_growth_rate = 2.0d0

        !> [Config] Absolute limits (Copies from time control for easy access) in Seconds
        real(real64) :: dt_min = 1.0d-5
        real(real64) :: dt_max = 1.0d+2
    contains
        procedure, public, pass(self) :: initialize => initialize_type_ats
        procedure, public, pass(self) :: is_active => is_active_ats
        procedure, public, pass(self) :: predict_next_dt
        procedure, public, pass(self) :: calc_retry_dt
    end type type_ats
contains
    ! ==========================================================================
    ! ATS (Adaptive Time Stepping)
    ! ==========================================================================

    !> Initialize ATS settings.
    subroutine initialize_type_ats(self, config_time_ats)
        implicit none
        class(type_ats), intent(inout) :: self
        type(type_config_time_ats), intent(in) :: config_time_ats

        self%active = config_time_ats%active
        self%iter_min = config_time_ats%iter_min
        self%iter_max = config_time_ats%iter_max
        self%scale_up = config_time_ats%scale_up
        self%scale_down = config_time_ats%scale_down
        self%scale_retry = config_time_ats%scale_retry
        self%safety_factor = config_time_ats%safety_factor
        self%max_growth_rate = config_time_ats%max_growth_rate
        self%dt_min = config_time_ats%dt_min
        self%dt_max = config_time_ats%dt_max

    end subroutine initialize_type_ats

    !> Check if ATS is enabled.
    pure function is_active_ats(self) result(is_active)
        implicit none
        class(type_ats), intent(in) :: self
        logical :: is_active

        is_active = self%active
    end function is_active_ats

    !> Calculate the recommended dt for the next step after successful convergence.
    pure subroutine predict_next_dt(self, current_dt, iter_count, next_dt)
        implicit none
        class(type_ats), intent(in) :: self
        real(real64), intent(in) :: current_dt
        integer(int32), intent(in) :: iter_count
        real(real64), intent(inout) :: next_dt

        real(real64) :: dt_temp

        next_dt = current_dt
        if (.not. self%active) return

        if (iter_count <= self%iter_min) then
            ! 1. Predict with scale_up and apply safety factor
            if (self%scale_up * self%safety_factor > 1.0d0) then
                dt_temp = current_dt * self%scale_up * self%safety_factor
            else
                dt_temp = current_dt * self%scale_up
            end if

            ! 2. Apply hard growth cap for BDF stability
            ! Even if scale_up is large, we limit the ratio dt_next/dt_now
            next_dt = min(dt_temp, current_dt * self%max_growth_rate)
        else if (iter_count >= self%iter_max) then
            ! Convergence is hard -> decelerate.
            next_dt = current_dt * self%scale_down
        end if

        ! Limiters
        next_dt = max(self%dt_min, min(next_dt, self%dt_max))
    end subroutine predict_next_dt

    !> Calculate a reduced dt for retry upon divergence.
    pure subroutine calc_retry_dt(self, current_dt, retry_dt)
        implicit none
        class(type_ats), intent(in) :: self
        real(real64), intent(in) :: current_dt
        real(real64), intent(inout) :: retry_dt

        retry_dt = current_dt
        if (.not. self%active) return

        retry_dt = current_dt * self%scale_retry

        ! Clip to minimum allowed dt
        if (retry_dt < self%dt_min) retry_dt = self%dt_min
    end subroutine calc_retry_dt

end module control_ats

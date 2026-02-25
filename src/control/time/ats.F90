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
        type(type_config_time_ats) :: config
    contains
        procedure, public, pass(self) :: initialize => initialize_type_ats
        procedure, public, pass(self) :: is_enabled => is_enabled_ats
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

        call self%config%copy(config_time_ats)

    end subroutine initialize_type_ats

    !> Check if ATS is enabled.
    pure function is_enabled_ats(self) result(is_enabled)
        implicit none
        class(type_ats), intent(in) :: self
        logical :: is_enabled

        is_enabled = self%config%active
    end function is_enabled_ats

    !> Calculate the recommended dt for the next step after successful convergence.
    pure subroutine predict_next_dt(self, current_dt, iter_count, next_dt)
        implicit none
        class(type_ats), intent(in) :: self
        real(real64), intent(in) :: current_dt
        integer(int32), intent(in) :: iter_count
        real(real64), intent(inout) :: next_dt

        real(real64) :: dt_temp

        next_dt = current_dt
        if (.not. self%config%active) return

        if (iter_count <= self%config%iter_min) then
            ! 1. Predict with scale_up and apply safety factor
            if (self%config%scale_up * self%config%safety_factor > 1.0d0) then
                dt_temp = current_dt * self%config%scale_up * self%config%safety_factor
            else
                dt_temp = current_dt * self%config%scale_up
            end if

            ! 2. Apply hard growth cap for BDF stability
            ! Even if scale_up is large, we limit the ratio dt_next/dt_now
            next_dt = min(dt_temp, current_dt * self%config%max_growth_rate)
        else if (iter_count >= self%config%iter_max) then
            ! Convergence is hard -> decelerate.
            next_dt = current_dt * self%config%scale_down
        end if

        ! Limiters
        next_dt = max(self%config%dt_min, min(next_dt, self%config%dt_max))
    end subroutine predict_next_dt

    !> Calculate a reduced dt for retry upon divergence.
    pure subroutine calc_retry_dt(self, current_dt, retry_dt)
        implicit none
        class(type_ats), intent(in) :: self
        real(real64), intent(in) :: current_dt
        real(real64), intent(inout) :: retry_dt

        retry_dt = current_dt
        if (.not. self%config%active) return

        retry_dt = current_dt * self%config%scale_retry

        ! Clip to minimum allowed dt
        if (retry_dt < self%config%dt_min) retry_dt = self%config%dt_min
    end subroutine calc_retry_dt

end module control_ats

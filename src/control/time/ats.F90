module control_time_ats
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

        integer(int32) :: iter_min = 4
        integer(int32) :: iter_max = 8

        real(real64) :: scale_up = 1.2d0
        real(real64) :: scale_down = 0.8d0
        real(real64) :: scale_retry = 0.5d0

        real(real64) :: safety_factor = 0.9d0
        real(real64) :: max_growth_rate = 2.0d0

        real(real64) :: dt_min = 1.0d-5
        real(real64) :: dt_max = 1.0d+2

        real(real64) :: max_dT_per_step = 5.0d0
        real(real64) :: max_relative_change = 0.3d0

        !> Error-controlled (PI) adaptive stepping. See type_config_time_ats.
        logical :: use_error_control = .false.
        real(real64) :: pi_k_i = 0.15d0
        real(real64) :: pi_k_p = 0.10d0
        real(real64) :: error_rtol = 1.0d-2
        !> Previous normalized error (state for the PI controller); 1.0 = on target.
        real(real64) :: error_prev = 1.0d0
    contains
        procedure, public, pass(self) :: initialize => initialize_type_ats
        procedure, public, pass(self) :: is_active => is_active_ats
        procedure, public, pass(self) :: predict_next_dt
        procedure, public, pass(self) :: pi_controller_dt
        procedure, public, pass(self) :: calc_retry_dt
        procedure, public, pass(self) :: limit_dt_by_solution_change
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
        self%use_error_control = config_time_ats%use_error_control
        self%pi_k_i = config_time_ats%pi_k_i
        self%pi_k_p = config_time_ats%pi_k_p
        self%error_rtol = config_time_ats%error_rtol
        self%max_dT_per_step = config_time_ats%max_dT_per_step
        self%max_relative_change = config_time_ats%max_relative_change_per_step
        self%error_prev = 1.0d0

        ! Echo the loaded ATS configuration so it is verifiable at runtime that the
        ! adaptive_stepping block from Conditions.json is actually reflected.
        write (output_unit, '(A)') "## ATS configuration loaded from input"
        write (output_unit, '(A,L1)') "   - active            : ", self%active
        write (output_unit, '(A,I0,A,I0)') "   - iter_min / max    : ", self%iter_min, " / ", self%iter_max
        write (output_unit, '(A,F7.4,A,F7.4,A,F7.4)') "   - scale up/down/retry: ", &
            self%scale_up, " / ", self%scale_down, " / ", self%scale_retry
        write (output_unit, '(A,ES12.5,A,ES12.5)') "   - dt_min / dt_max [s]: ", self%dt_min, " / ", self%dt_max
        write (output_unit, '(A,F7.4,A,F7.4)') "   - safety / max_growth: ", self%safety_factor, " / ", self%max_growth_rate
        write (output_unit, '(A,L1,A,F6.3,A,F6.3,A,ES10.3)') "   - err-control / kI / kP / rtol: ", &
            self%use_error_control, " / ", self%pi_k_i, " / ", self%pi_k_p, " / ", self%error_rtol

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

    !> PI-controlled time step from a normalized local-truncation-error estimate.
    !>
    !> Given the dimensionless error E (E<=1 means the step met the accuracy target)
    !> and the previous error, the next step follows the standard PI controller
    !> \( \Delta t_{new} = \kappa_{safe}\,\Delta t\; E^{-k_I}\, E_{prev}^{\,k_P} \)
    !> (PDF 6.3.5). The growth/shrink ratio is capped by max_growth_rate for BDF
    !> stability and the result is clamped to [dt_min, dt_max]. error_prev is
    !> advanced as controller state. A non-positive E (no estimate yet) is a no-op.
    subroutine pi_controller_dt(self, current_dt, error_norm, next_dt)
        implicit none
        class(type_ats), intent(inout) :: self
        real(real64), intent(in) :: current_dt
        real(real64), intent(in) :: error_norm
        real(real64), intent(inout) :: next_dt

        real(real64) :: e_cur, e_prev, ratio

        next_dt = current_dt
        if (.not. self%active) return
        if (error_norm <= 0.0d0) return

        ! Normalize the relative LTE by the target tolerance so E<=1 is on-target.
        e_cur = max(error_norm / self%error_rtol, 1.0d-12)
        e_prev = max(self%error_prev, 1.0d-12)

        ratio = self%safety_factor * e_cur**(-self%pi_k_i) * e_prev**(self%pi_k_p)
        ratio = min(max(ratio, 1.0d0 / self%max_growth_rate), self%max_growth_rate)

        next_dt = max(self%dt_min, min(current_dt * ratio, self%dt_max))
        self%error_prev = e_cur
    end subroutine pi_controller_dt

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

    pure subroutine limit_dt_by_solution_change(self, current_dt, max_abs_change, ref_scale, limited_dt)
        implicit none
        class(type_ats), intent(in) :: self
        real(real64), intent(in) :: current_dt
        real(real64), intent(in) :: max_abs_change
        real(real64), intent(in) :: ref_scale
        real(real64), intent(inout) :: limited_dt

        real(real64) :: ratio, dt_proposed

        limited_dt = current_dt
        if (.not. self%active) return

        if (max_abs_change > self%max_dT_per_step .and. max_abs_change > 0.0d0) then
            ratio = self%max_dT_per_step / max_abs_change
            dt_proposed = current_dt * ratio * self%safety_factor
            limited_dt = min(limited_dt, dt_proposed)
        end if

        if (ref_scale > 0.0d0) then
            ratio = max_abs_change / ref_scale
            if (ratio > self%max_relative_change .and. ratio > 0.0d0) then
                dt_proposed = current_dt * (self%max_relative_change / ratio) * self%safety_factor
                limited_dt = min(limited_dt, dt_proposed)
            end if
        end if

        limited_dt = max(self%dt_min, min(limited_dt, self%dt_max))
    end subroutine limit_dt_by_solution_change

end module control_time_ats

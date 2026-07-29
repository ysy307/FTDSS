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
        !> Set when a step is rejected, cleared by the first step accepted after
        !> it. While set, the PI controller may not grow dt.
        logical :: after_rejection = .false.

        real(real64) :: safety_factor = 0.9d0
        real(real64) :: max_growth_rate = 2.0d0

        real(real64) :: dt_min = 1.0d-5
        real(real64) :: dt_max = 1.0d+2

        real(real64) :: max_dT_per_step = 5.0d0
        real(real64) :: max_relative_change = 0.3d0

        !> Error-controlled (PI) adaptive stepping. See type_config_time_ats.
        logical :: use_error_control = .false.
        real(real64) :: pi_k_i = 0.15d0
        real(real64) :: pi_k_p = 0.20d0
        real(real64) :: error_rtol = 1.0d-2
        real(real64) :: error_atol_temperature = 1.0d-3
        real(real64) :: error_atol_pressure = 1.0d1
        !> Previous normalized error (state for the PI controller); 1.0 = on target.
        real(real64) :: error_prev = 1.0d0
    contains
        procedure, public, pass(self) :: initialize => initialize_type_ats
        procedure, public, pass(self) :: is_active => is_active_ats
        procedure, public, pass(self) :: predict_next_dt
        procedure, public, pass(self) :: pi_controller_dt
        procedure, public, pass(self) :: lte_retry_dt
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
        self%error_atol_temperature = config_time_ats%error_atol_temperature
        self%error_atol_pressure = config_time_ats%error_atol_pressure
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
        write (output_unit, '(A,2(ES10.3,1X))') "   - LTE atol T / p     : ", &
            self%error_atol_temperature, self%error_atol_pressure

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
    !> and the previous error, the next step follows the Gustafsson PI controller
    !> \( \Delta t_{new} = \kappa_{safe}\,\Delta t\; E^{-k_I}\,(E_{prev}/E)^{k_P} \)
    !> (Hairer & Wanner II, PI.4.2). At steady state (E_prev = E) the gain is
    !> \( \kappa_{safe} E^{-k_I} \), neutral at \( E = \kappa_{safe}^{1/k_I} \approx 0.5 \),
    !> so dt grows whenever the error is below about half the target and shrinks above.
    !> The growth/shrink ratio is capped by max_growth_rate for BDF stability and the
    !> result is clamped to [dt_min, dt_max]. error_prev is advanced as controller
    !> state. A non-positive E (no estimate yet) is a no-op.
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

        ! The estimator supplies an atol/rtol-normalized weighted RMS error.
        e_cur = max(error_norm, 1.0d-12)
        e_prev = max(self%error_prev, 1.0d-12)

        ratio = self%safety_factor * e_cur**(-(self%pi_k_i + self%pi_k_p)) * e_prev**(self%pi_k_p)
        ratio = min(max(ratio, 1.0d0 / self%max_growth_rate), self%max_growth_rate)

        ! Gustafsson's rule: the PI term is only meaningful on a run of accepted
        ! steps. Right after a rejection the error history describes a step that
        ! was never taken, and the step that did succeed is small precisely
        ! because the previous one failed - so its small error argues for growth
        ! exactly where growth is least safe. Measured at the freezing onset: a
        ! step accepted at dt = 73.4 s with E = 0.049 was followed by a proposal
        ! of 111.7 s, which failed, and the run then alternated between failing
        ! and recovering without advancing. Holding dt until one clean step has
        ! been taken costs one step of growth and removes that cycle.
        if (self%after_rejection) then
            ratio = min(ratio, 1.0d0)
            self%after_rejection = .false.
        end if

        next_dt = max(self%dt_min, min(current_dt * ratio, self%dt_max))
        self%error_prev = e_cur
    end subroutine pi_controller_dt

    !> Retry step after a converged solve violates the LTE tolerance.
    pure subroutine lte_retry_dt(self, current_dt, error_norm, retry_dt)
        implicit none
        class(type_ats), intent(in) :: self
        real(real64), intent(in) :: current_dt
        real(real64), intent(in) :: error_norm
        real(real64), intent(inout) :: retry_dt

        real(real64) :: ratio

        retry_dt = current_dt
        if (.not. self%active .or. error_norm <= 1.0d0) return

        ! BDF1 has a second-order local defect, hence exponent -1/2.
        ratio = self%safety_factor * error_norm**(-0.5d0)
        ratio = min(0.8d0, max(0.1d0, ratio))
        retry_dt = max(self%dt_min, min(current_dt * ratio, self%dt_max))
    end subroutine lte_retry_dt

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

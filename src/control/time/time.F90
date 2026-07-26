!> Module for managing time stepping control and BDF (Backward Differentiation Formula) coefficients.
!> Handles variable time steps, time unit conversions, and history management for high-order time integration schemes.
module control_time
    use, intrinsic :: iso_fortran_env, only: int32, real64, output_unit
    use :: stdlib_optval, only:optval
    use :: module_core
    use :: control_time_ats, only:type_ats

    implicit none
    private

    public :: type_time

    ! --- Constants ---
    !> Maximum supported order for BDF schemes.
    integer(int32), parameter :: MAX_BDF_ORDER = 6
    !> Small number for zero-division check
    real(real64), parameter :: EPS_TIME = 1.0d-14

    !> Data structure holding time stepping state and integration parameters.
    type :: type_time
        logical, private :: initialized = .false.
        type(type_config_time), private :: config

        !! --- Time Coefficients ---
        !> Current simulation time [s]
        real(real64), private :: current_time = 0.0d0
        !> Time at previous step [s]
        real(real64), private :: time_old = 0.0d0
        !> Current time step size [s]
        real(real64), private :: dt = 0.0d0
        !> Unclipped controller step [s]. Output synchronization may shorten dt
        !> for one step, but it must not permanently throttle ATS/fixed stepping.
        real(real64), private :: dt_controller = 0.0d0
        !> History of time steps
        real(real64), private, allocatable :: dt_history(:)

        ! --- BDF Coefficients ---
        !> Coefficients $\alpha_j$ for the BDF formula:
        !> $$ \frac{dy}{dt} \approx \sum_{j=0}^{k} \alpha_j y_{n-j} $$
        !> Note: Coefficients include the $1/\Delta t$ scaling.
        real(real64), private :: coeffs(0:MAX_BDF_ORDER) = 0.0d0
        !> Currently active BDF order (ramps up from 1 at start)
        integer(int32), private :: current_bdf_order = 1

        type(type_ats), private :: ats
    contains
        ! --- Public Interfaces ---
        !> Initialize time control settings from input.
        procedure, public, pass(self) :: initialize => initialize_type_time
        !> Update BDF coefficients based on current history.
        procedure, public, pass(self) :: update_bdf_coefficients
        !> Get current simulation time.
        procedure, public, pass(self) :: get_time
        !> Get current time step size.
        procedure, public, pass(self) :: get_dt
        !> Get current BDF order.
        procedure, public, pass(self) :: get_bdf_order
        !> Get BDF coefficients array.
        procedure, public, pass(self) :: get_bdf_coeffs
        !> Get simulation end time.
        procedure, public, pass(self) :: get_end_time
        !> Shift time history for the next step.
        procedure, public, pass(self) :: shift => shift_time
        !> Display current time status to standard output.
        procedure, public, pass(self) :: display => display_status
        !> Set current time step size.
        procedure, public, pass(self) :: set_dt
        !> Check if simulation reached end time.
        procedure, public, pass(self) :: is_end_time
        procedure, public, pass(self) :: is_min_dt => is_min_dt_reached
        procedure, public, pass(self) :: is_max_dt => is_max_dt_reached
        !> Synchronize time step with output interval.
        procedure, public, pass(self) :: sync_with_output
        procedure, public, pass(self) :: update => update_type_time

        procedure, public, pass(self) :: is_initialized => is_initialized_time

        ! --- Private Procedures ---
        !> Compute variable step BDF coefficients.
        procedure, private, pass(self) :: compute_bdf_coefficients
    end type type_time

contains

    ! ==========================================================================
    ! Initialization
    ! ==========================================================================

    !> Initialize the time control object using input configuration.
    subroutine initialize_type_time(self, config_time, config_time_ats)
        implicit none
        class(type_time), intent(inout) :: self
        type(type_config_time), intent(in) :: config_time
        type(type_config_time_ats), intent(in) :: config_time_ats

        call self%config%copy(config_time)

        ! --- BDF Settings ---
        if (self%config%target_bdf_order > MAX_BDF_ORDER) then
            self%config%target_bdf_order = MAX_BDF_ORDER
        end if
        self%current_bdf_order = 1

        self%dt = self%config%initial_step
        self%dt_controller = self%dt

        ! --- Allocate History ---
        call allocate_array(self%dt_history, self%config%target_bdf_order)

        if (allocated(self%dt_history)) then
            self%dt_history(:) = 0.0d0
            self%dt_history(1) = self%dt
        end if

        ! --- Compute Initial Coefficients ---
        call self%compute_bdf_coefficients()

        self%current_time = self%config%start_time

        ! --- 3. Initialize ATS ---
        call self%ats%initialize(config_time_ats)

        self%initialized = .true.
    end subroutine initialize_type_time
    ! ==========================================================================
    ! Time Stepping & BDF
    ! ==========================================================================

    !> Shift time history and update current time state after a successful step.
    !> Also manages the ramping up of BDF order during initial steps.
    subroutine shift_time(self)
        implicit none
        !> Time control instance
        class(type_time), intent(inout) :: self
        integer(int32) :: n

        if (.not. allocated(self%dt_history)) return
        n = size(self%dt_history)

        ! Update time
        self%time_old = self%current_time
        self%current_time = self%current_time + self%dt

        ! Shift history: dt_history(1) becomes current dt_n
        if (n > 1) then
            self%dt_history(2:n) = self%dt_history(1:n - 1)
        end if
        self%dt_history(1) = self%dt

        ! Update available BDF order (Ramp up strategy)
        if (self%current_bdf_order < self%config%target_bdf_order) then
            self%current_bdf_order = self%current_bdf_order + 1
        end if

        ! Recompute coefficients
        call self%compute_bdf_coefficients()

    end subroutine shift_time

    !> Explicitly trigger an update of BDF coefficients.
    subroutine update_bdf_coefficients(self)
        implicit none
        class(type_time), intent(inout) :: self

        call self%compute_bdf_coefficients()
    end subroutine update_bdf_coefficients

    !> Compute variable step-size BDF coefficients.
    !> Calculates \( \alpha_j \) such that \( \frac{dy}{dt}|_{t_n} \approx \sum_{j=0}^{k} \alpha_j y_{n-j} \).
    !> Note: Coefficients \( \alpha_j \) inherently include the \( 1/\Delta t \) scaling.
    subroutine compute_bdf_coefficients(self)
        implicit none
        !> Time control instance
        class(type_time), intent(inout) :: self

        integer(int32) :: k, j, m
        real(real64) :: tau(0:self%current_bdf_order)
        real(real64) :: prod_term

        k = self%current_bdf_order

        ! 0. Check for invalid dt
        if (self%dt <= EPS_TIME) then
            ! Fallback to Backward Euler for safety if dt is extremely small
            self%coeffs = 0.0d0
            if (self%dt > epsilon(0.0d0)) then
                self%coeffs(0) = 1.0d0 / self%dt
                self%coeffs(1) = -1.0d0 / self%dt
            end if
            return
        end if

        ! 1. Calculate relative time differences tau(j) = t_{n+1} - t_{n+1-j}.
        ! The CURRENT step uses the active dt (self%dt); older steps use the
        ! committed history. Using self%dt for tau(1) makes ATS dt changes and
        ! retried (reduced) steps take immediate effect in the BDF storage term,
        ! instead of lagging one step behind via dt_history(1).
        tau(0) = 0.0d0
        tau(1) = self%dt
        do j = 2, k
            tau(j) = tau(j - 1) + self%dt_history(j - 1)
        end do

        self%coeffs = 0.0d0

        ! 2. Compute derivative of Lagrange interpolating polynomial at t_n
        ! L_j(t) = prod_{m!=j} (t - t_{n-m}) / (t_{n-j} - t_{n-m})

        ! (A) Case j = 0 (Coefficient for y_n)
        ! L_0_(t_n) = sum_{m=1}^k (1 / tau(m))
        do m = 1, k
            self%coeffs(0) = self%coeffs(0) + (1.0d0 / tau(m))
        end do

        ! (B) Case j > 0 (Coefficient for y_{n-j})
        ! Coeff_j = (1 / -tau(j)) * prod_{m!=0, j} (tau(m) / (tau(m) - tau(j)))
        do j = 1, k
            prod_term = 1.0d0
            do m = 1, k
                if (m == j) cycle
                prod_term = prod_term * (tau(m) / (tau(m) - tau(j)))
            end do
            self%coeffs(j) = (-1.0d0 / tau(j)) * prod_term
        end do

    end subroutine compute_bdf_coefficients

    ! ==========================================================================
    ! Getters & Setters
    ! ==========================================================================

    !> Get current simulation time (seconds) with optional unit conversion.
    !> If a target time unit is provided, the `current_time` is converted accordingly.
    subroutine get_time(self, current_time, time_unit)
        implicit none
        class(type_time), intent(in) :: self
        !> Output current time
        real(real64), intent(inout) :: current_time
        !> Target time unit (optional)
        type(type_constant_value), intent(in), optional :: time_unit

        real(real64) :: coeff

        if (present(time_unit)) then
            coeff = 1.0d0 / time_unit%value
        else
            coeff = 1.0d0
        end if

        current_time = self%current_time * coeff
    end subroutine get_time

    !> Get current time step size (seconds) with optional unit conversion.
    !> If a target time unit is provided,  `dt` is converted accordingly.
    subroutine get_dt(self, dt, time_unit)
        implicit none
        class(type_time), intent(in) :: self
        !> Output time step
        real(real64), intent(inout) :: dt
        !> Target time unit (optional)
        type(type_constant_value), intent(in), optional :: time_unit

        real(real64) :: coeff

        if (present(time_unit)) then
            coeff = 1.0d0 / time_unit%value
        else
            coeff = 1.0d0
        end if

        dt = self%dt * coeff
    end subroutine get_dt

    pure subroutine get_end_time(self, end_time)
        implicit none
        class(type_time), intent(in) :: self
        real(real64), intent(inout) :: end_time

        end_time = self%config%end_time
    end subroutine get_end_time

    !> Set current time step size (seconds) with optional unit conversion.
    !> If a target time unit is provided, `dt` is converted accordingly before setting.
    subroutine set_dt(self, new_dt, time_unit)
        implicit none
        class(type_time), intent(inout) :: self
        !> Input time step
        real(real64), intent(in) :: new_dt
        !> Target time unit (optional)
        type(type_constant_value), intent(in), optional :: time_unit

        real(real64) :: dt

        if (present(time_unit)) then
            dt = new_dt * time_unit%value
        else
            dt = new_dt
        end if

        if (dt <= self%ats%dt_min) then
            self%dt = self%ats%dt_min
        else if (dt >= self%ats%dt_max) then
            self%dt = self%ats%dt_max
        else
            self%dt = dt
        end if
        self%dt_controller = self%dt

        call self%compute_bdf_coefficients()
    end subroutine set_dt

    !>
    !> Check if minimum time step is reached.
    pure function is_min_dt_reached(self) result(is_min_dt)
        implicit none
        !> Time control instance
        class(type_time), intent(in) :: self
        !> Result flag
        logical :: is_min_dt

        is_min_dt = abs(self%dt - self%ats%dt_min) <= EPS_TIME
    end function is_min_dt_reached

    !>
    !> Check if maximum time step is reached.
    pure function is_max_dt_reached(self) result(is_max_dt)
        implicit none
        !> Time control instance
        class(type_time), intent(in) :: self
        !> Result flag
        logical :: is_max_dt

        is_max_dt = abs(self%ats%dt_max - self%dt) <= EPS_TIME
    end function is_max_dt_reached

    !> Get current BDF order.
    subroutine get_bdf_order(self, bdf_order)
        implicit none
        class(type_time), intent(in) :: self
        integer(int32), intent(inout) :: bdf_order

        bdf_order = self%current_bdf_order
    end subroutine get_bdf_order

    !> Get pointer to current BDF coefficient array.
    subroutine get_bdf_coeffs(self, coeffs)
        implicit none
        class(type_time), intent(in), target :: self
        real(real64), intent(inout), pointer, dimension(:) :: coeffs

        coeffs => self%coeffs(0:self%current_bdf_order)
    end subroutine get_bdf_coeffs

    !> Check if the simulation has reached or exceeded the end time.
    pure function is_end_time(self) result(is_end)
        implicit none
        class(type_time), intent(in) :: self
        logical :: is_end

        is_end = self%current_time >= self%config%end_time
    end function is_end_time

    !> Display time status summary to stdout.
    subroutine display_status(self, unit_in)
        implicit none
        class(type_time), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: unit

        unit = optval(unit_in, output_unit)

        write (unit, '(a)') "## Time Status"
        write (unit, '(a)') "---"
        write (unit, *)

        write (unit, '(a)') "### Simulation Period"
        write (unit, '(" - Start Time       : ", ES12.5)') self%config%start_time
        write (unit, '(" - End Time         : ", ES12.5)') self%config%end_time
        write (unit, *)

        write (unit, '(a)') "### Current Time Step"
        write (unit, '(" - Current Time     : ", ES12.5)') self%current_time
        write (unit, '(" - Current dt       : ", ES12.5)') self%dt
        write (unit, '(" - BDF Order        : ", I0)') self%current_bdf_order
        write (unit, *)

    end subroutine display_status

    !> Perform all time-related updates after a nonlinear solve attempt
    subroutine update_type_time(self, success, iter_count, next_output_time, error_estimate)
        implicit none
        class(type_time), intent(inout) :: self
        logical, intent(in) :: success
        integer(int32), intent(in) :: iter_count
        real(real64), intent(in) :: next_output_time
        real(real64), intent(in), optional :: error_estimate
        real(real64) :: next_dt, dt_iter, dt_err, dt_tol
        logical :: output_limited_step

        if (success) then
            dt_tol = max(EPS_TIME, 100.0d0 * epsilon(1.0d0) * max(1.0d0, self%dt_controller))
            output_limited_step = self%dt_controller > 0.0d0 .and. &
                                  self%dt < self%dt_controller - dt_tol

            ! Step converged: advance time and predict next step
            call self%shift()

            if (output_limited_step) then
                ! The accepted step was shortened only to hit an output time.
                ! Keep the controller state from that artificial event step; a
                ! genuinely hard clipped step can still reduce the controller.
                next_dt = self%dt_controller
                if (self%ats%active .and. iter_count >= self%ats%iter_max) then
                    next_dt = self%dt_controller * self%ats%scale_down
                end if
                next_dt = max(self%ats%dt_min, min(next_dt, self%ats%dt_max))
            else if (self%ats%active .and. self%ats%use_error_control .and. present(error_estimate)) then
                ! Error-controlled stepping: accuracy (PI controller on the local
                ! truncation error) is the primary driver of dt; the iteration count
                ! acts only as a robustness brake that caps growth when the nonlinear
                ! solve is hard (iter >= iter_max), via the unified minimum (PDF 6.3.8).
                call self%ats%pi_controller_dt(self%dt, error_estimate, dt_err)
                next_dt = dt_err
                if (iter_count >= self%ats%iter_max .and. self%dt > self%ats%dt_min + dt_tol) then
                    next_dt = min(next_dt, self%dt * self%ats%scale_down)
                end if
                next_dt = max(self%ats%dt_min, min(next_dt, self%ats%dt_max))
            else
                ! Iteration-count prediction.
                call self%ats%predict_next_dt(self%dt, iter_count, dt_iter)
                next_dt = dt_iter
            end if
            self%dt_controller = next_dt
            self%dt = next_dt

            ! Sync with output time (modifies only the active dt if necessary).
            call self%sync_with_output(next_output_time)
        else
            ! A converged step rejected by LTE uses the BDF1 defect estimate;
            ! nonlinear failures retain the configured robustness retry factor.
            if (self%ats%active .and. self%ats%use_error_control .and. present(error_estimate)) then
                if (error_estimate > 1.0d0) then
                    call self%ats%lte_retry_dt(self%dt, error_estimate, next_dt)
                else
                    call self%ats%calc_retry_dt(self%dt, next_dt)
                end if
            else
                call self%ats%calc_retry_dt(self%dt, next_dt)
            end if
            self%dt_controller = next_dt
            self%dt = next_dt
            self%current_bdf_order = 1
        end if
        call self%compute_bdf_coefficients()
    end subroutine update_type_time

    !> Synchronize dt with the next output time.
    subroutine sync_with_output(self, next_output_time)
        implicit none
        class(type_time), intent(inout) :: self
        real(real64), intent(in) :: next_output_time

        real(real64) :: time_to_output

        ! Remaining time until the next output.
        time_to_output = next_output_time - self%current_time

        ! If "dt proposed by ATS" is longer than "remaining time",
        ! shorten dt to align exactly with the output time.
        if (self%dt > time_to_output .and. time_to_output > EPS_TIME) then

            ! Note: exercise caution if remaining time is smaller than min_step.
            ! (Adjustment might have been insufficient in the previous step; decide to force alignment or error.)
            ! Determine policy: issue warning and force alignment, or ignore if too small.

            self%dt = time_to_output
        end if
    end subroutine sync_with_output

    !> Check if the time control has been initialized.
    pure function is_initialized_time(self) result(is_initialized)
        implicit none
        class(type_time), intent(in) :: self
        logical :: is_initialized

        is_initialized = self%initialized
    end function is_initialized_time
end module control_time

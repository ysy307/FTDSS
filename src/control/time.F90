!> Module for managing time stepping control and BDF (Backward Differentiation Formula) coefficients.
!> Handles variable time steps, time unit conversions, and history management for high-order time integration schemes.
module control_time
    use, intrinsic :: iso_fortran_env, only: int32, real64, output_unit
    use :: omp_lib
    use :: stdlib_optval, only:optval
    use :: stdlib_strings, only:strip
    use :: module_core
    use :: module_input, only:type_input

    implicit none
    private

    public :: type_time

    ! --- Constants ---
    !> Maximum supported order for BDF schemes.
    integer(int32), parameter :: MAX_BDF_ORDER = 6
    !> Error code for time initialization failure.
    integer(int32), parameter :: ERR_TIME_INIT = 981
    !> Error code for profiler issues.
    integer(int32), parameter :: ERR_PROFILER = 982

    !> Data structure holding time stepping state and integration parameters.
    type :: type_time
        private
        ! --- Time Stepping State ---
        !> Simulation start time [s]
        real(real64) :: start_time = 0.0d0
        !> Simulation end time [s]
        real(real64) :: end_time = 0.0d0
        !> Current simulation time [s]
        real(real64) :: current_time_s = 0.0d0
        !> Time at previous step [s]
        real(real64) :: time_old = 0.0d0

        !> Current time step size \( \Delta t \) [s]
        real(real64) :: dt_s = 0.0d0
        !> History of time steps \( [\Delta t_n, \Delta t_{n-1}, \dots] \)
        real(real64), allocatable :: dt_s_history(:)
        !> Minimum allowable time step [s]
        real(real64) :: dt_s_min = 0.0d0
        !> Maximum allowable time step [s]
        real(real64) :: dt_s_max = 0.0d0

        !> Conversion factor for display units
        real(real64) :: time_conversion = 1.0d0

        ! --- BDF Coefficients ---
        !> Coefficients \( \alpha_j \) for the BDF formula:
        !> \( \frac{dy}{dt} \approx \sum_{j=0}^{k} \alpha_j y_{n-j} \).
        !> Note: Coefficients include the \( 1/\Delta t \) scaling.
        real(real64) :: coeffs(0:MAX_BDF_ORDER) = 0.0d0
        !> Target BDF order set by user
        integer(int32) :: target_bdf_order = 1
        !> Currently active BDF order (ramps up from 1 at start)
        integer(int32) :: current_bdf_order = 1
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
        !> Advance simulation time by one step.
        procedure, public, pass(self) :: advance => advance_time
        !> Shift time history for the next step.
        procedure, public, pass(self) :: shift => shift_time
        !> Display current time status to standard output.
        procedure, public, pass(self) :: display => display_status

        ! --- Private Procedures ---
        !> Compute variable step BDF coefficients.
        procedure, private, pass(self) :: compute_bdf_coefficients
        !> Helper to convert time units.
        procedure, public, pass(self) :: convert_time_unit
    end type type_time

contains

    ! ==========================================================================
    ! Initialization
    ! ==========================================================================

    !> Initialize the time control object using input configuration.
    !> Sets up initial time step, simulation period, and allocates history arrays.
    subroutine initialize_type_time(self, input)
        implicit none
        !> Time control instance
        class(type_time), intent(inout) :: self
        !> Input data structure
        type(type_input), intent(in) :: input

        integer(int32) :: i, istat
        real(real64) :: time_conv_coeff

        ! --- BDF Settings ---
        self%target_bdf_order = input%basic%solver_settings%bdf_order
        if (self%target_bdf_order > MAX_BDF_ORDER) then
            self%target_bdf_order = MAX_BDF_ORDER
        end if
        ! Start with 1st order (Backward Euler) as no history exists
        self%current_bdf_order = 1

        ! --- Time Unit Conversion ---
        associate (time_control => input%conditions%time_control)
            call self%convert_time_unit(time_control%time_stepping%unit, TIME_UNIT_SECONDS, time_conv_coeff)

            ! --- Set dt ---
            self%dt_s = time_control%time_stepping%initial_step * time_conv_coeff
            self%dt_s_max = time_control%time_stepping%max_step * time_conv_coeff
            self%dt_s_min = time_control%time_stepping%min_step * time_conv_coeff

            ! --- Allocate History ---
            call deallocate_array(self%dt_s_history)
            call allocate_array(self%dt_s_history, self%target_bdf_order)

            self%dt_s_history(:) = 0.0d0
            self%dt_s_history(1) = self%dt_s

            ! --- Compute Initial Coefficients (1st Order) ---
            call self%compute_bdf_coefficients()

            ! --- Simulation Period ---
            if (input%output_settings%field_output%file_format /= "none") then
                call self%convert_time_unit(time_control%simulation_period%unit, TIME_UNIT_SECONDS, time_conv_coeff)
                self%start_time = time_control%simulation_period%start * time_conv_coeff
                self%end_time = time_control%simulation_period%end * time_conv_coeff

                call self%convert_time_unit(input%output_settings%field_output%output_interval_unit, &
                                            time_control%simulation_period%unit, &
                                            self%time_conversion)
            end if

            ! Set initial time
            self%current_time_s = self%start_time

        end associate

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

        if (.not. allocated(self%dt_s_history)) return
        n = size(self%dt_s_history)

        ! Update time
        self%time_old = self%current_time_s
        self%current_time_s = self%current_time_s + self%dt_s

        ! Shift history: dt_history(1) becomes current dt_n
        if (n > 1) self%dt_s_history(2:n) = self%dt_s_history(1:n - 1)
        self%dt_s_history(1) = self%dt_s

        ! Update available BDF order (Ramp up strategy)
        if (self%current_bdf_order < self%target_bdf_order) then
            self%current_bdf_order = self%current_bdf_order + 1
        end if

        ! Recompute coefficients
        call self%compute_bdf_coefficients()

    end subroutine shift_time

    !> Advance the simulation time tentatively for the next step solver.
    subroutine advance_time(self, new_dt)
        implicit none
        !> Time control instance
        class(type_time), intent(inout) :: self
        !> New time step size (optional)
        real(real64), intent(in), optional :: new_dt

        self%dt_s = optval(new_dt, self%dt_s)

        self%current_time_s = self%current_time_s + self%dt_s
    end subroutine advance_time

    !> Explicitly trigger an update of BDF coefficients.
    subroutine update_bdf_coefficients(self)
        implicit none
        class(type_time), intent(inout) :: self

        call self%compute_bdf_coefficients()
    end subroutine update_bdf_coefficients

    ! --------------------------------------------------------------------------
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
        if (self%dt_s <= 1.0d-16) then
            ! Fallback to Backward Euler for safety if dt is extremely small
            self%coeffs = 0.0d0
            if (self%dt_s > 0.0d0) then
                self%coeffs(0) = 1.0d0 / self%dt_s
                self%coeffs(1) = -1.0d0 / self%dt_s
            end if
            return
        end if

        ! 1. Calculate relative time differences tau
        ! tau(j) = t_n - t_{n-j}
        tau(0) = 0.0d0
        do j = 1, k
            tau(j) = tau(j - 1) + self%dt_s_history(j)
        end do

        self%coeffs = 0.0d0

        ! 2. Compute derivative of Lagrange interpolating polynomial at t_n
        ! L_j(t) = prod_{m!=j} (t - t_{n-m}) / (t_{n-j} - t_{n-m})

        ! (A) Case j = 0 (Coefficient for y_n)
        ! L_0'(t_n) = sum_{m=1}^k (1 / tau(m))
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
    ! Getters
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

        current_time = self%current_time_s * coeff
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

        dt = self%dt_s * coeff
    end subroutine get_dt

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

    !> Convert value between time units based on internal factors.
    pure subroutine convert_time_unit(self, source_unit, target_unit, coefficient)
        implicit none
        class(type_time), intent(in) :: self
        integer(int32), intent(in) :: source_unit, target_unit
        real(real64), intent(inout) :: coefficient
        real(real64) :: to_seconds_factor(5)

        ! 1:sec, 2:min, 3:hour, 4:day, 5:year
        to_seconds_factor = [1.0d0, 60.0d0, 3600.0d0, 86400.0d0, 31557600.0d0]

        if (source_unit < 1 .or. source_unit > 5 .or. target_unit < 1 .or. target_unit > 5) then
            coefficient = 1.0d0
        else
            coefficient = to_seconds_factor(source_unit) / to_seconds_factor(target_unit)
        end if
    end subroutine convert_time_unit

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
        write (unit, '(" - Start Time       : ", ES12.5)') self%start_time
        write (unit, '(" - End Time         : ", ES12.5)') self%end_time
        write (unit, *)

        write (unit, '(a)') "### Current Time Step"
        write (unit, '(" - Current Time     : ", ES12.5)') self%current_time_s
        write (unit, '(" - Current dt       : ", ES12.5)') self%dt_s
        write (unit, '(" - BDF Order        : ", I0)') self%current_bdf_order
        write (unit, *)

    end subroutine display_status

end module control_time

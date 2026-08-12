submodule(app_ftcms) ftcms_solve
    use :: models_phase_change_chemical_potential, only:calc_T_high_celsius
    implicit none

    integer(int32), parameter :: SOLVE_STATUS_NOT_RUN = 0
    integer(int32), parameter :: SOLVE_STATUS_CONVERGED = 1
    integer(int32), parameter :: SOLVE_STATUS_LINEAR_FAILURE = 2
    integer(int32), parameter :: SOLVE_STATUS_NONLINEAR_DIVERGED = 3
    integer(int32), parameter :: SOLVE_STATUS_NONLINEAR_LIMIT = 4
    integer(int32), parameter :: SOLVE_STATUS_PHASE_FAILURE = 5
    integer(int32), parameter :: SOLVE_STATUS_LTE_REJECTED = 6

    !> Audit the assembled tangent against the element finite-difference tangent
    !> once, at the first iterate where the line search finds no usable step.
    !> Diagnostic only: it costs O(4 n_node) element residual evaluations per
    !> element and must not be left on for production runs.
    logical, parameter :: FD_JACOBIAN_AUDIT = .false.
    logical, save :: fd_audit_done = .false.

    !> One-shot scan of the residual along the Newton direction at a genuinely
    !> stalled iterate. Diagnostic only.
    logical, parameter :: RESIDUAL_SCAN = .false.
    integer(int32), parameter :: RESIDUAL_SCAN_ITER = 12
    logical, save :: residual_scan_done = .false.
    logical, save :: branch_report_done = .false.

    !> Freezing-band telemetry at the iterations that exhaust the budget.
    integer(int32), parameter :: FREEZE_REPORT_ITER = 10
    integer(int32), parameter :: FREEZE_REPORT_LIMIT = 40
    integer(int32), save :: freeze_report_count = 0
    real(real64), allocatable, save :: freeze_prev_liquid(:), freeze_prev_ice(:)

    !> Predictor used to start each nonlinear iteration.
    !>
    !>   0  previous step (zeroth order)
    !>   1  linear extrapolation of T and p
    !>   2  linear extrapolation clipped at the freezing point
    !>   3  linear extrapolation of the conserved quantities, inverted for T
    !>
    !> Mode 1 is kept only as the measured counter-example: extrapolating T
    !> linearly through the phase boundary drove the second step to p = 4.2e5 Pa
    !> with a pressure increment of -3.5e9 Pa. T(t) has a kink where a node
    !> freezes, so a straight line through it lands far past the transition.
    integer(int32), parameter :: PREDICTOR_MODE = 0

contains
    module subroutine solve_time_step_initial_setup_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        real(real64), pointer, contiguous, dimension(:) :: u

        nullify (u)

        ! Reset iteration control
        ! reset() may set the compute solver to NONE when config is NONE.
        call self%control%reset_iteration()

        ! Anderson(1) history is only meaningful within one nonlinear loop.
        self%aa_has_prev = .false.
        self%aa_gnorm_prev = -1.0d0
        self%aa_use_count = 0
        self%last_line_search_failures = 0
        self%last_line_search_trials = 0
        self%last_line_search_scale = 1.0d0
        self%aa_gamma_max_abs = 0.0d0
        if (allocated(self%phase_onset_reset)) self%phase_onset_reset = .false.

        ! [Important] Compute solver must always be PICARD or NEWTON if not NONE.
        ! Even for linear config (where iter=1 is forced), Picard discretization
        ! is often the base, but if explicitly NONE, we should respect it.
        if (.not. self%control%is_none()) then
            call self%control%set_nonlinear_solver(NONLINEAR_SOLVER%PICARD)
        end if

        call self%control%increment_total()
        call self%control%reset_acceleration()

        ! Supply the discretization scales the residual floor is derived from.
        ! The mesh measure is fixed, so it is summed once and cached; dt changes
        ! every attempt, so the floor is refreshed here rather than at setup.
        block
            real(real64) :: dt_now, measure
            real(real64) :: atol_temperature_u, atol_pressure_u, rtol_u
            integer(int32) :: i_elem, num_elements
            logical :: first_setup

            first_setup = (self%domain_measure_total <= 0.0d0)
            if (first_setup) then
                call self%domain%get_num_fe(num_elements)
                self%domain_measure_total = 0.0d0
                do i_elem = 1, num_elements
                    measure = 0.0d0
                    call self%domain%calc_measure(i_elem, measure)
                    self%domain_measure_total = self%domain_measure_total + abs(measure)
                end do
                call build_nodal_volume(self, num_elements)
            end if
            call self%control%get_dt(dt_now)
            if (first_setup) then
                ! ATS error-control tolerances (Conditions.json:
                ! adaptive_stepping/error_control) are static run
                ! configuration, so they are read once here and cached inside
                ! convergence_control, reused as the primary-variable
                ! tolerance scale of the local error measure (see
                ! local_error_block in convergence_control.F90).
                call self%control%get_error_control_tolerances(atol_temperature_u, atol_pressure_u, rtol_u)
                call self%control%set_residual_scale(self%domain_measure_total, dt_now, self%nodal_volume, &
                                                     atol_temperature_u=atol_temperature_u, &
                                                     atol_pressure_u=atol_pressure_u, rtol_u=rtol_u)
            else
                call self%control%set_residual_scale(self%domain_measure_total, dt_now, self%nodal_volume)
            end if
        end block

        ! Save previous step values (Previous <- Current)
        call self%porosity%get_previous(u)
        if (associated(u)) then
            call self%porosity%set_current(u)
            nullify (u)
        end if

        if (self%control%is_physics_active(PHYSICS_TYPES%THERMAL)) then
            call self%temperature%get_previous(u)
            if (associated(u)) then
                call self%temperature%set_current(u)
                nullify (u)
            end if
        end if

        if (self%control%is_physics_active(PHYSICS_TYPES%HYDRAULIC)) then
            call self%pressure%get_previous(u)
            if (associated(u)) then
                call self%pressure%set_current(u)
                nullify (u)
            end if
        end if

        ! Restore derived phase fields too. A failed nonlinear attempt may leave
        ! Qw/Qi/Qa/Qv at the rejected iterate; retrying a step must start from the
        ! last accepted thermodynamic state, just like T and p.
        call self%Qw%get_previous(u)
        if (associated(u)) then
            call self%Qw%set_current(u)
            nullify (u)
        end if

        call self%Qi%get_previous(u)
        if (associated(u)) then
            call self%Qi%set_current(u)
            nullify (u)
        end if

        call self%Qa%get_previous(u)
        if (associated(u)) then
            call self%Qa%set_current(u)
            nullify (u)
        end if

        call self%Qv%get_previous(u)
        if (associated(u)) then
            call self%Qv%set_current(u)
            nullify (u)
        end if

    end subroutine solve_time_step_initial_setup_ftcms

    !> Extrapolate the accepted solution history to start the nonlinear
    !> iteration from a predicted state rather than from the previous step.
    !>
    !> The iteration otherwise starts at y_n, which is a zeroth-order predictor:
    !> the whole change over the step has to be produced by the Newton
    !> corrections. At the freezing onset that means one correction has to carry
    !> several hundred nodes across the phase boundary at once, which is exactly
    !> the step that overshoots. A linear predictor
    !>   y^(0) = y_n + (dt_{n+1}/dt_n) (y_n - y_{n-1})
    !> starts with the front already advanced, so the corrections stay small.
    !> After advance() the history holds y_n in slots 1 and 2 and y_{n-1} in
    !> slot 3, which is the convention apply_phase_predictor also uses.
    !>
    !> This changes the starting point only; the converged state is unchanged.
    subroutine apply_solution_predictor(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        !> Never extrapolate further than the step is allowed to grow.
        real(real64), parameter :: PREDICTOR_MAX_RATIO = 2.0d0
        real(real64) :: dt_new, ratio, history(3)
        real(real64), allocatable :: pressure_pred(:)
        real(real64), pointer, contiguous :: current(:)
        real(real64), pointer, contiguous :: bdf_coeffs(:)
        integer(int32) :: node_id, num_nodes, bdf_order

        ! Two genuine accepted steps are required before anything can be
        ! extrapolated. With only one, the oldest history slot still holds the
        ! initial condition, and y_1 - y_0 is the response to switching the
        ! boundary conditions on - a startup transient, not a rate. Predicting
        ! from it doubles that transient: measured, it drove the second step to
        ! p = 4.2e5 Pa. lte_has_second_difference is exactly the flag that says
        ! two steps have been committed.
        if (.not. self%lte_has_second_difference) return
        if (self%last_accepted_dt <= tiny(1.0d0)) return
        call self%control%get_dt(dt_new)
        if (dt_new <= 0.0d0) return
        ratio = min(dt_new / self%last_accepted_dt, PREDICTOR_MAX_RATIO)

        call self%domain%get_num_nodes(num_nodes)
        nullify (current, bdf_coeffs)

        ! Pressure is predicted linearly in every mode: p carries no phase-change
        ! kink of its own, the cryosuction kink lives in psi_cryo(T).
        allocate (pressure_pred(num_nodes), source=0.0d0)
        if (self%is_active_hydraulic()) then
            call self%pressure%get_current(current)
            if (associated(current)) then
                do node_id = 1, min(num_nodes, size(current))
                    call self%pressure%get_history(node_id, history)
                    pressure_pred(node_id) = min(max(history(2) + ratio * (history(2) - history(3)), &
                                                     WALL_PRESS_MIN_PA), WALL_PRESS_MAX_PA)
                    current(node_id) = pressure_pred(node_id)
                end do
            end if
            nullify (current)
        end if

        if (self%is_active_thermal()) then
            select case (PREDICTOR_MODE)
            case (1)
                call predict_temperature_linear(self, num_nodes, ratio)
            case (2)
                call predict_temperature_clipped(self, num_nodes, ratio, pressure_pred)
            case (3)
                call predict_temperature_from_enthalpy(self, num_nodes, dt_new, ratio, pressure_pred)
            end select
        end if

        if (allocated(pressure_pred)) deallocate (pressure_pred)

        ! Everything derived from the primary variables has to follow the
        ! prediction, or the first residual is assembled from a mixed state.
        call self%calc_gradient_temperature()
        call self%calc_gradient_pressure()
        call self%update_nodal_phases()
        call self%control%get_bdf_coeffs(bdf_order, bdf_coeffs)
        if (self%is_active_thermal()) call self%temperature%compute_time_derivative(bdf_coeffs, bdf_order)
        if (self%is_active_hydraulic()) call self%pressure%compute_time_derivative(bdf_coeffs, bdf_order)
    end subroutine apply_solution_predictor

    !> Mode 1: straight line through the last two accepted temperatures.
    subroutine predict_temperature_linear(self, num_nodes, ratio)
        implicit none
        class(type_ftcms), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        real(real64), intent(in) :: ratio

        real(real64), pointer, contiguous :: current(:)
        real(real64) :: history(3)
        integer(int32) :: node_id

        nullify (current)
        call self%temperature%get_current(current)
        if (.not. associated(current)) return
        do node_id = 1, min(num_nodes, size(current))
            call self%temperature%get_history(node_id, history)
            current(node_id) = min(max(history(2) + ratio * (history(2) - history(3)), &
                                       WALL_TEMP_MIN_C), WALL_TEMP_MAX_C)
        end do
        nullify (current)
    end subroutine predict_temperature_linear

    !> Mode 2: the same line, stopped at the freezing point.
    !>
    !> A node still above its pressure-dependent freezing temperature is
    !> predicted no further than that temperature. The prediction may bring the
    !> front up to the transition but never through it, which is where the
    !> straight line stops being an extrapolation of anything: the latent heat
    !> holds T near T_crit while enthalpy keeps falling, so the slope on the far
    !> side bears no relation to the slope on this one. Crossing is left to the
    !> Newton correction, which has the apparent heat capacity to price it.
    subroutine predict_temperature_clipped(self, num_nodes, ratio, pressure_pred)
        implicit none
        class(type_ftcms), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        real(real64), intent(in) :: ratio
        real(real64), intent(in) :: pressure_pred(:)

        type(type_state) :: state
        real(real64), pointer, contiguous :: current(:)
        real(real64) :: history(3), predicted, rho_water, critical_temperature
        integer(int32) :: node_id, row_start, repr_elem

        nullify (current)
        call self%temperature%get_current(current)
        if (.not. associated(current)) return

        do node_id = 1, min(num_nodes, size(current))
            call self%temperature%get_history(node_id, history)
            predicted = min(max(history(2) + ratio * (history(2) - history(3)), &
                                WALL_TEMP_MIN_C), WALL_TEMP_MAX_C)

            if (predicted < history(2)) then
                row_start = self%node_material_table%ptr(node_id)
                if (row_start < self%node_material_table%ptr(node_id + 1)) then
                    repr_elem = self%node_material_table%repr_element(row_start)
                    call self%set_state(node_id, repr_elem, state, calc_physics=.false., include_fluxes=.false.)
                    call self%thermal%calc_density_water(state, rho_water)
                    call calc_T_high_celsius(pressure_pred(node_id), rho_water, critical_temperature)
                    if (history(2) > critical_temperature .and. predicted < critical_temperature) then
                        predicted = critical_temperature
                    end if
                end if
            end if
            current(node_id) = predicted
        end do
        nullify (current)
    end subroutine predict_temperature_clipped

    !> Mode 3: extrapolate the conserved quantity, then invert it for T.
    !>
    !> Enthalpy has no kink at the transition - that is what makes the enthalpy
    !> formulation well behaved - so a straight line through it is a sound
    !> extrapolation everywhere, including across freezing. Inverting
    !> U(T, p) = H_pred at the predicted pressure then places T wherever the
    !> latent heat says it belongs: a node absorbing latent heat has its
    !> enthalpy advanced while its temperature stays near T_crit, which is the
    !> behaviour a temperature extrapolation cannot produce.
    !>
    !> The enthalpy history is the one the local-error estimator already keeps.
    subroutine predict_temperature_from_enthalpy(self, num_nodes, dt_new, ratio, pressure_pred)
        implicit none
        class(type_ftcms), intent(inout) :: self
        integer(int32), intent(in) :: num_nodes
        real(real64), intent(in) :: dt_new, ratio
        real(real64), intent(in) :: pressure_pred(:)

        integer(int32), parameter :: MAX_INVERSION_ITER = 20
        type(type_state) :: state
        real(real64), pointer, contiguous :: current(:)
        real(real64) :: history(3), target_enthalpy, temperature, enthalpy, capacity
        real(real64) :: increment, tolerance
        integer(int32) :: node_id, row_start, repr_elem, material_id, iter
        logical :: converged

        if (.not. allocated(self%lte_state_prev_thermal)) then
            call predict_temperature_clipped(self, num_nodes, ratio, pressure_pred)
            return
        end if
        if (.not. allocated(self%lte_ydot_prev_thermal)) then
            call predict_temperature_clipped(self, num_nodes, ratio, pressure_pred)
            return
        end if

        nullify (current)
        call self%temperature%get_current(current)
        if (.not. associated(current)) return
        if (size(self%lte_state_prev_thermal) /= size(current)) then
            call predict_temperature_clipped(self, num_nodes, ratio, pressure_pred)
            return
        end if

        do node_id = 1, min(num_nodes, size(current))
            call self%temperature%get_history(node_id, history)
            target_enthalpy = self%lte_state_prev_thermal(node_id) + &
                              dt_new * self%lte_ydot_prev_thermal(node_id)

            row_start = self%node_material_table%ptr(node_id)
            if (row_start >= self%node_material_table%ptr(node_id + 1)) cycle
            repr_elem = self%node_material_table%repr_element(row_start)
            call self%domain%get_material_id(repr_elem, material_id)
            call self%set_state(node_id, repr_elem, state, calc_physics=.false., include_fluxes=.false.)
            call state%pressure%set(pressure_pred(node_id))

            ! Absolute enthalpy tolerance: the same scale the acceptance gate
            ! uses for the thermal conserved quantity.
            tolerance = 1.0d0
            temperature = history(2)
            converged = .false.
            do iter = 1, MAX_INVERSION_ITER
                call state%temperature%set(temperature)
                call self%thermal%update_water_phases(material_id, state)
                call self%thermal%calc_enthalpy_density(material_id, state, enthalpy)
                if (abs(enthalpy - target_enthalpy) <= tolerance) then
                    converged = .true.
                    exit
                end if
                call self%thermal%compute_mass_term(material_id, state, capacity)
                if (abs(capacity) <= tiny(1.0d0)) exit
                increment = -(enthalpy - target_enthalpy) / capacity
                temperature = min(max(temperature + increment, WALL_TEMP_MIN_C), WALL_TEMP_MAX_C)
            end do

            if (converged) then
                current(node_id) = temperature
            else
                current(node_id) = history(2)
            end if
        end do
        nullify (current)
    end subroutine predict_temperature_from_enthalpy

    !> Extrapolate the accepted ice history to provide the next outer-loop
    !> initial guess. The bounded projection and the monolithic T-p solve still
    !> determine the accepted state, so this changes iteration count only.
    subroutine apply_phase_predictor(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        integer(int32) :: node_id
        real(real64) :: dt, ratio, predicted_increment, ice_history(3)
        real(real64), pointer, contiguous, dimension(:) :: current_ice, current_porosity

        if (self%last_accepted_dt <= tiny(1.0d0)) return

        call self%control%get_dt(dt)
        ratio = min(max(dt / self%last_accepted_dt, 0.0d0), 1.0d0)
        nullify (current_ice, current_porosity)
        call self%Qi%get_current(current_ice)
        call self%porosity%get_current(current_porosity)
        do node_id = 1, size(current_ice)
            call self%Qi%get_history(node_id, ice_history)
            predicted_increment = ratio * (ice_history(2) - ice_history(3))
            current_ice(node_id) = min(max(ice_history(2) + predicted_increment, 0.0d0), &
                                       current_porosity(node_id))
        end do
        nullify (current_ice, current_porosity)

        call self%update_nodal_phases()
    end subroutine apply_phase_predictor

    module subroutine solve_time_step_setup_ftcms(self, prescribe_bc)
        implicit none
        class(type_ftcms), intent(inout) :: self
        logical, intent(inout) :: prescribe_bc

        integer(int32) :: iter

        call self%control%increment_nonlinear()
        call self%control%get_nonlinear_iter(iter)

        if (iter == 1) then
            prescribe_bc = .true.
        else
            prescribe_bc = .false.
        end if

        ! No gradient recomputation here: the nodal T/P gradients are already
        ! current at this point. Every T/P mutation is followed by a
        ! projection -- reflect_variables recomputes both gradients after the
        ! update (end of each nonlinear iteration), and the first iteration
        ! recomputes them right after prescribe_dirichlet (before assembly).
        ! Recomputing from unchanged inputs here produced identical values.

    end subroutine solve_time_step_setup_ftcms

    module subroutine solve_time_step_check_convergence_ftcms(self, target_physics)
        implicit none
        class(type_ftcms), intent(inout), target :: self
        type(type_constant_id), intent(in), optional :: target_physics

        real(real64), pointer, contiguous, dimension(:) :: current_value

        real(real64), allocatable :: residual(:)
        real(real64), allocatable :: increment(:)
        real(real64) :: relaxation_factor
        logical, parameter :: diverged = .true.

        logical :: check_thermal, check_hydraulic

        nullify (current_value)

        ! Conserved-quantity mode runs its (post-update) check separately; the
        ! residual/update per-physics check below does not apply to it.
        if (self%control%is_conserved()) return

        check_thermal = self%control%is_physics_active(PHYSICS_TYPES%THERMAL)
        check_hydraulic = self%control%is_physics_active(PHYSICS_TYPES%HYDRAULIC)

        if (present(target_physics)) then
            check_thermal = check_thermal .and. (target_physics%ID == PHYSICS_TYPES%THERMAL%ID)
            check_hydraulic = check_hydraulic .and. (target_physics%ID == PHYSICS_TYPES%HYDRAULIC%ID)

            if (.not. check_thermal .and. self%control%is_physics_active(PHYSICS_TYPES%THERMAL)) then
                call self%control%set_converged(PHYSICS_TYPES%THERMAL, .true.)
                call self%control%set_diverged(PHYSICS_TYPES%THERMAL, .false.)
            end if
            if (.not. check_hydraulic .and. self%control%is_physics_active(PHYSICS_TYPES%HYDRAULIC)) then
                call self%control%set_converged(PHYSICS_TYPES%HYDRAULIC, .true.)
                call self%control%set_diverged(PHYSICS_TYPES%HYDRAULIC, .false.)
            end if
        end if

        ! ----------------------------------------------------------------------
        ! Thermal Convergence Check
        ! ----------------------------------------------------------------------
        if (check_thermal) then
            call self%get_variable_residual(PHYSICS_TYPES%THERMAL, residual)
            call self%get_variable_increment(PHYSICS_TYPES%THERMAL, increment)

            if (.not. allocated(residual) .or. .not. allocated(increment) .or. &
                size(residual) == 0 .or. size(increment) == 0) then
                write (*, *) "Error: Thermal residual/increment is unavailable during convergence check."
                call self%control%set_diverged(PHYSICS_TYPES%THERMAL, diverged)
            else if (has_nan(residual) .or. has_nan(increment)) then
                write (*, *) "Error: NaN detected in thermal variables during convergence check."
                call self%control%set_diverged(PHYSICS_TYPES%THERMAL, diverged)
            else
                call self%control%get_current_relaxation(PHYSICS_TYPES%THERMAL, relaxation_factor)
                increment(:) = relaxation_factor * increment(:)
                call self%control%check_convergence(PHYSICS_TYPES%THERMAL, residual, increment)
            end if

        end if

        ! ----------------------------------------------------------------------
        ! Hydraulic Convergence Check
        ! ----------------------------------------------------------------------
        if (check_hydraulic) then
            call self%get_variable_residual(PHYSICS_TYPES%HYDRAULIC, residual)
            call self%get_variable_increment(PHYSICS_TYPES%HYDRAULIC, increment)

            if (.not. allocated(residual) .or. .not. allocated(increment) .or. &
                size(residual) == 0 .or. size(increment) == 0) then
                write (*, *) "Error: Hydraulic residual/increment is unavailable during convergence check."
                call self%control%set_diverged(PHYSICS_TYPES%HYDRAULIC, diverged)
            else if (has_nan(residual) .or. has_nan(increment)) then
                write (*, *) "Error: NaN detected in hydraulic variables during convergence check."
                call self%control%set_diverged(PHYSICS_TYPES%HYDRAULIC, diverged)
            else
                call self%control%get_current_relaxation(PHYSICS_TYPES%HYDRAULIC, relaxation_factor)
                increment(:) = relaxation_factor * increment(:)
                call self%control%check_convergence(PHYSICS_TYPES%HYDRAULIC, residual, increment)
            end if
        end if

        if (allocated(increment)) call deallocate_array(increment)
        if (allocated(residual)) call deallocate_array(residual)

    end subroutine solve_time_step_check_convergence_ftcms

    !> Conserved-quantity convergence check (PDF 6.2.4), evaluated on the updated
    !> state. Builds the nodal enthalpy/effective-density fields and the per-block
    !> residual vectors, then delegates the coupled decision to the control manager.
    module subroutine solve_time_step_check_convergence_conserved_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        real(real64), allocatable :: enthalpy(:)
        real(real64), allocatable :: density(:)
        real(real64), allocatable :: dH_dT(:)
        real(real64), allocatable :: drho_dp(:)
        real(real64), allocatable :: residual_thermal(:)
        real(real64), allocatable :: residual_hydraulic(:)
        logical :: check_thermal, check_hydraulic
        real(real64), pointer, contiguous, dimension(:) :: field
        ! Wall-contact tolerances: the bounded update lands exactly ON the wall
        ! when it binds, so a small absolute buffer is sufficient to detect it.
        real(real64), parameter :: TEMP_WALL_TOL = 1.0d-6
        real(real64), parameter :: PRESS_WALL_TOL = 1.0d-1

        check_thermal = self%is_active_thermal()
        check_hydraulic = self%is_active_hydraulic()

        ! Physical-validity guard: an iterate pinned at the sanity walls by the
        ! bounded update (reflect_variables) is outside the model's validity.
        ! Such a state must never be accepted as converged; declare divergence
        ! so the step fails and the ATS retries with a smaller dt.
        nullify (field)
        if (check_thermal) then
            call self%temperature%get_current(field)
            if (associated(field)) then
                if (minval(field) <= WALL_TEMP_MIN_C + TEMP_WALL_TOL .or. &
                    maxval(field) >= WALL_TEMP_MAX_C - TEMP_WALL_TOL) then
                    write (*, '(A,2(ES11.3,1X))') '   [GUARD] temperature pinned at validity wall; T min/max = ', &
                        minval(field), maxval(field)
                    call self%control%set_converged(PHYSICS_TYPES%THERMAL, .false.)
                    call self%control%set_diverged(PHYSICS_TYPES%THERMAL, .true.)
                    nullify (field)
                    return
                end if
            end if
            nullify (field)
        end if
        if (check_hydraulic) then
            call self%pressure%get_current(field)
            if (associated(field)) then
                if (minval(field) <= WALL_PRESS_MIN_PA + PRESS_WALL_TOL .or. &
                    maxval(field) >= WALL_PRESS_MAX_PA - PRESS_WALL_TOL) then
                    block
                        integer(int32) :: node_lo, node_hi
                        real(real64), pointer, contiguous, dimension(:) :: T_dbg
                        real(real64) :: T_lo, T_hi
                        node_lo = minloc(field, dim=1)
                        node_hi = maxloc(field, dim=1)
                        T_lo = 0.0d0
                        T_hi = 0.0d0
                        nullify (T_dbg)
                        call self%temperature%get_current(T_dbg)
                        if (associated(T_dbg)) then
                            T_lo = T_dbg(node_lo)
                            T_hi = T_dbg(node_hi)
                            nullify (T_dbg)
                        end if
                        write (*, '(A,2(ES11.3,1X),A,I0,A,ES11.3,A,I0,A,ES11.3)') &
                            '   [GUARD] pressure pinned at validity wall; P min/max = ', &
                            minval(field), maxval(field), ' node_min=', node_lo, ' T=', T_lo, &
                            ' node_max=', node_hi, ' T=', T_hi
                    end block
                    call self%control%set_converged(PHYSICS_TYPES%HYDRAULIC, .false.)
                    call self%control%set_diverged(PHYSICS_TYPES%HYDRAULIC, .true.)
                    nullify (field)
                    return
                end if
            end if
            nullify (field)
        end if

        ! Nodal conserved quantities at the updated iterate, together with the
        ! matching nodal storage sensitivities (dH/dT, d rho_eq/dp) the
        ! primary-variable local error measure needs (local_error_block).
        ! Both are as state-dependent as enthalpy/density themselves, so they
        ! are recomputed at the same cadence, here, rather than once at setup.
        call self%compute_nodal_conserved(enthalpy, density, dH_dT, drho_dp)

        ! Per-block residuals from the assembly at the updated iterate.
        if (check_thermal) call self%get_variable_residual(PHYSICS_TYPES%THERMAL, residual_thermal)
        if (check_hydraulic) call self%get_variable_residual(PHYSICS_TYPES%HYDRAULIC, residual_hydraulic)

        ! Refresh the convergence gate's cached sensitivity/primary-variable
        ! snapshot before the gate below reads them. nodal_volume and the ATS
        ! tolerances are left untouched: unsupplied optional arguments keep
        ! the values set once at solve_time_step_initial_setup. A disassociated
        ! pointer (inactive physics) is treated as an absent optional argument.
        block
            real(real64), pointer, contiguous, dimension(:) :: T_current, p_current
            real(real64) :: dt_now

            nullify (T_current, p_current)
            if (check_thermal) call self%temperature%get_current(T_current)
            if (check_hydraulic) call self%pressure%get_current(p_current)
            call self%control%get_dt(dt_now)
            call self%control%set_residual_scale(self%domain_measure_total, dt_now, &
                                                 dH_dT=dH_dT, drho_dp=drho_dp, &
                                                 u_thermal=T_current, u_hydraulic=p_current)
            nullify (T_current, p_current)
        end block

        ! Unallocated residual arrays propagate as absent optional arguments.
        call self%control%check_convergence_conserved(enthalpy, density, &
                                                      residual_thermal, residual_hydraulic, &
                                                      check_thermal, check_hydraulic)

        ! Pore-overflow guard: the accepted nodal water+ice content must not
        ! exceed the pore space. Transient iterates within the nonlinear loop
        ! are allowed to overshoot (phase_systems.F90 no longer clamps them),
        ! so the check runs only once the convergence gate above has declared
        ! the iterate acceptable; a violation then vetoes the acceptance via
        ! the same set_diverged/dt-retry mechanism as the wall-pinning guards.
        if (check_hydraulic .and. self%control%is_converged()) then
            block
                real(real64), pointer, contiguous, dimension(:) :: Qw_field, Qi_field, porosity_field
                real(real64) :: excess, worst_excess
                integer(int32) :: i_node, worst_node
                real(real64), parameter :: PORE_OVERFLOW_TOL = 1.0d-8
                ! Diagnostic switch: report the overflow but do not veto the
                ! acceptance. The rigid-pore model has no pressure-relief
                ! physics, so freezing a near-saturated node necessarily
                ! overflows the pore volume by up to (rho_w/rho_i - 1)*Theta;
                ! warn-only runs measure how large that overflow actually
                ! grows before deciding the rejection policy.
                logical, parameter :: PORE_OVERFLOW_WARN_ONLY = .false.

                nullify (Qw_field, Qi_field, porosity_field)
                call self%Qw%get_current(Qw_field)
                call self%Qi%get_current(Qi_field)
                call self%porosity%get_current(porosity_field)
                if (associated(Qw_field) .and. associated(Qi_field) .and. associated(porosity_field)) then
                    worst_excess = PORE_OVERFLOW_TOL
                    worst_node = 0
                    do i_node = 1, size(porosity_field)
                        excess = Qw_field(i_node) + Qi_field(i_node) - porosity_field(i_node)
                        if (excess > worst_excess) then
                            worst_excess = excess
                            worst_node = i_node
                        end if
                    end do
                    if (worst_node > 0) then
                        write (*, '(A,I0,A,ES11.3)') &
                            '   [GUARD] pore-volume overflow at accepted step; node=', worst_node, &
                            ' excess=', worst_excess
                        if (.not. PORE_OVERFLOW_WARN_ONLY) then
                            call self%control%set_converged(PHYSICS_TYPES%HYDRAULIC, .false.)
                            call self%control%set_diverged(PHYSICS_TYPES%HYDRAULIC, .true.)
                        end if
                    end if
                end if
                nullify (Qw_field, Qi_field, porosity_field)
            end block
        end if

        if (allocated(enthalpy)) deallocate (enthalpy)
        if (allocated(density)) deallocate (density)
        if (allocated(dH_dT)) deallocate (dH_dT)
        if (allocated(drho_dp)) deallocate (drho_dp)
        if (allocated(residual_thermal)) deallocate (residual_thermal)
        if (allocated(residual_hydraulic)) deallocate (residual_hydraulic)
    end subroutine solve_time_step_check_convergence_conserved_ftcms

    module subroutine solve_time_step_ftcms(self, is_step_converged)
        implicit none
        class(type_ftcms), intent(inout) :: self
        logical, intent(inout) :: is_step_converged
        logical :: prescribe_bc
        integer(int32) :: iter_nl
        real(real64) :: t_res, t_inc, h_res, h_inc

        ! The primary unknowns remain T and p. Ice is eliminated locally by a
        ! bounded constitutive return map after each coupled Modified Picard
        ! update; no ice DOF is added to the global block structure.
        logical :: do_phase_outer
        integer(int32) :: coupling_iter, node_id
        ! Keep the former outer solver available as a diagnostic fallback.
        ! The production path applies the same local return map at every
        ! Modified Picard iterate, together with its T-p tangent assembled in
        ! phase_systems.F90. Fully converging a fixed-ice T-p problem before
        ! changing ice discards that tangent and causes the measured period-two
        ! front oscillation.
        logical, parameter :: PHASE_USE_OUTER_PROJECTION = .false.
        ! Allow a contracting phase map to finish before ATS reduces dt.
        integer(int32), parameter :: MAX_PHASE_ITER = 240
        ! 5 kPa corresponds to about 4e-3 K through Clapeyron near 0 C,
        ! below the verified spatial temperature discretization error.
        real(real64), parameter :: PHASE_PRESSURE_TOL = 5.0d3
        ! The phase equation is a water-content equality. This tolerance is
        ! consistent with the configured hydraulic conserved-quantity scale.
        real(real64), parameter :: PHASE_CONTENT_TOL = 1.0d-3
        ! A local defect must not be hidden by the volume-weighted norm at the
        ! thin freezing front. Use the same physical content scale locally and
        ! globally; otherwise an ice-free node could accept a finite freezing
        ! target solely because its support is small.
        real(real64), parameter :: PHASE_CONTENT_MAX_TOL = PHASE_CONTENT_TOL
        real(real64), parameter :: PHASE_MIXING = 0.3d0
        real(real64), parameter :: PHASE_AA_GAMMA_MAX = 2.0d0
        real(real64), parameter :: PHASE_AA_STEP_GROWTH_MAX = 4.0d0
        real(real64), parameter :: PHASE_STEP_FLOOR = 1.0d-5
        integer(int32), parameter :: PHASE_STAGNATION_LIMIT = 40
        real(real64) :: phase_increment_max, phase_increment_norm
        real(real64) :: phase_temperature, phase_pressure
        real(real64) :: phase_current_ice, phase_projected_ice
        real(real64) :: phase_equilibrium_error
        real(real64) :: phase_step_limit, phase_step_max, phase_step_factor
        real(real64) :: phase_mixing_current
        real(real64) :: phase_aa_gamma
        real(real64) :: phase_merit, phase_best_merit
        ! --- Backtracking line-search state ---
        ! The measured descent region lies below alpha ~ 1e-5; backtracking by
        ! 1/4 reaches 2.4e-7 within MAX_TRIALS.
        integer(int32), parameter :: LINE_SEARCH_MAX_TRIALS = 12
        real(real64), parameter :: LINE_SEARCH_BACKTRACK = 2.5d-1
        real(real64), parameter :: LINE_SEARCH_MIN_SCALE = 1.0d-6
        !> Armijo sufficient-decrease coefficient on the residual norm.
        real(real64), parameter :: LINE_SEARCH_ARMIJO = 1.0d-4
        integer(int32) :: ls_trial, ls_diag
        real(real64) :: ls_scale, ls_reference_norm, ls_trial_norm, ls_aa_gnorm
        !> Per-block local error measure at the trial iterate (E_T, E_H; see
        !> local_error_block) and the mean-volume-floor fallback scale used
        !> only when the exact per-node data is not yet available.
        real(real64) :: ls_E_T, ls_E_H
        real(real64) :: ls_ref0_thermal, ls_ref0_hydraulic
        !> Per-trial backtracking record, kept only to separate a wrong direction
        !> from a step length the search does not actually control.
        real(real64) :: ls_trial_alpha(LINE_SEARCH_MAX_TRIALS)
        real(real64) :: ls_trial_ratio(LINE_SEARCH_MAX_TRIALS)
        real(real64) :: ls_trial_E_T(LINE_SEARCH_MAX_TRIALS)
        real(real64) :: ls_trial_E_H(LINE_SEARCH_MAX_TRIALS)
        logical :: ls_accepted, ls_aa_has_prev
        real(real64), allocatable :: ls_T_saved(:), ls_P_saved(:)
        real(real64), allocatable :: ls_aa_T(:), ls_aa_P(:), ls_aa_duT(:), ls_aa_duP(:)
        logical, allocatable :: ls_onset_saved(:)
        real(real64), allocatable :: initial_residual_thermal(:), initial_residual_hydraulic(:)
        !> Per-node frozen flags, used only by the active-set telemetry.
        logical, allocatable :: frozen_now(:), frozen_previous(:)
        real(real64), allocatable :: phase_increments(:), previous_phase_increments(:)
        real(real64), allocatable :: phase_update(:), previous_phase_update(:)
        integer(int32), allocatable :: phase_active_bounds(:)
        integer(int32) :: phase_max_node, num_phase_nodes
        integer(int32) :: num_active_nodes
        integer(int32) :: phase_stagnation_count
        logical :: linear_failed, phase_is_converged, phase_aa_usable
        logical :: phase_final_correction_applied
        logical :: min_dt_extension_announced
        integer(int32) :: max_iter_config, min_dt_iter_limit
        integer(int32), parameter :: MIN_DT_ITER_FACTOR = 5


        is_step_converged = .false.
        ls_ref0_thermal = 0.0d0
        ls_ref0_hydraulic = 0.0d0
        ls_E_T = 0.0d0
        ls_E_H = 0.0d0

        self%last_phase_iterations = 1
        self%last_inner_iterations = 0
        self%last_max_inner_iterations = 0
        self%last_nonlinear_work = 0
        self%last_solve_status = SOLVE_STATUS_NOT_RUN
        self%last_phase_metrics_available = .false.
        self%last_phase_converged = .false.
        self%last_phase_active_nodes = -1
        self%last_phase_increment_max = -1.0d0
        self%last_phase_increment_norm = -1.0d0
        self%last_phase_equilibrium_error = -1.0d0
        self%last_phase_merit = -1.0d0

        ! Dispatch to true staggered solver (H then T sequential nonlinear loops)
        if (self%control%is_staggered() .and. &
            self%is_active_thermal() .and. self%is_active_hydraulic()) then
            call self%solve_time_step_staggered(is_step_converged)
            return
        end if

        do_phase_outer = self%is_active_thermal() .and. self%is_active_hydraulic() .and. PHASE_USE_OUTER_PROJECTION
        phase_final_correction_applied = .false.

        ! Initialize per-time-step state only once.
        call self%solve_time_step_initial_setup()
        if (PREDICTOR_MODE > 0) call apply_solution_predictor(self)
        if (do_phase_outer) call apply_phase_predictor(self)
        call self%domain%get_num_nodes(num_phase_nodes)
        allocate (phase_update(num_phase_nodes), source=0.0d0)
        allocate (frozen_now(num_phase_nodes), source=.false.)
        allocate (frozen_previous(num_phase_nodes), source=.false.)
        allocate (previous_phase_update(num_phase_nodes), source=0.0d0)
        phase_mixing_current = PHASE_MIXING
        phase_best_merit = huge(1.0d0)
        phase_stagnation_count = 0
        ! Outer phase projection loop
        coupling_loop: do coupling_iter = 1, merge(MAX_PHASE_ITER, 1, do_phase_outer)
            self%last_phase_iterations = coupling_iter
            linear_failed = .false.

            ! For coupling iterations > 1, reset nonlinear controls only.
            if (coupling_iter > 1) then
                call self%control%reset_iteration()
                call self%control%set_nonlinear_solver(NONLINEAR_SOLVER%PICARD)
                call self%control%increment_total()
                call self%control%reset_acceleration()
                ! The local ice projection changes the fixed-point map between
                ! outer iterations. Retain the per-attempt AA telemetry, but
                ! never reuse a T-p secant pair across two different ice states.
                self%aa_has_prev = .false.
                self%aa_gnorm_prev = -1.0d0
            end if

            call self%control%get_max_iterations(max_iter_config)
            min_dt_iter_limit = max_iter_config
            if (do_phase_outer .and. self%control%is_conserved()) then
                min_dt_iter_limit = max(max_iter_config, MIN_DT_ITER_FACTOR * max_iter_config)
            end if
            if (self%control%is_min_dt() .and. self%control%is_conserved()) then
                min_dt_iter_limit = max(max_iter_config, MIN_DT_ITER_FACTOR * max_iter_config)
            end if
            min_dt_extension_announced = .false.

            ! Nonlinear iteration loop
            nonlinear: do
                if (.not. self%control%should_continue()) then
                    call self%control%get_nonlinear_iter(iter_nl)
                    if (self%control%is_conserved() .and. &
                        (.not. self%control%is_converged()) .and. (.not. self%control%is_diverged()) .and. &
                        iter_nl < min_dt_iter_limit) then
                        if (.not. min_dt_extension_announced) then
                            write (*, '(A,I0,A,I0,A)') '   [NONLINEAR] conserved continuation: iter limit ', &
                                max_iter_config, ' -> ', min_dt_iter_limit, ' while conserved iteration is still contracting.'
                            min_dt_extension_announced = .true.
                        end if
                    else
                        exit nonlinear
                    end if
                end if

                ! Setup (update iteration counter)
                call self%solve_time_step_setup(prescribe_bc)

                ! Prescribe Dirichlet values before assembly so gradients reflect BCs
                if (prescribe_bc) then
                    call self%prescribe_dirichlet()
                    call self%calc_gradient_temperature()
                    call self%calc_gradient_pressure()
                end if

                ! Assemble matrices and residual
                call self%assemble()

                ! Apply boundary conditions (natural + essential) to the linear system
                call self%apply_bc(prescribed=.false.)

                call self%control%get_nonlinear_iter(iter_nl)

                ! Relative residuals must be normalized by R(x_0), assembled
                ! before any nonlinear update in this fixed-phase subproblem.
                if (self%control%is_conserved() .and. iter_nl == 1) then
                    if (self%is_active_thermal()) then
                        call self%get_variable_residual(PHYSICS_TYPES%THERMAL, initial_residual_thermal)
                    end if
                    if (self%is_active_hydraulic()) then
                        call self%get_variable_residual(PHYSICS_TYPES%HYDRAULIC, initial_residual_hydraulic)
                    end if
                    call self%control%prime_conserved_residual(initial_residual_thermal, &
                                                               initial_residual_hydraulic, &
                                                               self%is_active_thermal(), &
                                                               self%is_active_hydraulic())
                    ! Fallback merit scale for the line search, used only when
                    ! local_error_block's exact per-node measure is not yet
                    ! available (nodal_volume or the storage-sensitivity
                    ! snapshot missing - only possible during the first
                    ! iterations of the very first step, before
                    ! solve_time_step_check_convergence_conserved has run
                    ! once). It must NOT be the step-initial residual norm
                    ! itself: a block that starts the step already satisfied
                    ! has ||R^0|| at round-off - measured, ||R_H^0|| = 2.75e-11
                    ! against ||R_T^0|| = 2.48e-01, ten orders apart - so
                    ! dividing by it makes any later residual in that block
                    ! dominate the merit no matter how small it is physically.
                    ! Scanned along the Newton direction at a stalled iterate,
                    ! the thermal block fell 33 percent at alpha = 0.46 while
                    ! the ratio-normalized hydraulic block outweighed it 2000
                    ! to 1 and pinned the search at alpha = 0.007. The floor
                    ! below is the imbalance a block may carry over the step
                    ! without moving its conserved quantity past its own
                    ! absolute tolerance; it does not collapse when the block
                    ! starts converged.
                    ls_ref0_thermal = 0.0d0
                    ls_ref0_hydraulic = 0.0d0
                    call self%control%get_residual_floors(num_phase_nodes, ls_ref0_thermal, ls_ref0_hydraulic)
                    if (allocated(initial_residual_thermal)) deallocate (initial_residual_thermal)
                    if (allocated(initial_residual_hydraulic)) deallocate (initial_residual_hydraulic)
                end if

                ! Reference merit for the line search, taken BEFORE the linear
                ! solve: solve() calls jacobi_equilibrate_bsr, which scales K and
                ! F in place to fix the 1e13 T/p conditioning disparity. Only du
                ! is unscaled afterwards, so F is left equilibrated and reading it
                ! after the solve compares a scaled residual against a freshly
                ! assembled unscaled one.
                if (self%control%is_conserved()) then
                    call ls_block_norms(self, ls_E_T, ls_E_H, ls_ref0_thermal, ls_ref0_hydraulic)
                    ls_reference_norm = ls_merit(ls_E_T, ls_E_H)
                end if

                ! Linear solve (K * du = F)
                call self%solve()

                ! If linear solver failed, mark as diverged and exit
                if (.not. self%solver%is_success()) then
                    linear_failed = .true.
                    if (self%is_active_thermal()) then
                        call self%control%set_converged( &
                            PHYSICS_TYPES%THERMAL, .false.)
                        call self%control%set_diverged( &
                            PHYSICS_TYPES%THERMAL, .true.)
                    end if
                    if (self%is_active_hydraulic()) then
                        call self%control%set_converged( &
                            PHYSICS_TYPES%HYDRAULIC, .false.)
                        call self%control%set_diverged( &
                            PHYSICS_TYPES%HYDRAULIC, .true.)
                    end if
                    exit nonlinear
                end if

                ! Convergence check; always converged when config is NONE
                call self%solve_time_step_check_convergence()

                if (self%control%is_conserved()) then
                    ! Backtracking line search on the assembled residual norm.
                    !
                    ! The Picard direction is not guaranteed to be a descent
                    ! direction at full step where the freezing curve is steep,
                    ! and a heuristic that damps only after detecting stagnation
                    ! is always one step behind. Choosing the step length so that
                    ! ||R|| actually decreases makes progress monotone by
                    ! construction and, when no admissible step exists, says so
                    ! immediately - that distinguishes a step-length problem from
                    ! a wrong-direction problem instead of hiding both.
                    !
                    ! The system was assembled at x_k, so its residual norm is
                    ! the reference. Each trial restores x_k, applies the scaled
                    ! update, and reassembles: the reassembly was already being
                    ! done once per iteration, so a converging search costs
                    ! nothing extra and only hard iterates pay for retries.
                    if (RESIDUAL_SCAN .and. .not. residual_scan_done .and. iter_nl >= RESIDUAL_SCAN_ITER) then
                        residual_scan_done = .true.
                        call scan_residual_along_direction(self, iter_nl, ls_ref0_thermal, ls_ref0_hydraulic)
                    end if

                    call ls_snapshot(self, ls_T_saved, ls_P_saved, ls_onset_saved, &
                                     ls_aa_T, ls_aa_P, ls_aa_duT, ls_aa_duP, &
                                     ls_aa_has_prev, ls_aa_gnorm)
                    ls_scale = 1.0d0
                    ls_accepted = .false.
                    ls_trial_ratio(:) = -1.0d0
                    ls_trial_alpha(:) = -1.0d0
                    ls_trial_E_T(:) = -1.0d0
                    ls_trial_E_H(:) = -1.0d0
                    do ls_trial = 1, LINE_SEARCH_MAX_TRIALS
                        if (ls_trial > 1) then
                            call ls_restore(self, ls_T_saved, ls_P_saved, ls_onset_saved, &
                                            ls_aa_T, ls_aa_P, ls_aa_duT, ls_aa_duP, &
                                            ls_aa_has_prev, ls_aa_gnorm)
                        end if
                        call self%reflect_variables(step_scale=ls_scale)
                        call self%assemble()
                        call self%apply_bc(prescribed=.false.)
                        call ls_block_norms(self, ls_E_T, ls_E_H, ls_ref0_thermal, ls_ref0_hydraulic)
                        ls_trial_norm = ls_merit(ls_E_T, ls_E_H)
                        if (ls_reference_norm <= 0.0d0) then
                            ls_accepted = .true.
                            exit
                        end if
                        ls_trial_alpha(ls_trial) = ls_scale
                        ls_trial_ratio(ls_trial) = ls_trial_norm / ls_reference_norm
                        ls_trial_E_T(ls_trial) = ls_E_T
                        ls_trial_E_H(ls_trial) = ls_E_H
                        if (ls_trial_norm <= (1.0d0 - LINE_SEARCH_ARMIJO * ls_scale) * ls_reference_norm) then
                            ls_accepted = .true.
                            exit
                        end if
                        if (ls_scale <= LINE_SEARCH_MIN_SCALE) exit
                        ls_scale = max(LINE_SEARCH_MIN_SCALE, LINE_SEARCH_BACKTRACK * ls_scale)
                    end do
                    self%last_line_search_scale = ls_scale
                    self%last_line_search_trials = ls_trial
                    if (.not. ls_accepted) then
                        self%last_line_search_failures = self%last_line_search_failures + 1
                        ! Put the iterate back where the search started.
                        !
                        ! Leaving the last trial in place applies a step that was
                        ! just measured to raise the merit, every iteration, so
                        ! the sequence is not monotone and can and does diverge:
                        ! with the iteration cap lifted to 300 the loop reached
                        ! the divergence guard at 45 iterations rather than
                        ! converging. A globalization that keeps a step it
                        ! rejected is not a globalization.
                        call ls_restore(self, ls_T_saved, ls_P_saved, ls_onset_saved, &
                                        ls_aa_T, ls_aa_P, ls_aa_duT, ls_aa_duP, &
                                        ls_aa_has_prev, ls_aa_gnorm)
                        call self%reflect_variables(step_scale=0.0d0)
                        call self%assemble()
                        call self%apply_bc(prescribed=.false.)
                        if (.not. branch_report_done .and. iter_nl >= 10) then
                            branch_report_done = .true.
                            call report_active_branches(self)
                        end if
                        ! Report the whole backtracking sequence, not just its
                        ! last entry. The two failure mechanisms are only
                        ! distinguishable from the trend in alpha:
                        !   ratio - 1 proportional to alpha  -> the direction is
                        !     genuinely an ascent direction for ||R||, i.e. the
                        !     linearization is wrong;
                        !   ratio - 1 constant in alpha      -> part of the update
                        !     does not scale with alpha, i.e. the search is not
                        !     actually controlling the step length.
                        write (*, '(A,I0,A,ES11.3)') &
                            '   [LS] no descent at iter ', iter_nl, ': ||R_k||=', ls_reference_norm
                        do ls_diag = 1, LINE_SEARCH_MAX_TRIALS
                            if (ls_trial_alpha(ls_diag) < 0.0d0) cycle
                            ! E_T/E_H are the same local_error_block measure the
                            ! [Conserved] line's locT/locH prints (<=1 passes),
                            ! so the two logs are directly comparable.
                            ! ES, not F: a rejected trial can raise the merit by
                            ! many orders of magnitude, and an F field that
                            ! overflows aborts the run on a Fortran output
                            ! conversion error - a diagnostic must never be able
                            ! to kill the solve it is reporting on.
                            write (*, '(A,ES9.2,A,ES12.4,A,ES11.3,A,ES11.3)') &
                                '        alpha=', ls_trial_alpha(ls_diag), &
                                '  merit/merit_k=', ls_trial_ratio(ls_diag), &
                                '  E_T=', ls_trial_E_T(ls_diag), &
                                '  E_H=', ls_trial_E_H(ls_diag)
                        end do
                    end if

                    ! Increment magnitudes. With the tangent and the linear
                    ! solve both verified, the size of du is the remaining
                    ! observable that says whether the step is bounded.
                    if (iter_nl >= 5) then
                        block
                            real(real64), allocatable :: du_report_T(:), du_report_P(:)
                            real(real64) :: max_du_T, max_du_P

                            call self%get_variable_increment(PHYSICS_TYPES%THERMAL, du_report_T)
                            call self%get_variable_increment(PHYSICS_TYPES%HYDRAULIC, du_report_P)
                            max_du_T = 0.0d0
                            max_du_P = 0.0d0
                            if (allocated(du_report_T)) max_du_T = maxval(abs(du_report_T))
                            if (allocated(du_report_P)) max_du_P = maxval(abs(du_report_P))
                            write (*, '(A,I0,A,ES11.3,A,ES11.3)') '   [DU] iter ', iter_nl, &
                                ' max|du_T|=', max_du_T, ' max|du_p|=', max_du_P
                        end block
                    end if

                    if (iter_nl >= FREEZE_REPORT_ITER .and. freeze_report_count < FREEZE_REPORT_LIMIT) then
                        freeze_report_count = freeze_report_count + 1
                        call report_freezing_band(self, iter_nl)
                    end if

                    ! Active-set telemetry.
                    !
                    ! The element tangent (all four blocks, audited against the
                    ! finite-difference tangent) and the linear solve (defect
                    ! below 1e-8) are both exact, so a Newton step that still
                    ! needs 14 to 30 iterations cannot be explained by either.
                    ! What is left is that the residual is only piecewise
                    ! smooth: theta_i switches at T = 0, at the pore-volume
                    ! bound, and at the phase-content clamps. A finite-difference
                    ! probe cannot see those - its perturbation is far too small
                    ! to straddle one - but a full Newton step is not. Counting
                    ! how many nodes change frozen state per iteration separates
                    ! a set that settles (smooth Newton) from one that keeps
                    ! flipping (semismooth chattering).
                    block
                        real(real64), pointer, contiguous :: active_set_temperature(:)
                        integer(int32) :: node, num_flipped

                        nullify (active_set_temperature)
                        call self%temperature%get_current(active_set_temperature)
                        if (associated(active_set_temperature) .and. allocated(frozen_now)) then
                            num_flipped = 0
                            do node = 1, min(size(active_set_temperature), num_phase_nodes)
                                frozen_now(node) = active_set_temperature(node) < 0.0d0
                                if (iter_nl > 1) then
                                    if (frozen_now(node) .neqv. frozen_previous(node)) &
                                        num_flipped = num_flipped + 1
                                end if
                            end do
                            if (iter_nl > 1 .and. num_flipped > 0) then
                                write (*, '(A,I0,A,I0,A,I0)') '   [ACTIVE-SET] iter ', iter_nl, &
                                    ': frozen nodes=', count(frozen_now), ', flipped=', num_flipped
                            end if
                            frozen_previous(:) = frozen_now(:)
                        end if
                        nullify (active_set_temperature)
                    end block

                    ! theta_i is a state function of (T,p) evaluated inside the
                    ! reassembly above, so there is no separate phase criterion.
                    call self%solve_time_step_check_convergence_conserved()
                else
                    ! Update solution with relaxation (Aitken for legacy Picard,
                    ! damped for Newton).
                    call self%reflect_variables()
                end if

                ! Force exit after one iteration when config is NONE (linear solve)
                if (self%control%is_none()) exit nonlinear

            end do nonlinear

            is_step_converged = self%control%is_converged()
            call self%control%get_nonlinear_iter(iter_nl)
            self%last_inner_iterations = iter_nl
            self%last_max_inner_iterations = max(self%last_max_inner_iterations, iter_nl)
            self%last_nonlinear_work = self%last_nonlinear_work + max(1_int32, iter_nl)

            if (.not. is_step_converged) then
                if (linear_failed) then
                    self%last_solve_status = SOLVE_STATUS_LINEAR_FAILURE
                else if (self%control%is_diverged()) then
                    self%last_solve_status = SOLVE_STATUS_NONLINEAR_DIVERGED
                else
                    self%last_solve_status = SOLVE_STATUS_NONLINEAR_LIMIT
                end if
                ! One-shot tangent audit at the first attempt the nonlinear
                ! solve gives up on. That state - not the first line-search
                ! failure, which happens before any ice forms - is the one the
                ! linearization has to explain.
                if (FD_JACOBIAN_AUDIT .and. .not. fd_audit_done) then
                    fd_audit_done = .true.
                    call self%report_fd_jacobian('nonlinear failure')
                end if
                t_res = 0.0d0
                t_inc = 0.0d0
                h_res = 0.0d0
                h_inc = 0.0d0
                if (self%is_active_thermal()) then
                    if (.not. linear_failed) then
                        call self%control%get_current_norm(PHYSICS_TYPES%THERMAL, NONLINEAR_NORM_CRITERIA%RESIDUAL, &
                                                           NORM_TYPES%LINF, t_res)
                        call self%control%get_current_norm(PHYSICS_TYPES%THERMAL, NONLINEAR_NORM_CRITERIA%UPDATE, &
                                                           NORM_TYPES%LINF, t_inc)
                    end if
                end if
                if (self%is_active_hydraulic()) then
                    if (.not. linear_failed) then
                        call self%control%get_current_norm(PHYSICS_TYPES%HYDRAULIC, NONLINEAR_NORM_CRITERIA%RESIDUAL, &
                                                           NORM_TYPES%LINF, h_res)
                        call self%control%get_current_norm(PHYSICS_TYPES%HYDRAULIC, NONLINEAR_NORM_CRITERIA%UPDATE, &
                                                           NORM_TYPES%LINF, h_inc)
                    end if
                end if
                if (linear_failed) then
                    write (*, '(A,I0,A,L1,A)') '   [NONLINEAR] failed: iter=', iter_nl, ', diverged=', &
                        self%control%is_diverged(), ', linear solver failure before nonlinear norm update.'
                else
                    write (*, '(A,I0,A,L1,A,4(ES11.3,1X))') '   [NONLINEAR] failed: iter=', iter_nl, ', diverged=', &
                        self%control%is_diverged(), ', T_res/T_inc/H_res/H_inc=', t_res, t_inc, h_res, h_inc
                end if
            end if

            ! If inner solve failed, skip coupling check
            if (.not. is_step_converged) exit coupling_loop

            ! The local phase return was already included in every inner
            ! Modified Picard iterate. Refresh derived fields once and exit;
            ! no fixed-ice outer solve is needed.
            if (.not. do_phase_outer) then
                call self%update_nodal_phases()
                exit coupling_loop
            end if

            call self%project_nodal_ice(.false., phase_update, phase_increment_max, phase_increment_norm, &
                                        phase_max_node, phase_temperature, phase_pressure, &
                                        phase_current_ice, phase_projected_ice, &
                                        phase_equilibrium_error, phase_increments, phase_active_bounds)

            phase_aa_gamma = 0.0d0
            phase_aa_usable = .false.
            if (allocated(previous_phase_increments)) then
                call compute_aa1_coefficient(phase_increments, previous_phase_increments, &
                                             PHASE_AA_GAMMA_MAX, phase_aa_gamma, phase_aa_usable)
            end if
            if (phase_aa_usable) then
                phase_update = phase_mixing_current * phase_increments - phase_aa_gamma * ( &
                    previous_phase_update + phase_mixing_current * &
                    (phase_increments - previous_phase_increments))
            else
                phase_update = phase_mixing_current * phase_increments
            end if

            ! Bound only the accelerated step size. Do not reject a period-two
            ! residual merely because its current branch is larger or because
            ! AA(1) crosses a component-wise Picard direction: the predicted
            ! residual test in compute_aa1_coefficient is the safeguard.
            phase_step_max = maxval(abs(phase_update))
            phase_step_limit = max(PHASE_STEP_FLOOR, &
                                   PHASE_AA_STEP_GROWTH_MAX * phase_mixing_current * phase_increment_max)
            if (phase_step_max > phase_step_limit) phase_update = phase_update * phase_step_limit / phase_step_max

            ! Once a node is within the accepted local content width of an
            ! active ice bound, its semismooth equation is simply Qi=0 or
            ! Qi=Qi_max. Apply that exact projected-Newton step instead of
            ! geometrically damping it with the global AA coefficient. This
            ! removes artificial tail iterations without relaxing any phase
            ! equilibrium or conservation criterion.
            do node_id = 1, num_phase_nodes
                if (phase_active_bounds(node_id) /= 0 .and. &
                    abs(phase_increments(node_id)) <= PHASE_CONTENT_MAX_TOL) then
                    phase_update(node_id) = phase_increments(node_id)
                end if
            end do
            num_active_nodes = count(phase_active_bounds /= 0)
            phase_step_factor = 0.0d0
            if (abs(phase_increments(phase_max_node)) > tiny(1.0d0)) then
                phase_step_factor = phase_update(phase_max_node) / phase_increments(phase_max_node)
            end if
            ! ES for every unbounded field: a diverging iterate can push T or
            ! the step factor past any fixed F width, and an overflowing F
            ! descriptor aborts the run on an output conversion error.
            write (*, '(A,I0,A,ES10.3,A,ES10.3,A,I0,A,ES11.3,A,ES11.3,' // &
                        'A,ES10.3,A,ES10.3,A,ES10.3,A,ES10.3,A,I0)') &
                '   [PHASE] outer:', coupling_iter, ' max|dQi|:', phase_increment_max, &
                ' rms|dQi|:', phase_increment_norm, ' node:', phase_max_node, &
                ' T:', phase_temperature, ' p:', phase_pressure, &
                ' Qi:', phase_current_ice, ' target:', phase_projected_ice, &
                ' eq[Pa]:', phase_equilibrium_error, ' omega:', phase_step_factor, &
                ' active_nodes:', num_active_nodes

            ! Both forms of the phase condition are required. In the flat part
            ! of a retention curve a small water-content defect can coexist with
            ! a large chemical-potential (Clapeyron pressure) disequilibrium.
            phase_is_converged = phase_equilibrium_error <= PHASE_PRESSURE_TOL .and. &
                                 phase_increment_norm <= PHASE_CONTENT_TOL .and. &
                                 phase_increment_max <= PHASE_CONTENT_MAX_TOL
            phase_merit = max(phase_equilibrium_error / PHASE_PRESSURE_TOL, &
                              phase_increment_norm / PHASE_CONTENT_TOL, &
                              phase_increment_max / PHASE_CONTENT_MAX_TOL)
            self%last_phase_metrics_available = .true.
            self%last_phase_converged = phase_is_converged
            self%last_phase_active_nodes = num_active_nodes
            self%last_phase_increment_max = phase_increment_max
            self%last_phase_increment_norm = phase_increment_norm
            self%last_phase_equilibrium_error = phase_equilibrium_error
            self%last_phase_merit = phase_merit
            if (phase_merit < 0.9d0 * phase_best_merit) then
                phase_best_merit = phase_merit
                phase_stagnation_count = 0
            else
                phase_stagnation_count = phase_stagnation_count + 1
            end if
            if (phase_is_converged .and. is_step_converged .and. &
                (phase_final_correction_applied .or. phase_increment_max <= tiny(1.0d0))) exit coupling_loop
            if (.not. phase_is_converged) phase_final_correction_applied = .false.

            if (coupling_iter == MAX_PHASE_ITER .or. &
                (phase_stagnation_count >= PHASE_STAGNATION_LIMIT .and. phase_best_merit > 1.0d0)) then
                write (*, '(A,ES11.3,A,ES11.3,A,ES11.3)') &
                    '   [PHASE] failed to reach local equilibrium; max|dQi|=', phase_increment_max, &
                    ', rms|dQi|=', phase_increment_norm, &
                    ', max pressure error [Pa]=', phase_equilibrium_error
                call self%control%set_converged(PHYSICS_TYPES%THERMAL, .false.)
                call self%control%set_converged(PHYSICS_TYPES%HYDRAULIC, .false.)
                call self%control%set_diverged(PHYSICS_TYPES%THERMAL, .true.)
                call self%control%set_diverged(PHYSICS_TYPES%HYDRAULIC, .true.)
                is_step_converged = .false.
                self%last_solve_status = SOLVE_STATUS_PHASE_FAILURE
                exit coupling_loop
            end if

            if (allocated(previous_phase_increments)) deallocate (previous_phase_increments)
            allocate (previous_phase_increments, source=phase_increments)
            phase_final_correction_applied = phase_is_converged
            call self%project_nodal_ice(.true., phase_update, phase_increment_max, phase_increment_norm, &
                                        phase_max_node, phase_temperature, phase_pressure, &
                                        phase_current_ice, phase_projected_ice, &
                                        phase_equilibrium_error, phase_increments, phase_active_bounds)
            previous_phase_update = phase_update
            call self%update_nodal_phases()

        end do coupling_loop

        if (is_step_converged) then
            self%last_solve_status = SOLVE_STATUS_CONVERGED
        else if (self%last_solve_status == SOLVE_STATUS_NOT_RUN) then
            if (self%control%is_diverged()) then
                self%last_solve_status = SOLVE_STATUS_NONLINEAR_DIVERGED
            else
                self%last_solve_status = SOLVE_STATUS_NONLINEAR_LIMIT
            end if
        end if

    end subroutine solve_time_step_ftcms

    !> Local nonlinear-error measure (E_T, E_H) of the thermal and hydraulic
    !> blocks, separately, at the current (trial) iterate.
    !>
    !> Delegates to convergence_control:local_error_block, the SAME formula
    !> and SAME cached per-node scales (nodal_volume, dH/dT or d rho_eq/dp,
    !> the primary-variable snapshot, the ATS tolerances) the conserved
    !> acceptance gate's local criterion (locT/locH in the [Conserved] line)
    !> evaluates, so the line search and the gate cannot disagree about what
    !> progress means. They must stay separate rather than combined into one
    !> raw residual norm: the thermal residual is an energy rate [W] and the
    !> hydraulic one a mass rate [kg/s], and on this problem they differ by
    !> about nine orders of magnitude, so a merit built from their raw sum of
    !> squares would be the thermal norm to machine precision and could not
    !> see the hydraulic block at all - local_error_block's per-block K/Pa
    !> scaling is what makes combining them in ls_merit meaningful.
    !>
    !> Falls back to the block residual ratioed against the mean-volume-floor
    !> scale (floor_thermal/floor_hydraulic, from get_residual_floors) when
    !> local_error_block reports its data as unavailable (sentinel < 0) -
    !> only possible during the first iterations of the very first step,
    !> before solve_time_step_check_convergence_conserved has run once.
    subroutine ls_block_norms(self, e_thermal, e_hydraulic, floor_thermal, floor_hydraulic)
        implicit none
        class(type_ftcms), intent(inout) :: self
        real(real64), intent(inout) :: e_thermal
        real(real64), intent(inout) :: e_hydraulic
        !> Mean-volume-floor fallback scale for each block (see get_residual_floors)
        real(real64), intent(in) :: floor_thermal, floor_hydraulic

        real(real64), allocatable :: r_thermal(:), r_hydraulic(:)
        real(real64) :: norm_thermal, norm_hydraulic

        e_thermal = 0.0d0
        e_hydraulic = 0.0d0
        if (self%is_active_thermal()) then
            call self%get_variable_residual(PHYSICS_TYPES%THERMAL, r_thermal)
            if (allocated(r_thermal)) then
                e_thermal = self%control%local_error_block(PHYSICS_TYPES%THERMAL, r_thermal)
                if (e_thermal < 0.0d0) then
                    norm_thermal = sqrt(dot_product(r_thermal, r_thermal))
                    e_thermal = norm_thermal / max(floor_thermal, tiny(1.0d0))
                end if
            end if
        end if
        if (self%is_active_hydraulic()) then
            call self%get_variable_residual(PHYSICS_TYPES%HYDRAULIC, r_hydraulic)
            if (allocated(r_hydraulic)) then
                e_hydraulic = self%control%local_error_block(PHYSICS_TYPES%HYDRAULIC, r_hydraulic)
                if (e_hydraulic < 0.0d0) then
                    norm_hydraulic = sqrt(dot_product(r_hydraulic, r_hydraulic))
                    e_hydraulic = norm_hydraulic / max(floor_hydraulic, tiny(1.0d0))
                end if
            end if
        end if
        if (allocated(r_thermal)) deallocate (r_thermal)
        if (allocated(r_hydraulic)) deallocate (r_hydraulic)
    end subroutine ls_block_norms

    !> Dimensionless line-search merit: L2 combination of the two blocks'
    !> local error measures E_T, E_H (ls_block_norms).
    !>
    !> This is deliberately the same quantity the conserved acceptance gate's
    !> local criterion evaluates per block (locT/locH <= 1), so the search and
    !> the gate cannot disagree about what progress means. With the previous
    !> raw-residual-over-R0 merit the search rejected every step that reduced
    !> a hydraulic residual sitting at resH/0 = 3.12 against a tolerance of
    !> 0.1, because the same step raised the nine-orders-larger thermal block
    !> by 1.6-1.9 percent; it then exhausted its backtracking budget and left
    !> alpha = 7.8e-3, which advances nothing. Measured: the iterate was
    !> bit-for-bit unchanged across successive nonlinear iterations while
    !> resT/0 stayed at 0.1145.
    pure function ls_merit(e_thermal, e_hydraulic) result(merit)
        implicit none
        real(real64), intent(in) :: e_thermal, e_hydraulic
        real(real64) :: merit

        merit = sqrt(e_thermal**2 + e_hydraulic**2)
    end function ls_merit

    !> Save everything a line-search trial mutates. The AA(1) secant pair is
    !> included because prepare_coupled_aa_step commits it unconditionally: a
    !> rejected trial must not leave its pair in the history, or the next
    !> iteration extrapolates from a step that was never taken. The per-node
    !> phase-onset flags are included for the same reason - they are once-per-step
    !> by design, so a rejected trial firing one would silently change the
    !> remaining trials.
    subroutine ls_snapshot(self, T_saved, P_saved, onset_saved, &
                           aa_T, aa_P, aa_duT, aa_duP, aa_has_prev, aa_gnorm)
        implicit none
        class(type_ftcms), intent(inout) :: self
        real(real64), allocatable, intent(inout) :: T_saved(:), P_saved(:)
        logical, allocatable, intent(inout) :: onset_saved(:)
        real(real64), allocatable, intent(inout) :: aa_T(:), aa_P(:), aa_duT(:), aa_duP(:)
        logical, intent(inout) :: aa_has_prev
        real(real64), intent(inout) :: aa_gnorm

        real(real64), pointer, contiguous :: field(:)

        nullify (field)
        if (self%is_active_thermal()) then
            call self%temperature%get_current(field)
            if (associated(field)) then
                if (allocated(T_saved)) deallocate (T_saved)
                allocate (T_saved, source=field)
            end if
            nullify (field)
        end if
        if (self%is_active_hydraulic()) then
            call self%pressure%get_current(field)
            if (associated(field)) then
                if (allocated(P_saved)) deallocate (P_saved)
                allocate (P_saved, source=field)
            end if
            nullify (field)
        end if
        if (allocated(self%phase_onset_reset)) then
            if (allocated(onset_saved)) deallocate (onset_saved)
            allocate (onset_saved, source=self%phase_onset_reset)
        end if
        call copy_alloc(aa_T, self%aa_T_prev)
        call copy_alloc(aa_P, self%aa_P_prev)
        call copy_alloc(aa_duT, self%aa_duT_prev)
        call copy_alloc(aa_duP, self%aa_duP_prev)
        aa_has_prev = self%aa_has_prev
        aa_gnorm = self%aa_gnorm_prev
    end subroutine ls_snapshot

    !> Restore the state saved by ls_snapshot so the next trial starts from x_k.
    subroutine ls_restore(self, T_saved, P_saved, onset_saved, &
                          aa_T, aa_P, aa_duT, aa_duP, aa_has_prev, aa_gnorm)
        implicit none
        class(type_ftcms), intent(inout) :: self
        real(real64), allocatable, intent(in) :: T_saved(:), P_saved(:)
        logical, allocatable, intent(in) :: onset_saved(:)
        real(real64), allocatable, intent(in) :: aa_T(:), aa_P(:), aa_duT(:), aa_duP(:)
        logical, intent(in) :: aa_has_prev
        real(real64), intent(in) :: aa_gnorm

        real(real64), pointer, contiguous :: field(:)

        nullify (field)
        if (allocated(T_saved) .and. self%is_active_thermal()) then
            call self%temperature%get_current(field)
            if (associated(field)) then
                if (size(field) == size(T_saved)) field(:) = T_saved(:)
            end if
            nullify (field)
        end if
        if (allocated(P_saved) .and. self%is_active_hydraulic()) then
            call self%pressure%get_current(field)
            if (associated(field)) then
                if (size(field) == size(P_saved)) field(:) = P_saved(:)
            end if
            nullify (field)
        end if
        if (allocated(onset_saved) .and. allocated(self%phase_onset_reset)) then
            if (size(self%phase_onset_reset) == size(onset_saved)) &
                self%phase_onset_reset(:) = onset_saved(:)
        end if
        call copy_alloc(self%aa_T_prev, aa_T)
        call copy_alloc(self%aa_P_prev, aa_P)
        call copy_alloc(self%aa_duT_prev, aa_duT)
        call copy_alloc(self%aa_duP_prev, aa_duP)
        self%aa_has_prev = aa_has_prev
        self%aa_gnorm_prev = aa_gnorm
    end subroutine ls_restore

    !> One-shot scan of the block residuals along the Newton direction.
    !>
    !> Eight backtracking points on a combined norm are not enough to say what
    !> shape the merit has. This walks alpha over four decades and reports each
    !> block separately, together with the node each block's maximum sits on and
    !> that node's thermodynamic state, so a smooth-but-ascending direction, a
    !> kink, and a flat plateau can be told apart.
    subroutine scan_residual_along_direction(self, iter_nl, ref_T, ref_H)
        implicit none
        class(type_ftcms), intent(inout) :: self
        integer(int32), intent(in) :: iter_nl
        real(real64), intent(in) :: ref_T, ref_H

        integer(int32), parameter :: NUM_SCAN = 25
        real(real64), allocatable :: T_saved(:), P_saved(:)
        logical, allocatable :: onset_saved(:)
        real(real64), allocatable :: aa_T(:), aa_P(:), aa_duT(:), aa_duP(:)
        logical :: aa_has_prev
        real(real64) :: aa_gnorm
        real(real64), allocatable :: r_thermal(:), r_hydraulic(:), du_T(:), du_P(:)
        real(real64), pointer, contiguous :: field(:)
        real(real64) :: alpha, norm_T, norm_H, linf_T, linf_H
        integer(int32) :: k, node_T, node_H

        call ls_snapshot(self, T_saved, P_saved, onset_saved, &
                         aa_T, aa_P, aa_duT, aa_duP, aa_has_prev, aa_gnorm)

        call self%get_variable_increment(PHYSICS_TYPES%THERMAL, du_T)
        call self%get_variable_increment(PHYSICS_TYPES%HYDRAULIC, du_P)

        write (*, '(A,I0)') '   [SCAN] residual along the Newton direction at iter ', iter_nl
        write (*, '(A,ES13.5,A,ES13.5)') '   [SCAN] step-initial norms: ||R_T^0||=', ref_T, &
            ' ||R_H^0||=', ref_H
        if (allocated(du_T) .and. allocated(du_P)) then
            write (*, '(A,ES11.3,A,ES11.3)') '   [SCAN] max|du_T|=', maxval(abs(du_T)), &
                ' max|du_p|=', maxval(abs(du_P))
        end if
        write (*, '(A)') '   [SCAN]     alpha      ||R_T||/0      ||R_H||/0     maxR_T@node     maxR_H@node'

        do k = 1, NUM_SCAN
            alpha = 10.0d0**(-4.0d0 * real(k - 1, real64) / real(NUM_SCAN - 1, real64))
            call ls_restore(self, T_saved, P_saved, onset_saved, &
                            aa_T, aa_P, aa_duT, aa_duP, aa_has_prev, aa_gnorm)
            call self%reflect_variables(step_scale=alpha)
            call self%assemble()
            call self%apply_bc(prescribed=.false.)

            call self%get_variable_residual(PHYSICS_TYPES%THERMAL, r_thermal)
            call self%get_variable_residual(PHYSICS_TYPES%HYDRAULIC, r_hydraulic)
            norm_T = 0.0d0; norm_H = 0.0d0
            linf_T = 0.0d0; linf_H = 0.0d0
            node_T = 0; node_H = 0
            if (allocated(r_thermal)) then
                norm_T = sqrt(dot_product(r_thermal, r_thermal))
                node_T = maxloc(abs(r_thermal), dim=1)
                linf_T = abs(r_thermal(node_T))
            end if
            if (allocated(r_hydraulic)) then
                norm_H = sqrt(dot_product(r_hydraulic, r_hydraulic))
                node_H = maxloc(abs(r_hydraulic), dim=1)
                linf_H = abs(r_hydraulic(node_H))
            end if
            write (*, '(A,ES10.3,2(2X,ES13.5),2(2X,ES10.3,A,I0))') '   [SCAN] ', alpha, &
                norm_T / max(ref_T, tiny(1.0d0)), norm_H / max(ref_H, tiny(1.0d0)), &
                linf_T, '@', node_T, linf_H, '@', node_H
        end do

        ! Report the state of the nodes that dominate each block at the base
        ! iterate, which is what the direction has to fix.
        call ls_restore(self, T_saved, P_saved, onset_saved, &
                        aa_T, aa_P, aa_duT, aa_duP, aa_has_prev, aa_gnorm)
        call self%reflect_variables(step_scale=0.0d0)
        call self%assemble()
        call self%apply_bc(prescribed=.false.)
        call self%get_variable_residual(PHYSICS_TYPES%THERMAL, r_thermal)
        call self%get_variable_residual(PHYSICS_TYPES%HYDRAULIC, r_hydraulic)
        if (allocated(r_thermal)) node_T = maxloc(abs(r_thermal), dim=1)
        if (allocated(r_hydraulic)) node_H = maxloc(abs(r_hydraulic), dim=1)
        call report_node(self, node_T, 'max R_T', du_T, du_P)
        call report_node(self, node_H, 'max R_H', du_T, du_P)

        call ls_restore(self, T_saved, P_saved, onset_saved, &
                        aa_T, aa_P, aa_duT, aa_duP, aa_has_prev, aa_gnorm)
        call self%reflect_variables(step_scale=0.0d0)
        call self%assemble()
        call self%apply_bc(prescribed=.false.)
    end subroutine scan_residual_along_direction

    !> Verify the analytic constitutive tangent against the state function it
    !> claims to differentiate, at one node, by central differences.
    !>
    !> A large apparent heat capacity is not by itself a fault: with the
    !> Clapeyron slope and a steep retention curve, dH/dT of order 1e8 is
    !> reachable. What matters is whether that tangent is the derivative of the
    !> H the residual actually evaluates. If it is, a correct Jacobian carrying
    !> it will SHRINK the temperature update, not enlarge it - so a single
    !> iteration moving the liquid content by a third of the pore volume points
    !> at a mismatch between the assembled tangent and the state update, not at
    !> the stiffness itself.
    !>
    !> The quotients must approach the analytic value as epsilon falls and then
    !> stop improving when round-off takes over. A ratio that stays away from
    !> one at every epsilon is a wrong derivative; one that drifts with epsilon
    !> without settling is a non-differentiable state function.
    subroutine verify_local_tangent(self, node_id)
        implicit none
        class(type_ftcms), intent(inout) :: self
        integer(int32), intent(in) :: node_id

        integer(int32), parameter :: NUM_EPSILON = 4
        integer(int32), parameter :: NUM_ALPHA = 6
        real(real64), parameter :: ALPHA_SCAN(NUM_ALPHA) = &
            [1.0d0, 1.0d-1, 1.0d-2, 1.0d-3, 1.0d-4, 1.0d-5]
        real(real64), parameter :: EPSILON_TEMPERATURE(NUM_EPSILON) = &
            [1.0d-6, 1.0d-5, 1.0d-4, 1.0d-3]
        type(type_state) :: state
        real(real64) :: temperature, pressure
        real(real64) :: enthalpy_plus, enthalpy_minus, liquid_plus, liquid_minus
        real(real64) :: ice_plus, ice_minus
        real(real64) :: analytic_dH_dT, analytic_dQw_dT, analytic_dQi_dT
        real(real64) :: fd_dH_dT, fd_dQw_dT, fd_dQi_dT
        real(real64) :: analytic_dH_dP, analytic_dQw_dP, step_T, step_P
        real(real64) :: enthalpy_base, liquid_base, predicted_H, predicted_Qw
        real(real64), allocatable :: increment_T(:), increment_P(:)
        integer(int32) :: k, row_start, repr_elem, material_id

        row_start = self%node_material_table%ptr(node_id)
        if (row_start >= self%node_material_table%ptr(node_id + 1)) return
        repr_elem = self%node_material_table%repr_element(row_start)
        call self%domain%get_material_id(repr_elem, material_id)

        call self%set_state(node_id, repr_elem, state, calc_physics=.true., include_fluxes=.false.)
        call state%temperature%get(temperature)
        call state%pressure%get(pressure)
        call state%dQw_dT%get(analytic_dQw_dT)
        call state%dQw_dP%get(analytic_dQw_dP)
        call state%dQi_dT%get(analytic_dQi_dT)
        analytic_dH_dT = 0.0d0
        call self%thermal%compute_mass_term(material_id, state, analytic_dH_dT)

        write (*, '(A,I0,A,ES12.4,A,ES12.4,A,ES12.4)') &
            '   [TANGENT] node ', node_id, ' analytic dH/dT=', analytic_dH_dT, &
            ' dQw/dT=', analytic_dQw_dT, ' dQi/dT=', analytic_dQi_dT
        write (*, '(A)') '   [TANGENT]      eps_T        dH/dT_fd     ratio      dQw/dT_fd    ratio' // &
            '      dQi/dT_fd    ratio'

        ! Linear prediction against the actual change along the Newton
        ! direction. The tangent may be exact and still be the wrong operator
        ! for the step the solver takes; this is the test that separates the
        ! two. r -> 1 as alpha falls means the linearization is valid and the
        ! step is simply too long; r staying away from 1 means the tangent does
        ! not describe the update.
        call self%get_variable_increment(PHYSICS_TYPES%THERMAL, increment_T)
        call self%get_variable_increment(PHYSICS_TYPES%HYDRAULIC, increment_P)
        step_T = 0.0d0
        step_P = 0.0d0
        if (allocated(increment_T)) then
            if (node_id <= size(increment_T)) step_T = increment_T(node_id)
        end if
        if (allocated(increment_P)) then
            if (node_id <= size(increment_P)) step_P = increment_P(node_id)
        end if
        analytic_dH_dP = 0.0d0
        call self%thermal%compute_coupling_mass_term(material_id, state, analytic_dH_dP)
        call self%thermal%calc_enthalpy_density(material_id, state, enthalpy_base)
        call state%water_content%get(liquid_base)

        write (*, '(A,ES11.3,A,ES11.3,A,ES12.4)') '   [TANGENT] step du_T=', step_T, &
            ' du_p=', step_P, ' analytic dH/dp=', analytic_dH_dP
        write (*, '(A)') '   [TANGENT]      alpha        dH_lin       dH_act      r_H' // &
            '        dQw_lin      dQw_act     r_Qw'
        do k = 1, NUM_ALPHA
            call evaluate_step(ALPHA_SCAN(k), enthalpy_plus, liquid_plus, ice_plus)
            predicted_H = (analytic_dH_dT * step_T + analytic_dH_dP * step_P) * ALPHA_SCAN(k)
            predicted_Qw = (analytic_dQw_dT * step_T + analytic_dQw_dP * step_P) * ALPHA_SCAN(k)
            ! ES, not F: safe_ratio spans many decades at a rejected iterate
            ! and an overflowing F field aborts the run from inside a print.
            write (*, '(A,ES11.3,2(2X,ES12.4),1X,ES11.3,2(2X,ES12.4),1X,ES11.3)') '   [TANGENT] ', &
                ALPHA_SCAN(k), predicted_H, enthalpy_plus - enthalpy_base, &
                safe_ratio(enthalpy_plus - enthalpy_base, predicted_H), &
                predicted_Qw, liquid_plus - liquid_base, &
                safe_ratio(liquid_plus - liquid_base, predicted_Qw)
        end do

        do k = 1, NUM_EPSILON
            call evaluate_at(temperature + EPSILON_TEMPERATURE(k), enthalpy_plus, liquid_plus, ice_plus)
            call evaluate_at(temperature - EPSILON_TEMPERATURE(k), enthalpy_minus, liquid_minus, ice_minus)
            fd_dH_dT = (enthalpy_plus - enthalpy_minus) / (2.0d0 * EPSILON_TEMPERATURE(k))
            fd_dQw_dT = (liquid_plus - liquid_minus) / (2.0d0 * EPSILON_TEMPERATURE(k))
            fd_dQi_dT = (ice_plus - ice_minus) / (2.0d0 * EPSILON_TEMPERATURE(k))
            write (*, '(A,ES11.3,3(2X,ES12.4,1X,ES11.3))') '   [TANGENT] ', EPSILON_TEMPERATURE(k), &
                fd_dH_dT, safe_ratio(fd_dH_dT, analytic_dH_dT), &
                fd_dQw_dT, safe_ratio(fd_dQw_dT, analytic_dQw_dT), &
                fd_dQi_dT, safe_ratio(fd_dQi_dT, analytic_dQi_dT)
        end do

    contains

        subroutine evaluate_step(alpha, enthalpy, liquid, ice)
            implicit none
            real(real64), intent(in) :: alpha
            real(real64), intent(inout) :: enthalpy, liquid, ice

            type(type_state) :: trial_state

            call trial_state%copy(state)
            call trial_state%temperature%set(temperature + alpha * step_T)
            call trial_state%pressure%set(pressure + alpha * step_P)
            call self%thermal%update_water_phases(material_id, trial_state)
            call self%thermal%calc_enthalpy_density(material_id, trial_state, enthalpy)
            call trial_state%water_content%get(liquid)
            call trial_state%ice_content%get(ice)
        end subroutine evaluate_step

        subroutine evaluate_at(trial_temperature, enthalpy, liquid, ice)
            implicit none
            real(real64), intent(in) :: trial_temperature
            real(real64), intent(inout) :: enthalpy, liquid, ice

            type(type_state) :: trial_state

            call trial_state%copy(state)
            call trial_state%temperature%set(trial_temperature)
            call self%thermal%update_water_phases(material_id, trial_state)
            call self%thermal%calc_enthalpy_density(material_id, trial_state, enthalpy)
            call trial_state%water_content%get(liquid)
            call trial_state%ice_content%get(ice)
        end subroutine evaluate_at

        pure function safe_ratio(numerator, denominator) result(ratio)
            implicit none
            real(real64), intent(in) :: numerator, denominator
            real(real64) :: ratio

            ratio = 0.0d0
            if (abs(denominator) > tiny(1.0d0)) ratio = numerator / denominator
        end function safe_ratio
    end subroutine verify_local_tangent

    !> State and constitutive derivatives across the freezing band.
    !>
    !> Reports, for an iteration that is not converging: how many nodes sit
    !> where the matric and freezing suctions cross (s_m = s_f is where the
    !> effective suction switches branch), the largest phase-content movement
    !> between iterations, and at the node that moves most the suctions and the
    !> three derivatives the linearization depends on. A non-smooth local update
    !> shows up as a large phase movement concentrated on the band with
    !> derivatives that jump between iterations.
    subroutine report_freezing_band(self, iter_nl)
        implicit none
        class(type_ftcms), intent(inout) :: self
        integer(int32), intent(in) :: iter_nl

        real(real64), parameter :: CLAPEYRON_SLOPE = rho_std * Lf0 / (Tf0 + TtoK)
        real(real64), parameter :: BAND_WIDTH_PA = 1.0d4
        type(type_state) :: state
        real(real64), pointer, contiguous :: liquid(:), ice(:)
        real(real64) :: temperature, pressure, suction_matric, suction_freezing, suction_effective
        real(real64) :: change_liquid, change_ice, max_change_liquid, max_change_ice
        real(real64) :: dliquid_dP, dice_dP, dliquid_dT, dice_dT, heat_capacity
        integer(int32) :: node_id, num_nodes, band_count, worst_node
        integer(int32) :: row_start, repr_elem, material_id

        nullify (liquid, ice)
        call self%domain%get_num_nodes(num_nodes)
        call self%Qw%get_current(liquid)
        call self%Qi%get_current(ice)
        if (.not. (associated(liquid) .and. associated(ice))) return

        if (.not. allocated(freeze_prev_liquid)) allocate (freeze_prev_liquid(num_nodes), source=0.0d0)
        if (.not. allocated(freeze_prev_ice)) allocate (freeze_prev_ice(num_nodes), source=0.0d0)

        band_count = 0
        max_change_liquid = 0.0d0
        max_change_ice = 0.0d0
        worst_node = 0
        do node_id = 1, min(num_nodes, size(liquid))
            call self%temperature%get_current(node_id, temperature)
            call self%pressure%get_current(node_id, pressure)
            suction_matric = -pressure
            suction_freezing = 0.0d0
            if (temperature < Tf0) suction_freezing = CLAPEYRON_SLOPE * (Tf0 - temperature)
            if (abs(suction_matric - suction_freezing) < BAND_WIDTH_PA) band_count = band_count + 1

            change_liquid = abs(liquid(node_id) - freeze_prev_liquid(node_id))
            change_ice = abs(ice(node_id) - freeze_prev_ice(node_id))
            max_change_liquid = max(max_change_liquid, change_liquid)
            if (change_ice > max_change_ice) then
                max_change_ice = change_ice
                worst_node = node_id
            end if
        end do

        write (*, '(A,I0,A,I0,A,ES11.3,A,ES11.3)') '   [FREEZE] iter ', iter_nl, &
            '  band|s_m-s_f|<1e4 Pa: ', band_count, '  max|dQw|=', max_change_liquid, &
            '  max|dQi|=', max_change_ice

        if (worst_node >= 1) then
            row_start = self%node_material_table%ptr(worst_node)
            if (row_start < self%node_material_table%ptr(worst_node + 1)) then
                repr_elem = self%node_material_table%repr_element(row_start)
                call self%domain%get_material_id(repr_elem, material_id)
                call self%set_state(worst_node, repr_elem, state, calc_physics=.true., include_fluxes=.false.)
                call self%temperature%get_current(worst_node, temperature)
                call self%pressure%get_current(worst_node, pressure)
                suction_matric = -pressure
                suction_freezing = 0.0d0
                if (temperature < Tf0) suction_freezing = CLAPEYRON_SLOPE * (Tf0 - temperature)
                suction_effective = 0.0d0
                call state%effective_suction%get(suction_effective)
                call state%dQw_dP%get(dliquid_dP)
                call state%dQi_dP%get(dice_dP)
                call state%dQw_dT%get(dliquid_dT)
                call state%dQi_dT%get(dice_dT)
                heat_capacity = 0.0d0
                call self%thermal%compute_mass_term(material_id, state, heat_capacity)
                write (*, '(A,I0,A,ES11.3,A,ES11.3,A,ES11.3,A,ES11.3,A,ES11.3)') &
                    '   [FREEZE]   worst dQi node=', worst_node, ' T=', temperature, &
                    ' s_m=', suction_matric, ' s_f=', suction_freezing, ' s_eff=', suction_effective, &
                    ' p=', pressure
                write (*, '(A,ES12.4,A,ES12.4,A,ES12.4,A,ES12.4,A,ES12.4)') &
                    '   [FREEZE]   dQw/dp=', dliquid_dP, ' dQi/dp=', dice_dP, &
                    ' dQw/dT=', dliquid_dT, ' dQi/dT=', dice_dT, ' dH/dT=', heat_capacity
                if (freeze_report_count <= 2) call verify_local_tangent(self, worst_node)
            end if
        end if

        do node_id = 1, min(num_nodes, size(liquid))
            freeze_prev_liquid(node_id) = liquid(node_id)
            freeze_prev_ice(node_id) = ice(node_id)
        end do
        nullify (liquid, ice)
    end subroutine report_freezing_band

    !> Report which constitutive branch the dominant residual nodes sit on.
    !>
    !> The element tangent matches a finite difference and the linear solve is
    !> exact, yet no step length reduces the merit. For an exact two-sided
    !> Jacobian that cannot happen, so the residual must only be piecewise
    !> differentiable there and the iterate must be sitting on a kink. The
    !> constitutive branches that can produce one are the pore-volume bound on
    !> the total water, the ice-content clamps at 0 and at the porosity, the
    !> saturation of the impedance ratio, and the phase-volume closure. All of
    !> them are decidable from the published nodal phase fields.
    subroutine report_active_branches(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        integer(int32), parameter :: NUM_REPORTED = 5
        real(real64), allocatable :: r_thermal(:), r_hydraulic(:)
        real(real64), pointer, contiguous :: field(:)
        real(real64) :: temperature, pressure, ice, water, porosity
        real(real64) :: theta_total, volume_bound, ratio_denominator, impedance_ratio
        real(real64) :: density_ratio
        integer(int32) :: k, node_id
        integer(int32) :: ranked(NUM_REPORTED)

        density_ratio = 917.0d0 / 1000.0d0
        call self%get_variable_residual(PHYSICS_TYPES%THERMAL, r_thermal)
        call self%get_variable_residual(PHYSICS_TYPES%HYDRAULIC, r_hydraulic)

        write (*, '(A)') '   [BRANCH] nodes carrying the largest residuals'
        write (*, '(A)') '   [BRANCH]  block  node        T          p        Qw       Qi      phi' // &
            '   theta_tot     bound    margin   Q_imp'

        call rank_nodes(r_thermal, ranked)
        do k = 1, NUM_REPORTED
            node_id = ranked(k)
            if (node_id < 1) cycle
            call emit('R_T', node_id)
        end do
        call rank_nodes(r_hydraulic, ranked)
        do k = 1, NUM_REPORTED
            node_id = ranked(k)
            if (node_id < 1) cycle
            call emit('R_H', node_id)
        end do

        if (allocated(r_thermal)) deallocate (r_thermal)
        if (allocated(r_hydraulic)) deallocate (r_hydraulic)

    contains

        subroutine rank_nodes(residual, ranking)
            implicit none
            real(real64), allocatable, intent(in) :: residual(:)
            integer(int32), intent(inout) :: ranking(:)

            real(real64), allocatable :: work(:)
            integer(int32) :: j

            ranking(:) = 0
            if (.not. allocated(residual)) return
            allocate (work, source=abs(residual))
            do j = 1, size(ranking)
                if (size(work) == 0) exit
                ranking(j) = maxloc(work, dim=1)
                work(ranking(j)) = -1.0d0
            end do
        end subroutine rank_nodes

        subroutine emit(label, node)
            implicit none
            character(len=*), intent(in) :: label
            integer(int32), intent(in) :: node

            call self%temperature%get_current(node, temperature)
            call self%pressure%get_current(node, pressure)
            call self%Qw%get_current(node, water)
            call self%Qi%get_current(node, ice)
            call self%porosity%get_current(node, porosity)

            theta_total = water + density_ratio * ice
            volume_bound = porosity * density_ratio + water * (1.0d0 - density_ratio)
            ratio_denominator = water + ice
            impedance_ratio = 0.0d0
            if (ratio_denominator > tiny(1.0d0)) impedance_ratio = ice / ratio_denominator

            write (*, '(A,A,2X,I6,F10.4,ES12.3,5F10.5,F9.4)') '   [BRANCH]  ', label, node, &
                temperature, pressure, water, ice, porosity, theta_total, volume_bound, &
                theta_total - volume_bound, impedance_ratio
        end subroutine emit
    end subroutine report_active_branches

    !> Print the primary and phase state of one node.
    subroutine report_node(self, node_id, label, du_T, du_P)
        implicit none
        class(type_ftcms), intent(inout) :: self
        integer(int32), intent(in) :: node_id
        character(len=*), intent(in) :: label
        real(real64), allocatable, intent(in) :: du_T(:), du_P(:)

        real(real64) :: temperature, pressure, ice, water, porosity
        real(real64) :: step_T, step_P

        if (node_id < 1) return
        call self%temperature%get_current(node_id, temperature)
        call self%pressure%get_current(node_id, pressure)
        call self%Qi%get_current(node_id, ice)
        call self%Qw%get_current(node_id, water)
        call self%porosity%get_current(node_id, porosity)
        step_T = 0.0d0
        step_P = 0.0d0
        if (allocated(du_T)) then
            if (node_id <= size(du_T)) step_T = du_T(node_id)
        end if
        if (allocated(du_P)) then
            if (node_id <= size(du_P)) step_P = du_P(node_id)
        end if
        write (*, '(A,A,A,I0,A,F10.5,A,ES12.4,A,F8.5,A,F8.5,A,F8.5,A,ES11.3,A,ES11.3)') &
            '   [SCAN] ', label, ' node=', node_id, ' T=', temperature, ' p=', pressure, &
            ' Qi=', ice, ' Qw=', water, ' phi=', porosity, ' du_T=', step_T, ' du_p=', step_P
    end subroutine report_node

    !> Copy an allocatable array, propagating the unallocated state.
    subroutine copy_alloc(dst, src)
        implicit none
        real(real64), allocatable, intent(inout) :: dst(:)
        real(real64), allocatable, intent(in) :: src(:)

        if (allocated(dst)) deallocate (dst)
        if (allocated(src)) allocate (dst, source=src)
    end subroutine copy_alloc


    module subroutine solve_time_step_staggered_ftcms(self, is_step_converged)
        implicit none
        class(type_ftcms), intent(inout) :: self
        logical, intent(inout) :: is_step_converged

        logical :: prescribe_bc
        integer(int32) :: iter_nl, coupling_iter, num_nodes, bdf_order
        integer(int32), parameter :: MAX_COUPLING_ITER = 10
        integer(int32), parameter :: MAX_PHASE_NL_ITER = 100
        real(real64), parameter :: COUPLING_TOL = 1.0d-3
        real(real64), parameter :: THERMAL_INCREMENT_GUARD = 1.0d6
        real(real64), parameter :: HYDRAULIC_INCREMENT_GUARD = 1.0d8
        real(real64) :: t_res, t_inc, h_res, h_inc
        real(real64) :: coupling_change_T, coupling_change_P, T_scale, P_scale
        real(real64) :: mean_pressure
        real(real64) :: phase_inc_max
        real(real64), allocatable :: T_old(:), P_old(:)
        real(real64), allocatable :: phase_increment(:)
        real(real64), allocatable :: Qw_save(:), dW_check(:)
        real(real64), allocatable :: hyd_residual_local(:)
        real(real64), pointer, contiguous :: T_cur(:) => null()
        real(real64), pointer, contiguous :: P_cur(:) => null()
        real(real64), pointer, contiguous :: Qw_cur(:) => null()
        logical :: linear_failed
        logical :: excessive_update
        character(len=16) :: phase_label

        is_step_converged = .false.

        call self%domain%get_num_nodes(num_nodes)
        allocate (T_old(num_nodes), P_old(num_nodes))

        call self%solve_time_step_initial_setup()
        self%last_phase_iterations = 1
        self%last_inner_iterations = 0
        self%last_max_inner_iterations = 0
        self%last_nonlinear_work = 0
        self%last_solve_status = SOLVE_STATUS_NOT_RUN
        self%last_phase_metrics_available = .false.
        self%last_phase_converged = .false.
        self%last_phase_active_nodes = -1
        self%last_phase_increment_max = -1.0d0
        self%last_phase_increment_norm = -1.0d0
        self%last_phase_equilibrium_error = -1.0d0
        self%last_phase_merit = -1.0d0

        coupling_loop: do coupling_iter = 1, MAX_COUPLING_ITER
            self%last_phase_iterations = coupling_iter

            if (coupling_iter > 1) then
                call self%temperature%get_current(T_cur)
                call self%pressure%get_current(P_cur)
                if (associated(T_cur)) T_old(:) = T_cur(:)
                if (associated(P_cur)) P_old(:) = P_cur(:)
                nullify (T_cur)
                nullify (P_cur)

                call self%control%reset_iteration()
                call self%control%set_nonlinear_solver(NONLINEAR_SOLVER%PICARD)
                call self%control%increment_total()
                call self%control%reset_acceleration()
            end if

            ! =============================================================
            ! Phase 1: Hydraulic nonlinear loop (T frozen)
            ! =============================================================
            phase_label = '[HYD_NL]'
            linear_failed = .false.

            if (self%is_active_hydraulic()) then
                hydraulic_nl: do while (self%control%should_continue())
                    call self%solve_time_step_setup(prescribe_bc)
                    if (prescribe_bc) then
                        call self%prescribe_dirichlet()
                        call self%calc_gradient_temperature()
                        call self%calc_gradient_pressure()
                    end if
                    call self%assemble()
                    call self%apply_bc(prescribed=.false.)
                    call self%freeze_physics_dofs(PHYSICS_TYPES%THERMAL)
                    call self%control%get_nonlinear_iter(iter_nl)
                    self%current_physics_id = PHYSICS_TYPES%HYDRAULIC%ID
                    call self%solve()
                    call self%zero_frozen_increment(PHYSICS_TYPES%THERMAL)

                    if (.not. self%solver%is_success()) then
                        linear_failed = .true.
                        self%last_solve_status = SOLVE_STATUS_LINEAR_FAILURE
                        call self%control%set_converged(PHYSICS_TYPES%HYDRAULIC, .false.)
                        call self%control%set_diverged(PHYSICS_TYPES%HYDRAULIC, .true.)
                        exit hydraulic_nl
                    end if

                    excessive_update = .false.
                    phase_inc_max = 0.0d0
                    if (allocated(phase_increment)) deallocate (phase_increment)
                    call self%get_variable_increment(PHYSICS_TYPES%HYDRAULIC, phase_increment)
                    if (allocated(phase_increment)) then
                        if (size(phase_increment) > 0) then
                            phase_inc_max = maxval(abs(phase_increment))
                            excessive_update = phase_inc_max > HYDRAULIC_INCREMENT_GUARD
                        end if
                        deallocate (phase_increment)
                    end if
                    if (excessive_update) then
                        write (*, '(A,ES13.5,A,ES13.5,A)') '   [HYD_NL] excessive hydraulic increment detected (> ', &
                            HYDRAULIC_INCREMENT_GUARD, ', max=', phase_inc_max, '). Continue with damped update.'
                    end if

                    ! Save Qw before update to compute dW for convergence
                    call self%Qw%get_current(Qw_cur)
                    if (.not. allocated(Qw_save)) allocate (Qw_save(num_nodes))
                    if (associated(Qw_cur)) then
                        Qw_save(:) = Qw_cur(:)
                    else
                        Qw_save(:) = 0.0d0
                    end if
                    nullify (Qw_cur)

                    ! Apply update (update_nodal_phases recomputes Qw inside)
                    call self%reflect_variables()

                    ! Compute dW = Qw_new - Qw_old
                    call self%Qw%get_current(Qw_cur)
                    if (.not. allocated(dW_check)) allocate (dW_check(num_nodes))
                    if (associated(Qw_cur)) then
                        dW_check(:) = Qw_cur(:) - Qw_save(:)
                    else
                        dW_check(:) = 0.0d0
                    end if
                    nullify (Qw_cur)

                    ! Convergence check: residual + dW (water-content-based update norm)
                    call self%get_variable_residual(PHYSICS_TYPES%HYDRAULIC, hyd_residual_local)
                    if (.not. allocated(hyd_residual_local) .or. size(hyd_residual_local) == 0) then
                        call self%control%set_diverged(PHYSICS_TYPES%HYDRAULIC, .true.)
                    else if (has_nan(hyd_residual_local) .or. has_nan(dW_check)) then
                        call self%control%set_diverged(PHYSICS_TYPES%HYDRAULIC, .true.)
                    else
                        call self%control%check_convergence(PHYSICS_TYPES%HYDRAULIC, hyd_residual_local, dW_check)
                    end if

                    call self%control%get_nonlinear_iter(iter_nl)
                    if ((.not. self%control%is_converged()) .and. iter_nl >= MAX_PHASE_NL_ITER) then
                        self%last_solve_status = SOLVE_STATUS_NONLINEAR_LIMIT
                        write (*, '(A,I0,A)') '   [HYD_NL] reached nonlinear iteration cap (', MAX_PHASE_NL_ITER, &
                            '). Triggering timestep retry.'
                        call self%control%set_converged(PHYSICS_TYPES%HYDRAULIC, .false.)
                        call self%control%set_diverged(PHYSICS_TYPES%HYDRAULIC, .true.)
                        exit hydraulic_nl
                    end if

                    if (self%control%is_none()) exit hydraulic_nl
                end do hydraulic_nl

                call self%control%get_nonlinear_iter(iter_nl)
                self%last_inner_iterations = iter_nl
                self%last_max_inner_iterations = max(self%last_max_inner_iterations, iter_nl)
                self%last_nonlinear_work = self%last_nonlinear_work + max(1_int32, iter_nl)

                if (.not. self%control%is_converged()) then
                    call self%control%get_nonlinear_iter(iter_nl)
                    h_res = 0.0d0
                    h_inc = 0.0d0
                    if (.not. linear_failed) then
                        call self%control%get_current_norm(PHYSICS_TYPES%HYDRAULIC, &
                                                           NONLINEAR_NORM_CRITERIA%RESIDUAL, NORM_TYPES%LINF, h_res)
                        call self%control%get_current_norm(PHYSICS_TYPES%HYDRAULIC, &
                                                           NONLINEAR_NORM_CRITERIA%UPDATE, NORM_TYPES%LINF, h_inc)
                    end if
                    if (linear_failed) then
                        write (*, '(A,A,A,I0,A,L1,A)') '   ', phase_label, &
                            ' failed: iter=', iter_nl, ', diverged=', self%control%is_diverged(), &
                            ', linear solver failure.'
                    else
                        write (*, '(A,A,A,I0,A,L1,A,2(ES11.3,1X))') '   ', phase_label, &
                            ' failed: iter=', iter_nl, ', diverged=', self%control%is_diverged(), &
                            ', H_res/H_inc=', h_res, h_inc
                    end if
                    if (self%last_solve_status == SOLVE_STATUS_NOT_RUN) then
                        if (self%control%is_diverged()) then
                            self%last_solve_status = SOLVE_STATUS_NONLINEAR_DIVERGED
                        else
                            self%last_solve_status = SOLVE_STATUS_NONLINEAR_LIMIT
                        end if
                    end if
                    exit coupling_loop
                end if
            else
                call self%control%set_converged(PHYSICS_TYPES%HYDRAULIC, .true.)
                call self%control%set_diverged(PHYSICS_TYPES%HYDRAULIC, .false.)
            end if

            ! =============================================================
            ! Phase 2: Thermal nonlinear loop (P frozen)
            ! =============================================================
            phase_label = '[THM_NL]'

            call self%control%reset_iteration()
            call self%control%set_nonlinear_solver(NONLINEAR_SOLVER%PICARD)
            call self%control%increment_total()
            call self%control%reset_acceleration()

            linear_failed = .false.

            if (self%is_active_thermal()) then
                thermal_nl: do while (self%control%should_continue())
                    call self%solve_time_step_setup(prescribe_bc)
                    if (prescribe_bc) then
                        call self%prescribe_dirichlet()
                        call self%calc_gradient_temperature()
                        call self%calc_gradient_pressure()
                    end if
                    call self%assemble()
                    call self%apply_bc(prescribed=.false.)
                    call self%freeze_physics_dofs(PHYSICS_TYPES%HYDRAULIC)
                    call self%control%get_nonlinear_iter(iter_nl)
                    self%current_physics_id = PHYSICS_TYPES%THERMAL%ID
                    call self%solve()
                    call self%zero_frozen_increment(PHYSICS_TYPES%HYDRAULIC)

                    if (allocated(self%solver_thermal)) then
                        if (.not. self%solver_thermal%is_success()) then
                            linear_failed = .true.
                            self%last_solve_status = SOLVE_STATUS_LINEAR_FAILURE
                            call self%control%set_converged(PHYSICS_TYPES%THERMAL, .false.)
                            call self%control%set_diverged(PHYSICS_TYPES%THERMAL, .true.)
                            exit thermal_nl
                        end if
                    else if (.not. self%solver%is_success()) then
                        linear_failed = .true.
                        self%last_solve_status = SOLVE_STATUS_LINEAR_FAILURE
                        call self%control%set_converged(PHYSICS_TYPES%THERMAL, .false.)
                        call self%control%set_diverged(PHYSICS_TYPES%THERMAL, .true.)
                        exit thermal_nl
                    end if

                    excessive_update = .false.
                    phase_inc_max = 0.0d0
                    if (allocated(phase_increment)) deallocate (phase_increment)
                    call self%get_variable_increment(PHYSICS_TYPES%THERMAL, phase_increment)
                    if (allocated(phase_increment)) then
                        if (size(phase_increment) > 0) then
                            phase_inc_max = maxval(abs(phase_increment))
                            excessive_update = phase_inc_max > THERMAL_INCREMENT_GUARD
                        end if
                        deallocate (phase_increment)
                    end if
                    if (excessive_update) then
                        write (*, '(A,ES13.5,A,ES13.5,A)') '   [THM_NL] excessive thermal increment detected (> ', &
                            THERMAL_INCREMENT_GUARD, ', max=', phase_inc_max, '). Continue with damped update.'
                    end if

                    call self%solve_time_step_check_convergence(PHYSICS_TYPES%THERMAL)
                    call self%reflect_variables()

                    call self%control%get_nonlinear_iter(iter_nl)
                    if ((.not. self%control%is_converged()) .and. iter_nl >= MAX_PHASE_NL_ITER) then
                        self%last_solve_status = SOLVE_STATUS_NONLINEAR_LIMIT
                        write (*, '(A,I0,A)') '   [THM_NL] reached nonlinear iteration cap (', MAX_PHASE_NL_ITER, &
                            '). Triggering timestep retry.'
                        call self%control%set_converged(PHYSICS_TYPES%THERMAL, .false.)
                        call self%control%set_diverged(PHYSICS_TYPES%THERMAL, .true.)
                        exit thermal_nl
                    end if

                    if (self%control%is_none()) exit thermal_nl
                end do thermal_nl

                call self%control%get_nonlinear_iter(iter_nl)
                self%last_inner_iterations = iter_nl
                self%last_max_inner_iterations = max(self%last_max_inner_iterations, iter_nl)
                self%last_nonlinear_work = self%last_nonlinear_work + max(1_int32, iter_nl)

                is_step_converged = self%control%is_converged()

                if (.not. is_step_converged) then
                    call self%control%get_nonlinear_iter(iter_nl)
                    t_res = 0.0d0
                    t_inc = 0.0d0
                    if (.not. linear_failed) then
                        call self%control%get_current_norm(PHYSICS_TYPES%THERMAL, &
                                                           NONLINEAR_NORM_CRITERIA%RESIDUAL, NORM_TYPES%LINF, t_res)
                        call self%control%get_current_norm(PHYSICS_TYPES%THERMAL, &
                                                           NONLINEAR_NORM_CRITERIA%UPDATE, NORM_TYPES%LINF, t_inc)
                    end if
                    if (linear_failed) then
                        write (*, '(A,A,A,I0,A,L1,A)') '   ', phase_label, &
                            ' failed: iter=', iter_nl, ', diverged=', self%control%is_diverged(), &
                            ', linear solver failure.'
                    else
                        write (*, '(A,A,A,I0,A,L1,A,2(ES11.3,1X))') '   ', phase_label, &
                            ' failed: iter=', iter_nl, ', diverged=', self%control%is_diverged(), &
                            ', T_res/T_inc=', t_res, t_inc
                    end if
                    if (self%last_solve_status == SOLVE_STATUS_NOT_RUN) then
                        if (self%control%is_diverged()) then
                            self%last_solve_status = SOLVE_STATUS_NONLINEAR_DIVERGED
                        else
                            self%last_solve_status = SOLVE_STATUS_NONLINEAR_LIMIT
                        end if
                    end if
                    exit coupling_loop
                end if
            else
                call self%control%set_converged(PHYSICS_TYPES%THERMAL, .true.)
                call self%control%set_diverged(PHYSICS_TYPES%THERMAL, .false.)
                is_step_converged = .true.
            end if

            if (coupling_iter == 1) cycle coupling_loop

            coupling_change_T = 0.0d0
            coupling_change_P = 0.0d0

            call self%temperature%get_current(T_cur)
            call self%pressure%get_current(P_cur)

            if (associated(T_cur)) then
                T_scale = maxval(abs(T_cur)) + 1.0d0
                coupling_change_T = maxval(abs(T_cur - T_old)) / T_scale
            end if
            if (associated(P_cur)) then
                P_scale = maxval(abs(P_cur)) + 1.0d0
                coupling_change_P = maxval(abs(P_cur - P_old)) / P_scale
            end if

            nullify (T_cur)
            nullify (P_cur)

            write (*, '("   [Coupling] Iter:", I2, " dT_rel:", ES10.3, " dP_rel:", ES10.3)') &
                coupling_iter, coupling_change_T, coupling_change_P

            if (coupling_change_T < COUPLING_TOL .and. coupling_change_P < COUPLING_TOL) then
                exit coupling_loop
            end if

        end do coupling_loop

        if (allocated(T_old)) deallocate (T_old)
        if (allocated(P_old)) deallocate (P_old)
        if (allocated(phase_increment)) deallocate (phase_increment)

        if (is_step_converged) then
            self%last_solve_status = SOLVE_STATUS_CONVERGED
        else if (self%last_solve_status == SOLVE_STATUS_NOT_RUN) then
            if (self%control%is_diverged()) then
                self%last_solve_status = SOLVE_STATUS_NONLINEAR_DIVERGED
            else
                self%last_solve_status = SOLVE_STATUS_NONLINEAR_LIMIT
            end if
        end if

    end subroutine solve_time_step_staggered_ftcms

    pure function solve_status_label(status) result(label)
        implicit none
        integer(int32), intent(in) :: status
        character(len=18) :: label

        select case (status)
        case (SOLVE_STATUS_CONVERGED)
            label = "accepted"
        case (SOLVE_STATUS_LINEAR_FAILURE)
            label = "linear_failure"
        case (SOLVE_STATUS_NONLINEAR_DIVERGED)
            label = "nonlinear_diverged"
        case (SOLVE_STATUS_NONLINEAR_LIMIT)
            label = "nonlinear_limit"
        case (SOLVE_STATUS_PHASE_FAILURE)
            label = "phase_failure"
        case (SOLVE_STATUS_LTE_REJECTED)
            label = "lte_rejected"
        case default
            label = "not_run"
        end select
    end function solve_status_label

    !> Write diagnostics after time-control update so dt_next and accepted time
    !> describe the actual state used by the following attempt.
    subroutine write_solver_history_attempt(self, attempt, accepted_step, time_start, time_trial, dt_used, &
                                            accepted, ats_iter, phase_spike, lte_error)
        implicit none
        class(type_ftcms), intent(inout) :: self
        integer(int32), intent(in) :: attempt, accepted_step, ats_iter
        real(real64), intent(in) :: time_start, time_trial, dt_used, lte_error
        logical, intent(in) :: accepted, phase_spike

        character(len=18) :: status
        real(real64) :: time_accepted, dt_next
        real(real64) :: t_res, t_inc, h_res, h_inc
        real(real64) :: omega_used, dq_norm_used

        if (self%solver_history_unit == -1) return

        call self%control%get_time(time_accepted)
        call self%control%get_dt(dt_next)
        omega_used = self%control%get_conserved_relaxation()
        dq_norm_used = self%control%get_conserved_dq_norm()
        status = solve_status_label(self%last_solve_status)

        t_res = -1.0d0
        t_inc = -1.0d0
        h_res = -1.0d0
        h_inc = -1.0d0
        if ((.not. self%control%is_conserved()) .and. &
            self%last_solve_status /= SOLVE_STATUS_LINEAR_FAILURE .and. &
            self%last_solve_status /= SOLVE_STATUS_NOT_RUN) then
            if (self%is_active_thermal()) then
                call self%control%get_current_norm(PHYSICS_TYPES%THERMAL, NONLINEAR_NORM_CRITERIA%RESIDUAL, &
                                                   NORM_TYPES%LINF, t_res)
                call self%control%get_current_norm(PHYSICS_TYPES%THERMAL, NONLINEAR_NORM_CRITERIA%UPDATE, &
                                                   NORM_TYPES%LINF, t_inc)
            end if
            if (self%is_active_hydraulic()) then
                call self%control%get_current_norm(PHYSICS_TYPES%HYDRAULIC, NONLINEAR_NORM_CRITERIA%RESIDUAL, &
                                                   NORM_TYPES%LINF, h_res)
                call self%control%get_current_norm(PHYSICS_TYPES%HYDRAULIC, NONLINEAR_NORM_CRITERIA%UPDATE, &
                                                   NORM_TYPES%LINF, h_inc)
            end if
        end if

        write (self%solver_history_unit, &
               '(2(I10,1X),5(ES15.7,1X),I1,1X,A18,1X,10(I10,1X),12(ES13.5,1X))') &
            attempt, accepted_step, time_start, time_trial, time_accepted, dt_used, dt_next, &
            merge(1_int32, 0_int32, accepted), status, &
            self%last_inner_iterations, self%last_max_inner_iterations, self%last_phase_iterations, &
            self%last_nonlinear_work, ats_iter, self%aa_use_count, merge(1_int32, 0_int32, phase_spike), &
            merge(1_int32, 0_int32, self%last_phase_metrics_available), &
            merge(1_int32, 0_int32, self%last_phase_converged), self%last_phase_active_nodes, &
            self%last_phase_increment_max, self%last_phase_increment_norm, &
            self%last_phase_equilibrium_error, self%last_phase_merit, self%aa_gamma_max_abs, &
            t_res, t_inc, h_res, h_inc, omega_used, dq_norm_used, lte_error
        flush (self%solver_history_unit)
    end subroutine write_solver_history_attempt

    module subroutine run_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        logical :: is_step_converged
        integer(int32) :: consecutive_failures
        integer(int32) :: step_counter
        integer(int32) :: attempt_counter
        integer(int32) :: nl_iter
        integer(int32) :: phase_iter
        integer(int32) :: nonlinear_work
        integer(int32) :: effective_iter
        logical :: phase_iteration_spike
        real(real64) :: time_s, time_start_s, time_trial_s, dt_s, dt_used
        real(real64) :: lte_error
        !> BDF order lte_error was measured at; the retry is resized at it
        integer(int32) :: lte_order
        integer(int32), parameter :: MAX_CONSECUTIVE_FAILURES = 50
        integer(int32), parameter :: PHASE_SPIKE_MIN_ITER = 16

        consecutive_failures = 0
        step_counter = 0
        attempt_counter = 0

        ! Loop until end time
        time_loop: do while (.not. self%control%is_end_time())
            call self%control%get_time(time_start_s)
            call self%run_assimilation(time_start_s, 1.0d0 + time_start_s / 86400.0d0)
            call self%solve_time_step(is_step_converged)
            call self%control%get_dt(dt_used)
            time_trial_s = time_start_s + dt_used
            nl_iter = self%last_inner_iterations
            phase_iter = self%last_phase_iterations
            nonlinear_work = self%last_nonlinear_work
            ! Outer phase work is diagnosed separately. LTE controls temporal
            ! accuracy and the final monolithic inner count supplies the existing
            ! nonlinear robustness brake; reducing dt did not reduce outer work.
            effective_iter = nl_iter
            phase_iteration_spike = phase_iter >= PHASE_SPIKE_MIN_ITER .and. &
                                    2 * phase_iter > 3 * self%last_accepted_phase_iterations

            ! Local-truncation-error estimate for error-controlled ATS, evaluated
            ! before the time/variable history is shifted (needs ydot_n and dt_n).
            lte_error = -1.0d0
            lte_order = 1
            if (is_step_converged) lte_error = self%compute_lte_error(order_used=lte_order)
            if (is_step_converged .and. self%lte_error_control_active .and. lte_error > 1.0d0) then
                is_step_converged = .false.
                self%last_solve_status = SOLVE_STATUS_LTE_REJECTED
                write (*, '(A,ES11.3,A,ES11.3)') &
                    '   [LTE] converged step rejected: normalized error=', lte_error, ', dt[s]=', dt_used
            end if

            attempt_counter = attempt_counter + 1

            if (is_step_converged) then
                self%last_accepted_dt = dt_used
                self%last_accepted_phase_iterations = phase_iter
                ! Genuine step acceptance: is_step_converged has already
                ! survived the LTE rejection check above, so the conserved-
                ! quantity drift measured at the nonlinear loop's last
                ! iterate (check_conserved_convergence_control, criterion 2)
                ! is the one that actually becomes part of the accepted
                ! history. Fold it into the cumulative budget here, not at
                ! every nonlinear iteration, since an iterate that satisfies
                ! the nonlinear gate can still be discarded by LTE or the
                ! outer phase loop before reaching this point.
                call self%control%commit_conserved_drift()
                call self%commit_lte_history()
                ! Update time and adaptive time stepping
                call self%control%update(is_step_converged, error_estimate=lte_error, &
                                         iteration_count=effective_iter, error_order=lte_order)

                consecutive_failures = 0
                step_counter = step_counter + 1
                call write_solver_history_attempt(self, attempt_counter, step_counter, time_start_s, &
                                                  time_trial_s, dt_used, is_step_converged, effective_iter, &
                                                  phase_iteration_spike, lte_error)
                call self%control%get_time(time_s)
                if (step_counter == 1 .or. mod(step_counter, 20) == 0 .or. effective_iter > 8) then
                    write (*, '(A,I0,A,ES13.5,A,I0,A,I0,A,I0)') '   [STEP] converged: n=', step_counter, &
                        ', t[s]=', time_s, ', nonlinear_iter=', nl_iter, &
                        ', outer_iter=', phase_iter, ', nonlinear_work=', nonlinear_work
                end if

                ! Shift variable history on convergence
                call self%shift()

                call self%update_variables()
                call self%output_fields()
                call self%output_history()
            else
                ! Update time and adaptive time stepping
                call self%control%update(is_step_converged, error_estimate=lte_error, &
                                         iteration_count=effective_iter, error_order=lte_order)

                ! Retry with smaller dt
                consecutive_failures = consecutive_failures + 1
                call write_solver_history_attempt(self, attempt_counter, step_counter, time_start_s, &
                                                  time_trial_s, dt_used, is_step_converged, effective_iter, &
                                                  phase_iteration_spike, lte_error)

                if (self%control%is_min_dt()) then
                    call self%control%get_dt(dt_s)
                    write (*, '(A,ES13.5,A)') '   [ERROR] Step failed at minimum dt=', dt_s, '. Stopping retry loop.'
                    exit time_loop
                end if

                write (*, '("   [WARNING] Step Failed (",I0,"/",I0,"). Retrying with smaller dt...")') &
                    consecutive_failures, MAX_CONSECUTIVE_FAILURES
                if (consecutive_failures >= MAX_CONSECUTIVE_FAILURES) then
                    write (*, '("   [ERROR] Too many consecutive failures. Stopping.")')
                    exit time_loop
                end if
                cycle time_loop
            end if

        end do time_loop

    end subroutine run_ftcms

    !> Build the nodal control-volume array V_i = sum_e int psi_i dOmega used
    !> by the convergence control's exact per-node local conservation gate
    !> (check_conserved_convergence_control, criterion 1). Computed once per
    !> run - the mesh is static - from the FE row-sum-lumped mass matrix with
    !> a unit coefficient: the same primitive governing_base uses to build a
    !> lumped physical capacity matrix (compute_K1_lumped), just with A_gp=1
    !> so the result is a pure geometric volume.
    !>
    !> No other nodal-volume provider exists in src/domain or src/numerical:
    !> the node_material_table built in ftcms_base.F90 sums whole adjacent-
    !> element measures per node to weight material averages, which is not
    !> integrated against shape functions and double-counts the volume shared
    !> between elements, so it is not a control volume.
    !>
    !> Cost: O(N_elem) FE evaluations, paid once.
    subroutine build_nodal_volume(self, num_elements)
        implicit none
        class(type_ftcms), intent(inout) :: self
        integer(int32), intent(in) :: num_elements

        class(abst_fe), pointer :: fe
        integer(int32), pointer, contiguous, dimension(:) :: connectivity
        real(real64), allocatable :: coordinates(:, :)
        real(real64), allocatable :: unit_gp(:)
        real(real64), allocatable :: elem_volume(:, :)
        integer(int32) :: num_nodes_total, num_nodes_local, num_gauss_local
        integer(int32) :: i_elem, i_node

        nullify (fe)
        nullify (connectivity)

        call self%domain%get_num_nodes(num_nodes_total)
        call allocate_array(self%nodal_volume, num_nodes_total)
        self%nodal_volume = 0.0d0
        if (num_nodes_total <= 0) return

        do i_elem = 1, num_elements
            call self%domain%get_fe(i_elem, fe)
            if (.not. associated(fe)) cycle
            call self%domain%get_fe_connectivity(i_elem, connectivity)
            if (.not. associated(connectivity)) cycle
            call self%domain%get_fe_coordinate(i_elem, coordinates)

            call fe%get_num_nodes(num_nodes_local)
            call fe%get_num_gauss(num_gauss_local)
            call allocate_array(unit_gp, num_gauss_local)
            unit_gp = 1.0d0
            call allocate_array(elem_volume, num_nodes_local, num_nodes_local)

            call fe%compute_K1_lumped(coordinates, unit_gp, elem_volume)
            do i_node = 1, num_nodes_local
                self%nodal_volume(connectivity(i_node)) = self%nodal_volume(connectivity(i_node)) &
                                                          + elem_volume(i_node, i_node)
            end do
        end do

        nullify (fe)
        nullify (connectivity)
        if (allocated(coordinates)) deallocate (coordinates)
        if (allocated(unit_gp)) deallocate (unit_gp)
        if (allocated(elem_volume)) deallocate (elem_volume)
    end subroutine build_nodal_volume

end submodule ftcms_solve

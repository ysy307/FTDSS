submodule(control_iteration_convergence) convergence_control
    implicit none

    !> Floor for the adaptive under-relaxation factor of the globalized modified
    !> Picard step. Below this the step is considered un-saveable by damping and
    !> the step is declared diverged so the ATS reduces dt instead.
    real(real64), parameter :: CONSERVED_OMEGA_MIN = 5.0d-2
    real(real64), parameter :: CONSERVED_OMEGA_GROW_MAX = 1.05d0
    real(real64), parameter :: CONSERVED_RESIDUAL_DECREASE = 9.5d-1
    real(real64), parameter :: CONSERVED_RESIDUAL_INCREASE = 1.05d0
    !> Consecutive residual increases tolerated before under-relaxation engages.
    !> A moving freezing front makes the coupled residual oscillate iteration to
    !> iteration as front nodes toggle phase; damping on a single increase is a
    !> false divergence signal. Sustained increase over this many iterations is
    !> genuine divergence and is damped as before.
    integer(int32), parameter :: CONSERVED_DIVERGE_PATIENCE = 3
    ! A windowed net-progress test was tried here to catch the oscillating stall
    ! that the consecutive-increase counter above cannot see. It is superseded by
    ! the backtracking line search in ftcms_solve, which enforces a decrease at
    ! every iterate instead of reacting after five of them, and the two actively
    ! conflict: the line search accepts an Armijo decrease that can be arbitrarily
    ! small, which the window test reads as stagnation and damps - measured, omega
    ! collapsed to its floor and the step stopped advancing.
    ! Warm start of the under-relaxation across nonlinear loops: the spectrum
    ! of the coupled Picard map changes little between consecutive time steps
    ! (and between a failed attempt and its retry), so re-exploring omega from
    ! a fixed initial value every loop wastes 2-3 iterations rediscovering the
    ! same optimum. Each loop starts from the previous final omega with a mild
    ! release toward the full step. The floor is deliberately high: carrying a
    ! floored-out omega across steps traps the run in a no-progress state where
    ! the per-iteration change is tiny, the change-norm criterion is satisfied
    ! vacuously, and steps are accepted while the physics stalls.
    real(real64), parameter :: CONSERVED_OMEGA_WARM_RELEASE = 1.25d0
    real(real64), parameter :: CONSERVED_OMEGA_WARM_FLOOR = 2.5d-1
    logical, parameter :: CONSERVED_VERBOSE = .true.

    !> Global-balance gate (redesign criterion 2): one accepted step may not
    !> move the block's domain-summed conserved quantity by more than its own
    !> absolute tolerance integrated over the domain,
    !>   |sum_i R_i| * dt <= atol * V_total.
    !> The bound never references the step's own initial residual R0, which is
    !> what made the previous form unpassable exactly when the predictor
    !> started near the solution.
    !>
    !> The signed running total is accumulated as a V&V observable only, never
    !> as a gate: a cumulative budget would couple step acceptance to run
    !> history, so once the account was spent every subsequent step would fail
    !> regardless of its own quality (measured: balT pinned at 1.014 with the
    !> account form, killing the run at dt_min).
    !>
    !> Report threshold for the cumulative drift diagnostic: print only once
    !> the running total reaches this fraction of the block's own per-step
    !> budget (atol*V_total), so a well-behaved run stays quiet.
    real(real64), parameter :: CONSERVED_DRIFT_REPORT_FRACTION = 1.0d-1

    !> Nonlinear-iteration error budget as a fraction of the time-
    !> discretization (LTE) error the ATS integrator already accepts at E<=1.
    !> The accepted step carries LTE + nonlinear error, so holding the
    !> nonlinear part at a third of the LTE tolerance bounds the total at
    !> 1.33x the error the step-size controller is already regulating - inside
    !> its own safety factor. Demanding more margin than that spends
    !> iterations buying an accuracy the accepted step cannot use, because the
    !> time-discretization error it is measured against is three times larger.
    real(real64), parameter :: NONLINEAR_TO_LTE_FACTOR = 1.0d0 / 3.0d0
    !> Floors for the nodal storage sensitivity s_i in local_error_block, so a
    !> vanishing apparent capacity (e.g. exactly at a phase-change plateau
    !> edge where the latent-heat curvature briefly cancels the sensible
    !> term) cannot divide the local error measure by (near) zero.
    real(real64), parameter :: DH_DT_FLOOR = 1.0d3 ! J/(m3 K)
    real(real64), parameter :: DRHO_DP_FLOOR = 1.0d-12 ! kg/(m3 Pa)
contains

    module subroutine initialize_convergence_control(self, config, max_iterations, reference_values)
        implicit none
        class(type_convergence_control), intent(inout) :: self
        type(type_config_iteration_nonlinear), intent(in) :: config
        integer(int32), intent(in) :: max_iterations
        real(real64), intent(in), optional :: reference_values(:)

        integer(int32) :: i
        logical :: check_res, check_upd

        self%norm_type = config%norm_type
        self%combination_logic = config%combination_logic
        self%convergence_norm_type = config%convergence_norm_type

        self%atol_enthalpy = config%atol_enthalpy
        self%atol_density = config%atol_density
        self%rtol_conserved = config%rtol_conserved
        self%residual_eps = config%residual_eps
        ! Cumulative drift is a whole-run V&V counter (criterion 2), not
        ! per-step state: zeroed here at construction, never by reset() below.
        self%cumulative_drift_thermal = 0.0d0
        self%cumulative_drift_hydraulic = 0.0d0
        call reset_conserved_state(self)

        check_res = self%should_check_residual()
        check_upd = self%should_check_update()

        do i = 1, PHYSICS_TYPES%NUM_ID
            if (present(reference_values)) then
                call self%residual(i)%initialize(config%residual(i), &
                                                 check_res, &
                                                 max_iterations, &
                                                 reference_values(i))
                call self%update(i)%initialize(config%update(i), &
                                               check_upd, &
                                               max_iterations, &
                                               reference_values(i))
            else
                call self%residual(i)%initialize(config%residual(i), &
                                                 check_res, &
                                                 max_iterations)
                call self%update(i)%initialize(config%update(i), &
                                               check_upd, &
                                               max_iterations)
            end if
        end do

    end subroutine initialize_convergence_control

    module subroutine reset_convergence_control(self)
        implicit none
        class(type_convergence_control), intent(inout) :: self

        integer(int32) :: i

        do i = 1, PHYSICS_TYPES%NUM_ID
            call self%residual(i)%reset()
            call self%update(i)%reset()
        end do

        call reset_conserved_state(self)

    end subroutine reset_convergence_control

    module function check_convergence_control(self, physics_type, nonlinear_iter, residual_vector, update_vector) result(is_ok)
        implicit none
        class(type_convergence_control), intent(inout) :: self
        type(type_constant_id), intent(in) :: physics_type
        integer(int32), intent(in) :: nonlinear_iter
        real(real64), intent(in), optional :: residual_vector(:)
        real(real64), intent(in), optional :: update_vector(:)
        logical :: is_ok

        logical :: is_residual_ok, is_update_ok
        logical :: check_residual, check_update

        check_residual = self%should_check_residual()
        check_update = self%should_check_update()

        is_residual_ok = .true.
        is_update_ok = .true.

        ! --- Residual vector check ---
        if (present(residual_vector)) then
            is_residual_ok = self%residual(physics_type%ID)%check_convergence(residual_vector, nonlinear_iter, self%norm_type)
            if (.not. check_residual) is_residual_ok = .true.
        else
            is_residual_ok = .not. check_residual
        end if

        ! --- Update vector check ---
        if (present(update_vector)) then
            is_update_ok = self%update(physics_type%ID)%check_convergence(update_vector, nonlinear_iter, self%norm_type)
            if (.not. check_update) is_update_ok = .true.
        else
            is_update_ok = .not. check_update
        end if

        ! --- Combine Logic (AND / OR) ---
        if (self%combination_logic == NONLINEAR_LOGIC%OR) then
            is_ok = is_residual_ok .or. is_update_ok
        else ! Default AND
            is_ok = is_residual_ok .and. is_update_ok
        end if
    end function check_convergence_control

    module pure function is_initialized_convergence_control(self) result(is_initialized)
        implicit none
        class(type_convergence_control), intent(in) :: self
        logical :: is_initialized

        is_initialized = self%initialized
    end function is_initialized_convergence_control

    module pure function should_check_residual_convergence_control(self) result(should_check)
        implicit none
        class(type_convergence_control), intent(in) :: self
        logical :: should_check

        if (self%convergence_norm_type == NONLINEAR_NORM_CRITERIA%RESIDUAL .or. &
            self%convergence_norm_type == NONLINEAR_NORM_CRITERIA%BOTH) then
            should_check = .true.
        else
            should_check = .false.
        end if
    end function should_check_residual_convergence_control

    module pure function should_check_update_convergence_control(self) result(should_check)
        implicit none
        class(type_convergence_control), intent(in) :: self
        logical :: should_check

        if (self%convergence_norm_type == NONLINEAR_NORM_CRITERIA%UPDATE .or. &
            self%convergence_norm_type == NONLINEAR_NORM_CRITERIA%BOTH) then
            should_check = .true.
        else
            should_check = .false.
        end if
    end function should_check_update_convergence_control

    module subroutine get_norm_type_convergence_control(self, norm_type)
        implicit none
        class(type_convergence_control), intent(in), target :: self
        type(type_constant_id), intent(inout), pointer :: norm_type

        norm_type => self%norm_type
    end subroutine get_norm_type_convergence_control

    module subroutine get_combination_logic_convergence_control(self, combination_logic)
        implicit none
        class(type_convergence_control), intent(in), target :: self
        type(type_constant_id), intent(inout), pointer :: combination_logic

        combination_logic => self%combination_logic
    end subroutine get_combination_logic_convergence_control

    module subroutine get_convergence_norm_type_convergence_control(self, convergence_norm_type)
        implicit none
        class(type_convergence_control), intent(in), target :: self
        type(type_constant_id), intent(inout), pointer :: convergence_norm_type

        convergence_norm_type => self%convergence_norm_type
    end subroutine get_convergence_norm_type_convergence_control

    module subroutine get_current_norm_convergence_control(self, physics_type, criteria_type, &
                                                           norm_type, nonlinear_iter, current_norm)
        implicit none
        class(type_convergence_control), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        type(type_constant_id), intent(in) :: criteria_type
        type(type_constant_id), intent(in) :: norm_type
        integer(int32), intent(in) :: nonlinear_iter
        real(real64), intent(inout) :: current_norm

        if (.not. PHYSICS_TYPES%is_valid(physics_type)) then
            current_norm = 0.0d0
            return
        end if

        if (criteria_type == NONLINEAR_NORM_CRITERIA%RESIDUAL) then
            call self%residual(physics_type%ID)%get_current_norm(norm_type, nonlinear_iter, current_norm)
        else if (criteria_type == NONLINEAR_NORM_CRITERIA%UPDATE) then
            call self%update(physics_type%ID)%get_current_norm(norm_type, nonlinear_iter, current_norm)
        else
            current_norm = 0.0d0
        end if

    end subroutine get_current_norm_convergence_control

    module subroutine get_tolerances_convergence_control(self, physics_type, absolute_tolerance, relative_tolerance)
        implicit none
        class(type_convergence_control), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        real(real64), intent(inout), optional :: absolute_tolerance
        real(real64), intent(inout), optional :: relative_tolerance

        if (.not. PHYSICS_TYPES%is_valid(physics_type)) then
            if (present(absolute_tolerance)) then
                absolute_tolerance = 0.0d0
            end if
            if (present(relative_tolerance)) then
                relative_tolerance = 0.0d0
            end if
            return
        end if

        call self%residual(physics_type%ID)%get_tolerances(absolute_tolerance, relative_tolerance)

    end subroutine get_tolerances_convergence_control

    !> Update per-physics reference values for relative convergence normalization.
    module subroutine update_reference_values_convergence_control(self, reference_values)
        implicit none
        class(type_convergence_control), intent(inout) :: self
        real(real64), intent(in) :: reference_values(:)

        integer(int32) :: i, n

        n = min(size(reference_values), PHYSICS_TYPES%NUM_ID)
        do i = 1, n
            call self%residual(i)%update_reference_value(reference_values(i))
            call self%update(i)%update_reference_value(reference_values(i))
        end do

    end subroutine update_reference_values_convergence_control

    !> Returns true when the conserved-quantity convergence mode is selected.
    module pure function is_conserved_convergence_control(self) result(is_conserved)
        implicit none
        class(type_convergence_control), intent(in) :: self
        logical :: is_conserved

        is_conserved = (self%convergence_norm_type == NONLINEAR_NORM_CRITERIA%CONSERVED)
    end function is_conserved_convergence_control

    !> Capture R(x_0) before the first nonlinear update. Using the residual after
    !> that update can create an arbitrarily small reference when the first linear
    !> solve happens to be nearly exact, making the relative test ill-conditioned.
    module subroutine prime_conserved_residual_convergence_control(self, residual_thermal, residual_hydraulic, &
                                                                   check_thermal, check_hydraulic)
        implicit none
        class(type_convergence_control), intent(inout) :: self
        real(real64), intent(in), optional :: residual_thermal(:)
        real(real64), intent(in), optional :: residual_hydraulic(:)
        logical, intent(in) :: check_thermal
        logical, intent(in) :: check_hydraulic

        if (check_thermal .and. present(residual_thermal)) then
            self%residual0_thermal = max(block_residual_norm(residual_thermal), tiny(1.0d0))
        end if
        if (check_hydraulic .and. present(residual_hydraulic)) then
            self%residual0_hydraulic = max(block_residual_norm(residual_hydraulic), tiny(1.0d0))
        end if
    end subroutine prime_conserved_residual_convergence_control

    !> Current adaptive under-relaxation factor for the globalized modified Picard.
    module pure function get_conserved_dq_norm_convergence_control(self) result(dq_norm)
        implicit none
        class(type_convergence_control), intent(in) :: self
        real(real64) :: dq_norm

        dq_norm = self%dq_norm_prev
    end function get_conserved_dq_norm_convergence_control

    module pure function get_conserved_relaxation_convergence_control(self) result(omega)
        implicit none
        class(type_convergence_control), intent(in) :: self
        real(real64) :: omega

        omega = self%relaxation_omega
    end function get_conserved_relaxation_convergence_control

    !> Conserved-quantity convergence check (PDF 6.2.4). See interface for the
    !> mathematical definition. Mutates the stored previous iterate and counters.
    module subroutine check_conserved_convergence_control(self, enthalpy, density, &
                                                          residual_thermal, residual_hydraulic, &
                                                          nonlinear_iter, check_thermal, check_hydraulic, &
                                                          is_ok, is_diverged)
        implicit none
        class(type_convergence_control), intent(inout) :: self
        real(real64), intent(in) :: enthalpy(:)
        real(real64), intent(in) :: density(:)
        real(real64), intent(in), optional :: residual_thermal(:)
        real(real64), intent(in), optional :: residual_hydraulic(:)
        integer(int32), intent(in) :: nonlinear_iter
        logical, intent(in) :: check_thermal
        logical, intent(in) :: check_hydraulic
        logical, intent(inout) :: is_ok
        logical, intent(inout) :: is_diverged

        real(real64) :: dq_norm, dq_effective, kappa, residual_norm, residual_ratio
        real(real64) :: rT, rH, ratioT, ratioH, balanceT, balanceH
        real(real64) :: floorT, floorH
        real(real64) :: budgetT, budgetH
        !> Local gate (criterion 1): V_i-weighted WRMS of the residual-induced
        !> nodal drift; <= 1 passes. Sentinel -1 means the exact per-node path
        !> was not available and the mean-volume-floor fallback ran instead.
        real(real64) :: e_localT, e_localH
        !> Global gate (criterion 2): signed (sum_i R_i)*dt, the realized
        !> drift of the block's total conserved quantity over the step [J or kg].
        real(real64) :: driftT, driftH
        logical :: dq_ok, residual_ok, balance_ok
        logical :: has_residual_norm
        integer(int32) :: n_cons
        real(real64), allocatable :: dH(:), drho(:)

        is_ok = .false.
        is_diverged = .false.
        dq_norm = huge(0.0d0)
        kappa = 0.0d0
        rT = 0.0d0
        rH = 0.0d0
        ratioT = 0.0d0
        ratioH = 0.0d0
        balanceT = 0.0d0
        balanceH = 0.0d0
        e_localT = -1.0d0
        e_localH = -1.0d0
        driftT = 0.0d0
        driftH = 0.0d0
        residual_norm = 0.0d0
        has_residual_norm = .false.

        ! Divergence guard: NaN in the conserved fields
        if (any(enthalpy /= enthalpy) .or. any(density /= density)) then
            is_diverged = .true.
            return
        end if

        ! Weighted-RMS norm of the inter-iteration conserved-quantity change
        if (self%has_prev_conserved) then
            n_cons = min(size(enthalpy), size(density), size(self%enthalpy_prev), size(self%density_prev))
            allocate (dH(n_cons))
            allocate (drho(n_cons))
            dH(:) = enthalpy(1:n_cons) - self%enthalpy_prev(1:n_cons)
            drho(:) = density(1:n_cons) - self%density_prev(1:n_cons)
            dq_norm = weighted_rms_conserved(self, dH, drho, enthalpy, density)
        end if

        ! Residuals are hard convergence gates. The transient hydraulic block has
        ! storage and therefore no additive-constant nullspace; retaining its mean
        ! component is required to retain the global water-balance equation.
        !
        ! Both gates below are now measured in conserved-quantity units against
        ! absolute physical tolerances, never against the step's own initial
        ! residual R0 (self%residual0_* is still tracked, but only to drive the
        ! omega/kappa globalization further down, a control decision, not this
        ! acceptance decision - see the design note at the top of this file's
        ! history for the rationale: an R0-normalized gate is unpassable exactly
        ! when the predictor starts near the solution, since R0 -> 0 there).
        !
        ! Criterion 1 (local): the primary-variable local error measure
        ! local_error_block, e_i = (R_i*dt/V_i)/(s_i*tau_i) in K or Pa - see
        ! its own doc comment for the full derivation. Exact node by node when
        ! nodal control volumes and the storage-sensitivity snapshot are both
        ! available (sentinel -1 otherwise); falls back to the mean-volume
        ! floor gate on the raw block residual when they are not.
        !
        ! Criterion 2 (global): signed drift account against the run budget
        ! atol*V_total (see the module-header doc). This needs only V_total
        ! (already available whenever the residual floor is), not V_i, so it
        ! does not participate in the criterion-1 fallback.
        residual_ok = .true.
        balance_ok = .true.
        if (check_thermal .and. present(residual_thermal)) then
            rT = block_residual_norm(residual_thermal)
            if (self%residual0_thermal < 0.0d0) self%residual0_thermal = max(rT, tiny(1.0d0))
            ratioT = rT / self%residual0_thermal

            e_localT = self%local_error_block(PHYSICS_TYPES%THERMAL, residual_thermal)
            if (e_localT >= 0.0d0) then
                residual_ok = residual_ok .and. (e_localT <= 1.0d0)
            else
                floorT = residual_floor(self%atol_enthalpy, self%residual_volume_total, &
                                        self%residual_dt, size(residual_thermal))
                residual_ok = residual_ok .and. &
                              (rT <= floorT + self%residual_eps * self%residual0_thermal)
            end if

            driftT = sum(residual_thermal) * self%residual_dt
            self%pending_drift_thermal = driftT
            self%pending_drift_thermal_valid = .true.
            ! Budget in the same primary-variable currency as the local gate
            ! (integrated_tolerance); falls back to the fixed conserved-unit
            ! form when the sensitivity snapshot is unavailable.
            budgetT = -1.0d0
            ! e_localT >= 0 is the signal that the nodal volume/sensitivity
            ! snapshot was available, i.e. the same data this budget needs.
            if (e_localT >= 0.0d0) then
                budgetT = integrated_tolerance(self%nodal_volume, self%dH_dT, self%u_thermal, &
                                               self%atol_temperature_u, self%rtol_u, DH_DT_FLOOR)
            end if
            if (budgetT <= 0.0d0) budgetT = self%atol_enthalpy * self%residual_volume_total
            if (budgetT > 0.0d0) then
                balanceT = abs(driftT) / budgetT
                balance_ok = balance_ok .and. (balanceT <= 1.0d0)
            end if
        else if (check_thermal) then
            residual_ok = .false.
        end if
        if (check_hydraulic .and. present(residual_hydraulic)) then
            rH = block_residual_norm(residual_hydraulic)
            if (self%residual0_hydraulic < 0.0d0) self%residual0_hydraulic = max(rH, tiny(1.0d0))
            ratioH = rH / self%residual0_hydraulic

            e_localH = self%local_error_block(PHYSICS_TYPES%HYDRAULIC, residual_hydraulic)
            if (e_localH >= 0.0d0) then
                residual_ok = residual_ok .and. (e_localH <= 1.0d0)
            else
                floorH = residual_floor(self%atol_density, self%residual_volume_total, &
                                        self%residual_dt, size(residual_hydraulic))
                residual_ok = residual_ok .and. &
                              (rH <= floorH + self%residual_eps * self%residual0_hydraulic)
            end if

            driftH = sum(residual_hydraulic) * self%residual_dt
            self%pending_drift_hydraulic = driftH
            self%pending_drift_hydraulic_valid = .true.
            ! Same integrated-tolerance budget as the thermal block above.
            budgetH = -1.0d0
            if (e_localH >= 0.0d0) then
                budgetH = integrated_tolerance(self%nodal_volume, self%drho_dp, self%u_hydraulic, &
                                               self%atol_pressure_u, self%rtol_u, DRHO_DP_FLOOR)
            end if
            if (budgetH <= 0.0d0) budgetH = self%atol_density * self%residual_volume_total
            if (budgetH > 0.0d0) then
                balanceH = abs(driftH) / budgetH
                balance_ok = balance_ok .and. (balanceH <= 1.0d0)
            end if
        else if (check_hydraulic) then
            residual_ok = .false.
        end if
        if (check_thermal .and. present(residual_thermal)) then
            residual_norm = residual_norm + ratioT * ratioT
            has_residual_norm = .true.
        end if
        if (check_hydraulic .and. present(residual_hydraulic)) then
            residual_norm = residual_norm + ratioH * ratioH
            has_residual_norm = .true.
        end if
        if (has_residual_norm) residual_norm = sqrt(residual_norm)

        ! Convergence requires both the conserved-quantity change and the complete
        ! assembled residual. The residual gate makes the raw change independent
        ! of under-relaxation: a small relaxed update cannot pass while the field
        ! equations remain unsatisfied. A contraction estimate is used only when
        ! no residual is available. It is not valid across active-set changes at a
        ! freezing front, where successive increments need not have one stationary
        ! linear convergence factor even arbitrarily close to the solution.
        ! dq_norm is the sentinel huge(0.0d0) until has_prev_conserved (no
        ! prior iterate to difference against yet); dividing that sentinel by
        ! omega (which can be driven down to CONSERVED_OMEGA_MIN) overflows a
        ! double. Skip the division while the sentinel is in effect and keep
        ! dq_effective at the same "not converged" huge value the raw dq_norm
        ! comparison used before this omega-scaled form was introduced.
        if (self%has_prev_conserved) then
            dq_effective = dq_norm / max(self%relaxation_omega, CONSERVED_OMEGA_MIN)
        else
            dq_effective = huge(0.0d0)
        end if
        dq_ok = self%has_prev_conserved .and. (dq_effective <= 1.0d0)
        if (dq_ok .and. (.not. has_residual_norm) .and. self%dq_norm_prev > 0.0d0) then
            kappa = dq_norm / self%dq_norm_prev
            if (kappa >= 1.0d0) then
                dq_ok = .false.
            else if (kappa > 0.0d0) then
                dq_ok = dq_norm * kappa / (1.0d0 - kappa) <= 1.0d0
            end if
        else if (dq_ok .and. (.not. has_residual_norm)) then
            ! No contraction estimate yet (first measurable change): do not accept
            ! on the raw change alone.
            dq_ok = .false.
        end if
        ! Conserved changes and global balance cannot replace the assembled
        ! field equations. All three are hard acceptance gates.
        is_ok = dq_ok .and. residual_ok .and. balance_ok

        ! Globalize the coupled Picard update using the freshly assembled nonlinear
        ! residual. The conserved-quantity increment can alternate while the true
        ! residual decreases, particularly around a moving freezing front; using
        ! that increment to re-amplify a step caused omega to collapse to its floor.
        ! Recover omega in proportion to a measured contraction, damp it after a
        ! measured increase, and retain it inside a five-percent noise band. The
        ! fallback uses kappa only when no residual exists.
        if (self%has_prev_conserved .and. self%dq_norm_prev > 0.0d0) then
            kappa = dq_norm / self%dq_norm_prev
            if (has_residual_norm .and. self%residual_norm_prev > 0.0d0) then
                residual_ratio = residual_norm / self%residual_norm_prev
                if (residual_ratio > CONSERVED_RESIDUAL_INCREASE) then
                    self%diverge_count = self%diverge_count + 1
                    ! Only under-relax on a SUSTAINED residual increase. A single
                    ! iteration's increase at a moving freezing front is chatter
                    ! from front nodes toggling across the phase boundary, not
                    ! divergence; damping on it collapses omega and stalls the
                    ! hydraulic block, which otherwise contracts at omega = 1.
                    if (self%diverge_count >= CONSERVED_DIVERGE_PATIENCE) then
                        self%relaxation_omega = max(CONSERVED_OMEGA_MIN, &
                                                    self%relaxation_omega / residual_ratio)
                    end if
                else
                    if (residual_ratio < CONSERVED_RESIDUAL_DECREASE) then
                        self%relaxation_omega = min(1.0d0, &
                                                    min(CONSERVED_OMEGA_GROW_MAX, 1.0d0 / &
                                                        max(residual_ratio, tiny(1.0d0))) * &
                                                    self%relaxation_omega)
                    end if
                    self%diverge_count = 0
                end if
            else if (kappa >= 1.0d0) then
                self%relaxation_omega = max(CONSERVED_OMEGA_MIN, &
                                            self%relaxation_omega / (1.0d0 + kappa))
                self%diverge_count = self%diverge_count + 1
            else
                self%diverge_count = 0
            end if
            if (self%diverge_count >= 15 .and. &
                self%relaxation_omega <= CONSERVED_OMEGA_MIN * (1.0d0 + epsilon(1.0d0))) then
                is_diverged = .true.
            end if
        end if

        if (CONSERVED_VERBOSE) then
            ! resT/0, resH/0 are the R0-normalized ratios: control diagnostics
            ! only (they drive omega above), not part of is_ok. locT/locH is
            ! the criterion-1 local gate (<=1 passes, -1 = fallback path).
            ! balT/balH is the criterion-2 global drift ratio (<=1 passes).
            write (*, '(A,I4,A,ES10.3,A,F6.4,A,ES10.3,A,ES10.3,A,ES10.3,A,ES10.3,A,ES10.3,A,ES10.3,A,L1,1X,L1,1X,L1)') &
                '    [Conserved] it:', nonlinear_iter, ' dQeff:', dq_effective, &
                ' om:', self%relaxation_omega, &
                ' resT/0:', ratioT, ' locT:', e_localT, ' balT:', balanceT, &
                ' resH/0:', ratioH, ' locH:', e_localH, ' balH:', balanceH, &
                ' dq/res/is_ok:', dq_ok, residual_ok, is_ok
        end if

        ! Store current iterate as previous for the next check
        if (allocated(self%enthalpy_prev)) deallocate (self%enthalpy_prev)
        if (allocated(self%density_prev)) deallocate (self%density_prev)
        allocate (self%enthalpy_prev(size(enthalpy)))
        allocate (self%density_prev(size(density)))
        self%enthalpy_prev = enthalpy
        self%density_prev = density
        self%has_prev_conserved = .true.
        if (dq_norm < huge(0.0d0)) self%dq_norm_prev = dq_norm
        if (has_residual_norm) self%residual_norm_prev = residual_norm

        if (allocated(dH) .and. allocated(drho)) then
            if (allocated(self%dH_prev)) then
                if (size(self%dH_prev) /= size(dH)) deallocate (self%dH_prev)
            end if
            if (allocated(self%drho_prev)) then
                if (size(self%drho_prev) /= size(drho)) deallocate (self%drho_prev)
            end if
            if (.not. allocated(self%dH_prev)) allocate (self%dH_prev(size(dH)))
            if (.not. allocated(self%drho_prev)) allocate (self%drho_prev(size(drho)))
            self%dH_prev(:) = dH(:)
            self%drho_prev(:) = drho(:)
            self%has_prev_conserved_increment = .true.
        end if
    end subroutine check_conserved_convergence_control

    !> Fold the pending per-block drift into the cumulative budget (PDF 6.2.4
    !> redesign, criterion 2). See interface for when the caller must invoke
    !> this. Not a gate: the cumulative total is only ever reported, never
    !> compared against a threshold to reject a step, since nothing requires
    !> the sign of the per-step drift to average to zero across steps and no
    !> V&V evidence yet exists for a sound cumulative bound.
    module subroutine commit_conserved_drift_convergence_control(self)
        implicit none
        class(type_convergence_control), intent(inout) :: self

        real(real64) :: budget_thermal, budget_hydraulic

        if (self%pending_drift_thermal_valid) then
            self%cumulative_drift_thermal = self%cumulative_drift_thermal + self%pending_drift_thermal
            self%pending_drift_thermal_valid = .false.
        end if
        if (self%pending_drift_hydraulic_valid) then
            self%cumulative_drift_hydraulic = self%cumulative_drift_hydraulic + self%pending_drift_hydraulic
            self%pending_drift_hydraulic_valid = .false.
        end if

        budget_thermal = self%atol_enthalpy * self%residual_volume_total
        budget_hydraulic = self%atol_density * self%residual_volume_total
        if (budget_thermal > 0.0d0 .and. &
            abs(self%cumulative_drift_thermal) > CONSERVED_DRIFT_REPORT_FRACTION * budget_thermal) then
            write (*, '(A,ES11.3,A,ES11.3,A)') &
                '   [Conserved] cumulative drift D_thermal=', self%cumulative_drift_thermal, &
                ' J (', 100.0d0 * self%cumulative_drift_thermal / budget_thermal, '% of one-step budget)'
        end if
        if (budget_hydraulic > 0.0d0 .and. &
            abs(self%cumulative_drift_hydraulic) > CONSERVED_DRIFT_REPORT_FRACTION * budget_hydraulic) then
            write (*, '(A,ES11.3,A,ES11.3,A)') &
                '   [Conserved] cumulative drift D_hydraulic=', self%cumulative_drift_hydraulic, &
                ' kg (', 100.0d0 * self%cumulative_drift_hydraulic / budget_hydraulic, '% of one-step budget)'
        end if
    end subroutine commit_conserved_drift_convergence_control

    !> Weighted-RMS norm of (Q_b - Q_a) for the Richardson local-error estimate.
    module subroutine compute_error_norm_convergence_control(self, enthalpy_a, density_a, &
                                                             enthalpy_b, density_b, eps)
        implicit none
        class(type_convergence_control), intent(in) :: self
        real(real64), intent(in) :: enthalpy_a(:)
        real(real64), intent(in) :: density_a(:)
        real(real64), intent(in) :: enthalpy_b(:)
        real(real64), intent(in) :: density_b(:)
        real(real64), intent(inout) :: eps

        ! p = 1 Richardson: e = (Q_b - Q_a)/(2^p - 1) = Q_b - Q_a ; eps = ||e||_W
        eps = weighted_rms_conserved(self, enthalpy_b - enthalpy_a, &
                                     density_b - density_a, enthalpy_b, density_b)
    end subroutine compute_error_norm_convergence_control

    ! --------------------------------------------------------------------------
    ! Submodule-local helpers for the conserved-quantity convergence
    ! --------------------------------------------------------------------------

    !> Clear the conserved-quantity convergence state at the start of a time step.
    subroutine reset_conserved_state(self)
        implicit none
        type(type_convergence_control), intent(inout) :: self

        if (allocated(self%enthalpy_prev)) deallocate (self%enthalpy_prev)
        if (allocated(self%density_prev)) deallocate (self%density_prev)
        if (allocated(self%dH_prev)) deallocate (self%dH_prev)
        if (allocated(self%drho_prev)) deallocate (self%drho_prev)
        self%has_prev_conserved = .false.
        self%has_prev_conserved_increment = .false.
        self%residual0_thermal = -1.0d0
        self%residual0_hydraulic = -1.0d0
        self%residual_norm_prev = -1.0d0
        self%dq_norm_prev = -1.0d0
        self%diverge_count = 0
        ! A fresh step attempt has no confirmed iterate yet; the previous
        ! attempt's pending drift (if any) must not be committed by a later,
        ! unrelated call to commit_conserved_drift.
        self%pending_drift_thermal_valid = .false.
        self%pending_drift_hydraulic_valid = .false.
        self%relaxation_omega = min(1.0d0, max(CONSERVED_OMEGA_WARM_FLOOR, &
                                               CONSERVED_OMEGA_WARM_RELEASE * self%relaxation_omega))
    end subroutine reset_conserved_state

    module subroutine get_residual_floors_convergence_control(self, num_nodes, floor_thermal, floor_hydraulic)
        implicit none
        class(type_convergence_control), intent(in) :: self
        integer(int32), intent(in) :: num_nodes
        real(real64), intent(inout) :: floor_thermal
        real(real64), intent(inout) :: floor_hydraulic

        floor_thermal = residual_floor(self%atol_enthalpy, self%residual_volume_total, &
                                       self%residual_dt, num_nodes)
        floor_hydraulic = residual_floor(self%atol_density, self%residual_volume_total, &
                                         self%residual_dt, num_nodes)
    end subroutine get_residual_floors_convergence_control

    module subroutine set_residual_scale_convergence_control(self, volume_total, dt, nodal_volume, &
                                                              dH_dT, drho_dp, u_thermal, u_hydraulic, &
                                                              atol_temperature_u, atol_pressure_u, rtol_u)
        implicit none
        class(type_convergence_control), intent(inout) :: self
        real(real64), intent(in) :: volume_total
        real(real64), intent(in) :: dt
        real(real64), intent(in), optional :: nodal_volume(:)
        real(real64), intent(in), optional :: dH_dT(:)
        real(real64), intent(in), optional :: drho_dp(:)
        real(real64), intent(in), optional :: u_thermal(:)
        real(real64), intent(in), optional :: u_hydraulic(:)
        real(real64), intent(in), optional :: atol_temperature_u
        real(real64), intent(in), optional :: atol_pressure_u
        real(real64), intent(in), optional :: rtol_u

        self%residual_volume_total = volume_total
        self%residual_dt = dt
        if (present(nodal_volume)) then
            if (size(nodal_volume) > 0) call allocate_array(self%nodal_volume, nodal_volume)
        end if
        if (present(dH_dT)) call allocate_array(self%dH_dT, dH_dT)
        if (present(drho_dp)) call allocate_array(self%drho_dp, drho_dp)
        if (present(u_thermal)) call allocate_array(self%u_thermal, u_thermal)
        if (present(u_hydraulic)) call allocate_array(self%u_hydraulic, u_hydraulic)
        if (present(atol_temperature_u)) self%atol_temperature_u = atol_temperature_u
        if (present(atol_pressure_u)) self%atol_pressure_u = atol_pressure_u
        if (present(rtol_u)) self%rtol_u = rtol_u
    end subroutine set_residual_scale_convergence_control

    !> Absolute residual floor for a block, from its conserved-quantity atol.
    !>
    !> An imbalance R_i sustained over the step changes the nodal conserved
    !> quantity by R_i*dt/V_i, so requiring that change to stay under atol gives
    !> |R_i| <= atol*V_i/dt and, for the L2 norm over N nodes with a mean control
    !> volume V_total/N, ||R||_2 <= atol*V_total/(dt*sqrt(N)). Returns zero when
    !> the scales are unset, which reproduces the pure-ratio gate exactly.
    pure function residual_floor(atol, volume_total, dt, num_nodes) result(floor_value)
        implicit none
        real(real64), intent(in) :: atol, volume_total, dt
        integer(int32), intent(in) :: num_nodes
        real(real64) :: floor_value

        floor_value = 0.0d0
        if (volume_total <= 0.0d0 .or. dt <= 0.0d0 .or. num_nodes <= 0) return
        floor_value = atol * volume_total / (dt * sqrt(real(num_nodes, real64)))
    end function residual_floor

    !> L2 norm of the complete block residual, including its mean component.
    function block_residual_norm(r) result(norm)
        implicit none
        real(real64), intent(in) :: r(:)
        real(real64) :: norm

        if (size(r) <= 0) then
            norm = 0.0d0
            return
        end if

        norm = vector_norm2(r)
    end function block_residual_norm

    !> Weighted root-mean-square norm (PDF eq 6.2.3) of a conserved-quantity
    !> increment (dH, drho) using weights w = atol + rtol*|Q|.
    pure function weighted_rms_conserved(self, dH, drho, H, rho) result(norm)
        implicit none
        type(type_convergence_control), intent(in) :: self
        real(real64), intent(in) :: dH(:), drho(:), H(:), rho(:)
        real(real64) :: norm

        integer(int32) :: j, n
        real(real64) :: wH, wR, acc

        n = min(size(dH), size(drho), size(H), size(rho))
        acc = 0.0d0
        do j = 1, n
            wH = self%atol_enthalpy + self%rtol_conserved * abs(H(j))
            wR = self%atol_density + self%rtol_conserved * abs(rho(j))
            if (wH > 0.0d0) acc = acc + (dH(j) / wH)**2
            if (wR > 0.0d0) acc = acc + (drho(j) / wR)**2
        end do

        if (n > 0) then
            norm = sqrt(acc / real(2 * n, real64))
        else
            norm = 0.0d0
        end if
    end function weighted_rms_conserved

    !> See the interface for the mathematical definition. Dispatches on the
    !> requested block to the matching cached sensitivity/primary-variable
    !> snapshot, then delegates the shared per-node accumulation to
    !> local_error_wrms. Returns the not-available sentinel -1 when
    !> nodal_volume, the block's sensitivity array or its primary-variable
    !> snapshot is unallocated or does not match residual in size.
    module function local_error_block_convergence_control(self, physics_type, residual) result(e_local)
        implicit none
        class(type_convergence_control), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        real(real64), intent(in) :: residual(:)
        real(real64) :: e_local

        integer(int32) :: n

        e_local = -1.0d0
        if (self%residual_dt <= 0.0d0) return
        if (.not. allocated(self%nodal_volume)) return
        n = size(residual)
        if (size(self%nodal_volume) /= n) return

        if (physics_type%ID == PHYSICS_TYPES%THERMAL%ID) then
            if (.not. allocated(self%dH_dT) .or. .not. allocated(self%u_thermal)) return
            if (size(self%dH_dT) /= n .or. size(self%u_thermal) /= n) return
            e_local = local_error_wrms(residual, self%nodal_volume, self%dH_dT, self%u_thermal, &
                                       self%atol_temperature_u, self%rtol_u, DH_DT_FLOOR, self%residual_dt)
        else if (physics_type%ID == PHYSICS_TYPES%HYDRAULIC%ID) then
            if (.not. allocated(self%drho_dp) .or. .not. allocated(self%u_hydraulic)) return
            if (size(self%drho_dp) /= n .or. size(self%u_hydraulic) /= n) return
            e_local = local_error_wrms(residual, self%nodal_volume, self%drho_dp, self%u_hydraulic, &
                                       self%atol_pressure_u, self%rtol_u, DRHO_DP_FLOOR, self%residual_dt)
        end if
    end function local_error_block_convergence_control

    !> Domain-integrated tolerance of a block, in the block's conserved units:
    !> sum_i V_i * s_i * tau_i with the SAME per-node tolerance tau_i the local
    !> measure uses.
    !>
    !> This is the budget the global-balance gate belongs against. The local
    !> gate converts the residual to primary-variable units through s_i =
    !> dH/dT (or d rho/dp); leaving the balance gate on a fixed conserved-unit
    !> tolerance instead makes the two gates disagree about what "at tolerance"
    !> means, and at a freezing front they disagree by the ratio of latent to
    !> sensible capacity - two orders of magnitude. Measured: an iterate whose
    !> local error was 0.117 (eight times inside tolerance) was rejected by a
    !> balance gate reading 2.16.
    pure function integrated_tolerance(volume, sensitivity, u, atol_u, rtol_u, s_floor) result(budget)
        implicit none
        real(real64), intent(in) :: volume(:), sensitivity(:), u(:)
        real(real64), intent(in) :: atol_u, rtol_u, s_floor
        real(real64) :: budget

        integer(int32) :: i, n
        real(real64) :: s_i, tol_i

        n = min(size(volume), size(sensitivity), size(u))
        budget = 0.0d0
        do i = 1, n
            s_i = max(abs(sensitivity(i)), s_floor)
            tol_i = NONLINEAR_TO_LTE_FACTOR * (atol_u + rtol_u * abs(u(i)))
            budget = budget + volume(i) * s_i * tol_i
        end do
    end function integrated_tolerance

    !> Primary-variable-unit local error measure shared by the thermal and
    !> hydraulic blocks: e_i = (R_i*dt/V_i) / (s_i*tau_i), tau_i =
    !> NONLINEAR_TO_LTE_FACTOR*(atol_u+rtol_u*|u_i|). See local_error_block
    !> for the full derivation; s_i is floored at s_floor away from zero.
    pure function local_error_wrms(residual, volume, sensitivity, u, atol_u, rtol_u, s_floor, dt) result(e_local)
        implicit none
        real(real64), intent(in) :: residual(:), volume(:), sensitivity(:), u(:)
        real(real64), intent(in) :: atol_u, rtol_u, s_floor, dt
        real(real64) :: e_local

        integer(int32) :: i, n
        real(real64) :: s_i, tol_i, denom, acc

        n = min(size(residual), size(volume), size(sensitivity), size(u))
        acc = 0.0d0
        do i = 1, n
            s_i = max(abs(sensitivity(i)), s_floor)
            tol_i = NONLINEAR_TO_LTE_FACTOR * (atol_u + rtol_u * abs(u(i)))
            denom = volume(i) * s_i * tol_i
            if (denom > 0.0d0) acc = acc + (residual(i) * dt / denom)**2
        end do

        if (n > 0) then
            e_local = sqrt(acc / real(n, real64))
        else
            e_local = 0.0d0
        end if
    end function local_error_wrms

end submodule convergence_control

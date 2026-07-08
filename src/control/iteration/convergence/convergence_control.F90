submodule(control_iteration_convergence) convergence_control
    implicit none

    !> Floor for the adaptive under-relaxation factor of the globalized modified
    !> Picard step. Below this the step is considered un-saveable by damping and
    !> the step is declared diverged so the ATS reduces dt instead.
    real(real64), parameter :: CONSERVED_OMEGA_MIN = 1.0d-4
    real(real64), parameter :: CONSERVED_OMEGA_GROW_MAX = 2.0d0
    real(real64), parameter :: CONSERVED_KAPPA_RECOVER = 5.0d-1
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
    logical, parameter :: CONSERVED_VERBOSE = .false.
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

        real(real64) :: dq_norm, kappa, lambda_est
        real(real64) :: aitken_omega, numerator, denominator
        real(real64) :: wH, wR, z, z_prev, dz
        real(real64) :: rT, rH, ratioT, ratioH
        logical :: dq_ok
        logical :: omega_updated
        integer(int32) :: j, n_cons
        real(real64), allocatable :: dH(:), drho(:)

        is_ok = .false.
        is_diverged = .false.
        dq_norm = huge(0.0d0)
        kappa = 0.0d0
        rT = 0.0d0
        rH = 0.0d0
        ratioT = 0.0d0
        ratioH = 0.0d0

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

        ! Per-block residual ratios; reference ||R^0|| captured on first evaluation
        ! Block residual ratios (peak-relative, with the constant/nullspace mode
        ! removed) are computed for monitoring and divergence context only. They are
        ! intentionally NOT a hard convergence gate: for stiff and all-Neumann blocks
        ! the Picard residual reduces far more slowly than the solution change, and
        ! forcing extra iterations to satisfy a residual ratio destabilises the
        ! iteration. Convergence is governed by the weighted conserved-quantity change
        ! ||dQ||_W <= 1 (a complete WRMS criterion over energy and water mass, PDF
        ! 6.2.3-6.2.4); genuine divergence is caught by the kappa monitor below.
        if (check_thermal .and. present(residual_thermal)) then
            rT = block_residual_norm(residual_thermal)
            self%residual0_thermal = max(self%residual0_thermal, rT, tiny(1.0d0))
            ratioT = rT / self%residual0_thermal
        end if
        if (check_hydraulic .and. present(residual_hydraulic)) then
            rH = block_residual_norm(residual_hydraulic)
            self%residual0_hydraulic = max(self%residual0_hydraulic, rH, tiny(1.0d0))
            ratioH = rH / self%residual0_hydraulic
        end if

        ! Convergence is the complete weighted-RMS criterion over the conserved
        ! quantities (energy and water mass, PDF 6.2.3-6.2.4), CORRECTED for the
        ! contraction rate: for a fixed-point iteration the true error obeys
        ! ||e_k|| <= ||dQ_k|| * kappa/(1 - kappa), so the raw change ||dQ||_W <= 1
        ! alone is vacuous when the step is strongly under-relaxed (small change
        ! per iteration says nothing about distance to the fixed point). Requiring
        ! the kappa-corrected bound as well makes acceptance omega-independent and
        ! prevents "converged" steps in which the physics has silently stalled.
        dq_ok = self%has_prev_conserved .and. (dq_norm <= 1.0d0)
        if (dq_ok .and. self%dq_norm_prev > 0.0d0) then
            kappa = dq_norm / self%dq_norm_prev
            if (kappa >= 1.0d0) then
                dq_ok = .false.
            else if (kappa > 0.0d0) then
                dq_ok = dq_norm * kappa / (1.0d0 - kappa) <= 1.0d0
            end if
        else if (dq_ok) then
            ! No contraction estimate yet (first measurable change): do not accept
            ! on the raw change alone.
            dq_ok = .false.
        end if
        is_ok = dq_ok

        ! Convergence-rate monitoring (PDF 6.2.4.3) with coupled globalization.
        ! The next Picard update uses the same omega for T and p, so freezing-front
        ! growth in the conserved variables cannot be hidden by independent block
        ! relaxation. For a growing fixed-point map, reducing omega by 1/(1+kappa)
        ! is a scale-free way to move the relaxed map back toward contraction; when
        ! the conserved change contracts strongly, omega is released back toward 1.
        if (self%has_prev_conserved .and. self%dq_norm_prev > 0.0d0) then
            kappa = dq_norm / self%dq_norm_prev
            if (kappa >= 1.0d0) then
                self%diverge_count = self%diverge_count + 1
            else
                self%diverge_count = 0
            end if

            omega_updated = .false.
            if (self%has_prev_conserved_increment .and. allocated(dH) .and. allocated(drho) .and. &
                allocated(self%dH_prev) .and. allocated(self%drho_prev)) then
                if (size(self%dH_prev) == size(dH) .and. size(self%drho_prev) == size(drho)) then
                    numerator = 0.0d0
                    denominator = 0.0d0
                    do j = 1, size(dH)
                        wH = self%atol_enthalpy + self%rtol_conserved * abs(enthalpy(j))
                        wR = self%atol_density + self%rtol_conserved * abs(density(j))

                        if (wH > 0.0d0) then
                            z = dH(j) / wH
                            z_prev = self%dH_prev(j) / wH
                            dz = z - z_prev
                            numerator = numerator + dz * z_prev
                            denominator = denominator + dz * dz
                        end if

                        if (wR > 0.0d0) then
                            z = drho(j) / wR
                            z_prev = self%drho_prev(j) / wR
                            dz = z - z_prev
                            numerator = numerator + dz * z_prev
                            denominator = denominator + dz * dz
                        end if
                    end do

                    if (denominator > epsilon(1.0d0)) then
                        aitken_omega = -self%relaxation_omega * (numerator / denominator)
                        if (aitken_omega == aitken_omega .and. aitken_omega > 0.0d0 .and. &
                            abs(aitken_omega) < huge(1.0d0)) then
                            self%relaxation_omega = max(CONSERVED_OMEGA_MIN, min(1.0d0, aitken_omega))
                            omega_updated = .true.
                        end if
                    end if
                end if
            end if

            if (.not. omega_updated) then
                if (kappa >= 1.0d0) then
                    self%relaxation_omega = max(CONSERVED_OMEGA_MIN, &
                                                self%relaxation_omega / (1.0d0 + kappa))
                else
                    ! Fallback rate estimate for the first two increments or
                    ! degenerate Aitken denominators.
                    lambda_est = 1.0d0 - (1.0d0 - kappa) / max(self%relaxation_omega, tiny(1.0d0))
                    if (lambda_est >= 0.0d0 .and. lambda_est < 1.0d0) then
                        ! The relaxed map is behaving like a monotone contraction:
                        ! kappa ~= 1 - omega*(1-lambda). In that regime the unrelaxed
                        ! step has contraction factor lambda < kappa, so holding omega
                        ! at 0.5 only slows convergence without adding stability.
                        self%relaxation_omega = 1.0d0
                    else if (lambda_est < 0.0d0) then
                        ! A negative inferred fixed-point factor means the relaxed map
                        ! is alternating across the front. Treat that as oscillatory
                        ! contraction, not as permission to release omega to 1; otherwise
                        ! a small kappa is followed by a full-step overshoot and omega
                        ! ratchets down to its floor.
                        self%relaxation_omega = max(CONSERVED_OMEGA_MIN, &
                                                    self%relaxation_omega / (1.0d0 + kappa))
                    else if (kappa < CONSERVED_KAPPA_RECOVER) then
                        self%relaxation_omega = min(1.0d0, &
                                                    self%relaxation_omega * &
                                                    min(CONSERVED_OMEGA_GROW_MAX, &
                                                        CONSERVED_KAPPA_RECOVER / max(kappa, tiny(1.0d0))))
                    end if
                end if
            end if
            if (self%diverge_count >= 15 .and. &
                self%relaxation_omega <= CONSERVED_OMEGA_MIN * (1.0d0 + epsilon(1.0d0))) then
                is_diverged = .true.
            end if
        end if

        if (CONSERVED_VERBOSE) then
            write (*, '(A,I4,A,ES12.5,A,F6.4,A,ES10.3,A,ES10.3,A,ES10.3)') &
                '    [Conserved] iter:', nonlinear_iter, '  ||dQ||_W:', dq_norm, &
                '  omega:', self%relaxation_omega, '  kappa:', kappa, &
                '  resT/0:', ratioT, '  resH/0:', ratioH
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
        self%dq_norm_prev = -1.0d0
        self%diverge_count = 0
        self%relaxation_omega = min(1.0d0, max(CONSERVED_OMEGA_WARM_FLOOR, &
                                               CONSERVED_OMEGA_WARM_RELEASE * self%relaxation_omega))
    end subroutine reset_conserved_state

    !> L2 norm of a block residual with the constant (nullspace) mode removed.
    !> For all-Neumann blocks (e.g. a closed/flux-only hydraulic domain) the
    !> consistent residual is defined only up to an additive constant; projecting
    !> that mode out makes the residual-decrease criterion meaningful and universal,
    !> while leaving well-posed blocks essentially unchanged near convergence.
    function block_residual_norm(r) result(norm)
        implicit none
        real(real64), intent(in) :: r(:)
        real(real64) :: norm

        integer(int32) :: n
        real(real64) :: mean

        n = size(r)
        if (n <= 0) then
            norm = 0.0d0
            return
        end if

        mean = sum(r) / real(n, real64)
        norm = vector_norm2(r - mean)
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

end submodule convergence_control

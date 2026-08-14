module control_iteration_convergence
    use, intrinsic :: iso_fortran_env
    use :: stdlib_optval, only:optval
    use :: module_core
    use :: module_linalg, only:vector_norm1, vector_norm2, vector_norminf
    implicit none
    private

    public :: type_convergence_criterion
    public :: type_convergence_control

    !>
    !> Individual convergence criterion (residual or update, for one physical quantity)
    !>
    type :: type_convergence_criterion
        !> Flag to indicate whether this criterion has been initialized
        logical, private :: initialized = .false.
        !> Flags whether to check convergence for this criterion
        logical, private :: should_check = .false.
        !> Criterion type (Absolute, Relative, Both, None)
        type(type_constant_id), private :: criterion = NONLINEAR_CRITERIA%none
        !> Tolerance of absolute error
        real(real64), private :: absolute_tolerance = 1.0d-8
        !> Tolerance of relative errorF
        real(real64), private :: relative_tolerance = 1.0d-6
        !> Maximum number of iterations for which to store norm history (used for convergence check and debugging)
        integer(int32), private :: max_iterations = 0
        !> Reference value for relative error evaluation.
        !> This value is used as the denominator to normalize the error.
        !> The default is 1.0, which effectively performs an absolute error check.
        !> Typically, this should be set to the characteristic physical scale (e.g., max-min range).
        real(real64), private :: reference_value = 1.0d0
        !> Norm history for each iteration
        !> Dimension: (num_norm_type, max_iteration)
        !> (1,:) -> L1 norm history
        !> (2,:) -> L2 norm history
        !> (3,:) -> LInf norm history
        real(real64), private, allocatable :: norms_history(:, :)
    contains
        ! ---- Lifecycle ----
        procedure, public, pass(self) :: initialize => initialize_type_convergence_criterion
        procedure, public, pass(self) :: destroy => destroy_type_convergence_criterion
        ! ---- Mutator ----
        procedure, public, pass(self) :: reset => reset_criterion
        procedure, public, pass(self) :: update_reference_value => update_reference_value_type_convergence_criterion
        ! ---- Algorithm / Operation ----
        procedure, public, pass(self) :: check_convergence => check_convergence_criterion
        ! ---- Inquiry ----
        ! ---- Getter ----
        procedure, public, pass(self) :: get_current_norm => get_current_norm_convergence_criterion
        procedure, public, pass(self) :: get_tolerances => get_tolerances_convergence_criterion
        ! ---- Meta / Utility ----
    end type type_convergence_criterion

    interface
        module subroutine initialize_type_convergence_criterion(self, config, should_check, max_iterations, reference_value)
            implicit none
            class(type_convergence_criterion), intent(inout) :: self
            type(type_config_iteration_criterion), intent(in) :: config
            logical, intent(in) :: should_check
            integer(int32), intent(in) :: max_iterations
            real(real64), intent(in), optional :: reference_value

        end subroutine initialize_type_convergence_criterion

        module subroutine destroy_type_convergence_criterion(self)
            implicit none
            class(type_convergence_criterion), intent(inout) :: self

        end subroutine destroy_type_convergence_criterion

        module subroutine reset_criterion(self)
            implicit none
            class(type_convergence_criterion), intent(inout) :: self

        end subroutine reset_criterion

        module function check_convergence_criterion(self, vector, iter, norm_type) result(is_ok)
            implicit none
            class(type_convergence_criterion), intent(inout) :: self
            real(real64), intent(in) :: vector(:)
            integer(int32), intent(in) :: iter
            type(type_constant_id), intent(in) :: norm_type
            logical :: is_ok

        end function check_convergence_criterion

        module subroutine get_current_norm_convergence_criterion(self, norm_type, nonlinear_iter, current_norm)
            implicit none
            class(type_convergence_criterion), intent(in) :: self
            type(type_constant_id), intent(in) :: norm_type
            integer(int32), intent(in) :: nonlinear_iter
            real(real64), intent(inout) :: current_norm

        end subroutine get_current_norm_convergence_criterion

        module subroutine get_tolerances_convergence_criterion(self, absolute_tolerance, relative_tolerance)
            implicit none
            class(type_convergence_criterion), intent(in) :: self
            real(real64), intent(inout), optional :: absolute_tolerance
            real(real64), intent(inout), optional :: relative_tolerance

        end subroutine get_tolerances_convergence_criterion

        module subroutine update_reference_value_type_convergence_criterion(self, reference_value)
            implicit none
            class(type_convergence_criterion), intent(inout) :: self
            real(real64), intent(in) :: reference_value

        end subroutine update_reference_value_type_convergence_criterion

    end interface

    !>
    !> Overall convergence control settings
    !>
    type :: type_convergence_control
        !> Flag to indicate whether this control has been initialized
        logical :: initialized = .false.
        !> Norm type used for convergence check (L2, LInf, etc.)
        type(type_constant_id), private :: norm_type = NORM_TYPES%L2
        !> Combination logic between multiple criteria (Residual and Update) (AND, OR)
        type(type_constant_id), private :: combination_logic = NONLINEAR_LOGIC%AND
        !> Convergence criterion target in NONLINEAR_NORM_CRITERIA:
        !>   RESIDUAL : norm of residual vector
        !>   UPDATE   : norm of solution update vector
        !>   BOTH     : both residual and update norms
        type(type_constant_id), private :: convergence_norm_type = NONLINEAR_NORM_CRITERIA%RESIDUAL
        !> Residual vector convergence criteria (for each physical quantity)
        type(type_convergence_criterion), private :: residual(PHYSICS_TYPES%NUM_ID)
        !> Update vector convergence criteria (for each physical quantity)
        type(type_convergence_criterion), private :: update(PHYSICS_TYPES%NUM_ID)

        ! --- Conserved-quantity convergence state (PDF 6.2.4) ---
        !> Absolute weight floor for enthalpy [J/m3]
        real(real64), private :: atol_enthalpy = 1.0d2
        !> Absolute weight floor for effective density [kg/m3]
        real(real64), private :: atol_density = 1.0d-3
        !> Relative weight for both conserved quantities [-]
        real(real64), private :: rtol_conserved = 1.0d-3
        !> Per-block residual ratio tolerance eps_R [-]
        real(real64), private :: residual_eps = 1.0d-3
        !> Physical scales that turn the residual gate from a pure ratio into the
        !> standard atol + rtol form. A ratio to the step's own initial residual
        !> is not scale invariant: once the iteration has reduced the residual to
        !> the floor the frozen-coefficient linearization can reach, the ratio
        !> saturates and the gate becomes unreachable even though the conserved
        !> quantities and the global balance are both well inside tolerance.
        !> The floor is derived, not tuned: an imbalance R may not move the nodal
        !> conserved quantity by more than its own atol over the step, so
        !> |R_i| <= atol * V_i / dt, hence ||R||_2 <= atol * V_total / (dt*sqrt(N)).
        !>
        !> Retained only for the mean-volume-floor fallback path (no nodal
        !> volumes supplied) of the local gate below and for the line-search
        !> merit scaling in ftcms_solve.F90; it is not part of the acceptance
        !> decision on the exact per-node path.
        real(real64), private :: residual_volume_total = -1.0d0
        real(real64), private :: residual_dt = -1.0d0
        !> Nodal control volumes V_i = sum_e int psi_i dOmega [m3], one entry
        !> per domain node, in the same node ordering as enthalpy/density and
        !> the per-block residual vectors. Built once (mesh is static) from the
        !> FE row-sum-lumped mass matrix (see set_residual_scale). Size 0 means
        !> "not supplied": the local gate then falls back to the mean-volume
        !> floor (residual_volume_total / N) for backward compatibility.
        real(real64), allocatable, private :: nodal_volume(:)
        !> Nodal storage sensitivity of the thermal conserved quantity to its
        !> primary variable, \( s_i = (\partial H/\partial T)_i \) [J/(m3 K)].
        !> State-dependent, unlike nodal_volume: refreshed every time the
        !> nodal conserved quantities themselves are recomputed (see
        !> set_residual_scale and its call site in
        !> solve_time_step_check_convergence_conserved). Size 0 means "not
        !> supplied": local_error_block then returns its not-available
        !> sentinel and the caller falls back to the mean-volume floor gate.
        real(real64), allocatable, private :: dH_dT(:)
        !> Nodal storage sensitivity of the hydraulic conserved quantity
        !> (water-equivalent mass density) to pressure,
        !> \( s_i = (\partial \rho_{eq}/\partial p)_i \) [kg/(m3 Pa)].
        real(real64), allocatable, private :: drho_dp(:)
        !> Nodal snapshot of the primary variable itself (temperature [C] or
        !> pressure [Pa]) used only to evaluate the atol+rtol|u_i| tolerance
        !> weight in local_error_block. Refreshed together with dH_dT/drho_dp.
        real(real64), allocatable, private :: u_thermal(:)
        real(real64), allocatable, private :: u_hydraulic(:)
        !> ATS error-control tolerances (adaptive_stepping/error_control in
        !> Conditions.json: error_absolute_tolerance_temperature,
        !> error_absolute_tolerance_pressure, error_relative_tolerance). Read
        !> once via a getter on the time/ATS controller (see
        !> ftcms_solve.F90:solve_time_step_initial_setup) and reused as the
        !> primary-variable tolerance scale in local_error_block, so the
        !> nonlinear solve is held to the same accuracy standard the time
        !> integrator itself already accepts.
        real(real64), private :: atol_temperature_u = 1.0d-3
        real(real64), private :: atol_pressure_u = 1.0d1
        real(real64), private :: rtol_u = 1.0d-2
        !> Nodal conserved quantities at the previous nonlinear iterate
        real(real64), allocatable, private :: enthalpy_prev(:)
        real(real64), allocatable, private :: density_prev(:)
        logical, private :: has_prev_conserved = .false.
        !> Previous conserved-quantity increment for coupled Aitken globalization
        real(real64), allocatable, private :: dH_prev(:)
        real(real64), allocatable, private :: drho_prev(:)
        logical, private :: has_prev_conserved_increment = .false.
        !> Reference block residual norms ||R^0|| (set on first evaluation)
        real(real64), private :: residual0_thermal = -1.0d0
        real(real64), private :: residual0_hydraulic = -1.0d0
        !> Previous combined relative residual norm for conserved-mode globalization.
        real(real64), private :: residual_norm_prev = -1.0d0
        !> Previous weighted-RMS norm and divergence counter for kappa monitoring
        real(real64), private :: dq_norm_prev = -1.0d0
        integer(int32), private :: diverge_count = 0
        !> Adaptive under-relaxation factor for the globalized modified-Picard step.
        !> Driven by the convergence rate kappa: damped when the iteration grows
        !> (kappa >= 1), relaxed back toward 1 when it contracts. Replaces ad-hoc
        !> per-variable step clamps with a principled, condition-agnostic globalization.
        real(real64), private :: relaxation_omega = 1.0d0
        !> Gate values of the last conserved check, kept so the caller can
        !> report and log the same numbers the acceptance decision used.
        !> All four error measures pass at <= 1; -1 means not evaluated.
        real(real64), private :: gate_local_thermal = -1.0d0
        real(real64), private :: gate_local_hydraulic = -1.0d0
        real(real64), private :: gate_balance_thermal = -1.0d0
        real(real64), private :: gate_balance_hydraulic = -1.0d0
        real(real64), private :: gate_dq_effective = -1.0d0
        logical, private :: gate_local_ok = .false.
        logical, private :: gate_balance_ok = .false.
        logical, private :: gate_dq_ok = .false.
        ! --- Global conserved-quantity drift budget (criterion 2) ---
        !> Realized drift (sum_i R_i)*dt of the last iterate evaluated by
        !> check_conserved, per block. Not yet folded into the cumulative
        !> total below: that happens only when the caller confirms the step
        !> was actually accepted (commit_conserved_drift), since a converged
        !> nonlinear iterate can still be rejected afterward (LTE, outer phase
        !> loop) without ever becoming part of the accepted-history budget.
        real(real64), private :: pending_drift_thermal = 0.0d0
        real(real64), private :: pending_drift_hydraulic = 0.0d0
        logical, private :: pending_drift_thermal_valid = .false.
        logical, private :: pending_drift_hydraulic_valid = .false.
        !> Cumulative realized drift D_block = sum over accepted steps of
        !> (sum_i R_i)*dt, signed [J for thermal, kg for hydraulic]. A V&V
        !> observable, not an acceptance gate (see check_conserved).
        real(real64), private :: cumulative_drift_thermal = 0.0d0
        real(real64), private :: cumulative_drift_hydraulic = 0.0d0
    contains
        ! ---- Lifecycle ----
        procedure, public, pass(self) :: initialize => initialize_convergence_control
        ! ---- Mutator ----
        procedure, public, pass(self) :: reset => reset_convergence_control
        procedure, public, pass(self) :: update_reference_values => update_reference_values_convergence_control
        procedure, public, pass(self) :: set_residual_scale => set_residual_scale_convergence_control
        procedure, public, pass(self) :: get_residual_floors => get_residual_floors_convergence_control
        ! ---- Algorithm / Operation ----
        procedure, public, pass(self) :: check_convergence => check_convergence_control
        ! ---- Inquiry ----
        procedure, public, pass(self) :: is_initialized => is_initialized_convergence_control
        procedure, public, pass(self) :: should_check_residual => should_check_residual_convergence_control
        procedure, public, pass(self) :: should_check_update => should_check_update_convergence_control
        ! ---- Getter ----
        procedure, public, pass(self) :: get_norm_type => get_norm_type_convergence_control
        procedure, public, pass(self) :: get_combination_logic => get_combination_logic_convergence_control
        procedure, public, pass(self) :: get_convergence_norm_type => get_convergence_norm_type_convergence_control
        procedure, public, pass(self) :: get_current_norm => get_current_norm_convergence_control
        procedure, public, pass(self) :: get_tolerances => get_tolerances_convergence_control
        ! ---- Conserved-quantity convergence (PDF 6.2.4) ----
        procedure, public, pass(self) :: is_conserved => is_conserved_convergence_control
        procedure, public, pass(self) :: prime_conserved_residual => prime_conserved_residual_convergence_control
        procedure, public, pass(self) :: check_conserved => check_conserved_convergence_control
        procedure, public, pass(self) :: compute_error_norm => compute_error_norm_convergence_control
        procedure, public, pass(self) :: get_conserved_relaxation => get_conserved_relaxation_convergence_control
        procedure, public, pass(self) :: get_conserved_dq_norm => get_conserved_dq_norm_convergence_control
        procedure, public, pass(self) :: get_conserved_gates => get_conserved_gates_convergence_control
        procedure, public, pass(self) :: commit_conserved_drift => commit_conserved_drift_convergence_control
        procedure, public, pass(self) :: local_error_block => local_error_block_convergence_control
        ! ---- Meta / Utility ----
    end type type_convergence_control

    interface
        module subroutine initialize_convergence_control(self, config, max_iterations, reference_values)
            implicit none
            class(type_convergence_control), intent(inout) :: self
            type(type_config_iteration_nonlinear), intent(in) :: config
            integer(int32), intent(in) :: max_iterations
            real(real64), intent(in), optional :: reference_values(:)

        end subroutine initialize_convergence_control

        module subroutine reset_convergence_control(self)
            implicit none
            class(type_convergence_control), intent(inout) :: self

        end subroutine reset_convergence_control

        module function check_convergence_control(self, physics_type, nonlinear_iter, residual_vector, update_vector) result(is_ok)
            implicit none
            class(type_convergence_control), intent(inout) :: self
            type(type_constant_id), intent(in) :: physics_type
            integer(int32), intent(in) :: nonlinear_iter
            real(real64), intent(in), optional :: residual_vector(:)
            real(real64), intent(in), optional :: update_vector(:)
            logical :: is_ok

        end function check_convergence_control

        !> Gate values and verdicts of the last conserved check.
        !>
        !> Mathematical definition:
        !> - \(e_{\mathrm{loc}}, e_{\mathrm{bal}}, \Delta Q_{\mathrm{eff}}\), each
        !>   passing at \(\le 1\)
        !>
        !> Assumptions:
        !> - check_conserved has run at least once; otherwise every value is -1
        !>   and every verdict is .false.
        !>
        !> Numerical guarantee:
        !> - Returns exactly the quantities the acceptance decision used
        !>
        !> Computational complexity:
        !> - Time: \(O(1)\)
        !> - Memory: \(O(1)\)
        !>
        !> Failure behavior:
        !> - Returns without error
        module pure subroutine get_conserved_gates_convergence_control(self, local_thermal, local_hydraulic, &
                                                                      balance_thermal, balance_hydraulic, &
                                                                      dq_effective, local_ok, balance_ok, dq_ok)
            implicit none
            class(type_convergence_control), intent(in) :: self
            !> Criterion-1 local error of each block
            real(real64), intent(inout) :: local_thermal, local_hydraulic
            !> Criterion-2 global drift ratio of each block
            real(real64), intent(inout) :: balance_thermal, balance_hydraulic
            !> Conserved-quantity change measure
            real(real64), intent(inout) :: dq_effective
            !> Verdict of each gate
            logical, intent(inout) :: local_ok, balance_ok, dq_ok

        end subroutine get_conserved_gates_convergence_control

        module pure function is_initialized_convergence_control(self) result(is_initialized)
            implicit none
            class(type_convergence_control), intent(in) :: self
            logical :: is_initialized

        end function is_initialized_convergence_control

        module pure function should_check_residual_convergence_control(self) result(should_check)
            implicit none
            class(type_convergence_control), intent(in) :: self
            logical :: should_check

        end function should_check_residual_convergence_control

        module pure function should_check_update_convergence_control(self) result(should_check)
            implicit none
            class(type_convergence_control), intent(in) :: self
            logical :: should_check

        end function should_check_update_convergence_control

        module subroutine get_norm_type_convergence_control(self, norm_type)
            implicit none
            class(type_convergence_control), intent(in), target :: self
            type(type_constant_id), intent(inout), pointer :: norm_type

        end subroutine get_norm_type_convergence_control

        module subroutine get_combination_logic_convergence_control(self, combination_logic)
            implicit none
            class(type_convergence_control), intent(in), target :: self
            type(type_constant_id), intent(inout), pointer :: combination_logic

        end subroutine get_combination_logic_convergence_control

        module subroutine get_convergence_norm_type_convergence_control(self, convergence_norm_type)
            implicit none
            class(type_convergence_control), intent(in), target :: self
            type(type_constant_id), intent(inout), pointer :: convergence_norm_type

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

        end subroutine get_current_norm_convergence_control

        module subroutine get_tolerances_convergence_control(self, physics_type, absolute_tolerance, relative_tolerance)
            implicit none
            class(type_convergence_control), intent(in) :: self
            type(type_constant_id), intent(in) :: physics_type
            real(real64), intent(inout), optional :: absolute_tolerance
            real(real64), intent(inout), optional :: relative_tolerance

        end subroutine get_tolerances_convergence_control

        module subroutine update_reference_values_convergence_control(self, reference_values)
            implicit none
            class(type_convergence_control), intent(inout) :: self
            real(real64), intent(in) :: reference_values(:)

        end subroutine update_reference_values_convergence_control

        !> Supply the discretization scales used by the residual floor.
        !> Absolute residual floors of the two conserved blocks, in the units of
        !> the assembled residuals: the largest imbalance that can be sustained
        !> over the step without moving the conserved quantity past its absolute
        !> tolerance. Zero where the discretization scales are not yet set.
        module subroutine get_residual_floors_convergence_control(self, num_nodes, floor_thermal, floor_hydraulic)
            implicit none
            class(type_convergence_control), intent(in) :: self
            !> Number of nodes in the block
            integer(int32), intent(in) :: num_nodes
            real(real64), intent(inout) :: floor_thermal
            real(real64), intent(inout) :: floor_hydraulic
        end subroutine get_residual_floors_convergence_control

        !> Supply the discretization scales the residual gates are built from.
        !> nodal_volume, when present and non-empty, is copied into the stored
        !> per-node control-volume array used by the exact local gate; when
        !> absent the previously stored array (if any) is left unchanged, so
        !> callers that only refresh dt every attempt (the mesh is static) do
        !> not need to resupply it. dH_dT/drho_dp/u_thermal/u_hydraulic follow
        !> the same "absent keeps previous value" convention, but are state-
        !> dependent and so are resupplied every time the nodal conserved
        !> quantities are recomputed (see local_error_block). The ATS
        !> tolerance scalars are static run configuration and are supplied
        !> once, at the same call that first builds nodal_volume.
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

        end subroutine set_residual_scale_convergence_control

        !> Returns true when the conserved-quantity convergence mode is selected.
        module pure function is_conserved_convergence_control(self) result(is_conserved)
            implicit none
            class(type_convergence_control), intent(in) :: self
            logical :: is_conserved
        end function is_conserved_convergence_control

        !> Stores residual norms assembled at the initial nonlinear iterate.
        module subroutine prime_conserved_residual_convergence_control(self, residual_thermal, residual_hydraulic, &
                                                                       check_thermal, check_hydraulic)
            implicit none
            class(type_convergence_control), intent(inout) :: self
            real(real64), intent(in), optional :: residual_thermal(:)
            real(real64), intent(in), optional :: residual_hydraulic(:)
            logical, intent(in) :: check_thermal
            logical, intent(in) :: check_hydraulic
        end subroutine prime_conserved_residual_convergence_control

        !> Conserved-quantity convergence check (PDF 6.2.4).
        !>
        !> Mathematical definition:
        !> - \( \lVert \delta Q \rVert_W = \sqrt{ \frac{1}{2N}\sum_j
        !>   [(\delta H_j/w^H_j)^2 + (\delta\rho_j/w^\rho_j)^2] } \le 1 \)
        !>   with \( w^H_j = atol_H + rtol|H_j| \), \( w^\rho_j = atol_\rho + rtol|\rho_j| \).
        !> - per-block residual ratio \( \lVert R_b \rVert / \lVert R_b^0 \rVert < \varepsilon_R \).
        !> Sets is_ok when both hold; sets is_diverged on NaN or kappa>=1 repeated.
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
        end subroutine check_conserved_convergence_control

        !> Weighted-RMS norm of the conserved-quantity difference (Q_b - Q_a),
        !> used for the Richardson local-error estimate (PDF 6.3.2-6.3.3, p=1).
        module subroutine compute_error_norm_convergence_control(self, enthalpy_a, density_a, &
                                                                 enthalpy_b, density_b, eps)
            implicit none
            class(type_convergence_control), intent(in) :: self
            real(real64), intent(in) :: enthalpy_a(:)
            real(real64), intent(in) :: density_a(:)
            real(real64), intent(in) :: enthalpy_b(:)
            real(real64), intent(in) :: density_b(:)
            real(real64), intent(inout) :: eps
        end subroutine compute_error_norm_convergence_control

        !> Current adaptive under-relaxation factor for the globalized modified
        !> Picard step (1 = full step). Updated by check_conserved from kappa.
        module pure function get_conserved_relaxation_convergence_control(self) result(omega)
            implicit none
            class(type_convergence_control), intent(in) :: self
            real(real64) :: omega
        end function get_conserved_relaxation_convergence_control

        !> Last weighted-RMS conserved-quantity change ||dQ||_W (diagnostics).
        module pure function get_conserved_dq_norm_convergence_control(self) result(dq_norm)
            implicit none
            class(type_convergence_control), intent(in) :: self
            real(real64) :: dq_norm
        end function get_conserved_dq_norm_convergence_control

        !> Fold the pending realized drift of the last check_conserved
        !> evaluation into the cumulative per-block budget, once the caller
        !> has confirmed the step is genuinely accepted (i.e. not later
        !> rejected by LTE or the outer phase loop). See check_conserved for
        !> the per-step drift bound this cumulative total is tracked against.
        module subroutine commit_conserved_drift_convergence_control(self)
            implicit none
            class(type_convergence_control), intent(inout) :: self
        end subroutine commit_conserved_drift_convergence_control

        !> Local nonlinear-error measure for one conserved block, expressed in
        !> the units of that block's own primary variable (K for thermal, Pa
        !> for hydraulic) rather than the conserved quantity's own units.
        !>
        !> Mathematical definition:
        !> \[ E_b = \sqrt{\frac{1}{N}\sum_{i=1}^N \left[\frac{R_i\,\Delta t /
        !>    V_i}{s_i\,\tau_i}\right]^2}, \qquad
        !>    \tau_i = f_{NL}\,(atol_u + rtol_u\,|u_i|) \]
        !> with \(R_i\) the assembled block residual, \(V_i\) the nodal
        !> control volume, \(s_i\) the nodal storage sensitivity (dH/dT or
        !> d rho_eq/dp, floored away from zero) and \(u_i\) the primary
        !> variable snapshot, all supplied by set_residual_scale.
        !> \(R_i\Delta t/V_i\) is the conserved-quantity error the residual
        !> would cause if sustained over the step [J/m3 or kg/m3]; dividing
        !> by \(s_i\) converts it into the primary variable's own error
        !> [K or Pa], measured against the same atol/rtol the time integrator
        !> already accepts (scaled by \(f_{NL}\) = NONLINEAR_TO_LTE_FACTOR).
        !> Assumptions: nodal_volume and the block's sensitivity/primary-
        !> variable snapshot were supplied and match residual in size;
        !> otherwise returns the sentinel -1 so the caller can fall back to
        !> the mean-volume floor gate (get_residual_floors).
        !> Numerical guarantees: none beyond IEEE arithmetic.
        !> Computational complexity: O(N) time, O(1) additional memory.
        !> Failure behavior: returns -1 on any missing or mismatched input;
        !> never aborts.
        module function local_error_block_convergence_control(self, physics_type, residual) result(e_local)
            implicit none
            class(type_convergence_control), intent(in) :: self
            !> Block identifier (PHYSICS_TYPES%THERMAL or PHYSICS_TYPES%HYDRAULIC)
            type(type_constant_id), intent(in) :: physics_type
            !> Assembled block residual at the current (trial) iterate
            real(real64), intent(in) :: residual(:)
            real(real64) :: e_local
        end function local_error_block_convergence_control

    end interface

end module control_iteration_convergence

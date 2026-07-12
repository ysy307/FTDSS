module app_ftcms
    use, intrinsic :: iso_fortran_env
    use :: omp_lib
    use :: mpi_f08

    use :: stdlib_optval, only:optval
    use :: stdlib_logger
    use :: module_core
    use :: module_input, only:type_input, input_translator
    use :: module_output, only:type_output_manager

    use :: module_control, only:type_control
    use :: module_domain
    ! use :: module_boundary, only:
    use :: module_initial, only:type_ic_manager
    use :: module_system, only:type_jacobian_matrix, type_residual_vector
    use :: module_constitutive, only:g => gravity_acceleration
    use :: module_linalg

    use :: module_governing
    use :: governing_atmosphere, only: type_da_config, type_assimilation_controller
    use :: module_solver
    implicit none

    !> Physical-validity walls shared by the bounded solution update
    !> (reflect_variables) and the acceptance guard of the nonlinear loop.
    !> An iterate pinned at a wall is outside the model's validity and must
    !> never be accepted as a converged step.
    real(real64), parameter :: WALL_TEMP_MIN_C = -80.0d0
    real(real64), parameter :: WALL_TEMP_MAX_C = 80.0d0
    real(real64), parameter :: WALL_PRESS_MIN_PA = -1.0d7
    real(real64), parameter :: WALL_PRESS_MAX_PA = 1.0d7

    type :: type_ftcms
        type(type_domain) :: domain

        type(type_variable) :: porosity
        type(type_variable) :: temperature
        type(type_variable) :: pressure

        type(type_variable) :: Qw
        type(type_variable) :: Qi
        type(type_variable) :: Qa
        type(type_variable) :: Qv

        type(type_jacobian_matrix) :: K
        type(type_residual_vector) :: F
        type(type_residual_vector) :: du

        type(type_thermal) :: thermal
        type(type_hydraulic) :: hydraulic

        type(type_bc_manager) :: bc(PHYSICS_TYPES%NUM_ID)

        class(abst_solver), allocatable :: solver
        class(abst_solver), allocatable :: solver_thermal

        integer(int32) :: current_physics_id = 0
        integer(int32) :: thermal_start_dof = 0
        integer(int32) :: hydraulic_start_dof = 0
        logical :: hydraulic_has_dirichlet_bc = .false.
        logical :: thermal_has_dirichlet_bc = .false.

        ! Reference mean pressure captured from the initial condition.
        ! Used to pin the null-mode of all-Neumann hydraulic systems without
        ! distorting the absolute pressure level (WRF relies on absolute P).
        logical :: hydraulic_ref_mean_set = .false.
        real(real64) :: hydraulic_ref_mean = 0.0d0

        ! DOF column scaling factors for variable non-dimensionalization
        real(real64), allocatable :: col_scale(:)
        real(real64), allocatable :: col_scale_inv(:)

        ! Local-truncation-error estimate state for error-controlled ATS. Stores the
        ! previous-step time derivative (ydot) per physics and the previous dt, used
        ! by compute_lte_error to form the divided-difference (curvature) estimate.
        real(real64), allocatable :: lte_ydot_prev_thermal(:)
        real(real64), allocatable :: lte_ydot_prev_hydraulic(:)
        real(real64) :: lte_prev_dt = 0.0d0
        logical :: lte_has_prev = .false.

        ! Anderson(1) acceleration state of the conserved coupled Picard loop:
        ! the previous iterate and previous fixed-point increment of (T, p),
        ! used by reflect_variables to form the depth-1 Anderson mixing.
        ! Cleared at the start of every nonlinear loop.
        real(real64), allocatable :: aa_T_prev(:)
        real(real64), allocatable :: aa_P_prev(:)
        real(real64), allocatable :: aa_duT_prev(:)
        real(real64), allocatable :: aa_duP_prev(:)
        logical :: aa_has_prev = .false.
        ! Weighted norm of the previous fixed-point increment ||g_{k-1}||_W;
        ! negative when unset. Safeguard: Anderson mixing is applied only while
        ! this sequence is non-increasing (contracting fixed-point iteration).
        real(real64) :: aa_gnorm_prev = -1.0d0

        ! Unit of Output/solver_history.log: one record per time-step attempt
        ! (step, time, dt, nonlinear iterations, accepted flag, omega, ||dQ||_W,
        ! LTE estimate). -1 when the log is not open (non-root ranks).
        integer(int32) :: solver_history_unit = -1

        ! Global mass-bias acceptance gate (conserved-mode nonlinear loop). Adds
        ! |sum_i R_H,i| * dt <= mass_bias_tolerance * mass_ref as an AND condition
        ! on top of the standard conserved-quantity convergence check, to prevent
        ! the FEM partition-of-unity bias (Sigma R_H = net spurious mass rate,
        ! valid when all hydraulic boundaries are zero-flux) from accumulating
        ! over many accepted steps. See ftcms_solve.F90 for the acceptance logic
        ! and ftcms_base.F90:compute_mass_reference_ftcms for M_ref. Off by
        ! default; enabled per project via solver_settings.nonlinear_solver.
        ! convergence.conserved.enable_mass_bias_gate.
        real(real64) :: mass_ref = 0.0d0
        real(real64) :: mass_bias_tolerance = 1.0d-6
        logical :: enable_mass_bias_gate = .false.
        ! Rate-form (Harlan/Hansson) freezing closure switch; mirrors the
        ! constitutive module switch (models_phase_change_fusion) and gates the
        ! nodal/GP ice-history plumbing below. Prototype: see
        ! design_rate_form_closure.md.
        logical :: enable_rate_form_freezing = .false.
        ! Diagnostic only: last evaluated mass_bias/mass_ref ratio, logged to
        ! solver_history.log regardless of enable_mass_bias_gate.
        real(real64) :: mass_bias_ratio_last = 0.0d0

        ! A1 prototype closure: prognostic ice content + Clapeyron pressure
        ! constraint (see design memo referenced in basic_interface.F90). When
        ! enabled, hydraulic-frozen nodes have their pressure DOF pinned each
        ! nonlinear iteration to P_eq(T) = -psi_cryo(T) (ftcms_boundary.F90:
        ! apply_clapeyron_pressure_constraint), and Qi is advanced from the
        ! excluded mass residual accumulated INSIDE the nonlinear loop
        ! (accumulate_prognostic_ice_flux; the fringe deposition dynamics is
        ! stiff - K impedance shuts the inflow down on a sub-dt time scale -
        ! so the ice/flux coupling must be solved implicitly as part of the
        ! Picard fixed point, not applied explicitly after the step), then
        ! finalized once per accepted step (apply_prognostic_ice_update)
        ! instead of being re-derived every iteration from the equilibrium
        ! Theta(psi_cap) closure. Off by default; bit-identical to the
        ! pre-existing behavior when .false.
        logical :: enable_clapeyron_pressure_constraint = .false.
        !> Per-node active-set flag (pressure-constrained "frozen" state),
        !> refreshed every nonlinear iteration. Size num_nodes.
        logical, allocatable :: clapeyron_frozen_mask(:)
        !> Raw hydraulic residual F_H saved at each constrained node immediately
        !> before its row is overwritten by the constraint (i.e. before it would
        !> otherwise be zeroed like a Dirichlet BC row); consumed by
        !> apply_prognostic_ice_update to advance Qi. Size num_nodes.
        real(real64), allocatable :: clapeyron_R_H_raw(:)
        !> Lumped nodal control volume V_i = sum over incident elements of
        !> (element measure / n_local_nodes), computed once at initialization.
        real(real64), allocatable :: clapeyron_node_volume(:)
        !> Within-step accumulator of the residual-driven (flux-transport) ice
        !> increment at constrained nodes [volumetric ice fraction]. Advanced by
        !> accumulate_prognostic_ice_flux every nonlinear iteration with the
        !> conserved relaxation factor as damping; reset to zero at the start of
        !> every time-step attempt. At the Picard fixed point the constrained
        !> nodes' mass residual vanishes and the accumulator stops moving, i.e.
        !> the frozen-node continuity equation is solved with Qi as the local
        !> unknown. Size num_nodes.
        real(real64), allocatable :: clapeyron_dQi_flux(:)

        type(type_control) :: control
        type(type_output_manager) :: output

        type(type_assimilation_controller) :: assimilation
        logical :: assimilation_enabled = .false.

    contains
        ! ---- Lifecycle ----
        ! initialize, destroy, reset, etc.
        procedure, public, pass(self) :: initialize => initialize_type_ftcms
        procedure, public, pass(self) :: destroy => destroy_type_ftcms

        ! ---- Mutator ----
        ! set_XXX, increment_XXX, update_XXX, etc.

        ! ---- Algorithm / Operation ----
        ! compute_XXX, check_XXX, solve_XXX, etc.

        ! ---- Inquiry ----
        ! is_XXX, has_XXX, should_XXX, etc.

        ! ---- Getter ----
        ! get_XXX, etc.

        ! ---- Meta / Utility ----
        ! display, to_string, etc.

        ! ---- Operator ----

        procedure, public, pass(self) :: shift => shift_ftcms

        procedure, public, pass(self) :: calc_gradient => calc_gradient_ftcms
        procedure, public, pass(self) :: calc_gradient_temperature => calc_gradient_temperature_ftcms
        procedure, public, pass(self) :: calc_gradient_pressure => calc_gradient_pressure_ftcms

        procedure, public, pass(self) :: calc_water_flux => calc_water_flux_ftcms
        procedure, public, pass(self) :: calc_vapor_flux => calc_vapor_flux_ftcms

        ! --- Boundary Condition Procedures ---
        procedure, public, pass(self) :: apply_bc => apply_bc_ftcms
        procedure, public, pass(self) :: prescribe_dirichlet => prescribe_dirichlet_ftcms
        procedure, private, pass(self) :: freeze_physics_dofs => freeze_physics_dofs_ftcms
        procedure, private, pass(self) :: zero_frozen_increment => zero_frozen_increment_ftcms
        procedure, private, pass(self) :: apply_clapeyron_pressure_constraint => apply_clapeyron_pressure_constraint_ftcms
        procedure, private, pass(self) :: prescribe_essential_bc_generic
        procedure, private, pass(self) :: apply_natural_bc_generic
        procedure, private, pass(self) :: apply_essential_bc_generic

        procedure, public, pass(self) :: solve => solve_ftcms

        procedure, public, pass(self) :: set_state => set_state_ftcms
        procedure, private, pass(self) :: set_states_from_connectivity => set_states_from_connectivity_ftcms
        procedure, public, pass(self) :: update_physical_properties => update_physical_properties_ftcms
        procedure, private, pass(self) :: update_physical_properties_bulk => update_physical_properties_bulk_ftcms

        procedure, public, pass(self) :: reflect_variables => reflect_variables_ftcms
        procedure, private, pass(self) :: apply_phase_change_temperature_correction => &
            apply_phase_change_temperature_correction_ftcms
        procedure, private, pass(self) :: update_nodal_phases => update_nodal_phases_ftcms
        procedure, private, pass(self) :: apply_prognostic_ice_update => apply_prognostic_ice_update_ftcms
        procedure, private, pass(self) :: accumulate_prognostic_ice_flux => accumulate_prognostic_ice_flux_ftcms
        procedure, private, pass(self) :: compute_nodal_conserved => compute_nodal_conserved_ftcms
        procedure, private, pass(self) :: compute_mass_reference => compute_mass_reference_ftcms
        procedure, public, pass(self) :: compute_lte_error => compute_lte_error_ftcms
        procedure, public, pass(self) :: nonlinear_residual_norm => nonlinear_residual_norm_ftcms
        procedure, public, pass(self) :: update_variables => update_variables_ftcms
        procedure, public, pass(self) :: assemble_local => assemble_local_ftcms
        procedure, public, pass(self) :: assemble => assemble_ftcms
        procedure, private, pass(self) :: assemble_initialize => assemble_initialize_ftcms
        procedure, private, pass(self) :: assemble_destroy => assemble_destroy_ftcms

        procedure, private, pass(self) :: get_variable_increment => get_variable_increment_ftcms
        procedure, private, pass(self) :: get_variable_residual => get_variable_residual_ftcms

        procedure, public, pass(self) :: reset => reset_ftcms

        !> Solve a single time step including the nonlinear iteration loop
        procedure, public, pass(self) :: solve_time_step => solve_time_step_ftcms
        procedure, private, pass(self) :: solve_time_step_staggered => solve_time_step_staggered_ftcms
        procedure, private, pass(self) :: solve_time_step_initial_setup => solve_time_step_initial_setup_ftcms
        procedure, private, pass(self) :: solve_time_step_setup => solve_time_step_setup_ftcms
        procedure, private, pass(self) :: solve_time_step_check_convergence => solve_time_step_check_convergence_ftcms
        procedure, private, pass(self) :: solve_time_step_check_convergence_conserved => &
            solve_time_step_check_convergence_conserved_ftcms

        procedure, public, pass(self) :: output_fields => output_fields_ftcms
        procedure, public, pass(self) :: output_history => output_history_ftcms

        procedure, public, pass(self) :: is_active_thermal => is_active_thermal_ftcms
        procedure, public, pass(self) :: is_active_hydraulic => is_active_hydraulic_ftcms

        procedure, public, pass(self) :: run => run_ftcms
        procedure, public, pass(self) :: run_assimilation => run_assimilation_ftcms

    end type type_ftcms

    interface
        module subroutine initialize_type_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine initialize_type_ftcms

        module subroutine prescribe_essential_bc_generic(self, physics_type, current_time, variable)
            implicit none
            class(type_ftcms), intent(inout), target :: self
            type(type_constant_id), intent(in) :: physics_type
            real(real64), intent(in) :: current_time
            type(type_variable), intent(inout) :: variable
        end subroutine prescribe_essential_bc_generic

        module subroutine apply_natural_bc_generic(self, physics_type, current_time, variable, dof_offset)
            implicit none
            class(type_ftcms), intent(inout), target :: self
            type(type_constant_id), intent(in) :: physics_type
            real(real64), intent(in) :: current_time
            type(type_variable), intent(in) :: variable
            integer(int32), intent(in) :: dof_offset

        end subroutine apply_natural_bc_generic

        module subroutine apply_essential_bc_generic(self, physics_type, current_time, variable, dof_offset)
            implicit none
            class(type_ftcms), intent(inout), target :: self
            type(type_constant_id), intent(in) :: physics_type
            real(real64), intent(in) :: current_time
            type(type_variable), intent(in) :: variable
            integer(int32), intent(in) :: dof_offset

        end subroutine apply_essential_bc_generic

        module subroutine apply_bc_ftcms(self, prescribed)
            implicit none
            class(type_ftcms), intent(inout) :: self
            logical, intent(in), optional :: prescribed

        end subroutine apply_bc_ftcms

        module subroutine prescribe_dirichlet_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine prescribe_dirichlet_ftcms

        module subroutine freeze_physics_dofs_ftcms(self, physics_type)
            implicit none
            class(type_ftcms), intent(inout) :: self
            type(type_constant_id), intent(in) :: physics_type
        end subroutine freeze_physics_dofs_ftcms

        module subroutine zero_frozen_increment_ftcms(self, frozen_physics)
            import :: type_ftcms, type_constant_id
            implicit none
            class(type_ftcms), intent(inout) :: self
            type(type_constant_id), intent(in) :: frozen_physics
        end subroutine zero_frozen_increment_ftcms

        !> A1 prototype closure (see enable_clapeyron_pressure_constraint):
        !> classify every hydraulic node as pressure-constrained ("frozen") when
        !> psi_cryo(T) > psi_cap(P) or Qi > 0, and unconstrained otherwise. For
        !> each constrained node, save the raw (pre-constraint) hydraulic residual
        !> into clapeyron_R_H_raw, then prescribe P = P_eq(T) = -psi_cryo(T) by
        !> writing the pressure DOF directly and zeroing the matrix row / RHS
        !> exactly like a Dirichlet BC row (see apply_essential_bc_generic).
        !> Called every nonlinear iteration from apply_bc (T moves each
        !> iteration, so P_eq(T) must be re-prescribed). Monolithic coupling
        !> only (no-op when staggered), matching freeze_physics_dofs_ftcms.
        module subroutine apply_clapeyron_pressure_constraint_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self
        end subroutine apply_clapeyron_pressure_constraint_ftcms

        module subroutine solve_ftcms(self)
            import :: type_ftcms
            implicit none
            class(type_ftcms), intent(inout) :: self
        end subroutine solve_ftcms

        module subroutine set_state_ftcms(self, node_id, element_id, state, calc_physics)
            implicit none
            class(type_ftcms), intent(inout) :: self
            integer(int32), intent(in) :: node_id
            integer(int32), intent(in) :: element_id
            type(type_state), intent(inout) :: state
            logical, intent(in), optional :: calc_physics
        end subroutine set_state_ftcms

        module subroutine set_states_from_connectivity_ftcms(self, connectivity, element_id, states, calc_physics)
            implicit none
            class(type_ftcms), intent(inout) :: self
            integer(int32), intent(in) :: connectivity(:)
            integer(int32), intent(in) :: element_id
            type(type_state), intent(inout) :: states(:)
            logical, intent(in), optional :: calc_physics
        end subroutine set_states_from_connectivity_ftcms

        module subroutine update_physical_properties_ftcms(self, material_id, state)
            implicit none
            class(type_ftcms), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(inout) :: state
        end subroutine update_physical_properties_ftcms

        module subroutine update_physical_properties_bulk_ftcms(self, material_id, states)
            implicit none
            class(type_ftcms), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(inout) :: states(:)
        end subroutine update_physical_properties_bulk_ftcms

        module subroutine shift_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine shift_ftcms

        module subroutine update_variables_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine update_variables_ftcms

        module subroutine reflect_variables_ftcms(self, step_scale)
            implicit none
            class(type_ftcms), intent(inout) :: self
            real(real64), intent(in), optional :: step_scale

        end subroutine reflect_variables_ftcms

        module subroutine update_nodal_phases_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self
        end subroutine update_nodal_phases_ftcms

        !> A1 prototype closure (see enable_clapeyron_pressure_constraint):
        !> advance the prognostic ice content Qi at every pressure-constrained
        !> node, once per ACCEPTED step (called from run_ftcms right before
        !> shift(), so the updated Qi becomes part of the BDF history propagated
        !> into the next step). The new prognostic value is
        !>   Qi_prog^{n+1} = [Qi_prog^n
        !>                    + (rho_w/rho_i)*max(0, theta_w(T_n) - theta_w(T_conv))]
        !>                   + Delta Qi
        !> where the bracket is the accepted-iterate state ice (step-start
        !> prognostic value plus the confirmed in-step local phase change - the
        !> same closure the nonlinear loop assembled with, see
        !> override_prognostic_ice in ftcms_base.F90) and Delta Qi is the pure
        !> flux-transport increment from the excluded mass residual saved by
        !> apply_clapeyron_pressure_constraint. See ftcms_base.F90 for the full
        !> sign derivation of Delta Qi from R_H,i. Monolithic coupling only
        !> (no-op when staggered).
        module subroutine apply_prognostic_ice_update_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self
        end subroutine apply_prognostic_ice_update_ftcms

        !> Accumulate the residual-driven ice increment at constrained nodes
        !> inside the nonlinear loop (one call per Picard iteration):
        !> \( dQi_i \mathrel{+}= \omega\,(\rho_w/\rho_i)\, R_{H,i}\, \Delta t / V_i \)
        !> with \(\omega\) the conserved relaxation factor. The fringe ice
        !> deposition is stiff (impedance shuts the inflow down on a sub-dt
        !> time scale), so the coupling is solved implicitly as part of the
        !> Picard fixed point: the accumulator stops moving exactly when the
        !> constrained nodes' mass residual vanishes. Monolithic coupling only
        !> (no-op when staggered or flag off).
        module subroutine accumulate_prognostic_ice_flux_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self
        end subroutine accumulate_prognostic_ice_flux_ftcms

        !> Evaluate the per-node conserved quantities (volumetric enthalpy density
        !> and pore-water effective density) at the current iterate, for the
        !> conserved-quantity convergence norm and the Richardson error estimate.
        module subroutine compute_nodal_conserved_ftcms(self, enthalpy, density)
            implicit none
            class(type_ftcms), intent(inout) :: self
            real(real64), allocatable, intent(inout) :: enthalpy(:)
            real(real64), allocatable, intent(inout) :: density(:)
        end subroutine compute_nodal_conserved_ftcms

        !> One-time integral of the mixed water-equivalent content Theta over the
        !> whole domain, M_ref = \( \int_\Omega \Theta \, d\Omega \), used as the
        !> reference scale of the global mass-bias acceptance gate. See ftcms_base.F90
        !> for the mathematical definition and the lumped-quadrature approximation used.
        module function compute_mass_reference_ftcms(self) result(m_ref)
            implicit none
            class(type_ftcms), intent(inout) :: self
            real(real64) :: m_ref
        end function compute_mass_reference_ftcms

        !> Relative local-truncation-error estimate of the just-converged step, for
        !> the error-controlled (PI) adaptive time stepping.
        !>
        !> Uses the divided-difference (curvature) estimate of the implicit-Euler
        !> LTE: \( \text{LTE} \approx \lVert \dot y_n - \dot y_{n-1}\rVert\,
        !> \Delta t_n^2/(\Delta t_n+\Delta t_{n-1}) \), normalized by \(\lVert y_n\rVert\)
        !> to be dimensionless and self-scaling (no per-case absolute tolerances).
        !> The temperature and pressure estimates are combined by a maximum.
        !> Returns -1 on the first step (no previous derivative yet) so the caller
        !> skips error control. Advances the stored previous derivative and dt.
        module function compute_lte_error_ftcms(self) result(error_rel)
            implicit none
            class(type_ftcms), intent(inout) :: self
            real(real64) :: error_rel
        end function compute_lte_error_ftcms

        !> Euclidean norm of the assembled nonlinear conservation residual (energy
        !> and water blocks combined), used as the merit function for the
        !> backtracking line search that globalizes the modified-Picard step.
        module function nonlinear_residual_norm_ftcms(self) result(rnorm)
            implicit none
            class(type_ftcms), intent(inout) :: self
            real(real64) :: rnorm
        end function nonlinear_residual_norm_ftcms

        module subroutine calc_gradient_ftcms(self, values_vec, grad)
            implicit none
            class(type_ftcms), intent(inout) :: self
            real(real64), intent(in) :: values_vec(:)
            type(type_coordinate_array_dp), intent(inout) :: grad

        end subroutine calc_gradient_ftcms

        module subroutine calc_gradient_temperature_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine calc_gradient_temperature_ftcms

        module subroutine calc_gradient_pressure_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine calc_gradient_pressure_ftcms

        module subroutine calc_water_flux_ftcms(self, material_id, state, grad_T, grad_P, water_flux)
            implicit none
            class(type_ftcms), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(in) :: state
            type(type_coordinate_dp), intent(in) :: grad_T, grad_P
            type(type_coordinate_dp), intent(inout) :: water_flux

        end subroutine calc_water_flux_ftcms

        module subroutine calc_vapor_flux_ftcms(self, material_id, state, grad_T, grad_P, water_flux)
            implicit none
            class(type_ftcms), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(in) :: state
            type(type_coordinate_dp), intent(in) :: grad_T, grad_P
            type(type_coordinate_dp), intent(inout) :: water_flux

        end subroutine calc_vapor_flux_ftcms

        module subroutine assemble_local_ftcms(self, workspace, local_K_TT, local_K_TH, &
                                               local_K_HH, local_K_HT, local_F_T, local_F_H)
            implicit none
            class(type_ftcms), intent(inout) :: self
            type(type_assemble_workspace), intent(inout) :: workspace
            type(type_matrix_dense), intent(inout), optional :: local_K_TT, local_K_TH, local_K_HH, local_K_HT
            type(type_vector_dp), intent(inout), optional :: local_F_T, local_F_H

        end subroutine assemble_local_ftcms
        module subroutine assemble_initialize_ftcms(self, element_id, workspace, local_K_TT, local_K_TH, &
                                                    local_K_HH, local_K_HT, local_F_T, local_F_H, &
                                                    coordinates, raw_coordinates, connectivity)
            implicit none

            class(type_ftcms), intent(inout) :: self
            integer(int32), intent(in) :: element_id
            type(type_assemble_workspace), intent(inout) :: workspace
            type(type_matrix_dense), intent(inout), optional :: local_K_TT, local_K_TH, local_K_HH, local_K_HT
            type(type_vector_dp), intent(inout), optional :: local_F_T, local_F_H
            real(real64), allocatable, intent(inout) :: coordinates(:, :)
            real(real64), allocatable, intent(inout) :: raw_coordinates(:, :)
            integer(int32), pointer, contiguous, intent(inout), optional :: connectivity(:)
        end subroutine assemble_initialize_ftcms

        module subroutine assemble_destroy_ftcms(self, workspace, local_K_TT, local_K_TH, &
                                                 local_K_HH, local_K_HT, local_F_T, local_F_H)
            implicit none
            class(type_ftcms), intent(inout) :: self
            type(type_assemble_workspace), intent(inout) :: workspace
            type(type_matrix_dense), intent(inout), optional :: local_K_TT, local_K_TH, local_K_HH, local_K_HT
            type(type_vector_dp), intent(inout), optional :: local_F_T, local_F_H

        end subroutine assemble_destroy_ftcms

        module subroutine get_variable_increment_ftcms(self, variable_id, variable)
            implicit none
            class(type_ftcms), intent(inout) :: self
            type(type_constant_id), intent(in) :: variable_id
            real(real64), intent(inout), allocatable :: variable(:)

        end subroutine get_variable_increment_ftcms

        module subroutine get_variable_residual_ftcms(self, variable_id, variable)
            implicit none
            class(type_ftcms), intent(inout) :: self
            type(type_constant_id), intent(in) :: variable_id
            real(real64), intent(inout), allocatable :: variable(:)

        end subroutine get_variable_residual_ftcms

        module subroutine reset_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine reset_ftcms

        module subroutine assemble_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine assemble_ftcms

        module subroutine solve_time_step_initial_setup_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine solve_time_step_initial_setup_ftcms

        module subroutine solve_time_step_setup_ftcms(self, prescribe_bc)
            implicit none
            class(type_ftcms), intent(inout) :: self
            logical, intent(inout) :: prescribe_bc

        end subroutine solve_time_step_setup_ftcms

        module subroutine solve_time_step_check_convergence_ftcms(self, target_physics)
            implicit none
            class(type_ftcms), intent(inout), target :: self
            type(type_constant_id), intent(in), optional :: target_physics

        end subroutine solve_time_step_check_convergence_ftcms

        !> Conserved-quantity convergence check (PDF 6.2.4), evaluated on the updated
        !> state: computes the nodal enthalpy/effective-density and per-block residual
        !> norms and delegates the coupled decision to the control manager.
        module subroutine solve_time_step_check_convergence_conserved_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self
        end subroutine solve_time_step_check_convergence_conserved_ftcms

        module subroutine solve_time_step_ftcms(self, is_step_converged)
            implicit none
            class(type_ftcms), intent(inout) :: self
            logical, intent(inout) :: is_step_converged

        end subroutine solve_time_step_ftcms

        module subroutine solve_time_step_staggered_ftcms(self, is_step_converged)
            implicit none
            class(type_ftcms), intent(inout) :: self
            logical, intent(inout) :: is_step_converged
        end subroutine solve_time_step_staggered_ftcms

        module subroutine output_fields_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine output_fields_ftcms

        module subroutine output_history_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine output_history_ftcms

        module subroutine run_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine run_ftcms

        module subroutine run_assimilation_ftcms(self, current_time, current_doy)
            implicit none
            class(type_ftcms), intent(inout) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(in) :: current_doy
        end subroutine run_assimilation_ftcms

        module subroutine apply_phase_change_temperature_correction_ftcms(self, T_old, T_new)
            implicit none
            class(type_ftcms), intent(inout) :: self
            real(real64), intent(in) :: T_old(:)
            real(real64), intent(inout) :: T_new(:)
        end subroutine apply_phase_change_temperature_correction_ftcms

        module subroutine destroy_type_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self

        end subroutine destroy_type_ftcms

        module function is_active_thermal_ftcms(self) result(is_active)
            implicit none
            class(type_ftcms), intent(in) :: self
            logical :: is_active

        end function is_active_thermal_ftcms

        module function is_active_hydraulic_ftcms(self) result(is_active)
            implicit none
            class(type_ftcms), intent(in) :: self
            logical :: is_active

        end function is_active_hydraulic_ftcms

    end interface

end module app_ftcms

module app_ftcms
    use, intrinsic :: iso_fortran_env
    use :: omp_lib
    use :: core_parallel_mpi

    use :: stdlib_optval, only:optval
    use :: stdlib_logger
    use :: module_core
    use :: module_input, only:type_input, input_translator
    use :: module_output, only:type_output_manager

    use :: module_control, only:type_control
    use :: control_acceleration_aa1_coupled, only:compute_aa1_coefficient, &
        compute_coupled_aa1_coefficient, coupled_weighted_norm
    use :: module_domain
    ! use :: module_boundary, only:
    use :: module_initial, only:type_ic_manager
    use :: module_system, only:type_jacobian_matrix, type_residual_vector
    use :: module_constitutive, only:g => gravity_acceleration
    use :: models_phase_change_chemical_potential, only:calc_T_high_celsius
    use :: models_phase_change_manager, only:phase_return_relaxation
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

    !>
    !> Static per-node table of the distinct materials among a node's
    !> adjacent elements, built once after the domain is initialized (the
    !> mesh and its material assignment are static for the lifetime of a
    !> run). Consumed by update_nodal_phases to evaluate the constitutive
    !> update once per distinct material per node, instead of once per
    !> adjacent element.
    !>
    !> CSR-like layout keyed by node id: node i's entries occupy the range
    !> ptr(i) : ptr(i+1)-1 in material_id / repr_element / measure_sum.
    !> Nodes with no adjacent elements have an empty row (ptr(i) == ptr(i+1)).
    !>
    !> repr_element(k) is the highest-index adjacent element carrying
    !> material_id(k): node->element adjacency is visited in ascending
    !> element-id order, so repeatedly overwriting the representative for a
    !> recurring material naturally leaves the highest-index one recorded.
    !> When a node touches a single material (the common case), this row is
    !> exactly the node's highest-index adjacent element overall, matching
    !> the "last element wins" semantics of the original per-element loop.
    !>
    type :: type_node_material_table
        !> Number of nodes the table was built for (0 if not yet built).
        integer(int32) :: num_nodes = 0
        !> Row pointers (size num_nodes + 1).
        integer(int32), allocatable :: ptr(:)
        !> Distinct material id for each (node, material) entry.
        integer(int32), allocatable :: material_id(:)
        !> Representative (highest-index) adjacent element for each entry.
        integer(int32), allocatable :: repr_element(:)
        !> Summed measure (volume/area) of adjacent elements sharing this material.
        real(real64), allocatable :: measure_sum(:)
    contains
        ! ---- Lifecycle ----
        procedure, pass(self), public :: initialize => initialize_node_material_table
        procedure, pass(self), public :: destroy => destroy_node_material_table
    end type type_node_material_table

    !>
    !> Static per-element Gauss-point geometry cache for the L2 (lumped
    !> mass) nodal gradient projection, built once after the domain is
    !> initialized (the mesh is static for the lifetime of a run). Caches
    !> exactly the quantities the projection re-derived per call: the
    !> shape-function nodal weights psi_k(x_p) * w_p * det(J_p), the global
    !> shape-function derivatives dpsi_k/dx(x_p), and the lumped nodal
    !> volume, so calc_gradient no longer calls calc_shape_function.
    !>
    !> Flattened CSR-like layout: element e's (gauss, node) entries occupy
    !> entry_ptr(e) : entry_ptr(e+1)-1 in shape_weight / dpsi_dx, ordered
    !> Gauss-point-major then local-node (the projection's loop order).
    !>
    type :: type_gradient_geometry_cache
        !> Number of elements the cache was built for (0 if not yet built).
        integer(int32) :: num_elements = 0
        !> Number of global nodes (size of nodal_vol).
        integer(int32) :: num_nodes = 0
        !> Spatial dimension of the computation (rows of dpsi_dx).
        integer(int32) :: dim = 0
        !> Entry offsets per element (size num_elements + 1).
        integer(int32), allocatable :: entry_ptr(:)
        !> Number of Gauss points per element (size num_elements).
        integer(int32), allocatable :: num_gauss(:)
        !> Number of local nodes per element (size num_elements).
        integer(int32), allocatable :: num_nodes_elem(:)
        !> psi_k(x_p) * w_p * det(J_p) per (element, gauss, node) entry.
        real(real64), allocatable :: shape_weight(:)
        !> Global shape-function derivatives dpsi_k/dx_d(x_p), shape (dim, entries).
        real(real64), allocatable :: dpsi_dx(:, :)
        !> Lumped nodal volume sum over adjacent Gauss points of
        !> psi_k w_p det(J_p) (size num_nodes).
        real(real64), allocatable :: nodal_vol(:)
    contains
        ! ---- Lifecycle ----
        procedure, pass(self), public :: initialize => initialize_gradient_geometry_cache
        procedure, pass(self), public :: destroy => destroy_gradient_geometry_cache
    end type type_gradient_geometry_cache

    type :: type_ftcms
        type(type_domain) :: domain

        !> Static per-node distinct-material table, built once from the
        !> domain's node->element adjacency (see type_node_material_table).
        !> Consumed by update_nodal_phases.
        type(type_node_material_table) :: node_material_table

        !> Static Gauss-point geometry cache for the nodal gradient
        !> projection (see type_gradient_geometry_cache). Consumed by
        !> calc_gradient.
        type(type_gradient_geometry_cache) :: gradient_cache

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
        ! Number of outer phase-equilibrium iterations in the latest solve.
        integer(int32) :: last_phase_iterations = 1
        ! Iterations in the final and most expensive inner nonlinear solves.
        integer(int32) :: last_inner_iterations = 0
        integer(int32) :: last_max_inner_iterations = 0
        ! Sum of inner nonlinear iterations over all outer phase iterations.
        integer(int32) :: last_nonlinear_work = 0
        ! Termination status and the latest evaluated phase-equilibrium metrics.
        integer(int32) :: last_solve_status = 0
        logical :: last_phase_metrics_available = .false.
        logical :: last_phase_converged = .false.
        integer(int32) :: last_phase_active_nodes = -1
        real(real64) :: last_phase_increment_max = -1.0d0
        real(real64) :: last_phase_increment_norm = -1.0d0
        real(real64) :: last_phase_equilibrium_error = -1.0d0
        real(real64) :: last_phase_merit = -1.0d0
        ! Outer count of the latest accepted step, used to detect a sudden
        ! increase in phase-coupling work without throttling a steady regime.
        integer(int32) :: last_accepted_phase_iterations = 1
        ! Time increment of the latest accepted step. Used only to extrapolate
        ! the initial ice-state guess; it does not enter the governing equations.
        real(real64) :: last_accepted_dt = 0.0d0
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

        ! Accepted-step state for transactional BDF1 LTE estimation.
        real(real64), allocatable :: lte_state_prev_thermal(:)
        real(real64), allocatable :: lte_state_prev_hydraulic(:)
        real(real64), allocatable :: lte_ydot_prev_thermal(:)
        real(real64), allocatable :: lte_ydot_prev_hydraulic(:)
        real(real64) :: lte_prev_dt = 0.0d0
        real(real64) :: lte_rtol = 1.0d-2
        real(real64) :: lte_atol_thermal = 1.0d-3
        real(real64) :: lte_atol_hydraulic = 1.0d1
        logical :: lte_error_control_active = .false.
        logical :: lte_has_state = .false.
        logical :: lte_has_derivative = .false.

        ! Anderson(1) acceleration state of the conserved coupled Picard loop:
        ! the previous iterate and previous fixed-point increment of (T, p),
        ! used by reflect_variables to form the depth-1 Anderson mixing.
        ! Cleared at the start of every nonlinear loop.
        real(real64), allocatable :: aa_T_prev(:)
        real(real64), allocatable :: aa_P_prev(:)
        real(real64), allocatable :: aa_duT_prev(:)
        real(real64), allocatable :: aa_duP_prev(:)
        logical :: aa_has_prev = .false.
        ! Weighted norm of the previous fixed-point increment ||g_{k-1}||_W.
        ! Retained for nonlinear diagnostics; growth alone does not disable AA(1).
        real(real64) :: aa_gnorm_prev = -1.0d0
        integer(int32) :: aa_use_count = 0
        real(real64) :: aa_gamma_max_abs = 0.0d0
        ! Per-attempt active-set marker for Hansson's one-time freezing-onset
        ! temperature reset. It is not a primary unknown or accepted history.
        logical, allocatable :: phase_onset_reset(:)

        ! Unit of Output/solver_history.log: one record per time-step attempt.
        ! The log distinguishes attempted/accepted time and dt, termination
        ! status, inner/outer work, phase metrics, nonlinear norms, and ATS
        ! diagnostics. -1 when the log is not open.
        integer(int32) :: solver_history_unit = -1

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
        procedure, private, pass(self) :: project_nodal_ice => project_nodal_ice_ftcms
        procedure, private, pass(self) :: compute_nodal_conserved => compute_nodal_conserved_ftcms
        procedure, public, pass(self) :: compute_lte_error => compute_lte_error_ftcms
        procedure, private, pass(self) :: initialize_lte_history => initialize_lte_history_ftcms
        procedure, private, pass(self) :: commit_lte_history => commit_lte_history_ftcms
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
        !> Build the per-node distinct-material table from the (already
        !> initialized) domain's node->element adjacency. See
        !> type_node_material_table for the storage layout and semantics.
        !>
        !> Computational complexity: O(N_nd * d) time and O(N_nd + E) memory,
        !> where d is the maximum node degree and E is the total number of
        !> distinct (node, material) entries (E <= sum of node degrees).
        !> Failure behavior: a domain with zero nodes leaves the table empty
        !> (num_nodes == 0); no error is raised.
        module subroutine initialize_node_material_table(self, domain)
            implicit none
            class(type_node_material_table), intent(inout) :: self
            type(type_domain), intent(in) :: domain
        end subroutine initialize_node_material_table

        !> Release the table's allocatable storage and reset it to empty.
        module subroutine destroy_node_material_table(self)
            implicit none
            class(type_node_material_table), intent(inout) :: self
        end subroutine destroy_node_material_table

        !> Build the Gauss-point geometry cache for the nodal gradient
        !> projection from the (already initialized) domain. See
        !> type_gradient_geometry_cache for the storage layout. The cached
        !> values are computed by the same calc_shape_function evaluations
        !> the projection previously performed per call, in the same order,
        !> so a cache-based projection is bit-identical to the direct one.
        !>
        !> Computational complexity: O(E_gp) time and memory, where E_gp is
        !> the total number of (element, gauss, node) entries.
        !> Failure behavior: a domain with zero elements or nodes leaves the
        !> cache empty (num_elements == 0); no error is raised.
        module subroutine initialize_gradient_geometry_cache(self, domain)
            implicit none
            class(type_gradient_geometry_cache), intent(inout) :: self
            type(type_domain), intent(in) :: domain
        end subroutine initialize_gradient_geometry_cache

        !> Release the cache's allocatable storage and reset it to empty.
        module subroutine destroy_gradient_geometry_cache(self)
            implicit none
            class(type_gradient_geometry_cache), intent(inout) :: self
        end subroutine destroy_gradient_geometry_cache

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

        module subroutine solve_ftcms(self)
            import :: type_ftcms
            implicit none
            class(type_ftcms), intent(inout) :: self
        end subroutine solve_ftcms

        module subroutine set_state_ftcms(self, node_id, element_id, state, calc_physics, include_fluxes)
            implicit none
            class(type_ftcms), intent(inout) :: self
            integer(int32), intent(in) :: node_id
            integer(int32), intent(in) :: element_id
            type(type_state), intent(inout) :: state
            logical, intent(in), optional :: calc_physics
            !> When .false. (and calc_physics applies), skip the Darcy flux
            !> evaluation and leave state%water_flux / state%vapor_flux
            !> unset. Only valid for callers that never read those fields.
            !> Default .true. (full constitutive evaluation).
            logical, intent(in), optional :: include_fluxes
        end subroutine set_state_ftcms

        module subroutine set_states_from_connectivity_ftcms(self, connectivity, element_id, states, calc_physics)
            implicit none
            class(type_ftcms), intent(inout) :: self
            integer(int32), intent(in) :: connectivity(:)
            integer(int32), intent(in) :: element_id
            type(type_state), intent(inout) :: states(:)
            logical, intent(in), optional :: calc_physics
        end subroutine set_states_from_connectivity_ftcms

        module subroutine update_physical_properties_ftcms(self, material_id, state, include_fluxes)
            implicit none
            class(type_ftcms), intent(inout) :: self
            integer(int32), intent(in) :: material_id
            type(type_state), intent(inout) :: state
            !> When .false., perform the phase-change update only and skip
            !> the Darcy flux evaluation, leaving state%water_flux /
            !> state%vapor_flux unset. Default .true. (compute fluxes).
            logical, intent(in), optional :: include_fluxes
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

        module subroutine project_nodal_ice_ftcms(self, apply_update, ice_update, max_increment, increment_norm, &
                                                   max_node, max_temperature, max_pressure, &
                                                   max_current_ice, max_projected_ice, &
                                                   max_equilibrium_error, increments, active_bounds, &
                                                   apply_projected_update, projected_update_scale)
            implicit none
            class(type_ftcms), intent(inout) :: self
            logical, intent(in) :: apply_update
            real(real64), intent(inout) :: ice_update(:)
            real(real64), intent(inout) :: max_increment
            real(real64), intent(inout) :: increment_norm
            integer(int32), intent(inout) :: max_node
            real(real64), intent(inout) :: max_temperature
            real(real64), intent(inout) :: max_pressure
            real(real64), intent(inout) :: max_current_ice
            real(real64), intent(inout) :: max_projected_ice
            real(real64), intent(inout) :: max_equilibrium_error
            real(real64), allocatable, intent(inout) :: increments(:)
            integer(int32), allocatable, intent(inout) :: active_bounds(:)
            logical, intent(in), optional :: apply_projected_update
            real(real64), intent(in), optional :: projected_update_scale
        end subroutine project_nodal_ice_ftcms

        !> Evaluate the per-node conserved quantities (volumetric enthalpy density
        !> and pore-water effective density) at the current iterate, for the
        !> conserved-quantity nonlinear convergence norm.
        module subroutine compute_nodal_conserved_ftcms(self, enthalpy, density)
            implicit none
            class(type_ftcms), intent(inout) :: self
            real(real64), allocatable, intent(inout) :: enthalpy(:)
            real(real64), allocatable, intent(inout) :: density(:)
        end subroutine compute_nodal_conserved_ftcms

        !> Normalized local-truncation-error estimate of the tentative BDF1 step.
        !>
        !> Uses the divided-difference (curvature) estimate of the implicit-Euler
        !> LTE: \( \text{LTE} \approx \lVert \dot y_n - \dot y_{n-1}\rVert\,
        !> \Delta t_n^2/(\Delta t_n+\Delta t_{n-1}) \), evaluated with a
        !> component-wise atol/rtol weighted RMS norm.
        !> Returns -1 on the first step (no previous derivative yet) so the caller
        !> skips error control. This function does not mutate accepted history.
        module function compute_lte_error_ftcms(self) result(error_rel)
            implicit none
            class(type_ftcms), intent(inout) :: self
            real(real64) :: error_rel
        end function compute_lte_error_ftcms

        module subroutine initialize_lte_history_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self
        end subroutine initialize_lte_history_ftcms

        module subroutine commit_lte_history_ftcms(self)
            implicit none
            class(type_ftcms), intent(inout) :: self
        end subroutine commit_lte_history_ftcms

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

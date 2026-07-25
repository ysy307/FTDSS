submodule(app_ftcms) ftcms_solve
    implicit none

    ! Keep the history predictor within the latent-heat scale that the next
    ! monolithic solve can absorb without crossing the phase boundary.
    real(real64), parameter :: PHASE_PREDICTOR_MAX_INCREMENT = 1.0d-3
    integer(int32), parameter :: SOLVE_STATUS_NOT_RUN = 0
    integer(int32), parameter :: SOLVE_STATUS_CONVERGED = 1
    integer(int32), parameter :: SOLVE_STATUS_LINEAR_FAILURE = 2
    integer(int32), parameter :: SOLVE_STATUS_NONLINEAR_DIVERGED = 3
    integer(int32), parameter :: SOLVE_STATUS_NONLINEAR_LIMIT = 4
    integer(int32), parameter :: SOLVE_STATUS_PHASE_FAILURE = 5

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

        ! [Important] Compute solver must always be PICARD or NEWTON if not NONE.
        ! Even for linear config (where iter=1 is forced), Picard discretization
        ! is often the base, but if explicitly NONE, we should respect it.
        if (.not. self%control%is_none()) then
            call self%control%set_nonlinear_solver(NONLINEAR_SOLVER%PICARD)
        end if

        call self%control%increment_total()
        call self%control%reset_acceleration()

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
            predicted_increment = min(max(predicted_increment, -PHASE_PREDICTOR_MAX_INCREMENT), &
                                      PHASE_PREDICTOR_MAX_INCREMENT)
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

        ! Nodal conserved quantities at the updated iterate
        call self%compute_nodal_conserved(enthalpy, density)

        ! Per-block residuals from the assembly at the updated iterate.
        if (check_thermal) call self%get_variable_residual(PHYSICS_TYPES%THERMAL, residual_thermal)
        if (check_hydraulic) call self%get_variable_residual(PHYSICS_TYPES%HYDRAULIC, residual_hydraulic)

        ! Unallocated residual arrays propagate as absent optional arguments.
        call self%control%check_convergence_conserved(enthalpy, density, &
                                                      residual_thermal, residual_hydraulic, &
                                                      check_thermal, check_hydraulic)

        if (allocated(enthalpy)) deallocate (enthalpy)
        if (allocated(density)) deallocate (density)
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

        ! Outer local phase-equilibrium iteration. The inner solve remains a
        ! monolithic T-p solve with no ice DOF added to its block structure.
        logical :: do_phase_outer
        integer(int32) :: coupling_iter
        ! Hansson et al. (2004) is a fully implicit single monolithic solve:
        ! ice is a state function theta_i(T,p) (calc_ice_content) evaluated in
        ! the residual, and the latent heat enters the fast Newton diagonal as
        ! the apparent heat capacity C_a = C_p - Lf*rho_i*dtheta_i/dT
        ! (thermal_coefficients.F90). No outer projection loop is needed; it is
        ! disabled here. Set true only to A/B against the old outer-lagged path.
        logical, parameter :: PHASE_USE_OUTER_PROJECTION = .false.
        ! Allow a contracting phase map to finish before ATS reduces dt.
        integer(int32), parameter :: MAX_PHASE_ITER = 240
        ! 5 kPa corresponds to about 4e-3 K through Clapeyron near 0 C,
        ! below the verified spatial temperature discretization error.
        real(real64), parameter :: PHASE_PRESSURE_TOL = 5.0d3
        ! The phase equation is a water-content equality. This tolerance is
        ! consistent with the configured hydraulic conserved-quantity scale.
        real(real64), parameter :: PHASE_CONTENT_TOL = 1.0d-3
        ! Keep a local guard without forcing a mesh-dependent L-infinity solve
        ! to the same tolerance as the volume-weighted phase balance. A 0.005
        ! volumetric ice-content defect is accepted only when the global RMS
        ! content error and local Clapeyron-pressure condition also pass.
        real(real64), parameter :: PHASE_CONTENT_MAX_TOL = 5.0d-3
        integer(int32), parameter :: PHASE_ANDERSON_DEPTH = 5
        real(real64), parameter :: PHASE_MIXING = 0.3d0
        real(real64), parameter :: PHASE_MIXING_MIN = 0.05d0
        real(real64), parameter :: PHASE_STEP_FLOOR = 1.0d-5
        integer(int32), parameter :: PHASE_STAGNATION_LIMIT = 40
        real(real64) :: phase_increment_max, phase_increment_norm
        real(real64) :: phase_temperature, phase_pressure
        real(real64) :: phase_current_ice, phase_projected_ice
        real(real64) :: phase_equilibrium_error
        real(real64) :: phase_step_limit, phase_step_max, phase_step_factor
        real(real64) :: phase_mixing_current, previous_phase_increment_norm
        real(real64) :: phase_merit, phase_best_merit
        real(real64), allocatable :: initial_residual_thermal(:), initial_residual_hydraulic(:)
        real(real64), allocatable :: phase_increments(:), previous_phase_increments(:)
        real(real64), allocatable :: phase_update(:), previous_phase_update(:)
        real(real64), allocatable :: anderson_dF(:, :), anderson_dX(:, :)
        real(real64), allocatable :: anderson_matrix(:, :), anderson_rhs(:), anderson_gamma(:)
        integer(int32), allocatable :: phase_active_bounds(:)
        integer(int32) :: phase_max_node, phase_node, num_phase_nodes, anderson_count
        integer(int32) :: num_active_nodes
        integer(int32) :: phase_stagnation_count
        logical :: linear_failed, phase_is_converged, anderson_success
        logical :: phase_reset_anderson
        logical :: phase_final_correction_applied
        logical :: min_dt_extension_announced
        integer(int32) :: max_iter_config, min_dt_iter_limit
        integer(int32), parameter :: MIN_DT_ITER_FACTOR = 5


        is_step_converged = .false.

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
        if (do_phase_outer) call apply_phase_predictor(self)
        call self%domain%get_num_nodes(num_phase_nodes)
        allocate (phase_update(num_phase_nodes), source=0.0d0)
        allocate (previous_phase_update(num_phase_nodes), source=0.0d0)
        allocate (anderson_dF(num_phase_nodes, PHASE_ANDERSON_DEPTH), source=0.0d0)
        allocate (anderson_dX(num_phase_nodes, PHASE_ANDERSON_DEPTH), source=0.0d0)
        allocate (anderson_matrix(PHASE_ANDERSON_DEPTH, PHASE_ANDERSON_DEPTH), source=0.0d0)
        allocate (anderson_rhs(PHASE_ANDERSON_DEPTH), source=0.0d0)
        allocate (anderson_gamma(PHASE_ANDERSON_DEPTH), source=0.0d0)
        anderson_count = 0
        phase_mixing_current = PHASE_MIXING
        previous_phase_increment_norm = -1.0d0
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
                    if (allocated(initial_residual_thermal)) deallocate (initial_residual_thermal)
                    if (allocated(initial_residual_hydraulic)) deallocate (initial_residual_hydraulic)
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

                ! Update solution with relaxation (adaptive omega for conserved mode,
                ! Aitken for legacy Picard, damped for Newton).
                call self%reflect_variables()

                if (self%control%is_conserved()) then
                    ! The solved system was assembled at x_k. Reassemble after
                    ! reflection so the residual and conserved fields both refer
                    ! to x_{k+1} when convergence is tested.
                    call self%assemble()
                    call self%apply_bc(prescribed=.false.)
                    call self%solve_time_step_check_convergence_conserved()
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

            ! Hansson single-solve path: ice is already the state function
            ! theta_i(T,p) used inside the just-converged monolithic solve.
            ! Refresh the nodal Qw/Qi/Qa/Qv fields from the converged (T,p) so
            ! the history/output carry the consistent phase state, then exit -
            ! no outer projection iteration.
            if (.not. do_phase_outer) then
                call self%update_nodal_phases()
                exit coupling_loop
            end if

            call self%project_nodal_ice(.false., phase_update, phase_increment_max, phase_increment_norm, &
                                        phase_max_node, phase_temperature, phase_pressure, &
                                        phase_current_ice, phase_projected_ice, &
                                        phase_equilibrium_error, phase_increments, phase_active_bounds)

            phase_reset_anderson = .false.
            if (previous_phase_increment_norm > 0.0d0) then
                if (phase_increment_norm > 1.2d0 * previous_phase_increment_norm) then
                    phase_mixing_current = max(PHASE_MIXING_MIN, 0.5d0 * phase_mixing_current)
                    anderson_count = 0
                    phase_reset_anderson = .true.
                else if (phase_increment_norm < 0.95d0 * previous_phase_increment_norm) then
                    phase_mixing_current = min(PHASE_MIXING, 1.2d0 * phase_mixing_current)
                end if
            end if

            if (allocated(previous_phase_increments) .and. .not. phase_reset_anderson) then
                if (anderson_count < PHASE_ANDERSON_DEPTH) then
                    anderson_count = anderson_count + 1
                else
                    anderson_dF(:, 1:PHASE_ANDERSON_DEPTH - 1) = anderson_dF(:, 2:PHASE_ANDERSON_DEPTH)
                    anderson_dX(:, 1:PHASE_ANDERSON_DEPTH - 1) = anderson_dX(:, 2:PHASE_ANDERSON_DEPTH)
                end if
                anderson_dF(:, anderson_count) = phase_increments - previous_phase_increments
                anderson_dX(:, anderson_count) = previous_phase_update

                anderson_matrix(1:anderson_count, 1:anderson_count) = matmul( &
                    transpose(anderson_dF(:, 1:anderson_count)), anderson_dF(:, 1:anderson_count))
                anderson_rhs(1:anderson_count) = matmul( &
                    transpose(anderson_dF(:, 1:anderson_count)), phase_increments)
                call solve_phase_anderson_system(anderson_matrix(1:anderson_count, 1:anderson_count), &
                                                  anderson_rhs(1:anderson_count), &
                                                  anderson_gamma(1:anderson_count), anderson_success)
                if (anderson_success) then
                    phase_update = phase_mixing_current * phase_increments - matmul( &
                        anderson_dX(:, 1:anderson_count) + phase_mixing_current * anderson_dF(:, 1:anderson_count), &
                        anderson_gamma(1:anderson_count))
                else
                    phase_update = phase_mixing_current * phase_increments
                    anderson_count = 0
                end if
                if (dot_product(phase_update, phase_increments) <= 0.0d0 .or. &
                    maxval(abs(phase_update)) < 0.25d0 * phase_mixing_current * phase_increment_max) then
                    phase_update = phase_mixing_current * phase_increments
                    anderson_count = 0
                end if
            else
                phase_update = phase_mixing_current * phase_increments
            end if

            ! A global Anderson descent direction can still move an individual
            ! node away from its bounded local phase projection. Preserve the
            ! local phase-transfer direction without restricting the coupled
            ! Anderson update at nodes where it already points toward the target.
            do phase_node = 1, num_phase_nodes
                if (abs(phase_increments(phase_node)) <= tiny(1.0d0)) then
                    phase_update(phase_node) = 0.0d0
                else if (phase_update(phase_node) * phase_increments(phase_node) <= 0.0d0 .or. &
                         abs(phase_update(phase_node)) <= tiny(1.0d0)) then
                    phase_update(phase_node) = phase_mixing_current * phase_increments(phase_node)
                end if
            end do
            phase_step_max = maxval(abs(phase_update))
            phase_step_limit = max(PHASE_STEP_FLOOR, 2.0d0 * phase_mixing_current * phase_increment_max)
            if (phase_step_max > phase_step_limit) phase_update = phase_update * phase_step_limit / phase_step_max
            num_active_nodes = count(phase_active_bounds /= 0)
            phase_step_factor = 0.0d0
            if (abs(phase_increments(phase_max_node)) > tiny(1.0d0)) then
                phase_step_factor = phase_update(phase_max_node) / phase_increments(phase_max_node)
            end if
            write (*, '(A,I0,A,ES10.3,A,ES10.3,A,I0,A,F9.4,A,ES11.3,' // &
                        'A,ES10.3,A,ES10.3,A,ES10.3,A,F6.3,A,I0)') &
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
            previous_phase_increment_norm = phase_increment_norm
            phase_final_correction_applied = phase_is_converged
            previous_phase_update = phase_update
            call self%project_nodal_ice(.true., phase_update, phase_increment_max, phase_increment_norm, &
                                        phase_max_node, phase_temperature, phase_pressure, &
                                        phase_current_ice, phase_projected_ice, &
                                        phase_equilibrium_error, phase_increments, phase_active_bounds)
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

    !> Solve the small regularized normal equation used by outer Anderson mixing.
    subroutine solve_phase_anderson_system(matrix, rhs, solution, success)
        implicit none
        real(real64), intent(in) :: matrix(:, :)
        real(real64), intent(in) :: rhs(:)
        real(real64), intent(inout) :: solution(:)
        logical, intent(inout) :: success

        real(real64) :: work_matrix(size(rhs), size(rhs)), work_rhs(size(rhs))
        real(real64) :: factor, pivot_value, regularization, row_value
        integer(int32) :: i, j, k, pivot, system_size

        system_size = size(rhs)
        solution = 0.0d0
        success = .false.
        if (system_size < 1) return

        work_matrix = matrix
        work_rhs = rhs
        regularization = 0.0d0
        do i = 1, system_size
            regularization = regularization + abs(work_matrix(i, i))
        end do
        regularization = max(1.0d-24, 1.0d-10 * regularization / real(system_size, real64))
        do i = 1, system_size
            work_matrix(i, i) = work_matrix(i, i) + regularization
        end do

        do k = 1, system_size - 1
            pivot = k - 1 + maxloc(abs(work_matrix(k:system_size, k)), dim=1)
            pivot_value = abs(work_matrix(pivot, k))
            if (pivot_value <= tiny(1.0d0)) return
            if (pivot /= k) then
                do j = k, system_size
                    row_value = work_matrix(k, j)
                    work_matrix(k, j) = work_matrix(pivot, j)
                    work_matrix(pivot, j) = row_value
                end do
                row_value = work_rhs(k)
                work_rhs(k) = work_rhs(pivot)
                work_rhs(pivot) = row_value
            end if
            do i = k + 1, system_size
                factor = work_matrix(i, k) / work_matrix(k, k)
                work_matrix(i, k) = 0.0d0
                work_matrix(i, k + 1:system_size) = work_matrix(i, k + 1:system_size) - &
                                                    factor * work_matrix(k, k + 1:system_size)
                work_rhs(i) = work_rhs(i) - factor * work_rhs(k)
            end do
        end do
        if (abs(work_matrix(system_size, system_size)) <= tiny(1.0d0)) return

        do i = system_size, 1, -1
            row_value = work_rhs(i)
            if (i < system_size) then
                row_value = row_value - dot_product(work_matrix(i, i + 1:system_size), &
                                                     solution(i + 1:system_size))
            end if
            if (abs(work_matrix(i, i)) <= tiny(1.0d0)) return
            solution(i) = row_value / work_matrix(i, i)
            if (.not. (solution(i) == solution(i) .and. abs(solution(i)) < huge(1.0d0))) return
        end do
        success = .true.
    end subroutine solve_phase_anderson_system

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
               '(2(I10,1X),5(ES15.7,1X),I1,1X,A18,1X,9(I10,1X),11(ES13.5,1X))') &
            attempt, accepted_step, time_start, time_trial, time_accepted, dt_used, dt_next, &
            merge(1_int32, 0_int32, accepted), status, &
            self%last_inner_iterations, self%last_max_inner_iterations, self%last_phase_iterations, &
            self%last_nonlinear_work, ats_iter, merge(1_int32, 0_int32, phase_spike), &
            merge(1_int32, 0_int32, self%last_phase_metrics_available), &
            merge(1_int32, 0_int32, self%last_phase_converged), self%last_phase_active_nodes, &
            self%last_phase_increment_max, self%last_phase_increment_norm, &
            self%last_phase_equilibrium_error, self%last_phase_merit, &
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
            if (is_step_converged) lte_error = self%compute_lte_error()

            attempt_counter = attempt_counter + 1

            if (is_step_converged) then
                self%last_accepted_dt = dt_used
                self%last_accepted_phase_iterations = phase_iter
                ! Update time and adaptive time stepping
                call self%control%update(is_step_converged, error_estimate=lte_error, &
                                         iteration_count=effective_iter)

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
                                         iteration_count=effective_iter)

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
end submodule ftcms_solve

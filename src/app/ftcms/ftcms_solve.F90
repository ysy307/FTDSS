submodule(app_ftcms) ftcms_solve
    implicit none

contains
    module subroutine solve_time_step_initial_setup_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        real(real64), pointer, contiguous, dimension(:) :: u

        nullify (u)

        ! Reset iteration control
        ! reset() may set the compute solver to NONE when config is NONE.
        call self%control%reset_iteration()

        ! [Important] Compute solver must always be PICARD or NEWTON.
        ! Even for NONE (linear) config, Picard discretization is used,
        ! so explicitly set PICARD here to override the reset state.
        call self%control%set_nonlinear_solver(NONLINEAR_SOLVER%PICARD)

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

    end subroutine solve_time_step_initial_setup_ftcms

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

        call self%calc_gradient_temperature()
        call self%calc_gradient_pressure()

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

    module subroutine solve_time_step_ftcms(self, is_step_converged)
        implicit none
        class(type_ftcms), intent(inout) :: self
        logical, intent(inout) :: is_step_converged
        logical :: prescribe_bc
        integer(int32) :: iter_nl
        real(real64) :: t_res, t_inc, h_res, h_inc

        ! Staggered coupling variables
        logical :: do_staggered
        integer(int32) :: coupling_iter
        integer(int32), parameter :: MAX_COUPLING_ITER = 3
        real(real64), parameter :: COUPLING_TOL = 1.0d-3
        real(real64) :: coupling_change_T, coupling_change_P
        real(real64), allocatable :: T_old(:)
        real(real64), allocatable :: P_old(:)
        real(real64), pointer, contiguous :: T_cur(:) => null()
        real(real64), pointer, contiguous :: P_cur(:) => null()
        integer(int32) :: bdf_order
        real(real64) :: T_scale, P_scale, mean_pressure
        integer(int32) :: num_nodes
        logical :: linear_failed

        is_step_converged = .false.

        ! Dispatch to true staggered solver (H then T sequential nonlinear loops)
        if (self%control%is_staggered() .and. &
            self%is_active_thermal() .and. self%is_active_hydraulic()) then
            call self%solve_time_step_staggered(is_step_converged)
            return
        end if

        ! Monolithic: single coupled nonlinear loop (no outer coupling iteration)
        do_staggered = .false.

        if (do_staggered) then
            call self%domain%get_num_nodes(num_nodes)
            allocate (T_old(num_nodes), P_old(num_nodes))
        end if

        ! Initialize per-time-step state only once.
        call self%solve_time_step_initial_setup()

        ! Outer coupling iteration loop
        coupling_loop: do coupling_iter = 1, merge(MAX_COUPLING_ITER, 1, do_staggered)
            linear_failed = .false.

            ! Save solution before inner nonlinear solve for coupling check
            if (do_staggered .and. coupling_iter > 1) then
                call self%temperature%get_current(T_cur)
                call self%pressure%get_current(P_cur)
                if (associated(T_cur)) T_old(:) = T_cur(:)
                if (associated(P_cur)) P_old(:) = P_cur(:)
                nullify (T_cur)
                nullify (P_cur)
            end if

            ! For coupling iterations > 1, reset nonlinear controls only.
            if (coupling_iter > 1) then
                call self%control%reset_iteration()
                call self%control%set_nonlinear_solver(NONLINEAR_SOLVER%PICARD)
                call self%control%increment_total()
                call self%control%reset_acceleration()
            end if

            ! Nonlinear iteration loop
            nonlinear: do while (self%control%should_continue())

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

                ! Update solution with relaxation (Aitken for Picard, damped for Newton)
                call self%reflect_variables()

                ! Anchor the all-Neumann null-mode to the initial mean pressure
                ! (preserves absolute level required by the WRF).
                if (self%is_active_hydraulic() .and. (.not. self%hydraulic_has_dirichlet_bc) &
                    .and. self%hydraulic_ref_mean_set) then
                    call self%pressure%get_current(P_cur)
                    if (associated(P_cur) .and. size(P_cur) > 0) then
                        mean_pressure = sum(P_cur) / real(size(P_cur), real64)
                        P_cur(:) = P_cur(:) - (mean_pressure - self%hydraulic_ref_mean)
                    end if
                    nullify (P_cur)
                end if

                ! Force exit after one iteration when config is NONE (linear solve)
                if (self%control%is_none()) exit nonlinear

            end do nonlinear

            is_step_converged = self%control%is_converged()

            if (.not. is_step_converged) then
                call self%control%get_nonlinear_iter(iter_nl)
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

            ! On first coupling iteration or if not staggered, exit
            if (.not. do_staggered .or. coupling_iter == 1) exit coupling_loop

            ! Check coupling convergence: has the solution changed significantly
            ! between coupling iterations?
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

        if (do_staggered) then
            if (allocated(T_old)) deallocate (T_old)
            if (allocated(P_old)) deallocate (P_old)
        end if

    end subroutine solve_time_step_ftcms

    module subroutine solve_time_step_staggered_ftcms(self, is_step_converged)
        implicit none
        class(type_ftcms), intent(inout) :: self
        logical, intent(inout) :: is_step_converged

        logical :: prescribe_bc
        integer(int32) :: iter_nl, coupling_iter, num_nodes, bdf_order
        integer(int32), parameter :: MAX_COUPLING_ITER = 1
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

        coupling_loop: do coupling_iter = 1, MAX_COUPLING_ITER

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
                        linear_failed = .true.
                        write (*, '(A,I0,A)') '   [HYD_NL] reached nonlinear iteration cap (', MAX_PHASE_NL_ITER, &
                            '). Triggering timestep retry.'
                        call self%control%set_converged(PHYSICS_TYPES%HYDRAULIC, .false.)
                        call self%control%set_diverged(PHYSICS_TYPES%HYDRAULIC, .true.)
                        exit hydraulic_nl
                    end if

                    ! Anchor the all-Neumann null-mode to the initial mean pressure.
                    if ((.not. self%hydraulic_has_dirichlet_bc) .and. self%hydraulic_ref_mean_set) then
                        call self%pressure%get_current(P_cur)
                        if (associated(P_cur) .and. size(P_cur) > 0) then
                            mean_pressure = sum(P_cur) / real(size(P_cur), real64)
                            P_cur(:) = P_cur(:) - (mean_pressure - self%hydraulic_ref_mean)
                        end if
                        nullify (P_cur)
                    end if

                    if (self%control%is_none()) exit hydraulic_nl
                end do hydraulic_nl

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
                            call self%control%set_converged(PHYSICS_TYPES%THERMAL, .false.)
                            call self%control%set_diverged(PHYSICS_TYPES%THERMAL, .true.)
                            exit thermal_nl
                        end if
                    else if (.not. self%solver%is_success()) then
                        linear_failed = .true.
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
                        linear_failed = .true.
                        write (*, '(A,I0,A)') '   [THM_NL] reached nonlinear iteration cap (', MAX_PHASE_NL_ITER, &
                            '). Triggering timestep retry.'
                        call self%control%set_converged(PHYSICS_TYPES%THERMAL, .false.)
                        call self%control%set_diverged(PHYSICS_TYPES%THERMAL, .true.)
                        exit thermal_nl
                    end if

                    ! Anchor the all-Neumann null-mode to the initial mean pressure.
                    if (self%is_active_hydraulic() .and. (.not. self%hydraulic_has_dirichlet_bc) &
                        .and. self%hydraulic_ref_mean_set) then
                        call self%pressure%get_current(P_cur)
                        if (associated(P_cur) .and. size(P_cur) > 0) then
                            mean_pressure = sum(P_cur) / real(size(P_cur), real64)
                            P_cur(:) = P_cur(:) - (mean_pressure - self%hydraulic_ref_mean)
                        end if
                        nullify (P_cur)
                    end if

                    if (self%control%is_none()) exit thermal_nl
                end do thermal_nl

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

    end subroutine solve_time_step_staggered_ftcms

    module subroutine run_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        logical :: is_step_converged
        integer(int32) :: consecutive_failures
        integer(int32) :: step_counter
        integer(int32) :: nl_iter
        real(real64) :: time_s, dt_s
        integer(int32), parameter :: MAX_CONSECUTIVE_FAILURES = 50

        consecutive_failures = 0
        step_counter = 0

        ! Loop until end time
        time_loop: do while (.not. self%control%is_end_time())
            call self%solve_time_step(is_step_converged)

            ! Update time and adaptive time stepping
            call self%control%update(is_step_converged)

            if (is_step_converged) then
                consecutive_failures = 0
                step_counter = step_counter + 1
                call self%control%get_nonlinear_iter(nl_iter)
                call self%control%get_time(time_s)
                if (step_counter == 1 .or. mod(step_counter, 20) == 0 .or. nl_iter > 8) then
                    write (*, '(A,I0,A,ES13.5,A,I0)') '   [STEP] converged: n=', step_counter, &
                        ', t[s]=', time_s, ', nonlinear_iter=', nl_iter
                end if
                ! Update segregated ice content (explicit forward Euler)
                call self%update_segregation_ice()

                ! Shift variable history on convergence
                call self%shift()

                call self%update_variables()
                call self%output_fields()
                call self%output_history()
            else
                ! Retry with smaller dt
                consecutive_failures = consecutive_failures + 1

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

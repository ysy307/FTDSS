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

    module subroutine solve_time_step_check_convergence_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout), target :: self

        integer(int32) :: iter

        real(real64), pointer, contiguous, dimension(:) :: current_value

        real(real64), allocatable :: residual(:)
        real(real64), allocatable :: increment(:)
        real(real64) :: current_norm
        real(real64) :: switch_norm(PHYSICS_TYPES%NUM_ID)
        logical :: should_switch
        logical, parameter :: diverged = .true.

        logical :: is_compute_newton, is_compute_picard, is_config_none

        nullify (current_value)

        ! Explicit initialization (avoid implicit SAVE from initializer expression)
        switch_norm(:) = [1.0d-2, 1.0d-4, 1.0d-4]
        should_switch = .true.

        ! Get compute (dynamic) solver state
        is_compute_newton = self%control%is_compute_newton()
        is_compute_picard = self%control%is_compute_picard()
        ! Check whether config (static) solver is NONE
        is_config_none = self%control%is_none()

        ! ----------------------------------------------------------------------
        ! Thermal Convergence Check
        ! ----------------------------------------------------------------------
        if (self%control%is_physics_active(PHYSICS_TYPES%THERMAL)) then
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
                ! When config is NONE, the iteration module returns converged immediately
                call self%control%check_convergence(PHYSICS_TYPES%THERMAL, residual, increment)
            end if

            ! Aitken relaxation check

        end if

        ! ----------------------------------------------------------------------
        ! Hydraulic Convergence Check
        ! ----------------------------------------------------------------------
        if (self%control%is_physics_active(PHYSICS_TYPES%HYDRAULIC)) then
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
                call self%control%check_convergence(PHYSICS_TYPES%HYDRAULIC, residual, increment)
            end if
        end if

        ! ----------------------------------------------------------------------
        ! 2. Hybrid method switch decision
        !    Skip when config is NONE (linear)
        ! ----------------------------------------------------------------------
        call self%control%get_nonlinear_iter(iter)

        if (iter > 1 .and. .not. self%control%is_diverged()) then
            if (is_compute_picard .and. .not. is_config_none) then
                should_switch = .true.

                ! Thermal Residual Check
                if (self%control%is_physics_active(PHYSICS_TYPES%THERMAL)) then
                    current_norm = 0.0d0
                    call self%control%get_current_norm(PHYSICS_TYPES%THERMAL, NONLINEAR_NORM_CRITERIA%RESIDUAL, &
                                                       NORM_TYPES%LINF, current_norm)
                    ! [Debug output skipped]
                    if (current_norm > switch_norm(PHYSICS_TYPES%THERMAL%ID)) then
                        should_switch = .false.
                    end if
                end if

                ! Hydraulic Residual Check
                if (self%control%is_physics_active(PHYSICS_TYPES%HYDRAULIC)) then
                    current_norm = 0.0d0
                    call self%control%get_current_norm(PHYSICS_TYPES%HYDRAULIC, NONLINEAR_NORM_CRITERIA%RESIDUAL, &
                                                       NORM_TYPES%LINF, current_norm)
                    if (current_norm > switch_norm(PHYSICS_TYPES%HYDRAULIC%ID)) then
                        should_switch = .false.
                    end if
                end if

                if (should_switch) then
                    write (*, '("   -> Residual small enough. Switching to Newton-Raphson.")')
                    call self%control%set_nonlinear_solver(NONLINEAR_SOLVER%NEWTON)
                end if
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
        real(real64) :: T_scale, P_scale
        integer(int32) :: num_nodes

        is_step_converged = .false.

        ! Use staggered coupling only when both thermal and hydraulic are active
        do_staggered = self%is_active_thermal() .and. self%is_active_hydraulic()

        if (do_staggered) then
            call self%domain%get_num_nodes(num_nodes)
            allocate (T_old(num_nodes), P_old(num_nodes))
        end if

        ! Outer coupling iteration loop
        coupling_loop: do coupling_iter = 1, merge(MAX_COUPLING_ITER, 1, do_staggered)

            ! Save solution before inner nonlinear solve for coupling check
            if (do_staggered .and. coupling_iter > 1) then
                call self%temperature%get_current(T_cur)
                call self%pressure%get_current(P_cur)
                if (associated(T_cur)) T_old(:) = T_cur(:)
                if (associated(P_cur)) P_old(:) = P_cur(:)
                nullify (T_cur)
                nullify (P_cur)
            end if

            ! Initial setup (compute solver is always set to PICARD here)
            call self%solve_time_step_initial_setup()

            ! Nonlinear iteration loop
            nonlinear: do while (self%control%should_continue())

                ! Setup (update iteration counter)
                call self%solve_time_step_setup(prescribe_bc)

                ! Assemble matrices and residual
                call self%assemble()

                ! Apply boundary conditions
                call self%apply_bc(prescribe_bc)

                ! Linear solve (K * du = F)
                call self%solve()

                ! If linear solver failed, mark as diverged and exit
                if (.not. self%solver%is_success()) then
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

                ! Force exit after one iteration when config is NONE (linear solve)
                if (self%control%is_none()) exit nonlinear

            end do nonlinear

            is_step_converged = self%control%is_converged()

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

    module subroutine run_ftcms(self)
        implicit none
        class(type_ftcms), intent(inout) :: self

        logical :: is_step_converged
        integer(int32) :: consecutive_failures
        integer(int32), parameter :: MAX_CONSECUTIVE_FAILURES = 50

        consecutive_failures = 0

        ! Loop until end time
        time_loop: do while (.not. self%control%is_end_time())
            call self%solve_time_step(is_step_converged)

            ! Update time and adaptive time stepping
            call self%control%update(is_step_converged)

            if (is_step_converged) then
                consecutive_failures = 0
                ! Shift variable history on convergence
                call self%shift()

                call self%update_variables()
                call self%output_fields()
                call self%output_history()
            else
                ! Retry with smaller dt
                consecutive_failures = consecutive_failures + 1
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

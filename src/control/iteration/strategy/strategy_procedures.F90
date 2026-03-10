submodule(control_iteration_strategy) iteration_strategy_procedure
    implicit none
contains
    module subroutine initialize_iteration_strategy(self, switch_iteration_threshold)
        implicit none
        class(type_iteration_strategy), intent(inout) :: self
        integer(int32), intent(in), optional :: switch_iteration_threshold

        self%switch_iteration_threshold = optval(switch_iteration_threshold, 0)
        self%initialized = .true.
    end subroutine initialize_iteration_strategy

    module subroutine destroy_iteration_strategy(self)
        implicit none
        class(type_iteration_strategy), intent(inout) :: self

        self%initialized = .false.
    end subroutine destroy_iteration_strategy

    module subroutine apply_initial_policy_iteration_strategy(self, nonlinear_solver_type, compute_solver_type)
        implicit none
        class(type_iteration_strategy), intent(in) :: self
        type(type_constant_id), intent(in) :: nonlinear_solver_type
        type(type_constant_id), intent(inout) :: compute_solver_type

        ! Initial step strategy:
        ! If solver type is not NONE, start with Picard.
        ! If NONE, set compute solver to NONE as well.
        if (nonlinear_solver_type /= NONLINEAR_SOLVER%NONE) then
            compute_solver_type = NONLINEAR_SOLVER%PICARD
        else
            compute_solver_type = NONLINEAR_SOLVER%NONE
        end if
    end subroutine apply_initial_policy_iteration_strategy

    module subroutine update_active_policy_iteration_strategy(self, nonlinear_iter, is_converged, compute_solver_type)
        implicit none
        class(type_iteration_strategy), intent(in) :: self
        integer(int32), intent(in) :: nonlinear_iter
        logical, intent(in) :: is_converged
        type(type_constant_id), intent(inout) :: compute_solver_type

        ! Skip if already converged or threshold is not set (<=0)
        if (is_converged .or. self%switch_iteration_threshold <= 0) return

        ! Switch from Picard to Newton when iteration count exceeds threshold
        if (compute_solver_type == NONLINEAR_SOLVER%PICARD .and. &
            nonlinear_iter >= self%switch_iteration_threshold) then
            compute_solver_type = NONLINEAR_SOLVER%NEWTON
        end if
    end subroutine update_active_policy_iteration_strategy

    module pure subroutine determine_continuation_iteration_strategy(self, nonlinear_iter, max_iterations, &
                                                                     is_converged, is_diverged, should_continue)
        implicit none
        class(type_iteration_strategy), intent(in) :: self
        integer(int32), intent(in) :: nonlinear_iter
        integer(int32), intent(in) :: max_iterations
        logical, intent(in) :: is_converged
        logical, intent(in) :: is_diverged
        logical, intent(inout) :: should_continue

        ! Exit loop immediately if diverged (safety mechanism)
        if (is_diverged) then
            should_continue = .false.
            return
        end if

        ! Always execute the first iteration (before convergence check)
        if (nonlinear_iter <= 1) then
            should_continue = .true.
            return
        end if

        ! Continue if not converged and max iterations not reached
        should_continue = (.not. is_converged) .and. (nonlinear_iter < max_iterations)
    end subroutine determine_continuation_iteration_strategy

    module subroutine assess_convergence_trend_iteration_strategy(self, current_norm, previous_norm, is_stagnating)
        implicit none
        class(type_iteration_strategy), intent(in) :: self
        real(real64), intent(in) :: current_norm
        real(real64), intent(in) :: previous_norm
        logical, intent(inout) :: is_stagnating

        ! Simple stagnation check:
        ! norm has barely decreased compared to previous iteration
        if (current_norm > previous_norm * 0.99d0) then
            is_stagnating = .true.
        else
            is_stagnating = .false.
        end if
    end subroutine assess_convergence_trend_iteration_strategy

end submodule iteration_strategy_procedure

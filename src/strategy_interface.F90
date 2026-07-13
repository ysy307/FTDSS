module control_iteration_strategy
    use, intrinsic :: iso_fortran_env
    use :: stdlib_optval, only:optval
    use :: module_core
    implicit none
    private

    public :: type_iteration_strategy

    type :: type_iteration_strategy
        !> Flags whether the strategy has been initialized
        logical, private :: initialized = .false.
        ! Strategy parameters (e.g., iteration threshold for switching methods)
        integer(int32), private :: switch_iteration_threshold = 0
    contains
        ! ---- Lifecycle ----
        procedure, public, pass(self) :: initialize => initialize_iteration_strategy
        procedure, public, pass(self) :: destroy => destroy_iteration_strategy
        ! ---- Mutator ----
        ! ---- Algorithm / Operation ----
        procedure, public, pass(self) :: apply_initial_policy => apply_initial_policy_iteration_strategy
        procedure, public, pass(self) :: update_active_policy => update_active_policy_iteration_strategy
        procedure, public, pass(self) :: determine_continuation => determine_continuation_iteration_strategy
        procedure, public, pass(self) :: assess_convergence_trend => assess_convergence_trend_iteration_strategy
        ! ---- Inquiry ----
        ! ---- Getter ----
        ! ---- Meta / Utility ----
    end type

    interface
        module subroutine initialize_iteration_strategy(self, switch_iteration_threshold)
            implicit none
            class(type_iteration_strategy), intent(inout) :: self
            integer(int32), intent(in), optional :: switch_iteration_threshold

        end subroutine initialize_iteration_strategy

        module subroutine destroy_iteration_strategy(self)
            implicit none
            class(type_iteration_strategy), intent(inout) :: self

        end subroutine destroy_iteration_strategy

        module subroutine apply_initial_policy_iteration_strategy(self, nonlinear_solver_type, compute_solver_type)
            implicit none
            class(type_iteration_strategy), intent(in) :: self
            type(type_constant_id), intent(in) :: nonlinear_solver_type
            type(type_constant_id), intent(inout) :: compute_solver_type
        end subroutine apply_initial_policy_iteration_strategy

        !> Decides whether to switch the solver method during iterations.
        module subroutine update_active_policy_iteration_strategy(self, nonlinear_iter, is_converged, compute_solver_type)
            implicit none
            class(type_iteration_strategy), intent(in) :: self
            integer(int32), intent(in) :: nonlinear_iter
            logical, intent(in) :: is_converged
            type(type_constant_id), intent(inout) :: compute_solver_type
        end subroutine update_active_policy_iteration_strategy

        !> Determines if the nonlinear iteration loop should continue.
        module pure subroutine determine_continuation_iteration_strategy(self, nonlinear_iter, max_iterations, &
                                                                         is_converged, is_diverged, should_continue)
            implicit none
            class(type_iteration_strategy), intent(in) :: self
            integer(int32), intent(in) :: nonlinear_iter
            integer(int32), intent(in) :: max_iterations
            logical, intent(in) :: is_converged
            logical, intent(in) :: is_diverged
            logical, intent(inout) :: should_continue
        end subroutine determine_continuation_iteration_strategy

        !> Assesses whether convergence is stagnating or progressing.
        module subroutine assess_convergence_trend_iteration_strategy(self, current_norm, previous_norm, is_stagnating)
            implicit none
            class(type_iteration_strategy), intent(in) :: self
            real(real64), intent(in) :: current_norm
            real(real64), intent(in) :: previous_norm
            logical, intent(inout) :: is_stagnating
        end subroutine assess_convergence_trend_iteration_strategy
    end interface

end module control_iteration_strategy

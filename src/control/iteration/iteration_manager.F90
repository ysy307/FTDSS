module control_iteration_manager
    use, intrinsic :: iso_fortran_env
    use :: stdlib_strings, only:strip
    use :: module_core
    use :: control_iteration_setting, only:type_iteration_setting
    use :: control_iteration_strategy, only:type_iteration_strategy
    implicit none
    private

    public :: type_iteration

    !>
    !> Iteration manager
    !>
    type :: type_iteration
        ! --- Counters ---
        integer(int32), private :: total_iter = 0
        integer(int32), private :: nonlinear_iter = 0

        ! --- State flags ---
        ! Holds convergence state per physics type (Thermal, Hydro, etc.)
        logical, private :: converged(PHYSICS_TYPES%NUM_ID) = .true.
        logical, private :: diverged(PHYSICS_TYPES%NUM_ID) = .false.

        ! --- Settings ---
        ! nonlinear_solver_type: static setting from input config (NONE/PICARD/NEWTON)
        ! compute_nonlinear_solver_type: dynamic solver type used during computation
        type(type_constant_id), private :: nonlinear_solver_type = NONLINEAR_SOLVER%PICARD
        type(type_constant_id), private :: compute_nonlinear_solver_type = NONLINEAR_SOLVER%PICARD

        type(type_iteration_setting), private :: settings
        type(type_iteration_strategy), private :: strategy
    contains
        ! ---- Lifecycle ----
        procedure, public, pass(self) :: initialize => initialize_type_iteration

        ! ---- Mutator ----
        procedure, public, pass(self) :: reset => reset_iteration
        procedure, public, pass(self) :: increment_nonlinear => increment_nonlinear_iteration
        procedure, public, pass(self) :: increment_total => increment_total_iteration
        procedure, public, pass(self) :: set_nonlinear_solver => set_nonlinear_solver_iteration
        procedure, public, pass(self) :: set_converged => set_converged_iteration
        procedure, public, pass(self) :: set_diverged => set_diverged_iteration

        ! ---- Algorithm / Operation ----
        procedure, public, pass(self) :: check_convergence => check_convergence_iteration

        ! ---- Inquiry ----
        procedure, public, pass(self) :: is_converged => is_converged_iteration
        procedure, public, pass(self) :: is_diverged => is_diverged_iteration
        procedure, public, pass(self) :: is_compute_newton => is_compute_newton_iteration
        procedure, public, pass(self) :: is_compute_picard => is_compute_picard_iteration
        procedure, public, pass(self) :: is_compute_none => is_compute_none_iteration
        procedure, public, pass(self) :: is_newton => is_newton_iteration
        procedure, public, pass(self) :: is_picard => is_picard_iteration
        procedure, public, pass(self) :: is_none => is_none_iteration
        procedure, public, pass(self) :: should_continue => should_continue_iteration

        ! ---- Getter ----
        procedure, public, pass(self) :: get_nonlinear_solver => get_nonlinear_solver_iteration
        procedure, public, pass(self) :: get_nonlinear_iter => get_nonlinear_iter_iteration
        procedure, public, pass(self) :: get_total_iter => get_total_iter_iteration
        procedure, public, pass(self) :: get_max_iterations => get_max_iterations_iteration
        procedure, public, pass(self) :: get_update_frequency => get_update_frequency_iteration
        procedure, public, pass(self) :: get_current_norm => get_current_norm_iteration
        procedure, public, pass(self) :: get_tolerances => get_tolerances_iteration
        ! ---- Meta / Utility ----
    end type type_iteration

contains

    subroutine initialize_type_iteration(self, config, reference_values)
        implicit none
        class(type_iteration), intent(inout) :: self
        type(type_config_iteration), intent(in) :: config
        real(real64), intent(in), optional :: reference_values(:)

        self%total_iter = 0
        self%nonlinear_iter = 0
        self%converged(:) = .true.
        self%diverged(:) = .false.

        self%nonlinear_solver_type = config%nonlinear_solver_type
        self%compute_nonlinear_solver_type = self%nonlinear_solver_type

        if (self%nonlinear_solver_type == NONLINEAR_SOLVER%NONE) then
            return
        end if

        call self%settings%initialize(config%nonlinear, reference_values)

    end subroutine initialize_type_iteration

    subroutine reset_iteration(self)
        implicit none
        class(type_iteration), intent(inout) :: self

        self%nonlinear_iter = 0
        self%converged(:) = .true.
        self%diverged(:) = .false.

        call self%settings%reset()

        call self%strategy%apply_initial_policy(self%nonlinear_solver_type, self%compute_nonlinear_solver_type)
    end subroutine reset_iteration

    subroutine increment_nonlinear_iteration(self)
        implicit none
        class(type_iteration), intent(inout) :: self

        self%nonlinear_iter = self%nonlinear_iter + 1
    end subroutine increment_nonlinear_iteration

    subroutine increment_total_iteration(self)
        implicit none
        class(type_iteration), intent(inout) :: self

        self%total_iter = self%total_iter + 1
    end subroutine increment_total_iteration

    subroutine set_nonlinear_solver_iteration(self, nonlinear_solver_type)
        implicit none
        class(type_iteration), intent(inout) :: self
        type(type_constant_id), intent(in) :: nonlinear_solver_type

        ! Set the dynamic solver type that changes during computation
        self%compute_nonlinear_solver_type = nonlinear_solver_type
    end subroutine set_nonlinear_solver_iteration

    subroutine set_converged_iteration(self, physics_type, converged)
        implicit none
        class(type_iteration), intent(inout) :: self
        type(type_constant_id), intent(in) :: physics_type
        logical, intent(in) :: converged

        if (.not. PHYSICS_TYPES%is_valid(physics_type)) then
            call raise_error(ERROR_CODES%INVALID_TYPE, opt=strip(physics_type%name))
        end if

        self%converged(physics_type%ID) = converged
    end subroutine set_converged_iteration

    subroutine set_diverged_iteration(self, physics_type, diverged)
        implicit none
        class(type_iteration), intent(inout) :: self
        type(type_constant_id), intent(in) :: physics_type
        logical, intent(in) :: diverged

        if (.not. PHYSICS_TYPES%is_valid(physics_type)) then
            call raise_error(ERROR_CODES%INVALID_TYPE, opt=strip(physics_type%name))
        end if

        self%diverged(physics_type%ID) = diverged
    end subroutine set_diverged_iteration

    subroutine check_convergence_iteration(self, physics_type, residual_vector, update_vector)
        implicit none
        class(type_iteration), intent(inout) :: self
        type(type_constant_id), intent(in) :: physics_type
        real(real64), intent(in), optional :: residual_vector(:)
        real(real64), intent(in), optional :: update_vector(:)

        logical :: is_ok

        if (.not. PHYSICS_TYPES%is_valid(physics_type)) then
            call raise_error(ERROR_CODES%INVALID_TYPE, opt=strip(physics_type%name))
        end if

        if (self%is_none()) then
            call self%set_converged(physics_type, .true.)
            call self%set_diverged(physics_type, .false.)
            return
        end if

        is_ok = self%settings%check_convergence(physics_type, self%nonlinear_iter, residual_vector, update_vector)

        ! Update convergence/divergence state from the check result
        call self%set_converged(physics_type, is_ok)
        call self%set_diverged(physics_type, .not. is_ok)
    end subroutine check_convergence_iteration

    pure function is_converged_iteration(self) result(is_converged)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: is_converged

        is_converged = all(self%converged)
    end function is_converged_iteration

    pure function is_diverged_iteration(self) result(is_diverged)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: is_diverged

        is_diverged = any(self%diverged)
    end function is_diverged_iteration

    !> Returns true if the ACTIVE solver is Newton-Raphson
    !> (Returns false if solver is NONE)
    pure function is_compute_newton_iteration(self) result(is_newton)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: is_newton

        is_newton = (self%compute_nonlinear_solver_type == NONLINEAR_SOLVER%NEWTON .or. &
                     self%compute_nonlinear_solver_type == NONLINEAR_SOLVER%MODIFIED_NEWTON)
    end function is_compute_newton_iteration

    !> Returns true if the ACTIVE solver is Picard
    !> (Returns false if solver is NONE)
    pure function is_compute_picard_iteration(self) result(is_picard)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: is_picard

        is_picard = (self%compute_nonlinear_solver_type == NONLINEAR_SOLVER%PICARD .or. &
                     self%compute_nonlinear_solver_type == NONLINEAR_SOLVER%MODIFIED_PICARD)
    end function is_compute_picard_iteration

    !> Returns true if the ACTIVE solver is Newton-Raphson
    !> (Returns false if solver is NONE)
    pure function is_newton_iteration(self) result(is_newton)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: is_newton

        is_newton = (self%nonlinear_solver_type == NONLINEAR_SOLVER%NEWTON .or. &
                     self%nonlinear_solver_type == NONLINEAR_SOLVER%MODIFIED_NEWTON)
    end function is_newton_iteration

    !> Returns true if the ACTIVE solver is NONE
    pure function is_compute_none_iteration(self) result(is_none)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: is_none

        is_none = (self%compute_nonlinear_solver_type == NONLINEAR_SOLVER%NONE)
    end function is_compute_none_iteration

    !> Returns true if the ACTIVE solver is Picard
    !> (Returns false if solver is NONE)
    pure function is_picard_iteration(self) result(is_picard)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: is_picard

        is_picard = (self%nonlinear_solver_type == NONLINEAR_SOLVER%PICARD .or. &
                     self%nonlinear_solver_type == NONLINEAR_SOLVER%MODIFIED_PICARD)
    end function is_picard_iteration

    !> Returns true if the ACTIVE solver is NONE
    pure function is_none_iteration(self) result(is_none)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: is_none

        is_none = (self%nonlinear_solver_type == NONLINEAR_SOLVER%NONE)
    end function is_none_iteration

    pure function should_continue_iteration(self) result(should_continue)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: should_continue

        integer(int32) :: max_iterations

        call self%settings%get_max_iterations(max_iterations)

        call self%strategy%determine_continuation(self%nonlinear_iter, max_iterations, &
                                                  self%is_converged(), self%is_diverged(), should_continue)
    end function should_continue_iteration

    subroutine get_nonlinear_solver_iteration(self, nonlinear_solver_type)
        implicit none
        class(type_iteration), intent(in), target :: self
        type(type_constant_id), intent(inout), pointer :: nonlinear_solver_type

        ! Return the current solver type for discretization routines
        nonlinear_solver_type => self%compute_nonlinear_solver_type
    end subroutine get_nonlinear_solver_iteration

    subroutine get_nonlinear_iter_iteration(self, nonlinear_iter)
        implicit none
        class(type_iteration), intent(in) :: self
        integer(int32), intent(inout) :: nonlinear_iter

        nonlinear_iter = self%nonlinear_iter
    end subroutine get_nonlinear_iter_iteration

    subroutine get_total_iter_iteration(self, total_iter)
        implicit none
        class(type_iteration), intent(in) :: self
        integer(int32), intent(inout) :: total_iter

        total_iter = self%total_iter
    end subroutine get_total_iter_iteration

    subroutine get_max_iterations_iteration(self, max_iterations)
        implicit none
        class(type_iteration), intent(in) :: self
        integer(int32), intent(inout) :: max_iterations

        call self%settings%get_max_iterations(max_iterations)
    end subroutine get_max_iterations_iteration

    subroutine get_update_frequency_iteration(self, update_frequency)
        implicit none
        class(type_iteration), intent(in) :: self
        integer(int32), intent(inout) :: update_frequency

        call self%settings%get_update_frequency(update_frequency)
    end subroutine get_update_frequency_iteration

    subroutine get_current_norm_iteration(self, physics_type, criteria_type, norm_type, current_norm)
        implicit none
        class(type_iteration), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        type(type_constant_id), intent(in) :: criteria_type
        type(type_constant_id), intent(in) :: norm_type
        real(real64), intent(inout) :: current_norm

        call self%settings%get_current_norm(physics_type, criteria_type, norm_type, self%nonlinear_iter, current_norm)
    end subroutine get_current_norm_iteration

    subroutine get_tolerances_iteration(self, physics_type, absolute_tolerance, relative_tolerance)
        implicit none
        class(type_iteration), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        real(real64), intent(inout), optional :: absolute_tolerance
        real(real64), intent(inout), optional :: relative_tolerance

        call self%settings%get_tolerances(physics_type, absolute_tolerance, relative_tolerance)
    end subroutine get_tolerances_iteration

end module control_iteration_manager

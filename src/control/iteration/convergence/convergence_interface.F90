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
        ! ---- Algorithm / Operation ----
        procedure, public, pass(self) :: check => check_convergence_criterion
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
    contains
        ! ---- Lifecycle ----
        procedure, public, pass(self) :: initialize => initialize_convergence_control
        ! ---- Mutator ----
        procedure, public, pass(self) :: reset => reset_convergence_control
        ! ---- Algorithm / Operation ----
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

        module pure function is_initialized_convergence_control(self) result(is_initialized)
            implicit none
            class(type_convergence_control), intent(in) :: self
            logical :: is_initialized

        end function is_initialized_convergence_control

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

    end interface

end module control_iteration_convergence

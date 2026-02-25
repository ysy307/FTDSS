module control_iteration_setting
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: control_iteration_convergence, only:type_convergence_control
    implicit none
    private

    public :: type_iteration_setting

    !>
    !> Iterator configuration container
    !>
    type :: type_iteration_setting
        !> Maximum number of iterations
        integer(int32), private :: max_iterations = 10
        !> Frequency of updating system matrices
        integer(int32), private :: update_frequency = 1
        !> Convergence control settings
        type(type_convergence_control), private :: convergence_control
    contains
        ! ---- Lifecycle ----
        procedure, public, pass(self) :: initialize => initialize_type_iteration_setting
        ! ---- Mutator ----
        procedure, public, pass(self) :: reset => reset_iteration_setting
        ! ---- Algorithm / Operation ----
        procedure, public, pass(self) :: check_convergence => check_convergence_setting
        ! ---- Inquiry ----
        ! ---- Getter ----
        procedure, public, pass(self) :: get_max_iterations => get_max_iterations_iteration_setting
        procedure, public, pass(self) :: get_update_frequency => get_update_frequency_iteration_setting
        procedure, public, pass(self) :: get_current_norm => get_current_norm_iteration_setting
        procedure, public, pass(self) :: get_tolerances => get_tolerances_iteration_setting
        ! ---- Meta / Utility ----
    end type type_iteration_setting

contains

    subroutine initialize_type_iteration_setting(self, config, reference_values)
        implicit none
        class(type_iteration_setting), intent(inout) :: self
        type(type_config_iteration_nonlinear), intent(in) :: config
        real(real64), intent(in), optional :: reference_values(:)

        self%max_iterations = config%max_iterations
        self%update_frequency = config%update_frequency

        call self%convergence_control%initialize(config, self%max_iterations, reference_values)

    end subroutine initialize_type_iteration_setting

    subroutine reset_iteration_setting(self)
        implicit none
        class(type_iteration_setting), intent(inout) :: self

        call self%convergence_control%reset()

    end subroutine reset_iteration_setting

    function check_convergence_setting(self, physics_type, nonlinear_iter, residual_vector, update_vector) result(is_ok)
        implicit none
        class(type_iteration_setting), intent(inout) :: self
        type(type_constant_id), intent(in) :: physics_type
        integer(int32), intent(in) :: nonlinear_iter
        real(real64), intent(in), optional :: residual_vector(:)
        real(real64), intent(in), optional :: update_vector(:)
        logical :: is_ok

        is_ok = self%convergence_control%check_convergence(physics_type, nonlinear_iter, residual_vector, update_vector)

    end function check_convergence_setting

    pure subroutine get_max_iterations_iteration_setting(self, max_iterations)
        implicit none
        class(type_iteration_setting), intent(in) :: self
        integer(int32), intent(inout) :: max_iterations

        max_iterations = self%max_iterations
    end subroutine get_max_iterations_iteration_setting

    pure subroutine get_update_frequency_iteration_setting(self, update_frequency)
        implicit none
        class(type_iteration_setting), intent(in) :: self
        integer(int32), intent(inout) :: update_frequency

        update_frequency = self%update_frequency
    end subroutine get_update_frequency_iteration_setting

    subroutine get_current_norm_iteration_setting(self, physics_type, criteria_type, norm_type, nonlinear_iter, current_norm)
        implicit none
        class(type_iteration_setting), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        type(type_constant_id), intent(in) :: criteria_type
        type(type_constant_id), intent(in) :: norm_type
        integer(int32), intent(in) :: nonlinear_iter
        real(real64), intent(inout) :: current_norm

        call self%convergence_control%get_current_norm(physics_type, criteria_type, norm_type, nonlinear_iter, current_norm)
    end subroutine get_current_norm_iteration_setting

    subroutine get_tolerances_iteration_setting(self, physics_type, absolute_tolerance, relative_tolerance)
        implicit none
        class(type_iteration_setting), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        real(real64), intent(inout), optional :: absolute_tolerance
        real(real64), intent(inout), optional :: relative_tolerance

        call self%convergence_control%get_tolerances(physics_type, absolute_tolerance, relative_tolerance)
    end subroutine get_tolerances_iteration_setting

end module control_iteration_setting

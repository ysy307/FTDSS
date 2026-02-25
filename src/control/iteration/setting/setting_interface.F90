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

    function evaluate_convergence_setting(self, physics_type, nonlinear_iter, &
                                          residual_vector, update_vector) result(is_ok)
        implicit none
        class(type_iteration_setting), intent(inout) :: self
        type(type_constant_id), intent(in) :: physics_type
        integer(int32), intent(in) :: nonlinear_iter
        real(real64), intent(in), optional :: residual_vector(:)
        real(real64), intent(in), optional :: update_vector(:)
        logical :: is_ok

        logical :: is_residual_ok, is_update_ok
        logical :: check_residual, check_update

        check_residual = self%convergence_control%should_check_residual()
        check_update = self%convergence_control%should_check_update()

        is_residual_ok = .true.
        is_update_ok = .true.

        associate (control => self%convergence_control)
            ! --- Residual vector check ---
            if (present(residual_vector)) then
                write (*, '(A, i0)') '    [Debug] Checking residual convergence for physics type ID: ', physics_type%ID
                is_residual_ok = control%residual(physics_type%ID)%check( &
                                 residual_vector, nonlinear_iter, control%norm_type)
                if (.not. check_residual) is_residual_ok = .true.
            else
                is_residual_ok = .not. check_residual
            end if

            ! --- Update vector check ---
            if (present(update_vector)) then
                write (*, '(A, i0)') '    [Debug] Checking update convergence for physics type ID: ', physics_type%ID
                is_update_ok = control%update(physics_type%ID)%check( &
                               update_vector, nonlinear_iter, control%norm_type)
                if (.not. check_update) is_update_ok = .true.
            else
                is_update_ok = .not. check_update
            end if

            ! --- Combine Logic (AND / OR) ---
            if (control%combination_logic == NONLINEAR_LOGIC%OR) then
                is_ok = is_residual_ok .or. is_update_ok
            else ! Default AND
                is_ok = is_residual_ok .and. is_update_ok
            end if
        end associate
    end function evaluate_convergence_setting

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

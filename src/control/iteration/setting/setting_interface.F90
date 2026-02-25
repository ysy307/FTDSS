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
        procedure, public, pass(self) :: initialize => initialize_type_iteration_setting
        procedure, public, pass(self) :: reset => reset_iteration_setting
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

end module control_iteration_setting

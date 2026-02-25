submodule(control_iteration_convergence) convergence_control
    implicit none
contains

    module subroutine initialize_convergence_control(self, config, max_iterations, reference_values)
        implicit none
        class(type_convergence_control), intent(inout) :: self
        type(type_config_iteration_nonlinear), intent(in) :: config
        integer(int32), intent(in) :: max_iterations
        real(real64), intent(in), optional :: reference_values(:)

        integer(int32) :: i
        logical :: check_res, check_upd

        self%norm_type = config%norm_type
        self%combination_logic = config%combination_logic
        self%convergence_norm_type = config%convergence_norm_type

        check_res = self%should_check_residual()
        check_upd = self%should_check_update()

        do i = 1, PHYSICS_TYPES%NUM_ID
            if (present(reference_values)) then
                call self%residual(i)%initialize(config%residual(i), &
                                                 check_res, &
                                                 max_iterations, &
                                                 reference_values(i))
                call self%update(i)%initialize(config%update(i), &
                                               check_upd, &
                                               max_iterations, &
                                               reference_values(i))
            else
                call self%residual(i)%initialize(config%residual(i), &
                                                 check_res, &
                                                 max_iterations)
                call self%update(i)%initialize(config%update(i), &
                                               check_upd, &
                                               max_iterations)
            end if
        end do

    end subroutine initialize_convergence_control

    module subroutine reset_convergence_control(self)
        implicit none
        class(type_convergence_control), intent(inout) :: self

        integer(int32) :: i

        do i = 1, PHYSICS_TYPES%NUM_ID
            call self%residual(i)%reset()
            call self%update(i)%reset()
        end do

    end subroutine reset_convergence_control

    module pure function should_check_residual_convergence_control(self) result(should_check)
        implicit none
        class(type_convergence_control), intent(in) :: self
        logical :: should_check

        if (self%convergence_norm_type == NONLINEAR_NORM_CRITERIA%RESIDUAL .or. &
            self%convergence_norm_type == NONLINEAR_NORM_CRITERIA%BOTH) then
            should_check = .true.
        else
            should_check = .false.
        end if
    end function should_check_residual_convergence_control

    module pure function should_check_update_convergence_control(self) result(should_check)
        implicit none
        class(type_convergence_control), intent(in) :: self
        logical :: should_check

        if (self%convergence_norm_type == NONLINEAR_NORM_CRITERIA%UPDATE .or. &
            self%convergence_norm_type == NONLINEAR_NORM_CRITERIA%BOTH) then
            should_check = .true.
        else
            should_check = .false.
        end if
    end function should_check_update_convergence_control

    module pure function is_initialized_convergence_control(self) result(is_initialized)
        implicit none
        class(type_convergence_control), intent(in) :: self
        logical :: is_initialized

        is_initialized = self%initialized
    end function is_initialized_convergence_control

    module subroutine get_norm_type_convergence_control(self, norm_type)
        implicit none
        class(type_convergence_control), intent(in), target :: self
        type(type_constant_id), intent(inout), pointer :: norm_type

        norm_type => self%norm_type
    end subroutine get_norm_type_convergence_control

    module subroutine get_combination_logic_convergence_control(self, combination_logic)
        implicit none
        class(type_convergence_control), intent(in), target :: self
        type(type_constant_id), intent(inout), pointer :: combination_logic

        combination_logic => self%combination_logic
    end subroutine get_combination_logic_convergence_control

    module subroutine get_convergence_norm_type_convergence_control(self, convergence_norm_type)
        implicit none
        class(type_convergence_control), intent(in), target :: self
        type(type_constant_id), intent(inout), pointer :: convergence_norm_type

        convergence_norm_type => self%convergence_norm_type
    end subroutine get_convergence_norm_type_convergence_control
end submodule convergence_control

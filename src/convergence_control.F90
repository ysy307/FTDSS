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

    module function check_convergence_control(self, physics_type, nonlinear_iter, residual_vector, update_vector) result(is_ok)
        implicit none
        class(type_convergence_control), intent(inout) :: self
        type(type_constant_id), intent(in) :: physics_type
        integer(int32), intent(in) :: nonlinear_iter
        real(real64), intent(in), optional :: residual_vector(:)
        real(real64), intent(in), optional :: update_vector(:)
        logical :: is_ok

        logical :: is_residual_ok, is_update_ok
        logical :: check_residual, check_update

        check_residual = self%should_check_residual()
        check_update = self%should_check_update()

        is_residual_ok = .true.
        is_update_ok = .true.

        ! --- Residual vector check ---
        if (present(residual_vector)) then
            is_residual_ok = self%residual(physics_type%ID)%check_convergence(residual_vector, nonlinear_iter, self%norm_type)
            if (.not. check_residual) is_residual_ok = .true.
        else
            is_residual_ok = .not. check_residual
        end if

        ! --- Update vector check ---
        if (present(update_vector)) then
            is_update_ok = self%update(physics_type%ID)%check_convergence(update_vector, nonlinear_iter, self%norm_type)
            if (.not. check_update) is_update_ok = .true.
        else
            is_update_ok = .not. check_update
        end if

        ! --- Combine Logic (AND / OR) ---
        if (self%combination_logic == NONLINEAR_LOGIC%OR) then
            is_ok = is_residual_ok .or. is_update_ok
        else ! Default AND
            is_ok = is_residual_ok .and. is_update_ok
        end if
    end function check_convergence_control

    module pure function is_initialized_convergence_control(self) result(is_initialized)
        implicit none
        class(type_convergence_control), intent(in) :: self
        logical :: is_initialized

        is_initialized = self%initialized
    end function is_initialized_convergence_control

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

    module subroutine get_current_norm_convergence_control(self, physics_type, criteria_type, &
                                                           norm_type, nonlinear_iter, current_norm)
        implicit none
        class(type_convergence_control), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        type(type_constant_id), intent(in) :: criteria_type
        type(type_constant_id), intent(in) :: norm_type
        integer(int32), intent(in) :: nonlinear_iter
        real(real64), intent(inout) :: current_norm

        if (.not. PHYSICS_TYPES%is_valid(physics_type)) then
            current_norm = 0.0d0
            return
        end if

        if (criteria_type == NONLINEAR_NORM_CRITERIA%RESIDUAL) then
            call self%residual(physics_type%ID)%get_current_norm(norm_type, nonlinear_iter, current_norm)
        else if (criteria_type == NONLINEAR_NORM_CRITERIA%UPDATE) then
            call self%update(physics_type%ID)%get_current_norm(norm_type, nonlinear_iter, current_norm)
        else
            current_norm = 0.0d0
        end if

    end subroutine get_current_norm_convergence_control

    module subroutine get_tolerances_convergence_control(self, physics_type, absolute_tolerance, relative_tolerance)
        implicit none
        class(type_convergence_control), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        real(real64), intent(inout), optional :: absolute_tolerance
        real(real64), intent(inout), optional :: relative_tolerance

        if (.not. PHYSICS_TYPES%is_valid(physics_type)) then
            if (present(absolute_tolerance)) then
                absolute_tolerance = 0.0d0
            end if
            if (present(relative_tolerance)) then
                relative_tolerance = 0.0d0
            end if
            return
        end if

        call self%residual(physics_type%ID)%get_tolerances(absolute_tolerance, relative_tolerance)

    end subroutine get_tolerances_convergence_control
end submodule convergence_control

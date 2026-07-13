submodule(control_iteration_convergence) convergence_criterion
    implicit none
contains

    !> Initialize convergence criterion
    module subroutine initialize_type_convergence_criterion(self, config, should_check, max_iterations, reference_value)
        implicit none
        !> Initialize the convergence criterion with specified parameters.
        class(type_convergence_criterion), intent(inout) :: self
        !> Configuration for the convergence criterion
        type(type_config_iteration_criterion), intent(in) :: config
        !> Whether to check convergence for this criterion
        logical, intent(in) :: should_check
        !> Maximum number of iterations for allocating norm history
        integer(int32), intent(in) :: max_iterations
        !> Reference value for relative error calculation
        real(real64), intent(in), optional :: reference_value

        self%should_check = should_check
        self%criterion = config%criterion
        self%absolute_tolerance = config%absolute_tolerance
        self%relative_tolerance = config%relative_tolerance

        self%max_iterations = max_iterations

        if (present(reference_value)) then
            if (abs(reference_value) < 1.0d-6) then
                self%reference_value = 1.0d0
            else
                self%reference_value = reference_value
            end if
        else
            self%reference_value = 1.0d0
        end if

        call deallocate_array(self%norms_history)
        call allocate_array(self%norms_history, NORM_TYPES%NUM_ID, max(self%max_iterations, 1))
        self%norms_history = 0.0d0

        self%initialized = .true.
    end subroutine initialize_type_convergence_criterion

    module subroutine destroy_type_convergence_criterion(self)
        implicit none
        class(type_convergence_criterion), intent(inout) :: self

        call deallocate_array(self%norms_history)
        self%initialized = .false.
    end subroutine destroy_type_convergence_criterion

    module subroutine reset_criterion(self)
        implicit none
        class(type_convergence_criterion), intent(inout) :: self

        if (allocated(self%norms_history)) then
            self%norms_history = 0.0d0
        end if
    end subroutine reset_criterion

    !> Calculate norms, store them in history, and check convergence criteria.
    module function check_convergence_criterion(self, vector, iter, norm_type) result(is_ok)
        implicit none
        class(type_convergence_criterion), intent(inout) :: self
        real(real64), intent(in) :: vector(:)
        integer(int32), intent(in) :: iter
        type(type_constant_id), intent(in) :: norm_type
        logical :: is_ok

        real(real64) :: current_norm
        logical :: abs_ok, rel_ok

        is_ok = .false.

        if (iter >= 1 .and. iter <= self%max_iterations) then
            self%norms_history(NORM_TYPES%L1%ID, iter) = vector_norm1(vector)
            self%norms_history(NORM_TYPES%L2%ID, iter) = vector_norm2(vector)
            self%norms_history(NORM_TYPES%LINF%ID, iter) = vector_norminf(vector)

            write (*, '(A, I6, A, ES13.6, A, ES13.6, A, ES13.6)') '    [Debug] Iteration:', iter, ' Norms - L1:', &
                self%norms_history(NORM_TYPES%L1%ID, iter), &
                ' L2:', self%norms_history(NORM_TYPES%L2%ID, iter), &
                ' LInf:', self%norms_history(NORM_TYPES%LINF%ID, iter)
        end if

        if (.not. self%should_check) then
            is_ok = .true.
            return
        end if

        if (self%criterion == NONLINEAR_CRITERIA%none) then
            is_ok = .true.
            return
        end if

        current_norm = self%norms_history(norm_type%ID, iter)

        abs_ok = (current_norm < self%absolute_tolerance)
        rel_ok = (current_norm / self%reference_value < self%relative_tolerance)

        if (self%criterion == NONLINEAR_CRITERIA%ABSOLUTE) then
            is_ok = abs_ok
        else if (self%criterion == NONLINEAR_CRITERIA%RELATIVE) then
            is_ok = rel_ok
        else if (self%criterion == NONLINEAR_CRITERIA%BOTH) then
            is_ok = abs_ok .and. rel_ok
        else
            is_ok = abs_ok
        end if

    end function check_convergence_criterion

    module subroutine get_current_norm_convergence_criterion(self, norm_type, nonlinear_iter, current_norm)
        implicit none
        class(type_convergence_criterion), intent(in) :: self
        type(type_constant_id), intent(in) :: norm_type
        integer(int32), intent(in) :: nonlinear_iter
        real(real64), intent(inout) :: current_norm

        current_norm = 0.0d0
        if (.not. NORM_TYPES%is_valid(norm_type)) return

        if (nonlinear_iter >= 1 .and. nonlinear_iter <= self%max_iterations) then
            current_norm = self%norms_history(norm_type%ID, nonlinear_iter)
        end if

    end subroutine get_current_norm_convergence_criterion

    module subroutine get_tolerances_convergence_criterion(self, absolute_tolerance, relative_tolerance)
        implicit none
        class(type_convergence_criterion), intent(in) :: self
        real(real64), intent(inout), optional :: absolute_tolerance
        real(real64), intent(inout), optional :: relative_tolerance

        if (present(absolute_tolerance)) then
            absolute_tolerance = self%absolute_tolerance
        end if

        if (present(relative_tolerance)) then
            relative_tolerance = self%relative_tolerance
        end if

    end subroutine get_tolerances_convergence_criterion

    !> Update the reference value for relative error normalization.
    !> Typically called per time step with a characteristic scale of the physical variable.
    module subroutine update_reference_value_type_convergence_criterion(self, reference_value)
        implicit none
        class(type_convergence_criterion), intent(inout) :: self
        real(real64), intent(in) :: reference_value

        if (abs(reference_value) < 1.0d-6) then
            self%reference_value = 1.0d0
        else
            self%reference_value = abs(reference_value)
        end if

    end subroutine update_reference_value_type_convergence_criterion

end submodule convergence_criterion

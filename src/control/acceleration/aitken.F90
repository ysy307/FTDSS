!> Implementation overview
!>
!> Algorithm:
!> - Aitken's delta-squared process for dynamic relaxation
!>
!> Numerical considerations:
!> - Relaxation factor is bounded by configuration limits to prevent instability
!> - Fallback to previous relaxation factor if denominator is too small
!>
!> Memory usage:
!> - Stores one previous increment vector per physics type
submodule(control_acceleration) acceleration_aitken
    implicit none
contains

    module subroutine initialize_acceleration_aitken(self, config)
        implicit none
        !> Aitken acceleration object
        class(type_acceleration_aitken), intent(inout) :: self
        !> Configuration parameters
        type(type_config_acceleration), intent(in) :: config

        self%method = config%method
        self%max_relaxation = config%max_relaxation
        self%min_relaxation = config%min_relaxation

        if (allocated(self%du_raw)) deallocate (self%du_raw)
        call allocate_array(self%du_raw, config%num_dofs, PHYSICS_TYPES%NUM_ID)

        call self%reset()

        self%initialized = .true.
    end subroutine initialize_acceleration_aitken

    module subroutine destroy_acceleration_aitken(self)
        implicit none
        !> Aitken acceleration object
        class(type_acceleration_aitken), intent(inout) :: self

        self%method = type_constant_id("", "", -1)
        self%max_relaxation = 0.0d0
        self%min_relaxation = 0.0d0
        call deallocate_array(self%du_raw)
        self%relaxation_factor(:) = 0.0d0
        self%previous_relaxation_factor(:) = 0.0d0
        self%initialized = .false.
    end subroutine destroy_acceleration_aitken

    module subroutine compute_relaxation_acceleration_aitken(self, physics_type, iter, du, vec)
        implicit none
        !> Aitken acceleration object
        class(type_acceleration_aitken), intent(inout) :: self
        !> Identifier for the physics type
        type(type_constant_id), intent(in) :: physics_type
        !> Current iteration number
        integer(int32), intent(in) :: iter
        !> Increment vector \(\Delta u_k\)
        real(real64), intent(in) :: du(:)
        !> State vector \(u_k\) on entry
        !> Overwritten by updated vector \(u_{k+1}\) on exit
        real(real64), intent(inout) :: vec(:)

        integer(int32) :: pid
        real(real64) :: numerator
        real(real64) :: denominator
        real(real64) :: omega

        pid = physics_type%ID

        if (iter > 1) then
            numerator = vector_dot((du - self%du_raw(:, pid)), self%du_raw(:, pid))
            denominator = vector_dot(du - self%du_raw(:, pid), du - self%du_raw(:, pid))

            if (denominator > epsilon(1.0d0)) then
                omega = -self%previous_relaxation_factor(pid) * (numerator / denominator)

                if (omega < self%min_relaxation) then
                    omega = self%min_relaxation
                else if (omega > self%max_relaxation) then
                    omega = self%max_relaxation
                end if

                self%relaxation_factor(pid) = omega
                self%previous_relaxation_factor(pid) = omega
            else
                omega = self%previous_relaxation_factor(pid)
            end if
        else
            omega = 1.0d0
            self%relaxation_factor(pid) = omega
        end if

        vec(:) = vec(:) + omega * du(:)

        self%du_raw(:, pid) = du(:)
    end subroutine compute_relaxation_acceleration_aitken

    module subroutine reset_acceleration_aitken(self)
        implicit none
        !> Aitken acceleration object
        class(type_acceleration_aitken), intent(inout) :: self

        self%du_raw(:, :) = 0.0d0
        self%relaxation_factor(:) = 1.0d0
        self%previous_relaxation_factor(:) = 0.0d0
    end subroutine reset_acceleration_aitken

    module pure function reach_minimum_relaxation_aitken(self, physics_type) result(reached)
        implicit none
        !> Aitken acceleration object
        class(type_acceleration_aitken), intent(in) :: self
        !> Identifier for the physics type
        type(type_constant_id), intent(in) :: physics_type
        !> Flag indicating if minimum relaxation is reached
        logical :: reached

        reached = self%relaxation_factor(physics_type%ID) <= self%min_relaxation
    end function reach_minimum_relaxation_aitken

    module pure function reach_maximum_relaxation_aitken(self, physics_type) result(reached)
        implicit none
        !> Aitken acceleration object
        class(type_acceleration_aitken), intent(in) :: self
        !> Identifier for the physics type
        type(type_constant_id), intent(in) :: physics_type
        !> Flag indicating if maximum relaxation is reached
        logical :: reached

        reached = self%relaxation_factor(physics_type%ID) >= self%max_relaxation
    end function reach_maximum_relaxation_aitken

    module subroutine get_current_relaxation_aitken(self, physics_type, relaxation)
        implicit none
        !> Aitken acceleration object
        class(type_acceleration_aitken), intent(in) :: self
        !> Identifier for the physics type
        type(type_constant_id), intent(in) :: physics_type
        !> Current relaxation factor
        real(real64), intent(inout) :: relaxation

        relaxation = self%relaxation_factor(physics_type%ID)
    end subroutine get_current_relaxation_aitken

    module subroutine get_previous_relaxation_aitken(self, physics_type, relaxation)
        implicit none
        !> Aitken acceleration object
        class(type_acceleration_aitken), intent(in) :: self
        !> Identifier for the physics type
        type(type_constant_id), intent(in) :: physics_type
        !> Previous relaxation factor
        real(real64), intent(inout) :: relaxation

        relaxation = self%previous_relaxation_factor(physics_type%ID)
    end subroutine get_previous_relaxation_aitken

end submodule acceleration_aitken

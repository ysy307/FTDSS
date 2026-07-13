!> Anderson acceleration AA(1) for nonlinear fixed-point iterations
!>
!> Algorithm:
!> - For iter=1: full step, store increment and Picard iterate
!> - For iter>=2: minimize ||f_k - theta*(f_k - f_{k-1})||^2
!>   where f_k = G(x_k) - x_k is the fixed-point residual (increment)
!>   Then x_{k+1} = (1 - theta)*g_k + theta*g_{k-1}
!>
!> Numerical considerations:
!> - Falls back to damped step if denominator is too small
!> - Relaxation factor (beta) applied to the mixed iterate for additional damping
!>
!> Memory usage:
!> - Stores one previous increment vector and one previous Picard iterate per physics type
submodule(control_acceleration) acceleration_anderson
    implicit none
contains

    module subroutine initialize_acceleration_anderson(self, config)
        implicit none
        class(type_acceleration_anderson), intent(inout) :: self
        type(type_config_acceleration), intent(in) :: config

        self%method = config%method
        self%max_relaxation = config%max_relaxation
        self%min_relaxation = config%min_relaxation

        if (allocated(self%du_prev)) deallocate (self%du_prev)
        call allocate_array(self%du_prev, config%num_dofs, PHYSICS_TYPES%NUM_ID)

        if (allocated(self%g_prev)) deallocate (self%g_prev)
        call allocate_array(self%g_prev, config%num_dofs, PHYSICS_TYPES%NUM_ID)

        call self%reset()

        self%initialized = .true.
    end subroutine initialize_acceleration_anderson

    module subroutine destroy_acceleration_anderson(self)
        implicit none
        class(type_acceleration_anderson), intent(inout) :: self

        self%method = type_constant_id("", "", -1)
        self%max_relaxation = 0.0d0
        self%min_relaxation = 0.0d0
        call deallocate_array(self%du_prev)
        call deallocate_array(self%g_prev)
        self%relaxation_factor(:) = 0.0d0
        self%previous_relaxation_factor(:) = 0.0d0
        self%initialized = .false.
    end subroutine destroy_acceleration_anderson

    module subroutine compute_relaxation_acceleration_anderson(self, physics_type, iter, du, vec)
        implicit none
        class(type_acceleration_anderson), intent(inout) :: self
        type(type_constant_id), intent(in) :: physics_type
        integer(int32), intent(in) :: iter
        real(real64), intent(in) :: du(:)
        real(real64), intent(inout) :: vec(:)

        integer(int32) :: pid, n
        real(real64) :: numerator, denominator, theta
        real(real64) :: beta
        real(real64), allocatable :: g_k(:), delta_f(:)

        pid = physics_type%ID
        n = size(du)
        beta = self%max_relaxation

        if (iter == 1) then
            ! Full step (with relaxation beta)
            vec(:) = vec(:) + beta * du(:)
            self%du_prev(:n, pid) = du(:)
            self%g_prev(:n, pid) = vec(:)
            self%relaxation_factor(pid) = beta
        else
            ! Compute current Picard iterate
            allocate (g_k(n))
            g_k(:) = vec(:) + du(:)

            ! Compute delta_f = f_k - f_{k-1} = du - du_prev
            allocate (delta_f(n))
            delta_f(:) = du(:) - self%du_prev(:n, pid)

            ! Solve 1D least-squares: theta = <du, delta_f> / <delta_f, delta_f>
            numerator = vector_dot(du, delta_f)
            denominator = vector_dot(delta_f, delta_f)

            if (denominator > epsilon(1.0d0)) then
                theta = numerator / denominator
                ! Clamp theta to [0, 1] for stability
                theta = max(0.0d0, min(1.0d0, theta))
            else
                ! delta_f ~ 0 means converged or identical iterates; use full step
                theta = 0.0d0
            end if

            ! Anderson mix: x_{k+1} = (1-theta)*g_k + theta*g_{k-1}
            ! With additional relaxation beta:
            ! x_{k+1} = vec + beta * ((1-theta)*du + theta*du_prev) + theta*(vec_prev_picard - vec)
            ! Simpler: direct mixing of Picard iterates
            vec(:) = (1.0d0 - theta) * g_k(:) + theta * self%g_prev(:n, pid)

            ! Apply additional damping if beta < 1
            if (beta < 1.0d0) then
                ! x_{k+1} = (1-beta)*x_k_original + beta*x_mixed
                ! But we already overwrote vec, so use: vec = vec_orig + beta*(vec_mixed - vec_orig)
                ! Since vec_orig = g_k - du and vec_mixed is current vec:
                vec(:) = (g_k(:) - du(:)) + beta * (vec(:) - (g_k(:) - du(:)))
            end if

            self%relaxation_factor(pid) = 1.0d0 - theta
            self%previous_relaxation_factor(pid) = self%relaxation_factor(pid)

            ! Store for next iteration
            self%du_prev(:n, pid) = du(:)
            self%g_prev(:n, pid) = g_k(:)

            deallocate (g_k, delta_f)
        end if
    end subroutine compute_relaxation_acceleration_anderson

    module subroutine reset_acceleration_anderson(self)
        implicit none
        class(type_acceleration_anderson), intent(inout) :: self

        if (allocated(self%du_prev)) self%du_prev(:, :) = 0.0d0
        if (allocated(self%g_prev)) self%g_prev(:, :) = 0.0d0
        self%relaxation_factor(:) = 1.0d0
        self%previous_relaxation_factor(:) = 0.0d0
    end subroutine reset_acceleration_anderson

    module pure function reach_minimum_relaxation_anderson(self, physics_type) result(reached)
        implicit none
        class(type_acceleration_anderson), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        logical :: reached

        reached = self%relaxation_factor(physics_type%ID) <= self%min_relaxation
    end function reach_minimum_relaxation_anderson

    module pure function reach_maximum_relaxation_anderson(self, physics_type) result(reached)
        implicit none
        class(type_acceleration_anderson), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        logical :: reached

        reached = self%relaxation_factor(physics_type%ID) >= self%max_relaxation
    end function reach_maximum_relaxation_anderson

    module subroutine get_current_relaxation_anderson(self, physics_type, relaxation)
        implicit none
        class(type_acceleration_anderson), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        real(real64), intent(inout) :: relaxation

        relaxation = self%relaxation_factor(physics_type%ID)
    end subroutine get_current_relaxation_anderson

    module subroutine get_previous_relaxation_anderson(self, physics_type, relaxation)
        implicit none
        class(type_acceleration_anderson), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        real(real64), intent(inout) :: relaxation

        relaxation = self%previous_relaxation_factor(physics_type%ID)
    end subroutine get_previous_relaxation_anderson

end submodule acceleration_anderson

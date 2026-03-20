!> GPBiCG (Generalized Product-type BiCG) solver.
!> Combines BiCG with a flexible polynomial to smooth convergence.
!> Reference: Zhang (1997).
submodule(numerical_solver_interface) impl_solve_type_solver_gpbicg
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none
contains

    module subroutine initialize_type_solver_gpbicg(self, solver_settings, preconditioner_settings)
        implicit none
        class(type_solver_gpbicg), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        integer(int32) :: ierr

        self%ID = solver_settings%ID
        self%name = "GPBiCG"
        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%max_iterations = solver_settings%max_iterations

        call self%r%initialize(self%num_nodes)
        call self%r0%initialize(self%num_nodes)
        call self%p%initialize(self%num_nodes)
        call self%t%initialize(self%num_nodes)
        call self%u%initialize(self%num_nodes)
        call self%w%initialize(self%num_nodes)
        call self%z%initialize(self%num_nodes)
        call self%Ap%initialize(self%num_nodes)
        call self%At%initialize(self%num_nodes)
        call self%residual_history%initialize(self%max_iterations)
        self%current_iteration = 0

        self%status = SOLVER_STATUS%SUCCESS%ID
        call create_preconditioner(self%pc, preconditioner_settings, ierr)
        self%status = ierr

    end subroutine initialize_type_solver_gpbicg

    module subroutine solve_type_solver_gpbicg(self, A, b, x)
        implicit none
        class(type_solver_gpbicg), intent(inout) :: self
        class(abst_matrix), intent(in) :: A
        type(type_vector_dp), intent(in) :: b
        type(type_vector_dp), intent(inout) :: x

        real(real64) :: rho, rho_old, alpha, beta, omega, resid, resid0
        real(real64) :: sigma, mu, nu, tau_val, zeta, eta
        integer(int32) :: iter, ierr

        call self%residual_history%zero()
        call self%pc%setup(A)

        ! r = b - Ax
        call matvec(A, x, self%r, ierr)
        call vector_axpyz(-1.0d0, self%r, b, self%r)

        resid0 = vector_norm2(self%r)
        call self%residual_history%set(MATRIX_OPS%INS, 1, resid0)
        if (resid0 < self%tolerance) then
            self%current_iteration = 0
            self%status = SOLVER_STATUS%SUCCESS%ID
            return
        end if

        call self%r0%copy(self%r)
        call self%p%copy(self%r)
        call self%t%zero()
        call self%u%zero()

        rho = vector_dot(self%r0, self%r)

        do iter = 1, self%max_iterations
            self%current_iteration = iter

            ! z = M^{-1} p
            call self%pc%apply(self%p, self%z)
            ! Ap = A * z
            call matvec(A, self%z, self%Ap, ierr)

            sigma = vector_dot(self%r0, self%Ap)
            if (abs(sigma) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            alpha = rho / sigma

            ! u = r - alpha * Ap (intermediate residual, like s in BiCGSTAB)
            call vector_axpyz(-alpha, self%Ap, self%r, self%u)

            ! t = M^{-1} u
            call self%pc%apply(self%u, self%w)
            ! At = A * w
            call matvec(A, self%w, self%At, ierr)

            ! omega = (At, u) / (At, At)
            tau_val = vector_dot(self%At, self%At)
            if (abs(tau_val) <= tiny(1.0d0)) then
                ! Fallback: just use alpha step
                call vector_axpy(alpha, self%z, x)
                call self%r%copy(self%u)
                resid = vector_norm2(self%r)
                call self%residual_history%set(MATRIX_OPS%INS, iter, resid)
                if (resid < self%tolerance) then
                    self%status = SOLVER_STATUS%SUCCESS%ID
                    return
                end if
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            omega = vector_dot(self%At, self%u) / tau_val

            ! x = x + alpha * z + omega * w
            call vector_axpy(alpha, self%z, x)
            call vector_axpy(omega, self%w, x)

            ! r = u - omega * At
            call vector_axpyz(-omega, self%At, self%u, self%r)

            resid = vector_norm2(self%r)
            call self%residual_history%set(MATRIX_OPS%INS, iter, resid)

            if (resid < self%tolerance .or. &
                (resid0 > self%tolerance .and. resid < self%relative_tolerance * resid0)) then
                self%status = SOLVER_STATUS%SUCCESS%ID
                return
            end if

            rho_old = rho
            rho = vector_dot(self%r0, self%r)

            if (abs(rho_old) <= tiny(1.0d0) .or. abs(omega) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            beta = (rho / rho_old) * (alpha / omega)

            ! p = r + beta * (p - omega * Ap)
            call vector_axpy(-omega, self%Ap, self%p)
            call vector_scale(beta, self%p)
            call vector_axpy(1.0d0, self%r, self%p)

            if (was_interrupted()) stop
        end do

        self%status = SOLVER_STATUS%MAXITER%ID

    end subroutine solve_type_solver_gpbicg

    module subroutine destroy_type_solver_gpbicg(self)
        implicit none
        class(type_solver_gpbicg), intent(inout) :: self

        self%ID = -1
        if (allocated(self%name)) deallocate (self%name)
        call self%r%destroy()
        call self%r0%destroy()
        call self%p%destroy()
        call self%t%destroy()
        call self%u%destroy()
        call self%w%destroy()
        call self%z%destroy()
        call self%Ap%destroy()
        call self%At%destroy()
        if (allocated(self%pc)) then
            call self%pc%destroy()
            deallocate (self%pc)
        end if
        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine destroy_type_solver_gpbicg

end submodule impl_solve_type_solver_gpbicg

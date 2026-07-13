!> Conjugate Gradient Squared (CGS) solver.
!> Transpose-free variant of BiCG. Convergence can be irregular.
submodule(numerical_solver_interface) impl_solve_type_solver_cgs
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none
contains

    module subroutine initialize_type_solver_cgs(self, solver_settings, preconditioner_settings)
        implicit none
        class(type_solver_cgs), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        integer(int32) :: ierr

        self%ID = solver_settings%ID
        self%name = "CGS"
        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%max_iterations = solver_settings%max_iterations

        call self%r%initialize(self%num_nodes)
        call self%r0%initialize(self%num_nodes)
        call self%p%initialize(self%num_nodes)
        call self%phat%initialize(self%num_nodes)
        call self%u%initialize(self%num_nodes)
        call self%uhat%initialize(self%num_nodes)
        call self%q%initialize(self%num_nodes)
        call self%qhat%initialize(self%num_nodes)
        call self%v%initialize(self%num_nodes)
        call self%residual_history%initialize(self%max_iterations)
        self%current_iteration = 0

        self%status = SOLVER_STATUS%SUCCESS%ID
        call create_preconditioner(self%pc, preconditioner_settings, ierr)
        self%status = ierr

    end subroutine initialize_type_solver_cgs

    module subroutine solve_type_solver_cgs(self, A, b, x)
        implicit none
        class(type_solver_cgs), intent(inout) :: self
        class(abst_matrix), intent(in) :: A
        type(type_vector_dp), intent(in) :: b
        type(type_vector_dp), intent(inout) :: x

        real(real64) :: rho, rho_old, alpha, beta, sigma, resid, resid0
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

        ! r0 = r (shadow)
        call self%r0%copy(self%r)

        ! p = r, u = r
        call self%p%copy(self%r)
        call self%u%copy(self%r)

        rho = vector_dot(self%r0, self%r)

        do iter = 1, self%max_iterations
            self%current_iteration = iter

            ! phat = M^{-1} p
            call self%pc%apply(self%p, self%phat)

            ! v = A * phat
            call matvec(A, self%phat, self%v, ierr)

            ! sigma = (r0, v)
            sigma = vector_dot(self%r0, self%v)
            if (abs(sigma) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            alpha = rho / sigma

            ! q = u - alpha * v
            call vector_axpyz(-alpha, self%v, self%u, self%q)

            ! uhat = M^{-1} (u + q)
            ! Compute u + q into qhat as temp, then precondition
            call vector_axpyz(1.0d0, self%u, self%q, self%qhat)
            call self%pc%apply(self%qhat, self%uhat)

            ! x = x + alpha * uhat
            call vector_axpy(alpha, self%uhat, x)

            ! qhat = A * uhat
            call matvec(A, self%uhat, self%qhat, ierr)

            ! r = r - alpha * qhat
            call vector_axpy(-alpha, self%qhat, self%r)

            resid = vector_norm2(self%r)
            call self%residual_history%set(MATRIX_OPS%INS, iter, resid)

            if (resid < self%tolerance .or. &
                (resid0 > self%tolerance .and. resid < self%relative_tolerance * resid0)) then
                self%status = SOLVER_STATUS%SUCCESS%ID
                return
            end if

            rho_old = rho
            rho = vector_dot(self%r0, self%r)

            if (abs(rho_old) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            beta = rho / rho_old

            ! u = r + beta * q
            call vector_axpyz(beta, self%q, self%r, self%u)

            ! p = u + beta * (q + beta * p)
            call vector_scale(beta, self%p)
            call vector_axpy(1.0d0, self%q, self%p)
            call vector_scale(beta, self%p)
            call vector_axpy(1.0d0, self%u, self%p)

            if (was_interrupted()) stop
        end do

        self%status = SOLVER_STATUS%MAXITER%ID

    end subroutine solve_type_solver_cgs

    module subroutine destroy_type_solver_cgs(self)
        implicit none
        class(type_solver_cgs), intent(inout) :: self

        self%ID = -1
        if (allocated(self%name)) deallocate (self%name)
        call self%r%destroy()
        call self%r0%destroy()
        call self%p%destroy()
        call self%phat%destroy()
        call self%u%destroy()
        call self%uhat%destroy()
        call self%q%destroy()
        call self%qhat%destroy()
        call self%v%destroy()
        if (allocated(self%pc)) then
            call self%pc%destroy()
            deallocate (self%pc)
        end if
        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine destroy_type_solver_cgs

end submodule impl_solve_type_solver_cgs

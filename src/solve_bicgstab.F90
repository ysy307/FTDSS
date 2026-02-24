submodule(solver_solve) solve_type_solver_bicgstab
    implicit none
contains

    !> Initialize the BiCGSTAB solver instance.
    !> It sets up internal vectors, parameters, and the preconditioner.
    module subroutine initialize_type_solver_bicgstab(self, solver_settings, preconditioner_settings)
        implicit none
        !> Solver instance to be initialized
        class(type_solver_bicgstab), intent(inout) :: self
        !> Configuration settings for the solver
        type(type_solver_settings), intent(in) :: solver_settings
        !> Configuration settings for the preconditioner
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        integer(int32) :: ierr

        self%ID = solver_settings%ID
        self%name = "BiCGSTAB"

        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%max_iterations = solver_settings%max_iterations

        call self%p%initialize(self%num_nodes)
        call self%phat%initialize(self%num_nodes)
        call self%s%initialize(self%num_nodes)
        call self%shat%initialize(self%num_nodes)
        call self%r%initialize(self%num_nodes)
        call self%r0%initialize(self%num_nodes)
        call self%t%initialize(self%num_nodes)
        call self%v%initialize(self%num_nodes)
        call self%x%initialize(self%num_nodes)
        call self%residual_history%initialize(self%max_iterations)
        self%current_iteration = 0

        self%status = SOLVER_STATUS%SUCCESS%ID

        ! Setup preconditioner
        call create_preconditioner(self%pc, preconditioner_settings, ierr)
        self%status = ierr

    end subroutine initialize_type_solver_bicgstab

    !> Solve the linear system \( Ax = b \) using the BiCGSTAB method.
    module subroutine solve_type_solver_bicgstab(self, A, b, x)
        implicit none
        !> Solver instance
        class(type_solver_bicgstab), intent(inout) :: self
        !> System matrix
        class(abst_matrix), intent(in) :: A
        !> Right-hand side vector
        type(type_vector_dp), intent(in) :: b
        !> Solution vector (initial guess on input, result on output)
        type(type_vector_dp), intent(inout) :: x

        real(real64) :: rho, rho_old, alpha, beta, omega
        real(real64) :: resid
        integer(int32) :: iter

        real(real64), dimension(:), pointer :: x_ptr
        integer(int32) :: ierr

        ! ==========================================================
        ! 1: Initialize
        ! ==========================================================
        rho = 1.0d0
        rho_old = 1.0d0
        alpha = 1.0d0
        beta = 1.0d0
        omega = 1.0d0

        call self%p%zero()
        call self%s%zero()
        call self%phat%zero()
        call self%shat%zero()

        call self%residual_history%zero()

        ! ==========================================================
        ! 2: Set an initial value x0
        ! ==========================================================
        call self%x%zero()

        ! ==========================================================
        ! 3: r0 = b - Ax0
        ! ==========================================================
        call self%r%zero()
        call matvec(A, self%x, self%r, ierr)
        call vector_axpyz(-1.0d0, self%r, b, self%r)

        ! ==========================================================
        ! 4: Create preconditioned matrix
        ! ==========================================================
        call self%pc%setup(A)

        ! ==========================================================
        ! 5: ^r0 = r0 such that (r*0, r0) != 0
        ! ==========================================================
        call self%r0%copy(self%r)

        do iter = 1, self%max_iterations
            ! 7: (^r0, rk)
            rho = vector_dot(self%r0, self%r)
            ! 8: rho check
            if (rho == 0.0d0) then
                self%current_iteration = iter
                call self%residual_history%set(MATRIX_OPS%INS, iter, vector_norm2(self%r))
                call x%copy(self%x)
                self%status = SOLVER_STATUS%SUCCESS%ID
                return
            end if

            if (iter == 1) then
                ! 10: p0 = r0
                call self%p%copy(self%r)
            else
                ! 12: beta = (rho / rho_old) * (alpha_k / omega_k)
                beta = (rho / rho_old) * (alpha / omega)
                ! 13: p_k = r_k + beta_k(p_(k-1) - omega_k * Av)
                call vector_axpy(-omega, self%v, self%p)
                call vector_scale(beta, self%p)
                call vector_axpy(1.0d0, self%r, self%p)
            end if
            ! 15: phat = M^-1 * p
            call self%pc%apply(self%p, self%phat)
            ! 16: v = A * phat
            call matvec(A, self%phat, self%v, ierr)
            ! 17: alpha_k = rho / (^r0, v)
            alpha = rho / vector_dot(self%r0, self%v)
            ! 18: s = r_k - alpha_k * v
            call vector_axpyz(-alpha, self%v, self%r, self%s)

            ! 19: shat = M^-1 * s
            call self%pc%apply(self%s, self%shat)
            ! 20: t = A * shat
            call matvec(A, self%shat, self%t, ierr)

            ! 21: omega_k = (t,s)/(t,t)
            omega = vector_dot(self%t, self%s) / vector_dot(self%t, self%t)

            ! 22: omega breakdown check
            if (omega == 0.0d0) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if

            ! 23: x(i) = x(i-1) + alpha * M^-1 p(i-1) + omega * M^-1 s(i)
            ! 24: r(i) = s(i-1) - omega * AM^-1 s(i-1)
            call vector_axpy(alpha, self%phat, self%x)
            call vector_axpy(omega, self%shat, self%x)
            call vector_axpyz(-omega, self%t, self%s, self%r)

            ! 25: ||r_k+1||_2
            resid = vector_norm2(self%r)
            call self%residual_history%set(MATRIX_OPS%INS, iter, resid)
            if (resid < self%tolerance) then
                self%current_iteration = iter
                self%status = SOLVER_STATUS%SUCCESS%ID
                call x%copy(self%x)
                return
            end if

            rho_old = rho

            if (was_interrupted()) stop
        end do
        self%current_iteration = iter
        self%status = SOLVER_STATUS%MAXITER%ID
        call x%copy(self%x)

    end subroutine solve_type_solver_bicgstab

    !> Finalize the solver instance and release memory.
    module subroutine destroy_type_solver_bicgstab(self)
        implicit none
        !> Solver instance to be destroyed
        class(type_solver_bicgstab), intent(inout) :: self

        self%ID = -1
        if (allocated(self%name)) deallocate (self%name)
        self%num_nodes = -1
        self%tolerance = 0.0d0
        self%max_iterations = 0

        call self%p%destroy()
        call self%phat%destroy()
        call self%s%destroy()
        call self%shat%destroy()
        call self%r%destroy()
        call self%r0%destroy()
        call self%t%destroy()
        call self%v%destroy()
        call self%x%destroy()

        if (allocated(self%pc)) then
            call self%pc%destroy()
            deallocate (self%pc)
        end if

        self%status = SOLVER_STATUS%SUCCESS%ID
    end subroutine destroy_type_solver_bicgstab
end submodule solve_type_solver_bicgstab

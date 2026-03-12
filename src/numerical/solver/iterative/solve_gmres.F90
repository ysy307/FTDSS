submodule(numerical_solver_interface) impl_solve_type_solver_gmres
    implicit none
contains

    !> Initialize the GMRES solver instance.
    !> It allocates memory for the Krylov subspace basis (V) and the Hessenberg matrix (H).
    module subroutine initialize_type_solver_gmres(self, solver_settings, preconditioner_settings)
        implicit none
        !> Solver instance to be initialized
        class(type_solver_gmres), intent(inout) :: self
        !> Configuration settings for the solver
        type(type_solver_settings), intent(in) :: solver_settings
        !> Configuration settings for the preconditioner
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        integer(int32) :: i, ierr

        self%ID = solver_settings%ID
        self%name = "GMRES"

        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%max_iterations = solver_settings%max_iterations
        self%m_restart = solver_settings%m_restart

        ! Check default restart value
        if (self%m_restart <= 0) self%m_restart = 30

        ! ==========================================================
        ! Memory Allocation
        ! ==========================================================

        ! Allocate basis vectors V
        if (allocated(self%v)) deallocate (self%v)
        allocate (self%v(self%m_restart + 1))
        do i = 1, self%m_restart + 1
            call self%v(i)%initialize(self%num_nodes)
        end do

        ! Initialize workspace vectors
        call self%r%initialize(self%num_nodes)
        call self%w%initialize(self%num_nodes)
        call self%z%initialize(self%num_nodes)
        call self%x_update%initialize(self%num_nodes)
        call self%r%zero()
        call self%w%zero()
        call self%z%zero()
        call self%x_update%zero()

        ! Allocate Hessenberg matrix and auxiliary arrays (small arrays dependent on m)
        call allocate_array(self%h, self%m_restart + 1, self%m_restart)
        call allocate_array(self%g, self%m_restart + 1)
        call allocate_array(self%cs, self%m_restart)
        call allocate_array(self%sn, self%m_restart)
        call allocate_array(self%y, self%m_restart)

        ! Zero clear
        self%h = 0.0d0
        self%g = 0.0d0
        self%cs = 0.0d0
        self%sn = 0.0d0
        self%y = 0.0d0

        ! Initialize history
        call self%residual_history%initialize(self%max_iterations)
        self%current_iteration = 0

        ! Create preconditioner
        call create_preconditioner(self%pc, preconditioner_settings, ierr)
        self%status = ierr

    end subroutine initialize_type_solver_gmres

    !> Solve the linear system \( Ax = b \) using the GMRES(m) method.
    !> Implements Right Preconditioning and Restarting.
    module subroutine solve_type_solver_gmres(self, A, b, x)
        implicit none
        !> Solver instance
        class(type_solver_gmres), intent(inout) :: self
        !> System matrix \( A \)
        class(abst_matrix), intent(in) :: A
        !> Right-hand side vector \( b \)
        type(type_vector_dp), intent(in) :: b
        !> Solution vector (initial guess on input, result on output)
        type(type_vector_dp), intent(inout) :: x

        real(real64) :: beta, w_norm, temp_val, resid
        integer(int32) :: i, k, ierr, iter_global, iter
        logical :: converged

        ! Initialize
        iter_global = 0
        self%current_iteration = 0
        converged = .false.

        ! Clear history
        call self%residual_history%zero()

        ! Setup Preconditioner
        call self%pc%setup(A)

        ! ==========================================================
        ! Restart Loop (Outer Loop)
        ! ==========================================================
        restart_loop: do

            ! ------------------------------------------------------
            ! 1. Compute initial residual: r0 = b - Ax
            ! ------------------------------------------------------
            ! Note: Recompute true residual from current x at every restart
            call self%r%zero()
            call matvec(A, x, self%r, ierr)
            call vector_axpyz(-1.0d0, self%r, b, self%r)

            ! beta = ||r0||_2
            beta = vector_norm2(self%r)

            ! Save history (first iteration or every restart)
            if (iter_global == 0) call self%residual_history%set(MATRIX_OPS%INS, 1, beta)

            ! Convergence check (Initial)
            if (beta < self%tolerance) then
                self%status = SOLVER_STATUS%SUCCESS%ID
                exit restart_loop
            end if

            ! ------------------------------------------------------
            ! 2. Initialize basis: v1 = r0 / beta
            ! ------------------------------------------------------
            ! Reset arrays
            self%h(:, :) = 0.0d0
            self%g(:) = 0.0d0
            self%cs(:) = 0.0d0
            self%sn(:) = 0.0d0
            self%y(:) = 0.0d0

            ! Initialize RHS vector g: g = [beta, 0, ..., 0]^T
            self%g(1) = beta

            ! Set v1
            call self%v(1)%copy(self%r)
            call vector_scale(1.0d0 / beta, self%v(1))

            ! ======================================================
            ! Arnoldi Loop (Inner Loop)
            ! ======================================================
            arnoldi_loop: do iter = 1, self%m_restart
                iter_global = iter_global + 1
                self%current_iteration = iter_global
                k = iter

                ! --------------------------------------------------
                ! Step 5: w = A * M^-1 * v_i (Right Preconditioning)
                ! --------------------------------------------------
                ! 1. Preconditioning: z = M^-1 * v(iter)
                call self%pc%apply(self%v(iter), self%z)

                ! 2. Matrix-Vector Product: w = A * z
                call matvec(A, self%z, self%w, ierr)

                ! --------------------------------------------------
                ! Step 6-9: Modified Gram-Schmidt (MGS)
                ! --------------------------------------------------
                do i = 1, iter
                    ! Dot product h(i, iter) = (w, v(i))
                    self%h(i, iter) = vector_dot(self%w, self%v(i))
                    ! Orthogonalization w = w - h(i, iter) * v(i)
                    call vector_axpy(-self%h(i, iter), self%v(i), self%w)
                end do

                ! --------------------------------------------------
                ! Step 10-11: Normalize & Breakdown Check
                ! --------------------------------------------------
                w_norm = vector_norm2(self%w)

                ! Happy Breakdown check
                if (w_norm < 1.0d-20) then
                    self%h(iter + 1, iter) = 0.0d0
                    ! Cannot extend basis further; break loop (residual should be small)
                    exit arnoldi_loop
                else
                    self%h(iter + 1, iter) = w_norm
                    ! v(iter+1) = w / w_norm
                    call self%v(iter + 1)%copy(self%w)
                    call vector_scale(1.0d0 / w_norm, self%v(iter + 1))
                end if

                ! --------------------------------------------------
                ! Givens Rotations
                ! --------------------------------------------------
                ! Apply previous rotations
                do i = 1, iter - 1
                    temp_val = self%cs(i) * self%h(i, iter) + self%sn(i) * self%h(i + 1, iter)
                    self%h(i + 1, iter) = -self%sn(i) * self%h(i, iter) + self%cs(i) * self%h(i + 1, iter)
                    self%h(i, iter) = temp_val
                end do

                ! Generate new rotation
                call generate_givens_rotation(self%h(iter, iter), self%h(iter + 1, iter), self%cs(iter), self%sn(iter))

                ! Apply rotation to current column (Diagonalize)
                self%h(iter, iter) = self%cs(iter) * self%h(iter, iter) + self%sn(iter) * self%h(iter + 1, iter)
                self%h(iter + 1, iter) = 0.0d0

                ! Apply rotation to RHS vector g
                self%g(iter + 1) = -self%sn(iter) * self%g(iter)
                self%g(iter) = self%cs(iter) * self%g(iter)

                ! --------------------------------------------------
                ! Convergence Check
                ! --------------------------------------------------
                resid = abs(self%g(iter + 1))
                call self%residual_history%set(MATRIX_OPS%INS, iter_global, resid)

                if (resid < self%tolerance) then
                    converged = .true.
                    exit arnoldi_loop
                end if

                if (iter_global >= self%max_iterations) then
                    self%status = SOLVER_STATUS%MAXITER%ID
                    exit arnoldi_loop
                end if

            end do arnoldi_loop

            ! ======================================================
            ! Update Solution
            ! ======================================================

            ! 1. Calculate Least Squares solution y (Solve upper triangular Hy = g)
            call backward_substitution(k, self%h, self%g, self%y)

            ! 2. x_update = V * y (Update vector in non-preconditioned space)
            call self%x_update%zero()
            do i = 1, k
                call vector_axpy(self%y(i), self%v(i), self%x_update)
            end do

            ! 3. Map to preconditioned space: z = M^-1 * x_update
            call self%pc%apply(self%x_update, self%z)

            ! 4. Update true solution: x = x + z
            call vector_axpy(1.0d0, self%z, x)

            ! ======================================================
            ! Restart Check
            ! ======================================================
            if (converged) then
                self%status = SOLVER_STATUS%SUCCESS%ID
                exit restart_loop
            end if

            if (self%status == SOLVER_STATUS%MAXITER%ID) then
                exit restart_loop
            end if

        end do restart_loop

    end subroutine solve_type_solver_gmres

    !> Finalize the GMRES solver instance and release memory.
    module subroutine destroy_type_solver_gmres(self)
        implicit none
        !> Solver instance to be destroyed
        class(type_solver_gmres), intent(inout) :: self
        integer(int32) :: i

        self%ID = -1
        if (allocated(self%name)) deallocate (self%name)

        ! Release vector arrays
        if (allocated(self%v)) then
            do i = 1, size(self%v)
                call self%v(i)%destroy()
            end do
            deallocate (self%v)
        end if

        call self%r%destroy()
        call self%z%destroy()
        call self%x_update%destroy()

        ! Release scalar arrays
        call deallocate_array(self%h)
        call deallocate_array(self%g)
        call deallocate_array(self%cs)
        call deallocate_array(self%sn)
        call deallocate_array(self%y)
        if (allocated(self%pc)) then
            call self%pc%destroy()
            deallocate (self%pc)
        end if

        self%status = SOLVER_STATUS%SUCCESS%ID
    end subroutine destroy_type_solver_gmres

    !> Generate Givens rotation coefficients \( c \) (cosine) and \( s \) (sine).
    pure subroutine generate_givens_rotation(dx, dy, c, s)
        implicit none
        !> Value on the diagonal
        real(real64), intent(in) :: dx
        !> Value below the diagonal
        real(real64), intent(in) :: dy
        !> Cosine component of rotation
        real(real64), intent(inout) :: c
        !> Sine component of rotation
        real(real64), intent(inout) :: s
        real(real64) :: temp

        if (dy == 0.0d0) then
            c = 1.0d0
            s = 0.0d0
        else if (abs(dy) > abs(dx)) then
            temp = dx / dy
            s = 1.0d0 / sqrt(1.0d0 + temp**2)
            c = temp * s
        else
            temp = dy / dx
            c = 1.0d0 / sqrt(1.0d0 + temp**2)
            s = temp * c
        end if
    end subroutine generate_givens_rotation

    !> Perform backward substitution to solve the upper triangular system \( Hy = g \).
    pure subroutine backward_substitution(n, H, g, y)
        implicit none
        !> Dimension of the system
        integer(int32), intent(in) :: n
        !> Upper triangular (Hessenberg) matrix H (size (n+1) x n)
        real(real64), intent(in) :: H(:, :)
        !> Right-hand side vector g (size n+1)
        real(real64), intent(in) :: g(:)
        !> Solution vector y (size n)
        real(real64), intent(inout) :: y(:)

        integer(int32) :: i, j
        real(real64) :: sum_val

        y = 0.0d0
        do i = n, 1, -1
            sum_val = g(i)
            do j = i + 1, n
                sum_val = sum_val - H(i, j) * y(j)
            end do
            y(i) = sum_val / H(i, i)
        end do
    end subroutine backward_substitution

end submodule impl_solve_type_solver_gmres

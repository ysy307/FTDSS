submodule(numerical_solver_interface) impl_solve_type_solver_fgmres
    implicit none
contains

    !> Initialize the FGMRES solver instance.
    !> Allocates Krylov basis V(m+1), preconditioned vectors Z(m), and Hessenberg matrix H.
    module subroutine initialize_type_solver_fgmres(self, solver_settings, preconditioner_settings)
        implicit none
        class(type_solver_fgmres), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        integer(int32) :: i, ierr

        self%ID = solver_settings%ID
        self%name = "FGMRES"

        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%max_iterations = solver_settings%max_iterations
        self%m_restart = solver_settings%m_restart

        if (self%m_restart <= 0) self%m_restart = 30

        ! Allocate Krylov basis V(1:m+1)
        if (allocated(self%v)) deallocate (self%v)
        allocate (self%v(self%m_restart + 1))
        do i = 1, self%m_restart + 1
            call self%v(i)%initialize(self%num_nodes)
        end do

        ! Allocate preconditioned vectors Z(1:m) — key difference from GMRES
        if (allocated(self%zv)) deallocate (self%zv)
        allocate (self%zv(self%m_restart))
        do i = 1, self%m_restart
            call self%zv(i)%initialize(self%num_nodes)
        end do

        ! Workspace vectors
        call self%r%initialize(self%num_nodes)
        call self%w%initialize(self%num_nodes)
        call self%r%zero()
        call self%w%zero()

        ! Hessenberg matrix and auxiliary arrays
        call allocate_array(self%h, self%m_restart + 1, self%m_restart)
        call allocate_array(self%g, self%m_restart + 1)
        call allocate_array(self%cs, self%m_restart)
        call allocate_array(self%sn, self%m_restart)
        call allocate_array(self%y, self%m_restart)

        self%h = 0.0d0
        self%g = 0.0d0
        self%cs = 0.0d0
        self%sn = 0.0d0
        self%y = 0.0d0

        call self%residual_history%initialize(self%max_iterations)
        self%current_iteration = 0

        call create_preconditioner(self%pc, preconditioner_settings, ierr)
        self%status = ierr

    end subroutine initialize_type_solver_fgmres

    !> Solve the linear system Ax = b using FGMRES(m).
    !>
    !> Flexible GMRES stores preconditioned vectors z_j = M^{-1} v_j at each
    !> Arnoldi step. The solution update is x += sum y_j * z_j, which allows
    !> the preconditioner to change between iterations.
    module subroutine solve_type_solver_fgmres(self, A, b, x)
        implicit none
        class(type_solver_fgmres), intent(inout) :: self
        class(abst_matrix), intent(in) :: A
        type(type_vector_dp), intent(in) :: b
        type(type_vector_dp), intent(inout) :: x

        real(real64) :: beta, w_norm, temp_val, resid
        integer(int32) :: i, k, ierr, iter_global, iter
        logical :: converged

        iter_global = 0
        self%current_iteration = 0
        self%status = SOLVER_STATUS%SUCCESS%ID
        converged = .false.

        call self%residual_history%zero()

        ! Setup preconditioner
        call self%pc%setup(A)

        ! ==========================================================
        ! Restart Loop
        ! ==========================================================
        restart_loop: do

            ! Compute initial residual: r = b - Ax
            call self%r%zero()
            call matvec(A, x, self%r, ierr)
            call vector_axpyz(-1.0d0, self%r, b, self%r)

            beta = vector_norm2(self%r)

            if (iter_global == 0) call self%residual_history%set(MATRIX_OPS%INS, 1, beta)

            if (beta < self%tolerance) then
                self%status = SOLVER_STATUS%SUCCESS%ID
                exit restart_loop
            end if

            ! Initialize: v1 = r / beta, g = [beta, 0, ...]
            self%h(:, :) = 0.0d0
            self%g(:) = 0.0d0
            self%cs(:) = 0.0d0
            self%sn(:) = 0.0d0
            self%y(:) = 0.0d0

            self%g(1) = beta
            call self%v(1)%copy(self%r)
            call vector_scale(1.0d0 / beta, self%v(1))

            ! ==========================================================
            ! Arnoldi Loop
            ! ==========================================================
            arnoldi_loop: do iter = 1, self%m_restart
                iter_global = iter_global + 1
                self%current_iteration = iter_global
                k = iter

                ! FGMRES: store z_j = M^{-1} v_j (each step may use different M)
                call self%pc%apply(self%v(iter), self%zv(iter))

                ! w = A * z_j
                call matvec(A, self%zv(iter), self%w, ierr)

                ! Modified Gram-Schmidt
                do i = 1, iter
                    self%h(i, iter) = vector_dot(self%w, self%v(i))
                    call vector_axpy(-self%h(i, iter), self%v(i), self%w)
                end do

                ! Normalize
                w_norm = vector_norm2(self%w)

                ! Happy breakdown
                if (w_norm < 1.0d-20) then
                    self%h(iter + 1, iter) = 0.0d0
                    do i = 1, iter - 1
                        temp_val = self%cs(i) * self%h(i, iter) + self%sn(i) * self%h(i + 1, iter)
                        self%h(i + 1, iter) = -self%sn(i) * self%h(i, iter) + self%cs(i) * self%h(i + 1, iter)
                        self%h(i, iter) = temp_val
                    end do
                    call generate_givens_rotation_fg(self%h(iter, iter), self%h(iter + 1, iter), &
                                                    self%cs(iter), self%sn(iter))
                    self%h(iter, iter) = self%cs(iter) * self%h(iter, iter) + self%sn(iter) * self%h(iter + 1, iter)
                    self%h(iter + 1, iter) = 0.0d0
                    self%g(iter + 1) = -self%sn(iter) * self%g(iter)
                    self%g(iter) = self%cs(iter) * self%g(iter)
                    converged = .true.
                    exit arnoldi_loop
                else
                    self%h(iter + 1, iter) = w_norm
                    call self%v(iter + 1)%copy(self%w)
                    call vector_scale(1.0d0 / w_norm, self%v(iter + 1))
                end if

                ! Apply previous Givens rotations
                do i = 1, iter - 1
                    temp_val = self%cs(i) * self%h(i, iter) + self%sn(i) * self%h(i + 1, iter)
                    self%h(i + 1, iter) = -self%sn(i) * self%h(i, iter) + self%cs(i) * self%h(i + 1, iter)
                    self%h(i, iter) = temp_val
                end do

                ! Generate new Givens rotation
                call generate_givens_rotation_fg(self%h(iter, iter), self%h(iter + 1, iter), &
                                                 self%cs(iter), self%sn(iter))
                self%h(iter, iter) = self%cs(iter) * self%h(iter, iter) + self%sn(iter) * self%h(iter + 1, iter)
                self%h(iter + 1, iter) = 0.0d0

                self%g(iter + 1) = -self%sn(iter) * self%g(iter)
                self%g(iter) = self%cs(iter) * self%g(iter)

                ! Convergence check
                resid = abs(self%g(iter + 1))
                call self%residual_history%set(MATRIX_OPS%INS, iter_global, resid)

                if (resid < self%tolerance .or. &
                    (beta > self%tolerance .and. resid < self%relative_tolerance * beta)) then
                    converged = .true.
                    exit arnoldi_loop
                end if

                if (iter_global >= self%max_iterations) then
                    self%status = SOLVER_STATUS%MAXITER%ID
                    exit arnoldi_loop
                end if

            end do arnoldi_loop

            ! ==========================================================
            ! Update Solution: x += Z * y  (key FGMRES difference)
            ! ==========================================================

            ! Backward substitution: Hy = g
            call backward_substitution_fg(k, self%h, self%g, self%y)

            ! x += sum_i y(i) * z(i)  — directly in preconditioned space
            do i = 1, k
                call vector_axpy(self%y(i), self%zv(i), x)
            end do

            ! Restart check
            if (converged) then
                self%status = SOLVER_STATUS%SUCCESS%ID
                exit restart_loop
            end if

            if (self%status == SOLVER_STATUS%MAXITER%ID) then
                write (*, '(A,ES13.5,A,ES13.5,A,I0,A,I0)') &
                    '   [FGMRES] beta0=', beta, &
                    ' resid=', resid, ' iters=', iter_global, &
                    ' maxiter=', self%max_iterations
                exit restart_loop
            end if

        end do restart_loop

    end subroutine solve_type_solver_fgmres

    !> Finalize the FGMRES solver instance.
    module subroutine destroy_type_solver_fgmres(self)
        implicit none
        class(type_solver_fgmres), intent(inout) :: self
        integer(int32) :: i

        self%ID = -1
        if (allocated(self%name)) deallocate (self%name)

        if (allocated(self%v)) then
            do i = 1, size(self%v)
                call self%v(i)%destroy()
            end do
            deallocate (self%v)
        end if

        if (allocated(self%zv)) then
            do i = 1, size(self%zv)
                call self%zv(i)%destroy()
            end do
            deallocate (self%zv)
        end if

        call self%r%destroy()
        call self%w%destroy()

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
    end subroutine destroy_type_solver_fgmres

    !> Generate Givens rotation coefficients.
    pure subroutine generate_givens_rotation_fg(dx, dy, c, s)
        implicit none
        real(real64), intent(in) :: dx, dy
        real(real64), intent(inout) :: c, s
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
    end subroutine generate_givens_rotation_fg

    !> Backward substitution for upper triangular Hy = g.
    pure subroutine backward_substitution_fg(n, H, g, y)
        implicit none
        integer(int32), intent(in) :: n
        real(real64), intent(in) :: H(:, :)
        real(real64), intent(in) :: g(:)
        real(real64), intent(inout) :: y(:)

        integer(int32) :: i, j
        real(real64) :: sum_val

        y = 0.0d0
        do i = n, 1, -1
            sum_val = g(i)
            do j = i + 1, n
                sum_val = sum_val - H(i, j) * y(j)
            end do
            if (abs(H(i, i)) > tiny(1.0d0)) then
                y(i) = sum_val / H(i, i)
            else
                y(i) = 0.0d0
            end if
        end do
    end subroutine backward_substitution_fg

end submodule impl_solve_type_solver_fgmres

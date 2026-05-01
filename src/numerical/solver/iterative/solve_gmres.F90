submodule(numerical_solver_interface) impl_solve_type_solver_gmres
    implicit none
contains

    module subroutine initialize_type_solver_gmres(self, solver_settings, preconditioner_settings)
        implicit none
        class(type_solver_gmres), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        integer(int32) :: m, ierr, i

        self%ID = solver_settings%ID
        self%name = "GMRES"
        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%max_iterations = solver_settings%max_iterations
        self%m_restart = solver_settings%m_restart
        self%projection_enabled = solver_settings%projection_enabled
        self%projection_offset = solver_settings%projection_offset
        self%projection_stride = solver_settings%projection_stride

        m = self%m_restart

        allocate (self%v(m + 1))
        do i = 1, m + 1
            call self%v(i)%initialize(self%num_nodes)
        end do
        call self%r%initialize(self%num_nodes)
        call self%z%initialize(self%num_nodes)
        call self%w%initialize(self%num_nodes)
        call self%x_update%initialize(self%num_nodes)

        allocate (self%h(m + 1, m))
        allocate (self%g(m + 1))
        allocate (self%cs(m))
        allocate (self%sn(m))
        allocate (self%y(m))

        self%h = 0.0d0
        self%g = 0.0d0
        self%cs = 0.0d0
        self%sn = 0.0d0
        self%y = 0.0d0

        call self%residual_history%initialize(self%max_iterations)
        self%current_iteration = 0
        self%status = SOLVER_STATUS%SUCCESS%ID

        call create_preconditioner(self%pc, preconditioner_settings, ierr)
        self%status = ierr

    end subroutine initialize_type_solver_gmres

    module subroutine destroy_type_solver_gmres(self)
        implicit none
        class(type_solver_gmres), intent(inout) :: self

        integer(int32) :: i

        if (allocated(self%v)) then
            do i = 1, size(self%v)
                call self%v(i)%destroy()
            end do
            deallocate (self%v)
        end if
        call self%r%destroy()
        call self%z%destroy()
        call self%w%destroy()
        call self%x_update%destroy()

        if (allocated(self%h)) deallocate (self%h)
        if (allocated(self%g)) deallocate (self%g)
        if (allocated(self%cs)) deallocate (self%cs)
        if (allocated(self%sn)) deallocate (self%sn)
        if (allocated(self%y)) deallocate (self%y)

        if (allocated(self%pc)) then
            call self%pc%destroy()
            deallocate (self%pc)
        end if

        if (allocated(self%name)) deallocate (self%name)
        self%ID = -1
        self%num_nodes = -1
        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine destroy_type_solver_gmres

    module subroutine solve_type_solver_gmres(self, A, b, x)
        implicit none
        class(type_solver_gmres), intent(inout) :: self
        class(abst_matrix), intent(in) :: A
        type(type_vector_dp), intent(in) :: b
        type(type_vector_dp), intent(inout) :: x

        real(real64) :: beta, b_norm, w_norm, temp_val, h_norm_before
        real(real64) :: resid_krylov, beta0_initial, beta_pre_update
        integer(int32) :: i, k, ierr, iter_global, iter
        integer(int32) :: bad_restart_count

        iter_global = 0
        bad_restart_count = 0
        self%current_iteration = 0
        self%status = SOLVER_STATUS%SUCCESS%ID

        block
            real(real64), pointer :: bptr(:)
            bptr => b%get_data()
            write (*, '(A,I0,A,I0,A,I0)') &
                '   [GMRES-INIT] n=', size(bptr), &
                ' m_restart=', self%m_restart, ' max_iter=', self%max_iterations
            flush(6)
            nullify(bptr)
        end block

        call self%residual_history%zero()
        call self%pc%setup(A)

        b_norm = vector_norm2(b)
        if (b_norm == 0.0d0) b_norm = 1.0d0

        call self%r%zero()
        call matvec(A, x, self%r, ierr)

        call vector_axpyz(-1.0d0, self%r, b, self%r)
        call project_component_mean_zero(self%r, self%projection_enabled, self%projection_offset, self%projection_stride)

        beta = vector_norm2(self%r)
        beta0_initial = beta

        write (*, '(A,ES12.4,A,ES12.4,A,ES12.4)') &
            '   [GMRES] beta0=', beta, '  tol=', self%tolerance, '  rel_tol*b=', self%relative_tolerance * b_norm
        flush(6)

        call self%residual_history%set(MATRIX_OPS%INS, 1, beta)

        if (beta < self%tolerance .or. beta < self%relative_tolerance * b_norm) then
            write (*, '(A)') '   [GMRES] converged on initial residual check'
            flush(6)
            return
        end if

        restart_loop: do

            self%h = 0.0d0
            self%g = 0.0d0
            self%cs = 0.0d0
            self%sn = 0.0d0
            self%y = 0.0d0

            self%g(1) = beta

            call self%v(1)%copy(self%r)
            call vector_scale(1.0d0 / beta, self%v(1))

            arnoldi_loop: do iter = 1, self%m_restart

                iter_global = iter_global + 1
                self%current_iteration = iter_global
                k = iter

                call self%pc%apply(self%v(iter), self%z)

                call matvec(A, self%z, self%w, ierr)

                ! MGS pass 1
                do i = 1, iter
                    self%h(i, iter) = vector_dot(self%w, self%v(i))
                    call vector_axpy(-self%h(i, iter), self%v(i), self%w)
                end do

                h_norm_before = vector_norm2(self%w)

                ! Conditional re-orthogonalization (Kahan criterion)
                if (h_norm_before < 0.5d0 * abs(self%h(iter, iter))) then
                    do i = 1, iter
                        temp_val = vector_dot(self%w, self%v(i))
                        self%h(i, iter) = self%h(i, iter) + temp_val
                        call vector_axpy(-temp_val, self%v(i), self%w)
                    end do
                end if

                w_norm = vector_norm2(self%w)

                if (w_norm <= 1.0d-14) then
                    self%h(iter + 1, iter) = 0.0d0
                    exit arnoldi_loop
                end if

                self%h(iter + 1, iter) = w_norm
                call self%v(iter + 1)%copy(self%w)
                call vector_scale(1.0d0 / w_norm, self%v(iter + 1))

                do i = 1, iter - 1
                    temp_val = self%cs(i)*self%h(i, iter) + self%sn(i)*self%h(i + 1, iter)
                    self%h(i + 1, iter) = -self%sn(i)*self%h(i, iter) + self%cs(i)*self%h(i + 1, iter)
                    self%h(i, iter) = temp_val
                end do

                call generate_givens_rotation(self%h(iter, iter), self%h(iter + 1, iter), self%cs(iter), self%sn(iter))

                self%h(iter, iter) = self%cs(iter)*self%h(iter, iter) + self%sn(iter)*self%h(iter + 1, iter)
                self%h(iter + 1, iter) = 0.0d0

                self%g(iter + 1) = -self%sn(iter)*self%g(iter)
                self%g(iter) = self%cs(iter)*self%g(iter)

                resid_krylov = abs(self%g(iter + 1))
                call self%residual_history%set(MATRIX_OPS%INS, iter_global, resid_krylov)

                if (mod(iter_global, 10) == 0) then
                    write (*, '(A,I0,A,ES12.4,A,ES12.4)', advance='yes') &
                        '   [GMRES] iter=', iter_global, &
                        '  resid=', resid_krylov, '  b_norm=', b_norm
                    flush(6)
                end if

                ! Exit when Krylov estimate is at tolerance, machine precision, or max iterations.
                ! The outer restart_loop checks the true residual after each restart and continues
                ! if not yet converged, so exiting here on tolerance is safe.
                if (resid_krylov < max(self%tolerance, 1.0d-14) .or. &
                    resid_krylov < self%relative_tolerance * b_norm .or. &
                    iter_global >= self%max_iterations) then
                    if (iter_global >= self%max_iterations) self%status = SOLVER_STATUS%MAXITER%ID
                    exit arnoldi_loop
                end if

            end do arnoldi_loop

            ! backward_substitution resets status to SUCCESS; preserve MAXITER across it
            block
                integer(int32) :: saved_status
                saved_status = self%status
                call backward_substitution(k, self%h, self%g, self%y, self%status)
                if (self%status /= SOLVER_STATUS%SUCCESS%ID) exit restart_loop
                if (saved_status == SOLVER_STATUS%MAXITER%ID) self%status = SOLVER_STATUS%MAXITER%ID
            end block

            call self%x_update%zero()
            do i = 1, k
                call vector_axpy(self%y(i), self%v(i), self%x_update)
            end do

            write (*, '(A,I0,A,ES12.4,A,ES12.4)') &
                '   [GMRES-UPD] k=', k, &
                '  |y|_inf=', maxval(abs(self%y(1:k))), &
                '  |x_update|=', vector_norm2(self%x_update)
            flush(6)

            call self%pc%apply(self%x_update, self%z)

            ! Save beta before the update to detect post-update divergence
            beta_pre_update = beta

            call vector_axpy(1.0d0, self%z, x)
            call project_component_mean_zero(x, self%projection_enabled, self%projection_offset, self%projection_stride)

            call matvec(A, x, self%r, ierr)

            call vector_axpyz(-1.0d0, self%r, b, self%r)
            call project_component_mean_zero(self%r, self%projection_enabled, self%projection_offset, self%projection_stride)

            beta = vector_norm2(self%r)

            call self%residual_history%set(MATRIX_OPS%INS, iter_global, beta)

            ! Restart divergence guard: if the update worsened the residual significantly,
            ! revert x to the pre-update state and restart from a clean residual.
            ! Threshold of 10x: catches the GMRES Krylov-exhaustion catastrophic
            ! cancellation case (true_resid 3.7x worse) while allowing mild fluctuations.
            ! On NaN/Inf always revert and terminate.
            ! bad_restart_count limits consecutive failed restarts to prevent infinite loops
            ! when the preconditioner is ill-conditioned and every update diverges.
            if (beta /= beta .or. beta > 1.0d2 * beta_pre_update) then
                write (*, '(A,ES12.4,A,ES12.4,A,I0)') &
                    '   [GMRES-GUARD] beta=', beta, '  pre=', beta_pre_update, '  bad_count=', bad_restart_count
                flush(6)
                call vector_axpy(-1.0d0, self%z, x)
                if (beta /= beta) then
                    self%status = SOLVER_STATUS%MAXITER%ID
                    exit restart_loop
                end if
                bad_restart_count = bad_restart_count + 1
                if (bad_restart_count >= 3) then
                    self%status = SOLVER_STATUS%MAXITER%ID
                    exit restart_loop
                end if
                ! Recompute residual from reverted x for a clean restart
                call matvec(A, x, self%r, ierr)
                call vector_axpyz(-1.0d0, self%r, b, self%r)
                call project_component_mean_zero(self%r, self%projection_enabled, &
                                                 self%projection_offset, self%projection_stride)
                beta = vector_norm2(self%r)
            else
                bad_restart_count = 0
            end if

            if (beta < self%tolerance .or. &
                beta < self%relative_tolerance * b_norm) then
                return
            end if

            if (self%status == SOLVER_STATUS%MAXITER%ID) return

        end do restart_loop

    end subroutine solve_type_solver_gmres

    pure subroutine generate_givens_rotation(dx, dy, c, s)
        implicit none
        real(real64), intent(in) :: dx, dy
        real(real64), intent(inout) :: c, s
        real(real64) :: r

        r = hypot(dx, dy)

        if (r == 0.0d0) then
            c = 1.0d0
            s = 0.0d0
        else
            c = dx / r
            s = dy / r
        end if
    end subroutine generate_givens_rotation

    pure subroutine backward_substitution(n, H, g, y, status)
        implicit none
        integer(int32), intent(in) :: n
        real(real64), intent(in) :: H(:, :)
        real(real64), intent(in) :: g(:)
        real(real64), intent(inout) :: y(:)
        integer(int32), intent(inout) :: status

        integer(int32) :: i, j
        real(real64) :: sum_val

        y = 0.0d0
        status = SOLVER_STATUS%SUCCESS%ID

        do i = n, 1, -1
            if (abs(H(i, i)) <= tiny(1.0d0)) then
                status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            sum_val = g(i)
            do j = i + 1, n
                sum_val = sum_val - H(i, j)*y(j)
            end do
            y(i) = sum_val / H(i, i)
        end do
    end subroutine backward_substitution

end submodule impl_solve_type_solver_gmres

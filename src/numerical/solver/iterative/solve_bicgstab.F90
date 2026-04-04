submodule(numerical_solver_interface) impl_solve_type_solver_bicgstab
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
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
        real(real64) :: denom, ratio1, ratio2
        real(real64) :: resid, resid0
        real(real64) :: resid_prev
        real(real64) :: best_resid
        real(real64) :: norm_r, norm_v, norm_t, norm_s, norm_p
        integer(int32) :: iter

        real(real64), dimension(:), pointer :: b_ptr, r_ptr, x_internal_ptr
        logical :: has_internal_x
        logical :: use_restart_direction
        integer(int32) :: rho_restart_count
        integer(int32) :: alpha_restart_count
        integer(int32) :: omega_restart_count
        integer(int32), parameter :: MAX_ALPHA_RESTARTS = 8
        integer(int32), parameter :: MAX_OMEGA_RESTARTS = 8
        real(real64), parameter :: ALPHA_ABS_CAP = 1.0d8
        real(real64), parameter :: SHADOW_PERTURB_EPS = 1.0d-12
        real(real64), parameter :: NORM_GUARD_CAP = 1.0d120
        real(real64), parameter :: STAGNATION_RATIO = 0.9999d0
        integer(int32), parameter :: STAGNATION_PATIENCE = 400
        real(real64), parameter :: PROGRESS_GAIN = 1.0d-3
        integer(int32), parameter :: NO_PROGRESS_PATIENCE = 400
        logical :: alpha_recovered
        integer(int32) :: stagnant_count
        integer(int32) :: no_progress_count
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
        rho_restart_count = 0
        alpha_restart_count = 0
        omega_restart_count = 0

        ! ==========================================================
        ! 2: Set an initial value x0 (use user initial guess)
        ! ==========================================================
        call self%x%copy(x)
        x_internal_ptr => self%x%get_data()
        has_internal_x = associated(x_internal_ptr)

        ! ==========================================================
        ! 3: r0 = b - Ax0
        ! ==========================================================
        call self%r%zero()
        call matvec(A, self%x, self%r, ierr)
        r_ptr => self%r%get_data()
        if (.not. associated(r_ptr)) then
            self%current_iteration = 0
            self%status = SOLVER_STATUS%BREAKDOWN%ID
            if (has_internal_x) call x%copy(self%x)
            return
        end if

        b_ptr => b%get_data()
        if (associated(b_ptr)) then
            call vector_axpyz(-1.0d0, self%r, b, self%r)
        else
            ! Fallback: if RHS data is unavailable, treat b as zero vector.
            r_ptr = -r_ptr
        end if

        ! ==========================================================
        ! 4: Create preconditioned matrix
        ! ==========================================================
        call self%pc%setup(A)

        ! ==========================================================
        ! 5: ^r0 = r0 such that (r*0, r0) != 0
        ! ==========================================================
        call self%r0%copy(self%r)

        ! Initial convergence check
        resid = vector_norm2(self%r)
        resid0 = resid
        resid_prev = resid
        best_resid = resid
        stagnant_count = 0
        no_progress_count = 0
        call self%residual_history%set(MATRIX_OPS%INS, 1, resid)
        if (resid < self%tolerance) then
            self%current_iteration = 0
            self%status = SOLVER_STATUS%SUCCESS%ID
            if (has_internal_x) call x%copy(self%x)
            return
        end if

        do iter = 1, self%max_iterations
            use_restart_direction = .false.
            ! 7: (^r0, rk)
            rho = vector_dot(self%r0, self%r)
            ! 8: rho check — rho==0 means BiCGStab breakdown (r0* has become orthogonal to r)
            if (abs(rho) <= tiny(1.0d0)) then
                resid = vector_norm2(self%r)
                call self%residual_history%set(MATRIX_OPS%INS, iter, resid)
                if (resid < self%tolerance) then
                    self%current_iteration = iter
                    self%status = SOLVER_STATUS%SUCCESS%ID
                    if (has_internal_x) call x%copy(self%x)
                    return
                end if

                ! Refresh shadow residual to recover from accidental orthogonality.
                call self%r0%copy(self%r)
                rho = vector_dot(self%r0, self%r)
                if (abs(rho) > tiny(1.0d0)) then
                    rho_restart_count = rho_restart_count + 1
                    use_restart_direction = .true.
                else
                    write (*, '(A,I0,A,I0,2(A,ES13.5))') 'BiCGSTAB breakdown(rho after restart): iter=', iter, &
                        ', restarts=', rho_restart_count, ', rho=', rho, ', ||r||2=', resid
                    self%current_iteration = iter
                    self%status = SOLVER_STATUS%BREAKDOWN%ID
                    if (has_internal_x) call x%copy(self%x)
                    return
                end if
            end if

            if (iter == 1 .or. use_restart_direction) then
                ! 10: p0 = r0
                call self%p%copy(self%r)
                rho_old = rho
                alpha = 1.0d0
                omega = 1.0d0
            else
                if (.not. ieee_is_finite(rho_old) .or. .not. ieee_is_finite(omega) .or. &
                    .not. ieee_is_finite(alpha) .or. abs(rho_old) <= tiny(1.0d0) .or. abs(omega) <= tiny(1.0d0)) then
                    norm_r = vector_norm2(self%r)
                    write (*, '(A,I0,3(A,ES13.5))') 'BiCG breakdown(beta): iter=', iter, &
                        ', rho_old=', rho_old, ', omega=', omega, ', ||r||2=', norm_r
                    self%current_iteration = iter
                    self%status = SOLVER_STATUS%BREAKDOWN%ID
                    if (has_internal_x) call x%copy(self%x)
                    return
                end if
                ! 12: beta = (rho / rho_old) * (alpha_k / omega_k)
                if (abs(rho) / huge(1.0d0) > abs(rho_old)) then
                    norm_r = vector_norm2(self%r)
                    write (*, '(A,I0,3(A,ES13.5))') 'BiCG breakdown(rho/rho_old overflow): iter=', iter, &
                        ', rho=', rho, ', rho_old=', rho_old, ', ||r||2=', norm_r
                    self%current_iteration = iter
                    self%status = SOLVER_STATUS%BREAKDOWN%ID
                    if (has_internal_x) call x%copy(self%x)
                    return
                end if
                if (abs(alpha) / huge(1.0d0) > abs(omega)) then
                    norm_r = vector_norm2(self%r)
                    write (*, '(A,I0,3(A,ES13.5))') 'BiCG breakdown(alpha/omega overflow): iter=', iter, &
                        ', alpha=', alpha, ', omega=', omega, ', ||r||2=', norm_r
                    self%current_iteration = iter
                    self%status = SOLVER_STATUS%BREAKDOWN%ID
                    if (has_internal_x) call x%copy(self%x)
                    return
                end if

                ratio1 = rho / rho_old
                ratio2 = alpha / omega

                if (abs(ratio1) > 1.0d0) then
                    if (abs(ratio2) > huge(1.0d0) / abs(ratio1)) then
                        norm_r = vector_norm2(self%r)
                        write (*, '(A,I0,3(A,ES13.5))') 'BiCG breakdown(beta overflow): iter=', iter, &
                            ', rho/rho_old=', ratio1, ', alpha/omega=', ratio2, ', ||r||2=', norm_r
                        self%current_iteration = iter
                        self%status = SOLVER_STATUS%BREAKDOWN%ID
                        if (has_internal_x) call x%copy(self%x)
                        return
                    end if
                end if

                if (abs(ratio2) > 1.0d0) then
                    if (abs(ratio1) > huge(1.0d0) / abs(ratio2)) then
                        norm_r = vector_norm2(self%r)
                        write (*, '(A,I0,3(A,ES13.5))') 'BiCG breakdown(beta overflow): iter=', iter, &
                            ', rho/rho_old=', ratio1, ', alpha/omega=', ratio2, ', ||r||2=', norm_r
                        self%current_iteration = iter
                        self%status = SOLVER_STATUS%BREAKDOWN%ID
                        if (has_internal_x) call x%copy(self%x)
                        return
                    end if
                end if

                beta = ratio1 * ratio2
                if (.not. ieee_is_finite(beta)) then
                    norm_r = vector_norm2(self%r)
                    write (*, '(A,I0,2(A,ES13.5))') 'BiCG invalid beta: iter=', iter, ', beta=', beta, ', ||r||2=', norm_r
                    self%current_iteration = iter
                    self%status = SOLVER_STATUS%BREAKDOWN%ID
                    if (has_internal_x) call x%copy(self%x)
                    return
                end if
                norm_p = vector_norminf(self%p)
                if (norm_p >= 1.0d0) then
                    if (abs(beta) > huge(1.0d0) / norm_p) then
                        norm_r = vector_norm2(self%r)
                        write (*, '(A,I0,3(A,ES13.5))') 'BiCG overflow(beta): iter=', iter, &
                            ', beta=', beta, ', ||p||inf=', norm_p, ', ||r||2=', norm_r
                        self%current_iteration = iter
                        self%status = SOLVER_STATUS%BREAKDOWN%ID
                        if (has_internal_x) call x%copy(self%x)
                        return
                    end if
                end if
                ! 13: p_k = r_k + beta_k(p_(k-1) - omega_k * Av)
                call vector_axpy(-omega, self%v, self%p)
                call vector_scale(beta, self%p)
                call vector_axpy(1.0d0, self%r, self%p)
            end if
            ! 15: phat = M^-1 * p
            call self%pc%apply(self%p, self%phat)
            ! 16: v = A * phat
            call matvec(A, self%phat, self%v, ierr)
            norm_v = vector_norm2(self%v)
            if ((.not. ieee_is_finite(norm_v)) .or. norm_v > NORM_GUARD_CAP) then
                norm_r = vector_norm2(self%r)
                write (*, '(A,I0,2(A,ES13.5))') 'BiCGSTAB breakdown(v norm): iter=', iter, ', ||v||2=', norm_v, &
                    ', ||r||2=', norm_r
                self%current_iteration = iter
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                if (has_internal_x) call x%copy(self%x)
                return
            end if
            ! 17: alpha_k = rho / (^r0, v)
            denom = vector_dot(self%r0, self%v)
            alpha_recovered = .false.
            if (abs(denom) <= tiny(1.0d0)) then
                norm_r = vector_norm2(self%r)
                if (norm_r < self%tolerance) then
                    self%current_iteration = iter
                    self%status = SOLVER_STATUS%SUCCESS%ID
                    if (has_internal_x) call x%copy(self%x)
                    return
                end if

                if (alpha_restart_count < MAX_ALPHA_RESTARTS) then
                    alpha_restart_count = alpha_restart_count + 1
                    ! Refresh shadow residual and restart search direction.
                    call self%r0%copy(self%r)
                    rho = vector_dot(self%r0, self%r)
                    if (abs(rho) > tiny(1.0d0)) then
                        call self%p%copy(self%r)
                        rho_old = rho
                        alpha = 1.0d0
                        omega = 1.0d0
                        cycle
                    end if
                end if

                ! Last-resort recovery: perturb shadow residual to avoid exact orthogonality.
                call self%r0%copy(self%r)
                call vector_axpy(SHADOW_PERTURB_EPS, self%v, self%r0)
                rho = vector_dot(self%r0, self%r)
                denom = vector_dot(self%r0, self%v)
                if (abs(denom) > tiny(1.0d0) .and. ieee_is_finite(denom) .and. ieee_is_finite(rho)) then
                    alpha = rho/denom
                    alpha_recovered = .true.
                end if

                if (.not. alpha_recovered) then
                    norm_v = vector_norm2(self%v)
                    write (*, '(A,I0,A,I0,2(A,ES13.5))') 'BiCGSTAB breakdown(alpha): iter=', iter, ', restarts=', &
                        alpha_restart_count, ', dot(r0,v)=', denom, ', ||r||2=', norm_r
                    write (*, '(A,ES13.5,A,ES13.5)') '  rho=', rho, ', ||v||2=', norm_v
                    self%current_iteration = iter
                    self%status = SOLVER_STATUS%BREAKDOWN%ID
                    if (has_internal_x) call x%copy(self%x)
                    return
                end if
            end if
            if (.not. alpha_recovered) alpha = rho / denom
            if ((.not. ieee_is_finite(alpha)) .or. abs(alpha) > ALPHA_ABS_CAP) then
                norm_r = vector_norm2(self%r)
                if (alpha_restart_count < MAX_ALPHA_RESTARTS) then
                    alpha_restart_count = alpha_restart_count + 1
                    ! Restart BiCG direction when alpha is numerically unsafe.
                    call self%r0%copy(self%r)
                    rho = vector_dot(self%r0, self%r)
                    if (abs(rho) > tiny(1.0d0)) then
                        call self%p%copy(self%r)
                        rho_old = rho
                        alpha = 1.0d0
                        omega = 1.0d0
                        cycle
                    end if
                end if
                write (*, '(A,I0,A,I0,2(A,ES13.5))') 'BiCGSTAB breakdown(alpha invalid): iter=', iter, ', restarts=', &
                    alpha_restart_count, ', alpha=', alpha, ', ||r||2=', norm_r
                self%current_iteration = iter
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                if (has_internal_x) call x%copy(self%x)
                return
            end if
            ! 18: s = r_k - alpha_k * v
            call vector_axpyz(-alpha, self%v, self%r, self%s)

            ! 19: shat = M^-1 * s
            call self%pc%apply(self%s, self%shat)
            ! 20: t = A * shat
            call matvec(A, self%shat, self%t, ierr)
            norm_t = vector_norm2(self%t)
            if ((.not. ieee_is_finite(norm_t)) .or. norm_t > NORM_GUARD_CAP) then
                norm_s = vector_norm2(self%s)
                write (*, '(A,I0,2(A,ES13.5))') 'BiCGSTAB breakdown(t norm): iter=', iter, ', ||t||2=', norm_t, &
                    ', ||s||2=', norm_s
                self%current_iteration = iter
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                if (has_internal_x) call x%copy(self%x)
                return
            end if

            ! 21: omega_k = (t,s)/(t,t)
            denom = vector_dot(self%t, self%t)
            if (abs(denom) <= tiny(1.0d0) .or. .not. ieee_is_finite(denom)) then
                norm_t = vector_norm2(self%t)
                norm_s = vector_norm2(self%s)
                write (*, '(A,I0,3(A,ES13.5))') 'BiCGSTAB breakdown(omega denom): iter=', iter, &
                    ', dot(t,t)=', denom, ', ||t||2=', norm_t, ', ||s||2=', norm_s
                self%current_iteration = iter
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                if (has_internal_x) call x%copy(self%x)
                return
            end if
            omega = vector_dot(self%t, self%s) / denom

            ! 22: omega breakdown check
            if (abs(omega) <= tiny(1.0d0) .or. (.not. ieee_is_finite(omega))) then
                ! Recover from omega breakdown by recomputing residual from a partial update.
                call vector_axpy(alpha, self%phat, self%x)
                call matvec(A, self%x, self%r, ierr)
                r_ptr => self%r%get_data()
                if (associated(r_ptr)) then
                    if (associated(b_ptr)) then
                        call vector_axpyz(-1.0d0, self%r, b, self%r)
                    else
                        r_ptr = -r_ptr
                    end if
                end if
                resid = vector_norm2(self%r)
                if (resid < self%tolerance) then
                    self%current_iteration = iter
                    self%status = SOLVER_STATUS%SUCCESS%ID
                    if (has_internal_x) call x%copy(self%x)
                    return
                end if

                if (omega_restart_count < MAX_OMEGA_RESTARTS) then
                    omega_restart_count = omega_restart_count + 1
                    call self%r0%copy(self%r)
                    rho = vector_dot(self%r0, self%r)
                    if (abs(rho) > tiny(1.0d0)) then
                        call self%p%copy(self%r)
                        rho_old = rho
                        alpha = 1.0d0
                        omega = 1.0d0
                        cycle
                    end if
                end if

                norm_t = vector_norm2(self%t)
                norm_s = vector_norm2(self%s)
                write (*, '(A,I0,A,I0,3(A,ES13.5))') 'BiCGSTAB breakdown(omega): iter=', iter, ', restarts=', &
                    omega_restart_count, ', omega=', omega, ', ||t||2=', norm_t, ', ||s||2=', norm_s
                self%current_iteration = iter
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                if (has_internal_x) call x%copy(self%x)
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
            if ((.not. ieee_is_finite(resid)) .or. resid > NORM_GUARD_CAP) then
                write (*, '(A,I0,A,ES13.5)') 'BiCGSTAB breakdown(residual norm): iter=', iter, ', ||r||2=', resid
                self%current_iteration = iter
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                if (has_internal_x) call x%copy(self%x)
                return
            end if
            if (resid < self%tolerance .or. &
                (resid0 > self%tolerance .and. resid < self%relative_tolerance * resid0)) then
                self%current_iteration = iter
                self%status = SOLVER_STATUS%SUCCESS%ID
                if (has_internal_x) call x%copy(self%x)
                return
            end if

            if (resid < (1.0d0 - PROGRESS_GAIN)*best_resid) then
                best_resid = resid
                no_progress_count = 0
            else
                no_progress_count = no_progress_count + 1
            end if
            if (no_progress_count >= NO_PROGRESS_PATIENCE) then
                write (*, '(A,I0,A,ES10.3,A,I0)') 'BiCGSTAB no-progress detected: gain=', int(PROGRESS_GAIN*1000.0d0), &
                    'e-3, best_resid=', best_resid, ', patience=', NO_PROGRESS_PATIENCE
                self%current_iteration = iter
                self%status = SOLVER_STATUS%MAXITER%ID
                if (has_internal_x) call x%copy(self%x)
                return
            end if

            if (resid >= STAGNATION_RATIO*resid_prev) then
                stagnant_count = stagnant_count + 1
            else
                stagnant_count = 0
            end if
            resid_prev = resid
            if (stagnant_count >= STAGNATION_PATIENCE) then
                write (*, '(A,I0,A,ES10.3)') 'BiCGSTAB stagnation detected: patience=', STAGNATION_PATIENCE, &
                    ', ratio=', STAGNATION_RATIO
                self%current_iteration = iter
                self%status = SOLVER_STATUS%MAXITER%ID
                if (has_internal_x) call x%copy(self%x)
                return
            end if

            rho_old = rho

            if (was_interrupted()) stop
        end do
        self%current_iteration = iter
        self%status = SOLVER_STATUS%MAXITER%ID
        if (has_internal_x) call x%copy(self%x)

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
end submodule impl_solve_type_solver_bicgstab

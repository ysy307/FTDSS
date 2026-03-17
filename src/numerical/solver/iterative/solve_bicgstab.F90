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
        self%projection_enabled = solver_settings%projection_enabled
        self%projection_offset = solver_settings%projection_offset
        self%projection_stride = solver_settings%projection_stride

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
        real(real64) :: resid
        real(real64) :: norm_r, norm_v, norm_t, norm_s, norm_p
        real(real64) :: norm_vinf, norm_phat, norm_shat, norm_tinf
        integer(int32) :: iter

        real(real64), dimension(:), pointer :: b_ptr, r_ptr, x_internal_ptr
        logical :: has_internal_x
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
        ! 2: Set an initial value x0 (use caller-provided initial guess)
        ! ==========================================================
        call self%x%copy(x)
        call project_component_mean_zero(self%x, self%projection_enabled, self%projection_offset, self%projection_stride)
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
        call project_component_mean_zero(self%r, self%projection_enabled, self%projection_offset, self%projection_stride)

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
                if (has_internal_x) call x%copy(self%x)
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if

            if (iter == 1) then
                ! 10: p0 = r0
                call self%p%copy(self%r)
                call project_component_mean_zero(self%p, self%projection_enabled, self%projection_offset, self%projection_stride)
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
                norm_vinf = vector_norminf(self%v)
                if (.not. ieee_is_finite(omega) .or. .not. ieee_is_finite(norm_vinf)) then
                    write (*, '(A,I0,2(A,ES13.5))') 'BiCG breakdown(non-finite p-update): iter=', iter, &
                        ', omega=', omega, ', ||v||inf=', norm_vinf
                    self%current_iteration = iter
                    self%status = SOLVER_STATUS%BREAKDOWN%ID
                    if (has_internal_x) call x%copy(self%x)
                    return
                end if
                if (norm_vinf >= 1.0d0) then
                    if (abs(omega) > huge(1.0d0) / norm_vinf) then
                        write (*, '(A,I0,2(A,ES13.5))') 'BiCG overflow(p-update): iter=', iter, &
                            ', omega=', omega, ', ||v||inf=', norm_vinf
                        self%current_iteration = iter
                        self%status = SOLVER_STATUS%BREAKDOWN%ID
                        if (has_internal_x) call x%copy(self%x)
                        return
                    end if
                end if
                call vector_axpy(-omega, self%v, self%p)
                call vector_scale(beta, self%p)
                call vector_axpy(1.0d0, self%r, self%p)
                call project_component_mean_zero(self%p, self%projection_enabled, self%projection_offset, self%projection_stride)
            end if
            ! 15: phat = M^-1 * p
            call self%pc%apply(self%p, self%phat)
            call project_component_mean_zero(self%phat, self%projection_enabled, self%projection_offset, self%projection_stride)
            ! 16: v = A * phat
            call matvec(A, self%phat, self%v, ierr)
            call project_component_mean_zero(self%v, self%projection_enabled, self%projection_offset, self%projection_stride)
            ! 17: alpha_k = rho / (^r0, v)
            denom = vector_dot(self%r0, self%v)
            if (abs(denom) <= tiny(1.0d0)) then
                norm_r = vector_norm2(self%r)
                norm_v = vector_norm2(self%v)
                write (*, '(A,I0,2(A,ES13.5))') 'BiCGSTAB breakdown(alpha): iter=', iter, ', dot(r0,v)=', denom, ', ||r||2=', norm_r
                write (*, '(A,ES13.5,A,ES13.5)') '  rho=', rho, ', ||v||2=', norm_v
                self%current_iteration = iter
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                if (has_internal_x) call x%copy(self%x)
                return
            end if
            alpha = rho / denom

            norm_vinf = vector_norminf(self%v)
            if (.not. ieee_is_finite(alpha) .or. .not. ieee_is_finite(norm_vinf)) then
                write (*, '(A,I0,2(A,ES13.5))') 'BiCG breakdown(non-finite alpha-step): iter=', iter, &
                    ', alpha=', alpha, ', ||v||inf=', norm_vinf
                self%current_iteration = iter
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                if (has_internal_x) call x%copy(self%x)
                return
            end if
            if (norm_vinf >= 1.0d0) then
                if (abs(alpha) > huge(1.0d0) / norm_vinf) then
                    write (*, '(A,I0,2(A,ES13.5))') 'BiCG overflow(alpha-step): iter=', iter, &
                        ', alpha=', alpha, ', ||v||inf=', norm_vinf
                    self%current_iteration = iter
                    self%status = SOLVER_STATUS%BREAKDOWN%ID
                    if (has_internal_x) call x%copy(self%x)
                    return
                end if
            end if
            ! 18: s = r_k - alpha_k * v
            call vector_axpyz(-alpha, self%v, self%r, self%s)
            call project_component_mean_zero(self%s, self%projection_enabled, self%projection_offset, self%projection_stride)

            ! 19: shat = M^-1 * s
            call self%pc%apply(self%s, self%shat)
            call project_component_mean_zero(self%shat, self%projection_enabled, self%projection_offset, self%projection_stride)
            ! 20: t = A * shat
            call matvec(A, self%shat, self%t, ierr)
            call project_component_mean_zero(self%t, self%projection_enabled, self%projection_offset, self%projection_stride)

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
            if (omega == 0.0d0) then
                norm_t = vector_norm2(self%t)
                norm_s = vector_norm2(self%s)
                write (*, '(A,I0,2(A,ES13.5))') 'BiCGSTAB breakdown(omega): iter=', iter, ', ||t||2=', norm_t, ', ||s||2=', norm_s
                self%current_iteration = iter
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                if (has_internal_x) call x%copy(self%x)
                return
            end if

            norm_phat = vector_norminf(self%phat)
            norm_shat = vector_norminf(self%shat)
            norm_tinf = vector_norminf(self%t)
            if (.not. ieee_is_finite(norm_phat) .or. .not. ieee_is_finite(norm_shat) .or. .not. ieee_is_finite(norm_tinf)) then
                write (*, '(A,I0,3(A,ES13.5))') 'BiCG breakdown(non-finite update vectors): iter=', iter, &
                    ', ||phat||inf=', norm_phat, ', ||shat||inf=', norm_shat, ', ||t||inf=', norm_tinf
                self%current_iteration = iter
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                if (has_internal_x) call x%copy(self%x)
                return
            end if

            if (norm_phat >= 1.0d0) then
                if (abs(alpha) > huge(1.0d0) / norm_phat) then
                    write (*, '(A,I0,2(A,ES13.5))') 'BiCG overflow(x-update alpha): iter=', iter, &
                        ', alpha=', alpha, ', ||phat||inf=', norm_phat
                    self%current_iteration = iter
                    self%status = SOLVER_STATUS%BREAKDOWN%ID
                    if (has_internal_x) call x%copy(self%x)
                    return
                end if
            end if

            if (norm_shat >= 1.0d0) then
                if (abs(omega) > huge(1.0d0) / norm_shat) then
                    write (*, '(A,I0,2(A,ES13.5))') 'BiCG overflow(x-update omega): iter=', iter, &
                        ', omega=', omega, ', ||shat||inf=', norm_shat
                    self%current_iteration = iter
                    self%status = SOLVER_STATUS%BREAKDOWN%ID
                    if (has_internal_x) call x%copy(self%x)
                    return
                end if
            end if

            if (norm_tinf >= 1.0d0) then
                if (abs(omega) > huge(1.0d0) / norm_tinf) then
                    write (*, '(A,I0,2(A,ES13.5))') 'BiCG overflow(r-update omega): iter=', iter, &
                        ', omega=', omega, ', ||t||inf=', norm_tinf
                    self%current_iteration = iter
                    self%status = SOLVER_STATUS%BREAKDOWN%ID
                    if (has_internal_x) call x%copy(self%x)
                    return
                end if
            end if

            ! 23: x(i) = x(i-1) + alpha * M^-1 p(i-1) + omega * M^-1 s(i)
            ! 24: r(i) = s(i-1) - omega * AM^-1 s(i-1)
            call vector_axpy(alpha, self%phat, self%x)
            call vector_axpy(omega, self%shat, self%x)
            call project_component_mean_zero(self%x, self%projection_enabled, self%projection_offset, self%projection_stride)
            call vector_axpyz(-omega, self%t, self%s, self%r)
            call project_component_mean_zero(self%r, self%projection_enabled, self%projection_offset, self%projection_stride)

            ! 25: ||r_k+1||_2
            resid = vector_norm2(self%r)
            call self%residual_history%set(MATRIX_OPS%INS, iter, resid)
            if (resid < self%tolerance) then
                self%current_iteration = iter
                self%status = SOLVER_STATUS%SUCCESS%ID
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

    subroutine project_component_mean_zero(vec, enabled, offset, stride)
        implicit none
        type(type_vector_dp), intent(inout) :: vec
        logical, intent(in) :: enabled
        integer(int32), intent(in) :: offset, stride

        real(real64), pointer :: data(:)
        real(real64) :: mean_val
        integer(int32) :: i, count, first_idx

        if (.not. enabled) return
        if (stride <= 0 .or. offset <= 0) return

        data => vec%get_data()
        if (.not. associated(data)) return

        first_idx = offset
        if (first_idx > size(data)) return

        mean_val = 0.0d0
        count = 0
        do i = first_idx, size(data), stride
            mean_val = mean_val + data(i)
            count = count + 1
        end do
        if (count <= 0) return

        mean_val = mean_val/real(count, real64)
        do i = first_idx, size(data), stride
            data(i) = data(i) - mean_val
        end do

        nullify (data)
    end subroutine project_component_mean_zero

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

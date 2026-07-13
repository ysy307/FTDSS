!> BiCRSafe solver — breakdown-free BiCRSTAB variant.
!> CR analogue of BiCGSafe.
submodule(numerical_solver_interface) impl_solve_type_solver_bicrsafe
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none
contains

    module subroutine initialize_type_solver_bicrsafe(self, solver_settings, preconditioner_settings)
        implicit none
        class(type_solver_bicrsafe), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        integer(int32) :: ierr

        self%ID = solver_settings%ID
        self%name = "BiCRSafe"
        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%max_iterations = solver_settings%max_iterations

        call self%r%initialize(self%num_nodes)
        call self%r0%initialize(self%num_nodes)
        call self%p%initialize(self%num_nodes)
        call self%phat%initialize(self%num_nodes)
        call self%s%initialize(self%num_nodes)
        call self%shat%initialize(self%num_nodes)
        call self%t%initialize(self%num_nodes)
        call self%v%initialize(self%num_nodes)
        call self%Ar%initialize(self%num_nodes)
        call self%residual_history%initialize(self%max_iterations)
        self%current_iteration = 0

        self%status = SOLVER_STATUS%SUCCESS%ID
        call create_preconditioner(self%pc, preconditioner_settings, ierr)
        self%status = ierr

    end subroutine initialize_type_solver_bicrsafe

    module subroutine solve_type_solver_bicrsafe(self, A, b, x)
        implicit none
        class(type_solver_bicrsafe), intent(inout) :: self
        class(abst_matrix), intent(in) :: A
        type(type_vector_dp), intent(in) :: b
        type(type_vector_dp), intent(inout) :: x

        real(real64) :: rho, rho_old, alpha, beta, omega, denom
        real(real64) :: resid, resid0
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
        call self%p%zero()
        call self%v%zero()

        ! Ar = A*r
        call matvec(A, self%r, self%Ar, ierr)

        rho = 1.0d0
        rho_old = 1.0d0
        alpha = 1.0d0
        omega = 1.0d0

        do iter = 1, self%max_iterations
            self%current_iteration = iter

            rho = vector_dot(self%r0, self%Ar)

            ! Breakdown avoidance
            if (abs(rho) < 1.0d-30 * resid0**2) then
                call self%r0%copy(self%r)
                call matvec(A, self%r, self%Ar, ierr)
                rho = vector_dot(self%r0, self%Ar)
                call self%p%copy(self%r)
                rho_old = rho
                alpha = 1.0d0
                omega = 1.0d0
            end if

            if (iter == 1) then
                call self%p%copy(self%r)
            else
                if (abs(rho_old) <= tiny(1.0d0) .or. abs(omega) <= tiny(1.0d0)) then
                    self%status = SOLVER_STATUS%BREAKDOWN%ID
                    return
                end if
                beta = (rho / rho_old) * (alpha / omega)
                call vector_axpy(-omega, self%v, self%p)
                call vector_scale(beta, self%p)
                call vector_axpy(1.0d0, self%r, self%p)
            end if

            ! phat = M^{-1} p
            call self%pc%apply(self%p, self%phat)
            ! v = A * phat
            call matvec(A, self%phat, self%v, ierr)

            denom = vector_dot(self%r0, self%v)
            if (abs(denom) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            alpha = rho / denom

            ! s = r - alpha * v
            call vector_axpyz(-alpha, self%v, self%r, self%s)

            resid = vector_norm2(self%s)
            if (resid < self%tolerance) then
                call vector_axpy(alpha, self%phat, x)
                call self%residual_history%set(MATRIX_OPS%INS, iter, resid)
                self%status = SOLVER_STATUS%SUCCESS%ID
                return
            end if

            ! shat = M^{-1} s
            call self%pc%apply(self%s, self%shat)
            ! t = A * shat
            call matvec(A, self%shat, self%t, ierr)

            denom = vector_dot(self%t, self%t)
            if (abs(denom) <= tiny(1.0d0)) then
                call vector_axpy(alpha, self%phat, x)
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            omega = vector_dot(self%t, self%s) / denom

            ! x = x + alpha * phat + omega * shat
            call vector_axpy(alpha, self%phat, x)
            call vector_axpy(omega, self%shat, x)

            ! r = s - omega * t
            call vector_axpyz(-omega, self%t, self%s, self%r)

            resid = vector_norm2(self%r)
            call self%residual_history%set(MATRIX_OPS%INS, iter, resid)

            if (resid < self%tolerance .or. &
                (resid0 > self%tolerance .and. resid < self%relative_tolerance * resid0)) then
                self%status = SOLVER_STATUS%SUCCESS%ID
                return
            end if

            ! Ar = A*r
            call matvec(A, self%r, self%Ar, ierr)

            rho_old = rho

            if (was_interrupted()) stop
        end do

        self%status = SOLVER_STATUS%MAXITER%ID

    end subroutine solve_type_solver_bicrsafe

    module subroutine destroy_type_solver_bicrsafe(self)
        implicit none
        class(type_solver_bicrsafe), intent(inout) :: self

        self%ID = -1
        if (allocated(self%name)) deallocate (self%name)
        call self%r%destroy()
        call self%r0%destroy()
        call self%p%destroy()
        call self%phat%destroy()
        call self%s%destroy()
        call self%shat%destroy()
        call self%t%destroy()
        call self%v%destroy()
        call self%Ar%destroy()
        if (allocated(self%pc)) then
            call self%pc%destroy()
            deallocate (self%pc)
        end if
        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine destroy_type_solver_bicrsafe

end submodule impl_solve_type_solver_bicrsafe

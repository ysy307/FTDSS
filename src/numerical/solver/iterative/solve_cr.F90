!> Conjugate Residual (CR) solver for symmetric systems (not necessarily SPD).
!> Minimizes ||r||_2 over the Krylov subspace.
submodule(numerical_solver_interface) impl_solve_type_solver_cr
    implicit none
contains

    module subroutine initialize_type_solver_cr(self, solver_settings, preconditioner_settings)
        implicit none
        class(type_solver_cr), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        integer(int32) :: ierr

        self%ID = solver_settings%ID
        self%name = "CR"
        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%max_iterations = solver_settings%max_iterations

        call self%r%initialize(self%num_nodes)
        call self%p%initialize(self%num_nodes)
        call self%q%initialize(self%num_nodes)
        call self%Ar%initialize(self%num_nodes)
        call self%Ap%initialize(self%num_nodes)
        call self%residual_history%initialize(self%max_iterations)
        self%current_iteration = 0

        self%status = SOLVER_STATUS%SUCCESS%ID
        call create_preconditioner(self%pc, preconditioner_settings, ierr)
        self%status = ierr

    end subroutine initialize_type_solver_cr

    !> Solve Ax = b using preconditioned CR.
    module subroutine solve_type_solver_cr(self, A, b, x)
        implicit none
        class(type_solver_cr), intent(inout) :: self
        class(abst_matrix), intent(in) :: A
        type(type_vector_dp), intent(in) :: b
        type(type_vector_dp), intent(inout) :: x

        real(real64) :: rAr, rAr_old, alpha, beta, denom, resid, resid0
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

        ! p = r
        call self%p%copy(self%r)

        ! Ar = A * r
        call matvec(A, self%r, self%Ar, ierr)

        ! Ap = Ar
        call self%Ap%copy(self%Ar)

        ! rAr = (r, Ar)
        rAr = vector_dot(self%r, self%Ar)

        do iter = 1, self%max_iterations
            self%current_iteration = iter

            ! alpha = (r, Ar) / (Ap, Ap)
            denom = vector_dot(self%Ap, self%Ap)
            if (abs(denom) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            alpha = rAr / denom

            ! x = x + alpha * p
            call vector_axpy(alpha, self%p, x)

            ! r = r - alpha * Ap
            call vector_axpy(-alpha, self%Ap, self%r)

            resid = vector_norm2(self%r)
            call self%residual_history%set(MATRIX_OPS%INS, iter, resid)

            if (resid < self%tolerance .or. &
                (resid0 > self%tolerance .and. resid < self%relative_tolerance * resid0)) then
                self%status = SOLVER_STATUS%SUCCESS%ID
                return
            end if

            ! Ar = A * r
            call matvec(A, self%r, self%Ar, ierr)

            rAr_old = rAr
            rAr = vector_dot(self%r, self%Ar)

            if (abs(rAr_old) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            beta = rAr / rAr_old

            ! p = r + beta * p
            call vector_scale(beta, self%p)
            call vector_axpy(1.0d0, self%r, self%p)

            ! Ap = Ar + beta * Ap
            call vector_scale(beta, self%Ap)
            call vector_axpy(1.0d0, self%Ar, self%Ap)

            if (was_interrupted()) stop
        end do

        self%status = SOLVER_STATUS%MAXITER%ID

    end subroutine solve_type_solver_cr

    module subroutine destroy_type_solver_cr(self)
        implicit none
        class(type_solver_cr), intent(inout) :: self

        self%ID = -1
        if (allocated(self%name)) deallocate (self%name)
        call self%r%destroy()
        call self%p%destroy()
        call self%q%destroy()
        call self%Ar%destroy()
        call self%Ap%destroy()
        if (allocated(self%pc)) then
            call self%pc%destroy()
            deallocate (self%pc)
        end if
        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine destroy_type_solver_cr

end submodule impl_solve_type_solver_cr

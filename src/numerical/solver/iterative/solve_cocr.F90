!> COCR (Conjugate Orthogonal Conjugate Residual) solver.
!> For complex symmetric systems. In real arithmetic, reduces to CR.
!> Kept for LIS API compatibility.
submodule(numerical_solver_interface) impl_solve_type_solver_cocr
    implicit none
contains

    module subroutine initialize_type_solver_cocr(self, solver_settings, preconditioner_settings)
        implicit none
        class(type_solver_cocr), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        integer(int32) :: ierr

        self%ID = solver_settings%ID
        self%name = "COCR"
        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%max_iterations = solver_settings%max_iterations

        call self%r%initialize(self%num_nodes)
        call self%p%initialize(self%num_nodes)
        call self%Ar%initialize(self%num_nodes)
        call self%Ap%initialize(self%num_nodes)
        call self%residual_history%initialize(self%max_iterations)
        self%current_iteration = 0

        self%status = SOLVER_STATUS%SUCCESS%ID
        call create_preconditioner(self%pc, preconditioner_settings, ierr)
        self%status = ierr

    end subroutine initialize_type_solver_cocr

    !> For real-valued systems, COCR is identical to CR.
    module subroutine solve_type_solver_cocr(self, A, b, x)
        implicit none
        class(type_solver_cocr), intent(inout) :: self
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

        call self%p%copy(self%r)
        call matvec(A, self%r, self%Ar, ierr)
        call self%Ap%copy(self%Ar)

        rAr = vector_dot(self%r, self%Ar)

        do iter = 1, self%max_iterations
            self%current_iteration = iter

            denom = vector_dot(self%Ap, self%Ap)
            if (abs(denom) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            alpha = rAr / denom

            call vector_axpy(alpha, self%p, x)
            call vector_axpy(-alpha, self%Ap, self%r)

            resid = vector_norm2(self%r)
            call self%residual_history%set(MATRIX_OPS%INS, iter, resid)

            if (resid < self%tolerance .or. &
                (resid0 > self%tolerance .and. resid < self%relative_tolerance * resid0)) then
                self%status = SOLVER_STATUS%SUCCESS%ID
                return
            end if

            call matvec(A, self%r, self%Ar, ierr)

            rAr_old = rAr
            rAr = vector_dot(self%r, self%Ar)

            if (abs(rAr_old) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            beta = rAr / rAr_old

            call vector_scale(beta, self%p)
            call vector_axpy(1.0d0, self%r, self%p)

            call vector_scale(beta, self%Ap)
            call vector_axpy(1.0d0, self%Ar, self%Ap)

            if (was_interrupted()) stop
        end do

        self%status = SOLVER_STATUS%MAXITER%ID

    end subroutine solve_type_solver_cocr

    module subroutine destroy_type_solver_cocr(self)
        implicit none
        class(type_solver_cocr), intent(inout) :: self

        self%ID = -1
        if (allocated(self%name)) deallocate (self%name)
        call self%r%destroy()
        call self%p%destroy()
        call self%Ar%destroy()
        call self%Ap%destroy()
        if (allocated(self%pc)) then
            call self%pc%destroy()
            deallocate (self%pc)
        end if
        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine destroy_type_solver_cocr

end submodule impl_solve_type_solver_cocr

!> Conjugate Gradient (CG) solver for symmetric positive definite systems.
!> Right-preconditioned: solve A M^{-1} (Mz) = b.
submodule(numerical_solver_interface) impl_solve_type_solver_cg
    implicit none
contains

    module subroutine initialize_type_solver_cg(self, solver_settings, preconditioner_settings)
        implicit none
        class(type_solver_cg), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        integer(int32) :: ierr

        self%ID = solver_settings%ID
        self%name = "CG"
        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%max_iterations = solver_settings%max_iterations

        call self%r%initialize(self%num_nodes)
        call self%z%initialize(self%num_nodes)
        call self%p%initialize(self%num_nodes)
        call self%q%initialize(self%num_nodes)
        call self%residual_history%initialize(self%max_iterations)
        self%current_iteration = 0

        self%status = SOLVER_STATUS%SUCCESS%ID
        call create_preconditioner(self%pc, preconditioner_settings, ierr)
        self%status = ierr

    end subroutine initialize_type_solver_cg

    !> Solve Ax = b using preconditioned CG.
    !> Algorithm: standard PCG with M^{-1} as preconditioner.
    module subroutine solve_type_solver_cg(self, A, b, x)
        implicit none
        class(type_solver_cg), intent(inout) :: self
        class(abst_matrix), intent(in) :: A
        type(type_vector_dp), intent(in) :: b
        type(type_vector_dp), intent(inout) :: x

        real(real64) :: rho, rho_old, alpha, beta, denom, resid, resid0
        integer(int32) :: iter, ierr

        call self%residual_history%zero()

        ! Setup preconditioner
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

        ! z = M^{-1} r
        call self%pc%apply(self%r, self%z)

        ! p = z
        call self%p%copy(self%z)

        ! rho = (r, z)
        rho = vector_dot(self%r, self%z)

        do iter = 1, self%max_iterations
            self%current_iteration = iter

            ! q = A * p
            call matvec(A, self%p, self%q, ierr)

            ! alpha = rho / (p, q)
            denom = vector_dot(self%p, self%q)
            if (abs(denom) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            alpha = rho / denom

            ! x = x + alpha * p
            call vector_axpy(alpha, self%p, x)

            ! r = r - alpha * q
            call vector_axpy(-alpha, self%q, self%r)

            resid = vector_norm2(self%r)
            call self%residual_history%set(MATRIX_OPS%INS, iter, resid)

            if (resid < self%tolerance .or. &
                (resid0 > self%tolerance .and. resid < self%relative_tolerance * resid0)) then
                self%status = SOLVER_STATUS%SUCCESS%ID
                return
            end if

            ! z = M^{-1} r
            call self%pc%apply(self%r, self%z)

            rho_old = rho
            rho = vector_dot(self%r, self%z)

            if (abs(rho_old) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            beta = rho / rho_old

            ! p = z + beta * p
            call vector_scale(beta, self%p)
            call vector_axpy(1.0d0, self%z, self%p)

            if (was_interrupted()) stop
        end do

        self%status = SOLVER_STATUS%MAXITER%ID

    end subroutine solve_type_solver_cg

    module subroutine destroy_type_solver_cg(self)
        implicit none
        class(type_solver_cg), intent(inout) :: self

        self%ID = -1
        if (allocated(self%name)) deallocate (self%name)
        call self%r%destroy()
        call self%z%destroy()
        call self%p%destroy()
        call self%q%destroy()
        if (allocated(self%pc)) then
            call self%pc%destroy()
            deallocate (self%pc)
        end if
        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine destroy_type_solver_cg

end submodule impl_solve_type_solver_cg

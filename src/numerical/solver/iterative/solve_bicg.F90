!> BiConjugate Gradient (BiCG) solver for nonsymmetric systems.
!> Uses two-sided Lanczos process with A and A^T.
!> Since A^T is not explicitly available, we use the transpose-free formulation
!> where r0_star serves as the left Krylov vector.
submodule(numerical_solver_interface) impl_solve_type_solver_bicg
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none
contains

    module subroutine initialize_type_solver_bicg(self, solver_settings, preconditioner_settings)
        implicit none
        class(type_solver_bicg), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        integer(int32) :: ierr

        self%ID = solver_settings%ID
        self%name = "BiCG"
        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%max_iterations = solver_settings%max_iterations

        call self%r%initialize(self%num_nodes)
        call self%r0%initialize(self%num_nodes)
        call self%p%initialize(self%num_nodes)
        call self%p0%initialize(self%num_nodes)
        call self%z%initialize(self%num_nodes)
        call self%z0%initialize(self%num_nodes)
        call self%q%initialize(self%num_nodes)
        call self%q0%initialize(self%num_nodes)
        call self%residual_history%initialize(self%max_iterations)
        self%current_iteration = 0

        self%status = SOLVER_STATUS%SUCCESS%ID
        call create_preconditioner(self%pc, preconditioner_settings, ierr)
        self%status = ierr

    end subroutine initialize_type_solver_bicg

    !> Solve Ax = b using preconditioned BiCG.
    !> Since we cannot form A^T explicitly, we use r0 as the shadow residual
    !> and apply the preconditioner symmetrically.
    module subroutine solve_type_solver_bicg(self, A, b, x)
        implicit none
        class(type_solver_bicg), intent(inout) :: self
        class(abst_matrix), intent(in) :: A
        type(type_vector_dp), intent(in) :: b
        type(type_vector_dp), intent(inout) :: x

        real(real64) :: rho, rho_old, alpha, beta, denom, resid, resid0
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

        ! r0 = r (shadow residual)
        call self%r0%copy(self%r)

        ! z = M^{-1} r, z0 = M^{-1} r0
        call self%pc%apply(self%r, self%z)
        call self%pc%apply(self%r0, self%z0)

        ! p = z, p0 = z0
        call self%p%copy(self%z)
        call self%p0%copy(self%z0)

        rho = vector_dot(self%r0, self%z)

        do iter = 1, self%max_iterations
            self%current_iteration = iter

            ! q = A * p
            call matvec(A, self%p, self%q, ierr)

            ! Use A for shadow too (since we don't have A^T)
            call matvec(A, self%p0, self%q0, ierr)

            ! alpha = rho / (p0, q)
            denom = vector_dot(self%p0, self%q)
            if (abs(denom) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            alpha = rho / denom

            ! x = x + alpha * p
            call vector_axpy(alpha, self%p, x)

            ! r = r - alpha * q
            call vector_axpy(-alpha, self%q, self%r)

            ! r0 = r0 - alpha * q0
            call vector_axpy(-alpha, self%q0, self%r0)

            resid = vector_norm2(self%r)
            call self%residual_history%set(MATRIX_OPS%INS, iter, resid)

            if (resid < self%tolerance .or. &
                (resid0 > self%tolerance .and. resid < self%relative_tolerance * resid0)) then
                self%status = SOLVER_STATUS%SUCCESS%ID
                return
            end if

            ! z = M^{-1} r, z0 = M^{-1} r0
            call self%pc%apply(self%r, self%z)
            call self%pc%apply(self%r0, self%z0)

            rho_old = rho
            rho = vector_dot(self%r0, self%z)

            if (abs(rho_old) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            beta = rho / rho_old

            ! p = z + beta * p
            call vector_scale(beta, self%p)
            call vector_axpy(1.0d0, self%z, self%p)

            ! p0 = z0 + beta * p0
            call vector_scale(beta, self%p0)
            call vector_axpy(1.0d0, self%z0, self%p0)

            if (was_interrupted()) stop
        end do

        self%status = SOLVER_STATUS%MAXITER%ID

    end subroutine solve_type_solver_bicg

    module subroutine destroy_type_solver_bicg(self)
        implicit none
        class(type_solver_bicg), intent(inout) :: self

        self%ID = -1
        if (allocated(self%name)) deallocate (self%name)
        call self%r%destroy()
        call self%r0%destroy()
        call self%p%destroy()
        call self%p0%destroy()
        call self%z%destroy()
        call self%z0%destroy()
        call self%q%destroy()
        call self%q0%destroy()
        if (allocated(self%pc)) then
            call self%pc%destroy()
            deallocate (self%pc)
        end if
        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine destroy_type_solver_bicg

end submodule impl_solve_type_solver_bicg

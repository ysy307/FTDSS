!> BiConjugate Residual (BiCR) solver for nonsymmetric systems.
!> CR analogue of BiCG. Uses shadow residual r0* for bi-orthogonalization.
submodule(numerical_solver_interface) impl_solve_type_solver_bicr
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none
contains

    module subroutine initialize_type_solver_bicr(self, solver_settings, preconditioner_settings)
        implicit none
        class(type_solver_bicr), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        integer(int32) :: ierr

        self%ID = solver_settings%ID
        self%name = "BiCR"
        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%max_iterations = solver_settings%max_iterations

        call self%r%initialize(self%num_nodes)
        call self%r0%initialize(self%num_nodes)
        call self%p%initialize(self%num_nodes)
        call self%p0%initialize(self%num_nodes)
        call self%Ap%initialize(self%num_nodes)
        call self%Ap0%initialize(self%num_nodes)
        call self%Ar%initialize(self%num_nodes)
        call self%Ar0%initialize(self%num_nodes)
        call self%residual_history%initialize(self%max_iterations)
        self%current_iteration = 0

        self%status = SOLVER_STATUS%SUCCESS%ID
        call create_preconditioner(self%pc, preconditioner_settings, ierr)
        self%status = ierr

    end subroutine initialize_type_solver_bicr

    module subroutine solve_type_solver_bicr(self, A, b, x)
        implicit none
        class(type_solver_bicr), intent(inout) :: self
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

        ! r0 = r (shadow)
        call self%r0%copy(self%r)

        ! p = r, p0 = r0
        call self%p%copy(self%r)
        call self%p0%copy(self%r0)

        ! Ar = A*r, Ar0 = A*r0
        call matvec(A, self%r, self%Ar, ierr)
        call matvec(A, self%r0, self%Ar0, ierr)

        ! rho = (r0, Ar)
        rho = vector_dot(self%r0, self%Ar)

        do iter = 1, self%max_iterations
            self%current_iteration = iter

            ! Ap = A*p
            call matvec(A, self%p, self%Ap, ierr)
            ! Ap0 = A*p0
            call matvec(A, self%p0, self%Ap0, ierr)

            ! alpha = rho / (p0, Ap)
            denom = vector_dot(self%p0, self%Ap)
            if (abs(denom) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            alpha = rho / denom

            ! x = x + alpha * p
            call vector_axpy(alpha, self%p, x)

            ! r = r - alpha * Ap
            call vector_axpy(-alpha, self%Ap, self%r)

            ! r0 = r0 - alpha * Ap0
            call vector_axpy(-alpha, self%Ap0, self%r0)

            resid = vector_norm2(self%r)
            call self%residual_history%set(MATRIX_OPS%INS, iter, resid)

            if (resid < self%tolerance .or. &
                (resid0 > self%tolerance .and. resid < self%relative_tolerance * resid0)) then
                self%status = SOLVER_STATUS%SUCCESS%ID
                return
            end if

            ! Ar = A*r, Ar0 = A*r0
            call matvec(A, self%r, self%Ar, ierr)
            call matvec(A, self%r0, self%Ar0, ierr)

            rho_old = rho
            rho = vector_dot(self%r0, self%Ar)

            if (abs(rho_old) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            beta = rho / rho_old

            ! p = r + beta * p
            call vector_scale(beta, self%p)
            call vector_axpy(1.0d0, self%r, self%p)

            ! p0 = r0 + beta * p0
            call vector_scale(beta, self%p0)
            call vector_axpy(1.0d0, self%r0, self%p0)

            if (was_interrupted()) stop
        end do

        self%status = SOLVER_STATUS%MAXITER%ID

    end subroutine solve_type_solver_bicr

    module subroutine destroy_type_solver_bicr(self)
        implicit none
        class(type_solver_bicr), intent(inout) :: self

        self%ID = -1
        if (allocated(self%name)) deallocate (self%name)
        call self%r%destroy()
        call self%r0%destroy()
        call self%p%destroy()
        call self%p0%destroy()
        call self%Ap%destroy()
        call self%Ap0%destroy()
        call self%Ar%destroy()
        call self%Ar0%destroy()
        if (allocated(self%pc)) then
            call self%pc%destroy()
            deallocate (self%pc)
        end if
        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine destroy_type_solver_bicr

end submodule impl_solve_type_solver_bicr

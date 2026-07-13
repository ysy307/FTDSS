!> COCG (Conjugate Orthogonal Conjugate Gradient) solver.
!> For complex symmetric systems, but here implemented for real symmetric
!> as it reduces to standard CG. Kept for LIS API compatibility.
submodule(numerical_solver_interface) impl_solve_type_solver_cocg
    implicit none
contains

    module subroutine initialize_type_solver_cocg(self, solver_settings, preconditioner_settings)
        implicit none
        class(type_solver_cocg), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        integer(int32) :: ierr

        self%ID = solver_settings%ID
        self%name = "COCG"
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

    end subroutine initialize_type_solver_cocg

    !> For real-valued systems, COCG is identical to CG.
    module subroutine solve_type_solver_cocg(self, A, b, x)
        implicit none
        class(type_solver_cocg), intent(inout) :: self
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

        call self%pc%apply(self%r, self%z)
        call self%p%copy(self%z)
        rho = vector_dot(self%r, self%z)

        do iter = 1, self%max_iterations
            self%current_iteration = iter

            call matvec(A, self%p, self%q, ierr)

            denom = vector_dot(self%p, self%q)
            if (abs(denom) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            alpha = rho / denom

            call vector_axpy(alpha, self%p, x)
            call vector_axpy(-alpha, self%q, self%r)

            resid = vector_norm2(self%r)
            call self%residual_history%set(MATRIX_OPS%INS, iter, resid)

            if (resid < self%tolerance .or. &
                (resid0 > self%tolerance .and. resid < self%relative_tolerance * resid0)) then
                self%status = SOLVER_STATUS%SUCCESS%ID
                return
            end if

            call self%pc%apply(self%r, self%z)

            rho_old = rho
            rho = vector_dot(self%r, self%z)

            if (abs(rho_old) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            beta = rho / rho_old

            call vector_scale(beta, self%p)
            call vector_axpy(1.0d0, self%z, self%p)

            if (was_interrupted()) stop
        end do

        self%status = SOLVER_STATUS%MAXITER%ID

    end subroutine solve_type_solver_cocg

    module subroutine destroy_type_solver_cocg(self)
        implicit none
        class(type_solver_cocg), intent(inout) :: self

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

    end subroutine destroy_type_solver_cocg

end submodule impl_solve_type_solver_cocg

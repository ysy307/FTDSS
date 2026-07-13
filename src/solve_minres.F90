!> MINRES solver for symmetric (possibly indefinite) systems.
!> Minimizes ||r||_2 over the Krylov subspace using Lanczos tridiagonalization.
submodule(numerical_solver_interface) impl_solve_type_solver_minres
    implicit none
contains

    module subroutine initialize_type_solver_minres(self, solver_settings, preconditioner_settings)
        implicit none
        class(type_solver_minres), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        integer(int32) :: ierr

        self%ID = solver_settings%ID
        self%name = "MINRES"
        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%max_iterations = solver_settings%max_iterations

        call self%v%initialize(self%num_nodes)
        call self%v_old%initialize(self%num_nodes)
        call self%v_new%initialize(self%num_nodes)
        call self%w%initialize(self%num_nodes)
        call self%w_old%initialize(self%num_nodes)
        call self%w_new%initialize(self%num_nodes)
        call self%residual_history%initialize(self%max_iterations)
        self%current_iteration = 0

        self%status = SOLVER_STATUS%SUCCESS%ID
        call create_preconditioner(self%pc, preconditioner_settings, ierr)
        self%status = ierr

    end subroutine initialize_type_solver_minres

    !> Solve Ax = b using MINRES.
    !> Uses three-term Lanczos recurrence with Givens rotations.
    module subroutine solve_type_solver_minres(self, A, b, x)
        implicit none
        class(type_solver_minres), intent(inout) :: self
        class(abst_matrix), intent(in) :: A
        type(type_vector_dp), intent(in) :: b
        type(type_vector_dp), intent(inout) :: x

        real(real64) :: beta_cur, beta_old, alpha_cur
        real(real64) :: c_old, c_cur, s_old, s_cur
        real(real64) :: r1, r2, r3, eta, resid, resid0
        real(real64) :: temp, denom
        integer(int32) :: iter, ierr

        call self%residual_history%zero()
        call self%pc%setup(A)

        ! r = b - Ax => stored in v_new temporarily
        call matvec(A, x, self%v_new, ierr)
        call vector_axpyz(-1.0d0, self%v_new, b, self%v_new)

        beta_cur = vector_norm2(self%v_new)
        resid0 = beta_cur
        call self%residual_history%set(MATRIX_OPS%INS, 1, resid0)
        if (resid0 < self%tolerance) then
            self%current_iteration = 0
            self%status = SOLVER_STATUS%SUCCESS%ID
            return
        end if

        ! v1 = r / beta
        call self%v%copy(self%v_new)
        call vector_scale(1.0d0 / beta_cur, self%v)

        ! Initialize
        call self%v_old%zero()
        call self%w%zero()
        call self%w_old%zero()

        eta = beta_cur
        c_old = 1.0d0
        s_old = 0.0d0
        c_cur = 1.0d0
        s_cur = 0.0d0
        beta_old = 0.0d0

        do iter = 1, self%max_iterations
            self%current_iteration = iter

            ! v_new = A * v
            call matvec(A, self%v, self%v_new, ierr)

            ! alpha = (v, v_new)
            alpha_cur = vector_dot(self%v, self%v_new)

            ! v_new = v_new - alpha * v - beta * v_old
            call vector_axpy(-alpha_cur, self%v, self%v_new)
            call vector_axpy(-beta_cur, self%v_old, self%v_new)

            beta_old = beta_cur
            beta_cur = vector_norm2(self%v_new)

            ! Apply previous Givens rotations to get r1, r2, r3
            r1 = s_old * beta_old
            r2 = c_old * c_cur * beta_old + s_cur * alpha_cur
            r3 = -s_cur * beta_old * c_old + c_cur * alpha_cur

            ! Generate new Givens rotation for (r3, beta_cur)
            denom = sqrt(r3**2 + beta_cur**2)
            if (abs(denom) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if

            c_old = c_cur
            s_old = s_cur
            c_cur = r3 / denom
            s_cur = beta_cur / denom

            ! w_new = (v - r1 * w_old - r2 * w) / denom
            call self%w_new%copy(self%v)
            call vector_axpy(-r1, self%w_old, self%w_new)
            call vector_axpy(-r2, self%w, self%w_new)
            call vector_scale(1.0d0 / denom, self%w_new)

            ! x = x + c_cur * eta * w_new
            call vector_axpy(c_cur * eta, self%w_new, x)

            eta = -s_cur * eta

            resid = abs(eta)
            call self%residual_history%set(MATRIX_OPS%INS, iter, resid)

            if (resid < self%tolerance .or. &
                (resid0 > self%tolerance .and. resid < self%relative_tolerance * resid0)) then
                self%status = SOLVER_STATUS%SUCCESS%ID
                return
            end if

            ! Cycle: v_old <- v, v <- v_new / beta_cur
            call self%v_old%copy(self%v)
            if (abs(beta_cur) > tiny(1.0d0)) then
                call self%v%copy(self%v_new)
                call vector_scale(1.0d0 / beta_cur, self%v)
            end if

            ! w_old <- w, w <- w_new
            call self%w_old%copy(self%w)
            call self%w%copy(self%w_new)

            if (was_interrupted()) stop
        end do

        self%status = SOLVER_STATUS%MAXITER%ID

    end subroutine solve_type_solver_minres

    module subroutine destroy_type_solver_minres(self)
        implicit none
        class(type_solver_minres), intent(inout) :: self

        self%ID = -1
        if (allocated(self%name)) deallocate (self%name)
        call self%v%destroy()
        call self%v_old%destroy()
        call self%v_new%destroy()
        call self%w%destroy()
        call self%w_old%destroy()
        call self%w_new%destroy()
        if (allocated(self%pc)) then
            call self%pc%destroy()
            deallocate (self%pc)
        end if
        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine destroy_type_solver_minres

end submodule impl_solve_type_solver_minres

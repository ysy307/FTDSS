!> Orthomin(m) solver — truncated generalized conjugate residual method.
!> Minimizes ||r||_2 using m-term recurrence (like GCR with truncation).
submodule(numerical_solver_interface) impl_solve_type_solver_orthomin
    implicit none
contains

    module subroutine initialize_type_solver_orthomin(self, solver_settings, preconditioner_settings)
        implicit none
        class(type_solver_orthomin), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        integer(int32) :: ierr, i

        self%ID = solver_settings%ID
        self%name = "Orthomin"
        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%max_iterations = solver_settings%max_iterations
        self%m_restart = solver_settings%m_restart
        if (self%m_restart <= 0) self%m_restart = 10

        call self%r%initialize(self%num_nodes)
        call self%z%initialize(self%num_nodes)
        call self%Az%initialize(self%num_nodes)

        if (allocated(self%p)) deallocate (self%p)
        allocate (self%p(self%m_restart))
        do i = 1, self%m_restart
            call self%p(i)%initialize(self%num_nodes)
        end do

        if (allocated(self%Ap)) deallocate (self%Ap)
        allocate (self%Ap(self%m_restart))
        do i = 1, self%m_restart
            call self%Ap(i)%initialize(self%num_nodes)
        end do

        call self%residual_history%initialize(self%max_iterations)
        self%current_iteration = 0

        self%status = SOLVER_STATUS%SUCCESS%ID
        call create_preconditioner(self%pc, preconditioner_settings, ierr)
        self%status = ierr

    end subroutine initialize_type_solver_orthomin

    module subroutine solve_type_solver_orthomin(self, A, b, x)
        implicit none
        class(type_solver_orthomin), intent(inout) :: self
        class(abst_matrix), intent(in) :: A
        type(type_vector_dp), intent(in) :: b
        type(type_vector_dp), intent(inout) :: x

        real(real64) :: alpha, beta, denom, resid, resid0
        integer(int32) :: iter, j, k_idx, ierr, m

        m = self%m_restart

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

        do iter = 1, self%max_iterations
            self%current_iteration = iter
            k_idx = mod(iter - 1, m) + 1

            ! z = M^{-1} r
            call self%pc%apply(self%r, self%z)

            ! Az = A * z
            call matvec(A, self%z, self%Az, ierr)

            ! p_k = z, Ap_k = Az
            call self%p(k_idx)%copy(self%z)
            call self%Ap(k_idx)%copy(self%Az)

            ! Orthogonalize against previous directions
            do j = 1, min(iter - 1, m)
                if (j == k_idx) cycle
                denom = vector_dot(self%Ap(j), self%Ap(j))
                if (abs(denom) > tiny(1.0d0)) then
                    beta = vector_dot(self%Az, self%Ap(j)) / denom
                    call vector_axpy(-beta, self%p(j), self%p(k_idx))
                    call vector_axpy(-beta, self%Ap(j), self%Ap(k_idx))
                end if
            end do

            ! alpha = (r, Ap_k) / (Ap_k, Ap_k)
            denom = vector_dot(self%Ap(k_idx), self%Ap(k_idx))
            if (abs(denom) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            alpha = vector_dot(self%r, self%Ap(k_idx)) / denom

            ! x = x + alpha * p_k
            call vector_axpy(alpha, self%p(k_idx), x)

            ! r = r - alpha * Ap_k
            call vector_axpy(-alpha, self%Ap(k_idx), self%r)

            resid = vector_norm2(self%r)
            call self%residual_history%set(MATRIX_OPS%INS, iter, resid)

            if (resid < self%tolerance .or. &
                (resid0 > self%tolerance .and. resid < self%relative_tolerance * resid0)) then
                self%status = SOLVER_STATUS%SUCCESS%ID
                return
            end if

            if (was_interrupted()) stop
        end do

        self%status = SOLVER_STATUS%MAXITER%ID

    end subroutine solve_type_solver_orthomin

    module subroutine destroy_type_solver_orthomin(self)
        implicit none
        class(type_solver_orthomin), intent(inout) :: self
        integer(int32) :: i

        self%ID = -1
        if (allocated(self%name)) deallocate (self%name)
        call self%r%destroy()
        call self%z%destroy()
        call self%Az%destroy()
        if (allocated(self%p)) then
            do i = 1, size(self%p)
                call self%p(i)%destroy()
            end do
            deallocate (self%p)
        end if
        if (allocated(self%Ap)) then
            do i = 1, size(self%Ap)
                call self%Ap(i)%destroy()
            end do
            deallocate (self%Ap)
        end if
        if (allocated(self%pc)) then
            call self%pc%destroy()
            deallocate (self%pc)
        end if
        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine destroy_type_solver_orthomin

end submodule impl_solve_type_solver_orthomin

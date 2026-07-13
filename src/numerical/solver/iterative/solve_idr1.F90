!> IDR(1) solver — simplified IDR with s=1.
!> Equivalent to BiCGSTAB in exact arithmetic but with different implementation.
submodule(numerical_solver_interface) impl_solve_type_solver_idr1
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none
contains

    module subroutine initialize_type_solver_idr1(self, solver_settings, preconditioner_settings)
        implicit none
        class(type_solver_idr1), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        integer(int32) :: ierr

        self%ID = solver_settings%ID
        self%name = "IDR(1)"
        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%max_iterations = solver_settings%max_iterations

        call self%r%initialize(self%num_nodes)
        call self%r0%initialize(self%num_nodes)
        call self%v%initialize(self%num_nodes)
        call self%t%initialize(self%num_nodes)
        call self%dv%initialize(self%num_nodes)
        call self%dr%initialize(self%num_nodes)
        call self%z%initialize(self%num_nodes)
        call self%residual_history%initialize(self%max_iterations)
        self%current_iteration = 0

        self%status = SOLVER_STATUS%SUCCESS%ID
        call create_preconditioner(self%pc, preconditioner_settings, ierr)
        self%status = ierr

    end subroutine initialize_type_solver_idr1

    module subroutine solve_type_solver_idr1(self, A, b, x)
        implicit none
        class(type_solver_idr1), intent(inout) :: self
        class(abst_matrix), intent(in) :: A
        type(type_vector_dp), intent(in) :: b
        type(type_vector_dp), intent(inout) :: x

        real(real64) :: omega, resid, resid0, denom, gamma_val, mu, nr, nt
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

        ! Initial dv = 0, dr = r
        call self%dv%zero()
        call self%dr%copy(self%r)

        omega = 1.0d0

        do iter = 1, self%max_iterations
            self%current_iteration = iter

            ! gamma = -(r0, r) / (r0, dr)
            denom = vector_dot(self%r0, self%dr)
            if (abs(denom) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            gamma_val = -vector_dot(self%r0, self%r) / denom

            ! v = r + gamma * dr (search direction in solution space)
            call vector_axpyz(gamma_val, self%dr, self%r, self%v)

            ! Precondition and compute matrix-vector product
            call self%pc%apply(self%v, self%z)
            call matvec(A, self%z, self%t, ierr)

            ! omega = (t, v) / (t, t)  (minimization step)
            nt = vector_dot(self%t, self%t)
            nr = vector_dot(self%t, self%v)
            if (abs(nt) <= tiny(1.0d0)) then
                self%status = SOLVER_STATUS%BREAKDOWN%ID
                return
            end if
            omega = nr / nt

            ! dv = gamma * dv + omega * z  (solution update direction)
            call vector_scale(gamma_val, self%dv)
            call vector_axpy(omega, self%z, self%dv)

            ! dr = gamma * dr + omega * t - v  (residual update)
            ! First: dr = gamma * dr
            call vector_scale(gamma_val, self%dr)
            ! Then: dr = dr + omega * t
            call vector_axpy(omega, self%t, self%dr)
            ! Then: dr = dr - v
            call vector_axpy(-1.0d0, self%v, self%dr)

            ! x = x + dv
            call vector_axpy(1.0d0, self%dv, x)

            ! r = r + dr  (note: dr = r_new - r_old)
            call vector_axpy(1.0d0, self%dr, self%r)

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

    end subroutine solve_type_solver_idr1

    module subroutine destroy_type_solver_idr1(self)
        implicit none
        class(type_solver_idr1), intent(inout) :: self

        self%ID = -1
        if (allocated(self%name)) deallocate (self%name)
        call self%r%destroy()
        call self%r0%destroy()
        call self%v%destroy()
        call self%t%destroy()
        call self%dv%destroy()
        call self%dr%destroy()
        call self%z%destroy()
        if (allocated(self%pc)) then
            call self%pc%destroy()
            deallocate (self%pc)
        end if
        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine destroy_type_solver_idr1

end submodule impl_solve_type_solver_idr1

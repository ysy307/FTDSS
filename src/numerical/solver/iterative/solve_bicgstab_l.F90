!> BiCGSTAB(l) solver — l-step stabilized BiCG.
!> Extends BiCGSTAB with higher-order MR polynomial for smoother convergence.
!> Default l=2.
submodule(numerical_solver_interface) impl_solve_type_solver_bicgstab_l
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none
contains

    module subroutine initialize_type_solver_bicgstab_l(self, solver_settings, preconditioner_settings)
        implicit none
        class(type_solver_bicgstab_l), intent(inout) :: self
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        integer(int32) :: ierr, j

        self%ID = solver_settings%ID
        self%name = "BiCGSTAB(l)"
        self%num_nodes = solver_settings%num_nodes
        self%tolerance = solver_settings%tolerance
        self%max_iterations = solver_settings%max_iterations
        self%ell = 2

        call self%r0%initialize(self%num_nodes)

        if (allocated(self%u)) deallocate (self%u)
        allocate (self%u(0:self%ell))
        do j = 0, self%ell
            call self%u(j)%initialize(self%num_nodes)
        end do

        if (allocated(self%rr)) deallocate (self%rr)
        allocate (self%rr(0:self%ell))
        do j = 0, self%ell
            call self%rr(j)%initialize(self%num_nodes)
        end do

        call self%z%initialize(self%num_nodes)

        call allocate_array(self%gamma, self%ell + 1)
        call allocate_array(self%gamma_p, self%ell + 1)
        call allocate_array(self%gamma_pp, self%ell + 1)
        call allocate_array(self%tau, self%ell + 1, self%ell + 1)
        call allocate_array(self%sigma, self%ell + 1)

        call self%residual_history%initialize(self%max_iterations)
        self%current_iteration = 0

        self%status = SOLVER_STATUS%SUCCESS%ID
        call create_preconditioner(self%pc, preconditioner_settings, ierr)
        self%status = ierr

    end subroutine initialize_type_solver_bicgstab_l

    module subroutine solve_type_solver_bicgstab_l(self, A, b, x)
        implicit none
        class(type_solver_bicgstab_l), intent(inout) :: self
        class(abst_matrix), intent(in) :: A
        type(type_vector_dp), intent(in) :: b
        type(type_vector_dp), intent(inout) :: x

        real(real64) :: rho0, rho1, alpha, beta, omega, resid, resid0
        integer(int32) :: iter, j, i, ierr
        integer(int32) :: ell

        ell = self%ell

        call self%residual_history%zero()
        call self%pc%setup(A)

        ! r0 = b - Ax
        call matvec(A, x, self%rr(0), ierr)
        call vector_axpyz(-1.0d0, self%rr(0), b, self%rr(0))

        resid0 = vector_norm2(self%rr(0))
        call self%residual_history%set(MATRIX_OPS%INS, 1, resid0)
        if (resid0 < self%tolerance) then
            self%current_iteration = 0
            self%status = SOLVER_STATUS%SUCCESS%ID
            return
        end if

        ! r0_star = r0
        call self%r0%copy(self%rr(0))

        ! u0 = 0
        call self%u(0)%zero()

        rho0 = 1.0d0
        alpha = 0.0d0
        omega = 1.0d0

        do iter = 1, self%max_iterations
            self%current_iteration = iter

            rho0 = -omega * rho0

            ! BiCG part
            do j = 0, ell - 1
                rho1 = vector_dot(self%r0, self%rr(j))
                if (abs(rho0) <= tiny(1.0d0)) then
                    self%status = SOLVER_STATUS%BREAKDOWN%ID
                    return
                end if
                beta = alpha * rho1 / rho0
                rho0 = rho1

                ! u_i = r_i - beta * u_i for i = 0..j
                do i = 0, j
                    call vector_scale(-beta, self%u(i))
                    call vector_axpy(1.0d0, self%rr(i), self%u(i))
                end do

                ! u_{j+1} = M^{-1} A u_j
                call self%pc%apply(self%u(j), self%z)
                call matvec(A, self%z, self%u(j + 1), ierr)

                alpha = rho0 / vector_dot(self%r0, self%u(j + 1))
                if (.not. ieee_is_finite(alpha)) then
                    self%status = SOLVER_STATUS%BREAKDOWN%ID
                    return
                end if

                ! r_i = r_i - alpha * u_{i+1} for i = 0..j
                do i = 0, j
                    call vector_axpy(-alpha, self%u(i + 1), self%rr(i))
                end do

                ! r_{j+1} = M^{-1} A r_j
                call self%pc%apply(self%rr(j), self%z)
                call matvec(A, self%z, self%rr(j + 1), ierr)

                ! x = x + alpha * u_0
                call vector_axpy(alpha, self%u(0), x)
            end do

            ! MR part: minimize over r_0 + sum gamma_j * r_j
            do j = 1, ell
                do i = 1, j - 1
                    self%tau(i, j) = vector_dot(self%rr(j), self%rr(i)) / self%sigma(i)
                    call vector_axpy(-self%tau(i, j), self%rr(i), self%rr(j))
                end do
                self%sigma(j) = vector_dot(self%rr(j), self%rr(j))
                if (abs(self%sigma(j)) <= tiny(1.0d0)) then
                    self%gamma_p(j) = 0.0d0
                else
                    self%gamma_p(j) = vector_dot(self%rr(0), self%rr(j)) / self%sigma(j)
                end if
            end do

            self%gamma(ell) = self%gamma_p(ell)
            omega = self%gamma(ell)

            ! Back-substitute for gamma
            do j = ell - 1, 1, -1
                self%gamma(j) = self%gamma_p(j)
                do i = j + 1, ell
                    self%gamma(j) = self%gamma(j) - self%tau(j, i) * self%gamma(i)
                end do
            end do

            ! gamma_pp
            do j = 1, ell - 1
                self%gamma_pp(j) = self%gamma(j + 1)
                do i = j + 1, ell - 1
                    self%gamma_pp(j) = self%gamma_pp(j) + self%tau(j, i) * self%gamma(i + 1)
                end do
            end do

            ! Update x, r0, u0
            ! x = x + gamma(1) * r0
            call vector_axpy(self%gamma(1), self%rr(0), x)
            ! r0 = r0 - gamma_p(ell) * r_ell
            call vector_axpy(-self%gamma_p(ell), self%rr(ell), self%rr(0))
            ! u0 = u0 - gamma(ell) * u_ell
            call vector_axpy(-self%gamma(ell), self%u(ell), self%u(0))

            do j = 1, ell - 1
                call vector_axpy(-self%gamma(j), self%u(j), self%u(0))
                call vector_axpy(self%gamma_pp(j), self%rr(j), x)
                call vector_axpy(-self%gamma_p(j), self%rr(j), self%rr(0))
            end do

            resid = vector_norm2(self%rr(0))
            call self%residual_history%set(MATRIX_OPS%INS, iter, resid)

            if (resid < self%tolerance .or. &
                (resid0 > self%tolerance .and. resid < self%relative_tolerance * resid0)) then
                self%status = SOLVER_STATUS%SUCCESS%ID
                return
            end if

            if (was_interrupted()) stop
        end do

        self%status = SOLVER_STATUS%MAXITER%ID

    end subroutine solve_type_solver_bicgstab_l

    module subroutine destroy_type_solver_bicgstab_l(self)
        implicit none
        class(type_solver_bicgstab_l), intent(inout) :: self
        integer(int32) :: j

        self%ID = -1
        if (allocated(self%name)) deallocate (self%name)
        call self%r0%destroy()
        if (allocated(self%u)) then
            do j = 0, size(self%u) - 1
                call self%u(j)%destroy()
            end do
            deallocate (self%u)
        end if
        if (allocated(self%rr)) then
            do j = 0, size(self%rr) - 1
                call self%rr(j)%destroy()
            end do
            deallocate (self%rr)
        end if
        call self%z%destroy()
        call deallocate_array(self%gamma)
        call deallocate_array(self%gamma_p)
        call deallocate_array(self%gamma_pp)
        call deallocate_array(self%tau)
        call deallocate_array(self%sigma)
        if (allocated(self%pc)) then
            call self%pc%destroy()
            deallocate (self%pc)
        end if
        self%status = SOLVER_STATUS%SUCCESS%ID

    end subroutine destroy_type_solver_bicgstab_l

end submodule impl_solve_type_solver_bicgstab_l

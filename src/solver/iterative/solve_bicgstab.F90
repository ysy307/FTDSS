submodule(Solver_Solve) Solve_BiCGSTAB
    implicit none
contains
    module function construct_type_solver_sparse_crs_bicgstab(N, tolerance, max_iterations, preconditioner) result(structure)
        implicit none
        integer(int32), intent(in) :: N
        real(real64), intent(in) :: tolerance
        integer(int32), intent(in) :: max_iterations
        integer(int32), intent(in) :: preconditioner
        class(abst_solver), allocatable :: structure

        allocate (type_solver_sparse_crs_bicgstab :: structure)
        select type (this => structure)
        type is (type_solver_sparse_crs_bicgstab)

            this%N = N
            this%tolerance = tolerance
            this%max_iterations = max_iterations
            this%preconditioner = preconditioner

            ! 配列の確保
            call allocate_array(this%M, this%N)
            call allocate_array(this%p, this%N)
            call allocate_array(this%phat, this%N)
            call allocate_array(this%s, this%N)
            call allocate_array(this%shat, this%N)
            call allocate_array(this%r, this%N)
            call allocate_array(this%r0, this%N)
            call allocate_array(this%t, this%N)
            call allocate_array(this%v, this%N)
            call allocate_array(this%x, this%N)

        end select

    end function construct_type_solver_sparse_crs_bicgstab

    module subroutine solve_sparse_crs_bicgstab(self, A, b, x, status)
        implicit none
        class(type_solver_sparse_crs_bicgstab), intent(inout) :: self
        class(abst_matrix), intent(in) :: A
        ! type(Type_CRS), intent(in) :: A
        real(real64), intent(inout) :: b(:)
        real(real64), intent(inout) :: x(:)
        integer(int32), intent(inout) :: status

        real(real64) :: rho, rho_old, alpha, beta, omega
        real(real64) :: resid
        integer(int32) :: iter, iN

        select type (matrix => A)
        type is (type_crs)

            ! 1:Initialize
            rho = 1.0d0
            rho_old = 1.0d0
            alpha = 1.0d0
            beta = 1.0d0
            omega = 1.0d0

            self%p(:) = 0.0d0
            self%s(:) = 0.0d0
            self%phat(:) = 0.0d0
            self%shat(:) = 0.0d0

            ! 2: Set an initial value x0
            self%x(:) = 0.0d0

            ! 3: r0 = b-Ax0
            self%r(:) = matrix * self%x(:)
            self%r(:) = b(:) - self%r(:)
            ! 4: Create preconditioned matrix
            call self%Create_Preconditioner(matrix)

            ! 5: ^r0 = r0, (r*0, r0)!=0
            self%r0(:) = self%r(:)

            do iter = 1, self%max_iterations
                ! 7: (^r0, rk)
                rho = dot(self%N, self%r(:), self%r0(:))
                ! 8: rho check
                if (rho == 0.0d0) then
                    status = 0
                    x(:) = self%x(:)
                    return
                end if

                if (iter == 1) then
                    ! 10: p0 = r0
                    self%p(:) = self%r(:)
                else
                    ! 12: beta = (rho / rho_old) * (alpha_k / omega_k)
                    beta = (rho / rho_old) * (alpha / omega)
                    ! 13: p_k = r_k + beta_k(p_(k-1) - omega_k * Av)
                    self%p(:) = self%r(:) + beta * (self%p(:) - omega * self%v(:))
                end if
                ! 15: phat = M^-1 * p
                call self%Apply_Preconditioner(self%p(:), self%phat(:))
                ! 16: v = A * phat
                self%v(:) = matrix * self%phat(:)
                ! call SpMV(self%CRS_A, self%phat, self%v)
                ! 17: alpha_k = rho / (^r0, v)
                alpha = rho / dot(self%N, self%r0(:), self%v(:))
                ! 18: s = r_k - alpha_k * v
                self%s(:) = self%r(:) - alpha * self%v(:)

                ! 19: shat = M^-1 * s
                call self%Apply_Preconditioner(self%s(:), self%shat(:))
                ! 20: t = A * shat
                self%t(:) = matrix * self%shat(:)

                ! 21: omega_k = (t,s)/(t,t)
                omega = dot(self%N, self%t(:), self%s(:)) / dot(self%N, self%t(:), self%t(:))

                ! 22: omega breakdown check
                if (omega == 0.0d0) then
                    status = -1
                    return
                end if

                ! 23: x(i) = x(i-1) + alpha * M^-1 p(i-1) + omega * M^-1 s(i)
                self%x(:) = self%x(:) + alpha * self%phat(:) + omega * self%shat(:)
                ! 24: r(i) = s(i-1) - omega * AM^-1 s(i-1)
                self%r(:) = self%s(:) - omega * self%t(:)

                ! 25: ||r_k+1||_2
                resid = norm(self%N, self%r(:))
                if (resid < self%tolerance) then
                    status = 0
                    x(:) = self%x(:)
                    return
                end if

                rho_old = rho
            end do
            status = -2

        end select
    end subroutine solve_sparse_crs_bicgstab

    module subroutine check_sparse_crs_bicgstab(self, status, time)
        implicit none
        class(type_solver_sparse_crs_bicgstab), intent(inout) :: self
        integer(int32), intent(in) :: status
        real(real64), intent(in) :: time

        if (status /= 0) then
            if (status == -1) then
                write (*, '(a,es13.4,a)'), "BiCGSTAB:", time, " Day: Temperature solver occures BREAKDOWN."
            else if (status == -2) then
                write (*, '(a,es13.4,a)'), "BiCGSTAB:", time, " Day: Temperature solver occures MAXITER."
            end if
            stop
        end if

    end subroutine check_sparse_crs_bicgstab

    module subroutine destruct_type_solver_sparse_crs_bicgstab(self)
        implicit none
        type(type_solver_sparse_crs_bicgstab), intent(inout) :: self

        call deallocate_array(self%m)
        call deallocate_array(self%p)
        call deallocate_array(self%phat)
        call deallocate_array(self%s)
        call deallocate_array(self%shat)
        call deallocate_array(self%r)
        call deallocate_array(self%r0)
        call deallocate_array(self%t)
        call deallocate_array(self%v)
        call deallocate_array(self%x)

    end subroutine destruct_type_solver_sparse_crs_bicgstab
end submodule Solve_BiCGSTAB

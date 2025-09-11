submodule(solver_solve) solve_bicgstab
    implicit none
contains
    module function construct_type_solver_bicgstab(A, tolerance, max_iterations, preconditioner) result(structure)
        implicit none
        type(type_jacobian_matrix), intent(in), target :: A
        real(real64), intent(in) :: tolerance
        integer(int32), intent(in) :: max_iterations
        integer(int32), intent(in) :: preconditioner
        class(abst_solver), allocatable :: structure

        allocate (type_solver_bicgstab :: structure)
        select type (this => structure)
        type is (type_solver_bicgstab)

            this%A => A%get_matrix()
            this%size = A%get_size()
            this%tolerance = tolerance
            this%max_iterations = max_iterations
            this%preconditioner = preconditioner

            ! 配列の確保
            call allocate_array(this%M, this%size)
            call allocate_array(this%p, this%size)
            call allocate_array(this%phat, this%size)
            call allocate_array(this%s, this%size)
            call allocate_array(this%shat, this%size)
            call allocate_array(this%r, this%size)
            call allocate_array(this%r0, this%size)
            call allocate_array(this%t, this%size)
            call allocate_array(this%v, this%size)
            call allocate_array(this%x, this%size)

        end select

    end function construct_type_solver_bicgstab

    module subroutine solve_bicgstab(self, b, x, status)
        implicit none
        class(type_solver_bicgstab), intent(inout) :: self
        real(real64), intent(inout) :: b(:)
        real(real64), intent(inout) :: x(:)
        integer(int32), intent(inout) :: status

        real(real64) :: rho, rho_old, alpha, beta, omega
        real(real64) :: resid
        integer(int32) :: iter, iN, vector_size

        select type (matrix => self%A)
        type is (type_crs)

            ! 1:Initialize
            rho = 1.0d0
            rho_old = 1.0d0
            alpha = 1.0d0
            beta = 1.0d0
            omega = 1.0d0

            do iN = 1, self%size
                self%p(iN) = 0.0d0
                self%s(iN) = 0.0d0
                self%phat(iN) = 0.0d0
                self%shat(iN) = 0.0d0

                ! 2: Set an initial value x0
                self%x(iN) = 0.0d0
            end do

            ! 3: r0 = b-Ax0
            call matrix%gemv(1.0d0, self%x(:), 0.0d0, self%r(:))
            ! self%r(:) = matrix * self%x(:)
            do iN = 1, self%size
                self%r(iN) = b(iN) - self%r(iN)
            end do
            ! 4: Create preconditioned matrix
            call self%Create_Preconditioner(matrix)

            ! 5: ^r0 = r0, (r*0, r0)!=0
            do iN = 1, self%size
                self%r0(iN) = self%r(iN)
            end do

            do iter = 1, self%max_iterations
                ! 7: (^r0, rk)
                rho = dot(self%r(:), self%r0(:))
                ! 8: rho check
                if (rho == 0.0d0) then
                    status = 0
                    do iN = 1, self%size
                        x(iN) = self%x(iN)
                    end do
                    return
                end if

                if (iter == 1) then
                    ! 10: p0 = r0
                    do iN = 1, self%size
                        self%p(iN) = self%r(iN)
                    end do
                else
                    ! 12: beta = (rho / rho_old) * (alpha_k / omega_k)
                    beta = (rho / rho_old) * (alpha / omega)
                    ! 13: p_k = r_k + beta_k(p_(k-1) - omega_k * Av)
                    do iN = 1, self%size
                        self%p(iN) = self%r(iN) + beta * (self%p(iN) - omega * self%v(iN))
                    end do
                end if
                ! 15: phat = M^-1 * p
                call self%Apply_Preconditioner(self%p(:), self%phat(:))
                ! 16: v = A * phat
                call matrix%gemv(1.0d0, self%phat(:), 0.0d0, self%v(:))
                ! call SpMV(self%CRS_A, self%phat, self%v)
                ! 17: alpha_k = rho / (^r0, v)
                alpha = rho / dot(self%r0(:), self%v(:))
                ! 18: s = r_k - alpha_k * v
                do iN = 1, self%size
                    self%s(iN) = self%r(iN) - alpha * self%v(iN)
                end do

                ! 19: shat = M^-1 * s
                call self%Apply_Preconditioner(self%s(:), self%shat(:))
                ! 20: t = A * shat
                call matrix%gemv(1.0d0, self%shat(:), 0.0d0, self%t(:))

                ! 21: omega_k = (t,s)/(t,t)
                omega = dot(self%t(:), self%s(:)) / dot(self%t(:), self%t(:))

                ! 22: omega breakdown check
                if (omega == 0.0d0) then
                    status = -1
                    return
                end if

                ! 23: x(i) = x(i-1) + alpha * M^-1 p(i-1) + omega * M^-1 s(i)
                ! 24: r(i) = s(i-1) - omega * AM^-1 s(i-1)
                do iN = 1, self%size
                    self%x(iN) = self%x(iN) + alpha * self%phat(iN) + omega * self%shat(iN)
                    self%r(iN) = self%s(iN) - omega * self%t(iN)
                end do

                ! 25: ||r_k+1||_2
                resid = norm_2(self%r(:))
                if (resid < self%tolerance) then
                    status = 0
                    do iN = 1, self%size
                        x(iN) = self%x(iN)
                    end do
                    return
                end if

                rho_old = rho

                if (was_interrupted()) stop
            end do
            status = -2

        end select
    end subroutine solve_bicgstab

    module subroutine check_bicgstab(self, status, time)
        implicit none
        class(type_solver_bicgstab), intent(inout) :: self
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

    end subroutine check_bicgstab

    module subroutine destruct_type_solver_bicgstab(self)
        implicit none
        type(type_solver_bicgstab), intent(inout) :: self

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

        self%A => null()

    end subroutine destruct_type_solver_bicgstab
end submodule solve_bicgstab

submodule(solver_solve) solve_bicgstab
    implicit none
contains
    module function construct_type_solver_bicgstab(settings) result(structure)
        implicit none
        type(type_solver_settings), intent(in) :: settings
        class(abst_solver), allocatable :: structure

        allocate (type_solver_bicgstab :: structure)
        select type (this => structure)
        type is (type_solver_bicgstab)

            this%num_nodes = settings%num_nodes
            this%num_dofs_per_node = settings%num_dofs_per_node
            this%tolerance = settings%tolerance
            this%max_iterations = settings%max_iterations

            ! 配列の確保
            call this%p%initialize(settings%num_nodes, settings%num_dofs_per_node)
            call this%phat%initialize(settings%num_nodes, settings%num_dofs_per_node)
            call this%s%initialize(settings%num_nodes, settings%num_dofs_per_node)
            call this%shat%initialize(settings%num_nodes, settings%num_dofs_per_node)
            call this%r%initialize(settings%num_nodes, settings%num_dofs_per_node)
            call this%r0%initialize(settings%num_nodes, settings%num_dofs_per_node)
            call this%t%initialize(settings%num_nodes, settings%num_dofs_per_node)
            call this%v%initialize(settings%num_nodes, settings%num_dofs_per_node)
            call this%x%initialize(settings%num_nodes, settings%num_dofs_per_node)

            ! 前処理の設定
            this%pc = create_preconditioner(settings%preconditioner_id)

        end select

    end function construct_type_solver_bicgstab

    module subroutine solve_bicgstab(self, A, b, x)
        implicit none
        class(type_solver_bicgstab), intent(inout) :: self
        type(type_jacobian_matrix), intent(in) :: A
        type(type_residual_vector), intent(in) :: b
        type(type_residual_vector), intent(inout) :: x

        real(real64) :: rho, rho_old, alpha, beta, omega
        real(real64) :: resid
        integer(int32) :: iter, iN, vector_size
        integer(int32) :: idof

        ! select type (matrix => self%A)
        ! type is (type_crs)

        ! 1:Initialize
        rho = 1.0d0
        rho_old = 1.0d0
        alpha = 1.0d0
        beta = 1.0d0
        omega = 1.0d0

        do idof = 1, self%num_dofs_per_node
            call self%p%zero()
            call self%s%zero()
            call self%phat%zero()
            call self%shat%zero()

            ! 2: Set an initial value x0
            call self%x%zero()
        end do

        ! 3: r0 = b-Ax0
        call ftdss_gemv(A, 1.0d0, self%x, 0.0d0, self%r)
        call ftdss_sub(b, self%r, self%r)

        ! 4: Create preconditioned matrix
        ! call self%Create_Preconditioner(matrix)

        ! 5: ^r0 = r0, (r*0, r0)!=0
        call self%r0%copy(self%r)

        do iter = 1, self%max_iterations
            ! 7: (^r0, rk)
            rho = ftdss_dot(self%r, self%r0)
            ! 8: rho check
            if (rho == 0.0d0) then
                self%solver_status = SOLVER_STATUS_SUCCESS
                do iN = 1, self%num_dofs
                    call x%set(iN, self%x(self%size * (iN - 1) + 1:self%size * iN))
                end do
                return
            end if

            if (iter == 1) then
                ! 10: p0 = r0
                call self%p%copy(self%r)
            else
                ! 12: beta = (rho / rho_old) * (alpha_k / omega_k)
                beta = (rho / rho_old) * (alpha / omega)
                ! 13: p_k = r_k + beta_k(p_(k-1) - omega_k * Av)
                do iN = 1, self%size
                    self%p(iN) = self%r(iN) + beta * (self%p(iN) - omega * self%v(iN))
                end do
            end if
            ! 15: phat = M^-1 * p
            call self%pc%apply(self%p, self%phat)
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
            call self%pc%apply(self%s, self%shat)
            ! 20: t = A * shat
            call matrix%gemv(1.0d0, self%shat(:), 0.0d0, self%t(:))

            ! 21: omega_k = (t,s)/(t,t)
            omega = dot(self%t(:), self%s(:)) / dot(self%t(:), self%t(:))

            ! 22: omega breakdown check
            if (omega == 0.0d0) then
                self%solver_status = SOLVER_STATUS_BREAKDOWN
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
                self%solver_status = SOLVER_STATUS_SUCCESS
                do iN = 1, self%num_dofs
                    call x%set(iN, self%x(self%size * (iN - 1) + 1:self%size * iN))
                end do
                return
            end if

            rho_old = rho

            if (was_interrupted()) stop
        end do
        self%solver_status = SOLVER_STATUS_MAXITER

        ! end select
    end subroutine solve_bicgstab

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

    end subroutine destruct_type_solver_bicgstab
end submodule solve_bicgstab

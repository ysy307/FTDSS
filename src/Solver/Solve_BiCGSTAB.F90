submodule(Solver_Solve) Solver_Solve_BiCGSTAB_Implementation
    use, intrinsic :: iso_fortran_env, only: int32
    use :: Matrix_CRS
    implicit none
    real(real64), allocatable, target :: work(:, :)

contains
    module function Solver_CRS_BiCGSTAB_Constructor(N, tol, maxiter, Preconditioner) result(structure)
        implicit none
        integer(int32), intent(in) :: N
        real(real64), intent(in) :: tol
        integer(int32), intent(in) :: maxiter
        integer(int32), intent(in) :: Preconditioner
        class(Abstract_Solver_CRS), allocatable :: structure

        allocate (Solver_CRS_BiCGSTAB :: structure)
        select type (this => structure)
        type is (Solver_CRS_BiCGSTAB)

            this%N = N
            this%tol = tol
            this%maxiter = maxiter
            this%Preconditioner = Preconditioner

            ! 配列の確保
            call Allocate_Array(work, N, 10_int32)

            ! ポインタの関連付け
            this%M => work(:, 1)
            this%p => work(:, 2)
            this%phat => work(:, 3)
            this%s => work(:, 4)
            this%shat => work(:, 5)
            this%r => work(:, 6)
            this%r0 => work(:, 7)
            this%t => work(:, 8)
            this%v => work(:, 9)
            this%x => work(:, 10)
        end select

    end function Solver_CRS_BiCGSTAB_Constructor

    module subroutine Solve_CRS_BiCGSTAB(self, A, b, x, status)
        use :: Matrix_CRS
        implicit none
        class(Solver_CRS_BiCGSTAB) :: self
        type(Type_CRS), intent(in) :: A
        real(real64), intent(inout) :: b(:)
        real(real64), intent(inout) :: x(:)
        integer(int32), intent(inout) :: status

        real(real64) :: rho, rho_old, alpha, beta, omega
        real(real64) :: resid
        integer(int32) :: iter, iN

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
        self%r(:) = A * self%x(:)
        self%r(:) = b(:) - self%r(:)
        ! 4: Create preconditioned matrix
        call self%Create_Preconditioner(A)

        ! 5: ^r0 = r0, (r*0, r0)!=0
        self%r0(:) = self%r(:)

        do iter = 1, self%maxiter
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
            self%v(:) = A * self%phat(:)
            ! call SpMV(self%CRS_A, self%phat, self%v)
            ! 17: alpha_k = rho / (^r0, v)
            alpha = rho / dot(self%N, self%r0(:), self%v(:))
            ! 18: s = r_k - alpha_k * v
            self%s(:) = self%r(:) - alpha * self%v(:)

            ! 19: shat = M^-1 * s
            call self%Apply_Preconditioner(self%s(:), self%shat(:))
            ! 20: t = A * shat
            self%t(:) = A * self%shat(:)

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
            if (resid < self%tol) then
                status = 0
                x(:) = self%x(:)
                return
            end if

            rho_old = rho
        end do
        status = -2
    end subroutine Solve_CRS_BiCGSTAB

    module subroutine Check_CRS_BiCGSTAB(self, status, time)
        implicit none
        class(Solver_CRS_BiCGSTAB) :: self
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

    end subroutine Check_CRS_BiCGSTAB

    module subroutine Solver_CRS_BiCGSTAB_Destructor(self)
        implicit none
        type(Solver_CRS_BiCGSTAB) :: self

        ! ポインタコンポーネントをnullify
        nullify (self%M)
        nullify (self%p)
        nullify (self%phat)
        nullify (self%s)
        nullify (self%shat)
        nullify (self%r)
        nullify (self%r0)
        nullify (self%t)
        nullify (self%v)
        nullify (self%x)

        if (allocated(work)) deallocate (work)

    end subroutine Solver_CRS_BiCGSTAB_Destructor
end submodule Solver_Solve_BiCGSTAB_Implementation

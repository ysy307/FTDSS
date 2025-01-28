module Solver_Solve
    use omp_lib
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    use :: Allocate_Allocate
    use :: Allocate_Structure
    use :: Solver_Precon_jacobi
    use :: Calculate_Product, only:SpMV => Matrix_Vector_Product_CRS
    use :: error
    use :: Calculate_BLAS, only:norm => norm_2, dot => ddots
    implicit none
    private

    type(CRS) :: CRS_A
    real(real64), allocatable, target :: work(:, :)
    ! Iterative Linear Solver
    type :: ILS
        private
        type(CRS) :: CRS_A
        real(real64), dimension(:), pointer :: M, p, phat, s, shat, r, r0, t, v, x, b
    contains
        procedure :: BiCGStab => Solver_BiCGStab
        procedure :: Chkerr => Solver_Check_Error
        final :: ILS_Destructor
    end type ILS

    interface ILS
        module procedure Solve_Initialize_BiCGStab
    end interface

    ! Direct Linear Solver
    type :: DLS
        private
        integer(int32) :: N
    contains
        procedure :: LU => Solver_LU
    end type

    interface DLS
        module procedure Solve_Initialize_LU
    end interface

    public :: ILS
    public :: DLS
contains

    type(ILS) function Solve_Initialize_BiCGStab(Solver, MCRS)
        implicit none
        type(SolverInfo), intent(in) :: Solver
        type(CRS), intent(in) :: MCRS

        call Allocate_Array(work, Solver%N%node, 11_int32)

        Solve_Initialize_BiCGStab%M => work(:, 1)
        Solve_Initialize_BiCGStab%p => work(:, 2)
        Solve_Initialize_BiCGStab%phat => work(:, 3)
        Solve_Initialize_BiCGStab%s => work(:, 4)
        Solve_Initialize_BiCGStab%shat => work(:, 5)
        Solve_Initialize_BiCGStab%r => work(:, 6)
        Solve_Initialize_BiCGStab%r0 => work(:, 7)
        Solve_Initialize_BiCGStab%t => work(:, 8)
        Solve_Initialize_BiCGStab%v => work(:, 9)
        Solve_Initialize_BiCGStab%x => work(:, 10)
        Solve_Initialize_BiCGStab%b => work(:, 11)

        call Duplicate_CRS(MCRS, Solve_Initialize_BiCGStab%CRS_A)

    end function Solve_Initialize_BiCGStab

    subroutine Solver_BiCGStab(self, Solver, BiCG_A, BiCG_b, BiCG_x, status)
        implicit none
        class(ILS) :: self
        type(SolverInfo), intent(inout) :: Solver
        type(CRS), intent(in) :: BiCG_A
        real(real64), intent(in) :: BiCG_b(:)
        real(real64), intent(inout) :: BiCG_x(:)
        integer(int32), intent(inout) :: status
        real(real64) :: rho, rho_old, alpha, beta, omega
        real(real64) :: tol, resid
        real(real64) :: ddot, dnrm2
        integer(int32) :: maxiter, iter, N, iN

        ! 1:Initialize
        N = Solver%N%node
        tol = 1.0d-12
        maxiter = N * 100
        maxiter = 50000
        self%CRS_A%Val(:) = BiCG_A%Val(:)
        self%b(:) = BiCG_b(:)
        rho = 1.0d0
        rho_old = 1.0d0
        alpha = 1.0d0
        beta = 1.0d0
        omega = 1.0d0

        do iN = 1, N
            self%p(iN) = 0.0d0
            self%s(iN) = 0.0d0
            self%phat(iN) = 0.0d0
            self%shat(iN) = 0.0d0
        end do

        ! 2: Set an initial value x0
        self%x(:) = 0.0d0

        ! 3: r0 = b-Ax0
        call SpMV(self%CRS_A, self%x, self%r)
        do iN = 1, N
            self%r(iN) = self%b(iN) - self%r(iN)
        end do

        ! 4: Create preconditioned matrix
        call Create_Precond_Jacobi(N, self%CRS_A, self%M)

        ! 5: ^r0 = r0, (r*0, r0)!=0
        self%r0(:) = self%r(:)

        do iter = 1, maxiter, 1
            ! 7: (^r0, rk)
            rho = dot(N, self%r, self%r0)
            ! rho = ddot(N, self%r, 1, self%r0, 1)
            ! 8: rho check
            if (rho == 0.0d0) then
                status = 0
                BiCG_x(:) = self%x(:)
                return
            end if

            if (iter == 1) then
                ! 10: p0 = r0
                self%p(:) = self%r(:)
            else
                ! 12: beta = (rho / rho_old) * (alpha_k / omega_k)
                beta = (rho / rho_old) * (alpha / omega)
                ! 13: p_k = r_k + beta_k(p_(k-1) - omega_k * Av)
                do iN = 1, N
                    self%p(iN) = self%r(iN) + beta * (self%p(iN) - omega * self%v(iN))
                end do
            end if
            ! 15: phat = M^-1 * p
            call Apply_Precond_Jacobi(N, self%M, self%p, self%phat)
            ! 16: v = A * phat
            call SpMV(self%CRS_A, self%phat, self%v)
            ! 17: alpha_k = rho / (^r0, v)
            alpha = rho / dot(N, self%r0, self%v)
            ! alpha = rho / ddot(N, self%r0, 1, self%v, 1)
            ! 18: s = r_k - alpha_k * v
            do iN = 1, N
                self%s(iN) = self%r(iN) - alpha * self%v(iN)
            end do

            ! 19: shat = M^-1 * s
            call Apply_Precond_Jacobi(N, self%M, self%s, self%shat)
            ! 20: t = A * shat
            call SpMV(self%CRS_A, self%shat, self%t)

            ! 21: omega_k = (t,s)/(t,t)
            omega = dot(N, self%t, self%s) / dot(N, self%t, self%t)
            ! omega = ddot(N, self%t, 1, self%s, 1) / ddot(N, self%t, 1, self%t, 1)

            ! 22: omega breakdown check
            if (omega == 0.0d0) then
                status = -1
                return
            end if

            do iN = 1, N
                ! 23: x(i) = x(i-1) + alpha * M^-1 p(i-1) + omega * M^-1 s(i)
                self%x(iN) = self%x(iN) + alpha * self%phat(iN) + omega * self%shat(iN)
            end do
            do iN = 1, N
                ! 24: r(i) = s(i-1) - omega * AM^-1 s(i-1)
                self%r(iN) = self%s(iN) - omega * self%t(iN)
            end do

            ! 25: ||r_k+1||_2
            ! resid = dnrm2(N, self%r(:), 1)
            resid = norm(N, self%r(:))
            if (resid < tol) then
                status = 0
                BiCG_x(:) = self%x(:)
                return
            end if

            rho_old = rho
        end do
        status = -2
    end subroutine Solver_BiCGStab

    subroutine Solver_Check_Error(self, ierr, time)
        implicit none
        class(ILS) :: self
        integer(int32), intent(in) :: ierr
        real(real64), intent(in) :: time

        if (ierr /= 0) then
            if (ierr == -1) then
                write (*, '(a,es13.4,a)'), "BiCGStab:", time, " Day: Temperature solver occures BREAKDOWN."
            else if (ierr == -2) then
                write (*, '(a,es13.4,a)'), "BiCGStab:", time, " Day: Temperature solver occures MAXITER."
            end if
            stop
        end if

    end subroutine Solver_Check_Error

    subroutine ILS_Destructor(self)
        implicit none
        type(ILS) :: self

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
        nullify (self%b)

        ! CRS_Aのリソースを解放
        if (allocated(self%CRS_A%Val)) deallocate (self%CRS_A%Val)
        if (allocated(self%CRS_A%Ind)) deallocate (self%CRS_A%Ind)
        if (allocated(self%CRS_A%Ptr)) deallocate (self%CRS_A%Ptr)

    end subroutine ILS_Destructor

    type(DLS) function Solve_Initialize_LU(Solver)
        implicit none
        type(SolverInfo) :: Solver

        Solve_Initialize_LU%N = Solver%N%node

    end function Solve_Initialize_LU

    subroutine Solver_LU(self, LU_A, LU_b, LU_x)
        implicit none
        class(DLS) :: self
        real(real64), intent(inout) :: LU_A(:, :), LU_b(:), LU_x(:)
        ! integer(int32), intent(in)    :: n
        integer(int32) :: ipiv(self%N), info

        !* LU decomposition
        call Dgetrf(self%N, self%N, LU_A, self%N, ipiv, info)
        if (info /= 0) call error_message(942)

        !* solve linear equation
        call Dgetrs('N', self%N, 1, LU_A, self%N, ipiv, LU_b, self%N, info)
        if (info /= 0) call error_message(943)

        LU_x(:) = LU_b(:)

    end subroutine Solver_LU
end module Solver_Solve

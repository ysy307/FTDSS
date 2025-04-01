module Solver_Solve
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Types
    use :: Allocate_Allocate
    use :: Allocate_Structure
    use :: error
    use :: Calculate_BLAS, only:norm => norm_2, dot
    use :: Matrix_CRS
#ifdef _OPENMP
    use omp_lib
#endif

    implicit none
    ! private
#ifdef _MKL
    include "mkl_pardiso.fi"
#endif

    type(CRS) :: CRS_A
    real(real64), allocatable, target :: work(:, :)

    type, abstract :: Abstract_Solver_CRS
    contains
        procedure(Abstract_Solve), public, pass(self), deferred :: Solve
        procedure(Abstract_Check), public, pass(self), deferred :: Check
    end type Abstract_Solver_CRS

    type, extends(Abstract_Solver_CRS) :: Solver_CRS_BiCGSTAB
        integer(int32) :: N
        real(real64), dimension(:), pointer :: M
        real(real64), dimension(:), pointer :: p
        real(real64), dimension(:), pointer :: phat
        real(real64), dimension(:), pointer :: s
        real(real64), dimension(:), pointer :: shat
        real(real64), dimension(:), pointer :: r
        real(real64), dimension(:), pointer :: r0
        real(real64), dimension(:), pointer :: t
        real(real64), dimension(:), pointer :: v
        real(real64), dimension(:), pointer :: x

        real(real64) :: tol
        integer(int32) :: maxiter

        integer(int32) :: Preconditioner
        !! 0: No Preconditioner (No implemented)
        !! 1: Jacobi Preconditioner
        !! 2: ILU Preconditioner (No implemented)
    contains
        procedure :: Solve => Solve_CRS_BiCGSTAB
        procedure :: Check => Check_CRS_BiCGSTAB
        procedure, private, pass(self) :: Create_Preconditioner => Create_Preconditioner_CRS_BiCGSTAB
        procedure, private, pass(self) :: Apply_Preconditioner => Apply_Preconditioner_CRS_BiCGSTAB
        final :: Solver_CRS_BiCGSTAB_Destructor
    end type Solver_CRS_BiCGSTAB

    type, extends(Abstract_Solver_CRS) :: Solver_CRS_LU
        integer :: N
        integer :: MAXFCT
        integer :: MNUM
        integer :: MTYPE
        integer :: PHASE
        integer :: NRHS
        integer :: MSGLVL
        integer :: ERROR
        integer, allocatable :: IA(:)
        integer, allocatable :: JA(:)
        integer, allocatable :: PERM(:)
        integer, allocatable :: IPARM(:)
        type(MKL_PARDISO_HANDLE), allocatable :: PT(:)
    contains
        procedure :: Solve => Solve_CRS_LU
        procedure :: Check => Check_CRS_LU
    end type Solver_CRS_LU

    interface Solver_CRS_BiCGSTAB
        module procedure Solver_CRS_BiCGSTAB_Constructor
    end interface
    ! generic :: Solver_CRS_BiCGSTAB => Solver_CRS_BiCGSTAB_Constructor
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

    abstract interface
        subroutine Abstract_Solve(self, A, b, x, status)
            use, intrinsic :: iso_fortran_env, only: int32, real64
            use :: Matrix_CRS
            import :: Abstract_Solver_CRS
            implicit none
            class(Abstract_Solver_CRS) :: self
            type(Type_CRS), intent(in) :: A
            real(real64), intent(inout) :: b(:)
            real(real64), intent(inout) :: x(:)
            integer(int32), intent(inout) :: status
        end subroutine Abstract_Solve

        subroutine Abstract_Check(self, status, time)
            use, intrinsic :: iso_fortran_env, only: int32, real64
            use :: Matrix_CRS
            import :: Abstract_Solver_CRS
            implicit none
            class(Abstract_Solver_CRS) :: self
            integer(int32), intent(in) :: status
            real(real64), intent(in) :: time
        end subroutine Abstract_Check
    end interface

    interface
        module function Solver_CRS_BiCGSTAB_Constructor(N, tol, maxiter, Preconditioner) result(structure)
            use, intrinsic :: iso_fortran_env, only: int32, real64
            import :: Abstract_Solver_CRS
            implicit none
            integer(int32), intent(in) :: N
            real(real64), intent(in) :: tol
            integer(int32), intent(in) :: maxiter
            integer(int32), intent(in) :: Preconditioner
            class(Abstract_Solver_CRS), allocatable :: structure
        end function Solver_CRS_BiCGSTAB_Constructor

        module subroutine Create_Preconditioner_Jacobi(N, A, M)
            implicit none
            integer(int32), intent(in) :: N
            type(Type_CRS), intent(in) :: A
            real(real64), intent(inout) :: M(:)

        end subroutine Create_Preconditioner_Jacobi

        module subroutine Apply_Preconditioner_Jacobi(N, M, r, z)
            implicit none
            integer(int32), intent(in) :: N
            real(real64), intent(in) :: M(:)
            real(real64), intent(in) :: r(:)
            real(real64), intent(inout) :: z(:)

        end subroutine Apply_Preconditioner_Jacobi

        module subroutine Create_Preconditioner_CRS_BiCGSTAB(self, A)
            use :: Matrix_CRS
            import :: Solver_CRS_BiCGSTAB
            implicit none
            class(Solver_CRS_BiCGSTAB) :: self
            type(Type_CRS), intent(in) :: A

        end subroutine Create_Preconditioner_CRS_BiCGSTAB

        module subroutine Apply_Preconditioner_CRS_BiCGSTAB(self, b, x)
            use, intrinsic :: iso_fortran_env, only: real64
            import :: Solver_CRS_BiCGSTAB
            implicit none
            class(Solver_CRS_BiCGSTAB) :: self
            real(real64), intent(inout) :: b(:)
            real(real64), intent(inout) :: x(:)
        end subroutine Apply_Preconditioner_CRS_BiCGSTAB

        module subroutine Solver_CRS_BiCGSTAB_Destructor(self)
            import :: Solver_CRS_BiCGSTAB
            implicit none
            type(Solver_CRS_BiCGSTAB) :: self

        end subroutine Solver_CRS_BiCGSTAB_Destructor

        module subroutine Solve_CRS_BiCGSTAB(self, A, b, x, status)
            use, intrinsic :: iso_fortran_env, only: int32, real64
            use :: Matrix_CRS
            import :: Solver_CRS_BiCGSTAB
            implicit none
            class(Solver_CRS_BiCGSTAB) :: self
            type(Type_CRS), intent(in) :: A
            real(real64), intent(inout) :: b(:)
            real(real64), intent(inout) :: x(:)
            integer(int32), intent(inout) :: status
        end subroutine

        module subroutine Check_CRS_BiCGSTAB(self, status, time)
            use, intrinsic :: iso_fortran_env, only: int32, real64
            use :: Matrix_CRS
            import :: Solver_CRS_BiCGSTAB
            implicit none
            class(Solver_CRS_BiCGSTAB) :: self
            integer(int32), intent(in) :: status
            real(real64), intent(in) :: time
        end subroutine Check_CRS_BiCGSTAB

        module subroutine Solve_CRS_LU(self, A, b, x, status)
            use, intrinsic :: iso_fortran_env, only: int32, real64
            use :: Matrix_CRS
            import :: Solver_CRS_LU
            implicit none
            class(Solver_CRS_LU) :: self
            type(Type_CRS), intent(in) :: A
            real(real64), intent(inout) :: b(:)
            real(real64), intent(inout) :: x(:)
            integer(int32), intent(inout) :: status
        end subroutine Solve_CRS_LU

        module subroutine Check_CRS_LU(self, status, time)
            use, intrinsic :: iso_fortran_env, only: int32, real64
            use :: Matrix_CRS
            import :: Solver_CRS_LU
            implicit none
            class(Solver_CRS_LU) :: self
            integer(int32), intent(in) :: status
            real(real64), intent(in) :: time
        end subroutine Check_CRS_LU

        module function Solver_CRS_LU_Constructor(N, MAXFCT, MNUM, MTYPE, PHASE, NRHS, MSGVLV, A) result(structure)
            use, intrinsic :: iso_fortran_env, only: int32, real64
            use :: Matrix_CRS
            implicit none
            integer(int32), intent(in) :: N
            integer(int32), intent(in) :: MAXFCT
            integer(int32), intent(in) :: MNUM
            integer(int32), intent(in) :: MTYPE
            integer(int32), intent(in) :: PHASE
            integer(int32), intent(in) :: NRHS
            integer(int32), intent(in) :: MSGVLV
            type(Type_CRS), intent(in) :: A

            class(Abstract_Solver_CRS), allocatable :: structure
        end function Solver_CRS_LU_Constructor
    end interface

    ! public :: ILS
    public :: DLS
contains
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

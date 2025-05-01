module Solver_Solve
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Core_BaseTypes
    use :: Core_Allocate
    use :: Core_error
    use :: Calculate_BLAS, only:norm => norm_2, dot
    use :: Matrix_CRS
#ifdef _OPENMP
    use omp_lib
#endif

    implicit none
    private
#ifdef _MKL
    include "mkl_pardiso.fi"
    include "mkl_lapack.fi"
#endif

    public :: Abstract_Solver_CRS
    public :: Abstract_Solver_Full
    public :: Solver_CRS_BiCGSTAB
    public :: Solver_CRS_LU
    public :: Solver_Full_LU

    public :: Solver_CRS_BiCGSTAB_Constructor
    public :: Solver_CRS_LU_Constructor
    public :: Solver_Full_LU_Constructor

    type, abstract :: Abstract_Solver_CRS
    contains
        procedure(Abstract_Solve_CRS), public, pass(self), deferred :: Solve
        procedure(Abstract_Check_CRS), public, pass(self), deferred :: Check
    end type Abstract_Solver_CRS

    type, abstract :: Abstract_Solver_Full
    contains
        procedure(Abstract_Solve_Full), public, pass(self), deferred :: Solve
        procedure(Abstract_Check_Full), public, pass(self), deferred :: Check
    end type Abstract_Solver_Full

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
        !! Maximum number of factors with identical sparsity structure that must be kept in memory at the same time. In most
        !! applications this value is equal to 1. It is possible to store several different factorizations with the same nonzero
        !! structure at the same time in the internal data structure management of the solver.
        !! pardiso can process several matrices with an identical matrix sparsity pattern and it can store the factors of these
        !! matrices at the same time. Matrices with a different sparsity structure can be kept in memory with different memory
        !! address pointers pt.
        integer :: MNUM
        !! Indicates the actual matrix for the solution phase. With this scalar you can define which matrix to factorize.
        !! The value must be: 1 ≤mnum≤maxfct. In most applications this value is 1.
        integer :: MTYPE
        !! Defines the matrix type, which influences the pivoting method. The Intel® oneAPI Math Kernel Library PARDISO solver
        !! supports the following matrices:
        !!**************************************************************************
        !!  1: real and structurally symmetric
        !!  2: real and symmetric positive definite
        !! -2: real and symmetric indefinite
        !!  3: complex and structurally symmetric
        !!  4: complex and Hermitian positive definite
        !! -4: complex and Hermitian indefinite
        !!  6: complex and symmetric
        !! 11: real and nonsymmetric
        !! 13: complex and nonsymmetric
        !!**************************************************************************
        integer :: PHASE
        !! Controls the execution of the solver. Usually it is a two- or three-digit integer. The first digit indicates the starting
        !! phase of execution and the second digit indicates the ending phase. Intel® oneAPI Math Kernel Library PARDISO has
        !! the following phases of execution:
        !!
        !! Phase 1: Fill-reduction analysis and symbolic factorization
        !! Phase 2: Numerical factorization
        !! Phase 3: Forward and Backward solve including optional iterative refinement
        !!
        !! This phase can be divided into two or three separate substitutions: forward, backward, and diagonal.
        !! Memory release phase (phase= 0 or phase= -1)
        !!
        !! If a previous call to the routine has computed information from previous phases, execution may start at any phase.
        !! The phase parameter can have the following values:
        !!**************************************************************************
        !! 11:  Analysis
        !! 12:  Analysis, numerical factorization
        !! 13:  Analysis, numerical factorization, solve, iterative refinement
        !! 22:  Numerical factorization
        !! 23:  Numerical factorization, solve, iterative refinement
        !! 33:  Solve, iterative refinement
        !! 331: like phase=33, but only forward substitution
        !! 332: like phase=33, but only diagonal substitution (if available)
        !! 333: like phase=33, but only backward substitution
        !! 0:   Release internal memory for L and U matrix number mnum
        !! -1:  Release all internal memory for all matrices
        !!**************************************************************************
        integer :: NRHS
        !! Number of right-hand sides that need to be solved for.
        integer :: MSGLVL
        !! Message level information. If msglvl = 0 then pardiso generates no output, if msglvl = 1 the solver prints
        !! statistical information to the screen.
        integer :: ERROR
        integer, allocatable :: IA(:)
        !! Array, size (n+1).
        !! For CSR3 format, ia[i] (i<n) points to the first column index of row i in the array ja. That is, ia[i] gives the index
        !! of the element in array a that contains the first non-zero element from row i of A. The last element ia[n] is taken
        !! to be equal to the number of non-zero elements in A, plus one.
        !! The array ia is accessed in all phases of the solution process.
        integer, allocatable :: JA(:)
        !! Array, size (nnz).
        !! For CSR3 format, array ja contains column indices of the sparse matrix A. It is important that the indices are
        !! in increasing order per row. For structurally symmetric matrices it is assumed that all diagonal elements are stored
        !! (even if they are zeros) in the list of non-zero elements in a and ja.
        integer, allocatable :: PERM(:)
        !! Array, size (64). This array is used to pass various parameters to Intel® oneAPI Math Kernel Library PARDISO
        !! and to return some useful information after execution of the solver.
        integer, allocatable :: IPARM(:)
        type(MKL_PARDISO_HANDLE), allocatable :: PT(:)
        !! Array with size of 64.
        !! Handle to internal data structure. The entries must be set to zero prior to the first call to pardiso.
        !! Unique for factorization.
        !! CAUTION: After the first call to pardiso do not directly modify pt, as that could cause a serious memory leak.
    contains
        procedure :: Solve => Solve_CRS_LU
        procedure :: Check => Check_CRS_LU
    end type Solver_CRS_LU

    type, extends(Abstract_Solver_Full) :: Solver_Full_LU
        integer :: N
        integer :: ERROR
        integer, allocatable :: IPIV(:)
    contains
        procedure :: Solve => Solve_Full_LU
        procedure :: Check => Check_Full_LU
    end type

    interface Solver_CRS_BiCGSTAB
        module procedure Solver_CRS_BiCGSTAB_Constructor
    end interface
    interface Solver_CRS_LU
        module procedure Solver_CRS_LU_Constructor
    end interface
    interface Solver_Full_LU
        module procedure Solver_Full_LU_Constructor
    end interface

    abstract interface
        subroutine Abstract_Solve_CRS(self, A, b, x, status)
            use, intrinsic :: iso_fortran_env, only: int32, real64
            use :: Matrix_CRS
            import :: Abstract_Solver_CRS
            implicit none
            class(Abstract_Solver_CRS) :: self
            type(Type_CRS), intent(in) :: A
            real(real64), intent(inout) :: b(:)
            real(real64), intent(inout) :: x(:)
            integer(int32), intent(inout) :: status
        end subroutine Abstract_Solve_CRS

        subroutine Abstract_Check_CRS(self, status, time)
            use, intrinsic :: iso_fortran_env, only: int32, real64
            use :: Matrix_CRS
            import :: Abstract_Solver_CRS
            implicit none
            class(Abstract_Solver_CRS) :: self
            integer(int32), intent(in) :: status
            real(real64), intent(in) :: time
        end subroutine Abstract_Check_CRS

        subroutine Abstract_Solve_Full(self, A, b, x, status)
            use, intrinsic :: iso_fortran_env, only: int32, real64
            import :: Abstract_Solver_Full
            implicit none
            class(Abstract_Solver_Full) :: self
            real(real64), intent(in) :: A(:, :)
            real(real64), intent(inout) :: b(:)
            real(real64), intent(inout) :: x(:)
            integer(int32), intent(inout) :: status
        end subroutine Abstract_Solve_Full

        subroutine Abstract_Check_Full(self, status, time)
            use, intrinsic :: iso_fortran_env, only: int32, real64
            import :: Abstract_Solver_Full
            implicit none
            class(Abstract_Solver_Full) :: self
            integer(int32), intent(in) :: status
            real(real64), intent(in) :: time
        end subroutine Abstract_Check_Full
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

        module subroutine Solve_Full_LU(self, A, b, x, status)
            use, intrinsic :: iso_fortran_env, only: int32, real64
            import :: Solver_Full_LU
            implicit none
            class(Solver_Full_LU) :: self
            real(real64), intent(in) :: A(:, :)
            real(real64), intent(inout) :: b(:)
            real(real64), intent(inout) :: x(:)
            integer(int32), intent(inout) :: status
        end subroutine Solve_Full_LU

        module subroutine Check_Full_LU(self, status, time)
            use, intrinsic :: iso_fortran_env, only: int32, real64
            import :: Solver_Full_LU
            implicit none
            class(Solver_Full_LU) :: self
            integer(int32), intent(in) :: status
            real(real64), intent(in) :: time
        end subroutine Check_Full_LU

        module function Solver_Full_LU_Constructor(N) result(structure)
            use, intrinsic :: iso_fortran_env, only: int32, real64
            implicit none
            integer(int32), intent(in) :: N

            class(Abstract_Solver_Full), allocatable :: structure
        end function Solver_Full_LU_Constructor
    end interface
end module Solver_Solve

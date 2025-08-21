module solver_solve
    use, intrinsic :: iso_fortran_env, only: int32, real64
!$  use omp_lib
    use :: module_core, only:allocate_array, deallocate_array, error_message, was_interrupted
    use :: module_calculate, only:norm => norm_2, dot => inner_product
    use :: module_matrix, only:type_crs, type_dense, abst_matrix, gemv, add
    implicit none
    private
#ifdef _MKL
    include "mkl_pardiso.fi"
#endif

    public :: abst_solver
    public :: type_solver_sparse_crs_bicgstab
    public :: type_solver_sparse_crs_lu
    public :: type_solver_dense_lu

    type, abstract :: abst_solver
    contains
        procedure(abst_solve), pass(self), deferred :: solve
        procedure(abst_check), pass(self), deferred :: check
    end type abst_solver

    abstract interface
        subroutine abst_solve(self, A, b, x, status)
            import :: abst_solver, abst_matrix, int32, real64
            implicit none
            class(abst_solver), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            real(real64), intent(inout) :: b(:)
            real(real64), intent(inout) :: x(:)
            integer(int32), intent(inout) :: status
        end subroutine abst_solve

        subroutine abst_check(self, status, time)
            import :: abst_solver, int32, real64
            implicit none
            class(abst_solver), intent(inout) :: self
            integer(int32), intent(in) :: status
            real(real64), intent(in) :: time
        end subroutine abst_check
    end interface

    type, extends(abst_solver) :: type_solver_sparse_crs_bicgstab
        integer(int32) :: size
        real(real64), allocatable :: m(:)
        real(real64), allocatable :: p(:)
        real(real64), allocatable :: phat(:)
        real(real64), allocatable :: s(:)
        real(real64), allocatable :: shat(:)
        real(real64), allocatable :: r(:)
        real(real64), allocatable :: r0(:)
        real(real64), allocatable :: t(:)
        real(real64), allocatable :: v(:)
        real(real64), allocatable :: x(:)

        real(real64) :: tolerance
        integer(int32) :: max_iterations

        integer(int32) :: preconditioner
        ! 0: No preconditioner (No implemented)
        ! 1: Jacobi preconditioner
        ! 2: ILU preconditioner (No implemented)
    contains
        procedure :: solve => solve_sparse_crs_bicgstab
        procedure :: check => check_sparse_crs_bicgstab
        procedure, private, pass(self) :: create_preconditioner => create_preconditioner_sparse_crs_bicgstab
        procedure, private, pass(self) :: apply_preconditioner => apply_preconditioner_sparse_crs_bicgstab
        final :: destruct_type_solver_sparse_crs_bicgstab
    end type type_solver_sparse_crs_bicgstab

    interface
        module function construct_type_solver_sparse_crs_bicgstab(size, tolerance, max_iterations, preconditioner) result(structure)
            implicit none
            integer(int32), intent(in) :: size
            real(real64), intent(in) :: tolerance
            integer(int32), intent(in) :: max_iterations
            integer(int32), intent(in) :: preconditioner
            class(abst_solver), allocatable :: structure

        end function construct_type_solver_sparse_crs_bicgstab

        module subroutine solve_sparse_crs_bicgstab(self, A, b, x, status)
            implicit none
            class(type_solver_sparse_crs_bicgstab), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            real(real64), intent(inout) :: b(:)
            real(real64), intent(inout) :: x(:)
            integer(int32), intent(inout) :: status

        end subroutine solve_sparse_crs_bicgstab

        module subroutine check_sparse_crs_bicgstab(self, status, time)
            implicit none
            class(type_solver_sparse_crs_bicgstab), intent(inout) :: self
            integer(int32), intent(in) :: status
            real(real64), intent(in) :: time

        end subroutine check_sparse_crs_bicgstab
    end interface

    type, extends(abst_solver) :: type_solver_sparse_crs_lu
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
        !! 33:  solve, iterative refinement
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
        integer :: IPARM(64)
        type(MKL_PARDISO_HANDLE) :: PT(64)
        !! Array with size of 64.
        !! Handle to internal data structure. The entries must be set to zero prior to the first call to pardiso.
        !! Unique for factorization.
        !! CAUTION: After the first call to pardiso do not directly modify pt, as that could cause a serious memory leak.
    contains
        procedure :: solve => solve_sparse_crs_lu
        procedure :: check => check_sparse_crs_lu
    end type type_solver_sparse_crs_lu

    interface
        module function construct_type_solver_sparse_crs_lu(N, MAXFCT, MNUM, MTYPE, PHASE, NRHS, MSGVLV, A) result(structure)
            implicit none
            integer(int32), intent(in) :: N
            integer(int32), intent(in) :: MAXFCT
            integer(int32), intent(in) :: MNUM
            integer(int32), intent(in) :: MTYPE
            integer(int32), intent(in) :: PHASE
            integer(int32), intent(in) :: NRHS
            integer(int32), intent(in) :: MSGVLV
            type(type_crs), intent(in) :: A
            class(abst_solver), allocatable :: structure

        end function construct_type_solver_sparse_crs_lu

        module subroutine solve_sparse_crs_lu(self, A, b, x, status)
            implicit none
            class(type_solver_sparse_crs_lu), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            real(real64), intent(inout) :: b(:)
            real(real64), intent(inout) :: x(:)
            integer(int32), intent(inout) :: status

        end subroutine solve_sparse_crs_lu

        module subroutine check_sparse_crs_lu(self, status, time)
            implicit none
            class(type_solver_sparse_crs_lu), intent(inout) :: self
            integer(int32), intent(in) :: status
            real(real64), intent(in) :: time

        end subroutine check_sparse_crs_lu

    end interface

    type, extends(abst_solver) :: type_solver_dense_lu
        integer :: N
        integer :: ERROR
        integer, allocatable :: IPIV(:)
    contains
        procedure :: solve => solve_dense_lu
        procedure :: check => check_dense_lu
    end type

    interface

        module function construct_type_solver_dense_lu(N) result(structure)
            implicit none
            integer(int32), intent(in) :: N
            class(abst_solver), allocatable :: structure

        end function construct_type_solver_dense_lu

        module subroutine solve_dense_lu(self, A, b, x, status)
            implicit none
            class(type_solver_dense_lu), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            real(real64), intent(inout) :: b(:)
            real(real64), intent(inout) :: x(:)
            integer(int32), intent(inout) :: status

        end subroutine solve_dense_lu

        module subroutine check_dense_lu(self, status, time)
            implicit none
            class(type_solver_dense_lu), intent(inout) :: self
            integer(int32), intent(in) :: status
            real(real64), intent(in) :: time

        end subroutine check_dense_lu
    end interface

    interface type_solver_sparse_crs_bicgstab
        module procedure :: construct_type_solver_sparse_crs_bicgstab
    end interface

    interface type_solver_sparse_crs_lu
        module procedure :: construct_type_solver_sparse_crs_lu
    end interface

    interface type_solver_dense_lu
        module procedure :: construct_type_solver_dense_lu
    end interface

    interface

        module subroutine create_preconditioner_jacobi(N, A, M)
            implicit none
            integer(int32), intent(in) :: N
            type(type_crs), intent(in) :: A
            real(real64), intent(inout) :: M(:)

        end subroutine create_preconditioner_jacobi

        module subroutine apply_preconditioner_jacobi(N, M, r, z)
            implicit none
            integer(int32), intent(in) :: N
            real(real64), intent(in) :: M(:)
            real(real64), intent(in) :: r(:)
            real(real64), intent(inout) :: z(:)

        end subroutine apply_preconditioner_jacobi

        module subroutine create_preconditioner_sparse_crs_bicgstab(self, A)
            implicit none
            class(type_solver_sparse_crs_bicgstab), intent(inout) :: self
            type(type_crs), intent(in) :: A

        end subroutine create_preconditioner_sparse_crs_bicgstab

        module subroutine apply_preconditioner_sparse_crs_bicgstab(self, b, x)
            implicit none
            class(type_solver_sparse_crs_bicgstab), intent(inout) :: self
            real(real64), intent(inout) :: b(:)
            real(real64), intent(inout) :: x(:)
        end subroutine apply_preconditioner_sparse_crs_bicgstab

        module subroutine destruct_type_solver_sparse_crs_bicgstab(self)
            implicit none
            type(type_solver_sparse_crs_bicgstab), intent(inout) :: self

        end subroutine destruct_type_solver_sparse_crs_bicgstab

    end interface
end module solver_solve

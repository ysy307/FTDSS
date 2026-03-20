!> Defines abstract and concrete preconditioner types.
module numerical_solver_preconditioner
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_linalg
    implicit none
    private

    public :: abst_preconditioner
    public :: type_preconditioner_none
    public :: type_preconditioner_jacobi
    public :: type_preconditioner_iluk
    public :: type_preconditioner_ssor
    public :: type_preconditioner_ilut
    public :: type_preconditioner_saamg
    public :: type_preconditioner_iluc
    public :: type_preconditioner_is
    public :: type_preconditioner_sainv
    public :: type_preconditioner_hybrid
    public :: type_preconditioner_adds

    public :: type_preconditioner_settings

    public :: create_preconditioner

    !> Configuration settings for preconditioner creation.
    !> Used to pass parameters like ID, system size, and block size to the factory.
    type :: type_preconditioner_settings
        private
        !> Preconditioner type identifier
        integer(int32) :: id = -1
        !> Number of nodes (or block rows) in the system
        integer(int32) :: num_nodes = -1
        !> Block size for block-based preconditioners (default is 1)
        integer(int32) :: block_size = -1
        !> ILU fill-in level (for ILU preconditioners)
        integer(int32) :: ilu_fill_level = 0
        !> SSOR relaxation parameter (default 1.0)
        real(real64) :: ssor_omega = 1.0d0
        !> ILUT drop tolerance (default 1.0e-3)
        real(real64) :: ilut_drop_tolerance = 1.0d-3
        !> ILUT maximum fill-in per row (default 20)
        integer(int32) :: ilut_max_fill = 20
        !> AMG strength threshold (default 0.25)
        real(real64) :: amg_strength_threshold = 0.25d0
        !> AMG smoother sweeps (default 2)
        integer(int32) :: amg_smoother_sweeps = 2
    contains
        !> Set configuration parameters.
        procedure :: set => set_preconditioner_settings
    end type type_preconditioner_settings

    !
    ! ==========================================================
    ! Abstract Preconditioner Type
    ! ==========================================================
    !
    !> Abstract base type for all preconditioners.
    !> Defines the common interface for initialization, setup, application, and destruction.
    type, abstract :: abst_preconditioner
        private
        !> Name of the preconditioner algorithm
        character(:), allocatable :: name
        !> Preconditioner type identifier
        integer(int32) :: id
        !> Internal status code
        integer(int32) :: status
    contains
        !> Initializes the preconditioner with given info.
        procedure(abst_preconditioner_initialize), pass(self), public, deferred :: initialize
        !> Sets up the preconditioner (e.g., computes factors) for a specific matrix.
        procedure(abst_preconditioner_setup), pass(self), public, deferred :: setup
        !> Applies the preconditioner \( M^{-1} \) to a vector \( r \), returning \( z \).
        procedure(abst_preconditioner_apply), pass(self), public, deferred :: apply
        !> Destructs the preconditioner instance.
        procedure(abst_preconditioner_destroy), pass(self), public, deferred :: destroy
    end type abst_preconditioner

    abstract interface
        !> Interface for initializing the preconditioner.
        subroutine abst_preconditioner_initialize(self, info)
            import :: abst_preconditioner, type_preconditioner_settings, int32
            implicit none
            !> Preconditioner instance
            class(abst_preconditioner), intent(inout) :: self
            !> Configuration settings
            type(type_preconditioner_settings), intent(in) :: info
        end subroutine abst_preconditioner_initialize

        !> Interface for setting up the preconditioner with a system matrix.
        subroutine abst_preconditioner_setup(self, A)
            import :: abst_preconditioner, abst_matrix, int32
            implicit none
            !> Preconditioner instance
            class(abst_preconditioner), intent(inout) :: self
            !> System matrix \( A \)
            class(abst_matrix), intent(in) :: A
        end subroutine abst_preconditioner_setup

        !> Interface for applying the preconditioner solve.
        subroutine abst_preconditioner_apply(self, r, z)
            import :: abst_preconditioner, type_vector_dp
            implicit none
            !> Preconditioner instance
            class(abst_preconditioner), intent(inout) :: self
            !> Input residual vector \( r \)
            type(type_vector_dp), intent(in) :: r
            !> Output vector \( z = M^{-1}r \)
            type(type_vector_dp), intent(inout) :: z
        end subroutine abst_preconditioner_apply

        !> Interface for destroying the preconditioner.
        subroutine abst_preconditioner_destroy(self)
            import :: abst_preconditioner
            implicit none
            !> Preconditioner instance
            class(abst_preconditioner), intent(inout) :: self
        end subroutine abst_preconditioner_destroy
    end interface

    !
    ! ==========================================================
    ! Identity Preconditioner (None)
    ! ==========================================================
    !
    !> Identity preconditioner (No preconditioning).
    !> Represents \( M = I \), effectively passing the residual through unchanged.
    type, extends(abst_preconditioner) :: type_preconditioner_none
    contains
        procedure :: initialize => initialize_preconditioner_none
        procedure :: setup => setup_preconditioner_none
        procedure :: apply => apply_preconditioner_none
        procedure :: destroy => destroy_preconditioner_none
    end type type_preconditioner_none

    interface
        module subroutine initialize_preconditioner_none(self, info)
            implicit none
            class(type_preconditioner_none), intent(inout) :: self
            type(type_preconditioner_settings), intent(in) :: info
        end subroutine initialize_preconditioner_none

        module subroutine setup_preconditioner_none(self, A)
            implicit none
            class(type_preconditioner_none), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
        end subroutine setup_preconditioner_none

        module subroutine apply_preconditioner_none(self, r, z)
            implicit none
            class(type_preconditioner_none), intent(inout) :: self
            type(type_vector_dp), intent(in) :: r
            type(type_vector_dp), intent(inout) :: z
        end subroutine apply_preconditioner_none

        module subroutine destroy_preconditioner_none(self)
            implicit none
            class(type_preconditioner_none), intent(inout) :: self
        end subroutine destroy_preconditioner_none
    end interface

    !
    ! ==========================================================
    ! Jacobi Preconditioner
    ! ==========================================================
    !
    !> Jacobi preconditioner.
    !> Uses the diagonal (Point Jacobi) or diagonal blocks (Block Jacobi) of the matrix as the preconditioner.
    type, extends(abst_preconditioner) :: type_preconditioner_jacobi
        !> Number of nodes (or block rows)
        integer(int32) :: num_nodes = -1
        !> Inverse of diagonal elements (for Point Jacobi)
        type(type_vector_dp) :: M_inv
        !> LU factored diagonal blocks (for Block Jacobi)
        real(real64), allocatable :: M_inv_blocks(:, :, :)
        !> Pivot indices for LU factored blocks (for Block Jacobi)
        integer(int32), allocatable :: ipiv_blocks(:, :)
        !> Size of the blocks (1 for Point Jacobi)
        integer(int32) :: block_size = 1
        !> Flag indicating if block Jacobi logic is active
        logical :: is_block = .false.
    contains
        procedure :: initialize => initialize_preconditioner_jacobi
        procedure :: setup => setup_preconditioner_jacobi
        procedure, pass(self), private :: setup_point => setup_preconditioner_jacobi_point
        procedure, pass(self), private :: setup_block => setup_preconditioner_jacobi_block
        procedure :: apply => apply_preconditioner_jacobi
        procedure :: destroy => destroy_preconditioner_jacobi
    end type type_preconditioner_jacobi

    interface
        module subroutine initialize_preconditioner_jacobi(self, info)
            implicit none
            class(type_preconditioner_jacobi), intent(inout) :: self
            type(type_preconditioner_settings), intent(in) :: info
        end subroutine initialize_preconditioner_jacobi

        module subroutine setup_preconditioner_jacobi(self, A)
            implicit none
            class(type_preconditioner_jacobi), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
        end subroutine setup_preconditioner_jacobi

        module subroutine setup_preconditioner_jacobi_point(self, A)
            implicit none
            class(type_preconditioner_jacobi), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
        end subroutine setup_preconditioner_jacobi_point

        module subroutine setup_preconditioner_jacobi_block(self, A)
            implicit none
            class(type_preconditioner_jacobi), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
        end subroutine setup_preconditioner_jacobi_block

        module subroutine apply_preconditioner_jacobi(self, r, z)
            implicit none
            class(type_preconditioner_jacobi), intent(inout) :: self
            type(type_vector_dp), intent(in) :: r
            type(type_vector_dp), intent(inout) :: z
        end subroutine apply_preconditioner_jacobi

        module subroutine destroy_preconditioner_jacobi(self)
            implicit none
            class(type_preconditioner_jacobi), intent(inout) :: self
        end subroutine destroy_preconditioner_jacobi
    end interface

!
    ! ==========================================================
    ! ILU(k) Preconditioner (Currently ILU(0))
    ! ==========================================================
    !
    !> ILU(0) / Block ILU(0) preconditioner.
    !> Stores the Incomplete LU factorization of the matrix A.
    type, extends(abst_preconditioner) :: type_preconditioner_iluk
        !> Number of nodes (rows)
        integer(int32) :: num_rows = -1
        !> Block size for BSR (1 for CSR)
        integer(int32) :: block_size = 1
        !> Level of fill-in (0 is implemented)
        integer(int32) :: fill_level = 0
        !> Flag for block matrix support
        logical :: is_block = .false.

        ! --- Storage for Sparse Structure (CSR/BSR common logic) ---
        !> Row pointers (size num_rows + 1)
        integer(int32), allocatable :: ptr(:)
        !> Column indices (size nnz)
        integer(int32), allocatable :: ind(:)
        !> Pointers to diagonal elements within val/ind arrays (size num_rows)
        integer(int32), allocatable :: diag_ptr(:)

        ! --- Storage for Values ---
        !> Non-zero values for scalar CSR (size nnz)
        real(real64), allocatable :: val(:)
        !> Non-zero blocks for BSR (size block_size, block_size, nnz)
        real(real64), allocatable :: val_blocks(:, :, :)

        ! --- Workspace for Block LU ---
        !> Pivot indices for diagonal block LU factorization (size block_size, num_rows)
        integer(int32), allocatable :: diag_pivots(:, :)

    contains
        procedure :: initialize => initialize_preconditioner_iluk
        procedure :: setup => setup_preconditioner_iluk
        procedure, pass(self), private :: setup_csr_ilu0
        procedure, pass(self), private :: setup_bsr_ilu0
        procedure :: apply => apply_preconditioner_iluk
        procedure, pass(self), private :: apply_csr_ilu0
        procedure, pass(self), private :: apply_bsr_ilu0
        procedure :: destroy => destroy_preconditioner_iluk
    end type type_preconditioner_iluk

    interface
        module subroutine initialize_preconditioner_iluk(self, info)
            implicit none
            class(type_preconditioner_iluk), intent(inout) :: self
            type(type_preconditioner_settings), intent(in) :: info
        end subroutine initialize_preconditioner_iluk

        module subroutine setup_preconditioner_iluk(self, A)
            implicit none
            class(type_preconditioner_iluk), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
        end subroutine setup_preconditioner_iluk

        module subroutine setup_csr_ilu0(self, A)
            implicit none
            class(type_preconditioner_iluk), intent(inout) :: self
            class(type_matrix_csr), intent(in) :: A
        end subroutine setup_csr_ilu0

        module subroutine setup_bsr_ilu0(self, A)
            implicit none
            class(type_preconditioner_iluk), intent(inout) :: self
            class(type_matrix_bsr), intent(in) :: A
        end subroutine setup_bsr_ilu0

        module subroutine apply_preconditioner_iluk(self, r, z)
            implicit none
            class(type_preconditioner_iluk), intent(inout) :: self
            type(type_vector_dp), intent(in) :: r
            type(type_vector_dp), intent(inout) :: z
        end subroutine apply_preconditioner_iluk

        module subroutine apply_csr_ilu0(self, r, z)
            implicit none
            class(type_preconditioner_iluk), intent(inout) :: self
            type(type_vector_dp), intent(in) :: r
            type(type_vector_dp), intent(inout) :: z
        end subroutine apply_csr_ilu0

        module subroutine apply_bsr_ilu0(self, r, z)
            implicit none
            class(type_preconditioner_iluk), intent(inout) :: self
            type(type_vector_dp), intent(in) :: r
            type(type_vector_dp), intent(inout) :: z
        end subroutine apply_bsr_ilu0

        module subroutine destroy_preconditioner_iluk(self)
            implicit none
            class(type_preconditioner_iluk), intent(inout) :: self
        end subroutine destroy_preconditioner_iluk
    end interface

    !
    ! ==========================================================
    ! SSOR Preconditioner
    ! ==========================================================
    !
    !> Symmetric Successive Over-Relaxation (SSOR) preconditioner.
    !> \( M = (D/\omega + L) (D/\omega)^{-1} (D/\omega + U) \)
    !> Supports both point and block variants for BSR matrices.
    type, extends(abst_preconditioner) :: type_preconditioner_ssor
        !> Number of nodes (or total DOFs for point mode)
        integer(int32) :: num_nodes = -1
        !> Number of block rows
        integer(int32) :: num_rows = -1
        !> Block size
        integer(int32) :: block_size = 1
        !> Relaxation parameter omega (0 < omega < 2)
        real(real64) :: omega = 1.0d0
        !> Flag for block mode
        logical :: is_block = .false.
        !> LU factored diagonal blocks scaled by 1/omega (block mode)
        real(real64), allocatable :: diag_blocks(:, :, :)
        !> Pivot indices for diagonal block LU
        integer(int32), allocatable :: diag_pivots(:, :)
        !> Diagonal pointer (index of diagonal block in ind array)
        integer(int32), allocatable :: diag_ptr(:)
        !> Diagonal values (point mode)
        type(type_vector_dp) :: diag_vals
        !> Pointers to BSR sparsity arrays (valid during solve)
        integer(int32), pointer :: a_ptr(:) => null()
        integer(int32), pointer :: a_ind(:) => null()
        real(real64), pointer :: a_val(:, :, :) => null()
    contains
        procedure :: initialize => initialize_preconditioner_ssor
        procedure :: setup => setup_preconditioner_ssor
        procedure, pass(self), private :: apply_point => apply_point_ssor
        procedure, pass(self), private :: apply_block => apply_block_ssor
        procedure :: apply => apply_preconditioner_ssor
        procedure :: destroy => destroy_preconditioner_ssor
    end type type_preconditioner_ssor

    interface
        module subroutine initialize_preconditioner_ssor(self, info)
            implicit none
            class(type_preconditioner_ssor), intent(inout) :: self
            type(type_preconditioner_settings), intent(in) :: info
        end subroutine initialize_preconditioner_ssor

        module subroutine setup_preconditioner_ssor(self, A)
            implicit none
            class(type_preconditioner_ssor), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
        end subroutine setup_preconditioner_ssor

        module subroutine apply_preconditioner_ssor(self, r, z)
            implicit none
            class(type_preconditioner_ssor), intent(inout) :: self
            type(type_vector_dp), intent(in) :: r
            type(type_vector_dp), intent(inout) :: z
        end subroutine apply_preconditioner_ssor

        module subroutine apply_point_ssor(self, r, z)
            implicit none
            class(type_preconditioner_ssor), intent(inout) :: self
            type(type_vector_dp), intent(in) :: r
            type(type_vector_dp), intent(inout) :: z
        end subroutine apply_point_ssor

        module subroutine apply_block_ssor(self, r, z)
            implicit none
            class(type_preconditioner_ssor), intent(inout) :: self
            type(type_vector_dp), intent(in) :: r
            type(type_vector_dp), intent(inout) :: z
        end subroutine apply_block_ssor

        module subroutine destroy_preconditioner_ssor(self)
            implicit none
            class(type_preconditioner_ssor), intent(inout) :: self
        end subroutine destroy_preconditioner_ssor
    end interface

    !
    ! ==========================================================
    ! ILUT Preconditioner
    ! ==========================================================
    !
    !> ILUT preconditioner: ILU with threshold dropping and fill-in control.
    type, extends(abst_preconditioner) :: type_preconditioner_ilut
        integer(int32) :: num_rows = -1
        integer(int32) :: block_size = 1
        logical :: is_block = .false.
        real(real64) :: drop_tolerance = 1.0d-3
        integer(int32) :: max_fill = 20
        integer(int32), allocatable :: ptr(:)
        integer(int32), allocatable :: ind(:)
        integer(int32), allocatable :: diag_ptr(:)
        real(real64), allocatable :: val_blocks(:, :, :)
        integer(int32), allocatable :: diag_pivots(:, :)
    contains
        procedure :: initialize => initialize_preconditioner_ilut
        procedure :: setup => setup_preconditioner_ilut
        procedure, pass(self), private :: setup_bsr => setup_bsr_ilut
        procedure :: apply => apply_preconditioner_ilut
        procedure, pass(self), private :: apply_bsr => apply_bsr_ilut
        procedure :: destroy => destroy_preconditioner_ilut
    end type type_preconditioner_ilut

    interface
        module subroutine initialize_preconditioner_ilut(self, info)
            implicit none
            class(type_preconditioner_ilut), intent(inout) :: self
            type(type_preconditioner_settings), intent(in) :: info
        end subroutine initialize_preconditioner_ilut

        module subroutine setup_preconditioner_ilut(self, A)
            implicit none
            class(type_preconditioner_ilut), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
        end subroutine setup_preconditioner_ilut

        module subroutine setup_bsr_ilut(self, A)
            implicit none
            class(type_preconditioner_ilut), intent(inout) :: self
            class(type_matrix_bsr), intent(in) :: A
        end subroutine setup_bsr_ilut

        module subroutine apply_preconditioner_ilut(self, r, z)
            implicit none
            class(type_preconditioner_ilut), intent(inout) :: self
            type(type_vector_dp), intent(in) :: r
            type(type_vector_dp), intent(inout) :: z
        end subroutine apply_preconditioner_ilut

        module subroutine apply_bsr_ilut(self, r, z)
            implicit none
            class(type_preconditioner_ilut), intent(inout) :: self
            type(type_vector_dp), intent(in) :: r
            type(type_vector_dp), intent(inout) :: z
        end subroutine apply_bsr_ilut

        module subroutine destroy_preconditioner_ilut(self)
            implicit none
            class(type_preconditioner_ilut), intent(inout) :: self
        end subroutine destroy_preconditioner_ilut
    end interface

    !
    ! ==========================================================
    ! SA-AMG Preconditioner
    ! ==========================================================
    !
    !> Two-level Smoothed Aggregation AMG preconditioner.
    type, extends(abst_preconditioner) :: type_preconditioner_saamg
        integer(int32) :: num_rows = -1
        integer(int32) :: block_size = 1
        logical :: is_block = .false.
        real(real64) :: strength_threshold = 0.25d0
        integer(int32) :: smoother_sweeps = 2
        integer(int32) :: num_coarse = -1
        !> Inverse of diagonal entries for Jacobi smoother
        real(real64), allocatable :: diag_inv(:)
        !> Diagonal block positions in fine-grid matrix
        integer(int32), allocatable :: fine_diag_ptr(:)
        !> Prolongation P (CSR: n_fine x n_coarse)
        integer(int32), allocatable :: P_ptr(:), P_ind(:)
        real(real64), allocatable :: P_val(:)
        !> Restriction R = P^T (CSR: n_coarse x n_fine)
        integer(int32), allocatable :: R_ptr(:), R_ind(:)
        real(real64), allocatable :: R_val(:)
        !> Coarse grid matrix (dense, n_coarse x n_coarse)
        real(real64), allocatable :: coarse_val(:, :)
        !> LU factorization of coarse matrix
        real(real64), allocatable :: coarse_lu(:, :)
        integer(int32), allocatable :: coarse_pivots(:)
        !> Workspace
        real(real64), allocatable :: work_fine(:), work_coarse(:)
        !> Pointers to fine-grid BSR arrays
        integer(int32), pointer :: a_ptr(:) => null()
        integer(int32), pointer :: a_ind(:) => null()
        real(real64), pointer :: a_val(:, :, :) => null()
    contains
        procedure :: initialize => initialize_preconditioner_saamg
        procedure :: setup => setup_preconditioner_saamg
        procedure :: apply => apply_preconditioner_saamg
        procedure :: destroy => destroy_preconditioner_saamg
    end type type_preconditioner_saamg

    interface
        module subroutine initialize_preconditioner_saamg(self, info)
            implicit none
            class(type_preconditioner_saamg), intent(inout) :: self
            type(type_preconditioner_settings), intent(in) :: info
        end subroutine initialize_preconditioner_saamg

        module subroutine setup_preconditioner_saamg(self, A)
            implicit none
            class(type_preconditioner_saamg), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
        end subroutine setup_preconditioner_saamg

        module subroutine apply_preconditioner_saamg(self, r, z)
            implicit none
            class(type_preconditioner_saamg), intent(inout) :: self
            type(type_vector_dp), intent(in) :: r
            type(type_vector_dp), intent(inout) :: z
        end subroutine apply_preconditioner_saamg

        module subroutine destroy_preconditioner_saamg(self)
            implicit none
            class(type_preconditioner_saamg), intent(inout) :: self
        end subroutine destroy_preconditioner_saamg
    end interface

    !
    ! ==========================================================
    ! Crout ILU Preconditioner
    ! ==========================================================
    !
    !> Crout ILU preconditioner with threshold dropping.
    type, extends(abst_preconditioner) :: type_preconditioner_iluc
        integer(int32) :: num_rows = -1
        integer(int32) :: block_size = 1
        logical :: is_block = .false.
        real(real64) :: drop_tolerance = 1.0d-3
        integer(int32), allocatable :: ptr(:), ind(:), diag_ptr(:)
        real(real64), allocatable :: val_blocks(:, :, :)
        integer(int32), allocatable :: diag_pivots(:, :)
    contains
        procedure :: initialize => initialize_preconditioner_iluc
        procedure :: setup => setup_preconditioner_iluc
        procedure, pass(self), private :: setup_bsr => setup_bsr_iluc
        procedure :: apply => apply_preconditioner_iluc
        procedure :: destroy => destroy_preconditioner_iluc
    end type type_preconditioner_iluc

    interface
        module subroutine initialize_preconditioner_iluc(self, info)
            implicit none
            class(type_preconditioner_iluc), intent(inout) :: self
            type(type_preconditioner_settings), intent(in) :: info
        end subroutine initialize_preconditioner_iluc

        module subroutine setup_preconditioner_iluc(self, A)
            implicit none
            class(type_preconditioner_iluc), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
        end subroutine setup_preconditioner_iluc

        module subroutine setup_bsr_iluc(self, A)
            implicit none
            class(type_preconditioner_iluc), intent(inout) :: self
            class(type_matrix_bsr), intent(in) :: A
        end subroutine setup_bsr_iluc

        module subroutine apply_preconditioner_iluc(self, r, z)
            implicit none
            class(type_preconditioner_iluc), intent(inout) :: self
            type(type_vector_dp), intent(in) :: r
            type(type_vector_dp), intent(inout) :: z
        end subroutine apply_preconditioner_iluc

        module subroutine destroy_preconditioner_iluc(self)
            implicit none
            class(type_preconditioner_iluc), intent(inout) :: self
        end subroutine destroy_preconditioner_iluc
    end interface

    !
    ! ==========================================================
    ! I+S Approximate Inverse Preconditioner
    ! ==========================================================
    !
    !> I+S preconditioner: z = (2I - D^{-1}A) r
    type, extends(abst_preconditioner) :: type_preconditioner_is
        integer(int32) :: num_rows = -1
        integer(int32) :: block_size = 1
        logical :: is_block = .false.
        real(real64), allocatable :: diag_blocks(:, :, :)
        integer(int32), allocatable :: diag_pivots(:, :)
        integer(int32), allocatable :: diag_ptr(:)
        real(real64), allocatable :: work(:)
        integer(int32), pointer :: a_ptr(:) => null()
        integer(int32), pointer :: a_ind(:) => null()
        real(real64), pointer :: a_val(:, :, :) => null()
    contains
        procedure :: initialize => initialize_preconditioner_is
        procedure :: setup => setup_preconditioner_is
        procedure :: apply => apply_preconditioner_is
        procedure :: destroy => destroy_preconditioner_is
    end type type_preconditioner_is

    interface
        module subroutine initialize_preconditioner_is(self, info)
            implicit none
            class(type_preconditioner_is), intent(inout) :: self
            type(type_preconditioner_settings), intent(in) :: info
        end subroutine initialize_preconditioner_is

        module subroutine setup_preconditioner_is(self, A)
            implicit none
            class(type_preconditioner_is), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
        end subroutine setup_preconditioner_is

        module subroutine apply_preconditioner_is(self, r, z)
            implicit none
            class(type_preconditioner_is), intent(inout) :: self
            type(type_vector_dp), intent(in) :: r
            type(type_vector_dp), intent(inout) :: z
        end subroutine apply_preconditioner_is

        module subroutine destroy_preconditioner_is(self)
            implicit none
            class(type_preconditioner_is), intent(inout) :: self
        end subroutine destroy_preconditioner_is
    end interface

    !
    ! ==========================================================
    ! SAINV Preconditioner
    ! ==========================================================
    !
    !> Sparse Approximate Inverse preconditioner.
    type, extends(abst_preconditioner) :: type_preconditioner_sainv
        integer(int32) :: num_rows = -1
        integer(int32) :: block_size = 1
        logical :: is_block = .false.
        real(real64) :: drop_tolerance = 1.0d-3
        real(real64), allocatable :: diag_blocks(:, :, :)
        integer(int32), allocatable :: diag_pivots(:, :)
        integer(int32), allocatable :: diag_ptr(:)
        real(real64), allocatable :: w_val(:, :, :)
        real(real64), allocatable :: work(:)
        integer(int32), pointer :: a_ptr(:) => null()
        integer(int32), pointer :: a_ind(:) => null()
        real(real64), pointer :: a_val(:, :, :) => null()
    contains
        procedure :: initialize => initialize_preconditioner_sainv
        procedure :: setup => setup_preconditioner_sainv
        procedure :: apply => apply_preconditioner_sainv
        procedure :: destroy => destroy_preconditioner_sainv
    end type type_preconditioner_sainv

    interface
        module subroutine initialize_preconditioner_sainv(self, info)
            implicit none
            class(type_preconditioner_sainv), intent(inout) :: self
            type(type_preconditioner_settings), intent(in) :: info
        end subroutine initialize_preconditioner_sainv

        module subroutine setup_preconditioner_sainv(self, A)
            implicit none
            class(type_preconditioner_sainv), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
        end subroutine setup_preconditioner_sainv

        module subroutine apply_preconditioner_sainv(self, r, z)
            implicit none
            class(type_preconditioner_sainv), intent(inout) :: self
            type(type_vector_dp), intent(in) :: r
            type(type_vector_dp), intent(inout) :: z
        end subroutine apply_preconditioner_sainv

        module subroutine destroy_preconditioner_sainv(self)
            implicit none
            class(type_preconditioner_sainv), intent(inout) :: self
        end subroutine destroy_preconditioner_sainv
    end interface

    !
    ! ==========================================================
    ! Hybrid Preconditioner
    ! ==========================================================
    !
    !> Hybrid preconditioner combining two preconditioners (SSOR + ILU).
    type, extends(abst_preconditioner) :: type_preconditioner_hybrid
        integer(int32) :: num_rows = -1
        integer(int32) :: block_size = 1
        logical :: is_block = .false.
        class(abst_preconditioner), allocatable :: inner_pc
        class(abst_preconditioner), allocatable :: outer_pc
        type(type_vector_dp) :: work1, work2
        integer(int32), pointer :: a_ptr(:) => null()
        integer(int32), pointer :: a_ind(:) => null()
        real(real64), pointer :: a_val(:, :, :) => null()
    contains
        procedure :: initialize => initialize_preconditioner_hybrid
        procedure :: setup => setup_preconditioner_hybrid
        procedure :: apply => apply_preconditioner_hybrid
        procedure :: destroy => destroy_preconditioner_hybrid
    end type type_preconditioner_hybrid

    interface
        module subroutine initialize_preconditioner_hybrid(self, info)
            implicit none
            class(type_preconditioner_hybrid), intent(inout) :: self
            type(type_preconditioner_settings), intent(in) :: info
        end subroutine initialize_preconditioner_hybrid

        module subroutine setup_preconditioner_hybrid(self, A)
            implicit none
            class(type_preconditioner_hybrid), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
        end subroutine setup_preconditioner_hybrid

        module subroutine apply_preconditioner_hybrid(self, r, z)
            implicit none
            class(type_preconditioner_hybrid), intent(inout) :: self
            type(type_vector_dp), intent(in) :: r
            type(type_vector_dp), intent(inout) :: z
        end subroutine apply_preconditioner_hybrid

        module subroutine destroy_preconditioner_hybrid(self)
            implicit none
            class(type_preconditioner_hybrid), intent(inout) :: self
        end subroutine destroy_preconditioner_hybrid
    end interface

    !
    ! ==========================================================
    ! Additive Schwarz Preconditioner
    ! ==========================================================
    !
    !> Additive Schwarz domain decomposition preconditioner.
    type, extends(abst_preconditioner) :: type_preconditioner_adds
        integer(int32) :: num_rows = -1
        integer(int32) :: block_size = 1
        logical :: is_block = .false.
        integer(int32) :: num_domains = 1
        integer(int32) :: overlap = 1
        integer(int32), allocatable :: domain_start(:), domain_end(:)
        real(real64), allocatable :: local_diag_blocks(:, :, :)
        integer(int32), allocatable :: local_diag_pivots(:, :)
        real(real64), allocatable :: work(:)
        integer(int32), pointer :: a_ptr(:) => null()
        integer(int32), pointer :: a_ind(:) => null()
        real(real64), pointer :: a_val(:, :, :) => null()
    contains
        procedure :: initialize => initialize_preconditioner_adds
        procedure :: setup => setup_preconditioner_adds
        procedure :: apply => apply_preconditioner_adds
        procedure :: destroy => destroy_preconditioner_adds
    end type type_preconditioner_adds

    interface
        module subroutine initialize_preconditioner_adds(self, info)
            implicit none
            class(type_preconditioner_adds), intent(inout) :: self
            type(type_preconditioner_settings), intent(in) :: info
        end subroutine initialize_preconditioner_adds

        module subroutine setup_preconditioner_adds(self, A)
            implicit none
            class(type_preconditioner_adds), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
        end subroutine setup_preconditioner_adds

        module subroutine apply_preconditioner_adds(self, r, z)
            implicit none
            class(type_preconditioner_adds), intent(inout) :: self
            type(type_vector_dp), intent(in) :: r
            type(type_vector_dp), intent(inout) :: z
        end subroutine apply_preconditioner_adds

        module subroutine destroy_preconditioner_adds(self)
            implicit none
            class(type_preconditioner_adds), intent(inout) :: self
        end subroutine destroy_preconditioner_adds
    end interface

contains

    !> Sets the preconditioner configuration settings.
    subroutine set_preconditioner_settings(self, id, num_nodes, block_size, ilu_fillin_level, ssor_omega)
        implicit none
        !> Settings instance to be configured
        class(type_preconditioner_settings), intent(inout) :: self
        !> Preconditioner type identifier
        integer(int32), intent(in) :: id
        !> Number of nodes/blocks (optional)
        integer(int32), intent(in), optional :: num_nodes
        !> Block size (optional, default 1)
        integer(int32), intent(in), optional :: block_size
        !> ILU fill-in level (optional, default 0)
        integer(int32), intent(in), optional :: ilu_fillin_level
        !> SSOR relaxation parameter (optional, default 1.0)
        real(real64), intent(in), optional :: ssor_omega

        self%ID = id
        select case (self%ID)
        case (PRECONDITIONER_TYPES%JACOBI%ID)
            if (present(num_nodes)) then
                self%num_nodes = num_nodes
            else
                self%num_nodes = -1
            end if
            if (present(block_size)) then
                self%block_size = block_size
            else
                self%block_size = 1
            end if
        case (PRECONDITIONER_TYPES%ILU%ID)
            if (present(num_nodes)) then
                self%num_nodes = num_nodes
            else
                self%num_nodes = -1
            end if
            if (present(block_size)) then
                self%block_size = block_size
            else
                self%block_size = 1
            end if
            if (present(ilu_fillin_level)) then
                self%ilu_fill_level = ilu_fillin_level
            else
                self%ilu_fill_level = 0
            end if
        case (PRECONDITIONER_TYPES%SSOR%ID)
            if (present(num_nodes)) then
                self%num_nodes = num_nodes
            else
                self%num_nodes = -1
            end if
            if (present(block_size)) then
                self%block_size = block_size
            else
                self%block_size = 1
            end if
            if (present(ssor_omega)) then
                self%ssor_omega = ssor_omega
            else
                self%ssor_omega = 1.0d0
            end if
        case (PRECONDITIONER_TYPES%ILUT%ID)
            if (present(num_nodes)) self%num_nodes = num_nodes
            if (present(block_size)) then
                self%block_size = block_size
            else
                self%block_size = 1
            end if
        case (PRECONDITIONER_TYPES%SAAMG%ID)
            if (present(num_nodes)) self%num_nodes = num_nodes
            if (present(block_size)) then
                self%block_size = block_size
            else
                self%block_size = 1
            end if
        case (PRECONDITIONER_TYPES%ILUC%ID, PRECONDITIONER_TYPES%IS%ID, &
              PRECONDITIONER_TYPES%SAINV%ID, PRECONDITIONER_TYPES%HYBRID%ID, &
              PRECONDITIONER_TYPES%ADDS%ID)
            if (present(num_nodes)) self%num_nodes = num_nodes
            if (present(block_size)) then
                self%block_size = block_size
            else
                self%block_size = 1
            end if
        end select
    end subroutine set_preconditioner_settings

    !> Factory routine to create a specific preconditioner instance.
    !> Allocates the derived type matching the requested ID in `info`.
    subroutine create_preconditioner(pc, info, ierr)
        implicit none
        !> Abstract pointer to hold the created preconditioner
        class(abst_preconditioner), allocatable, intent(inout) :: pc
        !> Settings defining which preconditioner to create
        type(type_preconditioner_settings), intent(in) :: info
        !> Error code
        integer(int32), intent(inout) :: ierr

        if (allocated(pc)) then
            deallocate (pc)
        end if

        select case (info%ID)
        case (PRECONDITIONER_TYPES%NONE%ID)
            allocate (type_preconditioner_none :: pc)
            call pc%initialize(info)
            ierr = pc%status
        case (PRECONDITIONER_TYPES%JACOBI%ID)
            allocate (type_preconditioner_jacobi :: pc)
            call pc%initialize(info)
            ierr = pc%status
        case (PRECONDITIONER_TYPES%ILU%ID)
            allocate (type_preconditioner_iluk :: pc)
            call pc%initialize(info)
            ierr = pc%status
        case (PRECONDITIONER_TYPES%SSOR%ID)
            allocate (type_preconditioner_ssor :: pc)
            call pc%initialize(info)
            ierr = pc%status
        case (PRECONDITIONER_TYPES%HYBRID%ID)
            allocate (type_preconditioner_hybrid :: pc)
            call pc%initialize(info)
            ierr = pc%status
        case (PRECONDITIONER_TYPES%IS%ID)
            allocate (type_preconditioner_is :: pc)
            call pc%initialize(info)
            ierr = pc%status
        case (PRECONDITIONER_TYPES%SAINV%ID)
            allocate (type_preconditioner_sainv :: pc)
            call pc%initialize(info)
            ierr = pc%status
        case (PRECONDITIONER_TYPES%SAAMG%ID)
            allocate (type_preconditioner_saamg :: pc)
            call pc%initialize(info)
            ierr = pc%status
        case (PRECONDITIONER_TYPES%ILUC%ID)
            allocate (type_preconditioner_iluc :: pc)
            call pc%initialize(info)
            ierr = pc%status
        case (PRECONDITIONER_TYPES%ILUT%ID)
            allocate (type_preconditioner_ilut :: pc)
            call pc%initialize(info)
            ierr = pc%status
        case (PRECONDITIONER_TYPES%ADDS%ID)
            allocate (type_preconditioner_adds :: pc)
            call pc%initialize(info)
            ierr = pc%status
        end select

    end subroutine create_preconditioner

end module numerical_solver_preconditioner

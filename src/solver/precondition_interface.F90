!> Defines abstract and concrete preconditioner types.
module solver_preconditioner
    use, intrinsic :: iso_fortran_env
    use :: module_core
    use :: module_linalg
    implicit none
    private

    public :: abst_preconditioner
    public :: type_preconditioner_none
    public :: type_preconditioner_jacobi
    public :: type_preconditioner_iluk

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

contains

    !> Sets the preconditioner configuration settings.
    subroutine set_preconditioner_settings(self, id, num_nodes, block_size, ilu_fillin_level)
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

        self%id = id
        select case (self%id)
        case (SOLVER_PRECONDITION_JACOBI)
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
        case (SOLVER_PRECONDITION_ILU)
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

        select case (info%id)
        case (SOLVER_PRECONDITION_NONE)
            allocate (type_preconditioner_none :: pc)
            call pc%initialize(info)
            ierr = pc%status
        case (SOLVER_PRECONDITION_JACOBI)
            allocate (type_preconditioner_jacobi :: pc)
            call pc%initialize(info)
            ierr = pc%status
        case (SOLVER_PRECONDITION_ILU)
            allocate (type_preconditioner_iluk :: pc)
            call pc%initialize(info)
            ierr = pc%status
        case (SOLVER_PRECONDITION_SSOR)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_PRECONDITION_HYBRID)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_PRECONDITION_IS)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_PRECONDITION_SAINV)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_PRECONDITION_SAAMG)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_PRECONDITION_ILUC)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_PRECONDITION_ILUT)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        end select

    end subroutine create_preconditioner

end module solver_preconditioner

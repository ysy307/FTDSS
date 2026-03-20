module numerical_solver_interface
    use, intrinsic :: iso_fortran_env, only: int32, real64, output_unit
!$  use omp_lib
    use :: stdlib_strings, only:strip
    use :: module_core
    use :: module_linalg
    use :: numerical_solver_preconditioner
    implicit none
    private

    public :: abst_solver
    public :: type_solver_bicgstab
    public :: type_solver_gmres
    public :: type_solver_fgmres
    public :: type_solver_cg
    public :: type_solver_bicg
    public :: type_solver_cgs
    public :: type_solver_bicgstab_l
    public :: type_solver_gpbicg
    public :: type_solver_tfqmr
    public :: type_solver_orthomin
    public :: type_solver_jacobi_iter
    public :: type_solver_gs
    public :: type_solver_sor
    public :: type_solver_bicgsafe
    public :: type_solver_cr
    public :: type_solver_bicr
    public :: type_solver_crs
    public :: type_solver_bicrstab
    public :: type_solver_gpbicr
    public :: type_solver_bicrsafe
    public :: type_solver_idr_s
    public :: type_solver_idr1
    public :: type_solver_minres
    public :: type_solver_cocg
    public :: type_solver_cocr

    public :: type_solver_settings
    public :: create_solver

    type :: type_solver_settings
        private
        integer(int32) :: id
        integer(int32) :: preconditioner_id
        real(real64) :: tolerance
        integer(int32) :: max_iterations
        integer(int32) :: m_restart

        integer(int32) :: num_nodes
        integer(int32) :: num_dofs_per_node
    contains
        procedure :: set => set_solver_settings
    end type type_solver_settings

    type, abstract :: abst_solver
        private
        ! Solver basic info
        !> Solver ID.
        integer(int32) :: id = -1
        !> Name of the solver.
        character(:), allocatable :: name
        !> Status of the solver after execution.
        integer(int32) :: status = SOLVER_STATUS%SUCCESS%ID

        integer(int32) :: num_nodes = -1
        integer(int32) :: num_dofs_per_node = -1
        real(real64) :: tolerance = 0.0d0
        real(real64) :: relative_tolerance = 1.0d-6
        integer(int32) :: max_iterations = 0

        type(type_vector_dp) :: residual_history
        integer(int32) :: current_iteration = 0

        !> Preconditioner associated with the solver.
        class(abst_preconditioner), allocatable :: pc
    contains
        procedure(abst_solver_initialize), pass(self), public, deferred :: initialize !&
        procedure(abst_solver_solve),      pass(self), public, deferred :: solve !&
        procedure,                         pass(self), public           :: check => check_solver !&
        procedure,                         pass(self), public           :: is_success => is_success_solver !&
        procedure,                         pass(self), public           :: display_rhistory => display_residual_history_solver !&
        procedure(abst_solver_destroy),    pass(self), public, deferred :: destroy !&
    end type abst_solver

    abstract interface
        subroutine abst_solver_initialize(self, solver_settings, preconditioner_settings)
            import :: abst_solver, type_solver_settings, type_preconditioner_settings
            implicit none
            class(abst_solver), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        end subroutine abst_solver_initialize

        subroutine abst_solver_solve(self, A, b, x)
            import :: abst_solver, abst_matrix, type_vector_dp, int32
            implicit none
            class(abst_solver), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x

        end subroutine abst_solver_solve

        subroutine abst_solver_destroy(self)
            import :: abst_solver
            implicit none
            class(abst_solver), intent(inout) :: self

        end subroutine abst_solver_destroy

    end interface

    ! ---- BiCGSTAB ----
    type, extends(abst_solver) :: type_solver_bicgstab
        type(type_vector_dp) :: p
        type(type_vector_dp) :: phat
        type(type_vector_dp) :: s
        type(type_vector_dp) :: shat
        type(type_vector_dp) :: r
        type(type_vector_dp) :: r0
        type(type_vector_dp) :: t
        type(type_vector_dp) :: v
        type(type_vector_dp) :: x

    contains
        procedure :: initialize => initialize_type_solver_bicgstab !&
        procedure :: solve      => solve_type_solver_bicgstab !&
        procedure :: destroy    => destroy_type_solver_bicgstab !&
    end type type_solver_bicgstab

    interface
        module subroutine initialize_type_solver_bicgstab(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_bicgstab), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        end subroutine initialize_type_solver_bicgstab

        module subroutine solve_type_solver_bicgstab(self, A, b, x)
            implicit none
            class(type_solver_bicgstab), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_bicgstab

        module subroutine destroy_type_solver_bicgstab(self)
            implicit none
            class(type_solver_bicgstab), intent(inout) :: self

        end subroutine destroy_type_solver_bicgstab
    end interface

    ! ---- GMRES ----
    type, extends(abst_solver) :: type_solver_gmres
        integer(int32) :: m_restart = 30

        type(type_vector_dp), allocatable :: v(:)
        type(type_vector_dp) :: r
        type(type_vector_dp) :: z
        type(type_vector_dp) :: w
        type(type_vector_dp) :: x_update

        real(real64), allocatable :: h(:, :)
        real(real64), allocatable :: g(:)
        real(real64), allocatable :: cs(:)
        real(real64), allocatable :: sn(:)
        real(real64), allocatable :: y(:)

    contains
        procedure :: initialize => initialize_type_solver_gmres
        procedure :: solve => solve_type_solver_gmres
        procedure :: destroy => destroy_type_solver_gmres
    end type type_solver_gmres

    interface
        module subroutine initialize_type_solver_gmres(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_gmres), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings

        end subroutine initialize_type_solver_gmres

        module subroutine solve_type_solver_gmres(self, A, b, x)
            implicit none
            class(type_solver_gmres), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_gmres

        module subroutine destroy_type_solver_gmres(self)
            implicit none
            class(type_solver_gmres), intent(inout) :: self

        end subroutine destroy_type_solver_gmres
    end interface

    ! ---- FGMRES ----
    type, extends(abst_solver) :: type_solver_fgmres
        integer(int32) :: m_restart = 30

        type(type_vector_dp), allocatable :: v(:)
        type(type_vector_dp), allocatable :: zv(:)
        type(type_vector_dp) :: r
        type(type_vector_dp) :: w

        real(real64), allocatable :: h(:, :)
        real(real64), allocatable :: g(:)
        real(real64), allocatable :: cs(:)
        real(real64), allocatable :: sn(:)
        real(real64), allocatable :: y(:)
    contains
        procedure :: initialize => initialize_type_solver_fgmres
        procedure :: solve => solve_type_solver_fgmres
        procedure :: destroy => destroy_type_solver_fgmres
    end type type_solver_fgmres

    interface
        module subroutine initialize_type_solver_fgmres(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_fgmres), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_fgmres

        module subroutine solve_type_solver_fgmres(self, A, b, x)
            implicit none
            class(type_solver_fgmres), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_fgmres

        module subroutine destroy_type_solver_fgmres(self)
            implicit none
            class(type_solver_fgmres), intent(inout) :: self
        end subroutine destroy_type_solver_fgmres
    end interface

    ! ---- CG ----
    type, extends(abst_solver) :: type_solver_cg
        type(type_vector_dp) :: r
        type(type_vector_dp) :: z
        type(type_vector_dp) :: p
        type(type_vector_dp) :: q
    contains
        procedure :: initialize => initialize_type_solver_cg
        procedure :: solve => solve_type_solver_cg
        procedure :: destroy => destroy_type_solver_cg
    end type type_solver_cg

    interface
        module subroutine initialize_type_solver_cg(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_cg), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_cg

        module subroutine solve_type_solver_cg(self, A, b, x)
            implicit none
            class(type_solver_cg), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_cg

        module subroutine destroy_type_solver_cg(self)
            implicit none
            class(type_solver_cg), intent(inout) :: self
        end subroutine destroy_type_solver_cg
    end interface

    ! ---- BiCG ----
    type, extends(abst_solver) :: type_solver_bicg
        type(type_vector_dp) :: r
        type(type_vector_dp) :: r0
        type(type_vector_dp) :: p
        type(type_vector_dp) :: p0
        type(type_vector_dp) :: z
        type(type_vector_dp) :: z0
        type(type_vector_dp) :: q
        type(type_vector_dp) :: q0
    contains
        procedure :: initialize => initialize_type_solver_bicg
        procedure :: solve => solve_type_solver_bicg
        procedure :: destroy => destroy_type_solver_bicg
    end type type_solver_bicg

    interface
        module subroutine initialize_type_solver_bicg(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_bicg), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_bicg

        module subroutine solve_type_solver_bicg(self, A, b, x)
            implicit none
            class(type_solver_bicg), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_bicg

        module subroutine destroy_type_solver_bicg(self)
            implicit none
            class(type_solver_bicg), intent(inout) :: self
        end subroutine destroy_type_solver_bicg
    end interface

    ! ---- CGS ----
    type, extends(abst_solver) :: type_solver_cgs
        type(type_vector_dp) :: r
        type(type_vector_dp) :: r0
        type(type_vector_dp) :: p
        type(type_vector_dp) :: phat
        type(type_vector_dp) :: u
        type(type_vector_dp) :: uhat
        type(type_vector_dp) :: q
        type(type_vector_dp) :: qhat
        type(type_vector_dp) :: v
    contains
        procedure :: initialize => initialize_type_solver_cgs
        procedure :: solve => solve_type_solver_cgs
        procedure :: destroy => destroy_type_solver_cgs
    end type type_solver_cgs

    interface
        module subroutine initialize_type_solver_cgs(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_cgs), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_cgs

        module subroutine solve_type_solver_cgs(self, A, b, x)
            implicit none
            class(type_solver_cgs), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_cgs

        module subroutine destroy_type_solver_cgs(self)
            implicit none
            class(type_solver_cgs), intent(inout) :: self
        end subroutine destroy_type_solver_cgs
    end interface

    ! ---- BiCGSTAB(l) ----
    type, extends(abst_solver) :: type_solver_bicgstab_l
        integer(int32) :: ell = 2
        type(type_vector_dp) :: r0
        type(type_vector_dp), allocatable :: u(:)
        type(type_vector_dp), allocatable :: rr(:)
        type(type_vector_dp) :: z
        real(real64), allocatable :: gamma(:)
        real(real64), allocatable :: gamma_p(:)
        real(real64), allocatable :: gamma_pp(:)
        real(real64), allocatable :: tau(:, :)
        real(real64), allocatable :: sigma(:)
    contains
        procedure :: initialize => initialize_type_solver_bicgstab_l
        procedure :: solve => solve_type_solver_bicgstab_l
        procedure :: destroy => destroy_type_solver_bicgstab_l
    end type type_solver_bicgstab_l

    interface
        module subroutine initialize_type_solver_bicgstab_l(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_bicgstab_l), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_bicgstab_l

        module subroutine solve_type_solver_bicgstab_l(self, A, b, x)
            implicit none
            class(type_solver_bicgstab_l), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_bicgstab_l

        module subroutine destroy_type_solver_bicgstab_l(self)
            implicit none
            class(type_solver_bicgstab_l), intent(inout) :: self
        end subroutine destroy_type_solver_bicgstab_l
    end interface

    ! ---- GPBiCG ----
    type, extends(abst_solver) :: type_solver_gpbicg
        type(type_vector_dp) :: r
        type(type_vector_dp) :: r0
        type(type_vector_dp) :: p
        type(type_vector_dp) :: t
        type(type_vector_dp) :: u
        type(type_vector_dp) :: w
        type(type_vector_dp) :: z
        type(type_vector_dp) :: Ap
        type(type_vector_dp) :: At
    contains
        procedure :: initialize => initialize_type_solver_gpbicg
        procedure :: solve => solve_type_solver_gpbicg
        procedure :: destroy => destroy_type_solver_gpbicg
    end type type_solver_gpbicg

    interface
        module subroutine initialize_type_solver_gpbicg(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_gpbicg), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_gpbicg

        module subroutine solve_type_solver_gpbicg(self, A, b, x)
            implicit none
            class(type_solver_gpbicg), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_gpbicg

        module subroutine destroy_type_solver_gpbicg(self)
            implicit none
            class(type_solver_gpbicg), intent(inout) :: self
        end subroutine destroy_type_solver_gpbicg
    end interface

    ! ---- TFQMR ----
    type, extends(abst_solver) :: type_solver_tfqmr
        type(type_vector_dp) :: r
        type(type_vector_dp) :: r0
        type(type_vector_dp) :: w
        type(type_vector_dp) :: u
        type(type_vector_dp) :: v
        type(type_vector_dp) :: d
        type(type_vector_dp) :: Au
        type(type_vector_dp) :: z
    contains
        procedure :: initialize => initialize_type_solver_tfqmr
        procedure :: solve => solve_type_solver_tfqmr
        procedure :: destroy => destroy_type_solver_tfqmr
    end type type_solver_tfqmr

    interface
        module subroutine initialize_type_solver_tfqmr(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_tfqmr), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_tfqmr

        module subroutine solve_type_solver_tfqmr(self, A, b, x)
            implicit none
            class(type_solver_tfqmr), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_tfqmr

        module subroutine destroy_type_solver_tfqmr(self)
            implicit none
            class(type_solver_tfqmr), intent(inout) :: self
        end subroutine destroy_type_solver_tfqmr
    end interface

    ! ---- Orthomin(m) ----
    type, extends(abst_solver) :: type_solver_orthomin
        integer(int32) :: m_restart = 10
        type(type_vector_dp) :: r
        type(type_vector_dp) :: z
        type(type_vector_dp) :: Az
        type(type_vector_dp), allocatable :: p(:)
        type(type_vector_dp), allocatable :: Ap(:)
    contains
        procedure :: initialize => initialize_type_solver_orthomin
        procedure :: solve => solve_type_solver_orthomin
        procedure :: destroy => destroy_type_solver_orthomin
    end type type_solver_orthomin

    interface
        module subroutine initialize_type_solver_orthomin(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_orthomin), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_orthomin

        module subroutine solve_type_solver_orthomin(self, A, b, x)
            implicit none
            class(type_solver_orthomin), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_orthomin

        module subroutine destroy_type_solver_orthomin(self)
            implicit none
            class(type_solver_orthomin), intent(inout) :: self
        end subroutine destroy_type_solver_orthomin
    end interface

    ! ---- Jacobi (iterative solver) ----
    type, extends(abst_solver) :: type_solver_jacobi_iter
        type(type_vector_dp) :: r
        type(type_vector_dp) :: z
    contains
        procedure :: initialize => initialize_type_solver_jacobi_iter
        procedure :: solve => solve_type_solver_jacobi_iter
        procedure :: destroy => destroy_type_solver_jacobi_iter
    end type type_solver_jacobi_iter

    interface
        module subroutine initialize_type_solver_jacobi_iter(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_jacobi_iter), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_jacobi_iter

        module subroutine solve_type_solver_jacobi_iter(self, A, b, x)
            implicit none
            class(type_solver_jacobi_iter), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_jacobi_iter

        module subroutine destroy_type_solver_jacobi_iter(self)
            implicit none
            class(type_solver_jacobi_iter), intent(inout) :: self
        end subroutine destroy_type_solver_jacobi_iter
    end interface

    ! ---- Gauss-Seidel ----
    type, extends(abst_solver) :: type_solver_gs
        type(type_vector_dp) :: r
        type(type_vector_dp) :: z
    contains
        procedure :: initialize => initialize_type_solver_gs
        procedure :: solve => solve_type_solver_gs
        procedure :: destroy => destroy_type_solver_gs
    end type type_solver_gs

    interface
        module subroutine initialize_type_solver_gs(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_gs), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_gs

        module subroutine solve_type_solver_gs(self, A, b, x)
            implicit none
            class(type_solver_gs), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_gs

        module subroutine destroy_type_solver_gs(self)
            implicit none
            class(type_solver_gs), intent(inout) :: self
        end subroutine destroy_type_solver_gs
    end interface

    ! ---- SOR ----
    type, extends(abst_solver) :: type_solver_sor
        type(type_vector_dp) :: r
        type(type_vector_dp) :: z
    contains
        procedure :: initialize => initialize_type_solver_sor
        procedure :: solve => solve_type_solver_sor
        procedure :: destroy => destroy_type_solver_sor
    end type type_solver_sor

    interface
        module subroutine initialize_type_solver_sor(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_sor), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_sor

        module subroutine solve_type_solver_sor(self, A, b, x)
            implicit none
            class(type_solver_sor), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_sor

        module subroutine destroy_type_solver_sor(self)
            implicit none
            class(type_solver_sor), intent(inout) :: self
        end subroutine destroy_type_solver_sor
    end interface

    ! ---- BiCGSafe ----
    type, extends(abst_solver) :: type_solver_bicgsafe
        type(type_vector_dp) :: r
        type(type_vector_dp) :: r0
        type(type_vector_dp) :: p
        type(type_vector_dp) :: phat
        type(type_vector_dp) :: s
        type(type_vector_dp) :: shat
        type(type_vector_dp) :: t
        type(type_vector_dp) :: v
    contains
        procedure :: initialize => initialize_type_solver_bicgsafe
        procedure :: solve => solve_type_solver_bicgsafe
        procedure :: destroy => destroy_type_solver_bicgsafe
    end type type_solver_bicgsafe

    interface
        module subroutine initialize_type_solver_bicgsafe(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_bicgsafe), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_bicgsafe

        module subroutine solve_type_solver_bicgsafe(self, A, b, x)
            implicit none
            class(type_solver_bicgsafe), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_bicgsafe

        module subroutine destroy_type_solver_bicgsafe(self)
            implicit none
            class(type_solver_bicgsafe), intent(inout) :: self
        end subroutine destroy_type_solver_bicgsafe
    end interface

    ! ---- CR ----
    type, extends(abst_solver) :: type_solver_cr
        type(type_vector_dp) :: r
        type(type_vector_dp) :: p
        type(type_vector_dp) :: q
        type(type_vector_dp) :: Ar
        type(type_vector_dp) :: Ap
    contains
        procedure :: initialize => initialize_type_solver_cr
        procedure :: solve => solve_type_solver_cr
        procedure :: destroy => destroy_type_solver_cr
    end type type_solver_cr

    interface
        module subroutine initialize_type_solver_cr(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_cr), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_cr

        module subroutine solve_type_solver_cr(self, A, b, x)
            implicit none
            class(type_solver_cr), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_cr

        module subroutine destroy_type_solver_cr(self)
            implicit none
            class(type_solver_cr), intent(inout) :: self
        end subroutine destroy_type_solver_cr
    end interface

    ! ---- BiCR ----
    type, extends(abst_solver) :: type_solver_bicr
        type(type_vector_dp) :: r
        type(type_vector_dp) :: r0
        type(type_vector_dp) :: p
        type(type_vector_dp) :: p0
        type(type_vector_dp) :: Ap
        type(type_vector_dp) :: Ap0
        type(type_vector_dp) :: Ar
        type(type_vector_dp) :: Ar0
    contains
        procedure :: initialize => initialize_type_solver_bicr
        procedure :: solve => solve_type_solver_bicr
        procedure :: destroy => destroy_type_solver_bicr
    end type type_solver_bicr

    interface
        module subroutine initialize_type_solver_bicr(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_bicr), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_bicr

        module subroutine solve_type_solver_bicr(self, A, b, x)
            implicit none
            class(type_solver_bicr), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_bicr

        module subroutine destroy_type_solver_bicr(self)
            implicit none
            class(type_solver_bicr), intent(inout) :: self
        end subroutine destroy_type_solver_bicr
    end interface

    ! ---- CRS ----
    type, extends(abst_solver) :: type_solver_crs
        type(type_vector_dp) :: r
        type(type_vector_dp) :: r0
        type(type_vector_dp) :: p
        type(type_vector_dp) :: phat
        type(type_vector_dp) :: u
        type(type_vector_dp) :: uhat
        type(type_vector_dp) :: q
        type(type_vector_dp) :: qhat
        type(type_vector_dp) :: v
        type(type_vector_dp) :: Ar
    contains
        procedure :: initialize => initialize_type_solver_crs
        procedure :: solve => solve_type_solver_crs
        procedure :: destroy => destroy_type_solver_crs
    end type type_solver_crs

    interface
        module subroutine initialize_type_solver_crs(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_crs), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_crs

        module subroutine solve_type_solver_crs(self, A, b, x)
            implicit none
            class(type_solver_crs), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_crs

        module subroutine destroy_type_solver_crs(self)
            implicit none
            class(type_solver_crs), intent(inout) :: self
        end subroutine destroy_type_solver_crs
    end interface

    ! ---- BiCRSTAB ----
    type, extends(abst_solver) :: type_solver_bicrstab
        type(type_vector_dp) :: r
        type(type_vector_dp) :: r0
        type(type_vector_dp) :: p
        type(type_vector_dp) :: phat
        type(type_vector_dp) :: s
        type(type_vector_dp) :: shat
        type(type_vector_dp) :: t
        type(type_vector_dp) :: v
        type(type_vector_dp) :: Ar
    contains
        procedure :: initialize => initialize_type_solver_bicrstab
        procedure :: solve => solve_type_solver_bicrstab
        procedure :: destroy => destroy_type_solver_bicrstab
    end type type_solver_bicrstab

    interface
        module subroutine initialize_type_solver_bicrstab(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_bicrstab), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_bicrstab

        module subroutine solve_type_solver_bicrstab(self, A, b, x)
            implicit none
            class(type_solver_bicrstab), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_bicrstab

        module subroutine destroy_type_solver_bicrstab(self)
            implicit none
            class(type_solver_bicrstab), intent(inout) :: self
        end subroutine destroy_type_solver_bicrstab
    end interface

    ! ---- GPBiCR ----
    type, extends(abst_solver) :: type_solver_gpbicr
        type(type_vector_dp) :: r
        type(type_vector_dp) :: r0
        type(type_vector_dp) :: p
        type(type_vector_dp) :: t
        type(type_vector_dp) :: u
        type(type_vector_dp) :: w
        type(type_vector_dp) :: z
        type(type_vector_dp) :: Ap
        type(type_vector_dp) :: At
        type(type_vector_dp) :: Ar
    contains
        procedure :: initialize => initialize_type_solver_gpbicr
        procedure :: solve => solve_type_solver_gpbicr
        procedure :: destroy => destroy_type_solver_gpbicr
    end type type_solver_gpbicr

    interface
        module subroutine initialize_type_solver_gpbicr(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_gpbicr), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_gpbicr

        module subroutine solve_type_solver_gpbicr(self, A, b, x)
            implicit none
            class(type_solver_gpbicr), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_gpbicr

        module subroutine destroy_type_solver_gpbicr(self)
            implicit none
            class(type_solver_gpbicr), intent(inout) :: self
        end subroutine destroy_type_solver_gpbicr
    end interface

    ! ---- BiCRSafe ----
    type, extends(abst_solver) :: type_solver_bicrsafe
        type(type_vector_dp) :: r
        type(type_vector_dp) :: r0
        type(type_vector_dp) :: p
        type(type_vector_dp) :: phat
        type(type_vector_dp) :: s
        type(type_vector_dp) :: shat
        type(type_vector_dp) :: t
        type(type_vector_dp) :: v
        type(type_vector_dp) :: Ar
    contains
        procedure :: initialize => initialize_type_solver_bicrsafe
        procedure :: solve => solve_type_solver_bicrsafe
        procedure :: destroy => destroy_type_solver_bicrsafe
    end type type_solver_bicrsafe

    interface
        module subroutine initialize_type_solver_bicrsafe(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_bicrsafe), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_bicrsafe

        module subroutine solve_type_solver_bicrsafe(self, A, b, x)
            implicit none
            class(type_solver_bicrsafe), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_bicrsafe

        module subroutine destroy_type_solver_bicrsafe(self)
            implicit none
            class(type_solver_bicrsafe), intent(inout) :: self
        end subroutine destroy_type_solver_bicrsafe
    end interface

    ! ---- IDR(s) ----
    type, extends(abst_solver) :: type_solver_idr_s
        integer(int32) :: s_param = 4
        type(type_vector_dp) :: r
        type(type_vector_dp) :: v
        type(type_vector_dp) :: t
        type(type_vector_dp) :: z
        type(type_vector_dp), allocatable :: P(:)
        type(type_vector_dp), allocatable :: G(:)
        type(type_vector_dp), allocatable :: U(:)
        real(real64), allocatable :: M_idr(:, :)
        real(real64), allocatable :: f(:)
        real(real64), allocatable :: c(:)
    contains
        procedure :: initialize => initialize_type_solver_idr_s
        procedure :: solve => solve_type_solver_idr_s
        procedure :: destroy => destroy_type_solver_idr_s
    end type type_solver_idr_s

    interface
        module subroutine initialize_type_solver_idr_s(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_idr_s), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_idr_s

        module subroutine solve_type_solver_idr_s(self, A, b, x)
            implicit none
            class(type_solver_idr_s), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_idr_s

        module subroutine destroy_type_solver_idr_s(self)
            implicit none
            class(type_solver_idr_s), intent(inout) :: self
        end subroutine destroy_type_solver_idr_s
    end interface

    ! ---- IDR(1) ----
    type, extends(abst_solver) :: type_solver_idr1
        type(type_vector_dp) :: r
        type(type_vector_dp) :: r0
        type(type_vector_dp) :: v
        type(type_vector_dp) :: t
        type(type_vector_dp) :: dv
        type(type_vector_dp) :: dr
        type(type_vector_dp) :: z
    contains
        procedure :: initialize => initialize_type_solver_idr1
        procedure :: solve => solve_type_solver_idr1
        procedure :: destroy => destroy_type_solver_idr1
    end type type_solver_idr1

    interface
        module subroutine initialize_type_solver_idr1(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_idr1), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_idr1

        module subroutine solve_type_solver_idr1(self, A, b, x)
            implicit none
            class(type_solver_idr1), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_idr1

        module subroutine destroy_type_solver_idr1(self)
            implicit none
            class(type_solver_idr1), intent(inout) :: self
        end subroutine destroy_type_solver_idr1
    end interface

    ! ---- MINRES ----
    type, extends(abst_solver) :: type_solver_minres
        type(type_vector_dp) :: v
        type(type_vector_dp) :: v_old
        type(type_vector_dp) :: v_new
        type(type_vector_dp) :: w
        type(type_vector_dp) :: w_old
        type(type_vector_dp) :: w_new
    contains
        procedure :: initialize => initialize_type_solver_minres
        procedure :: solve => solve_type_solver_minres
        procedure :: destroy => destroy_type_solver_minres
    end type type_solver_minres

    interface
        module subroutine initialize_type_solver_minres(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_minres), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_minres

        module subroutine solve_type_solver_minres(self, A, b, x)
            implicit none
            class(type_solver_minres), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_minres

        module subroutine destroy_type_solver_minres(self)
            implicit none
            class(type_solver_minres), intent(inout) :: self
        end subroutine destroy_type_solver_minres
    end interface

    ! ---- COCG ----
    type, extends(abst_solver) :: type_solver_cocg
        type(type_vector_dp) :: r
        type(type_vector_dp) :: z
        type(type_vector_dp) :: p
        type(type_vector_dp) :: q
    contains
        procedure :: initialize => initialize_type_solver_cocg
        procedure :: solve => solve_type_solver_cocg
        procedure :: destroy => destroy_type_solver_cocg
    end type type_solver_cocg

    interface
        module subroutine initialize_type_solver_cocg(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_cocg), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_cocg

        module subroutine solve_type_solver_cocg(self, A, b, x)
            implicit none
            class(type_solver_cocg), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_cocg

        module subroutine destroy_type_solver_cocg(self)
            implicit none
            class(type_solver_cocg), intent(inout) :: self
        end subroutine destroy_type_solver_cocg
    end interface

    ! ---- COCR ----
    type, extends(abst_solver) :: type_solver_cocr
        type(type_vector_dp) :: r
        type(type_vector_dp) :: p
        type(type_vector_dp) :: Ar
        type(type_vector_dp) :: Ap
    contains
        procedure :: initialize => initialize_type_solver_cocr
        procedure :: solve => solve_type_solver_cocr
        procedure :: destroy => destroy_type_solver_cocr
    end type type_solver_cocr

    interface
        module subroutine initialize_type_solver_cocr(self, solver_settings, preconditioner_settings)
            implicit none
            class(type_solver_cocr), intent(inout) :: self
            type(type_solver_settings), intent(in) :: solver_settings
            type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        end subroutine initialize_type_solver_cocr

        module subroutine solve_type_solver_cocr(self, A, b, x)
            implicit none
            class(type_solver_cocr), intent(inout) :: self
            class(abst_matrix), intent(in) :: A
            type(type_vector_dp), intent(in) :: b
            type(type_vector_dp), intent(inout) :: x
        end subroutine solve_type_solver_cocr

        module subroutine destroy_type_solver_cocr(self)
            implicit none
            class(type_solver_cocr), intent(inout) :: self
        end subroutine destroy_type_solver_cocr
    end interface

contains
    subroutine set_solver_settings(self, id, num_nodes, tolerance, max_iterations, m_restart)
        implicit none
        class(type_solver_settings), intent(inout) :: self
        integer(int32), intent(in) :: id
        integer(int32), intent(in) :: num_nodes
        real(real64), intent(in) :: tolerance
        integer(int32), intent(in) :: max_iterations
        integer(int32), intent(in), optional :: m_restart

        self%ID = id
        self%num_nodes = num_nodes
        self%tolerance = tolerance
        self%max_iterations = max_iterations

        select case (self%ID)
        case (LINEAR_SOLVER_TYPES%GMRES_M%ID, LINEAR_SOLVER_TYPES%FGMRES_M%ID, &
              LINEAR_SOLVER_TYPES%ORTHOMIN_M%ID)
            if (present(m_restart)) then
                self%m_restart = m_restart
            else
                self%m_restart = 100
            end if
        case default
            self%m_restart = 0
        end select
    end subroutine set_solver_settings

    subroutine check_solver(self, time, unit_display)
        implicit none
        class(abst_solver), intent(inout) :: self
        real(real64), intent(in), optional :: time
        integer(int32), intent(in), optional :: unit_display
        integer(int32) :: unit

        if (self%status == SOLVER_STATUS%SUCCESS%ID) return

        if (present(unit_display)) then
            unit = unit_display
        else
            unit = output_unit
        end if

        if (present(time)) then
            select case (self%status)
            case (SOLVER_STATUS%ILL_OPTIONS%ID)
                write (unit, '(a,es13.4,a)') strip(self%name), ": ", time, " Day: Solver encountered ILL_OPTIONS."
            case (SOLVER_STATUS%BREAKDOWN%ID)
                write (unit, '(a,es13.4,a)') strip(self%name), ": ", time, " Day: Solver encountered BREAKDOWN."
            case (SOLVER_STATUS%OUT_OF_MEMORY%ID)
                write (unit, '(a,es13.4,a)') strip(self%name), ": ", time, " Day: Solver encountered OUT_OF_MEMORY."
            case (SOLVER_STATUS%MAXITER%ID)
                write (unit, '(a,es13.4,a)') strip(self%name), ": ", time, " Day: Solver encountered MAXITER."
            case (SOLVER_STATUS%NOT_IMPLEMENTED%ID)
                write (unit, '(a,es13.4,a)') strip(self%name), ": ", time, " Day: Solver encountered NOT_IMPLEMENTED."
            end select
        else
            select case (self%status)
            case (SOLVER_STATUS%ILL_OPTIONS%ID)
                write (unit, '(2a)') strip(self%name), ": Solver encountered ILL_OPTIONS."
            case (SOLVER_STATUS%BREAKDOWN%ID)
                write (unit, '(2a)') strip(self%name), ": Solver encountered BREAKDOWN."
            case (SOLVER_STATUS%OUT_OF_MEMORY%ID)
                write (unit, '(2a)') strip(self%name), ": Solver encountered OUT_OF_MEMORY."
            case (SOLVER_STATUS%MAXITER%ID)
                write (unit, '(2a)') strip(self%name), ": Solver encountered MAXITER."
            case (SOLVER_STATUS%NOT_IMPLEMENTED%ID)
                write (unit, '(2a)') strip(self%name), ": Solver encountered NOT_IMPLEMENTED."
            end select
        end if

    end subroutine check_solver

    pure function is_success_solver(self) result(ret)
        implicit none
        class(abst_solver), intent(in) :: self
        logical :: ret
        ret = (self%status == SOLVER_STATUS%SUCCESS%ID)
    end function is_success_solver

    subroutine display_residual_history_solver(self, unit_display)
        implicit none
        class(abst_solver), intent(inout) :: self
        integer(int32), intent(in), optional :: unit_display

        integer(int32) :: unit
        integer(int32) :: i
        real(real64), pointer :: residual_history_ptr(:)

        if (present(unit_display)) then
            unit = unit_display
        else
            unit = output_unit
        end if

        residual_history_ptr => self%residual_history%get_data()

        write (unit, '(a)') "Residual history:"
        do i = 1, self%current_iteration
            write (unit, '(i6,2x,es13.6)') i, residual_history_ptr(i)
        end do

    end subroutine display_residual_history_solver

    subroutine create_solver(solver, solver_settings, preconditioner_settings, ierr)
        implicit none
        class(abst_solver), allocatable, intent(inout) :: solver
        type(type_solver_settings), intent(in) :: solver_settings
        type(type_preconditioner_settings), intent(in) :: preconditioner_settings
        integer(int32), intent(inout) :: ierr

        if (allocated(solver)) then
            deallocate (solver)
        end if

        select case (solver_settings%ID)
        case (LINEAR_SOLVER_TYPES%CG%ID)
            allocate (type_solver_cg :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%BICG%ID)
            allocate (type_solver_bicg :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%CGS%ID)
            allocate (type_solver_cgs :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%BICGSTAB%ID)
            allocate (type_solver_bicgstab :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%BICGSTAB_L%ID)
            allocate (type_solver_bicgstab_l :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%GPBICG%ID)
            allocate (type_solver_gpbicg :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%TFQMR%ID)
            allocate (type_solver_tfqmr :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%ORTHOMIN_M%ID)
            allocate (type_solver_orthomin :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%GMRES_M%ID)
            allocate (type_solver_gmres :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%JACOBI%ID)
            allocate (type_solver_jacobi_iter :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%GAUSS_SEIDEL%ID)
            allocate (type_solver_gs :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%SOR%ID)
            allocate (type_solver_sor :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%BICGSAFE%ID)
            allocate (type_solver_bicgsafe :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%CR%ID)
            allocate (type_solver_cr :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%BICR%ID)
            allocate (type_solver_bicr :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%CRS%ID)
            allocate (type_solver_crs :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%BICRSTAB%ID)
            allocate (type_solver_bicrstab :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%GPBICR%ID)
            allocate (type_solver_gpbicr :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%BICRSAFE%ID)
            allocate (type_solver_bicrsafe :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%FGMRES_M%ID)
            allocate (type_solver_fgmres :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%IDR_S%ID)
            allocate (type_solver_idr_s :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%IDR1%ID)
            allocate (type_solver_idr1 :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%MINRES%ID)
            allocate (type_solver_minres :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%COCG%ID)
            allocate (type_solver_cocg :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (LINEAR_SOLVER_TYPES%COCR%ID)
            allocate (type_solver_cocr :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case default
            ierr = SOLVER_STATUS%ILL_OPTIONS%ID
        end select

    end subroutine create_solver

end module numerical_solver_interface

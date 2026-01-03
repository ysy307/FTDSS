module solver_solve
    use, intrinsic :: iso_fortran_env, only: int32, real64, output_unit
!$  use omp_lib
    use :: stdlib_strings, only:strip
    use :: module_core
    use :: module_linalg
    use :: solver_preconditioner
    implicit none
    private

    public :: abst_solver
    public :: type_solver_bicgstab
    public :: type_solver_gmres

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
        integer(int32) :: status = SOLVER_STATUS_SUCCESS

        integer(int32) :: num_nodes = -1
        integer(int32) :: num_dofs_per_node = -1
        real(real64) :: tolerance = 0.0d0
        integer(int32) :: max_iterations = 0

        type(type_vector_dp) :: residual_history
        integer(int32) :: current_iteration = 0

        !> Preconditioner associated with the solver.
        class(abst_preconditioner), allocatable :: pc
    contains
        procedure(abst_solver_initialize), pass(self), public, deferred :: initialize !&
        procedure(abst_solver_solve),      pass(self), public, deferred :: solve !&
        procedure,                         pass(self), public           :: check => check_solver !&
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

    type, extends(abst_solver) :: type_solver_gmres
        integer(int32) :: m_restart = 30

        ! --- ベクトルオブジェクト（システムサイズ N） ---
        type(type_vector_dp), allocatable :: v(:) ! 基底ベクトル V (m+1)
        type(type_vector_dp) :: r ! 残差ベクトル
        type(type_vector_dp) :: z ! 作業用（前処理適用後など）
        type(type_vector_dp) :: w ! 作業用（前処理適用後など）
        type(type_vector_dp) :: x_update ! 解の更新用

        ! --- スカラー/小規模配列（サイズ m） ---
        ! これらはサイズが小さい(m x m)ため，計算効率と記述の簡潔さからFortran標準配列を使用
        real(real64), allocatable :: h(:, :) ! ヘッセンベルグ行列 (m+1, m)
        real(real64), allocatable :: g(:) ! 右辺ベクトル g (m+1)
        real(real64), allocatable :: cs(:) ! ギブンス回転 Cos (m)
        real(real64), allocatable :: sn(:) ! ギブンス回転 Sin (m)
        real(real64), allocatable :: y(:) ! 最小二乗解 (m)

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

contains
    subroutine set_solver_settings(self, id, num_nodes, tolerance, max_iterations, m_restart)
        implicit none
        class(type_solver_settings), intent(inout) :: self
        integer(int32), intent(in) :: id
        integer(int32), intent(in) :: num_nodes
        real(real64), intent(in) :: tolerance
        integer(int32), intent(in) :: max_iterations
        integer(int32), intent(in), optional :: m_restart

        self%id = id
        self%num_nodes = num_nodes
        self%tolerance = tolerance
        self%max_iterations = max_iterations

        select case (self%id)
        case (SOLVER_GMRES_M)
            if (present(m_restart)) then
                self%m_restart = m_restart
            else
                self%m_restart = 100
            end if
        case default
        end select
    end subroutine set_solver_settings

    subroutine check_solver(self, time, unit_display)
        implicit none
        class(abst_solver), intent(inout) :: self
        real(real64), intent(in), optional :: time
        integer(int32), intent(in), optional :: unit_display
        integer(int32) :: unit

        if (self%status == SOLVER_STATUS_SUCCESS) return

        if (present(unit_display)) then
            unit = unit_display
        else
            unit = output_unit
        end if

        if (present(time)) then
            select case (self%status)
            case (SOLVER_STATUS_ILL_OPTIONS)
                write (unit, '(a,es13.4,a)'), strip(self%name), ": ", time, " Day: Solver occures ILL_OPTIONS."
            case (SOLVER_STATUS_BREAKDOWN)
                write (unit, '(a,es13.4,a)'), strip(self%name), ": ", time, " Day: Solver occures BREAKDOWN."
            case (SOLVER_STATUS_OUT_OF_MEMORY)
                write (unit, '(a,es13.4,a)'), strip(self%name), ": ", time, " Day: Solver occures OUT_OF_MEMORY."
            case (SOLVER_STATUS_MAXITER)
                write (unit, '(a,es13.4,a)'), strip(self%name), ": ", time, " Day: Solver occures MAXITER."
            case (SOLVER_STATUS_NOT_IMPLEMENTED)
                write (unit, '(a,es13.4,a)'), strip(self%name), ": ", time, " Day: Solver occures NOT_IMPLEMENTED."
            end select
        else
            select case (self%status)
            case (SOLVER_STATUS_ILL_OPTIONS)
                write (unit, '(a,a)'), strip(self%name), ": Solver occures ILL_OPTIONS."
            case (SOLVER_STATUS_BREAKDOWN)
                write (unit, '(a,a)'), strip(self%name), ": Solver occures BREAKDOWN."
            case (SOLVER_STATUS_OUT_OF_MEMORY)
                write (unit, '(a,a)'), strip(self%name), ": Solver occures OUT_OF_MEMORY."
            case (SOLVER_STATUS_MAXITER)
                write (unit, '(a,a)'), strip(self%name), ": Solver occures MAXITER."
            case (SOLVER_STATUS_NOT_IMPLEMENTED)
                write (unit, '(a,a)'), strip(self%name), ": Solver occures NOT_IMPLEMENTED."
            end select
        end if
    end subroutine check_solver

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

        select case (solver_settings%id)
        case (SOLVER_CG)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_BICG)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_CGS)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_BICGSTAB)
            allocate (type_solver_bicgstab :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (SOLVER_BICGSTAB_L)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_GPBICG)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_TFQMR)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_ORTHOMIN_M)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_GMRES_M)
            allocate (type_solver_gmres :: solver)
            call solver%initialize(solver_settings, preconditioner_settings)
            ierr = solver%status
        case (SOLVER_JACOBI)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_GAUSS_SEIDEL)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_SOR)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_BICGSAFE)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_CR)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_BICR)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_CRS)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_BICRSTAB)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_GPBICR)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_BICRSAFE)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_FGMRES_M)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_IDR_S)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_IDR1)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_MINRES)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_COCG)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case (SOLVER_COCR)
            ierr = SOLVER_STATUS_NOT_IMPLEMENTED
        case default
            ierr = SOLVER_STATUS_ILL_OPTIONS
        end select

    end subroutine create_solver

end module solver_solve


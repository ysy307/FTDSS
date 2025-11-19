module solver_solve
    use, intrinsic :: iso_fortran_env, only: int32, real64
!$  use omp_lib
    use :: stdlib_strings, only:strip
    use :: module_input, only:type_input
    use :: module_core
    ! use :: module_linalg, only:norm_2, dot
    use :: module_field
    use :: solver_preconditioner
    implicit none
    private

    public :: abst_solver
    public :: type_solver_bicgstab

    public :: create_solver

    type :: type_solver_settings
        integer(int32) :: solver_id
        integer(int32) :: preconditioner_id
        real(real64) :: tolerance
        integer(int32) :: max_iterations
        integer(int32) :: m_restart

        integer(int32) :: num_nodes
        integer(int32) :: num_dofs_per_node

    end type type_solver_settings

    type, abstract :: abst_solver
        private
        character(:), allocatable :: solver_name
        integer(int32) :: solver_status
        integer(int32) :: num_nodes
        integer(int32) :: num_dofs_per_node
        real(real64) :: tolerance
        integer(int32) :: max_iterations
        class(abst_preconditioner), allocatable :: pc
    contains
        procedure(abst_solve), pass(self), public, deferred :: solve
        procedure, pass(self), public :: check => check_solver
    end type abst_solver

    abstract interface
        subroutine abst_solve(self, A, b, x)
            import :: abst_solver, type_jacobian_matrix, type_residual_vector, int32
            implicit none
            class(abst_solver), intent(inout) :: self
            type(type_jacobian_matrix), intent(in) :: A
            type(type_residual_vector), intent(in) :: b
            type(type_residual_vector), intent(inout) :: x

        end subroutine abst_solve

        subroutine abst_check(self, time)
            import :: abst_solver, int32, real64
            implicit none
            class(abst_solver), intent(inout) :: self
            real(real64), intent(in) :: time
        end subroutine abst_check
    end interface

    type, extends(abst_solver) :: type_solver_bicgstab
        type(type_residual_vector) :: p
        type(type_residual_vector) :: phat
        type(type_residual_vector) :: s
        type(type_residual_vector) :: shat
        type(type_residual_vector) :: r
        type(type_residual_vector) :: r0
        type(type_residual_vector) :: t
        type(type_residual_vector) :: v
        type(type_residual_vector) :: x

    contains
        procedure :: solve => solve_bicgstab
        final :: destruct_type_solver_bicgstab
    end type type_solver_bicgstab

    interface
        module function construct_type_solver_bicgstab(settings) result(structure)
            implicit none
            type(type_solver_settings), intent(in) :: settings
            class(abst_solver), allocatable :: structure

        end function construct_type_solver_bicgstab

        module subroutine solve_bicgstab(self, A, b, x)
            implicit none
            class(type_solver_bicgstab), intent(inout) :: self
            type(type_jacobian_matrix), intent(in) :: A
            type(type_residual_vector), intent(in) :: b
            type(type_residual_vector), intent(inout) :: x
        end subroutine solve_bicgstab

        module subroutine destruct_type_solver_bicgstab(self)
            implicit none
            type(type_solver_bicgstab), intent(inout) :: self

        end subroutine destruct_type_solver_bicgstab
    end interface

    type, extends(abst_solver) :: type_solver_gmres
        integer(int32) :: m_restart = 100 ! GMRESのリスタート回数 (m)

        ! --- GMRES のための作業配列 ---
        ! type(type_residual_vector) :: r(:) ! 残差 / 作業用 (n)
        ! type(type_residual_vector) :: z(:) ! 前処理済みベクトル (n)
        ! type(type_residual_vector) :: v(:, :) ! 基底ベクトル V (n, m+1)
        ! type(type_residual_vector) :: h(:, :) ! ヘッセンベルグ行列 H (m+1, m)
        ! type(type_residual_vector) :: g(:) ! ギブンス回転後の残差 g (m+1)
        ! type(type_residual_vector) :: c(:) ! ギブンス回転係数 c (m)
        ! type(type_residual_vector) :: s(:) ! ギブンス回転係数 s (m)
        ! type(type_residual_vector) :: y(:) ! 最小二乗問題の解 y (m)
        ! type(type_residual_vector) :: x_local(:) ! ローカルの解ベクトル (n)
    contains
        procedure :: solve => solve_gmres
        final :: destruct_type_solver_gmres
    end type type_solver_gmres

    interface
        module function construct_type_solver_gmres(settings) result(structure)
            implicit none
            type(type_solver_settings), intent(in) :: settings
            class(abst_solver), allocatable :: structure

        end function construct_type_solver_gmres

        module subroutine solve_gmres(self, A, b, x)
            implicit none
            class(type_solver_gmres), intent(inout) :: self
            type(type_jacobian_matrix), intent(in) :: A
            type(type_residual_vector), intent(in) :: b
            type(type_residual_vector), intent(inout) :: x
        end subroutine solve_gmres

        module subroutine destruct_type_solver_gmres(self)
            implicit none
            type(type_solver_gmres), intent(inout) :: self

        end subroutine destruct_type_solver_gmres
    end interface

    interface
        module function create_solver(input, target_solver, target_matrix, num_node) result(solver)
            implicit none
            type(type_input), intent(in) :: input
            character(*), intent(in) :: target_solver
            type(type_jacobian_matrix), intent(in), target :: target_matrix
            integer(int32), intent(in) :: num_node
            class(abst_solver), allocatable :: solver
        end function create_solver
    end interface

contains

    subroutine check_solver(self, time)
        implicit none
        class(abst_solver), intent(inout) :: self
        real(real64), intent(in), optional :: time

        if (self%solver_status == SOLVER_STATUS_SUCCESS) return

        if (present(time)) then
            select case (self%solver_status)
            case (SOLVER_STATUS_ILL_OPTIONS)
                write (*, '(a,es13.4,a)'), trim(self%solver_name), ": ", time, " Day: Solver occures ILL_OPTIONS."
            case (SOLVER_STATUS_BREAKDOWN)
                write (*, '(a,es13.4,a)'), trim(self%solver_name), ": ", time, " Day: Solver occures BREAKDOWN."
            case (SOLVER_STATUS_OUT_OF_MEMORY)
                write (*, '(a,es13.4,a)'), trim(self%solver_name), ": ", time, " Day: Solver occures OUT_OF_MEMORY."
            case (SOLVER_STATUS_MAXITER)
                write (*, '(a,es13.4,a)'), trim(self%solver_name), ": ", time, " Day: Solver occures MAXITER."
            case (SOLVER_STATUS_NOT_IMPLEMENTED)
                write (*, '(a,es13.4,a)'), trim(self%solver_name), ": ", time, " Day: Solver occures NOT_IMPLEMENTED."
            end select
        else
            select case (self%solver_status)
            case (SOLVER_STATUS_ILL_OPTIONS)
                write (*, '(a,a)'), trim(self%solver_name), ": Solver occures ILL_OPTIONS."
            case (SOLVER_STATUS_BREAKDOWN)
                write (*, '(a,a)'), trim(self%solver_name), ": Solver occures BREAKDOWN."
            case (SOLVER_STATUS_OUT_OF_MEMORY)
                write (*, '(a,a)'), trim(self%solver_name), ": Solver occures OUT_OF_MEMORY."
            case (SOLVER_STATUS_MAXITER)
                write (*, '(a,a)'), trim(self%solver_name), ": Solver occures MAXITER."
            case (SOLVER_STATUS_NOT_IMPLEMENTED)
                write (*, '(a,a)'), trim(self%solver_name), ": Solver occures NOT_IMPLEMENTED."
            end select
        end if
    end subroutine check_solver

end module solver_solve


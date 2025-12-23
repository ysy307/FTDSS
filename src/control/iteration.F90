module control_iteration
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core
    use :: module_input, only:type_input
    use :: module_linalg, only:vector_norm2, vector_norminf
    use :: core_constants_utils

    implicit none
    private

    public :: type_iteration

    ! --- Constants ---
    integer(int32), parameter :: ERR_ITER_INIT = 991
    real(real64), parameter :: TINY_NORM = 1.0d-12

    ! --- Types ---
    type :: type_convergence_control
        integer(int32) :: norm_type = NORM_TYPE_L2
        integer(int32) :: combination_logic = NONLINEAR_LOGIC_AND

        logical :: check_residual = .false.
        integer(int32) :: res_criteria = NONLINEAR_CRITERIA_RELATIVE
        real(real64) :: res_abs_tol = 1.0d-8
        real(real64) :: res_rel_tol = 1.0d-6

        logical :: check_update = .false.
        integer(int32) :: upd_criteria = NONLINEAR_CRITERIA_RELATIVE
        real(real64) :: upd_abs_tol = 1.0d-8
        real(real64) :: upd_rel_tol = 1.0d-6
    end type type_convergence_control

    type :: type_iterator_config
        integer(int32) :: max_iterations = 10
        integer(int32) :: update_frequency = 1
        type(type_convergence_control) :: conv_ctrl
    end type type_iterator_config

    type :: type_iteration
        private
        ! --- 全体管理 ---
        integer(int32) :: total_iter = 0

        ! --- 非線形ステップごとの管理 ---
        integer(int32) :: nonlinear_iter = 0
        logical :: is_converged = .false.

        ! --- 基準ノルム (初期値) ---
        real(real64) :: init_res_norm_l2 = 0.0d0
        real(real64) :: init_res_norm_inf = 0.0d0
        real(real64) :: init_upd_norm_l2 = 0.0d0
        real(real64) :: init_upd_norm_inf = 0.0d0

        integer(int32) :: nonlinear_solver_type = NONLINEAR_SOLVER_NONE
        type(type_iterator_config) :: config
    contains
        procedure, pass(self), public :: initialize
        procedure, pass(self), public :: reset_nonlinear
        procedure, pass(self), public :: set_initial_norms
        procedure, pass(self), public :: check_convergence
        procedure, pass(self), public :: increment_nonlinear
        procedure, pass(self), public :: increment_total
        procedure, pass(self), public :: should_continue

        ! Getters
        procedure, pass(self), public :: get_nonlinear_iter
        procedure, pass(self), public :: get_total_iter
        procedure, pass(self), public :: has_converged
        procedure, pass(self), public :: get_max_iterations

        ! Display
        procedure, pass(self), public :: display => display_status

        ! Internals
        procedure, pass(self), private :: check_single_criterion
    end type type_iteration

contains

    ! ==========================================================================
    ! Initialization
    ! ==========================================================================
    subroutine initialize(self, input)
        implicit none
        class(type_iteration), intent(inout) :: self
        type(type_input), intent(in) :: input

        self%total_iter = 0
        self%nonlinear_iter = 0
        self%is_converged = .false.

        associate ( &
            nls => input%basic%solver_settings%nonlinear_solver, &
            conv => input%basic%solver_settings%nonlinear_solver%convergence &
            )
            self%nonlinear_solver_type = nls%method

            select case (self%nonlinear_solver_type)
            case (NONLINEAR_SOLVER_NEWTON, NONLINEAR_SOLVER_MODIFIED_NEWTON, NONLINEAR_SOLVER_PICARD)
                ! --- 基本設定 ---
                self%config%max_iterations = nls%max_iterations
                self%config%update_frequency = nls%update_frequency

                ! --- 収束判定設定 ---
                self%config%conv_ctrl%norm_type = conv%norm_type
                self%config%conv_ctrl%combination_logic = conv%use_logic

                ! 判定対象のフラグ設定
                select case (conv%use_criteria)
                case (NONLINEAR_NORM_CRITERIA_NONE)
                    self%config%conv_ctrl%check_residual = .false.
                    self%config%conv_ctrl%check_update = .false.
                case (NONLINEAR_NORM_CRITERIA_RESIDUAL)
                    self%config%conv_ctrl%check_residual = .true.
                    self%config%conv_ctrl%check_update = .false.
                case (NONLINEAR_NORM_CRITERIA_UPDATE)
                    self%config%conv_ctrl%check_residual = .false.
                    self%config%conv_ctrl%check_update = .true.
                case (NONLINEAR_NORM_CRITERIA_BOTH)
                    self%config%conv_ctrl%check_residual = .true.
                    self%config%conv_ctrl%check_update = .true.
                case default
                    ! 安全策：不明な場合はチェックしない
                    self%config%conv_ctrl%check_residual = .false.
                    self%config%conv_ctrl%check_update = .false.
                end select

                ! 残差基準の詳細設定
                if (self%config%conv_ctrl%check_residual) then
                    self%config%conv_ctrl%res_criteria = conv%residual%criteria
                    self%config%conv_ctrl%res_abs_tol = conv%residual%absolute_tolerance
                    self%config%conv_ctrl%res_rel_tol = conv%residual%relative_tolerance
                end if

                ! 更新量基準の詳細設定
                if (self%config%conv_ctrl%check_update) then
                    self%config%conv_ctrl%upd_criteria = conv%update%criteria
                    self%config%conv_ctrl%upd_abs_tol = conv%update%absolute_tolerance
                    self%config%conv_ctrl%upd_rel_tol = conv%update%relative_tolerance
                end if

            case (NONLINEAR_SOLVER_NONE)
                ! 何もしない

            case default
                call error_message(ERR_ITER_INIT, c_opt="Invalid nonlinear_solver_type")
            end select
        end associate
    end subroutine initialize

    subroutine reset_nonlinear(self)
        implicit none
        class(type_iteration), intent(inout) :: self
        self%nonlinear_iter = 0
        self%is_converged = .false.
        self%init_res_norm_l2 = 0.0d0
        self%init_res_norm_inf = 0.0d0
        self%init_upd_norm_l2 = 0.0d0
        self%init_upd_norm_inf = 0.0d0
    end subroutine reset_nonlinear

    subroutine set_initial_norms(self, res_vec, upd_vec)
        implicit none
        class(type_iteration), intent(inout) :: self
        real(real64), intent(in), optional :: res_vec(:), upd_vec(:)

        if (present(res_vec)) then
            self%init_res_norm_l2 = vector_norm2(res_vec)
            self%init_res_norm_inf = vector_norminf(res_vec)
        end if
        if (present(upd_vec)) then
            self%init_upd_norm_l2 = vector_norm2(upd_vec)
            self%init_upd_norm_inf = vector_norminf(upd_vec)
        end if
    end subroutine set_initial_norms

    ! ==========================================================================
    ! Convergence Check
    ! ==========================================================================
    subroutine check_convergence(self, res_vec, upd_vec)
        implicit none
        class(type_iteration), intent(inout) :: self
        real(real64), intent(in) :: res_vec(:), upd_vec(:)

        logical :: is_res_ok, is_upd_ok

        ! ソルバーなしの場合は常に収束とみなす
        if (self%nonlinear_solver_type == NONLINEAR_SOLVER_NONE) then
            self%is_converged = .true.
            return
        end if

        is_res_ok = .true.
        is_upd_ok = .true.

        ! --- 残差チェック ---
        if (self%config%conv_ctrl%check_residual) then
            is_res_ok = self%check_single_criterion( &
                        self%config%conv_ctrl%norm_type, &
                        self%config%conv_ctrl%res_criteria, &
                        self%config%conv_ctrl%res_abs_tol, &
                        self%config%conv_ctrl%res_rel_tol, &
                        res_vec, &
                        self%init_res_norm_l2, &
                        self%init_res_norm_inf)
        end if

        ! --- 更新量チェック ---
        if (self%config%conv_ctrl%check_update) then
            is_upd_ok = self%check_single_criterion( &
                        self%config%conv_ctrl%norm_type, &
                        self%config%conv_ctrl%upd_criteria, &
                        self%config%conv_ctrl%upd_abs_tol, &
                        self%config%conv_ctrl%upd_rel_tol, &
                        upd_vec, &
                        self%init_upd_norm_l2, &
                        self%init_upd_norm_inf)
        end if

        ! --- 判定結果の結合 ---
        ! どちらもチェックしない場合は，1反復で収束とみなす（is_res_ok/is_upd_okの初期値はtrue）

        if (self%config%conv_ctrl%check_residual .and. self%config%conv_ctrl%check_update) then
            if (self%config%conv_ctrl%combination_logic == NONLINEAR_LOGIC_OR) then
                self%is_converged = is_res_ok .or. is_upd_ok
            else ! AND
                self%is_converged = is_res_ok .and. is_upd_ok
            end if
        else if (self%config%conv_ctrl%check_residual) then
            self%is_converged = is_res_ok
        else if (self%config%conv_ctrl%check_update) then
            self%is_converged = is_upd_ok
        else
            self%is_converged = .true.
        end if

    end subroutine check_convergence

    function check_single_criterion(self, norm_type, criteria, abs_tol, rel_tol, &
                                    vec, init_norm_l2, init_norm_inf) result(is_ok)
        implicit none
        class(type_iteration), intent(in) :: self
        integer(int32), intent(in) :: norm_type, criteria
        real(real64), intent(in) :: abs_tol, rel_tol, vec(:), init_norm_l2, init_norm_inf
        logical :: is_ok

        real(real64) :: current_norm, init_norm, rel_val
        logical :: abs_ok, rel_ok

        ! 1. ノルムの計算
        if (norm_type == NORM_TYPE_LINF) then
            current_norm = vector_norminf(vec)
            init_norm = init_norm_inf
        else ! NORM_TYPE_L2
            current_norm = vector_norm2(vec)
            init_norm = init_norm_l2
        end if

        ! 2. 相対誤差の計算（ゼロ除算防止）
        if (init_norm > TINY_NORM) then
            rel_val = current_norm / init_norm
        else
            ! 初期ノルムが極小の場合，相対誤差判定は危険なため 0 (OK判定寄り) または Huge (NG判定寄り) にする．
            ! ここでは，絶対誤差判定が主になると想定し，相対誤差は 0 として扱う．
            rel_val = 0.0d0
        end if

        abs_ok = (current_norm < abs_tol)
        rel_ok = (rel_val < rel_tol)

        ! 3. 判定
        select case (criteria)
        case (NONLINEAR_CRITERIA_NONE)
            ! 基準なし = 常にOK
            is_ok = .true.
        case (NONLINEAR_CRITERIA_ABSOLUTE)
            ! NOTE: 定数が定義されていると仮定．なければ NONE のフォールバックロジックを確認推奨
            is_ok = abs_ok
        case (NONLINEAR_CRITERIA_RELATIVE)
            is_ok = rel_ok
        case (NONLINEAR_CRITERIA_BOTH)
            is_ok = abs_ok .and. rel_ok
        case default
            ! デフォルトは絶対誤差とするか，エラーとするか．ここでは安全側に倒して絶対誤差．
            is_ok = abs_ok
        end select
    end function check_single_criterion

    ! ==========================================================================
    ! Utility Methods
    ! ==========================================================================
    subroutine increment_nonlinear(self)
        implicit none
        class(type_iteration), intent(inout) :: self
        self%nonlinear_iter = self%nonlinear_iter + 1
    end subroutine increment_nonlinear

    subroutine increment_total(self)
        implicit none
        class(type_iteration), intent(inout) :: self
        self%total_iter = self%total_iter + 1
    end subroutine increment_total

    function should_continue(self) result(continue_flag)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: continue_flag
        continue_flag = (.not. self%is_converged) .and. &
                        (self%nonlinear_iter < self%config%max_iterations)
    end function should_continue

    pure function get_nonlinear_iter(self) result(val)
        class(type_iteration), intent(in) :: self
        integer(int32) :: val
        val = self%nonlinear_iter
    end function get_nonlinear_iter

    pure function get_total_iter(self) result(val)
        class(type_iteration), intent(in) :: self
        integer(int32) :: val
        val = self%total_iter
    end function get_total_iter

    pure function has_converged(self) result(val)
        class(type_iteration), intent(in) :: self
        logical :: val
        val = self%is_converged
    end function has_converged

    pure function get_max_iterations(self) result(val)
        class(type_iteration), intent(in) :: self
        integer(int32) :: val
        val = self%config%max_iterations
    end function get_max_iterations

    ! ==========================================================================
    ! Display Status
    ! ==========================================================================
    subroutine display_status(self)
        implicit none
        class(type_iteration), intent(in) :: self

        write (*, '(a)') "## Iteration Status"
        write (*, '(a)') "---"
        write (*, *)

        write (*, '(a)') "### General Information"
        write (*, '(" - Total Iterations   : ", I0)') self%total_iter
        write (*, *)

        write (*, '(a)') "### Current Nonlinear Step"
        write (*, '(" - Nonlinear Iterations : ", I0)') self%nonlinear_iter
        write (*, '(" - Is Converged         : ", L1)') self%is_converged
        write (*, *)

        write (*, '(a)') "### Solver Configuration"
        write (*, '(" - Nonlinear Solver   : ", A)') trim(get_nonlinear_solver_type_string(self%nonlinear_solver_type))
        write (*, '(" - Max Iterations     : ", I0)') self%config%max_iterations
        if (self%nonlinear_solver_type == NONLINEAR_SOLVER_MODIFIED_NEWTON) then
            write (*, '(" - Update Frequency   : ", I0)') self%config%update_frequency
        end if
        write (*, *)

        ! --- Convergence Details ---
        write (*, '(a)') "### Convergence Control"
        if (.not. self%config%conv_ctrl%check_residual .and. .not. self%config%conv_ctrl%check_update) then
            write (*, '(a)') "- No convergence criteria specified."
        else
            write (*, '(" - Norm Type          : ", A)') trim(get_norm_type_string(self%config%conv_ctrl%norm_type))
            if (self%config%conv_ctrl%check_residual .and. self%config%conv_ctrl%check_update) then
                write (*, '(" - Combination Logic  : ", A)') trim(get_nonlinear_logic_string(self%config%conv_ctrl%combination_logic))
            end if
            write (*, *)

            if (self%config%conv_ctrl%check_residual) then
                write (*, '(a)') "#### Residual Criterion: ON"
                write (*, '(" - Criteria         : ", A)') trim(get_nonlinear_criteria_string(self%config%conv_ctrl%res_criteria))
                write (*, '(" - Absolute Tol.    : ", ES10.3)') self%config%conv_ctrl%res_abs_tol
                write (*, '(" - Relative Tol.    : ", ES10.3)') self%config%conv_ctrl%res_rel_tol
            else
                write (*, '(a)') "#### Residual Criterion: OFF"
            end if
            write (*, *)

            if (self%config%conv_ctrl%check_update) then
                write (*, '(a)') "#### Update Criterion: ON"
                write (*, '(" - Criteria         : ", A)') trim(get_nonlinear_criteria_string(self%config%conv_ctrl%upd_criteria))
                write (*, '(" - Absolute Tol.    : ", ES10.3)') self%config%conv_ctrl%upd_abs_tol
                write (*, '(" - Relative Tol.    : ", ES10.3)') self%config%conv_ctrl%upd_rel_tol
            else
                write (*, '(a)') "#### Update Criterion: OFF"
            end if
        end if
        write (*, '(a)') "---"
    end subroutine display_status

end module control_iteration

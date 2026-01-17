module control_iteration
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core
    use :: module_input, only:type_input
    use :: module_linalg, only:vector_norm1, vector_norm2, vector_norminf
    use :: core_constants_utils

    implicit none
    private

    public :: type_iteration

    ! --- Constants ---
    integer(int32), parameter :: ERR_ITER_INIT = 991

    !>
    !> 個別の収束判定基準（残差または更新量、1つの物理量に対して）
    !>
    type :: type_convergence_criterion
        !> この基準をチェックするかどうかのフラグ
        logical :: should_check = .false.
        !> 判定基準タイプ (Absolute, Relative, Both, None)
        type(type_constant_id) :: criterion = NONLINEAR_CRITERIA%none
        !> 絶対許容誤差
        real(real64) :: absolute_tolerance = 1.0d-8
        !> 相対許容誤差
        real(real64) :: relative_tolerance = 1.0d-6

        !> 各反復におけるノルムの履歴
        !> Dimension: (num_norm_type, max_iteration)
        real(real64), allocatable :: norms_history(:, :)
    contains
        procedure, public, pass(self) :: initialize => initialize_criterion
        procedure, public, pass(self) :: reset => reset_criterion
        procedure, public, pass(self) :: check => check_criterion_value
    end type type_convergence_criterion

    !>
    !> 全体の収束制御設定
    !>
    type :: type_convergence_control
        !> 収束判定に使用するノルムの種類 (L2, LInf, etc.)
        type(type_constant_id) :: norm_type = NORM_TYPES%L2
        !> 複数の基準（ResidualとUpdate）間の結合ロジック (AND, OR)
        type(type_constant_id) :: combination_logic = NONLINEAR_LOGICS%AND

        !> 残差ベクトルの収束基準（物理量ごと）
        type(type_convergence_criterion) :: residual(PHYSICS_TYPES%NUM_ID)
        !> 更新量ベクトルの収束基準（物理量ごと）
        type(type_convergence_criterion) :: update(PHYSICS_TYPES%NUM_ID)
    end type type_convergence_control

    !>
    !> イテレータの設定コンテナ
    !>
    type :: type_iterator_config
        integer(int32) :: max_iterations = 10
        integer(int32) :: update_frequency = 1
        type(type_convergence_control) :: convergence_control
    contains
        procedure, public, pass(self) :: initialize => initialize_config
    end type type_iterator_config

    !>
    !> 反復管理クラス (Main)
    !>
    type :: type_iteration
        private
        ! --- カウンタ ---
        integer(int32) :: total_iter = 0
        integer(int32) :: nonlinear_iter = 0

        ! --- 状態フラグ ---
        ! 物理量ごとに収束状態を保持する (Thermal, Hydro, etc.)
        logical :: is_converged(PHYSICS_TYPES%NUM_ID) = .true.

        ! --- 設定 ---
        type(type_constant_id) :: nonlinear_solver_type = NONLINEAR_SOLVER%NONE
        type(type_iterator_config) :: config

    contains
        ! Initialization / Reset
        procedure, pass(self), public :: initialize
        procedure, pass(self), public :: reset_nonlinear

        ! Operation
        procedure, pass(self), public :: increment_nonlinear
        procedure, pass(self), public :: increment_total
        procedure, pass(self), public :: check_convergence
        procedure, pass(self), public :: set_converged_flag

        ! Query
        procedure, pass(self), public :: should_continue
        procedure, pass(self), public :: has_converged
        procedure, pass(self), public :: get_nonlinear_iter
        procedure, pass(self), public :: get_total_iter
        procedure, pass(self), public :: get_max_iterations
    end type type_iteration

contains

    ! ==========================================================================
    ! type_convergence_criterion methods
    ! ==========================================================================
    subroutine initialize_criterion(self, should_check, criterion, &
                                    absolute_tolerance, relative_tolerance, max_iterations)
        implicit none
        class(type_convergence_criterion), intent(inout) :: self
        logical, intent(in) :: should_check
        type(type_constant_id), intent(in) :: criterion
        real(real64), intent(in) :: absolute_tolerance, relative_tolerance
        integer(int32), intent(in) :: max_iterations

        self%should_check = should_check
        self%criterion = criterion
        self%absolute_tolerance = absolute_tolerance
        self%relative_tolerance = relative_tolerance

        if (allocated(self%norms_history)) deallocate (self%norms_history)
        allocate (self%norms_history(NORM_TYPES%NUM_ID, max_iterations))
        self%norms_history = 0.0d0
    end subroutine initialize_criterion

    subroutine reset_criterion(self)
        implicit none
        class(type_convergence_criterion), intent(inout) :: self

        if (allocated(self%norms_history)) then
            self%norms_history = 0.0d0
        end if
    end subroutine reset_criterion

    !>
    !> ノルムを計算し、履歴に保存し、収束判定を行う
    !>
    function check_criterion_value(self, vector, iter, norm_type) result(is_ok)
        implicit none
        class(type_convergence_criterion), intent(inout) :: self
        real(real64), intent(in) :: vector(:)
        integer(int32), intent(in) :: iter
        type(type_constant_id), intent(in) :: norm_type
        logical :: is_ok

        real(real64) :: current_norm, init_norm, rel_val
        real(real64), parameter :: tiny_norm = 1.0d-14
        logical :: abs_ok, rel_ok

        ! チェック不要なら常にTrue
        if (.not. self%should_check) then
            is_ok = .true.
            return
        end if

        ! 基準なしならTrue
        if (self%criterion == NONLINEAR_CRITERIA%none) then
            is_ok = .true.
            return
        end if

        ! 1. ノルム計算と保存
        !    範囲外アクセス防止
        if (iter >= 1 .and. iter <= size(self%norms_history, 2)) then
            self%norms_history(NORM_TYPES%L1%id, iter) = vector_norm1(vector)
            self%norms_history(NORM_TYPES%L2%id, iter) = vector_norm2(vector)
            self%norms_history(NORM_TYPES%LINF%id, iter) = vector_norminf(vector)
        end if

        current_norm = self%norms_history(norm_type%id, iter)

        ! 2. 初回ノルムの取得 (Relative check用)
        !    ※ iter=1 の値を初期値とする
        init_norm = self%norms_history(norm_type%id, 1)

        ! 3. 判定ロジック
        ! Absolute Check
        abs_ok = (current_norm < self%absolute_tolerance)

        ! Relative Check
        if (init_norm > tiny_norm) then
            rel_val = current_norm / init_norm
        else
            ! 初期値がほぼゼロの場合、相対誤差は計算不能
            ! ここでは安全側に 0.0 とする（= 収束とみなす）
            rel_val = 0.0d0
        end if
        rel_ok = (rel_val < self%relative_tolerance)

        ! Criteriaによる分岐 (select case禁止のためif文)
        if (self%criterion == NONLINEAR_CRITERIA%ABSOLUTE) then
            is_ok = abs_ok
        else if (self%criterion == NONLINEAR_CRITERIA%RELATIVE) then
            is_ok = rel_ok
        else if (self%criterion == NONLINEAR_CRITERIA%BOTH) then
            is_ok = abs_ok .and. rel_ok
        else
            ! Default
            is_ok = abs_ok
        end if

    end function check_criterion_value

    ! ==========================================================================
    ! type_iterator_config methods
    ! ==========================================================================
    subroutine initialize_config(self, input)
        implicit none
        class(type_iterator_config), intent(inout) :: self
        type(type_input), intent(in) :: input

        integer(int32) :: i
        type(type_constant_id) :: nl_crit_type
        logical :: check_res, check_upd

        associate ( &
            nls => input%basic%solver_settings%nonlinear_solver, &
            conv => input%basic%solver_settings%nonlinear_solver%convergence &
            )
            ! --- 基本設定 ---
            self%max_iterations = nls%max_iterations
            self%update_frequency = nls%update_frequency

            ! --- 収束判定設定 ---
            self%convergence_control%norm_type = NORM_TYPES%to_object(conv%norm_type)
            self%convergence_control%combination_logic = NONLINEAR_LOGICS%to_object(conv%use_logic)

            nl_crit_type = NONLINEAR_NORM_CRITERIA%to_object(conv%use_criteria)

            ! Residual / Update チェックの有効化フラグ (if文で実装)
            check_res = .false.
            check_upd = .false.

            if (nl_crit_type == NONLINEAR_NORM_CRITERIA%RESIDUAL) then
                check_res = .true.
            else if (nl_crit_type == NONLINEAR_NORM_CRITERIA%UPDATE) then
                check_upd = .true.
            else if (nl_crit_type == NONLINEAR_NORM_CRITERIA%BOTH) then
                check_res = .true.
                check_upd = .true.
            end if

            ! --- 物理タイプごとの設定初期化 ---
            do i = 1, PHYSICS_TYPES%NUM_ID
                ! Residual Criteria
                call self%convergence_control%residual(i)%initialize( &
                    check_res, &
                    NONLINEAR_CRITERIA%to_object(conv%residual%criteria), &
                    conv%residual%absolute_tolerance, &
                    conv%residual%relative_tolerance, &
                    self%max_iterations)

                ! Update Criteria
                call self%convergence_control%update(i)%initialize( &
                    check_upd, &
                    NONLINEAR_CRITERIA%to_object(conv%update%criteria), &
                    conv%update%absolute_tolerance, &
                    conv%update%relative_tolerance, &
                    self%max_iterations)
            end do
        end associate
    end subroutine initialize_config

    ! ==========================================================================
    ! type_iteration methods
    ! ==========================================================================
    subroutine initialize(self, input)
        implicit none
        class(type_iteration), intent(inout) :: self
        type(type_input), intent(in) :: input

        self%total_iter = 0
        self%nonlinear_iter = 0
        ! 初期化時は安全のため .true. (計算対象外の変数が判定を邪魔しないように)
        self%is_converged(:) = .true.

        self%nonlinear_solver_type = NONLINEAR_SOLVER%to_object(input%basic%solver_settings%nonlinear_solver%method)

        if (self%nonlinear_solver_type == NONLINEAR_SOLVER%NONE) then
            return
        end if

        call self%config%initialize(input)

    end subroutine initialize

    subroutine reset_nonlinear(self)
        implicit none
        class(type_iteration), intent(inout) :: self
        integer(int32) :: i

        self%nonlinear_iter = 0

        ! [重要] 計算されていない変数がFalseのままだと永遠に終わらないため、
        !        一旦すべて「収束済み(.true.)」とする。
        !        計算対象の変数は、check_convergenceが呼ばれた時点で結果に応じて上書きされる。
        self%is_converged(:) = .true.

        do i = 1, PHYSICS_TYPES%NUM_ID
            call self%config%convergence_control%residual(i)%reset()
            call self%config%convergence_control%update(i)%reset()
        end do
    end subroutine reset_nonlinear

    pure subroutine increment_nonlinear(self)
        implicit none
        class(type_iteration), intent(inout) :: self
        self%nonlinear_iter = self%nonlinear_iter + 1
    end subroutine increment_nonlinear

    pure subroutine increment_total(self)
        implicit none
        class(type_iteration), intent(inout) :: self

        self%total_iter = self%total_iter + 1
    end subroutine increment_total

    !>
    !> 指定された物理フィールドに対する収束判定を行う
    !>
    !> @param field_id : PHYSICS_TYPES (Thermal, Hydro, etc.)
    !> @param res_vec  : 残差ベクトル
    !> @param upd_vec  : 更新量ベクトル
    !>
    subroutine check_convergence(self, field_id, res_vec, upd_vec)
        implicit none
        class(type_iteration), intent(inout) :: self
        type(type_constant_id), intent(in) :: field_id
        real(real64), intent(in) :: res_vec(:), upd_vec(:)

        logical :: is_res_ok, is_upd_ok

        ! ソルバーなしの場合は常にTrue
        if (self%nonlinear_solver_type == NONLINEAR_SOLVER%NONE) then
            self%is_converged(field_id%id) = .true.
            return
        end if

        associate (ctrl => self%config%convergence_control)

            ! 安全チェック
            if (field_id%id < 1 .or. field_id%id > PHYSICS_TYPES%NUM_ID) then
                return
            end if

            ! --- Residual Check ---
            is_res_ok = ctrl%residual(field_id%id)%check(res_vec, self%nonlinear_iter, ctrl%norm_type)

            ! --- Update Check ---
            is_upd_ok = ctrl%update(field_id%id)%check(upd_vec, self%nonlinear_iter, ctrl%norm_type)

            ! --- Combine Logic (AND / OR) ---
            if (ctrl%combination_logic == NONLINEAR_LOGICS%OR) then
                self%is_converged(field_id%id) = is_res_ok .or. is_upd_ok
            else if (ctrl%combination_logic == NONLINEAR_LOGICS%AND) then
                self%is_converged(field_id%id) = is_res_ok .and. is_upd_ok
            else
                ! Default AND
                self%is_converged(field_id%id) = is_res_ok .and. is_upd_ok
            end if

        end associate
    end subroutine check_convergence

    !>
    !> 全フィールドの収束判定が終わった後に、外部から全体収束フラグを強制設定する場合に使用
    !>
    subroutine set_converged_flag(self, converged)
        implicit none
        class(type_iteration), intent(inout) :: self
        logical, intent(in) :: converged
        self%is_converged(:) = converged
    end subroutine set_converged_flag

    function should_continue(self) result(continue_flag)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: continue_flag

        ! First iteration always continues
        if (self%nonlinear_iter == 0) then
            continue_flag = .true.
            return
        end if

        continue_flag = (.not. all(self%is_converged)) .and. &
                        (self%nonlinear_iter < self%config%max_iterations)
    end function should_continue

    pure function has_converged(self) result(val)
        class(type_iteration), intent(in) :: self
        logical :: val
        ! 全ての物理量が収束しているかチェック
        val = all(self%is_converged)
    end function has_converged

    ! --- Getters ---
    pure subroutine get_nonlinear_iter(self, val)
        class(type_iteration), intent(in) :: self
        integer(int32), intent(out) :: val
        val = self%nonlinear_iter
    end subroutine get_nonlinear_iter

    pure subroutine get_total_iter(self, val)
        class(type_iteration), intent(in) :: self
        integer(int32), intent(out) :: val
        val = self%total_iter
    end subroutine get_total_iter

    pure subroutine get_max_iterations(self, val)
        class(type_iteration), intent(in) :: self
        integer(int32), intent(out) :: val
        val = self%config%max_iterations
    end subroutine get_max_iterations

end module control_iteration

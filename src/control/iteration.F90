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
    !> Individual convergence criterion (residual or update, for one physical quantity)
    !>
    type :: type_convergence_criterion
        !> Flags whether to check convergence for this criterion
        logical, private :: should_check = .false.
        !> Criterion type (Absolute, Relative, Both, None)
        type(type_constant_id), private :: criterion = NONLINEAR_CRITERIA%none
        !> Tolerance of absolute error
        real(real64), private :: absolute_tolerance = 1.0d-8
        !> Tolerance of relative error
        real(real64), private :: relative_tolerance = 1.0d-6
        !> Reference value for relative error evaluation.
        !> This value is used as the denominator to normalize the error.
        !> The default is 1.0, which effectively performs an absolute error check.
        !> Typically, this should be set to the characteristic physical scale (e.g., max-min range).
        real(real64), private :: reference_value = 1.0d0

        !> Norm history for each iteration
        !> Dimension: (num_norm_type, max_iteration)
        !> (1,:) -> L1 norm history
        !> (2,:) -> L2 norm history
        !> (3,:) -> LInf norm history
        real(real64), allocatable, private :: norms_history(:, :)
    contains
        procedure, public, pass(self) :: initialize => initialize_type_convergence_criterion
        procedure, public, pass(self) :: reset => reset_criterion
        procedure, public, pass(self) :: check => check_convergence_criterion
    end type type_convergence_criterion

    !>
    !> Overall convergence control settings
    !>
    type :: type_convergence_control
        !> Norm type used for convergence check (L2, LInf, etc.)
        type(type_constant_id), private :: norm_type = NORM_TYPES%L2
        !> Combination logic between multiple criteria (Residual and Update) (AND, OR)
        type(type_constant_id), private :: combination_logic = NONLINEAR_LOGIC%AND
        !> Convergence criterion target in NONLINEAR_NORM_CRITERIA:
        !>   RESIDUAL : norm of residual vector
        !>   UPDATE   : norm of solution update vector
        !>   BOTH     : both residual and update norms
        type(type_constant_id), private :: convergence_norm_type = NONLINEAR_NORM_CRITERIA%RESIDUAL
        !> Residual vector convergence criteria (for each physical quantity)
        type(type_convergence_criterion), private :: residual(PHYSICS_TYPES%NUM_ID)
        !> Update vector convergence criteria (for each physical quantity)
        type(type_convergence_criterion), private :: update(PHYSICS_TYPES%NUM_ID)
    contains
        procedure, public, pass(self) :: should_check_residual => should_check_convergence_control
        procedure, public, pass(self) :: should_check_update => should_check_update_convergence_control
    end type type_convergence_control

    !>
    !> Iterator configuration container
    !>
    type :: type_iterator_config
        !> Maximum number of iterations
        integer(int32) :: max_iterations = 10
        !> Frequency of updating system matrices
        integer(int32) :: update_frequency = 1
        !> Convergence control settings
        type(type_convergence_control), private :: convergence_control
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
        logical, private :: is_converged(PHYSICS_TYPES%NUM_ID) = .true.

        ! --- 設定 ---
        type(type_constant_id), private :: nonlinear_solver_type = NONLINEAR_SOLVER%NONE
        type(type_iterator_config) :: config
    contains
        ! Initialization / Reset
        procedure, pass(self), public :: initialize
        procedure, pass(self), public :: reset_nonlinear

        ! Operation
        procedure, pass(self), public :: increment_nonlinear
        procedure, pass(self), public :: increment_total
        procedure, pass(self), public :: check_convergence
        ! Query
        procedure, pass(self), public :: should_continue
        procedure, pass(self), public :: has_converged
        procedure, pass(self), public :: get_nonlinear_iter
        procedure, pass(self), public :: get_total_iter
        procedure, pass(self), public :: get_max_iterations
        procedure, pass(self), public :: get_update_frequency
        procedure, pass(self), public :: get_nonlinear_solver => get_nonlinear_solver_type_iteration
    end type type_iteration

contains

    !> Initialize convergence criterion
    subroutine initialize_type_convergence_criterion(self, should_check, criterion, absolute_tolerance, &
                                                     relative_tolerance, max_iterations, reference_value)
        implicit none
        !> Initialize the convergence criterion with specified parameters.
        class(type_convergence_criterion), intent(inout) :: self
        !> Flag to indicate whether to check this criterion
        logical, intent(in) :: should_check
        !> Criterion type (Absolute, Relative, Both, None)
        type(type_constant_id), intent(in) :: criterion
        !> Tolerance of absolute error
        real(real64), intent(in) :: absolute_tolerance
        !> Tolerance of relative error
        real(real64), intent(in) :: relative_tolerance
        !> Maximum number of iterations for allocating norm history
        integer(int32), intent(in) :: max_iterations
        !> Reference value for relative error calculation
        real(real64), intent(in), optional :: reference_value

        self%should_check = should_check
        self%criterion = criterion
        self%absolute_tolerance = absolute_tolerance
        self%relative_tolerance = relative_tolerance

        ! Set reference value with a safeguard against near-zero values.
        ! If the physical scale is zero (e.g., uniform fields), default to 1.0 to effectively use absolute error.
        if (present(reference_value)) then
            if (abs(reference_value) < 1.0d-6) then
                self%reference_value = 1.0d0
            else
                self%reference_value = reference_value
            end if
        else
            self%reference_value = 1.0d0
        end if

        call deallocate_array(self%norms_history)
        call allocate_array(self%norms_history, NORM_TYPES%NUM_ID, max_iterations)
        self%norms_history = 0.0d0
    end subroutine initialize_type_convergence_criterion

    subroutine reset_criterion(self)
        implicit none
        class(type_convergence_criterion), intent(inout) :: self

        if (allocated(self%norms_history)) then
            self%norms_history = 0.0d0
        end if
    end subroutine reset_criterion

    !>
    !> Calculate norms, store them in history, and check convergence criteria.
    !>
    function check_convergence_criterion(self, vector, iter, norm_type) result(is_ok)
        implicit none
        class(type_convergence_criterion), intent(inout) :: self
        real(real64), intent(in) :: vector(:)
        integer(int32), intent(in) :: iter
        type(type_constant_id), intent(in) :: norm_type
        logical :: is_ok

        real(real64) :: current_norm
        logical :: abs_ok, rel_ok

        ! Return true immediately if checking is disabled
        if (.not. self%should_check) then
            is_ok = .true.
            return
        end if

        ! Return true if no criterion is set
        if (self%criterion == NONLINEAR_CRITERIA%none) then
            is_ok = .true.
            return
        end if

        ! 1. Calculate and store norms
        !    Prevent out-of-bounds access
        if (iter >= 1 .and. iter <= size(self%norms_history, 2)) then
            self%norms_history(NORM_TYPES%L1%id, iter) = vector_norm1(vector)
            self%norms_history(NORM_TYPES%L2%id, iter) = vector_norm2(vector)
            self%norms_history(NORM_TYPES%LINF%id, iter) = vector_norminf(vector)
        end if

        ! Retrieve the specified norm for the current iteration
        current_norm = self%norms_history(norm_type%id, iter)

        ! 3. Convergence logic

        ! Absolute Check
        abs_ok = (current_norm < self%absolute_tolerance)

        ! Perform standard relative error calculation
        rel_ok = (current_norm / self%reference_value < self%relative_tolerance)

        ! Determine final result based on the selected criterion
        if (self%criterion == NONLINEAR_CRITERIA%ABSOLUTE) then
            is_ok = abs_ok
        else if (self%criterion == NONLINEAR_CRITERIA%RELATIVE) then
            is_ok = rel_ok
        else if (self%criterion == NONLINEAR_CRITERIA%BOTH) then
            ! Both conditions must be met (AND condition)
            is_ok = abs_ok .and. rel_ok
        else
            ! Default to absolute check
            is_ok = abs_ok
        end if

    end function check_convergence_criterion

    !> Check if residual convergence check is enabled
    pure function should_check_convergence_control(self) result(should_check)
        implicit none
        class(type_convergence_control), intent(in) :: self
        logical :: should_check

        if (self%convergence_norm_type == NONLINEAR_NORM_CRITERIA%RESIDUAL .or. &
            self%convergence_norm_type == NONLINEAR_NORM_CRITERIA%BOTH) then
            should_check = .true.
        else
            should_check = .false.
        end if
    end function should_check_convergence_control

    !> Check if update convergence check is enabled
    pure function should_check_update_convergence_control(self) result(should_check)
        implicit none
        class(type_convergence_control), intent(in) :: self
        logical :: should_check

        if (self%convergence_norm_type == NONLINEAR_NORM_CRITERIA%UPDATE .or. &
            self%convergence_norm_type == NONLINEAR_NORM_CRITERIA%BOTH) then
            should_check = .true.
        else
            should_check = .false.
        end if
    end function should_check_update_convergence_control

    ! ==========================================================================
    ! type_iterator_config methods
    ! ==========================================================================
    subroutine initialize_config(self, input, reference_value)
        implicit none
        class(type_iterator_config), intent(inout) :: self
        type(type_input), intent(in) :: input
        real(real64), intent(in), optional :: reference_value

        integer(int32) :: i
        logical :: check_res, check_upd

        associate (nls => input%basic%solver_settings%nonlinear_solver)
            ! --- 基本設定 ---
            self%max_iterations = nls%max_iterations
            self%update_frequency = nls%update_frequency

            ! --- 収束判定設定 ---
            self%convergence_control%norm_type = NORM_TYPES%to_object(nls%convergence%norm_type)
            self%convergence_control%combination_logic = NONLINEAR_LOGIC%to_object(nls%convergence%use_logic)

            self%convergence_control%convergence_norm_type = NONLINEAR_NORM_CRITERIA%to_object(nls%convergence%use_criteria)

            ! Residual / Update チェックの有効化フラグ (if文で実装)
            check_res = self%convergence_control%should_check_residual()
            check_upd = self%convergence_control%should_check_update()

            ! --- 物理タイプごとの設定初期化 ---
            do i = 1, PHYSICS_TYPES%NUM_ID
                ! Residual Criteria
                call self%convergence_control%residual(i)%initialize( &
                    check_res, &
                    NONLINEAR_CRITERIA%to_object(nls%convergence%residual%criteria), &
                    nls%convergence%residual%absolute_tolerance, &
                    nls%convergence%residual%relative_tolerance, &
                    self%max_iterations, &
                    reference_value)

                ! Update Criteria
                call self%convergence_control%update(i)%initialize( &
                    check_upd, &
                    NONLINEAR_CRITERIA%to_object(nls%convergence%update%criteria), &
                    nls%convergence%update%absolute_tolerance, &
                    nls%convergence%update%relative_tolerance, &
                    self%max_iterations, &
                    reference_value)
            end do
        end associate
    end subroutine initialize_config

    ! ==========================================================================
    ! type_iteration methods
    ! ==========================================================================
    subroutine initialize(self, input, reference_value)
        implicit none
        class(type_iteration), intent(inout) :: self
        type(type_input), intent(in) :: input
        real(real64), intent(in), optional :: reference_value

        self%total_iter = 0
        self%nonlinear_iter = 0
        self%is_converged(:) = .true.

        self%nonlinear_solver_type = NONLINEAR_SOLVER%to_object(input%basic%solver_settings%nonlinear_solver%method)

        if (self%nonlinear_solver_type == NONLINEAR_SOLVER%NONE) then
            return
        end if

        call self%config%initialize(input, reference_value)

    end subroutine initialize

    subroutine reset_nonlinear(self)
        implicit none
        class(type_iteration), intent(inout) :: self
        integer(int32) :: i

        self%nonlinear_iter = 0

        ! [重要] 計算されていない変数がFalseのままだと永遠に終わらないため、
        !        一旦すべて「収束済み(.true.)」とする。
        !        計算対象の変数は、check_convergenceが呼ばれた時点で結果に応じて上書きされる。
        !
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
    !> Check convergence for a specific physics type based on residual and update vectors.
    !>
    subroutine check_convergence(self, physics_type, residual_vector, update_vector)
        implicit none
        class(type_iteration), intent(inout) :: self
        !> Physics type identifier defined in PHYSICS_TYPES.
        !> This identifier is used to select convergence criteria
        !> specific to each physics model.
        type(type_constant_id), intent(in) :: physics_type
        !> Residual vector used for convergence checking.
        !>
        !> This vector represents the residual evaluated by the nonlinear solver.
        !> Its exact definition depends on the solver type (e.g. Newton, Picard),
        !> but this routine treats it only as a residual norm input.
        !>
        !> This argument must be present if residual-based convergence
        !> checking is enabled.
        real(real64), intent(in), optional :: residual_vector(:)
        !> Solution update vector used for convergence checking.
        !>
        !> This vector represents the increment of the solution between
        !> successive nonlinear iterations.
        !>
        !> This argument must be present if update-based convergence
        !> checking is enabled.
        real(real64), intent(in), optional :: update_vector(:)

        logical :: is_residual_ok, is_update_ok
        logical :: check_residual, check_update

        ! ソルバーなしの場合は常にTrue
        if (self%nonlinear_solver_type == NONLINEAR_SOLVER%NONE) then
            self%is_converged(physics_type%id) = .true.
            return
        end if

        if (.not. PHYSICS_TYPES%is_valid(physics_type)) then
            !! TODO: エラーハンドリング
            return
        end if

        check_residual = self%config%convergence_control%should_check_residual()
        check_update = self%config%convergence_control%should_check_update()

        is_residual_ok = .true.
        is_update_ok   = .true.


        associate (control => self%config%convergence_control)
            ! --- Residual vector check ---
            if (check_residual) then
                if (present(residual_vector)) then
                    is_residual_ok = control%residual(physics_type%id)%check( &
                                     residual_vector, self%nonlinear_iter, control%norm_type)
                else
                    is_residual_ok = .false.
                end if
            end if

            ! --- Update vector check ---
            if (check_update) then
                if (present(update_vector)) then
                    is_update_ok = control%update(physics_type%id)%check( &
                                   update_vector, self%nonlinear_iter, control%norm_type)
                else
                    is_update_ok = .false.
                end if
            end if

            ! --- Combine Logic (AND / OR) ---
            if (control%combination_logic == NONLINEAR_LOGIC%OR) then
                self%is_converged(physics_type%id) = is_residual_ok .or. is_update_ok
            else ! Default AND
                self%is_converged(physics_type%id) = is_residual_ok .and. is_update_ok
            end if

        end associate
    end subroutine check_convergence

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
        implicit none
        class(type_iteration), intent(in) :: self
        integer(int32), intent(inout) :: val

        val = self%nonlinear_iter
    end subroutine get_nonlinear_iter

    pure subroutine get_total_iter(self, val)
        implicit none
        class(type_iteration), intent(in) :: self
        integer(int32), intent(inout) :: val

        val = self%total_iter
    end subroutine get_total_iter

    pure subroutine get_max_iterations(self, val)
        implicit none
        class(type_iteration), intent(in) :: self
        integer(int32), intent(inout) :: val

        val = self%config%max_iterations
    end subroutine get_max_iterations

    pure subroutine get_update_frequency(self, val)
        implicit none
        class(type_iteration), intent(in) :: self
        integer(int32), intent(inout) :: val

        val = self%config%update_frequency
    end subroutine get_update_frequency

    subroutine get_nonlinear_solver_type_iteration(self, val)
        implicit none
        class(type_iteration), intent(in), target :: self
        type(type_constant_id), intent(inout), pointer :: val

        val => self%nonlinear_solver_type
    end subroutine get_nonlinear_solver_type_iteration

end module control_iteration

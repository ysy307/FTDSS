module control_iteration
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_strings, only:strip
    use :: module_core
    use :: module_input, only:type_input
    use :: module_linalg, only:vector_norm1, vector_norm2, vector_norminf

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
        procedure, public, pass(self) :: should_check_residual => should_check_residual_convergence_control
        procedure, public, pass(self) :: should_check_update => should_check_update_convergence_control
    end type type_convergence_control

    !>
    !> Iterator configuration container
    !>
    type :: type_iterator_config
        !> Maximum number of iterations
        integer(int32), private :: max_iterations = 10
        !> Frequency of updating system matrices
        integer(int32), private :: update_frequency = 1
        !> Convergence control settings
        type(type_convergence_control), private :: convergence_control
    contains
        procedure, public, pass(self) :: initialize => initialize_config
    end type type_iterator_config

    !>
    !> 反復管理クラス (Main)
    !>
    type :: type_iteration
        ! --- カウンタ ---
        integer(int32), private :: total_iter = 0
        integer(int32), private :: nonlinear_iter = 0

        ! --- 状態フラグ ---
        ! 物理量ごとに収束状態を保持する (Thermal, Hydro, etc.)
        logical, private :: is_converged(PHYSICS_TYPES%NUM_ID) = .true.
        logical, private :: is_diverged(PHYSICS_TYPES%NUM_ID) = .false.

        ! --- 設定 ---
        ! nonlinear_solver_type: 入力設定に基づく静的な設定 (NONE/PICARD/NEWTON)
        ! compute_nonlinear_solver_type: 計算中に使用される動的なソルバータイプ
        type(type_constant_id), private :: nonlinear_solver_type = NONLINEAR_SOLVER%PICARD
        type(type_constant_id), private :: compute_nonlinear_solver_type = NONLINEAR_SOLVER%PICARD

        type(type_iterator_config), private :: config
    contains
        ! Initialization / Reset
        procedure, public, pass(self) :: initialize
        procedure, public, pass(self) :: reset => reset_nonlinear

        ! Operation
        procedure, public, pass(self) :: increment_nonlinear
        procedure, public, pass(self) :: increment_total
        procedure, public, pass(self) :: check_convergence
        ! Query
        procedure, public, pass(self) :: should_continue => should_continue_iteration
        procedure, public, pass(self) :: has_converged => has_converged_iteration
        procedure, public, pass(self) :: has_diverged => has_diverged_iteration

        procedure, public, pass(self) :: set_converged
        procedure, public, pass(self) :: set_diverged

        procedure, public, pass(self) :: get_nonlinear_iter
        procedure, public, pass(self) :: get_total_iter
        procedure, public, pass(self) :: get_max_iterations
        procedure, public, pass(self) :: get_update_frequency

        procedure, public, pass(self) :: get_current_update_norm => get_current_update_norm_iteration
        procedure, public, pass(self) :: get_current_residual_norm => get_current_residual_norm_iteration

        procedure, public, pass(self) :: get_absolute_tolerance => get_absolute_tolerance_iteration
        procedure, public, pass(self) :: get_relative_tolerance => get_relative_tolerance_iteration

        ! 以下のメソッドは compute_nonlinear_solver_type (動的) を操作/参照するように変更
        procedure, public, pass(self) :: get_nonlinear_solver => get_nonlinear_type_iteration
        procedure, public, pass(self) :: set_nonlinear_solver => set_nonlinear_type_iteration

        procedure, public, pass(self) :: is_compute_newton => is_compute_newton_method_iteration
        procedure, public, pass(self) :: is_compute_picard => is_compute_picard_method_iteration
        procedure, public, pass(self) :: is_compute_none => is_compute_none_method_iteration
        procedure, public, pass(self) :: is_newton => is_newton_method_iteration
        procedure, public, pass(self) :: is_picard => is_picard_method_iteration
        procedure, public, pass(self) :: is_none => is_none_method_iteration
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
        call allocate_array(self%norms_history, NORM_TYPES%NUM_ID, max(max_iterations, 1))
        self%norms_history = 0.0d0
    end subroutine initialize_type_convergence_criterion

    subroutine reset_criterion(self)
        implicit none
        class(type_convergence_criterion), intent(inout) :: self

        if (allocated(self%norms_history)) then
            self%norms_history = 0.0d0
        end if
    end subroutine reset_criterion

    !> Calculate norms, store them in history, and check convergence criteria.
    function check_convergence_criterion(self, vector, iter, norm_type) result(is_ok)
        implicit none
        class(type_convergence_criterion), intent(inout) :: self
        real(real64), intent(in) :: vector(:)
        integer(int32), intent(in) :: iter
        type(type_constant_id), intent(in) :: norm_type
        logical :: is_ok

        real(real64) :: current_norm
        logical :: abs_ok, rel_ok

        is_ok = .false.

        if (iter >= 1 .and. iter <= size(self%norms_history, 2)) then
            self%norms_history(NORM_TYPES%L1%ID, iter) = vector_norm1(vector)
            self%norms_history(NORM_TYPES%L2%ID, iter) = vector_norm2(vector)
            self%norms_history(NORM_TYPES%LINF%ID, iter) = vector_norminf(vector)

            write (*, '(A, I6, A, F12.6, A, F12.6, A, F12.6)') '    [Debug] Iteration:', iter, ' Norms - L1:', &
                self%norms_history(NORM_TYPES%L1%ID, iter), &
                ' L2:', self%norms_history(NORM_TYPES%L2%ID, iter), &
                ' LInf:', self%norms_history(NORM_TYPES%LINF%ID, iter)
        end if

        if (.not. self%should_check) then
            is_ok = .true.
            return
        end if

        if (self%criterion == NONLINEAR_CRITERIA%none) then
            is_ok = .true.
            return
        end if

        current_norm = self%norms_history(norm_type%ID, iter)

        abs_ok = (current_norm < self%absolute_tolerance)
        rel_ok = (current_norm / self%reference_value < self%relative_tolerance)

        if (self%criterion == NONLINEAR_CRITERIA%ABSOLUTE) then
            is_ok = abs_ok
        else if (self%criterion == NONLINEAR_CRITERIA%RELATIVE) then
            is_ok = rel_ok
        else if (self%criterion == NONLINEAR_CRITERIA%BOTH) then
            is_ok = abs_ok .and. rel_ok
        else
            is_ok = abs_ok
        end if

    end function check_convergence_criterion

    pure function should_check_residual_convergence_control(self) result(should_check)
        implicit none
        class(type_convergence_control), intent(in) :: self
        logical :: should_check

        if (self%convergence_norm_type == NONLINEAR_NORM_CRITERIA%RESIDUAL .or. &
            self%convergence_norm_type == NONLINEAR_NORM_CRITERIA%BOTH) then
            should_check = .true.
        else
            should_check = .false.
        end if
    end function should_check_residual_convergence_control

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
            self%max_iterations = nls%max_iterations
            self%update_frequency = nls%update_frequency

            self%convergence_control%norm_type = NORM_TYPES%to_object(nls%convergence%norm_type)
            self%convergence_control%combination_logic = NONLINEAR_LOGIC%to_object(nls%convergence%use_logic)
            self%convergence_control%convergence_norm_type = NONLINEAR_NORM_CRITERIA%to_object(nls%convergence%use_criteria)

            check_res = self%convergence_control%should_check_residual()
            check_upd = self%convergence_control%should_check_update()

            do i = 1, PHYSICS_TYPES%NUM_ID
                call self%convergence_control%residual(i)%initialize( &
                    check_res, &
                    NONLINEAR_CRITERIA%to_object(nls%convergence%residual%criteria), &
                    nls%convergence%residual%absolute_tolerance, &
                    nls%convergence%residual%relative_tolerance, &
                    self%max_iterations, &
                    reference_value)

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
        self%is_diverged(:) = .false.

        ! 1. 設定値(config)をロード
        self%nonlinear_solver_type = NONLINEAR_SOLVER%to_object(input%basic%solver_settings%nonlinear_solver%method)

        ! 2. 計算用(compute)の初期値を設定値と同じにする
        self%compute_nonlinear_solver_type = self%nonlinear_solver_type

        ! NONEの場合はコンフィグの初期化や以降の処理は不要だが，
        ! ルーチンの安全性のためコンフィグ初期化は呼んでも良い．
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

        self%is_converged(:) = .true.

        do i = 1, PHYSICS_TYPES%NUM_ID
            call self%config%convergence_control%residual(i)%reset()
            call self%config%convergence_control%update(i)%reset()
        end do

        ! 初期ステップ周辺の戦略設定:
        ! 設定(nonlinear_solver_type)がNONEでない場合のみ，Picard法から開始する．
        ! 設定がNONEの場合は，計算用(compute)もNONEとする．
        if (self%nonlinear_solver_type /= NONLINEAR_SOLVER%NONE) then
            call self%set_nonlinear_solver(NONLINEAR_SOLVER%PICARD)
        else
            call self%set_nonlinear_solver(NONLINEAR_SOLVER%NONE)
        end if

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

    subroutine check_convergence(self, physics_type, residual_vector, update_vector)
        implicit none
        class(type_iteration), intent(inout) :: self
        type(type_constant_id), intent(in) :: physics_type
        real(real64), intent(in), optional :: residual_vector(:)
        real(real64), intent(in), optional :: update_vector(:)

        logical :: is_residual_ok, is_update_ok
        logical :: check_residual, check_update

        ! 設定がソルバーなし(NONE)の場合は，収束判定をスキップして常にTrueとする．
        ! これにより，1回のループ(線形ステップ)後にループを抜けることができる．
        if (self%nonlinear_solver_type == NONLINEAR_SOLVER%NONE) then
            self%is_converged(physics_type%ID) = .true.
            return
        end if

        if (.not. PHYSICS_TYPES%is_valid(physics_type)) then
            call raise_error(ERROR_CODES%INVALID_TYPE, opt=strip(physics_type%name))
        end if

        check_residual = self%config%convergence_control%should_check_residual()
        check_update = self%config%convergence_control%should_check_update()

        is_residual_ok = .true.
        is_update_ok = .true.

        associate (control => self%config%convergence_control)
            ! --- Residual vector check ---
            if (present(residual_vector)) then
                write (*, '(A, i0)') '    [Debug] Checking residual convergence for physics type ID: ', physics_type%ID
                is_residual_ok = control%residual(physics_type%ID)%check( &
                                 residual_vector, self%nonlinear_iter, control%norm_type)
                if (.not. check_residual) is_residual_ok = .true.
            else
                is_residual_ok = .not. check_residual
            end if

            ! --- Update vector check ---
            if (present(update_vector)) then
                write (*, '(A, i0)') '    [Debug] Checking update convergence for physics type ID: ', physics_type%ID
                is_update_ok = control%update(physics_type%ID)%check( &
                               update_vector, self%nonlinear_iter, control%norm_type)
                if (.not. check_update) is_update_ok = .true.
            else
                is_update_ok = .not. check_update
            end if

            ! --- Combine Logic (AND / OR) ---
            if (control%combination_logic == NONLINEAR_LOGIC%OR) then
                self%is_converged(physics_type%ID) = is_residual_ok .or. is_update_ok
            else ! Default AND
                self%is_converged(physics_type%ID) = is_residual_ok .and. is_update_ok
            end if

        end associate
    end subroutine check_convergence

    function should_continue_iteration(self) result(should_continue)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: should_continue

        if (self%nonlinear_iter <= 1) then
            should_continue = .true.
            return
        end if

        should_continue = (.not. self%has_converged()) .and. &
                          (self%nonlinear_iter < self%config%max_iterations)
    end function should_continue_iteration

    pure function has_converged_iteration(self) result(has_converged)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: has_converged

        has_converged = all(self%is_converged)
    end function has_converged_iteration

    pure function has_diverged_iteration(self) result(has_diverged)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: has_diverged

        has_diverged = any(self%is_diverged)
    end function has_diverged_iteration

    subroutine set_converged(self, physics_type, converged)
        implicit none
        class(type_iteration), intent(inout) :: self
        type(type_constant_id), intent(in) :: physics_type
        logical, intent(in) :: converged

        if (.not. PHYSICS_TYPES%is_valid(physics_type)) then
            call raise_error(ERROR_CODES%INVALID_TYPE, opt=strip(physics_type%name))
        end if

        self%is_converged(physics_type%ID) = converged
    end subroutine set_converged

    subroutine set_diverged(self, physics_type, diverged)
        implicit none
        class(type_iteration), intent(inout) :: self
        type(type_constant_id), intent(in) :: physics_type
        logical, intent(in) :: diverged

        if (.not. PHYSICS_TYPES%is_valid(physics_type)) then
            call raise_error(ERROR_CODES%INVALID_TYPE, opt=strip(physics_type%name))
        end if

        self%is_diverged(physics_type%ID) = diverged
    end subroutine set_diverged

    ! --- Getters ---
    pure subroutine get_nonlinear_iter(self, nonlinear_iter)
        implicit none
        class(type_iteration), intent(in) :: self
        integer(int32), intent(inout) :: nonlinear_iter

        nonlinear_iter = self%nonlinear_iter
    end subroutine get_nonlinear_iter

    pure subroutine get_total_iter(self, total_iter)
        implicit none
        class(type_iteration), intent(in) :: self
        integer(int32), intent(inout) :: total_iter

        total_iter = self%total_iter
    end subroutine get_total_iter

    pure subroutine get_max_iterations(self, max_iterations)
        implicit none
        class(type_iteration), intent(in) :: self
        integer(int32), intent(inout) :: max_iterations

        max_iterations = self%config%max_iterations
    end subroutine get_max_iterations

    pure subroutine get_update_frequency(self, update_frequency)
        implicit none
        class(type_iteration), intent(in) :: self
        integer(int32), intent(inout) :: update_frequency

        update_frequency = self%config%update_frequency
    end subroutine get_update_frequency

    pure subroutine get_current_residual_norm_iteration(self, physics_type, norm_type, current_norm)
        implicit none
        class(type_iteration), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        type(type_constant_id), intent(in) :: norm_type
        real(real64), intent(inout) :: current_norm

        current_norm = 0.0d0
        if (.not. PHYSICS_TYPES%is_valid(physics_type)) return
        if (.not. NORM_TYPES%is_valid(norm_type)) return

        associate (criterion => self%config%convergence_control%residual(physics_type%ID))
            if (allocated(criterion%norms_history)) then
                if (self%nonlinear_iter >= 1 .and. self%nonlinear_iter <= size(criterion%norms_history, 2)) then
                    current_norm = criterion%norms_history(norm_type%ID, self%nonlinear_iter)
                end if
            end if
        end associate
    end subroutine get_current_residual_norm_iteration

    pure subroutine get_current_update_norm_iteration(self, physics_type, norm_type, current_norm)
        implicit none
        class(type_iteration), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        type(type_constant_id), intent(in) :: norm_type
        real(real64), intent(inout) :: current_norm

        current_norm = 0.0d0
        if (.not. PHYSICS_TYPES%is_valid(physics_type)) return
        if (.not. NORM_TYPES%is_valid(norm_type)) return

        if (self%nonlinear_iter >= 1 .and. self%nonlinear_iter <= self%config%max_iterations) then
            current_norm = &
                self%config%convergence_control%update(physics_type%ID)%norms_history(norm_type%ID, self%nonlinear_iter)
        end if
    end subroutine get_current_update_norm_iteration

    pure subroutine get_absolute_tolerance_iteration(self, physics_type, criteria_type, absolute_tolerance)
        implicit none
        class(type_iteration), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        type(type_constant_id), intent(in) :: criteria_type
        real(real64), intent(inout) :: absolute_tolerance

        absolute_tolerance = 0.0d0
        if (.not. PHYSICS_TYPES%is_valid(physics_type)) return
        if (.not. NONLINEAR_NORM_CRITERIA%is_valid(criteria_type)) return

        if (criteria_type == NONLINEAR_NORM_CRITERIA%UPDATE) then
            absolute_tolerance = &
                self%config%convergence_control%update(physics_type%ID)%absolute_tolerance
        elseif (criteria_type == NONLINEAR_NORM_CRITERIA%RESIDUAL) then
            absolute_tolerance = &
                self%config%convergence_control%residual(physics_type%ID)%absolute_tolerance
        end if
    end subroutine get_absolute_tolerance_iteration

    pure subroutine get_relative_tolerance_iteration(self, physics_type, criteria_type, relative_tolerance)
        implicit none
        class(type_iteration), intent(in) :: self
        type(type_constant_id), intent(in) :: physics_type
        type(type_constant_id), intent(in) :: criteria_type
        real(real64), intent(inout) :: relative_tolerance

        relative_tolerance = 0.0d0
        if (.not. PHYSICS_TYPES%is_valid(physics_type)) return
        if (.not. NONLINEAR_NORM_CRITERIA%is_valid(criteria_type)) return

        if (criteria_type == NONLINEAR_NORM_CRITERIA%UPDATE) then
            relative_tolerance = &
                self%config%convergence_control%update(physics_type%ID)%relative_tolerance
        elseif (criteria_type == NONLINEAR_NORM_CRITERIA%RESIDUAL) then
            relative_tolerance = &
                self%config%convergence_control%residual(physics_type%ID)%relative_tolerance
        end if
    end subroutine get_relative_tolerance_iteration

    !> Returns the ACTIVE (compute) solver type
    subroutine get_nonlinear_type_iteration(self, nonlinear_solver_type)
        implicit none
        class(type_iteration), intent(in), target :: self
        type(type_constant_id), intent(inout), pointer :: nonlinear_solver_type

        ! 離散化ルーチンが参照すべき「現在の」ソルバータイプを返す
        nonlinear_solver_type => self%compute_nonlinear_solver_type
    end subroutine get_nonlinear_type_iteration

    !> Sets the ACTIVE (compute) solver type
    subroutine set_nonlinear_type_iteration(self, nonlinear_solver_type)
        implicit none
        class(type_iteration), intent(inout) :: self
        type(type_constant_id), intent(in) :: nonlinear_solver_type

        ! 計算中に動的に切り替わるソルバータイプを設定する
        self%compute_nonlinear_solver_type = nonlinear_solver_type
    end subroutine set_nonlinear_type_iteration

    !> Returns true if the ACTIVE solver is Newton-Raphson
    !> (Returns false if solver is NONE)
    pure function is_compute_newton_method_iteration(self) result(is_newton)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: is_newton

        is_newton = (self%compute_nonlinear_solver_type == NONLINEAR_SOLVER%NEWTON .or. &
                     self%compute_nonlinear_solver_type == NONLINEAR_SOLVER%MODIFIED_NEWTON)
    end function is_compute_newton_method_iteration

    !> Returns true if the ACTIVE solver is Picard
    !> (Returns false if solver is NONE)
    pure function is_compute_picard_method_iteration(self) result(is_picard)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: is_picard

        is_picard = (self%compute_nonlinear_solver_type == NONLINEAR_SOLVER%PICARD .or. &
                     self%compute_nonlinear_solver_type == NONLINEAR_SOLVER%MODIFIED_PICARD)
    end function is_compute_picard_method_iteration

    !> Returns true if the ACTIVE solver is Newton-Raphson
    !> (Returns false if solver is NONE)
    pure function is_newton_method_iteration(self) result(is_newton)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: is_newton

        is_newton = (self%nonlinear_solver_type == NONLINEAR_SOLVER%NEWTON .or. &
                     self%nonlinear_solver_type == NONLINEAR_SOLVER%MODIFIED_NEWTON)
    end function is_newton_method_iteration

    !> Returns true if the ACTIVE solver is Picard
    !> (Returns false if solver is NONE)
    pure function is_picard_method_iteration(self) result(is_picard)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: is_picard

        is_picard = (self%nonlinear_solver_type == NONLINEAR_SOLVER%PICARD .or. &
                     self%nonlinear_solver_type == NONLINEAR_SOLVER%MODIFIED_PICARD)
    end function is_picard_method_iteration

    !> Returns true if the ACTIVE solver is NONE
    pure function is_compute_none_method_iteration(self) result(is_none)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: is_none

        is_none = (self%compute_nonlinear_solver_type == NONLINEAR_SOLVER%NONE)
    end function is_compute_none_method_iteration

    !> Returns true if the ACTIVE solver is NONE
    pure function is_none_method_iteration(self) result(is_none)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: is_none

        is_none = (self%nonlinear_solver_type == NONLINEAR_SOLVER%NONE)
    end function is_none_method_iteration

end module control_iteration

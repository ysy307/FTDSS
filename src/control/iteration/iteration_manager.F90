module control_iteration_manager
    use, intrinsic :: iso_fortran_env
    use :: stdlib_strings, only:strip
    use :: module_core
    use :: control_iteration_setting, only:type_iteration_setting
    use :: control_iteration_strategy, only:type_iteration_strategy
    implicit none

    !>
    !> 反復管理クラス (Main)
    !>
    type :: type_iteration
        ! --- カウンタ ---
        integer(int32), private :: total_iter = 0
        integer(int32), private :: nonlinear_iter = 0

        ! --- 状態フラグ ---
        ! 物理量ごとに収束状態を保持する (Thermal, Hydro, etc.)
        logical, private :: converged(PHYSICS_TYPES%NUM_ID) = .true.
        logical, private :: diverged(PHYSICS_TYPES%NUM_ID) = .false.

        ! --- 設定 ---
        ! nonlinear_solver_type: 入力設定に基づく静的な設定 (NONE/PICARD/NEWTON)
        ! compute_nonlinear_solver_type: 計算中に使用される動的なソルバータイプ
        type(type_constant_id), private :: nonlinear_solver_type = NONLINEAR_SOLVER%PICARD
        type(type_constant_id), private :: compute_nonlinear_solver_type = NONLINEAR_SOLVER%PICARD

        type(type_iteration_setting), private :: settings
        type(type_iteration_strategy), private :: strategy
    contains
        ! ---- Lifecycle ----
        procedure, public, pass(self) :: initialize => initialize_type_iteration

        ! ---- Mutator ----
        procedure, public, pass(self) :: reset => reset_iteration
        procedure, public, pass(self) :: set_nonlinear_solver => set_nonlinear_solver_iteration
        procedure, public, pass(self) :: set_converged => set_converged_iteration
        procedure, public, pass(self) :: set_diverged => set_diverged_iteration

        ! ---- Algorithm / Operation ----
        ! ---- Inquiry ----
        procedure, public, pass(self) :: is_converged => is_converged_iteration
        procedure, public, pass(self) :: is_diverged => is_diverged_iteration
        procedure, public, pass(self) :: is_compute_newton_method => is_compute_newton_method_iteration
        procedure, public, pass(self) :: is_compute_picard_method => is_compute_picard_method_iteration
        procedure, public, pass(self) :: is_compute_none_method => is_compute_none_method_iteration
        procedure, public, pass(self) :: is_newton_method => is_newton_method_iteration
        procedure, public, pass(self) :: is_picard_method => is_picard_method_iteration
        procedure, public, pass(self) :: is_none_method => is_none_method_iteration
        ! ---- Getter ----
        ! ---- Meta / Utility ----
    end type type_iteration

contains

    subroutine initialize_type_iteration(self, config, reference_values)
        implicit none
        class(type_iteration), intent(inout) :: self
        type(type_config_iteration), intent(in) :: config
        real(real64), intent(in), optional :: reference_values(:)

        self%total_iter = 0
        self%nonlinear_iter = 0
        self%converged(:) = .true.
        self%diverged(:) = .false.

        self%nonlinear_solver_type = config%nonlinear_solver_type
        self%compute_nonlinear_solver_type = self%nonlinear_solver_type

        if (self%nonlinear_solver_type == NONLINEAR_SOLVER%NONE) then
            return
        end if

        call self%settings%initialize(config%nonlinear, reference_values)

    end subroutine initialize_type_iteration

    subroutine reset_iteration(self)
        implicit none
        class(type_iteration), intent(inout) :: self

        integer(int32) :: i

        self%nonlinear_iter = 0
        self%converged(:) = .true.
        self%diverged(:) = .false.

        call self%settings%reset()

        ! do i = 1, PHYSICS_TYPES%NUM_ID
        !     call self%settings%convergence_control%residual(i)%reset()
        !     call self%settings%convergence_control%update(i)%reset()
        ! end do

        ! ! 初期ステップ周辺の戦略設定:
        ! ! 設定(nonlinear_solver_type)がNONEでない場合のみ，Picard法から開始する．
        ! ! 設定がNONEの場合は，計算用(compute)もNONEとする．
        ! if (self%nonlinear_solver_type /= NONLINEAR_SOLVER%NONE) then
        !     call self%set_nonlinear_solver(NONLINEAR_SOLVER%PICARD)
        ! else
        !     call self%set_nonlinear_solver(NONLINEAR_SOLVER%NONE)
        ! end if
    end subroutine reset_iteration

    !> Sets the ACTIVE (compute) solver type
    subroutine set_nonlinear_solver_iteration(self, nonlinear_solver_type)
        implicit none
        class(type_iteration), intent(inout) :: self
        type(type_constant_id), intent(in) :: nonlinear_solver_type

        ! 計算中に動的に切り替わるソルバータイプを設定する
        self%compute_nonlinear_solver_type = nonlinear_solver_type
    end subroutine set_nonlinear_solver_iteration

    subroutine set_converged_iteration(self, physics_type, converged)
        implicit none
        class(type_iteration), intent(inout) :: self
        type(type_constant_id), intent(in) :: physics_type
        logical, intent(in) :: converged

        if (.not. PHYSICS_TYPES%is_valid(physics_type)) then
            call raise_error(ERROR_CODES%INVALID_TYPE, opt=strip(physics_type%name))
        end if

        self%converged(physics_type%ID) = converged
    end subroutine set_converged_iteration

    subroutine set_diverged_iteration(self, physics_type, diverged)
        implicit none
        class(type_iteration), intent(inout) :: self
        type(type_constant_id), intent(in) :: physics_type
        logical, intent(in) :: diverged

        if (.not. PHYSICS_TYPES%is_valid(physics_type)) then
            call raise_error(ERROR_CODES%INVALID_TYPE, opt=strip(physics_type%name))
        end if

        self%diverged(physics_type%ID) = diverged
    end subroutine set_diverged_iteration

    pure function is_converged_iteration(self) result(is_converged)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: is_converged

        is_converged = all(self%converged)
    end function is_converged_iteration

    pure function is_diverged_iteration(self) result(is_diverged)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: is_diverged

        is_diverged = any(self%diverged)
    end function is_diverged_iteration

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

    !> Returns true if the ACTIVE solver is NONE
    pure function is_compute_none_method_iteration(self) result(is_none)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: is_none

        is_none = (self%compute_nonlinear_solver_type == NONLINEAR_SOLVER%NONE)
    end function is_compute_none_method_iteration

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
    pure function is_none_method_iteration(self) result(is_none)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: is_none

        is_none = (self%nonlinear_solver_type == NONLINEAR_SOLVER%NONE)
    end function is_none_method_iteration

end module control_iteration_manager

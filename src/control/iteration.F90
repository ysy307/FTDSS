module control_iteration
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_input, only:type_input, type_convergence
    use :: module_calculate, only:norm_2, norm_inf
    implicit none
    private

    public :: type_iteration

    ! --- プライベート: イテレータ設定格納型 ---
    type, private :: type_iterator_config
        integer(int32) :: max_iterations
        integer(int32) :: update_frequency
        type(type_convergence) :: convergence
    end type type_iterator_config

    ! --- パブリック: イテレーション管理派生型 ---
    type :: type_iteration
        private
        integer(int32) :: iter = 0 ! 非線形反復回数
        integer(int32) :: step = 0 ! ステップカウンタ
        logical :: is_converged = .false.

        real(real64) :: init_res_norm_l2 = 0.0d0
        real(real64) :: init_res_norm_inf = 0.0d0
        real(real64) :: init_upd_norm_l2 = 0.0d0
        real(real64) :: init_upd_norm_inf = 0.0d0

        character(:), allocatable :: algorithm
        type(type_iterator_config) :: config
    contains
        procedure, pass(self), public :: initialize => initialize_type_iteration
        procedure, pass(self), public :: reset_step
        procedure, pass(self), public :: set_initial_norms
        procedure, pass(self), public :: reset_timestep
        procedure, pass(self), public :: check_convergence
        procedure, pass(self), public :: increment_iter
        procedure, pass(self), public :: increment_step
        procedure, pass(self), public :: should_continue => continue_loop
        procedure, pass(self), public :: get_iter
        procedure, pass(self), public :: get_step
        procedure, pass(self), public :: get_algorithm_name
        procedure, pass(self), public :: has_converged => get_status
    end type type_iteration

contains

    subroutine initialize_type_iteration(self, input)
        implicit none
        class(type_iteration), intent(out) :: self
        type(type_input), intent(in) :: input

        self%iter = 0
        self%step = 0
        self%is_converged = .false.

        self%algorithm = input%basic%solver_settings%nonlinear_solver%method
        select case (trim(self%algorithm))
        case ("newton", "modified_newton", "picard")
            self%config%max_iterations = input%basic%solver_settings%nonlinear_solver%max_iterations
            self%config%update_frequency = input%basic%solver_settings%nonlinear_solver%update_frequency
            self%config%convergence = input%basic%solver_settings%nonlinear_solver%convergence
        end select
    end subroutine initialize_type_iteration

    subroutine reset_step(self)
        implicit none
        class(type_iteration), intent(inout) :: self

        self%step = 0
        self%is_converged = .false.
    end subroutine reset_step

    subroutine set_initial_norms(self, res_vec, upd_vec)
        implicit none
        class(type_iteration), intent(inout) :: self
        real(real64), intent(in), optional :: res_vec(:), upd_vec(:)

        ! 初期ノルム値設定
        if (present(res_vec)) then
            self%init_res_norm_l2 = norm_2(res_vec)
            self%init_res_norm_inf = norm_inf(res_vec)
        end if
        if (present(upd_vec)) then
            self%init_upd_norm_l2 = norm_2(upd_vec)
            self%init_upd_norm_inf = norm_inf(upd_vec)
        end if
    end subroutine set_initial_norms

    subroutine reset_timestep(self)
        implicit none
        class(type_iteration), intent(inout) :: self

        self%step = 0
        self%iter = 0
        self%is_converged = .false.
        self%init_res_norm_l2 = 0.0d0
        self%init_res_norm_inf = 0.0d0
        self%init_upd_norm_l2 = 0.0d0
        self%init_upd_norm_inf = 0.0d0
    end subroutine reset_timestep

    subroutine check_convergence(self, res_vec, upd_vec)
        implicit none
        class(type_iteration), intent(inout) :: self
        real(real64), intent(in) :: res_vec(:), upd_vec(:)
        logical :: ok_res, ok_upd
        real(real64) :: abs_res, rel_res, abs_upd, rel_upd

        if (trim(self%algorithm) == "none") then
            self%is_converged = .true.
            return
        end if

        ! --- ノルム計算 ---
        abs_res = norm_2(res_vec)
        abs_upd = norm_2(upd_vec)
        if (self%init_res_norm_l2 > 1.0d-12) then
            rel_res = abs_res / self%init_res_norm_l2
        else
            rel_res = 0.0d0
        end if
        if (self%init_upd_norm_l2 > 1.0d-12) then
            rel_upd = abs_upd / self%init_upd_norm_l2
        else
            rel_upd = 0.0d0
        end if

        ! --- 残差基準 ---
        select case (trim(self%config%convergence%residual%criteria))
        case ("absolute")
            ok_res = abs_res < self%config%convergence%residual%absolute_tolerance
        case ("relative")
            ok_res = rel_res < self%config%convergence%residual%relative_tolerance
        case ("both")
            if (trim(self%config%convergence%residual%logic) == "and") then
                ok_res = (abs_res < self%config%convergence%residual%absolute_tolerance) .and. &
                         (rel_res < self%config%convergence%residual%relative_tolerance)
            else
                ok_res = (abs_res < self%config%convergence%residual%absolute_tolerance) .or. &
                         (rel_res < self%config%convergence%residual%relative_tolerance)
            end if
        end select

        ! --- 更新基準 ---
        select case (trim(self%config%convergence%update%criteria))
        case ("absolute")
            ok_upd = abs_upd < self%config%convergence%update%absolute_tolerance
        case ("relative")
            ok_upd = rel_upd < self%config%convergence%update%relative_tolerance
        case ("both")
            if (trim(self%config%convergence%update%logic) == "and") then
                ok_upd = (abs_upd < self%config%convergence%update%absolute_tolerance) .and. &
                         (rel_upd < self%config%convergence%update%relative_tolerance)
            else
                ok_upd = (abs_upd < self%config%convergence%update%absolute_tolerance) .or. &
                         (rel_upd < self%config%convergence%update%relative_tolerance)
            end if
        end select

        ! --- 総合判定 ---
        select case (trim(self%config%convergence%use_criteria))
        case ("residual")
            self%is_converged = ok_res
        case ("update")
            self%is_converged = ok_upd
        case ("both")
            if (trim(self%config%convergence%use_logic) == "and") then
                self%is_converged = ok_res .and. ok_upd
            else
                self%is_converged = ok_res .or. ok_upd
            end if
        end select
    end subroutine check_convergence

    subroutine increment_iter(self)
        implicit none
        class(type_iteration), intent(inout) :: self

        self%iter = self%iter + 1
    end subroutine increment_iter

    subroutine increment_step(self)
        implicit none
        class(type_iteration), intent(inout) :: self

        self%step = self%step + 1
    end subroutine increment_step

    function continue_loop(self) result(should_continue_loop)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: should_continue_loop

        if (self%is_converged) then
            should_continue_loop = .false.
        else
            if (self%step == 0) then
                should_continue_loop = .true.
            else
                should_continue_loop = (self%step < self%config%max_iterations)
            end if
        end if
    end function continue_loop

    pure function get_iter(self) result(iter)
        implicit none
        class(type_iteration), intent(in) :: self
        integer(int32) :: iter

        iter = self%iter
    end function get_iter

    pure function get_step(self) result(step)
        implicit none
        class(type_iteration), intent(in) :: self
        integer(int32) :: step

        step = self%step
    end function get_step

    pure function get_status(self) result(is_converged)
        implicit none
        class(type_iteration), intent(in) :: self
        logical :: is_converged

        is_converged = self%is_converged
    end function get_status

    pure function get_algorithm_name(self) result(algorithm_name)
        implicit none
        class(type_iteration), intent(in) :: self
        character(:), allocatable :: algorithm_name

        algorithm_name = self%algorithm
    end function get_algorithm_name

end module control_iteration

module control_iteration
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_input, only:type_input
    use :: module_linalg, only:norm_2, norm_inf
    implicit none
    private

    public :: type_iteration

    type :: type_convergence_control
        character(:), allocatable :: norm_type
        character(:), allocatable :: combination_logic
        logical :: check_residual
        character(:), allocatable :: res_criteria
        real(real64) :: res_abs_tol = 1.0d-8
        real(real64) :: res_rel_tol = 1.0d-6
        logical :: check_update
        character(:), allocatable :: upd_criteria
        real(real64) :: upd_abs_tol = 1.0d-8
        real(real64) :: upd_rel_tol = 1.0d-6
    end type type_convergence_control

    type :: type_iterator_config
        integer(int32) :: max_iterations
        integer(int32) :: update_frequency
        type(type_convergence_control) :: conv_ctrl
    end type type_iterator_config

    type :: type_iteration
        private
        ! --- 全体管理 ---
        integer(int32) :: total_iter = 0 ! 全体計算での総イテレーション

        ! --- 非線形ステップごとの管理 ---
        integer(int32) :: nonlinear_iter = 0

        logical :: is_converged = .false.
        real(real64) :: init_res_norm_l2 = 0.0d0
        real(real64) :: init_res_norm_inf = 0.0d0
        real(real64) :: init_upd_norm_l2 = 0.0d0
        real(real64) :: init_upd_norm_inf = 0.0d0
        character(:), allocatable :: algorithm
        type(type_iterator_config) :: config
    contains
        procedure, pass(self), public :: initialize
        procedure, pass(self), public :: reset_nonlinear
        procedure, pass(self), public :: set_initial_norms
        procedure, pass(self), public :: check_convergence
        procedure, pass(self), public :: increment_nonlinear
        procedure, pass(self), public :: increment_total
        procedure, pass(self), public :: should_continue
        procedure, pass(self), public :: get_nonlinear_iter
        procedure, pass(self), public :: get_total_iter
        procedure, pass(self), public :: has_converged
        procedure, pass(self), public :: get_max_iterations
    end type type_iteration

    real(real64), private, parameter :: TINY_NORM = 1.0d-12

contains

    subroutine initialize(self, input)
        class(type_iteration), intent(out) :: self
        type(type_input), intent(in) :: input

        self%total_iter = 0
        self%nonlinear_iter = 0
        self%is_converged = .false.
        self%algorithm = input%basic%solver_settings%nonlinear_solver%method

        select case (trim(self%algorithm))
        case ("newton", "modified_newton", "picard")
            self%config%max_iterations = input%basic%solver_settings%nonlinear_solver%max_iterations
            self%config%update_frequency = input%basic%solver_settings%nonlinear_solver%update_frequency

            self%config%conv_ctrl%norm_type = input%basic%solver_settings%nonlinear_solver%convergence%norm_type
            self%config%conv_ctrl%combination_logic = input%basic%solver_settings%nonlinear_solver%convergence%use_logic

            select case (trim(input%basic%solver_settings%nonlinear_solver%convergence%use_criteria))
            case ("residual")
                self%config%conv_ctrl%check_residual = .true.
                self%config%conv_ctrl%check_update = .false.
            case ("update")
                self%config%conv_ctrl%check_residual = .false.
                self%config%conv_ctrl%check_update = .true.
            case ("both")
                self%config%conv_ctrl%check_residual = .true.
                self%config%conv_ctrl%check_update = .true.
            case default
                self%config%conv_ctrl%check_residual = .false.
                self%config%conv_ctrl%check_update = .false.
            end select

            if (self%config%conv_ctrl%check_residual) then
                self%config%conv_ctrl%res_criteria = input%basic%solver_settings%nonlinear_solver%convergence%residual%criteria
                self%config%conv_ctrl%res_abs_tol = input%basic%solver_settings%nonlinear_solver%convergence%residual%absolute_tolerance
                self%config%conv_ctrl%res_rel_tol = input%basic%solver_settings%nonlinear_solver%convergence%residual%relative_tolerance
            end if

            if (self%config%conv_ctrl%check_update) then
                self%config%conv_ctrl%upd_criteria = input%basic%solver_settings%nonlinear_solver%convergence%update%criteria
                self%config%conv_ctrl%upd_abs_tol = input%basic%solver_settings%nonlinear_solver%convergence%update%absolute_tolerance
                self%config%conv_ctrl%upd_rel_tol = input%basic%solver_settings%nonlinear_solver%convergence%update%relative_tolerance
            end if
        case default
            self%config%max_iterations = 1
        end select
    end subroutine initialize

    subroutine reset_nonlinear(self)
        class(type_iteration), intent(inout) :: self
        self%nonlinear_iter = 0
        self%is_converged = .false.
        self%init_res_norm_l2 = 0.0d0
        self%init_res_norm_inf = 0.0d0
        self%init_upd_norm_l2 = 0.0d0
        self%init_upd_norm_inf = 0.0d0
    end subroutine reset_nonlinear

    subroutine set_initial_norms(self, res_vec, upd_vec)
        class(type_iteration), intent(inout) :: self
        real(real64), intent(in), optional :: res_vec(:), upd_vec(:)
        if (present(res_vec)) then
            self%init_res_norm_l2 = norm_2(res_vec)
            self%init_res_norm_inf = norm_inf(res_vec)
        end if
        if (present(upd_vec)) then
            self%init_upd_norm_l2 = norm_2(upd_vec)
            self%init_upd_norm_inf = norm_inf(upd_vec)
        end if
    end subroutine set_initial_norms

    subroutine check_convergence(self, res_vec, upd_vec)
        class(type_iteration), intent(inout) :: self
        real(real64), intent(in) :: res_vec(:), upd_vec(:)
        logical :: is_res_ok = .false., is_upd_ok = .false.

        if (trim(self%algorithm) == "none") then
            self%is_converged = .true.
            return
        end if

        if (self%config%conv_ctrl%check_residual) then
            is_res_ok = check_single_criterion(self%config%conv_ctrl%norm_type, self%config%conv_ctrl%res_criteria, &
                                               self%config%conv_ctrl%res_abs_tol, self%config%conv_ctrl%res_rel_tol, res_vec, self%init_res_norm_l2, self%init_res_norm_inf)
        end if

        if (self%config%conv_ctrl%check_update) then
            is_upd_ok = check_single_criterion(self%config%conv_ctrl%norm_type, self%config%conv_ctrl%upd_criteria, &
                                               self%config%conv_ctrl%upd_abs_tol, self%config%conv_ctrl%upd_rel_tol, upd_vec, self%init_upd_norm_l2, self%init_upd_norm_inf)
        end if

        if (.not. self%config%conv_ctrl%check_residual .and. .not. self%config%conv_ctrl%check_update) then
            self%is_converged = .true.
        elseif (self%config%conv_ctrl%check_residual .and. .not. self%config%conv_ctrl%check_update) then
            self%is_converged = is_res_ok
        elseif (.not. self%config%conv_ctrl%check_residual .and. self%config%conv_ctrl%check_update) then
            self%is_converged = is_upd_ok
        else
            if (trim(self%config%conv_ctrl%combination_logic) == "and") then
                self%is_converged = is_res_ok .and. is_upd_ok
            else
                self%is_converged = is_res_ok .or. is_upd_ok
            end if
        end if
    end subroutine check_convergence

    function check_single_criterion(norm_type, criteria, abs_tol, rel_tol, &
                                    vec, init_norm_l2, init_norm_inf) result(is_ok)
        character(*), intent(in) :: norm_type, criteria
        real(real64), intent(in) :: abs_tol, rel_tol, vec(:), init_norm_l2, init_norm_inf
        logical :: is_ok
        real(real64) :: current_norm, init_norm, rel_val
        logical :: abs_ok, rel_ok

        if (trim(norm_type) == "inf") then
            current_norm = norm_inf(vec)
            init_norm = init_norm_inf
        else
            current_norm = norm_2(vec)
            init_norm = init_norm_l2
        end if

        if (init_norm > TINY_NORM) then
            rel_val = current_norm / init_norm
        else
            rel_val = 0.0d0
        end if

        abs_ok = current_norm < abs_tol
        rel_ok = rel_val < rel_tol

        select case (trim(criteria))
        case ("absolute")
            is_ok = abs_ok
        case ("relative")
            is_ok = rel_ok
        case ("both")
            is_ok = abs_ok .and. rel_ok
        case default
            is_ok = .false.
        end select
    end function check_single_criterion

    subroutine increment_nonlinear(self)
        class(type_iteration), intent(inout) :: self
        self%nonlinear_iter = self%nonlinear_iter + 1
    end subroutine increment_nonlinear

    subroutine increment_total(self)
        class(type_iteration), intent(inout) :: self
        self%total_iter = self%total_iter + 1
    end subroutine increment_total

    function should_continue(self) result(continue_flag)
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

end module control_iteration

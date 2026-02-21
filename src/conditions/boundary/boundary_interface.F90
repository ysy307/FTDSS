module conditions_boundary
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: stdlib_strings, only:to_string
    use :: module_core
    use :: module_input
    use :: module_control
    implicit none
    private

    ! --- Public Types ---
    public :: abst_bc
    public :: type_bc_dirichlet
    public :: type_bc_neumann
    public :: type_bc_robin
    public :: type_bc_zero_flux

    ! --- Public Constants ---
    integer(int32), public, parameter :: ERR_BC_UNKNOWN = 801
    integer(int32), public, parameter :: ERR_BC_INIT = 802

    ! 値配列のインデックス定義 (ManagerやSolverも使う可能性があるため公開)
    integer(int32), public, parameter :: IDX_BC_VAL = 1
    integer(int32), public, parameter :: IDX_BC_COEFF = 2

    ! ==========================================================================
    ! Abstract Base Class
    ! ==========================================================================
    type, abstract :: abst_bc
        integer(int32) :: boundary_id = -1
        type(type_state_bc) :: state
        integer(int32), private :: current_idx = 0
        logical :: initialized = .false.
    contains
        ! 初期化・破棄
        procedure, public, pass(self) :: initialize => initialize_bc
        procedure, public, pass(self) :: destroy => destroy_bc

        ! 内部計算 (Private)
        procedure, private, pass(self) :: calc_time_coefficient => calc_time_coefficient_bc
        procedure, private, pass(self) :: calc_values_raw => calc_values_raw_bc

        ! 公開アクセサ (Solverが呼ぶもの)
        ! 1. フラックス計算用 (Neumann / Robin) -> 残差とJacobianへの寄与を返す
        procedure, public, pass(self) :: get_flux_and_derivative => calc_flux_and_derivative_bc

        ! 2. 値固定用 (Dirichlet) -> 固定値と有効フラグを返す
        procedure, public, pass(self) :: get_dirichlet_value => calc_dirichlet_value_bc
    end type abst_bc

    interface
        ! --- Method Interfaces ---
        module subroutine initialize_bc(self, cell_id, state_bc)
            implicit none
            class(abst_bc), intent(inout) :: self
            integer(int32), intent(in) :: cell_id
            type(type_state_bc), intent(in) :: state_bc
        end subroutine initialize_bc

        module subroutine calc_time_coefficient_bc(self, current_time, coef, idx)
            implicit none
            class(abst_bc), intent(inout) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(inout) :: coef
            integer(int32), intent(inout) :: idx
        end subroutine calc_time_coefficient_bc

        module subroutine calc_values_raw_bc(self, current_time, out_values)
            implicit none
            class(abst_bc), intent(inout) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(inout) :: out_values(:)
        end subroutine calc_values_raw_bc

        module subroutine calc_flux_and_derivative_bc(self, current_time, u_curr, q_flux, dq_du)
            implicit none
            class(abst_bc), intent(inout) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(in) :: u_curr
            real(real64), intent(out) :: q_flux, dq_du
        end subroutine calc_flux_and_derivative_bc

        module subroutine calc_dirichlet_value_bc(self, current_time, val_fixed, is_active)
            implicit none
            class(abst_bc), intent(inout) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(out) :: val_fixed
            logical, intent(out) :: is_active
        end subroutine calc_dirichlet_value_bc

        module subroutine destroy_bc(self)
            implicit none
            class(abst_bc), intent(inout) :: self
        end subroutine destroy_bc
    end interface

    ! ==========================================================================
    ! Derived Classes
    ! ==========================================================================
    ! メンバ変数は全て親に移動したため、これらは型識別用のタグとして機能する

    type, extends(abst_bc) :: type_bc_dirichlet
    end type type_bc_dirichlet

    type, extends(abst_bc) :: type_bc_neumann
    end type type_bc_neumann

    type, extends(abst_bc) :: type_bc_robin
    end type type_bc_robin

    type, extends(abst_bc) :: type_bc_zero_flux
    end type type_bc_zero_flux

end module conditions_boundary

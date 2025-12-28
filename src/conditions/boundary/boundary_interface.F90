module conditions_boundary
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core
    use :: module_input
    use :: module_control
    implicit none
    private

    ! --- Public Types ---
    public :: abst_bc
    public :: type_bc_thermal_dirichlet
    public :: type_bc_thermal_adiabatic
    ! 将来用
    public :: type_bc_thermal_neumann
    public :: type_bc_thermal_robin

    ! --- Public Constants ---
    integer(int32), public, parameter :: ERR_BC_UNKNOWN = 801
    integer(int32), public, parameter :: ERR_BC_INIT = 802

    ! --- Public Interfaces ---
    public :: construct_type_bc_thermal_dirichlet
    public :: construct_type_bc_thermal_adiabatic
    ! public :: calculate_time_coefficient

    ! ==========================================================================
    ! Abstract Base Class
    ! ==========================================================================
    type, abstract :: abst_bc
        integer(int32), private :: boundary_id = -1
        integer(int32), private :: physics_type = -1
        real(real64), private, allocatable :: time_points(:)
        real(real64), private, allocatable :: values(:, :)
        integer(int32), private :: num_time_points = 0
        integer(int32), private :: num_variables = 0
        logical, private :: is_allocated = .false.
    contains
        ! procedure, public, pass(self) :: initizialize => initialize_bc
        procedure, private, pass(self) :: calc_time_coefficient => calc_time_coefficient_bc
        procedure, private, pass(self) :: calc_value_at_time => calc_value_at_time_bc
        procedure, public, pass(self) :: destroy => destroy_bc
        ! 必要に応じて共通メソッド定義
    end type abst_bc

    interface
        module subroutine calc_time_coefficient_bc(self, current_time, coef, idx)
            implicit none
            class(abst_bc), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(inout) :: coef
            integer(int32), intent(inout) :: idx

        end subroutine calc_time_coefficient_bc

        module subroutine calc_value_at_time_bc(self, current_time, values)
            implicit none
            class(abst_bc), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(inout) :: values(:)

        end subroutine calc_value_at_time_bc

        module subroutine destroy_bc(self)
            implicit none
            class(abst_bc), intent(inout) :: self

        end subroutine destroy_bc
    end interface

    ! ==========================================================================
    ! Derived Classes
    ! ==========================================================================
    ! --- Dirichlet (温度固定) ---
    type, extends(abst_bc) :: type_bc_thermal_dirichlet
        ! real(real64), allocatable :: time_points(:)
        ! real(real64), allocatable :: values(:)
    contains
        ! procedure, public :: get_value => get_dirichlet_value_at_time
    end type type_bc_thermal_dirichlet

    ! --- Adiabatic (断熱) ---
    type, extends(abst_bc) :: type_bc_thermal_adiabatic
    end type type_bc_thermal_adiabatic

    ! --- Neumann (熱流束固定) ---
    type, extends(abst_bc) :: type_bc_thermal_neumann
        ! real(real64), allocatable :: time_points(:)
        ! real(real64), allocatable :: values(:)
    end type type_bc_thermal_neumann

    ! --- Robin (熱伝達) ---
    type, extends(abst_bc) :: type_bc_thermal_robin
        real(real64) :: h_c
        real(real64) :: t_ref
    end type type_bc_thermal_robin

    ! ==========================================================================
    ! Interfaces for Submodules
    ! ==========================================================================
    interface
        ! コンストラクタ（各サブモジュールで実装）
        module function construct_type_bc_thermal_dirichlet(cell_id, input, controls) result(structure)
            integer(int32), intent(in) :: cell_id
            type(type_input), intent(in) :: input
            type(type_controls), intent(in) :: controls
            class(abst_bc), allocatable :: structure
        end function construct_type_bc_thermal_dirichlet

        module function construct_type_bc_thermal_adiabatic(cell_id, input, controls) result(structure)
            integer(int32), intent(in) :: cell_id
            type(type_input), intent(in) :: input
            type(type_controls), intent(in) :: controls
            class(abst_bc), allocatable :: structure
        end function construct_type_bc_thermal_adiabatic

        ! 共通ユーティリティ（boundary_baseで実装）

    end interface

end module conditions_boundary

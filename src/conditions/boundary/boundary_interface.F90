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
    public :: calculate_time_coefficient

    ! ==========================================================================
    ! Abstract Base Class
    ! ==========================================================================
    type, abstract :: abst_bc
        integer(int32) :: boundary_id = -1
    contains
        ! 必要に応じて共通メソッド定義
    end type abst_bc

    ! ==========================================================================
    ! Derived Classes
    ! ==========================================================================
    ! --- Dirichlet (温度固定) ---
    type, extends(abst_bc) :: type_bc_thermal_dirichlet
        real(real64), allocatable :: time_points(:)
        real(real64), allocatable :: values(:)
    contains
        procedure, public :: get_value => get_dirichlet_value_at_time
    end type type_bc_thermal_dirichlet

    ! --- Adiabatic (断熱) ---
    type, extends(abst_bc) :: type_bc_thermal_adiabatic
    end type type_bc_thermal_adiabatic

    ! --- Neumann (熱流束固定) ---
    type, extends(abst_bc) :: type_bc_thermal_neumann
        real(real64), allocatable :: time_points(:)
        real(real64), allocatable :: values(:)
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
        module subroutine calculate_time_coefficient(current_time, time_points, coef, idx)
            real(real64), intent(in) :: current_time
            real(real64), intent(in) :: time_points(:)
            real(real64), intent(inout) :: coef
            integer(int32), intent(inout) :: idx
        end subroutine calculate_time_coefficient
    end interface

contains

    ! 型に紐づく手続き（Type-bound procedures）はここに記述
    ! ※これらはインスタンスメソッドであり，サブモジュールには分離しにくい（Fortranの仕様）ためここに書きます．
    !   ただし，処理が複雑な場合は別途 helper 関数をサブモジュールに切り出してここで呼ぶ形にします．

    function get_dirichlet_value_at_time(self, current_time) result(val)
        implicit none
        class(type_bc_thermal_dirichlet), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64) :: val

        real(real64) :: coef
        integer(int32) :: idx

        idx = 1
        coef = 0.0d0

        if (allocated(self%time_points) .and. allocated(self%values)) then
            ! ここで呼ぶ calculate_time_coefficient は，interface経由で boundary_base の実装がリンクされる
            call calculate_time_coefficient(current_time, self%time_points, coef, idx)

            if (idx < size(self%values)) then
                val = self%values(idx) + coef * (self%values(idx + 1) - self%values(idx))
            else
                val = self%values(size(self%values))
            end if
        else
            val = 0.0d0
        end if
    end function get_dirichlet_value_at_time

end module conditions_boundary

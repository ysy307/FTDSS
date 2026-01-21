module conditions_boundary_manager
    use, intrinsic :: iso_fortran_env, only: int32
    use :: stdlib_strings, only:to_string
    use :: module_core
    use :: module_input
    use :: module_control
    use :: conditions_boundary ! 汎用BC型定義
    implicit none
    private

    public :: create_boundary_conditions

contains

    !> 境界条件オブジェクトのファクトリ関数
    function create_boundary_conditions(target_bc_id, cell_id, input, controls) result(bc)
        implicit none
        integer(int32), intent(in) :: target_bc_id ! Inputから読まれたBCタイプID
        integer(int32), intent(in) :: cell_id ! 適用する境界ID
        type(type_input), intent(in) :: input
        type(type_controls), intent(in) :: controls
        class(abst_bc), allocatable :: bc

        ! 1. IDに基づいて適切なクラスを割り当てる (Allocate)
        !    ※ 熱(Thermal)も水(Hydraulic)も、数学的性質が同じなら同じ型を使う
        select case (target_bc_id)

            ! --- Dirichlet (値固定) ---
        case (THERMAL_BC_DIRICHLET, &
              HYDRAULIC_BC_DIRICHLET)
            allocate (type_bc_dirichlet :: bc)

            ! --- Neumann / Flux (フラックス固定) ---
        case (THERMAL_BC_NEUMANN, THERMAL_BC_FLUX, &
              HYDRAULIC_BC_NEUMANN, HYDRAULIC_BC_FLUX, HYDRAULIC_BC_SEEPAGE)
            allocate (type_bc_neumann :: bc)

            ! --- Robin (伝達・対流・放射) ---
        case (THERMAL_BC_ROBIN, THERMAL_BC_CONVECTIVE, THERMAL_BC_RADIATION)
            allocate (type_bc_robin :: bc)

            ! --- Adiabatic / Impermeable (フラックスゼロ) ---
            ! ※ type_bc_adiabatic がある場合。なければ neumann で値0扱いでも可
        case (THERMAL_BC_ADIABATIC, &
              HYDRAULIC_BC_IMPERMEABLE)
            allocate (type_bc_adiabatic :: bc)

            ! --- Free / Open (自由流出など) ---
        case (THERMAL_BC_FREE)
            allocate (type_bc_neumann :: bc)
            ! ※ Free境界の実装によりますが、勾配0(Neumann)とみなす場合など

        case default
            call raise_error(ERROR_CODES%INVALID_BC_TYPE)
        end select

        ! 2. 共通初期化メソッドを呼ぶ
        !    ここでデータの読み込み、時間係数の計算準備などが行われる
        if (allocated(bc)) then
            ! target_bc_id を渡すことで、クラス内部で Thermal/Hydraulic の区別や
            ! 必要な変数成分数(1 or 2)を判断させる
            call bc%initialize(cell_id, target_bc_id, input, controls)

            ! 初期化に失敗していれば破棄する安全策を入れる場合
            ! if (.not. bc%initialized) deallocate(bc)
        end if

    end function create_boundary_conditions

end module conditions_boundary_manager

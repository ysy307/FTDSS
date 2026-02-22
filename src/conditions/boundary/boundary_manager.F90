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
    function create_boundary_conditions(config_bc) result(bc)
        implicit none
        type(type_config_bc), intent(in) :: config_bc
        class(abst_bc), allocatable :: bc

        ! 1. IDに基づいて適切なクラスを割り当てる (Allocate)
        !    ※ 熱(Thermal)も水(Hydraulic)も、数学的性質が同じなら同じ型を使う
        if (config_bc%bc_kind == THERMAL_BC_TYPES%DIRICHLET .or. &
            config_bc%bc_kind == HYDRAULIC_BC_TYPES%DIRICHLET) then
            allocate (type_bc_dirichlet :: bc)
        else if (config_bc%bc_kind == THERMAL_BC_TYPES%NEUMANN .or. &
                 config_bc%bc_kind == THERMAL_BC_TYPES%FLUX .or. &
                 config_bc%bc_kind == HYDRAULIC_BC_TYPES%NEUMANN .or. &
                 config_bc%bc_kind == HYDRAULIC_BC_TYPES%FLUX .or. &
                 config_bc%bc_kind == HYDRAULIC_BC_TYPES%SEEPAGE) then
            allocate (type_bc_neumann :: bc)
        else if (config_bc%bc_kind == THERMAL_BC_TYPES%ROBIN .or. &
                 config_bc%bc_kind == THERMAL_BC_TYPES%CONVECTIVE .or. &
                 config_bc%bc_kind == THERMAL_BC_TYPES%RADIATION) then
            allocate (type_bc_robin :: bc)
        else if (config_bc%bc_kind == THERMAL_BC_TYPES%ADIABATIC .or. &
                 config_bc%bc_kind == HYDRAULIC_BC_TYPES%IMPERMEABLE) then
            allocate (type_bc_zero_flux :: bc)
        else if (config_bc%bc_kind == THERMAL_BC_TYPES%FREE) then
            allocate (type_bc_neumann :: bc)
        else
            call raise_error(ERROR_CODES%INVALID_BC_TYPE)
        end if

        ! 2. 共通初期化メソッドを呼ぶ
        !    ここでデータの読み込み、時間係数の計算準備などが行われる
        if (allocated(bc)) then
            call bc%initialize(config_bc)

            ! 初期化に失敗していれば破棄する安全策を入れる場合
            ! if (.not. bc%initialized) deallocate(bc)
        end if

    end function create_boundary_conditions

end module conditions_boundary_manager

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
    function create_boundary_conditions(cell_id, state_bc) result(bc)
        ! function create_boundary_conditions(target_bc_id, cell_id, input, controls) result(bc)
        implicit none
        integer(int32), intent(in) :: cell_id ! 適用する境界ID
        type(type_state_bc), intent(in) :: state_bc
        ! integer(int32), intent(in) :: target_bc_id ! Inputから読まれたBCタイプID
        ! type(type_input), intent(in) :: input
        ! type(type_controls), intent(in) :: controls
        class(abst_bc), allocatable :: bc

        ! 1. IDに基づいて適切なクラスを割り当てる (Allocate)
        !    ※ 熱(Thermal)も水(Hydraulic)も、数学的性質が同じなら同じ型を使う
        if (state_bc%bc_kind == THERMAL_BC_TYPES%DIRICHLET .or. &
            state_bc%bc_kind == HYDRAULIC_BC_TYPES%DIRICHLET) then
            allocate (type_bc_dirichlet :: bc)
        else if (state_bc%bc_kind == THERMAL_BC_TYPES%NEUMANN .or. &
                 state_bc%bc_kind == THERMAL_BC_TYPES%FLUX .or. &
                 state_bc%bc_kind == HYDRAULIC_BC_TYPES%NEUMANN .or. &
                 state_bc%bc_kind == HYDRAULIC_BC_TYPES%FLUX .or. &
                 state_bc%bc_kind == HYDRAULIC_BC_TYPES%SEEPAGE) then
            allocate (type_bc_neumann :: bc)
        else if (state_bc%bc_kind == THERMAL_BC_TYPES%ROBIN .or. &
                 state_bc%bc_kind == THERMAL_BC_TYPES%CONVECTIVE .or. &
                 state_bc%bc_kind == THERMAL_BC_TYPES%RADIATION) then
            allocate (type_bc_robin :: bc)
        else if (state_bc%bc_kind == THERMAL_BC_TYPES%ADIABATIC .or. &
                 state_bc%bc_kind == HYDRAULIC_BC_TYPES%IMPERMEABLE) then
            allocate (type_bc_zero_flux :: bc)
        else if (state_bc%bc_kind == THERMAL_BC_TYPES%FREE) then
            allocate (type_bc_neumann :: bc)
        else
            call raise_error(ERROR_CODES%INVALID_BC_TYPE)
        end if

        ! 2. 共通初期化メソッドを呼ぶ
        !    ここでデータの読み込み、時間係数の計算準備などが行われる
        if (allocated(bc)) then
            call bc%initialize(cell_id, state_bc)

            ! 初期化に失敗していれば破棄する安全策を入れる場合
            ! if (.not. bc%initialized) deallocate(bc)
        end if

    end function create_boundary_conditions

end module conditions_boundary_manager

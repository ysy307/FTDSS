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

        ! ! 1. IDに基づいて適切なクラスを割り当てる (Allocate)
        ! !    ※ 熱(Thermal)も水(Hydraulic)も、数学的性質が同じなら同じ型を使う
        ! select case (config_bc%bc_kind%ID)
        ! case (THERMAL_BC_TYPES%DIRICHLET%ID, HYDRAULIC_BC_TYPES%DIRICHLET%ID)
        !     allocate (type_bc_dirichlet :: bc)
        ! case (THERMAL_BC_TYPES%NEUMANN%ID, THERMAL_BC_TYPES%FLUX%ID, HYDRAULIC_BC_TYPES%NEUMANN%ID, &
        !       HYDRAULIC_BC_TYPES%FLUX%ID, HYDRAULIC_BC_TYPES%SEEPAGE%ID)
        !     allocate (type_bc_neumann :: bc)
        ! case (THERMAL_BC_TYPES%ROBIN%ID, THERMAL_BC_TYPES%CONVECTIVE%ID, THERMAL_BC_TYPES%RADIATION%ID)
        !     allocate (type_bc_robin :: bc)
        ! case (THERMAL_BC_TYPES%ADIABATIC%ID, HYDRAULIC_BC_TYPES%IMPERMEABLE%ID)
        !     allocate (type_bc_zero_flux :: bc)
        ! case (THERMAL_BC_TYPES%FREE%ID)
        !     allocate (type_bc_neumann :: bc)
        ! case default
        !     call raise_error(ERROR_CODES%INVALID_BC_TYPE)
        ! end select

        ! ! 2. 共通初期化メソッドを呼ぶ
        ! !    ここでデータの読み込み、時間係数の計算準備などが行われる
        ! if (allocated(bc)) then
        !     call bc%initialize(config_bc)

        !     ! 初期化に失敗していれば破棄する安全策を入れる場合
        !     ! if (.not. bc%initialized) deallocate(bc)
        ! end if

    end function create_boundary_conditions

end module conditions_boundary_manager

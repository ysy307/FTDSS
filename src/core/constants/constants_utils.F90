module core_constants_utils
    use :: iso_fortran_env, only:int32
    use :: stdlib_strings, only:strip
    use :: stdlib_ascii, only:to_lower
    use :: core_constants
    use :: core_parallel_constants
    implicit none
    private

    public :: get_matrix_type
    public :: get_coupling_mode
    public :: get_physics_type
    public :: get_dof_type
    public :: get_thermal_bc_type
    public :: get_hydraulic_bc_type
    public :: get_initial_condition_type

contains

    !> 文字列から行列タイプIDを取得する
    pure function get_matrix_type(key) result(val)
        implicit none
        character(len=*), intent(in) :: key
        integer(int32) :: val

        select case (strip(to_lower(key)))
        case ("dense")
            val = MATRIX_DENSE
        case ("crs", "csr")
            val = MATRIX_CRS
        case ("coo")
            val = MATRIX_COO
        end select
    end function get_matrix_type

    !> 文字列からカップリングモードIDを取得する
    pure function get_coupling_mode(key) result(val)
        implicit none
        character(len=*), intent(in) :: key
        integer(int32) :: val

        select case (strip(to_lower(key)))
        case ("weak")
            val = COUPLING_MODE_STAGGERED
        case ("strong")
            val = COUPLING_MODE_MONOLITHIC
        end select
    end function get_coupling_mode

    !> 文字列から物理タイプIDを取得する
    pure function get_physics_type(key) result(val)
        implicit none
        character(len=*), intent(in) :: key
        integer(int32) :: val

        select case (strip(to_lower(key)))
        case ("thermal")
            val = PHYSICS_TYPE_THERMAL
        case ("hydraulic")
            val = PHYSICS_TYPE_HYDRAULIC
        case ("mechanical")
            val = PHYSICS_TYPE_MECHANICAL
        end select
    end function get_physics_type

    !> 文字列から自由度タイプIDを取得する
    pure function get_dof_type(key) result(val)
        implicit none
        character(len=*), intent(in) :: key
        integer(int32) :: val

        select case (strip(to_lower(key)))
        case ("t", "temperature")
            val = DOF_TYPE_T
        case ("p", "pressure")
            val = DOF_TYPE_P
        case ("dx", "displacement_x", "ux")
            val = DOF_TYPE_DX
        case ("dy", "displacement_y", "uy")
            val = DOF_TYPE_DY
        case ("dz", "displacement_z", "uz")
            val = DOF_TYPE_DZ
        end select
    end function get_dof_type

    !> 文字列から熱解析の境界条件タイプIDを取得する
    pure function get_thermal_bc_type(key) result(val)
        implicit none
        character(len=*), intent(in) :: key
        integer(int32) :: val

        select case (strip(to_lower(key)))
        case ("dirichlet")
            val = THERMAL_BC_DIRICHLET
        case ("neumann")
            val = THERMAL_BC_NEUMANN
        case ("flux")
            val = THERMAL_BC_FLUX
        case ("robin")
            val = THERMAL_BC_ROBIN
        case ("convective")
            val = THERMAL_BC_CONVECTIVE
        case ("radiation")
            val = THERMAL_BC_RADIATION
        case ("adiabatic")
            val = THERMAL_BC_ADIABATIC
        case ("free")
            val = THERMAL_BC_FREE
        end select
    end function get_thermal_bc_type

    !> 文字列から水理解析の境界条件タイプIDを取得する
    pure function get_hydraulic_bc_type(key) result(val)
        implicit none
        character(len=*), intent(in) :: key
        integer(int32) :: val

        select case (strip(to_lower(key)))
        case ("dirichlet")
            val = HYDRAULIC_BC_DIRICHLET
        case ("neumann")
            val = HYDRAULIC_BC_NEUMANN
        case ("flux")
            val = HYDRAULIC_BC_FLUX
        case ("impermeable")
            val = HYDRAULIC_BC_IMPERMEABLE
        case ("seepage")
            val = HYDRAULIC_BC_SEEPAGE
        end select
    end function get_hydraulic_bc_type

    !> 文字列から初期条件タイプIDを取得する
    pure function get_initial_condition_type(key) result(val)
        implicit none
        character(len=*), intent(in) :: key
        integer(int32) :: val

        select case (strip(to_lower(key)))
        case ("thermal")
            val = INITIAL_CONDITION_THERMAL
        case ("hydraulic")
            val = INITIAL_CONDITION_HYDRAULIC
        case ("mechanical")
            val = INITIAL_CONDITION_MECHANICAL
        case ("porosity")
            val = INITIAL_CONDITION_POROSITY
        end select
    end function get_initial_condition_type
end module core_constants_utils

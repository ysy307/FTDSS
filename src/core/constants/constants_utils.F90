module core_constants_utils
    use :: iso_fortran_env, only:int32
    use :: stdlib_strings, only:strip
    use :: stdlib_ascii, only:to_lower
    use :: core_constants_solver
    use :: core_constants_parallel
    use :: core_constants_physical
    implicit none
    private

    public :: get_value
    public :: get_matrix_type
    public :: get_coupling_mode
    public :: get_physics_type
    public :: get_dof_type
    public :: get_thermal_bc_type
    public :: get_hydraulic_bc_type
    public :: get_initial_condition_type
    public :: get_initial_condition_physical_type
    public :: get_norm_type
    public :: get_nonlinear_solver_type
    public :: get_nonlinear_norm_criteria
    public :: get_nonlinear_criteria
    public :: get_nonlinear_logic
    public :: get_time_unit
    public :: get_time_record
    public :: get_swcc_model_type
    public :: get_physics_unit

    ! String getter functions
    public :: get_string
    public :: get_matrix_type_string
    public :: get_coupling_mode_string
    public :: get_physics_type_string
    public :: get_dof_type_string
    public :: get_thermal_bc_type_string
    public :: get_hydraulic_bc_type_string
    public :: get_initial_condition_type_string
    public :: get_initial_condition_physical_type_string
    public :: get_norm_type_string
    public :: get_nonlinear_solver_type_string
    public :: get_nonlinear_norm_criteria_string
    public :: get_nonlinear_criteria_string
    public :: get_nonlinear_logic_string
    public :: get_time_unit_string
    public :: get_time_record_string
    public :: get_swcc_model_type_string

    public :: get_physics_unit_string

    interface
        pure function get_value(key) result(val)
            import :: int32
            implicit none
            character(len=*), intent(in) :: key
            integer(int32) :: val
        end function get_value

        pure function get_string(val) result(key)
            import :: int32
            implicit none
            integer(int32), intent(in) :: val
            character(:), allocatable :: key
        end function get_string
    end interface

contains

    !> 文字列から行列タイプIDを取得する
    pure function get_matrix_type(key) result(val)
        implicit none
        character(len=*), intent(in) :: key
        integer(int32) :: val

        select case (strip(to_lower(key)))
        case ("dense")
            val = MATRIX_DENSE
        case ("csr")
            val = MATRIX_CSR
        case ("coo")
            val = MATRIX_COO
        case ("bsr")
            val = MATRIX_BSR
        end select
    end function get_matrix_type

    pure function get_matrix_type_string(val) result(key)
        implicit none
        integer(int32), intent(in) :: val
        character(:), allocatable :: key

        select case (val)
        case (MATRIX_DENSE)
            key = "Dense Matrix"
        case (MATRIX_CSR)
            key = "CSR Matrix"
        case (MATRIX_COO)
            key = "COO Matrix"
        case (MATRIX_BSR)
            key = "BSR Matrix"
        case default
            key = "Unknown"
        end select
    end function get_matrix_type_string

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

    pure function get_coupling_mode_string(val) result(key)
        implicit none
        integer(int32), intent(in) :: val
        character(:), allocatable :: key

        select case (val)
        case (COUPLING_MODE_STAGGERED)
            key = "Weak coupling"
        case (COUPLING_MODE_MONOLITHIC)
            key = "Strong coupling"
        case default
            key = "Unknown"
        end select
    end function get_coupling_mode_string

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

    pure function get_physics_type_string(val) result(key)
        implicit none
        integer(int32), intent(in) :: val
        character(:), allocatable :: key

        select case (val)
        case (PHYSICS_TYPE_THERMAL)
            key = "Thermal"
        case (PHYSICS_TYPE_HYDRAULIC)
            key = "Hydraulic"
        case (PHYSICS_TYPE_MECHANICAL)
            key = "Mechanical"
        case default
            key = "Unknown"
        end select
    end function get_physics_type_string

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

    pure function get_dof_type_string(val) result(key)
        implicit none
        integer(int32), intent(in) :: val
        character(:), allocatable :: key

        select case (val)
        case (DOF_TYPE_T)
            key = "Temperature"
        case (DOF_TYPE_P)
            key = "Pressure"
        case (DOF_TYPE_DX)
            key = "Displacement X"
        case (DOF_TYPE_DY)
            key = "Displacement Y"
        case (DOF_TYPE_DZ)
            key = "Displacement Z"
        case default
            key = "Unknown"
        end select
    end function get_dof_type_string

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

    pure function get_thermal_bc_type_string(val) result(key)
        implicit none
        integer(int32), intent(in) :: val
        character(:), allocatable :: key

        select case (val)
        case (THERMAL_BC_DIRICHLET)
            key = "Dirichlet"
        case (THERMAL_BC_NEUMANN)
            key = "Neumann"
        case (THERMAL_BC_FLUX)
            key = "Flux"
        case (THERMAL_BC_ROBIN)
            key = "Robin"
        case (THERMAL_BC_CONVECTIVE)
            key = "Convective"
        case (THERMAL_BC_RADIATION)
            key = "Radiation"
        case (THERMAL_BC_ADIABATIC)
            key = "Adiabatic"
        case (THERMAL_BC_FREE)
            key = "Free"
        case default
            key = "Unknown"
        end select
    end function get_thermal_bc_type_string

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

    pure function get_hydraulic_bc_type_string(val) result(key)
        implicit none
        integer(int32), intent(in) :: val
        character(:), allocatable :: key

        select case (val)
        case (HYDRAULIC_BC_DIRICHLET)
            key = "Dirichlet"
        case (HYDRAULIC_BC_NEUMANN)
            key = "Neumann"
        case (HYDRAULIC_BC_FLUX)
            key = "Flux"
        case (HYDRAULIC_BC_IMPERMEABLE)
            key = "Impermeable"
        case (HYDRAULIC_BC_SEEPAGE)
            key = "Seepage"
        case default
            key = "Unknown"
        end select
    end function get_hydraulic_bc_type_string

    !> 文字列から初期条件タイプIDを取得する
    pure function get_initial_condition_physical_type(key) result(val)
        implicit none
        character(len=*), intent(in) :: key
        integer(int32) :: val

        select case (strip(to_lower(key)))
        case ("thermal")
            val = IC_TARGET_THERMAL
        case ("hydraulic")
            val = IC_TARGET_HYDRAULIC
        case ("mechanical")
            val = IC_TARGET_MECHANICAL
        case ("porosity")
            val = IC_TARGET_POROSITY
        end select
    end function get_initial_condition_physical_type

    pure function get_initial_condition_physical_type_string(val) result(key)
        implicit none
        integer(int32), intent(in) :: val
        character(:), allocatable :: key

        select case (val)
        case (IC_TARGET_THERMAL)
            key = "Thermal"
        case (IC_TARGET_HYDRAULIC)
            key = "Hydraulic"
        case (IC_TARGET_MECHANICAL)
            key = "Mechanical"
        case (IC_TARGET_POROSITY)
            key = "Porosity"
        case default
            key = "Unknown"
        end select
    end function get_initial_condition_physical_type_string

    !> 文字列から初期条件タイプIDを取得する
    pure function get_initial_condition_type(key) result(val)
        implicit none
        character(len=*), intent(in) :: key
        integer(int32) :: val

        select case (strip(to_lower(key)))
        case ("uniform")
            val = IC_METHOD_UNIFORM
        case ("laplace")
            val = IC_METHOD_LAPLACE
        case ("file")
            val = IC_METHOD_FROM_FILE
        end select
    end function get_initial_condition_type

    pure function get_initial_condition_type_string(val) result(key)
        implicit none
        integer(int32), intent(in) :: val
        character(:), allocatable :: key

        select case (val)
        case (IC_METHOD_UNIFORM)
            key = "Uniform"
        case (IC_METHOD_LAPLACE)
            key = "Laplace"
        case (IC_METHOD_FROM_FILE)
            key = "File"
        case default
            key = "Unknown"
        end select
    end function get_initial_condition_type_string

    !> 文字列からノルムタイプIDを取得する
    pure function get_norm_type(key) result(val)
        implicit none
        character(len=*), intent(in) :: key
        integer(int32) :: val

        select case (strip(to_lower(key)))
        case ("l2")
            val = NORM_TYPE_L2
        case ("linf")
            val = NORM_TYPE_LINF
        end select
    end function get_norm_type

    pure function get_norm_type_string(val) result(key)
        implicit none
        integer(int32), intent(in) :: val
        character(:), allocatable :: key

        select case (val)
        case (NORM_TYPE_L2)
            key = "L2 Norm"
        case (NORM_TYPE_LINF)
            key = "L-Infinity Norm"
        case default
            key = "Unknown"
        end select
    end function get_norm_type_string

    pure function get_nonlinear_solver_type(key) result(val)
        implicit none
        character(len=*), intent(in) :: key
        integer(int32) :: val

        select case (strip(to_lower(key)))
        case ("none")
            val = NONLINEAR_SOLVER_NONE
        case ("newton")
            val = NONLINEAR_SOLVER_NEWTON
        case ("modified_newton")
            val = NONLINEAR_SOLVER_MODIFIED_NEWTON
        case ("picard")
            val = NONLINEAR_SOLVER_PICARD
        end select
    end function get_nonlinear_solver_type

    pure function get_nonlinear_solver_type_string(val) result(key)
        implicit none
        integer(int32), intent(in) :: val
        character(:), allocatable :: key

        select case (val)
        case (NONLINEAR_SOLVER_NONE)
            key = "None"
        case (NONLINEAR_SOLVER_NEWTON)
            key = "Newton"
        case (NONLINEAR_SOLVER_MODIFIED_NEWTON)
            key = "Modified Newton"
        case (NONLINEAR_SOLVER_PICARD)
            key = "Picard"
        case default
            key = "Unknown"
        end select
    end function get_nonlinear_solver_type_string

    pure function get_nonlinear_norm_criteria(key) result(val)
        implicit none
        character(len=*), intent(in) :: key
        integer(int32) :: val

        select case (strip(to_lower(key)))
        case ("none")
            val = NONLINEAR_NORM_CRITERIA_NONE
        case ("residual")
            val = NONLINEAR_NORM_CRITERIA_RESIDUAL
        case ("update")
            val = NONLINEAR_NORM_CRITERIA_UPDATE
        case ("both")
            val = NONLINEAR_NORM_CRITERIA_BOTH
        end select
    end function get_nonlinear_norm_criteria

    pure function get_nonlinear_norm_criteria_string(val) result(key)
        implicit none
        integer(int32), intent(in) :: val
        character(:), allocatable :: key

        select case (val)
        case (NONLINEAR_NORM_CRITERIA_NONE)
            key = "None"
        case (NONLINEAR_NORM_CRITERIA_RESIDUAL)
            key = "Residual"
        case (NONLINEAR_NORM_CRITERIA_UPDATE)
            key = "Update"
        case (NONLINEAR_NORM_CRITERIA_BOTH)
            key = "Both"
        case default
            key = "Unknown"
        end select
    end function get_nonlinear_norm_criteria_string

    pure function get_nonlinear_criteria(key) result(val)
        implicit none
        character(len=*), intent(in) :: key
        integer(int32) :: val

        select case (strip(to_lower(key)))
        case ("none")
            val = NONLINEAR_CRITERIA_NONE
        case ("absolute")
            val = NONLINEAR_CRITERIA_ABSOLUTE
        case ("relative")
            val = NONLINEAR_CRITERIA_RELATIVE
        case ("both")
            val = NONLINEAR_CRITERIA_BOTH
        end select
    end function get_nonlinear_criteria

    pure function get_nonlinear_criteria_string(val) result(key)
        implicit none
        integer(int32), intent(in) :: val
        character(:), allocatable :: key

        select case (val)
        case (NONLINEAR_CRITERIA_NONE)
            key = "None"
        case (NONLINEAR_CRITERIA_ABSOLUTE)
            key = "Absolute"
        case (NONLINEAR_CRITERIA_RELATIVE)
            key = "Relative"
        case (NONLINEAR_CRITERIA_BOTH)
            key = "Both"
        case default
            key = "Unknown"
        end select
    end function get_nonlinear_criteria_string

    pure function get_nonlinear_logic(key) result(val)
        implicit none
        character(len=*), intent(in) :: key
        integer(int32) :: val

        select case (strip(to_lower(key)))
        case ("or")
            val = NONLINEAR_LOGIC_OR
        case ("and")
            val = NONLINEAR_LOGIC_AND
        end select
    end function get_nonlinear_logic

    pure function get_nonlinear_logic_string(val) result(key)
        implicit none
        integer(int32), intent(in) :: val
        character(:), allocatable :: key

        select case (val)
        case (NONLINEAR_LOGIC_OR)
            key = "OR"
        case (NONLINEAR_LOGIC_AND)
            key = "AND"
        case default
            key = "Unknown"
        end select
    end function get_nonlinear_logic_string

    pure function get_time_unit(key) result(val)
        implicit none
        character(len=*), intent(in) :: key
        integer(int32) :: val

        select case (strip(to_lower(key)))
        case ("second")
            val = TIME_UNIT_SECONDS
        case ("minute")
            val = TIME_UNIT_MINUTES
        case ("hour")
            val = TIME_UNIT_HOURS
        case ("day")
            val = TIME_UNIT_DAYS
        case ("year")
            val = TIME_UNIT_YEARS
        case default
            val = -1
        end select
    end function get_time_unit

    pure function get_time_unit_string(val) result(key)
        implicit none
        integer(int32), intent(in) :: val
        character(:), allocatable :: key

        select case (val)
        case (TIME_UNIT_SECONDS)
            key = "Seconds"
        case (TIME_UNIT_MINUTES)
            key = "Minutes"
        case (TIME_UNIT_HOURS)
            key = "Hours"
        case (TIME_UNIT_DAYS)
            key = "Days"
        case (TIME_UNIT_YEARS)
            key = "Years"
        case default
            key = "Unknown"
        end select
    end function get_time_unit_string

    pure function get_time_record(key) result(val)
        implicit none
        character(len=*), intent(in) :: key
        integer(int32) :: val

        select case (strip(to_lower(key)))
        case ("start")
            val = TIME_RECORD_START
        case ("end")
            val = TIME_RECORD_END
        end select
    end function get_time_record

    pure function get_time_record_string(val) result(key)
        implicit none
        integer(int32), intent(in) :: val
        character(:), allocatable :: key

        select case (val)
        case (TIME_RECORD_START)
            key = "Start"
        case (TIME_RECORD_END)
            key = "End"
        case default
            key = "Unknown"
        end select
    end function get_time_record_string

    !> 文字列からSWCCモデルタイプIDを取得する
    pure function get_swcc_model_type(key) result(val)
        implicit none
        character(len=*), intent(in) :: key
        integer(int32) :: val

        val = -1
        select case (strip(to_lower(key)))
        case ("brooks_corey", "bc")
            val = SWCC_BC
        case ("van_genuchten", "vg")
            val = SWCC_VG
        case ("kosugi", "ko")
            val = SWCC_KO
        case ("modified_van_genuchten", "mvg")
            val = SWCC_MVG
        case ("durner")
            val = SWCC_DURNER
        case ("dvgch")
            val = SWCC_DVGCH
        end select
    end function get_swcc_model_type

    pure function get_swcc_model_type_string(val) result(key)
        implicit none
        integer(int32), intent(in) :: val
        character(:), allocatable :: key

        select case (val)
        case (SWCC_BC)
            key = "Brooks-Corey"
        case (SWCC_VG)
            key = "van-Genuchten"
        case (SWCC_KO)
            key = "Kosugi"
        case (SWCC_MVG)
            key = "Modified van-Genuchten"
        case (SWCC_DURNER)
            key = "Durner"
        case (SWCC_DVGCH)
            key = "DVGCH"
        case default
            key = "Unknown"
        end select
    end function get_swcc_model_type_string

    !> Physics unit in systems
    pure function get_physics_unit(key) result(val)
        implicit none
        character(len=*), intent(in) :: key
        integer(int32) :: val

        select case (strip(to_lower(key)))
        case ("m")
            val = PHYSICS_UNIT_M
        case ("cm")
            val = PHYSICS_UNIT_CM
        case ("pa")
            val = PHYSICS_UNIT_PA
        end select
    end function get_physics_unit

    pure function get_physics_unit_string(val) result(key)
        implicit none
        integer(int32), intent(in) :: val
        character(:), allocatable :: key

        select case (val)
        case (PHYSICS_UNIT_M)
            key = "m"
        case (PHYSICS_UNIT_CM)
            key = "cm"
        case (PHYSICS_UNIT_PA)
            key = "Pa"
        case default
            key = "unknown"
        end select
    end function get_physics_unit_string

end module core_constants_utils

module core_constants
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    private

    ! --- Matrix Types ---
    !> Dense matrix storage format
    integer(int32), parameter, public :: MATRIX_DENSE = 1 !&
    !> Compressed Row Storage format
    integer(int32), parameter, public :: MATRIX_CRS   = 2 !&
    !> Coordinate list format
    integer(int32), parameter, public :: MATRIX_COO   = 3 !&

    ! --- Coupling modes ---
    !> staggered coupling
    integer(int32), parameter, public :: COUPLING_MODE_STAGGERED = 1 !&
    !> monolithic coupling
    integer(int32), parameter, public :: COUPLING_MODE_MONOLITHIC = 2 !&

    ! --- Physics Types ---
    !> Thermal analysis
    integer(int32), parameter, public :: PHYSICS_TYPE_THERMAL    = 1 !&
    !> Hydraulic analysis
    integer(int32), parameter, public :: PHYSICS_TYPE_HYDRAULIC  = 2 !&
    !> Mechanical analysis
    integer(int32), parameter, public :: PHYSICS_TYPE_MECHANICAL = 3 !&

    !> Total number of supported physics types
    integer(int32), parameter, public :: NUM_PHYSICS_TYPES = 3

    ! --- DOF Types ---
    !> Temperature DOF
    integer(int32), parameter, public :: DOF_TYPE_T = 1
    !> Pressure DOF
    integer(int32), parameter, public :: DOF_TYPE_P = 2
    !> Displacement X-DOF
    integer(int32), parameter, public :: DOF_TYPE_DX = 3
    !> Displacement Y-DOF
    integer(int32), parameter, public :: DOF_TYPE_DY = 4
    !> Displacement Z-DOF
    integer(int32), parameter, public :: DOF_TYPE_DZ = 5

    ! Definition of boundary condition type IDs for thermal analysis
    integer(int32), parameter, public :: NUM_THERMAL_BC_TYPES  = 8 !&
    integer(int32), parameter, public :: THERMAL_BC_DIRICHLET  = 101 !&
    integer(int32), parameter, public :: THERMAL_BC_NEUMANN    = 102 !&
    integer(int32), parameter, public :: THERMAL_BC_FLUX       = 103 !&
    integer(int32), parameter, public :: THERMAL_BC_ROBIN      = 104 !&
    integer(int32), parameter, public :: THERMAL_BC_CONVECTIVE = 105 !&
    integer(int32), parameter, public :: THERMAL_BC_RADIATION  = 106 !&
    integer(int32), parameter, public :: THERMAL_BC_ADIABATIC  = 107 !&
    integer(int32), parameter, public :: THERMAL_BC_FREE       = 108 !&

    ! -----------------------------
    ! Thermal BC application sequence
    ! -----------------------------
    integer(int32), parameter, public :: THERMAL_BC_SEQUENCE(NUM_THERMAL_BC_TYPES) = [ &
                                         THERMAL_BC_ROBIN, &
                                         THERMAL_BC_CONVECTIVE, &
                                         THERMAL_BC_RADIATION, &
                                         THERMAL_BC_FLUX, &
                                         THERMAL_BC_ADIABATIC, &
                                         THERMAL_BC_FREE, &
                                         THERMAL_BC_NEUMANN, &
                                         THERMAL_BC_DIRICHLET]

    ! Definition of boundary condition type IDs for hydraulic analysis
    integer(int32), parameter, public :: NUM_HYDRAULIC_BC_TYPES = 5 !&
    integer(int32), parameter, public :: HYDRAULIC_BC_DIRICHLET   = 201 !&
    integer(int32), parameter, public :: HYDRAULIC_BC_NEUMANN     = 202 !&
    integer(int32), parameter, public :: HYDRAULIC_BC_FLUX        = 203 !&
    integer(int32), parameter, public :: HYDRAULIC_BC_IMPERMEABLE = 204 !&
    integer(int32), parameter, public :: HYDRAULIC_BC_SEEPAGE     = 205 !&

    ! -----------------------------
    ! Hydraulic BC application sequence
    ! -----------------------------
    integer(int32), parameter, public :: HYDRAULIC_BC_SEQUENCE(NUM_HYDRAULIC_BC_TYPES) = [ &
                                         HYDRAULIC_BC_FLUX, &
                                         HYDRAULIC_BC_IMPERMEABLE, &
                                         HYDRAULIC_BC_SEEPAGE, &
                                         HYDRAULIC_BC_NEUMANN, &
                                         HYDRAULIC_BC_DIRICHLET]

    !
    ! For initial condition type IDs
    !
    integer(int32), parameter, public :: NUM_INITIAL_CONDITIONS = 4 !&
    !> Thermal initial condition ID
    integer(int32), parameter, public :: INITIAL_CONDITION_THERMAL    = 1 !&
    !> Hydraulic initial condition ID
    integer(int32), parameter, public :: INITIAL_CONDITION_HYDRAULIC  = 2 !&
    !> Mechanical initial condition ID
    integer(int32), parameter, public :: INITIAL_CONDITION_MECHANICAL = 3 !&
    !> Porosity initial condition ID
    integer(int32), parameter, public :: INITIAL_CONDITION_POROSITY   = 4 !&

    integer(int32), parameter, public ::  INITIAL_CONDITION_UNIFORM  = 1 !&
    integer(int32), parameter, public ::  INITIAL_CONDITION_LAPLACE  = 2 !&
    integer(int32), parameter, public ::  INITIAL_CONDITION_FILE     = 3 !&

    integer(int32), parameter, public :: NORM_TYPE_L2      = 1 !&
    integer(int32), parameter, public :: NORM_TYPE_LINF    = 2 !&

    integer(int32), parameter, public :: NONLINEAR_SOLVER_NONE = 0
    integer(int32), parameter, public :: NONLINEAR_SOLVER_NEWTON = 1
    integer(int32), parameter, public :: NONLINEAR_SOLVER_MODIFIED_NEWTON = 2
    integer(int32), parameter, public :: NONLINEAR_SOLVER_PICARD = 3

    integer(int32), parameter, public :: NONLINEAR_NORM_CRITERIA_NONE = 0
    integer(int32), parameter, public :: NONLINEAR_NORM_CRITERIA_RESIDUAL = 1
    integer(int32), parameter, public :: NONLINEAR_NORM_CRITERIA_UPDATE = 2
    integer(int32), parameter, public :: NONLINEAR_NORM_CRITERIA_BOTH = 3

    integer(int32), parameter, public :: NONLINEAR_CRITERIA_NONE = 0
    integer(int32), parameter, public :: NONLINEAR_CRITERIA_RELATIVE = 1
    integer(int32), parameter, public :: NONLINEAR_CRITERIA_ABSOLUTE = 2
    integer(int32), parameter, public :: NONLINEAR_CRITERIA_BOTH = 3

    integer(int32), parameter, public :: NONLINEAR_LOGIC_OR = 1
    integer(int32), parameter, public :: NONLINEAR_LOGIC_AND = 2

    integer(int32), parameter, public :: TIME_UNIT_SECONDS = 1
    integer(int32), parameter, public :: TIME_UNIT_MINUTES = 2
    integer(int32), parameter, public :: TIME_UNIT_HOURS = 3
    integer(int32), parameter, public :: TIME_UNIT_DAYS = 4
    integer(int32), parameter, public :: TIME_UNIT_YEARS = 5

end module core_constants

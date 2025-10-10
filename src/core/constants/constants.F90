!> Defines core constants used throughout the application.
module core_constants
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    private

    ! ==========================================================
    ! Matrix Storage Types
    ! ==========================================================
    !> Dense matrix storage format
    integer(int32), parameter, public :: MATRIX_DENSE = 1
    !> Compressed Row Storage format
    integer(int32), parameter, public :: MATRIX_CRS = 2
    !> Coordinate list format (COO)
    integer(int32), parameter, public :: MATRIX_COO = 3

    ! ==========================================================
    ! Coupling Modes
    ! ==========================================================
    !> Staggered (sequential) coupling scheme
    integer(int32), parameter, public :: COUPLING_MODE_STAGGERED = 1
    !> Monolithic (fully coupled) scheme
    integer(int32), parameter, public :: COUPLING_MODE_MONOLITHIC = 2

    ! ==========================================================
    ! Physics Types
    ! ==========================================================
    !> Thermal analysis identifier
    integer(int32), parameter, public :: PHYSICS_TYPE_THERMAL = 1
    !> Hydraulic analysis identifier
    integer(int32), parameter, public :: PHYSICS_TYPE_HYDRAULIC = 2
    !> Mechanical analysis identifier
    integer(int32), parameter, public :: PHYSICS_TYPE_MECHANICAL = 3
    !> Total number of supported physics types
    integer(int32), parameter, public :: NUM_PHYSICS_TYPES = 3

    ! ==========================================================
    ! Degree of Freedom (DOF) Types
    ! ==========================================================
    !> Temperature degree of freedom
    integer(int32), parameter, public :: DOF_TYPE_T = 1
    !> Pressure degree of freedom
    integer(int32), parameter, public :: DOF_TYPE_P = 2
    !> Displacement in the X-direction
    integer(int32), parameter, public :: DOF_TYPE_DX = 3
    !> Displacement in the Y-direction
    integer(int32), parameter, public :: DOF_TYPE_DY = 4
    !> Displacement in the Z-direction
    integer(int32), parameter, public :: DOF_TYPE_DZ = 5

    ! ==========================================================
    ! Thermal Boundary Conditions
    ! ==========================================================
    !> Dirichlet boundary condition (prescribed temperature)
    integer(int32), parameter, public :: THERMAL_BC_DIRICHLET = 101
    !> Neumann boundary condition (prescribed heat flux)
    integer(int32), parameter, public :: THERMAL_BC_NEUMANN = 102
    !> Specific heat flux condition
    integer(int32), parameter, public :: THERMAL_BC_FLUX = 103
    !> Robin boundary condition (mixed)
    integer(int32), parameter, public :: THERMAL_BC_ROBIN = 104
    !> Convective heat transfer condition
    integer(int32), parameter, public :: THERMAL_BC_CONVECTIVE = 105
    !> Radiative heat transfer condition
    integer(int32), parameter, public :: THERMAL_BC_RADIATION = 106
    !> Adiabatic condition (zero heat flux)
    integer(int32), parameter, public :: THERMAL_BC_ADIABATIC = 107
    !> Free surface condition
    integer(int32), parameter, public :: THERMAL_BC_FREE = 108
    !> Total number of thermal boundary condition types
    integer(int32), parameter, public :: NUM_THERMAL_BC_TYPES = 8
    !> Defines the application priority for thermal BCs.
    integer(int32), parameter, public :: THERMAL_BC_SEQUENCE(NUM_THERMAL_BC_TYPES) = [ &
                                         THERMAL_BC_ROBIN, &
                                         THERMAL_BC_CONVECTIVE, &
                                         THERMAL_BC_RADIATION, &
                                         THERMAL_BC_FLUX, &
                                         THERMAL_BC_ADIABATIC, &
                                         THERMAL_BC_FREE, &
                                         THERMAL_BC_NEUMANN, &
                                         THERMAL_BC_DIRICHLET]

    ! ==========================================================
    ! Hydraulic Boundary Conditions
    ! ==========================================================
    !> Dirichlet boundary condition (prescribed pressure/head)
    integer(int32), parameter, public :: HYDRAULIC_BC_DIRICHLET = 201
    !> Neumann boundary condition (prescribed flow rate)
    integer(int32), parameter, public :: HYDRAULIC_BC_NEUMANN = 202
    !> Specific flux condition
    integer(int32), parameter, public :: HYDRAULIC_BC_FLUX = 203
    !> Impermeable condition (zero flow)
    integer(int32), parameter, public :: HYDRAULIC_BC_IMPERMEABLE = 204
    !> Seepage face condition
    integer(int32), parameter, public :: HYDRAULIC_BC_SEEPAGE = 205
    !> Total number of hydraulic boundary condition types
    integer(int32), parameter, public :: NUM_HYDRAULIC_BC_TYPES = 5
    !> Defines the application priority for hydraulic BCs.
    integer(int32), parameter, public :: HYDRAULIC_BC_SEQUENCE(NUM_HYDRAULIC_BC_TYPES) = [ &
                                         HYDRAULIC_BC_FLUX, &
                                         HYDRAULIC_BC_IMPERMEABLE, &
                                         HYDRAULIC_BC_SEEPAGE, &
                                         HYDRAULIC_BC_NEUMANN, &
                                         HYDRAULIC_BC_DIRICHLET]

    ! ==========================================================
    ! Initial Conditions
    ! ==========================================================
    ! --- Target variable for the initial condition ---
    !> Target for thermal initial condition
    integer(int32), parameter, public :: IC_TARGET_THERMAL = 1
    !> Target for hydraulic initial condition
    integer(int32), parameter, public :: IC_TARGET_HYDRAULIC = 2
    !> Target for mechanical initial condition
    integer(int32), parameter, public :: IC_TARGET_MECHANICAL = 3
    !> Target for porosity initial condition
    integer(int32), parameter, public :: IC_TARGET_POROSITY = 4
    !> Total number of initial condition targets
    integer(int32), parameter, public :: NUM_IC_TARGETS = 4
    ! --- Method for setting the initial condition ---
    !> Set a uniform value across the domain
    integer(int32), parameter, public :: IC_METHOD_UNIFORM = 1
    !> Interpolate from boundary values using a Laplace equation
    integer(int32), parameter, public :: IC_METHOD_LAPLACE = 2
    !> Read values from an input file
    integer(int32), parameter, public :: IC_METHOD_FROM_FILE = 3

    ! ==========================================================
    ! Nonlinear Solver Settings
    ! ==========================================================
    !> No nonlinear solver is used
    integer(int32), parameter, public :: NONLINEAR_SOLVER_NONE = 0
    !> Newton-Raphson method
    integer(int32), parameter, public :: NONLINEAR_SOLVER_NEWTON = 1
    !> Modified Newton-Raphson method
    integer(int32), parameter, public :: NONLINEAR_SOLVER_MODIFIED_NEWTON = 2
    !> Picard iteration method
    integer(int32), parameter, public :: NONLINEAR_SOLVER_PICARD = 3

    !> Do not check a norm for convergence
    integer(int32), parameter, public :: NONLINEAR_NORM_CRITERIA_NONE = 0
    !> Use the residual vector norm for convergence
    integer(int32), parameter, public :: NONLINEAR_NORM_CRITERIA_RESIDUAL = 1
    !> Use the solution update vector norm for convergence
    integer(int32), parameter, public :: NONLINEAR_NORM_CRITERIA_UPDATE = 2
    !> Use both the residual and update norms for convergence
    integer(int32), parameter, public :: NONLINEAR_NORM_CRITERIA_BOTH = 3

    !> Do not use a convergence criteria
    integer(int32), parameter, public :: NONLINEAR_CRITERIA_NONE = 0
    !> Use a relative tolerance for convergence
    integer(int32), parameter, public :: NONLINEAR_CRITERIA_RELATIVE = 1
    !> Use an absolute tolerance for convergence
    integer(int32), parameter, public :: NONLINEAR_CRITERIA_ABSOLUTE = 2
    !> Use both relative and absolute tolerances for convergence
    integer(int32), parameter, public :: NONLINEAR_CRITERIA_BOTH = 3

    !> Logical OR for combining criteria
    integer(int32), parameter, public :: NONLINEAR_LOGIC_OR = 1
    !> Logical AND for combining criteria
    integer(int32), parameter, public :: NONLINEAR_LOGIC_AND = 2

    ! ==========================================================
    ! Miscellaneous Constants
    ! ==========================================================
    !> L2 (Euclidean) norm
    integer(int32), parameter, public :: NORM_TYPE_L2 = 1
    !> L-infinity (maximum absolute value) norm
    integer(int32), parameter, public :: NORM_TYPE_LINF = 2

    !> Time unit in seconds
    integer(int32), parameter, public :: TIME_UNIT_SECONDS = 1
    !> Time unit in minutes
    integer(int32), parameter, public :: TIME_UNIT_MINUTES = 2
    !> Time unit in hours
    integer(int32), parameter, public :: TIME_UNIT_HOURS = 3
    !> Time unit in days
    integer(int32), parameter, public :: TIME_UNIT_DAYS = 4
    !> Time unit in years
    integer(int32), parameter, public :: TIME_UNIT_YEARS = 5

    !> Time recording point at the start
    integer(int32), parameter, public :: TIME_RECORD_START = 1
    !> Time recording point at the end
    integer(int32), parameter, public :: TIME_RECORD_END = 2

    ! --- Internal constants for spatial dimensions ---
    integer(int32), parameter :: PLANE_XY = 1
    integer(int32), parameter :: PLANE_XZ = 2
    integer(int32), parameter :: THREE_DIM = 3

end module core_constants

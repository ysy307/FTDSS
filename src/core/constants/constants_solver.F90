!> Defines core constants used throughout the application.
module core_constants_solver
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    private

    ! ==========================================================
    ! Matrix Storage Types
    ! ==========================================================
    !> Dense matrix storage format
    integer(int32), parameter, public :: MATRIX_DENSE = 1
    !> Compressed Row Storage format
    integer(int32), parameter, public :: MATRIX_CSR = 2
    !> Coordinate list format (COO)
    integer(int32), parameter, public :: MATRIX_COO = 3
    !> Block Sparse Row format
    integer(int32), parameter, public :: MATRIX_BSR = 4

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
    !> 2D XY dimension
    integer(int32), parameter, public :: COMP_TYPE_2D_XY = 1
    !> 2D XZ dimension
    integer(int32), parameter, public :: COMP_TYPE_2D_XZ = 2
    !> 3D XYZ dimension
    integer(int32), parameter, public :: COMP_TYPE_3D = 3

    ! ==========================================================
    ! Linear Solver Types
    ! ==========================================================
    !> Conjugate Gradient
    integer(int32), parameter, public :: SOLVER_CG           = 1 !&
    !> Bi-Conjugate Gradient
    integer(int32), parameter, public :: SOLVER_BICG         = 2 !&
    !> Conjugate Gradient Squared
    integer(int32), parameter, public :: SOLVER_CGS          = 3 !&
    !> Bi-Conjugate Gradient Stabilized
    integer(int32), parameter, public :: SOLVER_BICGSTAB     = 4 !&
    !> Bi-Conjugate Gradient Stabilized (l)
    integer(int32), parameter, public :: SOLVER_BICGSTAB_L   = 5 !&
    !> Generalized Product-type Bi-Conjugate Gradient
    integer(int32), parameter, public :: SOLVER_GPBICG       = 6 !&
    !> Transpose-Free Quasi-Minimal Residual
    integer(int32), parameter, public :: SOLVER_TFQMR        = 7 !&
    !> Orthomin(m)
    integer(int32), parameter, public :: SOLVER_ORTHOMIN_M   = 8 !&
    !> Generalized Minimal Residual (m)
    integer(int32), parameter, public :: SOLVER_GMRES_M      = 9 !&
    !> Jacobi
    integer(int32), parameter, public :: SOLVER_JACOBI       = 10 !&
    !> Gauss-Seidel
    integer(int32), parameter, public :: SOLVER_GAUSS_SEIDEL = 11 !&
    !> Successive Over-Relaxation
    integer(int32), parameter, public :: SOLVER_SOR          = 12 !&
    !> BiCGSafe
    integer(int32), parameter, public :: SOLVER_BICGSAFE     = 13 !&
    !> Conjugate Residual
    integer(int32), parameter, public :: SOLVER_CR           = 14 !&
    !> Bi-Conjugate Residual
    integer(int32), parameter, public :: SOLVER_BICR         = 15 !&
    !> Conjugate Residual Squared
    integer(int32), parameter, public :: SOLVER_CRS          = 16 !&
    !> Bi-Conjugate Residual Stabilized
    integer(int32), parameter, public :: SOLVER_BICRSTAB     = 17 !&
    !> Generalized Product-type Bi-Conjugate Residual
    integer(int32), parameter, public :: SOLVER_GPBICR       = 18 !&
    !> BiCRSafe
    integer(int32), parameter, public :: SOLVER_BICRSAFE     = 19 !&
    !> Flexible GMRES (m)
    integer(int32), parameter, public :: SOLVER_FGMRES_M     = 20 !&
    !> Induced Dimension Reduction (s)
    integer(int32), parameter, public :: SOLVER_IDR_S        = 21 !&
    !> IDR(1)
    integer(int32), parameter, public :: SOLVER_IDR1         = 22 !&
    !> Minimal Residual
    integer(int32), parameter, public :: SOLVER_MINRES       = 23 !&
    !> Conjugate Orthogonal Conjugate Gradient
    integer(int32), parameter, public :: SOLVER_COCG         = 24 !&
    !> Conjugate Orthogonal Conjugate Residual
    integer(int32), parameter, public :: SOLVER_COCR         = 25 !&

    ! ==========================================================
    ! Preconditioner Types
    ! ==========================================================
    !> No preconditioner
    integer(int32), parameter, public :: SOLVER_PRECONDITION_NONE   = 0 !&
    !> Jacobi preconditioner
    integer(int32), parameter, public :: SOLVER_PRECONDITION_JACOBI = 1 !&
    !> ILU(k) (Incomplete LU)
    integer(int32), parameter, public :: SOLVER_PRECONDITION_ILU    = 2 !&
    !> SSOR (Symmetric Successive Over-Relaxation)
    integer(int32), parameter, public :: SOLVER_PRECONDITION_SSOR   = 3 !&
    !> Hybrid preconditioner
    integer(int32), parameter, public :: SOLVER_PRECONDITION_HYBRID = 4 !&
    !> I+S preconditioner
    integer(int32), parameter, public :: SOLVER_PRECONDITION_IS     = 5 !&
    !> SAINV (Sparse Approximate Inverse)
    integer(int32), parameter, public :: SOLVER_PRECONDITION_SAINV  = 6 !&
    !> SA-AMG (Sparse Approximate Algebraic Multigrid)
    integer(int32), parameter, public :: SOLVER_PRECONDITION_SAAMG  = 7 !&
    !> Crout ILU
    integer(int32), parameter, public :: SOLVER_PRECONDITION_ILUC   = 8 !&
    !> ILUT (Incomplete LU with threshold)
    integer(int32), parameter, public :: SOLVER_PRECONDITION_ILUT   = 9 !&

    ! ==========================================================
    ! Solver Status Codes
    ! ==========================================================
    !> Solver completed successfully
    integer(int32), parameter, public :: SOLVER_STATUS_SUCCESS = 0
    !> Solver encountered ill-conditioned options
    integer(int32), parameter, public :: SOLVER_STATUS_ILL_OPTIONS = -1
    !> Solver encountered a breakdown
    integer(int32), parameter, public :: SOLVER_STATUS_BREAKDOWN = -2
    !> Solver ran out of memory
    integer(int32), parameter, public :: SOLVER_STATUS_OUT_OF_MEMORY = -3
    !> Solver reached maximum iterations without convergence
    integer(int32), parameter, public :: SOLVER_STATUS_MAXITER = -4
    !> Preconditioner setup failure
    integer(int32), parameter, public :: SOLVER_STATUS_DECOMPOSITION_FAILURE = -5
    !> Solver method not implemented
    integer(int32), parameter, public :: SOLVER_STATUS_NOT_IMPLEMENTED = -6

    ! =========================================================
    ! Matrix Operations
    ! ==========================================================
    !> Insert operation
    integer(int32), parameter, public :: OP_INS = 1
    !> Add operation
    integer(int32), parameter, public :: OP_ADD = 2

    integer(int32), parameter, public :: OP_SCALE_SYMM_DIAG = 3
    integer(int32), parameter, public :: OP_SCALE_JACOBI = 4

    !=========================================================
    ! Matrix Check Status Codes
    !=========================================================
    !> Matrix is valid
    integer(int32), parameter, public :: MATRIX_STATUS_SUCCESS = 0
    !> Matrix has an error
    integer(int32), parameter, public :: MATRIX_STATUS_ILL_OPERATIONS = -1
    integer(int32), parameter, public :: MATRIX_STATUS_OUT_OF_MEMORY = -2
    integer(int32), parameter, public :: MATRIX_STATUS_NOT_INITIALIZED = -3

    !> Matrix method not implemented
    integer(int32), parameter, public :: MATRIX_STATUS_NOT_IMPLEMENTED = -5

    integer(int32), parameter, public :: VECTOR_STATUS_SUCCESS = 0
    integer(int32), parameter, public :: VECTOR_STATUS_ILL_OPERATIONS = -1
    integer(int32), parameter, public :: VECTOR_STATUS_OUT_OF_MEMORY = -3
    integer(int32), parameter, public :: VECTOR_STATUS_NOT_IMPLEMENTED = -5

    !=========================================================
    ! IAPWS Constants
    !=========================================================
    integer(int32), parameter, public :: IAPWS_OUT_OF_RANGE = -1
    integer(int32), parameter, public :: IAPWS_REGION_ICE_Ih = 14
    integer(int32), parameter, public :: IAPWS97_REGION_1 = 1 ! Liquid
    integer(int32), parameter, public :: IAPWS97_REGION_2 = 2 ! Vapor
    integer(int32), parameter, public :: IAPWS97_REGION_3 = 3 ! Critical / High P
    integer(int32), parameter, public :: IAPWS97_REGION_4 = 4 ! Saturation (通常は判定結果として返さないが内部で使用)
    integer(int32), parameter, public :: IAPWS97_REGION_5 = 5 ! High Temp Vapor
end module core_constants_solver

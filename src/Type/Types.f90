module Types
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
    public
#ifdef _MPI
    include 'mpif.h'
#endif
    integer(int32), parameter :: Temperature = 1, Pressure = 2, Stress = 3
    integer(int32), parameter :: Linear = 1, pTransition = 2, NonLinear = 3, nTransition = 4
    real(real64), parameter :: GravityAcceleration = 9.80655d0
    integer(int32), parameter :: undumped = 0, dumped = 1

    abstract interface
        logical(4) function condition_function(num, group)
            use, intrinsic :: iso_fortran_env, only: int32
            implicit none
            integer(int32), intent(in) :: num
            integer(int32), intent(in) :: group
        end function condition_function
    end interface

    type :: VC
        sequence
        real(real64) :: x, y
    end type VC

    type :: Vector2D
        sequence
        real(real64) :: x, y
    end type Vector2D

    type :: Vector3D
        sequence
        real(real64) :: x, y, z
    end type Vector3D

    type :: DP2d
        sequence
        real(real64), allocatable :: x(:), y(:)
    end type DP2d

    type :: DP3d
        sequence
        real(real64), allocatable :: x(:), y(:), z(:)
    end type DP3d

    type :: INT2d
        sequence
        integer(int32), allocatable :: x(:), y(:)
    end type INT2d

    type :: INT3d
        sequence
        integer(int32), allocatable :: x(:), y(:), z(:)
    end type INT3d

    type :: PH
        sequence
        real(real64) :: soil, water, ice
    end type PH

    type :: Phases
        sequence
        real(real64) :: soil, water, ice
    end type Phases

    type :: Shape
        sequence
        real(real64), allocatable :: a(:, :), b(:, :), c(:, :), d(:, :)
    end type Shape

    type :: BoudaryConditionInfo
        integer(int32), allocatable :: Node(:) ! <= BC.in
        integer(int32), allocatable :: TypeKey(:) ! <= BC.in
        integer(int32), allocatable :: type(:) ! <= BCtype.in
        real(real64), allocatable :: value(:) ! <= BCtype.in
        type(INT2d) :: Edges
        integer(int32), allocatable :: EdgesDirection(:)
        real(real64), allocatable :: EdgesDistance(:)
    end type BoudaryConditionInfo

    type :: BoudaryCondition
        type(BoudaryConditionInfo) :: Heat, Water, Stress
        integer(int32) :: numNode, numType, numEdges
    end type BoudaryCondition

    type :: Boudary_Condition_Dirichlet
        integer(int32) :: Num_Node, Num_Type
        integer(int32), allocatable :: Node(:), Node_Type(:), Value_Info(:)
        real(real64), allocatable :: value(:)
    end type Boudary_Condition_Dirichlet

    type :: Boudary_Condition_Neumann
        integer(int32) :: Num_Edge, Num_Edge_Type, Num_Type
        integer(int32), allocatable :: Edge(:, :), Edge_Type(:), Value_Info(:)
        real(real64), allocatable :: value(:), Heat_Transfer(:)

    end type Boudary_Condition_Neumann

    type :: Boudary_Condition
        type(Boudary_Condition_Dirichlet) :: Dirichlet
        type(Boudary_Condition_Neumann) :: Neumann
    end type Boudary_Condition

    type :: InitialConditionInfo
        integer(int32) :: type
        real(real64) :: value
        logical(4) :: isSet
    end type InitialConditionInfo

    type :: InitialCondition
        type(InitialConditionInfo) :: Heat, Water, Stress
    end type InitialCondition

#ifdef _MPI
    type :: MPIInfo
        integer(int32) :: size, rank
    end type MPIInfo
#endif

    type :: Basic_params
        integer(int32) :: Element
        integer(int32) :: Node
        integer(int32) :: ShapeType
        integer(int32) :: DimensionType !! 1 is 2D horizontal, 2 is 2D vertical, 3 is 3D
        integer(int32) :: Region
        character(:), allocatable :: Calculation_timeUnit
        character(:), allocatable :: Input_timeUnit
        character(:), allocatable :: Output_timeUnit
        character(:), allocatable :: Interval_timeUnit
        integer(int32) :: Calculation_step
        integer(int32) :: CalculationPeriod
        integer(int32) :: Interval
        logical(4) :: isDisplayPrompt
        character(:), allocatable :: FileOutput
        real(real64) :: TimeDiscretization
    end type Basic_params

    type :: Type_Solver
        class(Base_Solver), allocatable :: Thermal ! Solver for heat transfer
        class(Base_Solver), allocatable :: Hydraulic ! Solver for water flow
    end type Type_Solver

    type :: Base_Solver
        integer(int32) :: useSolver ! Solver fpr execute solution 1: Direct, 2: 22Iterative
    end type Base_Solver

    type, extends(Base_Solver) :: Type_Solver_Iterative
        integer(int32) :: SolverType !! Solver type
        integer(int32) :: PreconditionerType !! Preconditioner type
        integer(int32) :: MaxIter !! Maximum number of iterations
        real(real64) :: Tol !! Convergence criterion
    end type Type_Solver_Iterative

    type :: BC_Condition
        character(:), allocatable :: type
        real(real64) :: value
    end type BC_Condition

    type :: IC_Condition
        character(:), allocatable :: type
        !! Constant initial condition
        real(real64) :: value
        !! Following the laplace equation
        type(BC_Condition), allocatable :: IC_BC(:)
    end type IC_Condition

    type :: Type_Conditions
        integer(int32), allocatable :: BCGroup(:)
        type(BC_Condition), allocatable :: BC_Thermal(:)
        type(BC_Condition), allocatable :: BC_Hydraulic(:)
        type(IC_Condition) :: IC_Thermal
        type(IC_Condition) :: IC_Hydraulic
    end type Type_Conditions

    type :: Base_Density
    end type Base_Density

    type, extends(Base_Density) :: Type_Density_3Phase
        real(real64) :: Soil, Water, Ice
    end type Type_Density_3Phase

    type, extends(Base_Density) :: Type_Density_2Phase
        real(real64) :: Phase1, Phase2
    end type Type_Density_2Phase

    type, extends(Base_Density) :: Type_Density_1Phase
        real(real64) :: Phase1
    end type Type_Density_1Phase

    type :: Base_SpecificHeat
    end type Base_SpecificHeat

    type, extends(Base_SpecificHeat) :: Type_SpecificHeat_3Phase
        real(real64) :: Soil ! Soil specific heat
        real(real64) :: Water ! Water specific heat
        real(real64) :: Ice ! Ice specific heat
    end type Type_SpecificHeat_3Phase

    type, extends(Base_SpecificHeat) :: Type_SpecificHeat_2Phase
        real(real64) :: Phase1, Phase2
    end type Type_SpecificHeat_2Phase

    type, extends(Base_SpecificHeat) :: Type_SpecificHeat_1Phase
        real(real64) :: Phase1
    end type Type_SpecificHeat_1Phase

    type :: Base_ThermalConductivity
    end type Base_ThermalConductivity

    type, extends(Base_ThermalConductivity) :: Type_ThermalConductivity_3Phase
        real(real64) :: Soil, Water, Ice
    end type Type_ThermalConductivity_3Phase

    type, extends(Type_ThermalConductivity_3Phase) :: Type_ThermalConductivity_3Phase_Dispersity_2D
        type(Vector2D) :: dispersity
    end type Type_ThermalConductivity_3Phase_Dispersity_2D

    type, extends(Type_ThermalConductivity_3Phase) :: Type_ThermalConductivity_3Phase_Dispersity_3D
        type(Vector3D) :: dispersity
    end type Type_ThermalConductivity_3Phase_Dispersity_3D

    type, extends(Base_ThermalConductivity) :: Type_ThermalConductivity_2Phase
        real(real64) :: Phase1, Phase2
    end type Type_ThermalConductivity_2Phase

    type, extends(Base_ThermalConductivity) :: Type_ThermalConductivity_1Phase
        real(real64) :: Phase1
    end type Type_ThermalConductivity_1Phase

    type :: Base_Ice
        real(real64) :: LatentHeat !! Latent heat of fusion
    end type Base_Ice

    type, extends(Base_Ice) :: Type_Ice_TRM
        real(real64) :: Tf !! Freezing point
    end type Type_Ice_TRM

    type, extends(Base_Ice) :: Type_Ice_GCC
        real(real64) :: Tf !! Freezing point
        integer(int32) :: ModelType
        class(Base_WRF), allocatable :: WRF
    end type Type_Ice_GCC

    type, extends(Base_Ice) :: Type_Ice_EXP
        real(real64) :: Tf !! Freezing point
        real(real64) :: a !! power model parameter
    end type Type_Ice_EXP

    type :: Type_Thermal
        class(Base_Density), allocatable :: Density !! Density
        class(Base_SpecificHeat), allocatable :: SpecificHeat !! Specific heat
        class(Base_ThermalConductivity), allocatable :: ThermalConductivity !! Thermal conductivity
        real(real64) :: Porosity !! Porosity
        class(Base_Ice), allocatable :: Ice !! Ice
    end type Type_Thermal

    type :: Type_Hydraulic
        integer(int32) :: useHCF
        class(Base_HCF), allocatable :: HCF
        logical(4) :: useImpedance
        class(Base_Impedance), allocatable :: Impedance
        integer(int32) :: useKTDynamics
        class(Base_KTDynamics), allocatable :: KTDynamics
        real(real64) :: Ks
    end type Type_Hydraulic

    type Base_Impedance
    end type Base_Impedance

    type, extends(Base_Impedance) :: Type_Impedance
        real(real64) :: Omega
    end type Type_Impedance

    type :: Base_KTDynamics
    end type Base_KTDynamics

    type, extends(Base_KTDynamics) :: Type_KTDynamics
        real(real64) :: kzero
    end type Type_KTDynamics

    type :: Type_Region_Flags
        logical(4) :: isHeat, isWater, isStress
        logical(4) :: is1Phase, is2Phase, is3Phase
        logical(4) :: isCompression, isFrostHeavePressure, isDispersity
        logical(4) :: isFrozen
    end type Type_Region_Flags

    type :: Type_Region
        integer(int32) :: BelongingSurface
        integer(int32), allocatable :: BelongingEdge(:)
        integer(int32) :: CalculationType
        integer(int32) :: Modelnumber
        type(Type_Thermal) :: Thermal
        type(Type_Hydraulic) :: Hydraulic
        type(Type_Region_Flags) :: Flags
    end type Type_Region

    type :: Type_VTK
        character(:), allocatable :: format !! ASCII or BINARY
        character(:), allocatable :: dataset !! STRUCTURED_POINTS, STRUCTURED_GRID, RECTILINEAR_GRID, POLYDATA, UNSTRUCTURED_GRID
        character(:), allocatable :: POINTS_DATATYPE !! dataType is one of the types bit, unsigned_char, char, unsigned_short, short, unsigned_int, int,  unsigned_long, long, float, or double.
        integer(int32) :: numPoints !! Number of points
        type(DP3d) :: POINTS !! VTK 3D geometry coordinates
        integer(int32) :: numCells !! Number of cells
        integer(int32) :: numCellsList !! Number of cells in the list
        integer(int32) :: numCellTypes !! Number of cell types

        type(Type_VTK_CELLS), allocatable :: CELLS(:) !! Cell information
        integer(int32), allocatable :: Invalid_CELLS_LIST(:) !! Cell information in the list
        integer(int32), allocatable :: CellEntityIds(:) !! Cell entity IDs

    end type Type_VTK

    type :: Type_VTK_CELLS
        integer(int32) :: nCells !! Number of cells beloging to the region
        integer(int32), allocatable :: Nodes(:, :) !! Node numbers of the cells
        integer(int32), allocatable :: Nodes_Array(:) !! Node numbers of the cells in the array
    end type Type_VTK_CELLS

    type :: DF
        real(real64), allocatable :: new(:), old(:), pre(:), dif(:), div(:), tmp(:)
    end type DF

    type :: Variables
        real(real64), allocatable :: new(:), old(:), pre(:), dif(:), div(:), tmp(:)
    end type Variables

    type :: Base_Parameters
    end type Base_Parameters

    type, extends(Base_Parameters) :: Heat_Parameters
        type(Variables) :: Cs, Cp, lambda, rho, Ca
        type(DP3d) :: Tgrad, TFlux
        integer(int32), allocatable :: Phase(:)
        type(Type_Thermal) :: Constants
    end type Heat_Parameters

    type, extends(Base_Parameters) :: Water_Parameters
        type(Variables) :: Klh
        type(DP3d) :: wFlux, hGrad
        type(Type_Hydraulic) :: Constants
    end type Water_Parameters

    type :: Flag
        logical(4) :: isTRM, isGCC, isPower, isSwitchTRM, isSwitchOnceTRM
        logical(4) :: isStdOut, isOutputAll, isOutput, isPrintLisMem
        logical(4), allocatable :: outOBS(:)
    end type Flag

    type :: CRS
        integer(int32) :: nnz
        integer(int32), allocatable :: Ptr(:), Ind(:)
        real(real64), allocatable :: Val(:)
    end type CRS

    type :: Lis
        integer(int32) :: TSolver, TOption, PSolver, POption, Maxiter
        real(real64) :: Tol
        logical(4) :: isOMP
    end type Lis

    type :: Observation2d
        integer(int32) :: nObs, nObsType
        ! 1: Nodes
        integer(int32), allocatable :: obsPoint(:)
        ! 2: Coordinate
        type(DP2d) :: obsCOO
        integer(int32), allocatable :: nAreaObs(:)
        real(real64), allocatable :: vAreaObs(:, :)

    end type Observation2d

    type :: HeatVariables
        type(DF) :: Cs, Cp, lambda, rho, Ca
        type(DP2d) :: Tgrad, TFlux
        integer(int32), allocatable :: Phase(:)
    end type HeatVariables

    type HeatConstants
        type(Phases) :: Density, ThermalConductivity, SpecificHeat, HeatCapacity
        type(Vector2d) :: dispersity
        real(real64) :: Porosity, LatentHeat
    end type HeatConstants

    type PowerModel
        real(real64) :: phi, Tf, a
        real(real64) :: Ca_max
    end type PowerModel

    type :: GCCModel
        real(real64) :: thetaS, thetaR, alpha, n, m
        real(real64) :: Tf
        real(real64) :: Ca_max
    end type GCCModel

    type :: WRF_Parameters
        ! w1, w2は先に計算しておく
        real(real64) :: thetaS, thetaR, alpha1, alpha2, n1, n2, m1, m2, hcrit, w1, w2
    end type WRF_Parameters

    type :: Base_WRF
        real(real64) :: thetaS, thetaR
    end type Base_WRF

    ! type, extends(Base_WRF) :: Type_WRF_BC
    !     real(real64) :: alpha1, n1
    ! end type Type_WRF_BC

    ! type, extends(Base_WRF) :: Type_WRF_VG
    !     real(real64) :: alpha1, n1, m1
    ! end type Type_WRF_VG

    ! type, extends(Base_WRF) :: Type_WRF_KO
    !     real(real64) :: alpha1, n1
    ! end type Type_WRF_KO

    ! type, extends(Base_WRF) :: Type_WRF_MVG
    !     real(real64) :: alpha1, n1, m1, hcrit
    ! end type Type_WRF_MVG

    ! type, extends(Base_WRF) :: Type_WRF_Durner
    !     real(real64) :: alpha1, n1, m1, alpha2, n2, m2, w1, w2
    ! end type Type_WRF_Durner

    ! type, extends(Base_WRF) :: Type_WRF_DVGCH
    !     real(real64) :: alpha1, n1, m1, n2, m2, w1, w2
    ! end type Type_WRF_DVGCH

    type :: HCF_Parameters
        real(real64) :: thetaS, thetaR, alpha1, alpha2, n1, n2, m1, m2, hcrit, w1, w2
        real(real64) :: Ks, kzero, l, Omega
    end type HCF_Parameters

    type :: Base_HCF
        real(real64) :: thetaS, thetaR
    end type Base_HCF

    type, extends(Base_HCF) :: Type_HCF_BC
        real(real64) :: alpha1, n1, l
    end type Type_HCF_BC

    type, extends(Base_HCF) :: Type_HCF_VG
        real(real64) :: alpha1, n1, m1, l
    end type Type_HCF_VG

    type, extends(Base_HCF) :: Type_HCF_KO
        real(real64) :: alpha1, n1
    end type Type_HCF_KO

    type, extends(Base_HCF) :: Type_HCF_MVG
        real(real64) :: alpha1, n1, m1, hcrit, l
    end type Type_HCF_MVG

    type, extends(Base_HCF) :: Type_HCF_Durner
        real(real64) :: alpha1, n1, m1, alpha2, n2, m2, w1, w2, l
    end type Type_HCF_Durner

    type, extends(Base_HCF) :: Type_HCF_DVGCH
        real(real64) :: alpha1, n1, m1, n2, m2, w1, w2, l
    end type Type_HCF_DVGCH

    type :: LatentHeatTreatment
        integer(int32) :: useModel ! 20: GCC, 30: Power
        real(real64) :: Lf, rhoI
        real(real64) :: Cp_unf
        type(GCCModel) :: GCC
        type(PowerModel) :: Power
    end type LatentHeatTreatment

    type :: HeatFields
        type(HeatVariables) :: Variables
        type(HeatConstants) :: Constants
        type(LatentHeatTreatment) :: Latent
        type(CRS) :: LHS_A
        real(real64), allocatable :: RA(:, :)
        real(real64), allocatable :: Rhs(:)
    end type HeatFields

    type WaterVariables
        type(DF) :: Klh
        type(DP2d) :: wFlux, hGrad
    end type WaterVariables

    type WaterConstants
        type(Phases) :: HydraulicConductivity
        real(real64) :: zeta
    end type WaterConstants

    type :: WaterFields
        type(WaterVariables) :: Variables
        type(WaterConstants) :: Constants
        type(CRS) :: LHS_A
        real(real64), allocatable :: RA(:, :)
        real(real64), allocatable :: Rhs(:)
    end type WaterFields

    type :: Geometry2d
        integer(int32) :: element, node, shape, dim, ShCoe
        integer(int32), allocatable :: pElement(:, :)
        type(DP2d) :: vCood
        real(real64), allocatable :: eArea(:)
        type(Shape) :: Basis
    end type Geometry2d

    type :: Type_Geometry
        type(Basic_params) :: Basic !! Basic parameters
        integer(int32), allocatable :: Element(:, :)
        integer(int32), allocatable :: Element_Region(:)
        type(DP3d) :: Nodes
        real(real64), allocatable :: Area(:)
        type(Shape) :: Basis
    end type Type_Geometry

    type :: TimeInfo
        character(3) :: tUnit
        ! tUnit <- nmk 1: Second, 2: Minute, 3: Hour, 4: Day, 5: Month, 6: Year
        ! n : Calculation time unit, m: dt unit, k: output interval time unit
        real(real64) :: cTime, cdt, cinterval
        real(real64) :: ts, te, max_dt, min_dt, tconv
        real(real64), pointer :: tst, dt, odt
    end type TimeInfo

    type :: Iteration
        integer(int32) :: itermax, iNLmax, iNI
        integer(int32), pointer :: iter, titer, iNL
        integer(int32) :: digits_itermax
    end type Iteration

    type :: SolverInfo
        type(Geometry2d) :: N
        logical(4) :: isHeat, isWater, isStress
        integer(int32) :: nAnalysis, nFrTreat, nTimeDisc, isStdOut, outputFile
        type(TimeInfo) :: Time
        type(Iteration) :: Iter
        type(Observation2d) :: Obs
        type(HeatFields) :: Heat
        type(WaterFields) :: Water
        type(BoudaryCondition) :: BC
        type(InitialCondition) :: IC
        type(DF) :: mWater, mIce
        type(DF) :: T, P
        type(DF) :: Si, Sw
        type(Lis) :: Lis
        type(Flag) :: Flags
        character(64) :: fmt_Stdout, fmt_Fileout
#ifdef _MPI
        type(MPIInfo) :: MPI
#endif
    end type SolverInfo
end module Types

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
        logical :: isSet
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
        integer(int32) :: Shape
        integer(int32) :: Dim
        integer(int32) :: Region
        character(:), allocatable :: Calculation_timeUnit
        character(:), allocatable :: Input_timeUnit
        character(:), allocatable :: Output_timeUnit
        character(:), allocatable :: Interval_timeUnit
        integer(int32) :: Calculation_step
        integer(int32) :: CalculationPeriod
        integer(int32) :: Interval
        logical :: isDisplayPrompt
        character(:), allocatable :: FileOutput
    end type Basic_params

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
        real(real64) :: Soil, Water, Ice
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
        real(real64) :: LatentHeat
    end type Base_Ice

    type, extends(Base_Ice) :: Type_Ice_TRM
        real(real64) :: Tf
    end type Type_Ice_TRM

    type, extends(Base_Ice) :: Type_Ice_GCC
        real(real64) :: Tf
        integer(int32) :: ModelType
        class(Base_WRF), allocatable :: WRF
    end type Type_Ice_GCC

    type, extends(Base_Ice) :: Type_Ice_EXP
        real(real64) :: Tf, a
    end type Type_Ice_EXP

    type :: Type_Thermal
        class(Base_Density), allocatable :: Density
        class(Base_SpecificHeat), allocatable :: SpecificHeat
        class(Base_ThermalConductivity), allocatable :: ThermalConductivity
        real(real64) :: Porosity

        class(Base_Ice), allocatable :: Ice
    end type Type_Thermal

    type :: Base_Hydraulic
        logical :: useImpedance
        class(Base_Impedance), allocatable :: Impedance
        integer(int32) :: useKTDynamics
        class(Base_KTDynamics), allocatable :: KTDynamics
        logical :: useHCF
        class(Base_HCF), allocatable :: HCF
        real(real64) :: Ks
    end type Base_Hydraulic

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
        logical :: isHeat, isWater, isStress
        logical :: is1Phase, is2Phase, is3Phase
        logical :: isCompression, isFrostHeavePressure, isDispersity
        logical :: isFrozen
    end type Type_Region_Flags

    type :: Type_Region
        integer(int32) :: CalculationType
        integer(int32) :: Modelnumber
        type(Type_Thermal) :: Thermal
        type(Type_Region_Flags) :: Flags
    end type Type_Region

    type :: DF
        real(real64), allocatable :: new(:), old(:), pre(:), dif(:), div(:), tmp(:)
    end type DF

    type :: Flag
        logical :: isTRM, isGCC, isPower, isSwitchTRM, isSwitchOnceTRM
        logical :: isStdOut, isOutputAll, isOutput, isPrintLisMem
        logical, allocatable :: outOBS(:)
    end type Flag

    type :: CRS
        integer(int32) :: nnz
        integer(int32), allocatable :: Ptr(:), Ind(:)
        real(real64), allocatable :: Val(:)
    end type CRS

    type :: Lis
        integer(int32) :: TSolver, TOption, PSolver, POption, Maxiter
        real(real64) :: Tol
        logical :: isOMP
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

    type, extends(Base_WRF) :: Type_WRF_BC
        real(real64) :: alpha1, n1
    end type Type_WRF_BC

    type, extends(Base_WRF) :: Type_WRF_VG
        real(real64) :: alpha1, n1, m1
    end type Type_WRF_VG

    type, extends(Base_WRF) :: Type_WRF_KO
        real(real64) :: alpha1, n1
    end type Type_WRF_KO

    type, extends(Base_WRF) :: Type_WRF_MVG
        real(real64) :: alpha1, n1, m1, hcrit
    end type Type_WRF_MVG

    type, extends(Base_WRF) :: Type_WRF_Durner
        real(real64) :: alpha1, n1, m1, alpha2, n2, m2, w1, w2
    end type Type_WRF_Durner

    type, extends(Base_WRF) :: Type_WRF_DVGCH
        real(real64) :: alpha1, n1, m1, n2, m2, w1, w2
    end type Type_WRF_DVGCH

    type :: HCF_Parameters
        real(real64) :: thetaS, thetaR, alpha1, alpha2, n1, n2, m1, m2, hcrit, w1, w2
        real(real64) :: Ks, kzero, l, Omega
    end type HCF_Parameters

    type :: Base_HCF
        real(real64) :: thetaS, thetaR
    end type Base_HCF

    type, extends(Base_HCF) :: Type_HCF_BC
        real(real64) :: alpha1, n1
    end type Type_HCF_BC

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

    type :: Geometry_2D
        integer(int32) :: Num_Elements, Num_Nodes, Num_Shape, Num_Dimention, Num_Shape_Type, Num_Region
        integer(int32), allocatable :: Element(:, :)
        integer(int32), allocatable :: Element_Region(:), COO_Region(:)
        type(DP2d) :: Nodes_2D
        real(real64), allocatable :: Area(:)
        type(Shape) :: Shape_Function
    end type Geometry_2D

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
        logical :: isHeat, isWater, isStress
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

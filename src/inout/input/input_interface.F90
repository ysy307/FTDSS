module inout_input
    use, intrinsic :: iso_fortran_env, only: int32, real64, output_unit
!$  use :: omp_lib
    use :: stdlib_strings, only:to_string, ends_with
    use :: stdlib_logger
    use :: json_module, only:json_file
    use :: inout_project_settings, only:get_project_path
    use :: module_core, only:type_vtk, type_dp_3d, type_dp_vector_3d, allocate_array, deallocate_array, & !&
                             error_message, join, value_in_range
    implicit none
!     private

    integer(int32), parameter :: min_calculation_type = 1, max_calculation_type = 7
    integer(int32), parameter :: min_model_type = 11, max_model_type = 18
    integer(int32), parameter :: min_Coordinate_Dimesion_type = 1, max_Coordinate_Dimesion_type = 3
    character(*), parameter :: ThermalName = "Thermal"
    character(*), parameter :: HydraulicName = "Hydraulic"
    character(*), parameter :: ElementName = "Element"
    character(*), parameter :: NodeName = "Node"
    character(*), parameter :: ShapeName = "ShapeType"

    character(*), parameter :: BelongName = "Belong"
    character(*), parameter :: SurfaceName = "Surface"
    character(*), parameter :: EdgeName = "Edge"
    character(*), parameter :: CalculationName = "Calculation"
    character(*), parameter :: InputName = "Input"
    character(*), parameter :: OutputName = "Output"
    character(*), parameter :: IntervalName = "Interval"

    !!------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: BasicName = "Basic"
    character(*), parameter :: DimensionName = "DimensionType"
    character(*), parameter :: RegionName = "Region"
    character(*), parameter :: TimeName = "Time"
    character(*), parameter :: UnitName = "Unit"
    character(*), parameter :: StepName = "Step"
    character(*), parameter :: StepMinimumName = "StepMinimum"
    character(*), parameter :: StepMaximumName = "StepMaximum"
    character(*), parameter :: StartCalculationName = "StartCalculation"
    character(*), parameter :: EndCalculationName = "EndCalculation"
    character(*), parameter :: shouldDisplayPromptName = "shouldDisplayPrompt"
    !!------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: CalculationTypeName = "CalculationType"
    character(*), parameter :: ModelnumberName = "Modelnumber"
    character(*), parameter :: isFrozenName = "isFrozen"
    character(*), parameter :: PorosityName = "Porosity"
    character(*), parameter :: LatentHeatName = "LatentHeat"
    character(*), parameter :: DensityName = "Density"
    character(*), parameter :: SpecificHeatName = "SpecificHeat"
    character(*), parameter :: ThermalConductivityName = "ThermalConductivity"
    character(*), parameter :: DispersityName = "ThermalConductivityDispersity"
    !!------------------------------------------------------------------------------------------------------------------------------
    character(*), parameter :: calculationPeriodName = "calculationPeriod"

    character(*), parameter :: Phase1Name = "Phase1"
    character(*), parameter :: Phase2Name = "Phase2"
    character(*), parameter :: SoilName = "Soil"
    character(*), parameter :: WaterName = "Water"
    character(*), parameter :: IceName = "Ice"

    character(*), parameter :: xName = "x"
    character(*), parameter :: yName = "y"
    character(*), parameter :: zName = "z"
    character(*), parameter :: QiceTypeName = "QiceType"
    character(*), parameter :: TfName = "Tf"
    character(*), parameter :: ParametersName = "Parameters"
    character(*), parameter :: ModelName = "Model"
    character(*), parameter :: thetaSName = "thetaS"
    character(*), parameter :: thetaRName = "thetaR"
    character(*), parameter :: alpha1Name = "alpha1"
    character(*), parameter :: alpha2Name = "alpha2"
    character(*), parameter :: n1Name = "n1"
    character(*), parameter :: n2Name = "n2"
    character(*), parameter :: w1Name = "w1"
    character(*), parameter :: hcritName = "hcrit"
    character(*), parameter :: KTDynamicsName = "KTDynamics"
    character(*), parameter :: ImpedanceName = "Impedance"
    character(*), parameter :: KsName = "Ks"

    character(*), parameter :: AName = "a"
    character(*), parameter :: phiName = "phi"

    character(*), parameter :: useHCFName = "useHCF"
    character(*), parameter :: useImpedanceName = "useImpedance"
    character(*), parameter :: useKTDynamicsName = "useKTDynamics"
    character(*), parameter :: lName = "l"
    character(*), parameter :: OmegaName = "Omega"
    character(*), parameter :: TimeDiscretizationName = "TimeDiscretization"
    character(*), parameter :: OrderName = "Order"
    character(*), parameter :: MaxNonlinearIterationName = "MaxNonlinearIteration"
    character(*), parameter :: SolverName = "Solver"
    character(*), parameter :: PreconditionerName = "Preconditioner"
    character(*), parameter :: MaxIterationName = "MaxIteration"
    character(*), parameter :: ToleranceName = "Tolerance"
    character(*), parameter :: useSolverName = "useSolver"

    character(*), parameter :: BCName = "Boundaryconditions"
    character(*), parameter :: ICName = "Initialconditions"
    character(*), parameter :: ConstantName = "Constant"
    character(*), parameter :: LaplaceName = "Laplace"

    character(*), parameter :: GroupName = "Groups"
    character(*), parameter :: TypeName = "Type"
    character(*), parameter :: ValueName = "Value"
    character(*), parameter :: DirichletName = "Dirichlet"
    character(*), parameter :: AdiabaticName = "Adiabatic"
    character(*), parameter :: ImpermeableName = "Impermeable"
    character(*), parameter :: FreeHeatTransferName = "FreeHeatTransfer"
    character(*), parameter :: NoneName = "None"
    character(*), parameter :: HeatTransferName = "HeatTransfer"
    character(*), parameter :: UniformName = "Uniform"

    !!-------------------------------------
    character(*), parameter :: FileOutputName = "FileOutput"
    character(*), parameter :: TimeSettingsName = "TimeSettings"
    character(*), parameter :: ObservationName = "Observation"
    character(*), parameter :: TotalNumberName = "TotalNumber"
    character(*), parameter :: CoordinatesName = "Coordinates"
    character(*), parameter :: OutputSettingsName = "OutputSettings"
    character(*), parameter :: KindsName = "Kinds"
    character(*), parameter :: TempName = "Temp"
    character(*), parameter :: SiName = "Si"
    character(*), parameter :: TCName = "TC"
    character(*), parameter :: CName = "C"
    character(*), parameter :: PresName = "Pres"
    character(*), parameter :: FluxName = "Flux"
    character(*), parameter :: KName = "K"
    !!-------------------------------------

    !! Positive NaN
    real(real64), parameter :: NaNValue = transfer(Z'7FF8000000000000', 0.0_real64)

    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_simulation_settings
        character(:), allocatable :: title
        integer(int32) :: calculate_type
        integer(int32) :: calculate_dimension
    end type type_simulation_settings
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_analysis_controls
        logical :: calculate_thermal
        logical :: calculate_hydraulic
        logical :: calculate_mechanical
        character(:), allocatable :: coupling_mode
    end type type_analysis_controls
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_geometry_settings
        character(:), allocatable :: file_name
        character(:), allocatable :: cell_id_array_name
        character(:), allocatable :: integration_type
        real(real64) :: integration_points
    end type type_geometry_settings
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_materials_wrf
        integer(int32) :: model_number
        real(real64) :: theta_s
        real(real64) :: theta_r
        real(real64) :: alpha1
        real(real64) :: n1
        real(real64) :: m1
        real(real64) :: w1
        real(real64) :: h_crit
        real(real64) :: alpha2
        real(real64) :: n2
        real(real64) :: m2
        real(real64) :: w2
    end type type_materials_wrf

    type :: type_materials_gcc
        logical :: is_segregation
        character(:), allocatable :: unit
    end type type_materials_gcc

    type :: type_materials_phase_change
        real(real64) :: latent_heat
        real(real64) :: freezing_temperature
        type(type_materials_wrf) :: wrf
        type(type_materials_gcc) :: gcc
    end type type_materials_phase_change

    type :: type_materials_thermal
        real(real64), allocatable :: density(:)
        real(real64), allocatable :: specific_heat(:)
        real(real64), allocatable :: thermal_conductivity(:)
        real(real64), allocatable :: thermal_conductivity_dispersity(:)
        type(type_materials_phase_change) :: phase_change
    end type type_materials_thermal

    type, extends(type_materials_wrf) :: type_materials_hcf
        real(real64) :: l
    end type type_materials_hcf

    type :: type_materials_hydraulic
        integer(int32) :: model_number
        real(real64) :: impedance_factor
        real(real64) :: hydraulic_conductivity
        type(type_materials_hcf) :: hcf
        integer(int32) :: water_viscosity_model
    end type type_materials_hydraulic

    type :: type_material_settings
        integer(int32) :: id
        character(:), allocatable :: name
        integer(int32) :: phase
        logical :: is_frozen
        logical :: is_dispersed
        type(type_materials_thermal) :: thermal
        type(type_materials_hydraulic) :: hydraulic
    end type type_material_settings
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_convergence_criteria
        character(:), allocatable :: criteria
        character(:), allocatable :: logic
        real(real64) :: absolute_tolerance
        real(real64) :: relative_tolerance
    end type type_convergence_criteria

    type :: type_convergence
        character(:), allocatable :: use_criteria
        character(:), allocatable :: use_logic
        type(type_convergence_criteria) :: residual
        type(type_convergence_criteria) :: update
    end type type_convergence

    type :: type_nonlinear_solver
        character(:), allocatable :: method
        integer(int32) :: update_frequency
        integer(int32) :: max_iterations
        type(type_convergence) :: convergence
    end type type_nonlinear_solver

    type :: type_linear_solver_iterative
        integer(int32) :: solver_type
        integer(int32) :: preconditioner_type
        integer(int32) :: max_iterations
        real(real64) :: tolerance
    end type type_linear_solver_iterative

    type :: type_linear_solver_settings
        character(:), allocatable :: method
        type(type_linear_solver_iterative) :: iterative_solver
    end type type_linear_solver_settings

    type :: type_linear_solver
        type(type_linear_solver_settings) :: thermal
        type(type_linear_solver_settings) :: hydraulic
        type(type_linear_solver_settings) :: mechanical
    end type type_linear_solver

    type :: type_parallel_threads
        logical :: is_parallel
        integer(int32) :: num_threads
        character(:), allocatable :: schedule
        logical :: dynamic_adjustment
        logical :: nested_parallelism
        integer(int32) :: max_active_levels
    end type type_parallel_threads

    type :: type_parallel_settings
        type(type_parallel_threads) :: threads
    end type type_parallel_settings

    type :: type_solver_settings
        integer(int32) :: bdf_order
        character(:), allocatable :: reordering
        character(:), allocatable :: coloring
        type(type_nonlinear_solver) :: nonlinear_solver
        type(type_linear_solver) :: linear_solver
        type(type_parallel_settings) :: parallel_settings
    end type
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: input_basic
        type(type_simulation_settings) :: simulation_settings
        type(type_analysis_controls) :: analysis_controls
        type(type_geometry_settings) :: geometry_settings
        integer(int32) :: num_materials
        type(type_material_settings), allocatable :: materials(:)
        type(type_solver_settings) :: solver_settings

        !! ここから下は後で消す
        ! integer(int32) :: DimensionType
        ! integer(int32) :: numRegion
        ! character(:), allocatable :: Calculation_TimeUnit
        ! real(real64) :: Calculation_Step
        ! real(real64) :: Calculation_StepMinimum
        ! real(real64) :: Calculation_StepMaximum

        ! character(:), allocatable :: Input_TimeUnit
        ! real(real64) :: StartCalculation
        ! real(real64) :: EndCalculation

        ! logical(4) :: shouldDisplayPrompt
        ! integer(int32) :: Order
        ! integer(int32) :: MaxNonlinearIteration
    end type input_basic
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_time_control_simulation_period
        character(:), allocatable :: unit
        real(real64) :: start
        real(real64) :: end
    end type type_time_control_simulation_period

    type :: type_time_control_time_stepping
        character(:), allocatable :: unit
        real(real64) :: initial_step
        real(real64) :: min_step
        real(real64) :: max_step
    end type type_time_control_time_stepping

    type :: type_time_control
        type(type_time_control_simulation_period) :: simulation_period
        type(type_time_control_time_stepping) :: time_stepping
        real(real64), allocatable :: boundary_time_points(:)
    end type type_time_control
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_boundary_local
        character(:), allocatable :: type
        logical :: is_uniform
        real(real64), allocatable :: values(:)
    end type type_boundary_local

    type :: type_boundary_conditions
        integer(int32) :: id
        type(type_boundary_local) :: thermal
        type(type_boundary_local) :: hydraulic
    end type type_boundary_conditions
    !!------------------------------------------------------------------------------------------------------------------------------
    type, extends(type_boundary_local) :: type_boundary_local_initial
        integer(int32) :: id
    end type

    type :: type_initial_local
        character(:), allocatable :: type
        real(real64) :: value
        type(type_boundary_local_initial), allocatable :: boundary(:)
        character(:), allocatable :: field_name
    end type type_initial_local

    type :: type_initail_conditions
        type(type_initial_local) :: thermal
        type(type_initial_local) :: hydraulic
    end type type_initail_conditions
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_conditions
        type(type_time_control) :: time_control
        type(type_boundary_conditions), allocatable :: boundary_conditions(:)
        integer(int32) :: num_boundaries
        type(type_initail_conditions) :: initial_conditions
    end type type_conditions
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_field_output
        character(:), allocatable :: file_format
        logical :: coloring
        character(:), allocatable :: output_interval_unit
        real(real64) :: output_interval_step
        character(:), allocatable :: variable_names(:)
    end type type_field_output
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: types_history_output
        character(:), allocatable :: file_format
        character(:), allocatable :: observation_type
        character(:), allocatable :: output_interval_unit
        real(real64) :: output_interval_step
        character(:), allocatable :: variable_names(:)
        integer(int32) :: num_observations
        type(type_dp_vector_3d), allocatable :: coordinates(:)
        integer(int32), allocatable :: node_ids(:)
    end type types_history_output
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_standard_output
        logical :: print_progress
        character(:), allocatable :: print_progress_unit
        real(real64) :: print_progress_interval
    end type type_standard_output
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_output_settings
        type(type_field_output) :: field_output
        type(types_history_output) :: history_output
        type(type_standard_output) :: standard_output
    end type type_output_settings
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_geometry
        type(type_vtk) :: vtk
        character(:), allocatable :: point_data_names(:)
        real(real64), allocatable :: initial_values(:, :)
    end type type_geometry
    !!------------------------------------------------------------------------------------------------------------------------------

    ! type :: Input_OutputSettings
    !     character(:), allocatable :: FileFormat
    !     character(:), allocatable :: Output_TimeUnit
    !     character(:), allocatable :: Interval_TimeUnit
    !     real(real64) :: Interval_Step

    !     integer(int32) :: ObservationType
    !     integer(int32) :: NumObservation
    !     type(type_dp_3d) :: Cood_Obs
    !     integer(int32), allocatable :: ObsID(:)

    !     logical(4) :: outTemp
    !     logical(4) :: outSi
    !     logical(4) :: outTC
    !     logical(4) :: outC
    !     logical(4) :: outPres
    !     logical(4) :: outFlux
    !     logical(4) :: outK
    ! end type Input_OutputSettings

    ! type :: Input_Ice
    !     !***********************************************************************
    !     integer(int32) :: QiceType
    !     real(real64) :: Tf
    !     !***********************************************************************
    !     ! GCC optional parameters
    !     !***********************************************************************
    !     integer(int32) :: ModelType
    !     logical(4) :: isSegregation
    !     character(:), allocatable :: c_unit
    !     real(real64) :: thetaS
    !     real(real64) :: thetaR
    !     real(real64) :: alpha1
    !     real(real64) :: n1
    !     real(real64) :: w1
    !     real(real64) :: hcrit
    !     real(real64) :: alpha2
    !     real(real64) :: n2
    !     real(real64) :: rhoI
    !     !***********************************************************************
    !     ! EXP optional parameters
    !     !***********************************************************************
    !     real(real64) :: phi
    !     real(real64) :: a
    !     !***********************************************************************
    ! end type Input_Ice

    ! type :: Input_Thermal
    !     real(real64) :: Porosity
    !     real(real64) :: LatentHeat
    !     real(real64), allocatable :: Cp(:)
    !     real(real64), allocatable :: c(:)
    !     real(real64), allocatable :: rho(:)
    !     real(real64), allocatable :: lambda(:)
    !     real(real64), allocatable :: lambdaDispersity(:)
    ! end type Input_Thermal

    ! type :: Input_Flags
    !     logical(4) :: isHeat
    !     logical(4) :: isWater
    !     logical(4) :: isStress
    !     logical(4) :: is1Phase
    !     logical(4) :: is2Phase
    !     logical(4) :: is3Phase
    !     logical(4) :: is4Phase
    !     logical(4) :: isCompression
    !     logical(4) :: isFrostHeavePressure
    !     logical(4) :: isDispersity
    !     logical(4) :: isFrozen
    ! end type

    ! type :: Input_Solver
    !     !***********************************************************************
    !     integer(int32) :: useSolver
    !     !***********************************************************************
    !     integer(int32) :: useSolverType
    !     integer(int32) :: usePreconditionerType
    !     integer(int32) :: maxIteration
    !     real(real64) :: tolerance
    !     !***********************************************************************
    ! end type

    ! type :: Input_BC_Local
    !     character(:), allocatable :: type
    !     logical(4) :: isUniform
    !     real(real64), allocatable :: value(:)
    ! end type Input_BC_Local

    ! type :: Input_Boundary
    !     integer(int32), allocatable :: Groups(:)
    !     real(real64), allocatable :: Time(:)
    !     type(Input_BC_Local), allocatable :: Heat(:)
    !     type(Input_BC_Local), allocatable :: Water(:)
    ! end type Input_Boundary

    ! type :: Input_IC_Local
    !     character(:), allocatable :: type
    !     real(real64) :: value
    !     type(Input_BC_Local) :: Laplace
    ! end type Input_IC_Local

    ! type :: Input_Initial
    !     type(Input_IC_Local) :: Heat
    ! end type Input_Initial

    ! type :: Input_Region
    !     integer(int32), allocatable :: BelongingSurface(:)
    !     integer(int32), allocatable :: BelongingEdge(:)
    !     integer(int32) :: CalculationType
    !     integer(int32) :: ModelNumber
    !     logical(4) :: isFrozen
    !     type(Input_Flags) :: Flag
    !     type(Input_Ice) :: Ice
    !     type(Input_Thermal) :: Thermal
    ! end type Input_Region

    type :: type_input
        character(:), allocatable :: project_path
        character(:), allocatable :: basic_file_name
        character(:), allocatable :: conditions_file_name
        character(:), allocatable :: geometry_file_name
        character(:), allocatable :: output_file_name

        type(input_basic) :: basic
        type(type_conditions) :: conditions
        type(type_output_settings) :: output_settings
        type(type_geometry) :: geometry

        ! type(Input_Region), allocatable :: Regions(:)
        ! type(Input_Solver) :: Solver_Thermal
        ! type(Input_Solver) :: Solver_Hydraulic
        ! type(Input_Boundary) :: BC
        ! type(Input_Initial) :: IC
        ! type(Input_OutputSettings) :: OutputSettings

        ! type(type_vtk) :: vtk

    contains
        procedure, pass(self), public :: initialize => type_input_initialize

        procedure :: read_parameters => inout_read_basic_parameters
        procedure :: read_conditions => inout_read_conditions
        procedure :: read_output_settings => inout_read_output_settings
        procedure :: read_geometry => inout_read_geometry

    end type type_input

    interface
        module subroutine inout_read_basic_parameters(self)
            implicit none
            class(type_input), intent(inout) :: self

        end subroutine inout_read_basic_parameters

        module subroutine inout_read_conditions(self)
            implicit none
            class(type_input), intent(inout) :: self

        end subroutine inout_read_conditions

        module subroutine inout_read_output_settings(self)
            implicit none
            class(type_input), intent(inout) :: self

        end subroutine inout_read_output_settings

        module subroutine inout_read_geometry(self)
            implicit none
            class(type_input), intent(inout) :: self

        end subroutine inout_read_geometry

    end interface

contains

    subroutine type_input_initialize(self)
        implicit none
        class(type_input), intent(inout) :: self

        logical :: exists ! File existence status

        ! Path settings
        self%project_path = trim(adjustl(get_project_path()))

        inquire (directory=self%project_path//"Input/", exist=exists)
        if (.not. exists) call error_message(901)

        self%basic_file_name = self%project_path//"Input/Basic.json"
        self%conditions_file_name = self%project_path//"Input/Conditions.json"
        self%output_file_name = self%project_path//"Input/Output.json"

        ! Check the existence of the file
        inquire (file=self%basic_file_name, exist=exists)
        if (.not. exists) call error_message(902, c_opt=self%basic_file_name)

        inquire (file=self%conditions_file_name, exist=exists)
        if (.not. exists) call error_message(902, c_opt=self%conditions_file_name)

        inquire (file=self%output_file_name, exist=exists)
        if (.not. exists) call error_message(902, c_opt=self%output_file_name)

        call self%read_parameters()
        call self%read_conditions()
        call self%read_output_settings()
        call self%read_geometry()
    end subroutine type_input_initialize

end module inout_input

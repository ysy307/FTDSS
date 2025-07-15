module inout_input
    use, intrinsic :: iso_fortran_env, only: int32, real64, output_unit
!$  use :: omp_lib
    use :: stdlib_strings, only:to_string
    use :: stdlib_logger
    use :: json_module, only:json_file
    use :: inout_project_settings, only:get_project_path
    use :: module_core, only:type_vtk, type_dp_3d, allocate_array, deallocate_array, error_message, join, value_in_range
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

    !-------------------------------------------------------------------------------
    type :: type_simulation_settings
        character(:), allocatable :: title
        integer(int32) :: calculate_type
        integer(int32) :: calculate_dimension
    end type type_simulation_settings
    !-------------------------------------------------------------------------------
    type :: type_analysis_controls
        logical :: calculate_thermal
        logical :: calculate_hydraulic
        logical :: calculate_mechanical
        character(:), allocatable :: coupling_mode
    end type type_analysis_controls
    !-------------------------------------------------------------------------------
    type :: type_geometry_settings
        character(:), allocatable :: file_name
        character(:), allocatable :: cell_id_array_name
        character(:), allocatable :: integration_type
        real(real64) :: integration_points
    end type type_geometry_settings
    !-------------------------------------------------------------------------------
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
    !-------------------------------------------------------------------------------
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
    !-------------------------------------------------------------------------------

    type :: Input_OutputSettings
        character(:), allocatable :: FileFormat
        character(:), allocatable :: Output_TimeUnit
        character(:), allocatable :: Interval_TimeUnit
        real(real64) :: Interval_Step

        integer(int32) :: ObservationType
        integer(int32) :: NumObservation
        type(type_dp_3d) :: Cood_Obs
        integer(int32), allocatable :: ObsID(:)

        logical(4) :: outTemp
        logical(4) :: outSi
        logical(4) :: outTC
        logical(4) :: outC
        logical(4) :: outPres
        logical(4) :: outFlux
        logical(4) :: outK
    end type Input_OutputSettings

    type :: input_basic
        type(type_simulation_settings) :: simulation_settings
        type(type_analysis_controls) :: analysis_controls
        type(type_geometry_settings) :: geometry_settings
        integer(int32) :: num_materials
        type(type_material_settings), allocatable :: materials(:)
        type(type_solver_settings) :: solver_settings

        integer(int32) :: DimensionType
        integer(int32) :: numRegion
        character(:), allocatable :: Calculation_TimeUnit
        real(real64) :: Calculation_Step
        real(real64) :: Calculation_StepMinimum
        real(real64) :: Calculation_StepMaximum

        character(:), allocatable :: Input_TimeUnit
        real(real64) :: StartCalculation
        real(real64) :: EndCalculation

        logical(4) :: shouldDisplayPrompt
        integer(int32) :: Order
        integer(int32) :: MaxNonlinearIteration
    end type input_basic

    type :: Input_Ice
        !***********************************************************************
        integer(int32) :: QiceType
        real(real64) :: Tf
        !***********************************************************************
        ! GCC optional parameters
        !***********************************************************************
        integer(int32) :: ModelType
        logical(4) :: isSegregation
        character(:), allocatable :: c_unit
        real(real64) :: thetaS
        real(real64) :: thetaR
        real(real64) :: alpha1
        real(real64) :: n1
        real(real64) :: w1
        real(real64) :: hcrit
        real(real64) :: alpha2
        real(real64) :: n2
        real(real64) :: rhoI
        !***********************************************************************
        ! EXP optional parameters
        !***********************************************************************
        real(real64) :: phi
        real(real64) :: a
        !***********************************************************************
    end type Input_Ice

    type :: Input_Thermal
        real(real64) :: Porosity
        real(real64) :: LatentHeat
        real(real64), allocatable :: Cp(:)
        real(real64), allocatable :: c(:)
        real(real64), allocatable :: rho(:)
        real(real64), allocatable :: lambda(:)
        real(real64), allocatable :: lambdaDispersity(:)
    end type Input_Thermal

    type :: Input_Flags
        logical(4) :: isHeat
        logical(4) :: isWater
        logical(4) :: isStress
        logical(4) :: is1Phase
        logical(4) :: is2Phase
        logical(4) :: is3Phase
        logical(4) :: is4Phase
        logical(4) :: isCompression
        logical(4) :: isFrostHeavePressure
        logical(4) :: isDispersity
        logical(4) :: isFrozen
    end type

    type :: Input_Solver
        !***********************************************************************
        integer(int32) :: useSolver
        !***********************************************************************
        integer(int32) :: useSolverType
        integer(int32) :: usePreconditionerType
        integer(int32) :: maxIteration
        real(real64) :: tolerance
        !***********************************************************************
    end type

    type :: Input_BC_Local
        character(:), allocatable :: type
        logical(4) :: isUniform
        real(real64), allocatable :: value(:)
    end type Input_BC_Local

    type :: Input_Boundary
        integer(int32), allocatable :: Groups(:)
        real(real64), allocatable :: Time(:)
        type(Input_BC_Local), allocatable :: Heat(:)
        type(Input_BC_Local), allocatable :: Water(:)
    end type Input_Boundary

    type :: Input_IC_Local
        character(:), allocatable :: type
        real(real64) :: value
        type(Input_BC_Local) :: Laplace
    end type Input_IC_Local

    type :: Input_Initial
        type(Input_IC_Local) :: Heat
    end type Input_Initial

    type :: Input_Region
        integer(int32), allocatable :: BelongingSurface(:)
        integer(int32), allocatable :: BelongingEdge(:)
        integer(int32) :: CalculationType
        integer(int32) :: ModelNumber
        logical(4) :: isFrozen
        type(Input_Flags) :: Flag
        type(Input_Ice) :: Ice
        type(Input_Thermal) :: Thermal
    end type Input_Region

    type :: type_input
        character(:), allocatable :: project_path
        character(:), allocatable :: basic_file_name
        character(:), allocatable :: conditions_file_name
        character(:), allocatable :: geometry_file_name
        character(:), allocatable :: output_file_name

        type(input_basic) :: basic
        type(Input_Region), allocatable :: Regions(:)
        type(Input_Solver) :: Solver_Thermal
        type(Input_Solver) :: Solver_Hydraulic
        type(type_vtk) :: vtk
        type(Input_Boundary) :: conditions
        type(Input_Initial) :: IC
        type(Input_OutputSettings) :: OutputSettings

    contains
        procedure, pass(self), public :: initialize => type_input_initialize

        procedure :: read_parameters => inout_input_basic_parameters
        procedure :: Input_Geometry => inout_input_geometry_VTK
        procedure :: Input_conditions => inout_input_conditions_JSON
        procedure :: Input_OutputSettings => inout_input_OutputSettings_JSON

    end type type_input

    interface
        module subroutine inout_input_basic_parameters(self)
            !< Load the input parameters from the JSON file
            implicit none
            class(type_input), intent(inout) :: self

        end subroutine inout_input_basic_parameters

    end interface

    interface type_input
        module procedure :: type_input_initialize
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
        self%conditions_file_name = self%project_path//"Input/conditions.json"
        self%output_file_name = self%project_path//"Input/Output.json"

        ! Check the existence of the file
        inquire (file=self%basic_file_name, exist=exists)
        if (.not. exists) call error_message(902, c_opt=self%basic_file_name)

        inquire (file=self%conditions_file_name, exist=exists)
        if (.not. exists) call error_message(902, c_opt=self%conditions_file_name)

        inquire (file=self%output_file_name, exist=exists)
        if (.not. exists) call error_message(902, c_opt=self%output_file_name)

        call self%read_parameters()
        call self%Input_conditions()
        call self%Input_OutputSettings()
        call self%Input_Geometry()
    end subroutine type_input_initialize

    ! subroutine inout_read_parameters_JSON(self)
    !     !< Load the input parameters from the JSON file
    !     implicit none
    !     class(type_input) :: self
    !     type(json_file) :: json
    !     integer(int32) :: status, unit_num
    !     integer(int32) :: iRegion

    !     call json%initialize()

    !     call json%load(filename=self%basic_file_name)
    !     call json%print_error_message(output_unit)

    !     call inout_read_parameters_JSON_Basic(self, json)
    !     if (.not. allocated(self%Regions)) allocate (self%Regions(self%Basic%numRegion))
    !     do iRegion = 1, self%Basic%numRegion
    !         call inout_read_parameters_JSON_Reigion_Infomation(self, json, iRegion)
    !         if (self%Regions(iRegion)%Flag%isHeat) then
    !             call inout_read_parameters_JSON_Thermal(self, json, iRegion)
    !         end if
    !         !     if (self%Regions(iRegion)%Flags%isWater) then
    !         !         call inout_read_parameters_JSON_Hydraulic(self, json, iRegion)
    !         !     end if
    !     end do
    !     call inout_read_parameters_JSON_Solver(self, json)

    !     call json%destroy()
    !     call json%print_error_message(output_unit)
    ! end subroutine inout_read_parameters_JSON

    ! subroutine inout_read_parameters_JSON_Basic(self, json)
    !     !> Load the basic input parameters from the JSON file
    !     implicit none
    !     class(type_input) :: self
    !     type(json_file), intent(inout) :: json !! JSON parser
    !     character(:), allocatable :: key

    !     key = Connect_dot(BasicName, DimensionName)
    !     call json%get(key, self%Basic%DimensionType)
    !     call json%print_error_message(output_unit)

    !     key = Connect_dot(BasicName, RegionName)
    !     call json%get(key, self%Basic%numRegion)
    !     call json%print_error_message(output_unit)

    !     key = Connect_dot(BasicName, TimeName, CalculationName, UnitName)
    !     call json%get(key, self%Basic%Calculation_TimeUnit)
    !     call json%print_error_message(output_unit)

    !     key = Connect_dot(BasicName, TimeName, CalculationName, StepName)
    !     call json%get(key, self%Basic%Calculation_Step)
    !     call json%print_error_message(output_unit)

    !     key = Connect_dot(BasicName, TimeName, CalculationName, StepMinimumName)
    !     call json%get(key, self%Basic%Calculation_StepMinimum)
    !     call json%print_error_message(output_unit)

    !     key = Connect_dot(BasicName, TimeName, CalculationName, StepMaximumName)
    !     call json%get(key, self%Basic%Calculation_StepMaximum)
    !     call json%print_error_message(output_unit)

    !     key = Connect_dot(BasicName, TimeName, InputName, UnitName)
    !     call json%get(key, self%Basic%Input_TimeUnit)
    !     call json%print_error_message(output_unit)

    !     key = Connect_dot(BasicName, TimeName, InputName, StartCalculationName)
    !     call json%get(key, self%Basic%StartCalculation)
    !     call json%print_error_message(output_unit)

    !     key = Connect_dot(BasicName, TimeName, InputName, EndCalculationName)
    !     call json%get(key, self%Basic%EndCalculation)
    !     call json%print_error_message(output_unit)

    !     key = Connect_dot(BasicName, shouldDisplayPromptName)
    !     call json%get(key, self%Basic%shouldDisplayPrompt)
    !     call json%print_error_message(output_unit)

    ! end subroutine inout_read_parameters_JSON_Basic

    subroutine inout_read_parameters_JSON_Reigion_Infomation(self, json, iRegion)
        !> load the region information from the JSON file
        implicit none
        class(type_input) :: self
        type(json_file), intent(inout) :: json !! JSON parser
        integer(int32), intent(in) :: iRegion !! Region number

        !     character(8) :: region_name
        !     character(:), allocatable :: key

        !     write (region_name, '(a, i0)') RegionName, iRegion

        !     key = Connect_dot(region_name, BelongName, SurfaceName)
        !     call json%get(key, self%Regions(iRegion)%BelongingSurface)
        !     call json%print_error_message(output_unit)

        !     key = Connect_dot(region_name, BelongName, EdgeName)
        !     call json%get(key, self%Regions(iRegion)%BelongingEdge)
        !     call json%print_error_message(output_unit)

        !     key = Connect_dot(region_name, CalculationTypeName)
        !     call json%get(key, self%Regions(iRegion)%CalculationType)
        !     call json%print_error_message(output_unit)

        !     key = Connect_dot(region_name, ModelnumberName)
        !     call json%get(key, self%Regions(iRegion)%ModelNumber)
        !     call json%print_error_message(output_unit)

        !     select case (self%Regions(iRegion)%CalculationType)
        !     case (1)
        !         call inout_read_parameters_JSON_SetCalculationTypes(self, iRegion, .false., .false., .true.)
        !     case (2)
        !         call inout_read_parameters_JSON_SetCalculationTypes(self, iRegion, .false., .true., .false.)
        !     case (3)
        !         call inout_read_parameters_JSON_SetCalculationTypes(self, iRegion, .false., .true., .true.)
        !     case (4)
        !         call inout_read_parameters_JSON_SetCalculationTypes(self, iRegion, .true., .false., .false.)
        !     case (5)
        !         call inout_read_parameters_JSON_SetCalculationTypes(self, iRegion, .true., .false., .true.)
        !     case (6)
        !         call inout_read_parameters_JSON_SetCalculationTypes(self, iRegion, .true., .true., .false.)
        !     case (7)
        !         call inout_read_parameters_JSON_SetCalculationTypes(self, iRegion, .true., .true., .true.)
        !     case default
        !         call error_message(903, copt1=CalculationTypeName)
        !     end select

        !     select case (self%Regions(iRegion)%Modelnumber)
        !     case (10)
        !         call inout_read_parameters_JSON_SetFlags(self, iRegion, .true., .false., .false., .false.)
        !     case (20)
        !         call inout_read_parameters_JSON_SetFlags(self, iRegion, .false., .true., .false., .false.)
        !     case (31)
        !         call inout_read_parameters_JSON_SetFlags(self, iRegion, .false., .false., .true., .false., .false., .false., .false.)
        !     case (32)
        !         call inout_read_parameters_JSON_SetFlags(self, iRegion, .false., .false., .true., .false., .false., .false., .true.)
        !     case (33)
        !         call inout_read_parameters_JSON_SetFlags(self, iRegion, .false., .false., .true., .false., .false., .true., .false.)
        !     case (34)
        !         call inout_read_parameters_JSON_SetFlags(self, iRegion, .false., .false., .true., .false., .false., .true., .true.)
        !     case (35)
        !         call inout_read_parameters_JSON_SetFlags(self, iRegion, .false., .false., .true., .false., .true., .false., .false.)
        !     case (36)
        !         call inout_read_parameters_JSON_SetFlags(self, iRegion, .false., .false., .true., .false., .true., .false., .true.)
        !     case (37)
        !         call inout_read_parameters_JSON_SetFlags(self, iRegion, .false., .false., .true., .false., .true., .true., .false.)
        !     case (38)
        !         call inout_read_parameters_JSON_SetFlags(self, iRegion, .false., .false., .true., .false., .true., .true., .true.)
        !     case (41)
        !         call inout_read_parameters_JSON_SetFlags(self, iRegion, .false., .false., .false., .true., .false., .false., .false.)
        !     case (42)
        !         call inout_read_parameters_JSON_SetFlags(self, iRegion, .false., .false., .false., .true., .false., .false., .true.)
        !     case (43)
        !         call inout_read_parameters_JSON_SetFlags(self, iRegion, .false., .false., .false., .true., .false., .true., .false.)
        !     case (44)
        !         call inout_read_parameters_JSON_SetFlags(self, iRegion, .false., .false., .false., .true., .false., .true., .true.)
        !     case (45)
        !         call inout_read_parameters_JSON_SetFlags(self, iRegion, .false., .false., .false., .true., .true., .false., .false.)
        !     case (46)
        !         call inout_read_parameters_JSON_SetFlags(self, iRegion, .false., .false., .false., .true., .true., .false., .true.)
        !     case (47)
        !         call inout_read_parameters_JSON_SetFlags(self, iRegion, .false., .false., .false., .true., .true., .true., .false.)
        !     case (48)
        !         call inout_read_parameters_JSON_SetFlags(self, iRegion, .false., .false., .false., .true., .true., .true., .true.)
        !     case default
        !         call error_message(903, copt1=ModelnumberName)
        !     end select

        !     key = Connect_dot(region_name, isFrozenName)
        !     call json%get(key, self%Regions(iRegion)%Flag%isFrozen)
        !     call json%print_error_message(output_unit)

    end subroutine inout_read_parameters_JSON_Reigion_Infomation

    subroutine inout_read_parameters_JSON_SetCalculationTypes(self, iRegion, isHeat, isWater, isStress)
        !> Set the calculation types
        implicit none
        class(type_input) :: self
        integer(int32), intent(in) :: iRegion !! Region number
        logical(4), intent(in) :: isHeat !! Heat calculation
        logical(4), intent(in) :: isWater !! Water calculation
        logical(4), intent(in) :: isStress !! Stress calculation

        self%Regions(iRegion)%Flag%isHeat = isHeat
        self%Regions(iRegion)%Flag%isWater = isWater
        self%Regions(iRegion)%Flag%isStress = isStress

    end subroutine inout_read_parameters_JSON_SetCalculationTypes

    subroutine inout_read_parameters_JSON_SetFlags(self, iRegion, is1Phase, is2Phase, is3Phase, is4Phase, isCompression, isFrostHeavePressure, isDispersity)
        !> Set the calculation flags
        implicit none
        class(type_input) :: self
        integer(int32), intent(in) :: iRegion !! Region number
        logical(4), intent(in) :: is1Phase !! 1 Phase calculation
        logical(4), intent(in) :: is2Phase !! 2 Phase calculation
        logical(4), intent(in) :: is3Phase !! 3 Phase calculation
        logical(4), intent(in) :: is4Phase !! 3 Phase calculation
        logical(4), intent(in), optional :: isCompression !! consideer the water/ice compression
        logical(4), intent(in), optional :: isFrostHeavePressure !! Frost heave pressure calculation
        logical(4), intent(in), optional :: isDispersity !! Thermalc onductivity dispersity calculation

        self%Regions(iRegion)%Flag%is1Phase = is1Phase
        self%Regions(iRegion)%Flag%is2Phase = is2Phase
        self%Regions(iRegion)%Flag%is3Phase = is3Phase
        if (present(isCompression)) self%Regions(iRegion)%Flag%isCompression = isCompression
        if (present(isFrostHeavePressure)) self%Regions(iRegion)%Flag%isFrostHeavePressure = isFrostHeavePressure
        if (present(isDispersity)) self%Regions(iRegion)%Flag%isDispersity = isDispersity

    end subroutine inout_read_parameters_JSON_SetFlags

    subroutine inout_read_parameters_JSON_Thermal(self, json, iRegion)
        !> Load the thermal parameters from the JSON file
        implicit none
        class(type_input) :: self
        type(json_file), intent(inout) :: json !! JSON parser
        integer(int32), intent(in) :: iRegion !! Region number

        character(8) :: region_name
        integer(int32) :: QiceType
        character(:), allocatable :: key

        ! write (region_name, '(a, i0)') RegionName, iRegion
        ! if (.not. self%Regions(iRegion)%Flag%is1Phase) then
        !     key = Connect_dot(region_name, ThermalName, PorosityName)
        !     call json%get(key, self%Regions(iRegion)%Thermal%Porosity)
        !     call json%print_error_message(output_unit)
        ! end if
        ! if (self%Regions(iRegion)%Flag%isFrozen) then
        !     key = Connect_dot(region_name, ThermalName, LatentHeatName)
        !     call json%get(key, self%Regions(iRegion)%Thermal%LatentHeat)
        !     call json%print_error_message(output_unit)
        ! end if

        ! key = Connect_dot(region_name, ThermalName, DensityName)
        ! call json%get(key, self%Regions(iRegion)%Thermal%rho)
        ! call json%print_error_message(output_unit)

        ! key = Connect_dot(region_name, ThermalName, SpecificHeatName)
        ! call json%get(key, self%Regions(iRegion)%Thermal%c)
        ! call json%print_error_message(output_unit)

        ! if (allocated(self%Regions(iRegion)%Thermal%c) .and. &
        !     allocated(self%Regions(iRegion)%Thermal%rho)) then
        !     allocate (self%Regions(iRegion)%Thermal%Cp, mold=self%Regions(iRegion)%Thermal%c)
        !     self%Regions(iRegion)%Thermal%Cp(:) = self%Regions(iRegion)%Thermal%c(:) * self%Regions(iRegion)%Thermal%rho(:)
        ! end if

        ! key = Connect_dot(region_name, ThermalName, ThermalConductivityName)
        ! call json%get(key, self%Regions(iRegion)%Thermal%lambda)
        ! call json%print_error_message(output_unit)

        ! if (self%Regions(iRegion)%Flag%isDispersity) then
        !     key = Connect_dot(region_name, ThermalName, DispersityName)
        !     call json%get(key, self%Regions(iRegion)%Thermal%lambdaDispersity)
        !     call json%print_error_message(output_unit)
        ! end if

        ! if (self%Regions(iRegion)%Flag%isFrozen) then
        !     key = Connect_dot(region_name, ThermalName, IceName, QiceTypeName)
        !     call json%get(key, self%Regions(iRegion)%Ice%QiceType)
        !     call json%print_error_message(output_unit)

        !     key = Connect_dot(region_name, ThermalName, IceName, TfName)
        !     call json%get(key, self%Regions(iRegion)%Ice%Tf)
        !     call json%print_error_message(output_unit)

        !     if (self%Regions(iRegion)%Ice%QiceType == 2) then
        !         !! GCC model

        !         key = Connect_dot(region_name, ThermalName, IceName, ParametersName, ModelName)
        !         call json%get(key, self%Regions(iRegion)%Ice%ModelType)
        !         call json%print_error_message(output_unit)

        !         key = Connect_dot(region_name, ThermalName, IceName, ParametersName, thetaSName)
        !         call json%get(key, self%Regions(iRegion)%Ice%thetaS)
        !         call json%print_error_message(output_unit)

        !         key = Connect_dot(region_name, ThermalName, IceName, ParametersName, thetaRName)
        !         call json%get(key, self%Regions(iRegion)%Ice%thetaR)
        !         call json%print_error_message(output_unit)

        !         key = Connect_dot(region_name, ThermalName, IceName, ParametersName, alpha1Name)
        !         call json%get(key, self%Regions(iRegion)%Ice%alpha1)
        !         call json%print_error_message(output_unit)

        !         key = Connect_dot(region_name, ThermalName, IceName, ParametersName, n1Name)
        !         call json%get(key, self%Regions(iRegion)%Ice%n1)
        !         call json%print_error_message(output_unit)

        !         select case (self%Regions(iRegion)%Ice%ModelType)
        !         case (4)
        !             key = Connect_dot(region_name, ThermalName, IceName, ParametersName, hcritName)
        !             call json%get(key, self%Regions(iRegion)%Ice%hcrit)
        !             call json%print_error_message(output_unit)
        !         case (5)
        !             key = Connect_dot(region_name, ThermalName, IceName, ParametersName, alpha2Name)
        !             call json%get(key, self%Regions(iRegion)%Ice%alpha2)
        !             call json%print_error_message(output_unit)

        !             key = Connect_dot(region_name, ThermalName, IceName, ParametersName, n2Name)
        !             call json%get(key, self%Regions(iRegion)%Ice%n2)
        !             call json%print_error_message(output_unit)

        !             key = Connect_dot(region_name, ThermalName, IceName, ParametersName, w1Name)
        !             call json%get(key, self%Regions(iRegion)%Ice%w1)
        !             call json%print_error_message(output_unit)
        !         case (6)
        !             key = Connect_dot(region_name, ThermalName, IceName, ParametersName, n2Name)
        !             call json%get(key, self%Regions(iRegion)%Ice%n2)
        !             call json%print_error_message(output_unit)

        !             key = Connect_dot(region_name, ThermalName, IceName, ParametersName, w1Name)
        !             call json%get(key, self%Regions(iRegion)%Ice%w1)
        !             call json%print_error_message(output_unit)
        !         end select

        !         key = Connect_dot(region_name, ThermalName, IceName, UnitName)
        !         call json%get(key, self%Regions(iRegion)%Ice%c_unit)
        !         call json%print_error_message(output_unit)

        !         self%Regions(iRegion)%Ice%isSegregation = self%Regions(iRegion)%Flag%isFrostHeavePressure
        !         if (self%Regions(iRegion)%Flag%is3Phase .or. self%Regions(iRegion)%Flag%is4Phase) then
        !             self%Regions(iRegion)%Ice%rhoI = self%Regions(iRegion)%Thermal%rho(3)
        !         end if

        !     else if (self%Regions(iRegion)%Ice%QiceType == 3) then
        !         !! EXP model
        !         key = Connect_dot(region_name, ThermalName, IceName, ParametersName, phiName)
        !         call json%get(key, self%Regions(iRegion)%Ice%phi)
        !         call json%print_error_message(output_unit)

        !         key = Connect_dot(region_name, ThermalName, IceName, ParametersName, aName)
        !         call json%get(key, self%Regions(iRegion)%Ice%a)
        !         call json%print_error_message(output_unit)
        !     end if
        ! end if
    end subroutine inout_read_parameters_JSON_Thermal

!     subroutine inout_read_parameters_JSON_Hydraulic(self, json, iRegion)
!         !> Load the hydraulic parameters from the JSON file
!         implicit none
!         class(Input) :: self
!         type(json_file), intent(inout) :: json !! JSON parser
!         integer(int32), intent(in) :: iRegion !! Region number

!         character(8) :: region_name
!         character(:), allocatable :: key

!         write (region_name, '(a, i0)') RegionName, iRegion

!         key = inout_input_Connect_dot(region_name, HydraulicName, useHCFName)
!         call json%get(key, self%Regions(iRegion)%Hydraulic%useHCF)
!         call json%print_error_message(output_unit)

!         key = inout_input_Connect_dot(region_name, HydraulicName, useImpedanceName)
!         call json%get(key, self%Regions(iRegion)%Hydraulic%useImpedance)
!         call json%print_error_message(output_unit)

!         key = inout_input_Connect_dot(region_name, HydraulicName, useKTDynamicsName)
!         call json%get(key, self%Regions(iRegion)%Hydraulic%useKTDynamics)
!         call json%print_error_message(output_unit)

!         call Allocate_Structure_Hydraulic_Type(self%Regions(iRegion)%Hydraulic)

!         key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, KsName)
!         call json%get(key, self%Regions(iRegion)%Hydraulic%Ks)
!         call json%print_error_message(output_unit)

!         if (allocated(self%Regions(iRegion)%Hydraulic%HCF)) then
!             select type (HCF => self%Regions(iRegion)%Hydraulic%HCF)
!             type is (Type_HCF_BC)
!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, thetaSName)
!                 call json%get(key, HCF%thetaS)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, thetaRName)
!                 call json%get(key, HCF%thetaR)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, alpha1Name)
!                 call json%get(key, HCF%alpha1)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, n1Name)
!                 call json%get(key, HCF%n1)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, lName)
!                 call json%get(key, HCF%l)
!                 call json%print_error_message(output_unit)

!             type is (Type_HCF_VG)
!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, thetaSName)
!                 call json%get(key, HCF%thetaS)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, thetaRName)
!                 call json%get(key, HCF%thetaR)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, alpha1Name)
!                 call json%get(key, HCF%alpha1)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, n1Name)
!                 call json%get(key, HCF%n1)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, lName)
!                 call json%get(key, HCF%l)
!                 call json%print_error_message(output_unit)

!                 HCF%m1 = 1.0 - 1.0 / HCF%n1

!             type is (Type_HCF_KO)
!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, thetaSName)
!                 call json%get(key, HCF%thetaS)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, thetaRName)
!                 call json%get(key, HCF%thetaR)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, alpha1Name)
!                 call json%get(key, HCF%alpha1)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, n1Name)
!                 call json%get(key, HCF%n1)
!                 call json%print_error_message(output_unit)

!             type is (Type_HCF_MVG)
!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, thetaSName)
!                 call json%get(key, HCF%thetaS)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, thetaRName)
!                 call json%get(key, HCF%thetaR)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, alpha1Name)
!                 call json%get(key, HCF%alpha1)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, n1Name)
!                 call json%get(key, HCF%n1)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, hcritName)
!                 call json%get(key, HCF%hcrit)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, lName)
!                 call json%get(key, HCF%l)
!                 call json%print_error_message(output_unit)

!                 HCF%m1 = 1.0 - 1.0 / HCF%n1

!             type is (Type_HCF_Durner)
!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, thetaSName)
!                 call json%get(key, HCF%thetaS)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, thetaRName)
!                 call json%get(key, HCF%thetaR)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, alpha1Name)
!                 call json%get(key, HCF%alpha1)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, n1Name)
!                 call json%get(key, HCF%n1)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, alpha2Name)
!                 call json%get(key, HCF%alpha2)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, n2Name)
!                 call json%get(key, HCF%n2)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, w1Name)
!                 call json%get(key, HCF%w1)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, lName)
!                 call json%get(key, HCF%l)
!                 call json%print_error_message(output_unit)

!                 HCF%m1 = 1.0 - 1.0 / HCF%n1
!                 HCF%m2 = 1.0 - 1.0 / HCF%n2
!                 HCF%w2 = 1.0 - HCF%w1

!             type is (Type_HCF_DVGCH)
!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, thetaSName)
!                 call json%get(key, HCF%thetaS)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, thetaRName)
!                 call json%get(key, HCF%thetaR)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, alpha1Name)
!                 call json%get(key, HCF%alpha1)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, n1Name)
!                 call json%get(key, HCF%n1)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, n2Name)
!                 call json%get(key, HCF%n2)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, w1Name)
!                 call json%get(key, HCF%w1)
!                 call json%print_error_message(output_unit)

!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, lName)
!                 call json%get(key, HCF%l)
!                 call json%print_error_message(output_unit)

!                 HCF%m1 = 1.0 - 1.0 / HCF%n1
!                 HCF%m2 = 1.0 - 1.0 / HCF%n2
!                 HCF%w2 = 1.0 - HCF%w1
!             end select
!         end if

!         if (allocated(self%Regions(iRegion)%Hydraulic%Impedance)) then
!             select type (Impedance => self%Regions(iRegion)%Hydraulic%Impedance)
!             type is (Type_Impedance)
!                 key = inout_input_Connect_dot(region_name, HydraulicName, ParametersName, OmegaName)
!                 call json%get(key, Impedance%Omega)
!                 call json%print_error_message(output_unit)
!             end select
!         end if

!     end subroutine inout_read_parameters_JSON_Hydraulic

    subroutine inout_read_parameters_JSON_Solver(self, json)
        !> load Solver settings from the JSON file
        implicit none
        class(type_input) :: self
        type(json_file), intent(inout) :: json !! JSON parser

        character(:), allocatable :: key

        ! if (any(self%Regions(:)%Flag%isHeat)) then
        !     key = Connect_dot(SolverName, OrderName)
        !     call json%get(key, self%Basic%Order)
        !     call json%print_error_message(output_unit)

        !     key = Connect_dot(SolverName, MaxNonlinearIterationName)
        !     call json%get(key, self%Basic%MaxNonlinearIteration)
        !     call json%print_error_message(output_unit)

        !     key = Connect_dot(SolverName, ThermalName, useSolverName)
        !     call json%get(key, self%Solver_Thermal%useSolver)
        !     call json%print_error_message(output_unit)

        !     if (self%Solver_Thermal%useSolver == 2) then
        !         key = Connect_dot(SolverName, ThermalName, ParametersName, SolverName)
        !         call json%get(key, self%Solver_Thermal%useSolverType)
        !         call json%print_error_message(output_unit)

        !         key = Connect_dot(SolverName, ThermalName, ParametersName, PreconditionerName)
        !         call json%get(key, self%Solver_Thermal%usePreconditionerType)
        !         call json%print_error_message(output_unit)

        !         key = Connect_dot(SolverName, ThermalName, ParametersName, MaxIterationName)
        !         call json%get(key, self%Solver_Thermal%maxIteration)
        !         call json%print_error_message(output_unit)

        !         key = Connect_dot(SolverName, ThermalName, ParametersName, ToleranceName)
        !         call json%get(key, self%Solver_Thermal%tolerance)
        !         call json%print_error_message(output_unit)
        !     end if

        !     if (.not. (self%Solver_Thermal%useSolver == 1 .or. &
        !                self%Solver_Thermal%useSolver == 2)) then
        !         call error_message(903, copt1=SolverName, copt2=ThermalName)
        !     end if

        ! end if
        ! if (any(self%Regions(:)%Flag%isWater)) then
        !     key = Connect_dot(SolveName, HydraulicName, useSolverName)
        !     call json%get(key, useSolver)
        !     call json%print_error_message(output_unit)

        !     call inout_read_parameters_JSON_Solver_Settings(self, json, useSolver, HydraulicName)
        ! end if

    end subroutine inout_read_parameters_JSON_Solver

    subroutine inout_input_conditions_JSON(self)
        !> Load the boundary/initial conditions from the JSON file
        implicit none
        class(type_input) :: self

        type(json_file) :: json
        character(:), allocatable :: key
        integer(int32) :: iRegion

        call json%initialize()
        call json%load(filename=self%conditions_file_name)
        call json%print_error_message(output_unit)

        call inout_input_conditions_JSON_BC(self, json)
        call inout_input_conditions_JSON_IC(self, json)

        call json%destroy()
        call json%print_error_message(output_unit)

    end subroutine inout_input_conditions_JSON

    subroutine inout_input_conditions_JSON_BC(self, json)
        !> Load the boundary conditions from the JSON file
        implicit none
        class(type_input) :: self
        type(json_file), intent(inout) :: json !! JSON parser

        character(:), allocatable :: key
        character(2) :: cBCGroup
        integer(int32) :: iBC
        integer(int32) :: minium, maximum

        integer(int32) :: numGroup
        integer(int32) :: iGroup

        ! key = Connect_dot(BCName, GroupName)
        ! call json%get(key, self%conditions%Groups)
        ! call json%print_error_message(output_unit)

        ! numGroup = size(self%conditions%Groups)
        ! minium = minval(self%conditions%Groups)
        ! maximum = maxval(self%conditions%Groups)
        ! allocate (self%conditions%Heat(minium:maximum))
        ! allocate (self%conditions%Water(minium:maximum))

        ! key = Connect_dot(BCName, TimeName)
        ! call json%get(key, self%conditions%Time)
        ! call json%print_error_message(output_unit)

        ! do iBC = 1, numGroup
        !     iGroup = self%conditions%Groups(iBC)
        !     write (cBCGroup, '(i0)') iGroup
        !     key = Connect_dot(BCName, cBCGroup, ThermalName, TypeName)
        !     call json%get(key, self%conditions%Heat(iGroup)%type)
        !     call json%print_error_message(output_unit)

        !     select case (self%conditions%Heat(iGroup)%type)
        !     case (DirichletName, HeatTransferName)
        !         key = Connect_dot(BCName, cBCGroup, ThermalName, UniformName)
        !         call json%get(key, self%conditions%Heat(iGroup)%isUniform)
        !         call json%print_error_message(output_unit)

        !         key = Connect_dot(BCName, cBCGroup, ThermalName, ValueName)
        !         call json%get(key, self%conditions%Heat(iGroup)%value)
        !         call json%print_error_message(output_unit)
        !     end select

        ! key = inout_input_Connect_dot(BCName, cBCGroup, HydsraulicName, TypeName)
        ! call json%get(key, self%conditions%BC_Hydraulic(iBC)%type)
        ! call json%print_error_message(output_unit)

        ! select case (self%conditions%BC_Hydraulic(iBC)%type)
        ! case (DirichletName, HeatTransferName)
        !     key = inout_input_Connect_dot(BCName, cBCGroup, HydraulicName, ValueName)
        !     call json%get(key, self%conditions%BC_Hydraulic(iBC)%value)
        !     call json%print_error_message(output_unit)
        ! case default
        !     self%conditions%BC_Hydraulic(iBC)%value = NaNValue
        ! end select
        ! end do

    end subroutine inout_input_conditions_JSON_BC

    subroutine inout_input_conditions_JSON_IC(self, json)
        !> Load the initialy conditions from the JSON file
        implicit none
        class(type_input) :: self
        type(json_file), intent(inout) :: json !! JSON parser

        character(:), allocatable :: key
        character(:), allocatable :: tmp

        character(2) :: cICGroup
        integer(int32) :: i, count
        logical(4) :: isFind

        ! key = Connect_Dot(ICName, ThermalName, TypeName)
        ! call json%get(key, self%IC%Heat%type)
        ! call json%print_error_message(output_unit)

        ! select case (self%IC%Heat%type)
        ! case (ConstantName)
        !     key = Connect_Dot(ICName, ThermalName, ValueName)
        !     call json%get(key, self%IC%Heat%value)
        !     call json%print_error_message(output_unit)
        ! case (LaplaceName)
        !     stop 'Laplace type is not supported yet, sorry'

        ! end select

        ! key = Connect_Dot(ICName, HydraulicName, TypeName)
        ! call json%get(key, self%conditions%IC_Hydraulic%type)
        ! call json%print_error_message(output_unit)

        ! select case (self%conditions%IC_Hydraulic%type)
        ! case (ConstantName)
        !     key = Connect_Dot(ICName, HydraulicName, ValueName)
        !     call json%get(key, self%conditions%IC_Hydraulic%value)
        !     call json%print_error_message(output_unit)
        ! case (LaplaceName)
        !     count = 0
        !     do i = 1, size(self%conditions%BCGroup)
        !         write (cICGroup, '(i0)') self%conditions%BCGroup(i)
        !         key = Connect_Dot(ICName, HydraulicName, ValueName, cICGroup, TypeName)
        !         call json%get(key, tmp, found=isFind)
        !         if (isFind) count = count + 1
        !     end do
        !     allocate (self%conditions%IC_Hydraulic%IC_BC(count))
        !     count = 0
        !     do i = 1, size(self%conditions%BCGroup)
        !         write (cICGroup, '(i0)') self%conditions%BCGroup(i)
        !         key = Connect_Dot(ICName, HydraulicName, ValueName, cICGroup, TypeName)
        !         call json%get(key, tmp, found=isFind)

        !         if (.not. isFind) cycle
        !         count = count + 1

        !         key = Connect_Dot(ICName, HydraulicName, ValueName, cICGroup, TypeName)
        !         call json%get(key, self%conditions%IC_Hydraulic%IC_BC(count)%type)
        !         call json%print_error_message(output_unit)

        !         key = Connect_Dot(ICName, HydraulicName, ValueName, cICGroup, ValueName)
        !         call json%get(key, self%conditions%IC_Hydraulic%IC_BC(count)%value)
        !         call json%print_error_message(output_unit)
        !     end do
        ! end select

    end subroutine inout_input_conditions_JSON_IC

    subroutine inout_input_geometry_VTK(self)
        !> Load the geometry from the VTK file
        implicit none
        class(type_input) :: self

        call self%vtk%initialize(self%geometry_file_name, self%basic%geometry_settings%cell_id_array_name)
    end subroutine inout_input_geometry_VTK

!     ! subroutine inout_input_Finalize(self)
!     !     implicit none
!     !     type(Input) :: self

!     !     if (allocated(self%Work_Region_Basic_Infomatin)) deallocate (self%Work_Region_Basic_Infomatin)
!     !     if (allocated(self%Work_Region_Paremeters_real64)) deallocate (self%Work_Region_Paremeters_real64)
!     !     if (allocated(self%Work_Region_Parameters_int32)) deallocate (self%Work_Region_Parameters_int32)
!     !     if (allocated(self%Work_Region_Parameters_Number)) deallocate (self%Work_Region_Parameters_Number)
!     !     if (allocated(self%Work_Coordinates)) deallocate (self%Work_Coordinates)
!     !     if (allocated(self%Work_Coordinates_Region)) deallocate (self%Work_Coordinates_Region)
!     !     if (allocated(self%Work_Top)) deallocate (self%Work_Top)
!     !     if (allocated(self%Work_NBC_Node)) deallocate (self%Work_NBC_Node)
!     !     if (allocated(self%Work_NBC_Node_Type)) deallocate (self%Work_NBC_Node_Type)
!     !     if (allocated(self%Work_NBC_Node_Value_Info)) deallocate (self%Work_NBC_Node_Value_Info)
!     !     if (allocated(self%Work_NBC_Node_Value)) deallocate (self%Work_NBC_Node_Value)
!     !     if (allocated(self%Work_EBC_Edge)) deallocate (self%Work_EBC_Edge)
!     !     if (allocated(self%Work_EBC_Edge_Type)) deallocate (self%Work_EBC_Edge_Type)
!     !     if (allocated(self%Work_EBC_Edge_Value_Info)) deallocate (self%Work_EBC_Edge_Value_Info)
!     !     if (allocated(self%Work_EBC_Edge_Value)) deallocate (self%Work_EBC_Edge_Value)
!     !     if (allocated(self%Work_IC_Type)) deallocate (self%Work_IC_Type)
!     !     if (allocated(self%Work_IC_Value)) deallocate (self%Work_IC_Value)
!     !     if (allocated(self%Work_Observation_Node)) deallocate (self%Work_Observation_Node)
!     !     if (allocated(self%Work_Observation_Coordinate)) deallocate (self%Work_Observation_Coordinate)
!     !     if (allocated(self%Work_Observation_Flag)) deallocate (self%Work_Observation_Flag)

!     ! end subroutine inout_input_Finalize

    subroutine inout_input_OutputSettings_JSON(self)
        implicit none
        class(type_input) :: self
        type(json_file) :: json
        integer(int32) :: status, unit_num
        integer(int32) :: iRegion
        character(:), allocatable :: key

        call json%initialize()

        call json%load(filename=self%output_file_name)
        call json%print_error_message(output_unit)

        ! key = trim(adjustl(FileOutputName))
        ! call json%get(key, self%OutputSettings%FileFormat)
        ! call json%print_error_message(output_unit)

        ! key = Connect_dot(TimeSettingsName, UnitName)
        ! call json%get(key, self%OutputSettings%Output_TimeUnit)
        ! call json%print_error_message(output_unit)

        ! key = Connect_dot(TimeSettingsName, IntervalName, UnitName)
        ! call json%get(key, self%OutputSettings%Interval_TimeUnit)
        ! call json%print_error_message(output_unit)

        ! key = Connect_dot(TimeSettingsName, IntervalName, StepName)
        ! call json%get(key, self%OutputSettings%Interval_Step)
        ! call json%print_error_message(output_unit)

        ! key = Connect_dot(ObservationName, TypeName)
        ! call json%get(key, self%OutputSettings%ObservationType)
        ! call json%print_error_message(output_unit)

        ! key = Connect_dot(ObservationName, TotalNumberName)
        ! call json%get(key, self%OutputSettings%NumObservation)
        ! call json%print_error_message(output_unit)

        ! select case (self%OutputSettings%ObservationType)
        ! case (1)
        !     key = Connect_dot(ObservationName, NodeName)
        !     call json%get(key, self%OutputSettings%ObsID)
        !     call json%print_error_message(output_unit)
        !     if (.not. size(self%OutputSettings%ObsID) == self%OutputSettings%NumObservation) then
        !         write (*, *) "dont match shapes"
        !         stop
        !     end if
        ! case (2)
        !     key = Connect_dot(ObservationName, CoordinatesName, xName)
        !     call json%get(key, self%OutputSettings%Cood_Obs%x)
        !     call json%print_error_message(output_unit)

        !     key = Connect_dot(ObservationName, CoordinatesName, yName)
        !     call json%get(key, self%OutputSettings%Cood_Obs%y)
        !     call json%print_error_message(output_unit)

        !     key = Connect_dot(ObservationName, CoordinatesName, zName)
        !     call json%get(key, self%OutputSettings%Cood_Obs%z)
        !     call json%print_error_message(output_unit)

        !     if (.not. size(self%OutputSettings%Cood_Obs%x) == self%OutputSettings%NumObservation .or. &
        !         .not. size(self%OutputSettings%Cood_Obs%y) == self%OutputSettings%NumObservation .or. &
        !         .not. size(self%OutputSettings%Cood_Obs%z) == self%OutputSettings%NumObservation) then
        !         write (*, *) "dont match shapes"
        !         stop
        !     end if
        ! end select

        ! key = Connect_dot(OutputSettingsName, KindsName, TempName)
        ! call json%get(key, self%OutputSettings%outTemp)
        ! call json%print_error_message(output_unit)

        ! key = Connect_dot(OutputSettingsName, KindsName, SiName)
        ! call json%get(key, self%OutputSettings%outSi)
        ! call json%print_error_message(output_unit)

        ! key = Connect_dot(OutputSettingsName, KindsName, TCName)
        ! call json%get(key, self%OutputSettings%outTC)
        ! call json%print_error_message(output_unit)

        ! key = Connect_dot(OutputSettingsName, KindsName, CName)
        ! call json%get(key, self%OutputSettings%outC)
        ! call json%print_error_message(output_unit)

        ! key = Connect_dot(OutputSettingsName, KindsName, PresName)
        ! call json%get(key, self%OutputSettings%outPres)
        ! call json%print_error_message(output_unit)

        ! key = Connect_dot(OutputSettingsName, KindsName, FluxName)
        ! call json%get(key, self%OutputSettings%outFlux)
        ! call json%print_error_message(output_unit)

        ! key = Connect_dot(OutputSettingsName, KindsName, KName)
        ! call json%get(key, self%OutputSettings%outK)
        ! call json%print_error_message(output_unit)

    end subroutine inout_input_OutputSettings_JSON

end module inout_input

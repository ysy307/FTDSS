module Inout_Input
    use, intrinsic :: iso_fortran_env, only: int32, real64, output_unit
    use :: Inout_SetProjectPath, only:GetProjectPath => Inout_SetProjectPath_GetProjectPath
    use :: error
    use :: Allocate_Allocate
    use :: Allocate_Structure, only:Allocate_Structure_Thermal_Type, Allocate_Structure_Ice_Type, Allocate_Structure_WRF_Type, Allocate_Structure_Hydraulic_Type
    use :: Types
    ! use :: tomlf
    use :: json_module, only:json_file
    use :: Inout_VTK
    implicit none
    private

    integer(int32), parameter :: min_calculation_type = 1, max_calculation_type = 7
    integer(int32), parameter :: min_model_type = 11, max_model_type = 18
    integer(int32), parameter :: min_Coordinate_Dimesion_type = 1, max_Coordinate_Dimesion_type = 3
    character(*), parameter :: BasicName = "Basic"
    character(*), parameter :: ThermalName = "Thermal"
    character(*), parameter :: HydraulicName = "Hydraulic"
    character(*), parameter :: ElementName = "Element"
    character(*), parameter :: NodeName = "Node"
    character(*), parameter :: ShapeName = "ShapeType"
    character(*), parameter :: DimensionName = "DimensionType"
    character(*), parameter :: RegionName = "Region"
    character(*), parameter :: BelongName = "Belong"
    character(*), parameter :: SurfaceName = "Surface"
    character(*), parameter :: EdgeName = "Edge"
    character(*), parameter :: CalculationName = "Calculation"
    character(*), parameter :: InputName = "Input"
    character(*), parameter :: OutputName = "Output"
    character(*), parameter :: IntervalName = "Interval"
    character(*), parameter :: timeUnitName = "timeUnit"
    character(*), parameter :: stepName = "step"
    character(*), parameter :: calculationPeriodName = "calculationPeriod"
    character(*), parameter :: isDisplayPromptName = "isDisplayPrompt"
    character(*), parameter :: FileOutputName = "FileOutput"

    character(*), parameter :: CalculationTypeName = "CalculationType"
    character(*), parameter :: ModelnumberName = "Modelnumber"
    character(*), parameter :: isFrozenName = "isFrozen"
    character(*), parameter :: PorosityName = "Porosity"
    character(*), parameter :: LatentHeatName = "LatentHeat"
    character(*), parameter :: Phase1Name = "Phase1"
    character(*), parameter :: Phase2Name = "Phase2"
    character(*), parameter :: SoilName = "Soil"
    character(*), parameter :: WaterName = "Water"
    character(*), parameter :: IceName = "Ice"
    character(*), parameter :: DensityName = "Density"
    character(*), parameter :: SpecificHeatName = "SpecificHeat"
    character(*), parameter :: ThermalConductivityName = "ThermalConductivity"
    character(*), parameter :: dispersityName = "dispersity"
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

    character(*), parameter :: useHCFName = "useHCF"
    character(*), parameter :: useImpedanceName = "useImpedance"
    character(*), parameter :: useKTDynamicsName = "useKTDynamics"
    character(*), parameter :: lName = "l"
    character(*), parameter :: OmegaName = "Omega"
    character(*), parameter :: TimeDiscretizationName = "TimeDiscretization"
    character(*), parameter :: SolveName = "Solve"
    character(*), parameter :: SolverName = "Solver"
    character(*), parameter :: PreconditionerName = "Preconditioner"
    character(*), parameter :: MaxIterationName = "MaxIteration"
    character(*), parameter :: ToleranceName = "Tolerance"
    character(*), parameter :: useSolverName = "useSolver"

    character(*), parameter :: BCName = "BoundaryConditions"
    character(*), parameter :: ICName = "InitialConditions"
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

    !! Positive NaN
    real(real64), parameter :: NaNValue = transfer(Z'7FF8000000000000', 0.0_real64)

#ifdef _MPI
    integer(int32), parameter :: root = 0
#endif

    public :: Input

    type :: Input
        private
        character(256) :: Basic_FileName, Conditions_FileName, Geometry_FileName
        ! character(256) :: Basic_FileName, COO_FileName, BC_FileName, Obs_FileName, ObsFlag_FileName, Top_FileName, IC_FileName

        ! Basic.in
        ! Basic section
        type(Basic_params) :: Basic
        type(Type_Region), allocatable :: Regions(:)
        type(Type_Solver) :: Solver
        type(Type_VTK) :: VTK
        type(Type_Conditions) :: Conditions

    contains

        procedure :: Input_Parameters => Inout_Input_Parameters_JSON
        procedure :: Input_Geometry => Inout_Input_Geometry_VTK
        procedure :: Input_Conditions => Inout_Input_Conditions_JSON

        procedure, pass :: Input_Get_Basic_Params => Inout_Input_Get_Basic_Params
        procedure, pass :: Input_Get_Regional_Params => Inout_Input_Get_Regional_Params
        procedure, pass :: Input_Get_Thermal_Params => Inout_Input_Get_Themal_Params
        procedure, pass :: Input_Get_BoundaryConditions => Inout_Input_Get_BoundaryConditions
        procedure, pass :: Input_Get_InitialConditions => Inout_Input_Get_InitialConditions
        procedure, pass :: Input_Get_DP3d => Inout_Input_Get_DP3d
        procedure, pass :: Input_Get_int32 => Inout_Input_Get_int32
        procedure, pass :: Input_Get_int32_rank1 => Inout_Input_Get_int32_rank1
        procedure, pass :: Input_Get_int32_rank2 => Inout_Input_Get_int32_rank2
        generic :: Get => Input_Get_Basic_Params, Input_Get_Regional_Params, Input_Get_int32, Input_Get_int32_rank1, Input_Get_int32_rank2, Input_Get_DP3d, Input_Get_BoundaryConditions, Input_Get_InitialConditions, Input_Get_Thermal_Params

        ! final :: Inout_Input_Finalize

    end type Input

    interface Input
        module procedure Input_Constructor
    end interface

    interface Inout_Input_Connect_dot
        procedure :: Inout_Input_Connect_dot_2
        procedure :: Inout_Input_Connect_dot_3
        procedure :: Inout_Input_Connect_dot_4
        procedure :: Inout_Input_Connect_dot_5
        procedure :: Inout_Input_Connect_dot_6
    end interface

contains

    type(Input) function Input_Constructor
        implicit none
        character(256) :: dir_Path ! Path to the project directory
        integer(int32) :: access ! File access status
        integer(int32) :: status ! File access status
        logical(4) :: exists ! File existence status
        character(256) :: access_mode

        ! Path settings
        dir_Path = GetProjectPath()

        inquire (DIRECTORY=trim(adjustl(dir_Path))//"Input/", exist=exists)

        Input_Constructor%Basic_FileName = trim(adjustl(dir_Path))//"Input/Basic.json"
        Input_Constructor%Conditions_FileName = trim(adjustl(dir_Path))//"Input/Conditions.json"
        Input_Constructor%Geometry_FileName = trim(adjustl(dir_Path))//"Input/Geometry.vtk"
        ! Input_Constructor%IC_FileName = trim(adjustl(dir_Path))//"Input/IC.in"
        ! Input_Constructor%Obs_FileName = trim(adjustl(dir_Path))//"Input/Obs.in"
        ! Input_Constructor%ObsFlag_FileName = trim(adjustl(dir_Path))//"Input/printobs.in"
        ! Input_Constructor%Top_FileName = trim(adjustl(dir_Path))//"Input/top.in"
        ! Input_Constructor%COO_FileName = trim(adjustl(dir_Path))//"Input/coordinate.in"

        ! Check the existence of the file
        inquire (file=Input_Constructor%Basic_FileName, exist=exists)
        if (.not. exists) call error_message(901, opt_file_name=Input_Constructor%Basic_FileName)

        inquire (file=Input_Constructor%Conditions_FileName, exist=exists)
        if (.not. exists) call error_message(901, opt_file_name=Input_Constructor%Conditions_FileName)

        inquire (file=Input_Constructor%Geometry_FileName, exist=exists)
        if (.not. exists) call error_message(901, opt_file_name=Input_Constructor%Geometry_FileName)

        call Input_Constructor%Input_Parameters()
        call Input_Constructor%Input_Geometry()
        call Input_Constructor%Input_Conditions()
        ! call Input_Constructor%Input_IC()
        ! call Input_Constructor%Input_Observation()
        ! call Input_Constructor%Input_Flags()

    end function Input_Constructor

    subroutine Inout_Input_Parameters_JSON(self)
        !< Load the input parameters from the JSON file
        implicit none
        class(Input) :: self
        type(json_file) :: json
        integer(int32) :: status, unit_num
        integer(int32) :: iRegion

        call json%initialize()

        call json%load(filename=self%Basic_FileName)
        call json%print_error_message(output_unit)

        call Inout_Input_Parameters_JSON_Basic(self, json)
        if (.not. allocated(self%Regions)) allocate (self%Regions(self%Basic%Region))
        do iRegion = 1, self%Basic%Region
            call Inout_Input_Parameters_JSON_Reigion_Infomation(self, json, iRegion)
            if (self%Regions(iRegion)%Flags%isHeat) then
                call Inout_Input_Parameters_JSON_Thermal(self, json, iRegion)
            end if
            if (self%Regions(iRegion)%Flags%isWater) then
                call Inout_Input_Parameters_JSON_Hydraulic(self, json, iRegion)
            end if
        end do
        call Inout_Input_Parameters_JSON_Solver(self, json)

        call json%destroy()
        call json%print_error_message(output_unit)
    end subroutine Inout_Input_Parameters_JSON

    subroutine Inout_Input_Parameters_JSON_Basic(self, json)
        !> Load the basic input parameters from the JSON file
        implicit none
        class(Input) :: self
        type(json_file), intent(inout) :: json !! JSON parser
        character(:), allocatable :: key

        key = Inout_Input_Connect_dot(BasicName, ElementName)
        call json%get(key, self%Basic%Element)
        call json%print_error_message(output_unit)

        key = Inout_Input_Connect_dot(BasicName, NodeName)
        call json%get(key, self%Basic%Node)
        call json%print_error_message(output_unit)

        key = Inout_Input_Connect_dot(BasicName, ShapeName)
        call json%get(key, self%Basic%ShapeType)
        call json%print_error_message(output_unit)

        key = Inout_Input_Connect_dot(BasicName, DimensionName)
        call json%get(key, self%Basic%DimensionType)
        call json%print_error_message(output_unit)

        key = Inout_Input_Connect_dot(BasicName, RegionName)
        call json%get(key, self%Basic%Region)
        call json%print_error_message(output_unit)

        key = Inout_Input_Connect_dot(BasicName, CalculationName, timeUnitName)
        call json%get(key, self%Basic%Calculation_timeUnit)
        call json%print_error_message(output_unit)

        key = Inout_Input_Connect_dot(BasicName, CalculationName, stepName)
        call json%get(key, self%Basic%Calculation_step)
        call json%print_error_message(output_unit)

        key = Inout_Input_Connect_dot(BasicName, InputName, timeUnitName)
        call json%get(key, self%Basic%Input_timeUnit)
        call json%print_error_message(output_unit)

        key = Inout_Input_Connect_dot(BasicName, InputName, calculationPeriodName)
        call json%get(key, self%Basic%CalculationPeriod)
        call json%print_error_message(output_unit)

        key = Inout_Input_Connect_dot(BasicName, OutputName, timeUnitName)
        call json%get(key, self%Basic%Output_timeUnit)
        call json%print_error_message(output_unit)

        key = Inout_Input_Connect_dot(BasicName, IntervalName, timeUnitName)
        call json%get(key, self%Basic%Interval_timeUnit)
        call json%print_error_message(output_unit)

        key = Inout_Input_Connect_dot(BasicName, IntervalName, stepName)
        call json%get(key, self%Basic%Interval)
        call json%print_error_message(output_unit)

        key = Inout_Input_Connect_dot(BasicName, isDisplayPromptName)
        call json%get(key, self%Basic%isDisplayPrompt)
        call json%print_error_message(output_unit)

        key = Inout_Input_Connect_dot(BasicName, FileOutputName)
        call json%get(key, self%Basic%FileOutput)
        call json%print_error_message(output_unit)

    end subroutine Inout_Input_Parameters_JSON_Basic

    subroutine Inout_Input_Parameters_JSON_Reigion_Infomation(self, json, iRegion)
        !> load the region information from the JSON file
        implicit none
        class(Input) :: self
        type(json_file), intent(inout) :: json !! JSON parser
        integer(int32), intent(in) :: iRegion !! Region number

        character(8) :: region_name
        character(:), allocatable :: key

        write (region_name, '(a, i0)') RegionName, iRegion

        key = Inout_Input_Connect_dot(region_name, BelongName, SurfaceName)
        call json%get(key, self%Regions(iRegion)%BelongingSurface)
        call json%print_error_message(output_unit)

        key = Inout_Input_Connect_dot(region_name, BelongName, EdgeName)
        call json%get(key, self%Regions(iRegion)%BelongingEdge)
        call json%print_error_message(output_unit)

        key = Inout_Input_Connect_dot(region_name, CalculationTypeName)
        call json%get(key, self%Regions(iRegion)%CalculationType)
        call json%print_error_message(output_unit)

        key = Inout_Input_Connect_dot(region_name, ModelnumberName)
        call json%get(key, self%Regions(iRegion)%Modelnumber)
        call json%print_error_message(output_unit)

        select case (self%Regions(iRegion)%CalculationType)
        case (1)
            call Inout_Input_Parameters_JSON_SetCalculationTypes(self, iRegion, .false., .false., .true.)
        case (2)
            call Inout_Input_Parameters_JSON_SetCalculationTypes(self, iRegion, .false., .true., .false.)
        case (3)
            call Inout_Input_Parameters_JSON_SetCalculationTypes(self, iRegion, .false., .true., .true.)
        case (4)
            call Inout_Input_Parameters_JSON_SetCalculationTypes(self, iRegion, .true., .false., .false.)
        case (5)
            call Inout_Input_Parameters_JSON_SetCalculationTypes(self, iRegion, .true., .false., .true.)
        case (6)
            call Inout_Input_Parameters_JSON_SetCalculationTypes(self, iRegion, .true., .true., .false.)
        case (7)
            call Inout_Input_Parameters_JSON_SetCalculationTypes(self, iRegion, .true., .true., .true.)
        case default
            call error_message(903, copt1=CalculationTypeName)
        end select

        select case (self%Regions(iRegion)%Modelnumber)
        case (10)
            call Inout_Input_Parameters_JSON_SetFlags(self, iRegion, .true., .false., .false.)
        case (20)
            call Inout_Input_Parameters_JSON_SetFlags(self, iRegion, .false., .true., .false.)
        case (31)
            call Inout_Input_Parameters_JSON_SetFlags(self, iRegion, .false., .false., .true., .false., .false., .false.)
        case (32)
            call Inout_Input_Parameters_JSON_SetFlags(self, iRegion, .false., .false., .true., .false., .false., .true.)
        case (33)
            call Inout_Input_Parameters_JSON_SetFlags(self, iRegion, .false., .false., .true., .false., .true., .false.)
        case (34)
            call Inout_Input_Parameters_JSON_SetFlags(self, iRegion, .false., .false., .true., .false., .true., .true.)
        case (35)
            call Inout_Input_Parameters_JSON_SetFlags(self, iRegion, .false., .false., .true., .true., .false., .false.)
        case (36)
            call Inout_Input_Parameters_JSON_SetFlags(self, iRegion, .false., .false., .true., .true., .false., .true.)
        case (37)
            call Inout_Input_Parameters_JSON_SetFlags(self, iRegion, .false., .false., .true., .true., .true., .false.)
        case (38)
            call Inout_Input_Parameters_JSON_SetFlags(self, iRegion, .false., .false., .true., .true., .true., .true.)
        case default
            call error_message(903, copt1=ModelnumberName)
        end select

        key = Inout_Input_Connect_dot(region_name, isFrozenName)
        call json%get(key, self%Regions(iRegion)%Flags%isFrozen)
        call json%print_error_message(output_unit)

    end subroutine Inout_Input_Parameters_JSON_Reigion_Infomation

    subroutine Inout_Input_Parameters_JSON_SetCalculationTypes(self, iRegion, isHeat, isWater, isStress)
        !> Set the calculation types
        implicit none
        class(Input) :: self
        integer(int32), intent(in) :: iRegion !! Region number
        logical(4), intent(in) :: isHeat !! Heat calculation
        logical(4), intent(in) :: isWater !! Water calculation
        logical(4), intent(in) :: isStress !! Stress calculation

        self%Regions(iRegion)%Flags%isHeat = isHeat
        self%Regions(iRegion)%Flags%isWater = isWater
        self%Regions(iRegion)%Flags%isStress = isStress

    end subroutine Inout_Input_Parameters_JSON_SetCalculationTypes

    subroutine Inout_Input_Parameters_JSON_SetFlags(self, iRegion, is1Phase, is2Phase, is3Phase, isCompression, isFrostHeavePressure, isDispersity)
        !> Set the calculation flags
        implicit none
        class(Input) :: self
        integer(int32), intent(in) :: iRegion !! Region number
        logical(4), intent(in) :: is1Phase !! 1 Phase calculation
        logical(4), intent(in) :: is2Phase !! 2 Phase calculation
        logical(4), intent(in) :: is3Phase !! 3 Phase calculation
        logical(4), intent(in), optional :: isCompression !! consideer the water/ice compression
        logical(4), intent(in), optional :: isFrostHeavePressure !! Frost heave pressure calculation
        logical(4), intent(in), optional :: isDispersity !! Thermalc onductivity dispersity calculation

        self%Regions(iRegion)%Flags%is1Phase = is1Phase
        self%Regions(iRegion)%Flags%is2Phase = is2Phase
        self%Regions(iRegion)%Flags%is3Phase = is3Phase
        if (present(isCompression)) self%Regions(iRegion)%Flags%isCompression = isCompression
        if (present(isFrostHeavePressure)) self%Regions(iRegion)%Flags%isFrostHeavePressure = isFrostHeavePressure
        if (present(isDispersity)) self%Regions(iRegion)%Flags%isDispersity = isDispersity

    end subroutine Inout_Input_Parameters_JSON_SetFlags

    subroutine Inout_Input_Parameters_JSON_Thermal(self, json, iRegion)
        !> Load the thermal parameters from the JSON file
        implicit none
        class(Input) :: self
        type(json_file), intent(inout) :: json !! JSON parser
        integer(int32), intent(in) :: iRegion !! Region number

        character(8) :: region_name
        integer(int32) :: QiceType
        character(:), allocatable :: key

        call Allocate_Structure_Thermal_Type(self%Regions(iRegion)%Thermal, self%Regions(iRegion)%Flags)

        write (region_name, '(a, i0)') RegionName, iRegion
        if (self%Regions(iRegion)%Flags%isFrozen) then
            if (self%Regions(iRegion)%Flags%is3Phase) then
                key = Inout_Input_Connect_dot(region_name, ThermalName, PorosityName)
                call json%get(key, self%Regions(iRegion)%Thermal%Porosity)
                call json%print_error_message(output_unit)
            end if
        end if

        select type (Density => self%Regions(iRegion)%Thermal%Density)
        type is (Type_Density_1Phase)
            key = Inout_Input_Connect_dot(region_name, ThermalName, DensityName)
            call json%get(key, Density%Phase1)
            call json%print_error_message(output_unit)
        type is (Type_Density_2Phase)
            key = Inout_Input_Connect_dot(region_name, ThermalName, DensityName, Phase1Name)
            call json%get(key, Density%Phase1)
            call json%print_error_message(output_unit)

            key = Inout_Input_Connect_dot(region_name, ThermalName, DensityName, Phase2Name)
            call json%get(key, Density%Phase2)
            call json%print_error_message(output_unit)
        type is (Type_Density_3Phase)
            key = Inout_Input_Connect_dot(region_name, ThermalName, DensityName, SoilName)
            call json%get(key, Density%Soil)
            call json%print_error_message(output_unit)

            key = Inout_Input_Connect_dot(region_name, ThermalName, DensityName, WaterName)
            call json%get(key, Density%Water)
            call json%print_error_message(output_unit)

            key = Inout_Input_Connect_dot(region_name, ThermalName, DensityName, IceName)
            call json%get(key, Density%Ice)
            call json%print_error_message(output_unit)
        end select

        select type (SpecificHeat => self%Regions(iRegion)%Thermal%SpecificHeat)
        type is (Type_SpecificHeat_1Phase)
            key = Inout_Input_Connect_dot(region_name, ThermalName, SpecificHeatName)
            call json%get(key, SpecificHeat%Phase1)
            call json%print_error_message(output_unit)
        type is (Type_SpecificHeat_2Phase)
            key = Inout_Input_Connect_dot(region_name, ThermalName, SpecificHeatName, Phase1Name)
            call json%get(key, SpecificHeat%Phase1)
            call json%print_error_message(output_unit)

            key = Inout_Input_Connect_dot(region_name, ThermalName, SpecificHeatName, Phase2Name)
            call json%get(key, SpecificHeat%Phase2)
            call json%print_error_message(output_unit)
        type is (Type_SpecificHeat_3Phase)
            key = Inout_Input_Connect_dot(region_name, ThermalName, SpecificHeatName, SoilName)
            call json%get(key, SpecificHeat%Soil)
            call json%print_error_message(output_unit)

            key = Inout_Input_Connect_dot(region_name, ThermalName, SpecificHeatName, WaterName)
            call json%get(key, SpecificHeat%Water)
            call json%print_error_message(output_unit)

            key = Inout_Input_Connect_dot(region_name, ThermalName, SpecificHeatName, IceName)
            call json%get(key, SpecificHeat%Ice)
            call json%print_error_message(output_unit)
        end select

        select type (ThermalConductivity => self%Regions(iRegion)%Thermal%ThermalConductivity)
        type is (Type_ThermalConductivity_1Phase)
            key = Inout_Input_Connect_dot(region_name, ThermalName, ThermalConductivityName)
            call json%get(key, ThermalConductivity%Phase1)
            call json%print_error_message(output_unit)
        type is (Type_ThermalConductivity_2Phase)
            key = Inout_Input_Connect_dot(region_name, ThermalName, ThermalConductivityName, Phase1Name)
            call json%get(key, ThermalConductivity%Phase1)
            call json%print_error_message(output_unit)

            key = Inout_Input_Connect_dot(region_name, ThermalName, ThermalConductivityName, Phase2Name)
            call json%get(key, ThermalConductivity%Phase2)
            call json%print_error_message(output_unit)
        type is (Type_ThermalConductivity_3Phase)
            key = Inout_Input_Connect_dot(region_name, ThermalName, ThermalConductivityName, SoilName)
            call json%get(key, ThermalConductivity%Soil)
            call json%print_error_message(output_unit)

            key = Inout_Input_Connect_dot(region_name, ThermalName, ThermalConductivityName, WaterName)
            call json%get(key, ThermalConductivity%Water)
            call json%print_error_message(output_unit)

            key = Inout_Input_Connect_dot(region_name, ThermalName, ThermalConductivityName, IceName)
            call json%get(key, ThermalConductivity%Ice)
            call json%print_error_message(output_unit)
        type is (Type_ThermalConductivity_3Phase_Dispersity_2D)
            key = Inout_Input_Connect_dot(region_name, ThermalName, ThermalConductivityName, SoilName)
            call json%get(key, ThermalConductivity%Soil)
            call json%print_error_message(output_unit)

            key = Inout_Input_Connect_dot(region_name, ThermalName, ThermalConductivityName, WaterName)
            call json%get(key, ThermalConductivity%Water)
            call json%print_error_message(output_unit)

            key = Inout_Input_Connect_dot(region_name, ThermalName, ThermalConductivityName, IceName)
            call json%get(key, ThermalConductivity%Ice)
            call json%print_error_message(output_unit)

            key = Inout_Input_Connect_dot(region_name, ThermalName, ThermalConductivityName, dispersityName, xName)
            call json%get(key, ThermalConductivity%dispersity%x)
            call json%print_error_message(output_unit)

            key = Inout_Input_Connect_dot(region_name, ThermalName, ThermalConductivityName, dispersityName, yName)
            call json%get(key, ThermalConductivity%dispersity%y)
            call json%print_error_message(output_unit)
        type is (Type_ThermalConductivity_3Phase_Dispersity_3D)
            key = Inout_Input_Connect_dot(region_name, ThermalName, ThermalConductivityName, SoilName)
            call json%get(key, ThermalConductivity%Soil)
            call json%print_error_message(output_unit)

            key = Inout_Input_Connect_dot(region_name, ThermalName, ThermalConductivityName, WaterName)
            call json%get(key, ThermalConductivity%Water)
            call json%print_error_message(output_unit)

            key = Inout_Input_Connect_dot(region_name, ThermalName, ThermalConductivityName, IceName)
            call json%get(key, ThermalConductivity%Ice)
            call json%print_error_message(output_unit)

            key = Inout_Input_Connect_dot(region_name, ThermalName, ThermalConductivityName, dispersityName, xName)
            call json%get(key, ThermalConductivity%dispersity%x)
            call json%print_error_message(output_unit)

            key = Inout_Input_Connect_dot(region_name, ThermalName, ThermalConductivityName, dispersityName, yName)
            call json%get(key, ThermalConductivity%dispersity%y)
            call json%print_error_message(output_unit)

            key = Inout_Input_Connect_dot(region_name, ThermalName, ThermalConductivityName, dispersityName, zName)
            call json%get(key, ThermalConductivity%dispersity%z)
            call json%print_error_message(output_unit)
        end select

        if (self%Regions(iRegion)%Flags%isFrozen) then
            key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, QiceTypeName)
            call json%get(key, QiceType)
            call json%print_error_message(output_unit)

            call Allocate_Structure_Ice_Type(self%Regions(iRegion)%Thermal, QiceType)

            select type (Ice => self%Regions(iRegion)%Thermal%Ice)
            type is (Type_Ice_TRM)
                key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, TfName)
                call json%get(key, Ice%Tf)
                call json%print_error_message(output_unit)
            type is (Type_Ice_GCC)
                key = Inout_Input_Connect_dot(region_name, ThermalName, LatentHeatName)
                call json%get(key, self%Regions(iRegion)%Thermal%Ice%LatentHeat)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, TfName)
                call json%get(key, Ice%Tf)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, ModelName)
                call json%get(key, Ice%ModelType)
                call json%print_error_message(output_unit)

                call Allocate_Structure_WRF_Type(self%Regions(iRegion)%Thermal, Ice%ModelType)

                select type (WRF => Ice%WRF)
                type is (Type_WRF_BC)
                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, thetaSName)
                    call json%get(key, WRF%thetaS)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, thetaRName)
                    call json%get(key, WRF%thetaR)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, alpha1Name)
                    call json%get(key, WRF%alpha1)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, n1Name)
                    call json%get(key, WRF%n1)
                    call json%print_error_message(output_unit)
                type is (Type_WRF_VG)
                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, thetaSName)
                    call json%get(key, WRF%thetaS)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, thetaRName)
                    call json%get(key, WRF%thetaR)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, alpha1Name)
                    call json%get(key, WRF%alpha1)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, n1Name)
                    call json%get(key, WRF%n1)
                    call json%print_error_message(output_unit)

                    WRF%m1 = 1.0 - 1.0 / WRF%n1
                type is (Type_WRF_KO)
                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, thetaSName)
                    call json%get(key, WRF%thetaS)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, thetaRName)
                    call json%get(key, WRF%thetaR)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, alpha1Name)
                    call json%get(key, WRF%alpha1)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, n1Name)
                    call json%get(key, WRF%n1)
                    call json%print_error_message(output_unit)

                type is (Type_WRF_MVG)
                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, thetaSName)
                    call json%get(key, WRF%thetaS)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, thetaRName)
                    call json%get(key, WRF%thetaR)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, alpha1Name)
                    call json%get(key, WRF%alpha1)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, n1Name)
                    call json%get(key, WRF%n1)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, hcritName)
                    call json%get(key, WRF%hcrit)
                    call json%print_error_message(output_unit)

                    WRF%m1 = 1.0 - 1.0 / WRF%n1

                type is (Type_WRF_Durner)
                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, thetaSName)
                    call json%get(key, WRF%thetaS)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, thetaRName)
                    call json%get(key, WRF%thetaR)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, alpha1Name)
                    call json%get(key, WRF%alpha1)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, n1Name)
                    call json%get(key, WRF%n1)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, alpha2Name)
                    call json%get(key, WRF%alpha2)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, n2Name)
                    call json%get(key, WRF%n2)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, w1Name)
                    call json%get(key, WRF%w1)
                    call json%print_error_message(output_unit)

                    WRF%m1 = 1.0 - 1.0 / WRF%n1
                    WRF%m2 = 1.0 - 1.0 / WRF%n2
                    WRF%w2 = 1.0 - WRF%w1
                type is (Type_WRF_DVGCH)
                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, thetaSName)
                    call json%get(key, WRF%thetaS)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, thetaRName)
                    call json%get(key, WRF%thetaR)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, alpha1Name)
                    call json%get(key, WRF%alpha1)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, n1Name)
                    call json%get(key, WRF%n1)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, n2Name)
                    call json%get(key, WRF%n2)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, ParametersName, w1Name)
                    call json%get(key, WRF%w1)
                    call json%print_error_message(output_unit)

                    WRF%m1 = 1.0 - 1.0 / WRF%n1
                    WRF%m2 = 1.0 - 1.0 / WRF%n2
                    WRF%w2 = 1.0 - WRF%w1
                end select

            type is (Type_Ice_EXP)
                key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, TfName)
                call json%get(key, Ice%Tf)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, ThermalName, IceName, AName)
                call json%get(key, Ice%a)
                call json%print_error_message(output_unit)
            end select
        end if
    end subroutine Inout_Input_Parameters_JSON_Thermal

    subroutine Inout_Input_Parameters_JSON_Hydraulic(self, json, iRegion)
        !> Load the hydraulic parameters from the JSON file
        implicit none
        class(Input) :: self
        type(json_file), intent(inout) :: json !! JSON parser
        integer(int32), intent(in) :: iRegion !! Region number

        character(8) :: region_name
        character(:), allocatable :: key

        write (region_name, '(a, i0)') RegionName, iRegion

        key = Inout_Input_Connect_dot(region_name, HydraulicName, useHCFName)
        call json%get(key, self%Regions(iRegion)%Hydraulic%useHCF)
        call json%print_error_message(output_unit)

        key = Inout_Input_Connect_dot(region_name, HydraulicName, useImpedanceName)
        call json%get(key, self%Regions(iRegion)%Hydraulic%useImpedance)
        call json%print_error_message(output_unit)

        key = Inout_Input_Connect_dot(region_name, HydraulicName, useKTDynamicsName)
        call json%get(key, self%Regions(iRegion)%Hydraulic%useKTDynamics)
        call json%print_error_message(output_unit)

        call Allocate_Structure_Hydraulic_Type(self%Regions(iRegion)%Hydraulic)

        key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, KsName)
        call json%get(key, self%Regions(iRegion)%Hydraulic%Ks)
        call json%print_error_message(output_unit)

        if (allocated(self%Regions(iRegion)%Hydraulic%HCF)) then
            select type (HCF => self%Regions(iRegion)%Hydraulic%HCF)
            type is (Type_HCF_BC)
                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, thetaSName)
                call json%get(key, HCF%thetaS)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, thetaRName)
                call json%get(key, HCF%thetaR)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, alpha1Name)
                call json%get(key, HCF%alpha1)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, n1Name)
                call json%get(key, HCF%n1)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, lName)
                call json%get(key, HCF%l)
                call json%print_error_message(output_unit)

            type is (Type_HCF_VG)
                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, thetaSName)
                call json%get(key, HCF%thetaS)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, thetaRName)
                call json%get(key, HCF%thetaR)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, alpha1Name)
                call json%get(key, HCF%alpha1)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, n1Name)
                call json%get(key, HCF%n1)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, lName)
                call json%get(key, HCF%l)
                call json%print_error_message(output_unit)

                HCF%m1 = 1.0 - 1.0 / HCF%n1

            type is (Type_HCF_KO)
                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, thetaSName)
                call json%get(key, HCF%thetaS)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, thetaRName)
                call json%get(key, HCF%thetaR)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, alpha1Name)
                call json%get(key, HCF%alpha1)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, n1Name)
                call json%get(key, HCF%n1)
                call json%print_error_message(output_unit)

            type is (Type_HCF_MVG)
                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, thetaSName)
                call json%get(key, HCF%thetaS)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, thetaRName)
                call json%get(key, HCF%thetaR)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, alpha1Name)
                call json%get(key, HCF%alpha1)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, n1Name)
                call json%get(key, HCF%n1)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, hcritName)
                call json%get(key, HCF%hcrit)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, lName)
                call json%get(key, HCF%l)
                call json%print_error_message(output_unit)

                HCF%m1 = 1.0 - 1.0 / HCF%n1

            type is (Type_HCF_Durner)
                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, thetaSName)
                call json%get(key, HCF%thetaS)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, thetaRName)
                call json%get(key, HCF%thetaR)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, alpha1Name)
                call json%get(key, HCF%alpha1)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, n1Name)
                call json%get(key, HCF%n1)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, alpha2Name)
                call json%get(key, HCF%alpha2)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, n2Name)
                call json%get(key, HCF%n2)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, w1Name)
                call json%get(key, HCF%w1)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, lName)
                call json%get(key, HCF%l)
                call json%print_error_message(output_unit)

                HCF%m1 = 1.0 - 1.0 / HCF%n1
                HCF%m2 = 1.0 - 1.0 / HCF%n2
                HCF%w2 = 1.0 - HCF%w1

            type is (Type_HCF_DVGCH)
                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, thetaSName)
                call json%get(key, HCF%thetaS)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, thetaRName)
                call json%get(key, HCF%thetaR)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, alpha1Name)
                call json%get(key, HCF%alpha1)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, n1Name)
                call json%get(key, HCF%n1)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, n2Name)
                call json%get(key, HCF%n2)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, w1Name)
                call json%get(key, HCF%w1)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, lName)
                call json%get(key, HCF%l)
                call json%print_error_message(output_unit)

                HCF%m1 = 1.0 - 1.0 / HCF%n1
                HCF%m2 = 1.0 - 1.0 / HCF%n2
                HCF%w2 = 1.0 - HCF%w1
            end select
        end if

        if (allocated(self%Regions(iRegion)%Hydraulic%Impedance)) then
            select type (Impedance => self%Regions(iRegion)%Hydraulic%Impedance)
            type is (Type_Impedance)
                key = Inout_Input_Connect_dot(region_name, HydraulicName, ParametersName, OmegaName)
                call json%get(key, Impedance%Omega)
                call json%print_error_message(output_unit)
            end select
        end if

    end subroutine Inout_Input_Parameters_JSON_Hydraulic

    subroutine Inout_Input_Parameters_JSON_Solver(self, json)
        !> load Solver settings from the JSON file
        implicit none
        class(Input) :: self
        type(json_file), intent(inout) :: json !! JSON parser

        character(:), allocatable :: key
        integer(int32) :: useSolver

        key = Inout_Input_Connect_dot(SolveName, TimeDiscretizationName)
        call json%get(key, self%Basic%TimeDiscretization)
        call json%print_error_message(output_unit)
        if (any(self%Regions(:)%Flags%isHeat)) then
            key = Inout_Input_Connect_dot(SolveName, ThermalName, useSolverName)
            call json%get(key, useSolver)
            call json%print_error_message(output_unit)

            call Inout_Input_Parameters_JSON_Solver_Settings(self, json, useSolver, ThermalName)
        end if
        if (any(self%Regions(:)%Flags%isWater)) then
            key = Inout_Input_Connect_dot(SolveName, HydraulicName, useSolverName)
            call json%get(key, useSolver)
            call json%print_error_message(output_unit)

            call Inout_Input_Parameters_JSON_Solver_Settings(self, json, useSolver, HydraulicName)
        end if

    end subroutine Inout_Input_Parameters_JSON_Solver

    subroutine Inout_Input_Parameters_JSON_Solver_Settings(self, json, useSolver, c_target)
        !> Load the solver detail settings from the JSON file
        implicit none
        class(Input) :: self
        type(json_file), intent(inout) :: json !! JSON parser
        integer(int32), intent(in) :: useSolver !! Solver type
        character(*), intent(in) :: c_target !! Target name

        character(:), allocatable :: key
        select case (c_target)
        case (ThermalName)
            select case (useSolver)
            case (1)
                allocate (Base_Solver :: self%Solver%Thermal)
                self%Solver%Thermal%useSolver = useSolver
            case (2)
                allocate (Type_Solver_Iterative :: self%Solver%Thermal)
                self%Solver%Thermal%useSolver = useSolver

                select type (Thermal => self%Solver%Thermal)
                type is (Type_Solver_Iterative)
                    key = Inout_Input_Connect_dot(SolveName, ThermalName, ParametersName, SolverName)
                    call json%get(key, Thermal%SolverType)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(SolveName, ThermalName, ParametersName, PreconditionerName)
                    call json%get(key, Thermal%PreconditionerType)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(SolveName, ThermalName, ParametersName, MaxIterationName)
                    call json%get(key, Thermal%MaxIter)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(SolveName, ThermalName, ParametersName, ToleranceName)
                    call json%get(key, Thermal%Tol)
                    call json%print_error_message(output_unit)
                class default
                    ! エラー処理
                    write (*, '(A)') "Error: Unexpected type assigned to self%Solver%Thermal"
                    stop
                end select
            end select
        case (HydraulicName)
            select case (useSolver)
            case (1)
                allocate (Base_Solver :: self%Solver%Hydraulic)
                self%Solver%Hydraulic%useSolver = useSolver
            case (2)
                allocate (Type_Solver_Iterative :: self%Solver%Hydraulic)
                self%Solver%Hydraulic%useSolver = useSolver

                select type (Hydraulic => self%Solver%Hydraulic)
                type is (Type_Solver_Iterative)
                    key = Inout_Input_Connect_dot(SolveName, HydraulicName, ParametersName, SolverName)
                    call json%get(key, Hydraulic%SolverType)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(SolveName, HydraulicName, ParametersName, PreconditionerName)
                    call json%get(key, Hydraulic%PreconditionerType)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(SolveName, HydraulicName, ParametersName, MaxIterationName)
                    call json%get(key, Hydraulic%MaxIter)
                    call json%print_error_message(output_unit)

                    key = Inout_Input_Connect_dot(SolveName, HydraulicName, ParametersName, ToleranceName)
                    call json%get(key, Hydraulic%Tol)
                    call json%print_error_message(output_unit)
                class default
                    ! エラー処理
                    write (*, '(A)') "Error: Unexpected type assigned to self%Solver%Hydraulic"
                    stop
                end select
            end select
        end select

    end subroutine Inout_Input_Parameters_JSON_Solver_Settings

    subroutine Inout_Input_Conditions_JSON(self)
        !> Load the boundary/initial conditions from the JSON file
        implicit none
        class(Input) :: self

        type(json_file) :: json
        character(:), allocatable :: key
        integer(int32) :: iRegion

        call json%initialize()
        call json%load(filename=self%Conditions_FileName)
        call json%print_error_message(output_unit)

        call Inout_Input_Conditions_JSON_BC(self, json)
        call Inout_Input_Conditions_JSON_IC(self, json)

        call json%destroy()
        call json%print_error_message(output_unit)

    end subroutine Inout_Input_Conditions_JSON

    subroutine Inout_Input_Conditions_JSON_BC(self, json)
        !> Load the boundary conditions from the JSON file
        implicit none
        class(Input) :: self
        type(json_file), intent(inout) :: json !! JSON parser

        character(:), allocatable :: key
        character(2) :: cBCGroup
        integer(int32) :: iBC

        key = Inout_Input_Connect_dot(BCName, GroupName)
        call json%get(key, self%Conditions%BCGroup)
        call json%print_error_message(output_unit)
        allocate (self%Conditions%BC_Thermal(size(self%Conditions%BCGroup)))
        allocate (self%Conditions%BC_Hydraulic(size(self%Conditions%BCGroup)))

        do iBC = 1, size(self%Conditions%BCGroup)
            write (cBCGroup, '(i0)') self%Conditions%BCGroup(iBC)
            key = Inout_Input_Connect_dot(BCName, cBCGroup, ThermalName, TypeName)
            call json%get(key, self%Conditions%BC_Thermal(iBC)%type)
            call json%print_error_message(output_unit)

            select case (self%Conditions%BC_Thermal(iBC)%type)
            case (DirichletName, HeatTransferName)
                key = Inout_Input_Connect_dot(BCName, cBCGroup, ThermalName, ValueName)
                call json%get(key, self%Conditions%BC_Thermal(iBC)%value)
                call json%print_error_message(output_unit)
            case default
                self%Conditions%BC_Thermal(iBC)%value = NaNValue
            end select

            key = Inout_Input_Connect_dot(BCName, cBCGroup, HydraulicName, TypeName)
            call json%get(key, self%Conditions%BC_Hydraulic(iBC)%type)
            call json%print_error_message(output_unit)

            select case (self%Conditions%BC_Hydraulic(iBC)%type)
            case (DirichletName, HeatTransferName)
                key = Inout_Input_Connect_dot(BCName, cBCGroup, HydraulicName, ValueName)
                call json%get(key, self%Conditions%BC_Hydraulic(iBC)%value)
                call json%print_error_message(output_unit)
            case default
                self%Conditions%BC_Hydraulic(iBC)%value = NaNValue
            end select
        end do

    end subroutine Inout_Input_Conditions_JSON_BC

    subroutine Inout_Input_Conditions_JSON_IC(self, json)
        !> Load the initialy conditions from the JSON file
        implicit none
        class(Input) :: self
        type(json_file), intent(inout) :: json !! JSON parser

        character(:), allocatable :: key
        character(:), allocatable :: tmp

        character(2) :: cICGroup
        integer(int32) :: i, count
        logical(4) :: isFind

        key = Inout_Input_Connect_dot(ICName, ThermalName, TypeName)
        call json%get(key, self%Conditions%IC_Thermal%type)
        call json%print_error_message(output_unit)

        select case (self%Conditions%IC_Thermal%type)
        case (ConstantName)
            key = Inout_Input_Connect_dot(ICName, ThermalName, ValueName)
            call json%get(key, self%Conditions%IC_Thermal%value)
            call json%print_error_message(output_unit)
        case (LaplaceName)
            count = 0
            do i = 1, size(self%Conditions%BCGroup)
                write (cICGroup, '(i0)') self%Conditions%BCGroup(i)
                key = Inout_Input_Connect_dot(ICName, ThermalName, ValueName, cICGroup, TypeName)
                call json%get(key, tmp, found=isFind)
                if (isFind) count = count + 1
            end do
            allocate (self%Conditions%IC_Thermal%IC_BC(count))
            do i = 1, size(self%Conditions%BCGroup)
                write (cICGroup, '(i0)') self%Conditions%BCGroup(i)
                key = Inout_Input_Connect_dot(ICName, ThermalName, ValueName, cICGroup, TypeName)
                call json%get(key, tmp, found=isFind)
                if (.not. isFind) cycle

                key = Inout_Input_Connect_dot(ICName, ThermalName, ValueName, cICGroup, TypeName)
                call json%get(key, self%Conditions%IC_Thermal%IC_BC(i)%type)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(ICName, ThermalName, ValueName, cICGroup, ValueName)
                call json%get(key, self%Conditions%IC_Thermal%IC_BC(i)%value)
                call json%print_error_message(output_unit)
            end do

        end select

        key = Inout_Input_Connect_dot(ICName, HydraulicName, TypeName)
        call json%get(key, self%Conditions%IC_Hydraulic%type)
        call json%print_error_message(output_unit)

        select case (self%Conditions%IC_Hydraulic%type)
        case (ConstantName)
            key = Inout_Input_Connect_dot(ICName, HydraulicName, ValueName)
            call json%get(key, self%Conditions%IC_Hydraulic%value)
            call json%print_error_message(output_unit)
        case (LaplaceName)
            count = 0
            do i = 1, size(self%Conditions%BCGroup)
                write (cICGroup, '(i0)') self%Conditions%BCGroup(i)
                key = Inout_Input_Connect_dot(ICName, HydraulicName, ValueName, cICGroup, TypeName)
                call json%get(key, tmp, found=isFind)
                if (isFind) count = count + 1
            end do
            allocate (self%Conditions%IC_Hydraulic%IC_BC(count))
            count = 0
            do i = 1, size(self%Conditions%BCGroup)
                write (cICGroup, '(i0)') self%Conditions%BCGroup(i)
                key = Inout_Input_Connect_dot(ICName, HydraulicName, ValueName, cICGroup, TypeName)
                call json%get(key, tmp, found=isFind)

                if (.not. isFind) cycle
                count = count + 1

                key = Inout_Input_Connect_dot(ICName, HydraulicName, ValueName, cICGroup, TypeName)
                call json%get(key, self%Conditions%IC_Hydraulic%IC_BC(count)%type)
                call json%print_error_message(output_unit)

                key = Inout_Input_Connect_dot(ICName, HydraulicName, ValueName, cICGroup, ValueName)
                call json%get(key, self%Conditions%IC_Hydraulic%IC_BC(count)%value)
                call json%print_error_message(output_unit)
            end do
        end select

    end subroutine Inout_Input_Conditions_JSON_IC

    subroutine Inout_Input_Geometry_VTK(self)
        !> Load the geometry from the VTK file
        implicit none
        class(Input) :: self

        call Inout_VTK_Read(self%Geometry_FileName, self%VTK)
    end subroutine Inout_Input_Geometry_VTK

    ! subroutine Inout_Input_Finalize(self)
    !     implicit none
    !     type(Input) :: self

    !     if (allocated(self%Work_Region_Basic_Infomatin)) deallocate (self%Work_Region_Basic_Infomatin)
    !     if (allocated(self%Work_Region_Paremeters_real64)) deallocate (self%Work_Region_Paremeters_real64)
    !     if (allocated(self%Work_Region_Parameters_int32)) deallocate (self%Work_Region_Parameters_int32)
    !     if (allocated(self%Work_Region_Parameters_Number)) deallocate (self%Work_Region_Parameters_Number)
    !     if (allocated(self%Work_Coordinates)) deallocate (self%Work_Coordinates)
    !     if (allocated(self%Work_Coordinates_Region)) deallocate (self%Work_Coordinates_Region)
    !     if (allocated(self%Work_Top)) deallocate (self%Work_Top)
    !     if (allocated(self%Work_NBC_Node)) deallocate (self%Work_NBC_Node)
    !     if (allocated(self%Work_NBC_Node_Type)) deallocate (self%Work_NBC_Node_Type)
    !     if (allocated(self%Work_NBC_Node_Value_Info)) deallocate (self%Work_NBC_Node_Value_Info)
    !     if (allocated(self%Work_NBC_Node_Value)) deallocate (self%Work_NBC_Node_Value)
    !     if (allocated(self%Work_EBC_Edge)) deallocate (self%Work_EBC_Edge)
    !     if (allocated(self%Work_EBC_Edge_Type)) deallocate (self%Work_EBC_Edge_Type)
    !     if (allocated(self%Work_EBC_Edge_Value_Info)) deallocate (self%Work_EBC_Edge_Value_Info)
    !     if (allocated(self%Work_EBC_Edge_Value)) deallocate (self%Work_EBC_Edge_Value)
    !     if (allocated(self%Work_IC_Type)) deallocate (self%Work_IC_Type)
    !     if (allocated(self%Work_IC_Value)) deallocate (self%Work_IC_Value)
    !     if (allocated(self%Work_Observation_Node)) deallocate (self%Work_Observation_Node)
    !     if (allocated(self%Work_Observation_Coordinate)) deallocate (self%Work_Observation_Coordinate)
    !     if (allocated(self%Work_Observation_Flag)) deallocate (self%Work_Observation_Flag)

    ! end subroutine Inout_Input_Finalize

    subroutine Inout_Input_Get_Basic_Params(self, Structure)
        !> Get the Basic_Params of the input data
        implicit none
        class(Input) :: self
        type(Basic_params), intent(inout) :: Structure

        Structure%Element = self%Basic%Element
        Structure%Node = self%Basic%Node
        Structure%ShapeType = self%Basic%ShapeType
        Structure%DimensionType = self%Basic%DimensionType
        Structure%Region = self%Basic%Region
        Structure%Calculation_timeUnit = self%Basic%Calculation_timeUnit
        Structure%Input_timeUnit = self%Basic%Input_timeUnit
        Structure%Output_timeUnit = self%Basic%Output_timeUnit
        Structure%Interval_timeUnit = self%Basic%Interval_timeUnit
        Structure%Calculation_step = self%Basic%Calculation_step
        Structure%CalculationPeriod = self%Basic%CalculationPeriod
        Structure%Interval = self%Basic%Interval
        Structure%isDisplayPrompt = self%Basic%isDisplayPrompt
        Structure%FileOutput = self%Basic%FileOutput
        Structure%TimeDiscretization = self%Basic%TimeDiscretization

    end subroutine Inout_Input_Get_Basic_Params

    subroutine Inout_Input_Get_Regional_Params(self, Structure, iRegion, cType)
        !> Get the Basic_Params of the input data
        implicit none
        class(Input) :: self
        type(Type_Region), intent(inout) :: Structure
        integer(int32), intent(in) :: iRegion
        character(*), intent(in) :: cType

        select case (cType)
        case ("Thermal")
            Structure%BelongingSurface = self%Regions(iRegion)%BelongingSurface
            allocate (Structure%BelongingEdge, source=self%Regions(iRegion)%BelongingEdge)
            Structure%CalculationType = self%Regions(iRegion)%CalculationType
            Structure%Modelnumber = self%Regions(iRegion)%Modelnumber
            Structure%Flags%is1Phase = self%Regions(iRegion)%Flags%is1Phase
            Structure%Flags%is2Phase = self%Regions(iRegion)%Flags%is2Phase
            Structure%Flags%is3Phase = self%Regions(iRegion)%Flags%is3Phase
            Structure%Flags%isHeat = self%Regions(iRegion)%Flags%isHeat
            Structure%Flags%isWater = self%Regions(iRegion)%Flags%isWater
            Structure%Flags%isStress = self%Regions(iRegion)%Flags%isStress
            Structure%Flags%isCompression = self%Regions(iRegion)%Flags%isCompression
            Structure%Flags%isFrostHeavePressure = self%Regions(iRegion)%Flags%isFrostHeavePressure
            Structure%Flags%isDispersity = self%Regions(iRegion)%Flags%isDispersity
            Structure%Flags%isFrozen = self%Regions(iRegion)%Flags%isFrozen
            call Allocate_Structure_Thermal_Type(Structure%Thermal, Structure%Flags)
        case ("Hydraulic")
            Structure%BelongingSurface = self%Regions(iRegion)%BelongingSurface
            allocate (Structure%BelongingEdge, source=self%Regions(iRegion)%BelongingEdge)
            Structure%CalculationType = self%Regions(iRegion)%CalculationType
            Structure%Modelnumber = self%Regions(iRegion)%Modelnumber
            Structure%Flags%is1Phase = self%Regions(iRegion)%Flags%is1Phase
            Structure%Flags%is2Phase = self%Regions(iRegion)%Flags%is2Phase
            Structure%Flags%is3Phase = self%Regions(iRegion)%Flags%is3Phase
            Structure%Flags%isHeat = self%Regions(iRegion)%Flags%isHeat
            Structure%Flags%isWater = self%Regions(iRegion)%Flags%isWater
            Structure%Flags%isStress = self%Regions(iRegion)%Flags%isStress
            Structure%Flags%isCompression = self%Regions(iRegion)%Flags%isCompression
            Structure%Flags%isFrostHeavePressure = self%Regions(iRegion)%Flags%isFrostHeavePressure
            Structure%Flags%isDispersity = self%Regions(iRegion)%Flags%isDispersity
            Structure%Flags%isFrozen = self%Regions(iRegion)%Flags%isFrozen
            call Allocate_Structure_Hydraulic_Type(Structure%Hydraulic)
        end select

    end subroutine Inout_Input_Get_Regional_Params

    subroutine Inout_Input_Get_Themal_Params(self, Structure, iRegion)
        !> Get the Themal_Params of the input data
        implicit none
        class(Input) :: self
        type(Type_Thermal), intent(inout) :: Structure
        integer(int32), intent(in) :: iRegion

        select type (Density => Structure%Density)
        type is (Type_Density_1Phase)
            select type (self_Density => self%Regions(iRegion)%Thermal%Density)
            type is (Type_Density_1Phase)
                Density%Phase1 = self_Density%Phase1
            end select
        type is (Type_Density_2Phase)
            select type (self_Density => self%Regions(iRegion)%Thermal%Density)
            type is (Type_Density_2Phase)
                Density%Phase1 = self_Density%Phase1
                Density%Phase2 = self_Density%Phase2
            end select
        type is (Type_Density_3Phase)
            select type (self_Density => self%Regions(iRegion)%Thermal%Density)
            type is (Type_Density_3Phase)
                Density%Soil = self_Density%Soil
                Density%Water = self_Density%Water
                Density%Ice = self_Density%Ice
            end select
        end select

        select type (SpecificHeat => Structure%SpecificHeat)
        type is (Type_SpecificHeat_1Phase)
            select type (self_SpecificHeat => self%Regions(iRegion)%Thermal%SpecificHeat)
            type is (Type_SpecificHeat_1Phase)
                SpecificHeat%Phase1 = self_SpecificHeat%Phase1
            end select
        type is (Type_SpecificHeat_2Phase)
            select type (self_SpecificHeat => self%Regions(iRegion)%Thermal%SpecificHeat)
            type is (Type_SpecificHeat_2Phase)
                SpecificHeat%Phase1 = self_SpecificHeat%Phase1
                SpecificHeat%Phase2 = self_SpecificHeat%Phase2
            end select
        type is (Type_SpecificHeat_3Phase)
            select type (self_SpecificHeat => self%Regions(iRegion)%Thermal%SpecificHeat)
            type is (Type_SpecificHeat_3Phase)
                SpecificHeat%Soil = self_SpecificHeat%Soil
                SpecificHeat%Water = self_SpecificHeat%Water
                SpecificHeat%Ice = self_SpecificHeat%Ice
            end select
        end select

        select type (ThermalConductivity => Structure%ThermalConductivity)
        type is (Type_ThermalConductivity_1Phase)
            select type (self_ThermalConductivity => self%Regions(iRegion)%Thermal%ThermalConductivity)
            type is (Type_ThermalConductivity_1Phase)
                ThermalConductivity%Phase1 = self_ThermalConductivity%Phase1
            end select
        type is (Type_ThermalConductivity_2Phase)
            select type (self_ThermalConductivity => self%Regions(iRegion)%Thermal%ThermalConductivity)
            type is (Type_ThermalConductivity_2Phase)
                ThermalConductivity%Phase1 = self_ThermalConductivity%Phase1
                ThermalConductivity%Phase2 = self_ThermalConductivity%Phase2
            end select
        type is (Type_ThermalConductivity_3Phase)
            select type (self_ThermalConductivity => self%Regions(iRegion)%Thermal%ThermalConductivity)
            type is (Type_ThermalConductivity_3Phase)
                ThermalConductivity%Soil = self_ThermalConductivity%Soil
                ThermalConductivity%Water = self_ThermalConductivity%Water
                ThermalConductivity%Ice = self_ThermalConductivity%Ice
            end select
        type is (Type_ThermalConductivity_3Phase_Dispersity_2D)
            select type (self_ThermalConductivity => self%Regions(iRegion)%Thermal%ThermalConductivity)
            type is (Type_ThermalConductivity_3Phase_Dispersity_2D)
                ThermalConductivity%Soil = self_ThermalConductivity%Soil
                ThermalConductivity%Water = self_ThermalConductivity%Water
                ThermalConductivity%Ice = self_ThermalConductivity%Ice
                ThermalConductivity%dispersity%x = self_ThermalConductivity%dispersity%x
                ThermalConductivity%dispersity%y = self_ThermalConductivity%dispersity%y
            end select
        type is (Type_ThermalConductivity_3Phase_Dispersity_3D)
            select type (self_ThermalConductivity => self%Regions(iRegion)%Thermal%ThermalConductivity)
            type is (Type_ThermalConductivity_3Phase_Dispersity_3D)
                ThermalConductivity%Soil = self_ThermalConductivity%Soil
                ThermalConductivity%Water = self_ThermalConductivity%Water
                ThermalConductivity%Ice = self_ThermalConductivity%Ice
                ThermalConductivity%dispersity%x = self_ThermalConductivity%dispersity%x
                ThermalConductivity%dispersity%y = self_ThermalConductivity%dispersity%y
                ThermalConductivity%dispersity%z = self_ThermalConductivity%dispersity%z
            end select
        end select

        if (self%Regions(iRegion)%Flags%isFrozen) then
            select type (self_Ice => self%Regions(iRegion)%Thermal%Ice)
            type is (Type_Ice_TRM)
                allocate (Type_Ice_TRM :: Structure%Ice)
                select type (Ice => Structure%Ice)
                type is (Type_Ice_TRM)
                    Ice%Tf = self_Ice%Tf
                end select
            type is (Type_Ice_GCC)
                allocate (Type_Ice_GCC :: Structure%Ice)
                call Allocate_Structure_WRF_Type(Structure, self_Ice%ModelType)
                select type (Ice => Structure%Ice)
                type is (Type_Ice_GCC)
                    Ice%LatentHeat = self_Ice%LatentHeat
                    Ice%Tf = self_Ice%Tf
                    Ice%ModelType = self_Ice%ModelType

                    select type (WRF => Ice%WRF)
                    type is (Type_WRF_BC)
                        select type (self_WRF => self_Ice%WRF)
                        type is (Type_WRF_BC)
                            WRF%thetaS = self_WRF%thetaS
                            WRF%thetaR = self_WRF%thetaR
                            WRF%alpha1 = self_WRF%alpha1
                            WRF%n1 = self_WRF%n1
                        end select
                    type is (Type_WRF_VG)
                        select type (self_WRF => self_Ice%WRF)
                        type is (Type_WRF_VG)
                            WRF%thetaS = self_WRF%thetaS
                            WRF%thetaR = self_WRF%thetaR
                            WRF%alpha1 = self_WRF%alpha1
                            WRF%n1 = self_WRF%n1
                            WRF%m1 = self_WRF%m1
                        end select
                    type is (Type_WRF_KO)
                        select type (self_WRF => self_Ice%WRF)
                        type is (Type_WRF_KO)
                            WRF%thetaS = self_WRF%thetaS
                            WRF%thetaR = self_WRF%thetaR
                            WRF%alpha1 = self_WRF%alpha1
                            WRF%n1 = self_WRF%n1
                        end select
                    type is (Type_WRF_MVG)
                        select type (self_WRF => self_Ice%WRF)
                        type is (Type_WRF_MVG)
                            WRF%thetaS = self_WRF%thetaS
                            WRF%thetaR = self_WRF%thetaR
                            WRF%alpha1 = self_WRF%alpha1
                            WRF%n1 = self_WRF%n1
                            WRF%m1 = self_WRF%m1
                        end select
                    type is (Type_WRF_Durner)
                        select type (self_WRF => self_Ice%WRF)
                        type is (Type_WRF_Durner)
                            WRF%thetaS = self_WRF%thetaS
                            WRF%thetaR = self_WRF%thetaR
                            WRF%alpha1 = self_WRF%alpha1
                            WRF%n1 = self_WRF%n1
                            WRF%m1 = self_WRF%m1
                            WRF%alpha2 = self_WRF%alpha2
                            WRF%n2 = self_WRF%n2
                            WRF%m2 = self_WRF%m2
                            WRF%w1 = self_WRF%w1
                            WRF%w2 = self_WRF%w2
                        end select
                    type is (Type_WRF_DVGCH)
                        select type (self_WRF => self_Ice%WRF)
                        type is (Type_WRF_DVGCH)
                            WRF%thetaS = self_WRF%thetaS
                            WRF%thetaR = self_WRF%thetaR
                            WRF%alpha1 = self_WRF%alpha1
                            WRF%n1 = self_WRF%n1
                            WRF%m1 = self_WRF%m1
                            WRF%n2 = self_WRF%n2
                            WRF%m2 = self_WRF%m2
                            WRF%w1 = self_WRF%w1
                            WRF%w2 = self_WRF%w2
                        end select
                    end select
                end select
            type is (Type_Ice_EXP)
                allocate (Type_Ice_EXP :: Structure%Ice)
                select type (Ice => Structure%Ice)
                type is (Type_Ice_EXP)
                    Ice%Tf = self_Ice%Tf
                    Ice%a = self_Ice%a
                end select
            end select
        end if
    end subroutine Inout_Input_Get_Themal_Params

    subroutine Inout_Input_Get_DP3d(self, key, Structure_DP)
        !> Get the DP2d/3d of the input data
        implicit none
        class(Input) :: self
        character(*), intent(in) :: key !! Key of the array
        type(DP3d), intent(inout) :: Structure_DP

        select case (key)
        case ("POINTS")
            allocate (Structure_DP%x, source=self%VTK%POINTS%x)
            allocate (Structure_DP%y, source=self%VTK%POINTS%y)
            allocate (Structure_DP%z, source=self%VTK%POINTS%z)
        end select

    end subroutine Inout_Input_Get_DP3d

    subroutine Inout_Input_Get_BoundaryConditions(self, key, group, Structure_BC)
        !> Get the BoundaryConditions of the input data
        implicit none
        class(Input) :: self
        character(*), intent(in) :: key !! Key of the array
        integer(int32), allocatable, intent(inout) :: group(:) !! Group of the array
        type(BC_Condition), allocatable, intent(inout) :: Structure_BC(:) !! Structure to store the data

        integer(int32) :: i

        select case (key)
        case (ThermalName)
            allocate (group, source=self%Conditions%BCGroup)
            allocate (Structure_BC(size(self%Conditions%BCGroup)))
            do i = 1, size(self%Conditions%BCGroup)
                Structure_BC(i)%type = self%Conditions%BC_Thermal(i)%type
                Structure_BC(i)%value = self%Conditions%BC_Thermal(i)%value
            end do
        case (HydraulicName)
            allocate (group, source=self%Conditions%BCGroup)
            allocate (Structure_BC(size(self%Conditions%BCGroup)))
            do i = 1, size(self%Conditions%BCGroup)
                Structure_BC(i)%type = self%Conditions%BC_Hydraulic(i)%type
                Structure_BC(i)%value = self%Conditions%BC_Hydraulic(i)%value
            end do
        end select

    end subroutine Inout_Input_Get_BoundaryConditions

    subroutine Inout_Input_Get_InitialConditions(self, key, Structure_IC)
        !> Get the InitialConditions of the input data
        implicit none
        class(Input) :: self
        character(*), intent(in) :: key !! Key of the array
        type(IC_Condition), intent(inout) :: Structure_IC
        integer(int32) :: i

        select case (key)
        case (ThermalName)
            Structure_IC%type = self%Conditions%IC_Thermal%type
            Structure_IC%value = self%Conditions%IC_Thermal%value
            if (allocated(self%Conditions%IC_Thermal%IC_BC)) then
                allocate (Structure_IC%IC_BC(size(self%Conditions%IC_Thermal%IC_BC)))
                do i = 1, size(self%Conditions%IC_Thermal%IC_BC)
                    Structure_IC%IC_BC(i)%type = self%Conditions%IC_Thermal%IC_BC(i)%type
                    Structure_IC%IC_BC(i)%value = self%Conditions%IC_Thermal%IC_BC(i)%value
                end do
            end if
        case (HydraulicName)
            Structure_IC%type = self%Conditions%IC_Hydraulic%type
            Structure_IC%value = self%Conditions%IC_Hydraulic%value
            if (allocated(self%Conditions%IC_Hydraulic%IC_BC)) then
                allocate (Structure_IC%IC_BC(size(self%Conditions%IC_Hydraulic%IC_BC)))
                do i = 1, size(self%Conditions%IC_Hydraulic%IC_BC)
                    Structure_IC%IC_BC(i)%type = self%Conditions%IC_Hydraulic%IC_BC(i)%type
                    Structure_IC%IC_BC(i)%value = self%Conditions%IC_Hydraulic%IC_BC(i)%value
                end do
            end if

        end select

    end subroutine Inout_Input_Get_InitialConditions

    subroutine Inout_Input_Get_int32(self, key, idata, optnum)
        !> Get the int32 of the input data
        implicit none
        class(Input) :: self
        character(*), intent(in) :: key !! Key of the array
        integer(int32), intent(inout) :: idata !! Array to store the data
        integer(int32), intent(in), optional :: optnum !! Number of the array

        select case (key)
        case ("numCellTypes")
            idata = self%VTK%numCellTypes
        case ("nCell")
            if (present(optnum)) then
                idata = self%VTK%CELLS(optnum)%nCells
            end if
        end select

    end subroutine Inout_Input_Get_int32

    subroutine Inout_Input_Get_int32_rank1(self, key, array_int32, optnum)
        !> Get the int32 rank1 array of the input data
        implicit none
        class(Input) :: self
        character(*), intent(in) :: key !! Key of the array
        integer(int32), intent(inout), allocatable :: array_int32(:) !! Array to store the data
        integer(int32), intent(in), optional :: optnum !! Number of the array

        select case (key)
        case ("CellEntityIds")
            allocate (array_int32, source=self%VTK%CellEntityIds)

        end select

    end subroutine Inout_Input_Get_int32_rank1

    subroutine Inout_Input_Get_int32_rank2(self, key, array_int32, optnum)
        !> Get the int32 rank1 array of the input data
        implicit none
        class(Input) :: self
        character(*), intent(in) :: key !! Key of the array
        integer(int32), intent(inout), allocatable :: array_int32(:, :) !! Array to store the data
        integer(int32), intent(in), optional :: optnum !! Number of the array

        select case (key)
        case ("CellNodes")
            if (present(optnum)) then
                allocate (array_int32, source=self%VTK%Cells(optnum)%Nodes)
            end if
        end select

    end subroutine Inout_Input_Get_int32_rank2

    function Inout_Input_Connect_dot_2(c1, c2) result(key)
        !> connect two strings with dot
        implicit none
        character(*), intent(in) :: c1 !! First string
        character(*), intent(in) :: c2 !! Second string
        character(:), allocatable :: key

        key = trim(adjustl(c1))//"."//trim(adjustl(c2))

    end function Inout_Input_Connect_dot_2

    function Inout_Input_Connect_dot_3(c1, c2, c3) result(key)
        !> connect three strings with dot
        implicit none
        character(*), intent(in) :: c1 !! First string
        character(*), intent(in) :: c2 !! Second string
        character(*), intent(in) :: c3 !! Third string
        character(:), allocatable :: key

        key = trim(adjustl(c1))//"."//trim(adjustl(c2))//"."//trim(adjustl(c3))

    end function Inout_Input_Connect_dot_3

    function Inout_Input_Connect_dot_4(c1, c2, c3, c4) result(key)
        !> connect four strings with dot
        implicit none
        character(*), intent(in) :: c1 !! First string
        character(*), intent(in) :: c2 !! Second string
        character(*), intent(in) :: c3 !! Third string
        character(*), intent(in) :: c4 !! Fourth string
        character(:), allocatable :: key

        key = trim(adjustl(c1))//"."//trim(adjustl(c2))//"."//trim(adjustl(c3))//"."//trim(adjustl(c4))

    end function Inout_Input_Connect_dot_4

    function Inout_Input_Connect_dot_5(c1, c2, c3, c4, c5) result(key)
        !> connect five strings with dot
        implicit none
        character(*), intent(in) :: c1 !! First string
        character(*), intent(in) :: c2 !! Second string
        character(*), intent(in) :: c3 !! Third string
        character(*), intent(in) :: c4 !! Fourth string
        character(*), intent(in) :: c5 !! Fifth string
        character(:), allocatable :: key

        key = trim(adjustl(c1))//"."//trim(adjustl(c2))//"."//trim(adjustl(c3))//"."//trim(adjustl(c4))//"."//trim(adjustl(c5))

    end function Inout_Input_Connect_dot_5

    function Inout_Input_Connect_dot_6(c1, c2, c3, c4, c5, c6) result(key)
        !> connect six strings with dot
        implicit none
        character(*), intent(in) :: c1 !! First string
        character(*), intent(in) :: c2 !! Second string
        character(*), intent(in) :: c3 !! Third string
        character(*), intent(in) :: c4 !! Fourth string
        character(*), intent(in) :: c5 !! Fifth string
        character(*), intent(in) :: c6 !! Sixth string
        character(:), allocatable :: key

        key = trim(adjustl(c1))//"."//trim(adjustl(c2))//"."//trim(adjustl(c3))//"."//trim(adjustl(c4))//"."//trim(adjustl(c5))//"."//trim(adjustl(c6))

    end function Inout_Input_Connect_dot_6
end module Inout_Input

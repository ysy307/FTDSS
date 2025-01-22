module Inout_Input
    use, intrinsic :: iso_fortran_env, only: int32, real64, output_unit
    use :: Inout_SetProjectPath, only:GetProjectPath => Inout_SetProjectPath_GetProjectPath
    use :: error
    use :: allocate
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

#ifdef _MPI
    integer(int32), parameter :: root = 0
#endif

    public :: Input

    type :: Input
        private
        character(256) :: Basic_FileName, COO_FileName, BC_FileName, Obs_FileName, ObsFlag_FileName, Top_FileName, IC_FileName

        ! Basic.in
        ! Basic section
        type(Basic_params) :: Basic
        type(Type_Region), allocatable :: Regions(:)
        type(Type_Solver) :: Solver
        type(Type_VTK) :: VTK

    contains

        procedure :: Input_Parameters => Inout_Input_Parameters_JSON
        procedure :: Input_Geometry => Inout_Input_Geometry_VTK

        procedure, pass :: Input_Get_Basic_Params => Inout_Input_Get_Basic_Params
        procedure, pass :: Input_Get_Regional_Params => Inout_Input_Get_Regional_Params
        procedure, pass :: Input_Get_DP3d => Inout_Input_Get_DP3d
        procedure, pass :: Input_Get_int32 => Inout_Input_Get_int32
        procedure, pass :: Input_Get_int32_rank1 => Inout_Input_Get_int32_rank1
        procedure, pass :: Input_Get_int32_rank2 => Inout_Input_Get_int32_rank2
        generic :: Get => Input_Get_Basic_Params, Input_Get_Regional_Params, Input_Get_int32, Input_Get_int32_rank1, Input_Get_int32_rank2, Input_Get_DP3d

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

        ! Path settings
        dir_Path = GetProjectPath()

        Input_Constructor%Basic_FileName = trim(adjustl(dir_Path))//"Input/Basic.in"
        Input_Constructor%BC_FileName = trim(adjustl(dir_Path))//"Input/BC.in"
        Input_Constructor%IC_FileName = trim(adjustl(dir_Path))//"Input/IC.in"
        Input_Constructor%Obs_FileName = trim(adjustl(dir_Path))//"Input/Obs.in"
        Input_Constructor%ObsFlag_FileName = trim(adjustl(dir_Path))//"Input/printobs.in"
        Input_Constructor%Top_FileName = trim(adjustl(dir_Path))//"Input/top.in"
        Input_Constructor%COO_FileName = trim(adjustl(dir_Path))//"Input/coordinate.in"

        ! Check the existence of the file
        status = access(Input_Constructor%Basic_FileName, "r")
        if (status /= 0) call error_message(901, opt_file_name=Input_Constructor%Basic_FileName)

        status = access(Input_Constructor%BC_FileName, "r")
        if (status /= 0) call error_message(901, opt_file_name=Input_Constructor%BC_FileName)

        status = access(Input_Constructor%IC_FileName, "r")
        if (status /= 0) call error_message(901, opt_file_name=Input_Constructor%IC_FileName)

        status = access(Input_Constructor%Obs_FileName, "r")
        if (status /= 0) call error_message(901, opt_file_name=Input_Constructor%Obs_FileName)

        status = access(Input_Constructor%ObsFlag_FileName, "r")
        if (status /= 0) call error_message(901, opt_file_name=Input_Constructor%ObsFlag_FileName)

        status = access(Input_Constructor%Top_FileName, "r")
        if (status /= 0) call error_message(901, opt_file_name=Input_Constructor%Top_FileName)

        status = access(Input_Constructor%COO_FileName, "r")
        if (status /= 0) call error_message(901, opt_file_name=Input_Constructor%COO_FileName)

        call Input_Constructor%Input_Parameters()
        call Input_Constructor%Input_Geometry()

        ! call Input_Constructor%Input_Vertices()
        ! call Input_Constructor%Input_BC()
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

        call json%load(filename='/workspaces/FTDSS/Inout/new-toml/Basic.json')
        call json%print_error_message(output_unit)

        ! call json%print(output_unit)
        ! call json%print_error_message(output_unit)

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
        logical, intent(in) :: isHeat !! Heat calculation
        logical, intent(in) :: isWater !! Water calculation
        logical, intent(in) :: isStress !! Stress calculation

        self%Regions(iRegion)%Flags%isHeat = isHeat
        self%Regions(iRegion)%Flags%isWater = isWater
        self%Regions(iRegion)%Flags%isStress = isStress

    end subroutine Inout_Input_Parameters_JSON_SetCalculationTypes

    subroutine Inout_Input_Parameters_JSON_SetFlags(self, iRegion, is1Phase, is2Phase, is3Phase, isCompression, isFrostHeavePressure, isDispersity)
        !> Set the calculation flags
        implicit none
        class(Input) :: self
        integer(int32), intent(in) :: iRegion !! Region number
        logical, intent(in) :: is1Phase !! 1 Phase calculation
        logical, intent(in) :: is2Phase !! 2 Phase calculation
        logical, intent(in) :: is3Phase !! 3 Phase calculation
        logical, intent(in), optional :: isCompression !! consideer the water/ice compression
        logical, intent(in), optional :: isFrostHeavePressure !! Frost heave pressure calculation
        logical, intent(in), optional :: isDispersity !! Thermalc onductivity dispersity calculation

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

    ! subroutine Inout_Input_Parameters(self)
    !     implicit none
    !     class(Input) :: self
    !     integer(int32) :: status, unit_num
    !     integer(int32) :: iRegion
    !     integer(int32) :: id, ii
    !     character(256) :: c_dummy

    !     open (newunit=unit_num, file=self%Basic_FileName, status="old", action="read", iostat=status)
    !     if (status /= 0) call error_message(902, opt_file_name=self%Basic_FileName)

    !     read (unit_num, *)
    !     read (unit_num, *)
    !     read (unit_num, *)
    !     read (unit_num, *)
    !     read (unit_num, *) Self%Elements, Self%Nodes, Self%Shape, Self%Dimemsion, Self%Region
    !     read (unit_num, *)
    !     read (unit_num, *) self%Time_Unit, self%Calculation_Time, self%dt, self%Output_Interval_Time, self%StandardOutput, self%OutputFile

    !     call Allocate_Matrix(self%Work_Region_Basic_Infomatin, 2, Self%Region)
    !     call Allocate_Matrix(self%Work_Region_Parameters_Number, 10, Self%Region)
    !     call Allocate_Matrix(self%Work_Region_Parameters_int32, 10, Self%Region)
    !     call Allocate_Matrix(self%Work_Region_Paremeters_real64, 50, Self%Region)
    !     do iRegion = 1, Self%Region
    !         read (unit_num, *)
    !         read (unit_num, *) c_dummy, self%Work_Region_Basic_Infomatin(1, iRegion), self%Work_Region_Basic_Infomatin(2, iRegion)
    !         read (unit_num, *)
    !         if (.not. value_in_range(self%Work_Region_Basic_Infomatin(1, iRegion), min_calculation_type, max_calculation_type)) then
    !             call error_message(903, copt1="calculation type")
    !         end if
    !         if (.not. value_in_range(self%Work_Region_Basic_Infomatin(2, iRegion), min_model_type, max_model_type)) then
    !             if (self%Work_Region_Basic_Infomatin(2, iRegion) /= 20 .and. self%Work_Region_Basic_Infomatin(2, iRegion) /= 30) then
    !                 call error_message(903, copt1="model type")
    !             end if
    !         end if
    !         ! end do
    !         ! end do

    !         ! Heat parameters input
    !         if (mod(self%Work_Region_Basic_Infomatin(1, iRegion), 8) >= 4) then
    !             id = 1
    !             ii = 1

    !             read (unit_num, *)
    !             read (unit_num, *)
    !             read (unit_num, *)
    !             select case (self%Work_Region_Basic_Infomatin(2, iRegion))
    !             case (min_model_type:max_model_type)
    !                 ! Porosity and Latent Heat
    !                 read (unit_num, *)
    !                 read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 1, iRegion)
    !                 id = id + 2

    !                 ! Density
    !                 read (unit_num, *)
    !                 read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 2, iRegion)
    !                 id = id + 3

    !                 ! Specific Heat
    !                 read (unit_num, *)
    !                 read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 2, iRegion)
    !                 id = id + 3

    !                 ! Thermal Conductivity
    !                 read (unit_num, *)
    !                 if (mod(self%Work_Region_Basic_Infomatin(2, iRegion) - min_model_type, 2) == 0) then
    !                     read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 2, iRegion)
    !                     id = id + 3
    !                 else
    !                     read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 4, iRegion)
    !                     id = id + 5
    !                 end if

    !                 ! bulk modulus
    !                 if (mod(self%Work_Region_Basic_Infomatin(2, iRegion) - min_model_type, 4) >= 2) then
    !                     read (unit_num, *)
    !                     read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 1, iRegion)
    !                     id = id + 2
    !                 end if

    !                 ! Qice type
    !                 read (unit_num, *)
    !                 read (unit_num, *) self%Work_Region_Parameters_int32(ii, iRegion)
    !                 ii = ii + 1
    !                 select case (self%Work_Region_Parameters_int32(ii - 1, iRegion))
    !                 case (1)
    !                     ! TRM
    !                     read (unit_num, *)
    !                     read (unit_num, *) self%Work_Region_Paremeters_real64(id, iRegion)
    !                     id = id + 1

    !                 case (2)
    !                     ! GCC Model
    !                     ! SWC type
    !                     read (unit_num, *)
    !                     read (unit_num, *) self%Work_Region_Parameters_int32(ii, iRegion)

    !                     read (unit_num, *)

    !                     select case (self%Work_Region_Parameters_int32(ii, iRegion))
    !                     case (1:3)
    !                         ! BC, VG, KO
    !                         read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 4, iRegion)
    !                         id = id + 5

    !                     case (4)
    !                         ! MVG
    !                         read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 5, iRegion)
    !                         id = id + 6

    !                     case (5)
    !                         ! Durner
    !                         read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 8, iRegion)
    !                         id = id + 9

    !                     case (6)
    !                         ! Dual-VG-CH
    !                         read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 7, iRegion)
    !                         id = id + 8
    !                     case default
    !                         call error_message(903, copt1="SWC type")
    !                     end select

    !                     ii = ii + 1
    !                 case (3)
    !                     ! Power Model
    !                     read (unit_num, *)
    !                     read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 1, iRegion)
    !                     id = id + 2
    !                 case default
    !                     call error_message(903, copt1="Qice type")
    !                 end select
    !             case (20)
    !                 ! Density
    !                 read (unit_num, *)
    !                 read (unit_num, *) self%Work_Region_Paremeters_real64(id, iRegion)
    !                 id = id + 1

    !                 ! Specific Heat
    !                 read (unit_num, *)
    !                 read (unit_num, *) self%Work_Region_Paremeters_real64(id, iRegion)
    !                 id = id + 1

    !                 ! Thermal Conductivity
    !                 read (unit_num, *)
    !                 read (unit_num, *) self%Work_Region_Paremeters_real64(id, iRegion)
    !                 id = id + 1
    !             case (30)
    !                 ! Porosity
    !                 read (unit_num, *)
    !                 read (unit_num, *) self%Work_Region_Paremeters_real64(id, iRegion)
    !                 id = id + 1

    !                 ! Density
    !                 read (unit_num, *)
    !                 read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 1, iRegion)
    !                 id = id + 2

    !                 ! Specific Heat
    !                 read (unit_num, *)
    !                 read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 1, iRegion)
    !                 id = id + 2

    !                 ! Thermal Conductivity
    !                 read (unit_num, *)
    !                 read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 1, iRegion)
    !                 id = id + 2
    !             end select
    !             self%Work_Region_Parameters_Number(1, iRegion) = id - 1
    !             self%Work_Region_Parameters_Number(2, iRegion) = ii - 1
    !         end if

    !         ! Water parameters input
    !         if (mod(self%Work_Region_Basic_Infomatin(1, iRegion), 4) >= 2) then

    !             read (unit_num, *)
    !             read (unit_num, *)
    !             read (unit_num, *)

    !             ! krType
    !             read (unit_num, *)
    !             read (unit_num, *) self%Work_Region_Parameters_int32(ii, iRegion)
    !             ii = ii + 1

    !             read (unit_num, *)
    !             select case (self%Work_Region_Parameters_int32(ii - 1, iRegion))
    !             case (10)
    !                 ! Hydraulic Conductivity
    !                 read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 1, iRegion)
    !                 id = id + 2
    !             case (21:26, 31:36)
    !                 select case (mod(self%Work_Region_Parameters_int32(ii - 1, iRegion), 10))
    !                 case (1:3)
    !                     ! BC, VG, KO
    !                     read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 6, iRegion)
    !                     id = id + 7

    !                 case (4)
    !                     ! MVG
    !                     read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 7, iRegion)
    !                     id = id + 8

    !                 case (5)
    !                     ! Durner
    !                     read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 10, iRegion)
    !                     id = id + 11

    !                 case (6)
    !                     ! Dual-VG-CH
    !                     read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 9, iRegion)
    !                     id = id + 10
    !                 end select
    !             case (41:46, 51:56)
    !                 select case (mod(self%Work_Region_Parameters_int32(ii - 1, iRegion), 10))
    !                 case (1:3)
    !                     ! BC, VG, KO
    !                     read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 6, iRegion)
    !                     id = id + 7

    !                 case (4)
    !                     ! MVG
    !                     read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 8, iRegion)
    !                     id = id + 9

    !                 case (5)
    !                     ! Durner
    !                     read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 11, iRegion)
    !                     id = id + 12

    !                 case (6)
    !                     ! Dual-VG-CH
    !                     read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 10, iRegion)
    !                     id = id + 11
    !                 case default
    !                     call error_message(903, copt1="SWC type")
    !                 end select

    !             end select

    !             self%Work_Region_Parameters_Number(3, iRegion) = id - self%Work_Region_Parameters_Number(1, iRegion) - 1
    !             self%Work_Region_Parameters_Number(4, iRegion) = ii - self%Work_Region_Parameters_Number(2, iRegion) - 1
    !         end if
    !     end do

    !     read (unit_num, *)
    !     read (unit_num, *)
    !     read (unit_num, *)

    !     ! Time Discretization
    !     read (unit_num, *)
    !     read (unit_num, *) self%Time_Discretization

    !     ! Heat Solver
    !     if (any(mod(self%Work_Region_Basic_Infomatin(1, :), 8) >= 4)) then
    !         read (unit_num, *)
    !         read (unit_num, *)
    !         read (unit_num, *)
    !         read (unit_num, *)
    !         read (unit_num, *) self%SolverDI(1)

    !         select case (self%SolverDI(1))
    !         case (1)
    !             read (unit_num, *)
    !             read (unit_num, *) self%Solve_Type(1), self%Solve_Pre(1), self%Solve_Maxiter(1), self%Solve_Tol(1)
    !         end select
    !     end if

    !     ! Water Solver
    !     if (any(mod(self%Work_Region_Basic_Infomatin(1, :), 4) >= 2)) then
    !         read (unit_num, *)
    !         read (unit_num, *)
    !         read (unit_num, *)
    !         read (unit_num, *)
    !         read (unit_num, *) self%SolverDI(2)

    !         select case (self%SolverDI(2))
    !         case (1)
    !             read (unit_num, *)
    !             read (unit_num, *) self%Solve_Type(2), self%Solve_Pre(2), self%Solve_Maxiter(2), self%Solve_Tol(2)
    !         end select
    !     end if

    !     close (unit_num)
    ! end subroutine Inout_Input_Parameters

    subroutine Inout_Input_Geometry_VTK(self)
        !> Load the geometry from the VTK file
        implicit none
        class(Input) :: self

        call Inout_VTK_Read('/workspaces/FTDSS/Inout/new-toml/Geometry.vtk', self%VTK)
        ! stop
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

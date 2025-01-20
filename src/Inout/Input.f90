module Inout_Input
    use, intrinsic :: iso_fortran_env, only: int32, real64, output_unit
    use :: Inout_SetProjectPath, only:GetProjectPath => Inout_SetProjectPath_GetProjectPath
    use :: error
    use :: allocate
    use :: Allocate_Structure, only:Allocate_Structure_Thermal_Type, Allocate_Structure_Ice_Type, Allocate_Structure_WRF_Type, Allocate_Structure_Hydraulic_Type
    use :: Types
    use :: tomlf
    use :: json_module
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
    character(*), parameter :: ShapeName = "Shape"
    character(*), parameter :: DimensionName = "Dimension"
    character(*), parameter :: RegionName = "Region"
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
        integer(int32) :: Elements, Nodes, Shape, Dimemsion, Region
        integer(int32) :: StandardOutput, OutputFile
        real(real64) :: Calculation_Time, dt, Output_Interval_Time
        character(3) :: Time_Unit
        ! Region
        integer(int32), allocatable :: Work_Region_Basic_Infomatin(:, :)
        integer(int32), allocatable :: Work_Region_Parameters_Number(:, :)
        integer(int32), allocatable :: Work_Region_Parameters_int32(:, :)
        real(real64), allocatable :: Work_Region_Paremeters_real64(:, :)

        integer(int32) :: SolverDI(3), Solve_Type(3), Solve_Pre(3), Solve_Maxiter(3)
        real(real64) :: Time_Discretization
        real(real64) :: Solve_Tol(3)

        ! Coordinate.in
        integer(int32) :: COO_Dimension
        real(real64), allocatable :: Work_Coordinates(:, :)
        integer(int32), allocatable :: Work_Coordinates_Region(:)

        ! Top.in
        integer(int32), allocatable :: Work_Top(:, :)
        integer(int32), allocatable :: Work_Top_Regions(:)

        ! BC.in
        ! Nodes BC information
        integer(int32) :: Num_BC_Node, Num_BC_Node_Type, Num_NBC_Type
        integer(int32), allocatable :: Work_NBC_Node(:), Work_NBC_Node_Type(:), Work_NBC_Node_Value_Info(:, :)
        real(real64), allocatable :: Work_NBC_Node_Value(:, :)
        ! Edeges BC information
        integer(int32) :: Num_BC_Edge, Num_BC_Edge_Type, Num_EBC_Edge
        integer(int32), allocatable :: Work_EBC_Edge(:, :), Work_EBC_Edge_Type(:), Work_EBC_Edge_Value_Info(:, :)
        real(real64), allocatable :: Work_EBC_Edge_Value(:, :)

        ! IC.in
        integer(int32) :: IC_Type
        integer(int32), allocatable :: Work_IC_Type(:)
        real(real64), allocatable :: Work_IC_Value(:)

        ! Obs.in
        integer(int32) :: Observation_Type, Num_Observation
        integer(int32), allocatable :: Work_Observation_Node(:)
        real(real64), allocatable :: Work_Observation_Coordinate(:, :)

        ! printobs.in
        integer(int32) :: Num_Observation_Flag
        integer(int32), allocatable :: Work_Observation_Flag(:)

#ifdef _MPI
        integer(int32) :: myrank, ierr
#endif

    contains

        procedure :: Input_Parameters => Inout_Input_Parameters_JSON
        procedure :: Input_Coodinates => Inout_Input_Coodinates
        procedure :: Input_Vertices => Inout_Input_Vertices
        procedure :: Input_BC => Inout_Input_BC
        procedure :: Input_IC => Inout_Input_IC
        procedure :: Input_Observation => Inout_Input_Observation
        procedure :: Input_Flags => Inout_Input_Flags

        procedure :: Input_Get_Elements => Inout_Input_Get_Elements
        procedure :: Input_Get_Nodes => Inout_Input_Get_Nodes
        procedure :: Input_Get_Shape => Inout_Input_Get_Shape
        procedure :: Input_Get_Dimemsion => Inout_Input_Get_Dimension
        procedure :: Input_Get_Region => Inout_Input_Get_Regions
        procedure :: Input_Get_Standard_Output => Inout_Input_Get_Standard_Output
        procedure :: Input_Get_Output_File => Inout_Input_Get_Output_File
        procedure :: Input_Get_Observation_Flag => Inout_Input_Get_Observation_Flag
        procedure :: Input_Get_Top => Inout_Input_Get_Top
        procedure :: Input_Get_Top_Region => Inout_Input_Get_Top_Region

        procedure :: Input_Get_Coordinates => Inout_Input_Get_Coordinates

        ! procedure :: Input_Get_BC_Node   => Inout_Input_Get_BC_Node
        procedure :: Input_Get_Coordinates_Region => Inout_Input_Get_Coordinates_Region

        procedure :: Input_Get_BC_Node => Inout_Input_Get_BC_Node
        procedure :: Input_Get_BC_Node_Type => Inout_Input_Get_BC_Node_Type
        procedure :: Input_Get_BC_Node_Value_Info => Inout_Input_Get_BC_Node_Value_Info
        procedure :: Input_Get_BC_Node_Value => Inout_Input_Get_BC_Node_Value

        procedure :: Input_Get_BC_Edge => Inout_Input_Get_BC_Edge
        procedure :: Input_Get_BC_Edge_Type => Inout_Input_Get_BC_Edge_Type

        final :: Inout_Input_Finalize

    end type Input

    interface Input
        module procedure Input_Constructor
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

#ifdef _MPI
        call MPI_Comm_rank(MPI_COMM_WORLD, Input_Constructor%myrank, Input_Constructor%ierr)
#endif

        call Input_Constructor%Input_Parameters()
        call Input_Constructor%Input_Coodinates()
        call Input_Constructor%Input_Vertices()
        call Input_Constructor%Input_BC()
        call Input_Constructor%Input_IC()
        call Input_Constructor%Input_Observation()
        call Input_Constructor%Input_Flags()

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

        call json%print(output_unit)
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
        ! if (self%Regions(iRegion)%Flags%isHeat) then
        call Inout_Input_Parameters_JSON_Solver(self, json)
        ! end if
        ! if (self%Regions(iRegion)%Flags%isWater) then
        !     call Inout_Input_Parameters_JSON_Hydraulic(self, json, iRegion)
        ! end if

        stop
    end subroutine Inout_Input_Parameters_JSON

    subroutine Inout_Input_Parameters_JSON_Basic(self, json)
        !> Load the basic input parameters from the JSON file
        implicit none
        class(Input) :: self
        type(json_file), intent(inout) :: json !! JSON parser
        character(256) :: key

        write (key, '(3a)') BasicName, ".", ElementName
        call json%get(key, self%Basic%Element)
        call json%print_error_message(output_unit)

        write (key, '(3a)') BasicName, ".", NodeName
        call json%get(key, self%Basic%Node)
        call json%print_error_message(output_unit)

        write (key, '(3a)') BasicName, ".", ShapeName
        call json%get(key, self%Basic%Shape)
        call json%print_error_message(output_unit)

        write (key, '(3a)') BasicName, ".", DimensionName
        call json%get(key, self%Basic%Dim)
        call json%print_error_message(output_unit)

        write (key, '(3a)') BasicName, ".", RegionName
        call json%get(key, self%Basic%Region)
        call json%print_error_message(output_unit)

        write (key, '(5a)') BasicName, ".", CalculationName, ".", timeUnitName
        call json%get(key, self%Basic%Calculation_timeUnit)
        call json%print_error_message(output_unit)

        write (key, '(5a)') BasicName, ".", CalculationName, ".", stepName
        call json%get(key, self%Basic%Calculation_step)
        call json%print_error_message(output_unit)

        write (key, '(5a)') BasicName, ".", InputName, ".", timeUnitName
        call json%get(key, self%Basic%Input_timeUnit)
        call json%print_error_message(output_unit)

        write (key, '(5a)') BasicName, ".", InputName, ".", calculationPeriodName
        call json%get(key, self%Basic%CalculationPeriod)
        call json%print_error_message(output_unit)

        write (key, '(5a)') BasicName, ".", OutputName, ".", timeUnitName
        call json%get(key, self%Basic%Output_timeUnit)
        call json%print_error_message(output_unit)

        write (key, '(5a)') BasicName, ".", IntervalName, ".", timeUnitName
        call json%get(key, self%Basic%Interval_timeUnit)
        call json%print_error_message(output_unit)

        write (key, '(5a)') BasicName, ".", IntervalName, ".", stepName
        call json%get(key, self%Basic%Interval)
        call json%print_error_message(output_unit)

        write (key, '(3a)') BasicName, ".", isDisplayPromptName
        call json%get(key, self%Basic%isDisplayPrompt)
        call json%print_error_message(output_unit)

        write (key, '(3a)') BasicName, ".", FileOutputName
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
        character(256) :: key

        write (region_name, '(a, i0)') RegionName, iRegion

        write (key, '(3a)') trim(region_name), ".", CalculationTypeName
        call json%get(key, self%Regions(iRegion)%CalculationType)
        call json%print_error_message(output_unit)

        write (key, '(3a)') trim(region_name), ".", ModelnumberName
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

        write (key, '(3a)') trim(region_name), ".", isFrozenName
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
        character(256) :: key

        call Allocate_Structure_Thermal_Type(self%Regions(iRegion)%Thermal, self%Regions(iRegion)%Flags)

        write (region_name, '(a, i0)') RegionName, iRegion
        if (self%Regions(iRegion)%Flags%isFrozen) then
            if (self%Regions(iRegion)%Flags%is3Phase) then
                write (key, '(5a)') trim(region_name), ".", ThermalName, ".", PorosityName
                call json%get(key, self%Regions(iRegion)%Thermal%Porosity)
                call json%print_error_message(output_unit)
            end if
        end if

        select type (Density => self%Regions(iRegion)%Thermal%Density)
        type is (Type_Density_1Phase)
            write (key, '(5a)') trim(region_name), ".", ThermalName, ".", DensityName
            call json%get(key, Density%Phase1)
            call json%print_error_message(output_unit)
        type is (Type_Density_2Phase)
            write (key, '(7a)') trim(region_name), ".", ThermalName, ".", DensityName, ".", Phase1Name
            call json%get(key, Density%Phase1)
            call json%print_error_message(output_unit)

            write (key, '(7a)') trim(region_name), ".", ThermalName, ".", DensityName, ".", Phase2Name
            call json%get(key, Density%Phase2)
            call json%print_error_message(output_unit)
        type is (Type_Density_3Phase)
            write (key, '(7a)') trim(region_name), ".", ThermalName, ".", DensityName, ".", SoilName
            call json%get(key, Density%Soil)
            call json%print_error_message(output_unit)

            write (key, '(7a)') trim(region_name), ".", ThermalName, ".", DensityName, ".", WaterName
            call json%get(key, Density%Water)
            call json%print_error_message(output_unit)

            write (key, '(7a)') trim(region_name), ".", ThermalName, ".", DensityName, ".", IceName
            call json%get(key, Density%Ice)
            call json%print_error_message(output_unit)
        end select

        select type (SpecificHeat => self%Regions(iRegion)%Thermal%SpecificHeat)
        type is (Type_SpecificHeat_1Phase)
            write (key, '(5a)') trim(region_name), ".", ThermalName, ".", SpecificHeatName
            call json%get(key, SpecificHeat%Phase1)
            call json%print_error_message(output_unit)
        type is (Type_SpecificHeat_2Phase)
            write (key, '(7a)') trim(region_name), ".", ThermalName, ".", SpecificHeatName, ".", Phase1Name
            call json%get(key, SpecificHeat%Phase1)
            call json%print_error_message(output_unit)

            write (key, '(7a)') trim(region_name), ".", ThermalName, ".", SpecificHeatName, ".", Phase2Name
            call json%get(key, SpecificHeat%Phase2)
            call json%print_error_message(output_unit)
        type is (Type_SpecificHeat_3Phase)
            write (key, '(7a)') trim(region_name), ".", ThermalName, ".", SpecificHeatName, ".", SoilName
            call json%get(key, SpecificHeat%Soil)
            call json%print_error_message(output_unit)

            write (key, '(7a)') trim(region_name), ".", ThermalName, ".", SpecificHeatName, ".", WaterName
            call json%get(key, SpecificHeat%Water)
            call json%print_error_message(output_unit)

            write (key, '(7a)') trim(region_name), ".", ThermalName, ".", SpecificHeatName, ".", IceName
            call json%get(key, SpecificHeat%Ice)
            call json%print_error_message(output_unit)
        end select

        select type (ThermalConductivity => self%Regions(iRegion)%Thermal%ThermalConductivity)
        type is (Type_ThermalConductivity_1Phase)
            write (key, '(7a)') trim(region_name), ".", ThermalName, ".", ThermalConductivityName
            call json%get(key, ThermalConductivity%Phase1)
            call json%print_error_message(output_unit)
        type is (Type_ThermalConductivity_2Phase)
            write (key, '(9a)') trim(region_name), ".", ThermalName, ".", ThermalConductivityName, ".", Phase1Name
            call json%get(key, ThermalConductivity%Phase1)
            call json%print_error_message(output_unit)

            write (key, '(9a)') trim(region_name), ".", ThermalName, ".", ThermalConductivityName, ".", Phase2Name
            call json%get(key, ThermalConductivity%Phase2)
            call json%print_error_message(output_unit)
        type is (Type_ThermalConductivity_3Phase)
            write (key, '(9a)') trim(region_name), ".", ThermalName, ".", ThermalConductivityName, ".", SoilName
            call json%get(key, ThermalConductivity%Soil)
            call json%print_error_message(output_unit)

            write (key, '(9a)') trim(region_name), ".", ThermalName, ".", ThermalConductivityName, ".", WaterName
            call json%get(key, ThermalConductivity%Water)
            call json%print_error_message(output_unit)

            write (key, '(9a)') trim(region_name), ".", ThermalName, ".", ThermalConductivityName, ".", IceName
            call json%get(key, ThermalConductivity%Ice)
            call json%print_error_message(output_unit)
        type is (Type_ThermalConductivity_3Phase_Dispersity_2D)
            write (key, '(9a)') trim(region_name), ".", ThermalName, ".", ThermalConductivityName, ".", SoilName
            call json%get(key, ThermalConductivity%Soil)
            call json%print_error_message(output_unit)

            write (key, '(9a)') trim(region_name), ".", ThermalName, ".", ThermalConductivityName, ".", WaterName
            call json%get(key, ThermalConductivity%Water)
            call json%print_error_message(output_unit)

            write (key, '(9a)') trim(region_name), ".", ThermalName, ".", ThermalConductivityName, ".", IceName
            call json%get(key, ThermalConductivity%Ice)
            call json%print_error_message(output_unit)

            write (key, '(11a)') trim(region_name), ".", ThermalName, ".", ThermalConductivityName, ".", dispersityName, ".", xName
            call json%get(key, ThermalConductivity%dispersity%x)
            call json%print_error_message(output_unit)

            write (key, '(11a)') trim(region_name), ".", ThermalName, ".", ThermalConductivityName, ".", dispersityName, ".", yName
            call json%get(key, ThermalConductivity%dispersity%y)
            call json%print_error_message(output_unit)
        type is (Type_ThermalConductivity_3Phase_Dispersity_3D)
            write (key, '(9a)') trim(region_name), ".", ThermalName, ".", ThermalConductivityName, ".", SoilName
            call json%get(key, ThermalConductivity%Soil)
            call json%print_error_message(output_unit)

            write (key, '(9a)') trim(region_name), ".", ThermalName, ".", ThermalConductivityName, ".", WaterName
            call json%get(key, ThermalConductivity%Water)
            call json%print_error_message(output_unit)

            write (key, '(9a)') trim(region_name), ".", ThermalName, ".", ThermalConductivityName, ".", IceName
            call json%get(key, ThermalConductivity%Ice)
            call json%print_error_message(output_unit)

            write (key, '(11a)') trim(region_name), ".", ThermalName, ".", ThermalConductivityName, ".", dispersityName, ".", xName
            call json%get(key, ThermalConductivity%dispersity%x)
            call json%print_error_message(output_unit)

            write (key, '(11a)') trim(region_name), ".", ThermalName, ".", ThermalConductivityName, ".", dispersityName, ".", yName
            call json%get(key, ThermalConductivity%dispersity%y)
            call json%print_error_message(output_unit)

            write (key, '(11a)') trim(region_name), ".", ThermalName, ".", ThermalConductivityName, ".", dispersityName, ".", zName
            call json%get(key, ThermalConductivity%dispersity%z)
            call json%print_error_message(output_unit)
        end select

        if (self%Regions(iRegion)%Flags%isFrozen) then
            write (key, '(7a)') trim(region_name), ".", ThermalName, ".", IceName, ".", QiceTypeName
            call json%get(key, QiceType)
            call json%print_error_message(output_unit)

            call Allocate_Structure_Ice_Type(self%Regions(iRegion)%Thermal, QiceType)

            select type (Ice => self%Regions(iRegion)%Thermal%Ice)
            type is (Type_Ice_TRM)
                write (key, '(7a)') trim(region_name), ".", ThermalName, ".", IceName, ".", TfName
                call json%get(key, Ice%Tf)
                call json%print_error_message(output_unit)
            type is (Type_Ice_GCC)
                write (key, '(5a)') trim(region_name), ".", ThermalName, ".", LatentHeatName
                call json%get(key, self%Regions(iRegion)%Thermal%Ice%LatentHeat)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", ThermalName, ".", IceName, ".", TfName
                call json%get(key, Ice%Tf)
                call json%print_error_message(output_unit)

                write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", ModelName
                call json%get(key, Ice%ModelType)
                call json%print_error_message(output_unit)

                call Allocate_Structure_WRF_Type(self%Regions(iRegion)%Thermal, Ice%ModelType)

                select type (WRF => Ice%WRF)
                type is (Type_WRF_BC)
                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", thetaSName
                    call json%get(key, WRF%thetaS)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", thetaRName
                    call json%get(key, WRF%thetaR)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", alpha1Name
                    call json%get(key, WRF%alpha1)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", n1Name
                    call json%get(key, WRF%n1)
                    call json%print_error_message(output_unit)
                type is (Type_WRF_VG)
                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", thetaSName
                    call json%get(key, WRF%thetaS)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", thetaRName
                    call json%get(key, WRF%thetaR)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", alpha1Name
                    call json%get(key, WRF%alpha1)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", n1Name
                    call json%get(key, WRF%n1)
                    call json%print_error_message(output_unit)

                    WRF%m1 = 1.0 - 1.0 / WRF%n1
                type is (Type_WRF_KO)
                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", thetaSName
                    call json%get(key, WRF%thetaS)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", thetaRName
                    call json%get(key, WRF%thetaR)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", alpha1Name
                    call json%get(key, WRF%alpha1)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", n1Name
                    call json%get(key, WRF%n1)
                    call json%print_error_message(output_unit)

                type is (Type_WRF_MVG)
                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", thetaSName
                    call json%get(key, WRF%thetaS)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", thetaRName
                    call json%get(key, WRF%thetaR)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", alpha1Name
                    call json%get(key, WRF%alpha1)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", n1Name
                    call json%get(key, WRF%n1)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", hcritName
                    call json%get(key, WRF%hcrit)
                    call json%print_error_message(output_unit)

                    WRF%m1 = 1.0 - 1.0 / WRF%n1

                type is (Type_WRF_Durner)
                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", thetaSName
                    call json%get(key, WRF%thetaS)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", thetaRName
                    call json%get(key, WRF%thetaR)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", alpha1Name
                    call json%get(key, WRF%alpha1)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", n1Name
                    call json%get(key, WRF%n1)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", alpha2Name
                    call json%get(key, WRF%alpha2)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", n2Name
                    call json%get(key, WRF%n2)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", w1Name
                    call json%get(key, WRF%w1)
                    call json%print_error_message(output_unit)

                    WRF%m1 = 1.0 - 1.0 / WRF%n1
                    WRF%m2 = 1.0 - 1.0 / WRF%n2
                    WRF%w2 = 1.0 - WRF%w1
                type is (Type_WRF_DVGCH)
                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", thetaSName
                    call json%get(key, WRF%thetaS)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", thetaRName
                    call json%get(key, WRF%thetaR)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", alpha1Name
                    call json%get(key, WRF%alpha1)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", n1Name
                    call json%get(key, WRF%n1)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", n2Name
                    call json%get(key, WRF%n2)
                    call json%print_error_message(output_unit)

                    write (key, '(9a)') trim(region_name), ".", ThermalName, ".", IceName, ".", ParametersName, ".", w1Name
                    call json%get(key, WRF%w1)
                    call json%print_error_message(output_unit)

                    WRF%m1 = 1.0 - 1.0 / WRF%n1
                    WRF%m2 = 1.0 - 1.0 / WRF%n2
                    WRF%w2 = 1.0 - WRF%w1
                end select

            type is (Type_Ice_EXP)
                write (key, '(7a)') trim(region_name), ".", ThermalName, ".", IceName, ".", TfName
                call json%get(key, Ice%Tf)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", ThermalName, ".", IceName, ".", AName
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
        character(256) :: key

        write (region_name, '(a, i0)') RegionName, iRegion

        write (key, '(5a)') trim(region_name), ".", HydraulicName, ".", useHCFName
        call json%get(key, self%Regions(iRegion)%Hydraulic%useHCF)
        call json%print_error_message(output_unit)

        write (key, '(5a)') trim(region_name), ".", HydraulicName, ".", useImpedanceName
        call json%get(key, self%Regions(iRegion)%Hydraulic%useImpedance)
        call json%print_error_message(output_unit)

        write (key, '(5a)') trim(region_name), ".", HydraulicName, ".", useKTDynamicsName
        call json%get(key, self%Regions(iRegion)%Hydraulic%useKTDynamics)
        call json%print_error_message(output_unit)

        call Allocate_Structure_Hydraulic_Type(self%Regions(iRegion)%Hydraulic)

        write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", KsName
        call json%get(key, self%Regions(iRegion)%Hydraulic%Ks)
        call json%print_error_message(output_unit)

        if (allocated(self%Regions(iRegion)%Hydraulic%HCF)) then
            select type (HCF => self%Regions(iRegion)%Hydraulic%HCF)
            type is (Type_HCF_BC)
                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", thetaSName
                call json%get(key, HCF%thetaS)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", thetaRName
                call json%get(key, HCF%thetaR)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", alpha1Name
                call json%get(key, HCF%alpha1)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", n1Name
                call json%get(key, HCF%n1)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", lName
                call json%get(key, HCF%l)
                call json%print_error_message(output_unit)

            type is (Type_HCF_VG)
                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", thetaSName
                call json%get(key, HCF%thetaS)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", thetaRName
                call json%get(key, HCF%thetaR)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", alpha1Name
                call json%get(key, HCF%alpha1)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", n1Name
                call json%get(key, HCF%n1)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", lName
                call json%get(key, HCF%l)
                call json%print_error_message(output_unit)

                HCF%m1 = 1.0 - 1.0 / HCF%n1

            type is (Type_HCF_KO)
                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", thetaSName
                call json%get(key, HCF%thetaS)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", thetaRName
                call json%get(key, HCF%thetaR)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", alpha1Name
                call json%get(key, HCF%alpha1)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", n1Name
                call json%get(key, HCF%n1)
                call json%print_error_message(output_unit)

            type is (Type_HCF_MVG)
                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", thetaSName
                call json%get(key, HCF%thetaS)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", thetaRName
                call json%get(key, HCF%thetaR)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", alpha1Name
                call json%get(key, HCF%alpha1)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", n1Name
                call json%get(key, HCF%n1)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", hcritName
                call json%get(key, HCF%hcrit)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", lName
                call json%get(key, HCF%l)
                call json%print_error_message(output_unit)

                HCF%m1 = 1.0 - 1.0 / HCF%n1

            type is (Type_HCF_Durner)
                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", thetaSName
                call json%get(key, HCF%thetaS)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", thetaRName
                call json%get(key, HCF%thetaR)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", alpha1Name
                call json%get(key, HCF%alpha1)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", n1Name
                call json%get(key, HCF%n1)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", alpha2Name
                call json%get(key, HCF%alpha2)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", n2Name
                call json%get(key, HCF%n2)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", w1Name
                call json%get(key, HCF%w1)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", lName
                call json%get(key, HCF%l)
                call json%print_error_message(output_unit)

                HCF%m1 = 1.0 - 1.0 / HCF%n1
                HCF%m2 = 1.0 - 1.0 / HCF%n2
                HCF%w2 = 1.0 - HCF%w1

            type is (Type_HCF_DVGCH)
                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", thetaSName
                call json%get(key, HCF%thetaS)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", thetaRName
                call json%get(key, HCF%thetaR)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", alpha1Name
                call json%get(key, HCF%alpha1)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", n1Name
                call json%get(key, HCF%n1)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", n2Name
                call json%get(key, HCF%n2)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", w1Name
                call json%get(key, HCF%w1)
                call json%print_error_message(output_unit)

                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", lName
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
                write (key, '(7a)') trim(region_name), ".", HydraulicName, ".", ParametersName, ".", OmegaName
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

        character(256) :: key
        integer(int32) :: useSolver

        write (key, '(3a)') SolveName, ".", TimeDiscretizationName
        call json%get(key, self%Basic%TimeDiscretization)
        call json%print_error_message(output_unit)
        print *, self%Basic%TimeDiscretization
        if (any(self%Regions(:)%Flags%isHeat)) then
            write (key, '(5a)') SolveName, ".", ThermalName, ".", useSolverName
            call json%get(key, useSolver)
            call json%print_error_message(output_unit)

            call Inout_Input_Parameters_JSON_Solver_Settings(self, json, useSolver, ThermalName)
        end if
        if (any(self%Regions(:)%Flags%isWater)) then
            write (key, '(5a)') SolveName, ".", HydraulicName, ".", useSolverName
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

        character(256) :: key
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
                    write (key, '(7a)') SolveName, ".", ThermalName, ".", ParametersName, ".", SolverName
                    call json%get(key, Thermal%SolverType)
                    call json%print_error_message(output_unit)

                    write (key, '(7a)') SolveName, ".", ThermalName, ".", ParametersName, ".", PreconditionerName
                    call json%get(key, Thermal%PreconditionerType)
                    call json%print_error_message(output_unit)

                    write (key, '(7a)') SolveName, ".", ThermalName, ".", ParametersName, ".", MaxIterationName
                    call json%get(key, Thermal%MaxIter)
                    call json%print_error_message(output_unit)

                    write (key, '(7a)') SolveName, ".", ThermalName, ".", ParametersName, ".", ToleranceName
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
                    write (key, '(7a)') SolveName, ".", HydraulicName, ".", ParametersName, ".", SolverName
                    call json%get(key, Hydraulic%SolverType)
                    call json%print_error_message(output_unit)

                    write (key, '(7a)') SolveName, ".", HydraulicName, ".", ParametersName, ".", PreconditionerName
                    call json%get(key, Hydraulic%PreconditionerType)
                    call json%print_error_message(output_unit)

                    write (key, '(7a)') SolveName, ".", HydraulicName, ".", ParametersName, ".", MaxIterationName
                    call json%get(key, Hydraulic%MaxIter)
                    call json%print_error_message(output_unit)

                    write (key, '(7a)') SolveName, ".", HydraulicName, ".", ParametersName, ".", ToleranceName
                    call json%get(key, Hydraulic%Tol)
                    call json%print_error_message(output_unit)
                    print *, Hydraulic%SolverType, Hydraulic%PreconditionerType, Hydraulic%MaxIter, Hydraulic%Tol
                class default
                    ! エラー処理
                    write (*, '(A)') "Error: Unexpected type assigned to self%Solver%Hydraulic"
                    stop
                end select
            end select
        end select

    end subroutine Inout_Input_Parameters_JSON_Solver_Settings

    subroutine Inout_Input_Parameters(self)
        implicit none
        class(Input) :: self
        integer(int32) :: status, unit_num
        integer(int32) :: iRegion
        integer(int32) :: id, ii
        character(256) :: c_dummy

#ifdef _MPI
        if (self%myrank == 0) then
#endif
            open (newunit=unit_num, file=self%Basic_FileName, status="old", action="read", iostat=status)
            if (status /= 0) call error_message(902, opt_file_name=self%Basic_FileName)

            read (unit_num, *)
            read (unit_num, *)
            read (unit_num, *)
            read (unit_num, *)
            read (unit_num, *) Self%Elements, Self%Nodes, Self%Shape, Self%Dimemsion, Self%Region
            read (unit_num, *)
            read (unit_num, *) self%Time_Unit, self%Calculation_Time, self%dt, self%Output_Interval_Time, self%StandardOutput, self%OutputFile

            call Allocate_Matrix(self%Work_Region_Basic_Infomatin, 2, Self%Region)
            call Allocate_Matrix(self%Work_Region_Parameters_Number, 10, Self%Region)
            call Allocate_Matrix(self%Work_Region_Parameters_int32, 10, Self%Region)
            call Allocate_Matrix(self%Work_Region_Paremeters_real64, 50, Self%Region)
            do iRegion = 1, Self%Region
                read (unit_num, *)
                read (unit_num, *) c_dummy, self%Work_Region_Basic_Infomatin(1, iRegion), self%Work_Region_Basic_Infomatin(2, iRegion)
                read (unit_num, *)
                if (.not. value_in_range(self%Work_Region_Basic_Infomatin(1, iRegion), min_calculation_type, max_calculation_type)) then
                    call error_message(903, copt1="calculation type")
                end if
                if (.not. value_in_range(self%Work_Region_Basic_Infomatin(2, iRegion), min_model_type, max_model_type)) then
                    if (self%Work_Region_Basic_Infomatin(2, iRegion) /= 20 .and. self%Work_Region_Basic_Infomatin(2, iRegion) /= 30) then
                        call error_message(903, copt1="model type")
                    end if
                end if
                ! end do
                ! end do

                ! Heat parameters input
                if (mod(self%Work_Region_Basic_Infomatin(1, iRegion), 8) >= 4) then
                    id = 1
                    ii = 1

                    read (unit_num, *)
                    read (unit_num, *)
                    read (unit_num, *)
                    select case (self%Work_Region_Basic_Infomatin(2, iRegion))
                    case (min_model_type:max_model_type)
                        ! Porosity and Latent Heat
                        read (unit_num, *)
                        read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 1, iRegion)
                        id = id + 2

                        ! Density
                        read (unit_num, *)
                        read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 2, iRegion)
                        id = id + 3

                        ! Specific Heat
                        read (unit_num, *)
                        read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 2, iRegion)
                        id = id + 3

                        ! Thermal Conductivity
                        read (unit_num, *)
                        if (mod(self%Work_Region_Basic_Infomatin(2, iRegion) - min_model_type, 2) == 0) then
                            read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 2, iRegion)
                            id = id + 3
                        else
                            read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 4, iRegion)
                            id = id + 5
                        end if

                        ! bulk modulus
                        if (mod(self%Work_Region_Basic_Infomatin(2, iRegion) - min_model_type, 4) >= 2) then
                            read (unit_num, *)
                            read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 1, iRegion)
                            id = id + 2
                        end if

                        ! Qice type
                        read (unit_num, *)
                        read (unit_num, *) self%Work_Region_Parameters_int32(ii, iRegion)
                        ii = ii + 1
                        select case (self%Work_Region_Parameters_int32(ii - 1, iRegion))
                        case (1)
                            ! TRM
                            read (unit_num, *)
                            read (unit_num, *) self%Work_Region_Paremeters_real64(id, iRegion)
                            id = id + 1

                        case (2)
                            ! GCC Model
                            ! SWC type
                            read (unit_num, *)
                            read (unit_num, *) self%Work_Region_Parameters_int32(ii, iRegion)

                            read (unit_num, *)

                            select case (self%Work_Region_Parameters_int32(ii, iRegion))
                            case (1:3)
                                ! BC, VG, KO
                                read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 4, iRegion)
                                id = id + 5

                            case (4)
                                ! MVG
                                read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 5, iRegion)
                                id = id + 6

                            case (5)
                                ! Durner
                                read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 8, iRegion)
                                id = id + 9

                            case (6)
                                ! Dual-VG-CH
                                read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 7, iRegion)
                                id = id + 8
                            case default
                                call error_message(903, copt1="SWC type")
                            end select

                            ii = ii + 1
                        case (3)
                            ! Power Model
                            read (unit_num, *)
                            read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 1, iRegion)
                            id = id + 2
                        case default
                            call error_message(903, copt1="Qice type")
                        end select
                    case (20)
                        ! Density
                        read (unit_num, *)
                        read (unit_num, *) self%Work_Region_Paremeters_real64(id, iRegion)
                        id = id + 1

                        ! Specific Heat
                        read (unit_num, *)
                        read (unit_num, *) self%Work_Region_Paremeters_real64(id, iRegion)
                        id = id + 1

                        ! Thermal Conductivity
                        read (unit_num, *)
                        read (unit_num, *) self%Work_Region_Paremeters_real64(id, iRegion)
                        id = id + 1
                    case (30)
                        ! Porosity
                        read (unit_num, *)
                        read (unit_num, *) self%Work_Region_Paremeters_real64(id, iRegion)
                        id = id + 1

                        ! Density
                        read (unit_num, *)
                        read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 1, iRegion)
                        id = id + 2

                        ! Specific Heat
                        read (unit_num, *)
                        read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 1, iRegion)
                        id = id + 2

                        ! Thermal Conductivity
                        read (unit_num, *)
                        read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 1, iRegion)
                        id = id + 2
                    end select
                    self%Work_Region_Parameters_Number(1, iRegion) = id - 1
                    self%Work_Region_Parameters_Number(2, iRegion) = ii - 1
                end if

                ! Water parameters input
                if (mod(self%Work_Region_Basic_Infomatin(1, iRegion), 4) >= 2) then

                    read (unit_num, *)
                    read (unit_num, *)
                    read (unit_num, *)

                    ! krType
                    read (unit_num, *)
                    read (unit_num, *) self%Work_Region_Parameters_int32(ii, iRegion)
                    ii = ii + 1

                    read (unit_num, *)
                    select case (self%Work_Region_Parameters_int32(ii - 1, iRegion))
                    case (10)
                        ! Hydraulic Conductivity
                        read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 1, iRegion)
                        id = id + 2
                    case (21:26, 31:36)
                        select case (mod(self%Work_Region_Parameters_int32(ii - 1, iRegion), 10))
                        case (1:3)
                            ! BC, VG, KO
                            read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 6, iRegion)
                            id = id + 7

                        case (4)
                            ! MVG
                            read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 7, iRegion)
                            id = id + 8

                        case (5)
                            ! Durner
                            read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 10, iRegion)
                            id = id + 11

                        case (6)
                            ! Dual-VG-CH
                            read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 9, iRegion)
                            id = id + 10
                        end select
                    case (41:46, 51:56)
                        select case (mod(self%Work_Region_Parameters_int32(ii - 1, iRegion), 10))
                        case (1:3)
                            ! BC, VG, KO
                            read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 6, iRegion)
                            id = id + 7

                        case (4)
                            ! MVG
                            read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 8, iRegion)
                            id = id + 9

                        case (5)
                            ! Durner
                            read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 11, iRegion)
                            id = id + 12

                        case (6)
                            ! Dual-VG-CH
                            read (unit_num, *) self%Work_Region_Paremeters_real64(id:id + 10, iRegion)
                            id = id + 11
                        case default
                            call error_message(903, copt1="SWC type")
                        end select

                    end select

                    self%Work_Region_Parameters_Number(3, iRegion) = id - self%Work_Region_Parameters_Number(1, iRegion) - 1
                    self%Work_Region_Parameters_Number(4, iRegion) = ii - self%Work_Region_Parameters_Number(2, iRegion) - 1
                end if
            end do

            read (unit_num, *)
            read (unit_num, *)
            read (unit_num, *)

            ! Time Discretization
            read (unit_num, *)
            read (unit_num, *) self%Time_Discretization

            ! Heat Solver
            if (any(mod(self%Work_Region_Basic_Infomatin(1, :), 8) >= 4)) then
                read (unit_num, *)
                read (unit_num, *)
                read (unit_num, *)
                read (unit_num, *)
                read (unit_num, *) self%SolverDI(1)

                select case (self%SolverDI(1))
                case (1)
                    read (unit_num, *)
                    read (unit_num, *) self%Solve_Type(1), self%Solve_Pre(1), self%Solve_Maxiter(1), self%Solve_Tol(1)
                end select
            end if

            ! Water Solver
            if (any(mod(self%Work_Region_Basic_Infomatin(1, :), 4) >= 2)) then
                read (unit_num, *)
                read (unit_num, *)
                read (unit_num, *)
                read (unit_num, *)
                read (unit_num, *) self%SolverDI(2)

                select case (self%SolverDI(2))
                case (1)
                    read (unit_num, *)
                    read (unit_num, *) self%Solve_Type(2), self%Solve_Pre(2), self%Solve_Maxiter(2), self%Solve_Tol(2)
                end select
            end if

            close (unit_num)
#ifdef _MPI
        end if
            !!! FIXME: Bcastについては後で実装する
#endif
    end subroutine Inout_Input_Parameters

    subroutine Inout_Input_Coodinates(self)
        implicit none
        class(Input) :: self
        integer(int32) :: status, unit_num, iN, ierr
        real(real64) :: d_dummy
        character(64) :: c_dummy

#ifdef _MPI
        if (self%myrank == root) then
#endif

            open (newunit=unit_num, file=self%COO_FileName, status="old", action="read", iostat=status)
            if (status /= 0) call error_message(902, opt_file_name=self%COO_FileName)
            read (unit_num, *)
            read (unit_num, *) c_dummy, self%COO_Dimension
            read (unit_num, *)
            if (.not. value_in_range(self%COO_Dimension, min_Coordinate_Dimesion_type, max_Coordinate_Dimesion_type)) then
                call error_message(903, copt1="Coordinate Dimension type")
            end if

#ifdef _MPI
        end if
        call MPI_Bcast(self%COO_Dimension, 1, MPI_INTEGER, root, MPI_COMM_WORLD, ierr)
#endif

        if (self%COO_Dimension == 3) then
            call Allocate_Matrix(self%Work_Coordinates, self%Nodes, 3)
        else
            call Allocate_Matrix(self%Work_Coordinates, self%Nodes, 2)
        end if
        call Allocate_Vector(self%Work_Coordinates_Region, self%Nodes)

#ifdef _MPI
        if (self%myrank == root) then
#endif

            read (unit_num, *)
            if (self%COO_Dimension == 1) then
                do iN = 1, self%Nodes
                    read (unit_num, *) self%Work_Coordinates(iN, 1), self%Work_Coordinates(iN, 2), d_dummy, self%Work_Coordinates_Region(iN)
                end do
            else if (self%COO_Dimension == 2) then
                do iN = 1, self%Nodes
                    read (unit_num, *) self%Work_Coordinates(iN, 1), d_dummy, self%Work_Coordinates(iN, 2), self%Work_Coordinates_Region(iN)
                end do
            else if (self%COO_Dimension == 3) then
                do iN = 1, self%Nodes
                    read (unit_num, *) self%Work_Coordinates(iN, 1), self%Work_Coordinates(iN, 2), self%Work_Coordinates(iN, 3), self%Work_Coordinates_Region(iN)
                end do
            end if

            close (unit_num)
#ifdef _MPI
        end if
            !!! FIXME: Bcastについては後で実装する
#endif
    end subroutine Inout_Input_Coodinates

    subroutine Inout_Input_Vertices(self)
        implicit none
        class(Input) :: self
        integer(int32) :: status, unit_num, iElem

        if (self%Shape == 3 .and. self%Dimemsion == 1) call Allocate_Matrix(self%Work_Top, 3, self%Elements)
        call Allocate_Vector(self%Work_Top_Regions, self%Elements)

#ifdef _MPI
        if (self%myrank == root) then
#endif

            open (newunit=unit_num, file=self%Top_FileName, status="old", action="read", iostat=status)
            if (status /= 0) call error_message(902, opt_file_name=self%Top_FileName)

            read (unit_num, *)
            if (self%Shape == 3 .and. self%Dimemsion == 1) then
                do iElem = 1, self%Elements
                    read (unit_num, *) self%Work_Top(1, iElem), self%Work_Top(2, iElem), self%Work_Top(3, iElem), self%Work_Top_Regions(iElem)
                end do
            end if
            close (unit_num)

#ifdef _MPI
        end if
            !!! FIXME: Bcastについては後で実装するx
#endif
    end subroutine Inout_Input_Vertices

    subroutine Inout_Input_BC(self)
        implicit none
        class(Input) :: self
        integer(int32) :: status, unit_num
        integer(int32) :: iNBC, iEBC
        character(256) :: c_dummy

#ifdef _MPI
        if (self%myrank == root) then
#endif

            open (newunit=unit_num, file=self%BC_FileName, status="old", action="read", iostat=status)
            if (status /= 0) call error_message(902, opt_file_name=self%BC_FileName)

            read (unit_num, *)
            read (unit_num, *)
            read (unit_num, *) self%Num_BC_Node, self%Num_BC_Node_Type
            read (unit_num, *)
            read (unit_num, *) c_dummy, self%Num_NBC_Type

#ifdef _MPI
        end if

        call MPI_Bcast(self%Num_BC_Node, 1, MPI_INTEGER, root, MPI_COMM_WORLD, ierr)
        call MPI_Bcast(self%Num_NBC_Type, 1, MPI_INTEGER, root, MPI_COMM_WORLD, ierr)
#endif

        call Allocate_Vector(self%Work_NBC_Node, self%Num_BC_Node)
        call Allocate_Vector(self%Work_NBC_Node_Type, self%Num_BC_Node)

        call Allocate_Matrix(self%Work_NBC_Node_Value_Info, self%Num_NBC_Type, 2)
        call Allocate_Matrix(self%Work_NBC_Node_Value, self%Num_NBC_Type, 3)

#ifdef _MPI
        if (self%myrank == root) then
#endif
            do iNBC = 1, self%Num_NBC_Type
                read (unit_num, *) self%Work_NBC_Node_Value_Info(iNBC, 1:2), self%Work_NBC_Node_Value(iNBC, 1:3)
            end do
            read (unit_num, *)
            read (unit_num, *)
            do iNBC = 1, self%Num_BC_Node
                read (unit_num, *) self%Work_NBC_Node(iNBC), self%Work_NBC_Node_Type(iNBC)
            end do

            read (unit_num, *)
            read (unit_num, *)
            read (unit_num, *) self%Num_BC_Edge, self%Num_BC_Edge_Type
            read (unit_num, *)
            read (unit_num, *) c_dummy, self%Num_EBC_Edge

#ifdef _MPI
        end if

        call MPI_Bcast(self%Num_BC_Edge, 1, MPI_INTEGER, root, MPI_COMM_WORLD, ierr)
        call MPI_Bcast(self%Num_EBC_Edge, 1, MPI_INTEGER, root, MPI_COMM_WORLD, ierr)
#endif

        call Allocate_Matrix(self%Work_EBC_Edge, self%Num_BC_Edge, 2)
        call Allocate_Vector(self%Work_EBC_Edge_Type, self%Num_BC_Edge)

        call Allocate_Matrix(self%Work_EBC_Edge_Value_Info, self%Num_EBC_Edge, 5)
        call Allocate_Matrix(self%Work_EBC_Edge_Value, self%Num_EBC_Edge, 6)

#ifdef _MPI
        if (self%myrank == root) then
#endif
            do iEBC = 1, self%Num_EBC_Edge
                read (unit_num, *) self%Work_EBC_Edge_Value_Info(iEBC, 1:5)

                if (mod(self%Work_EBC_Edge_Value_Info(iEBC, 3), 10) /= 0) then
                    read (unit_num, *) self%Work_EBC_Edge_Value(iEBC, 1), self%Work_EBC_Edge_Value(iEBC, 2)
                end if
                if (mod(self%Work_EBC_Edge_Value_Info(iEBC, 4), 10) /= 0) then
                    read (unit_num, *) self%Work_EBC_Edge_Value(iEBC, 3), self%Work_EBC_Edge_Value(iEBC, 4)
                end if
                if (mod(self%Work_EBC_Edge_Value_Info(iEBC, 5), 10) /= 0) then
                    read (unit_num, *) self%Work_EBC_Edge_Value(iEBC, 5), self%Work_EBC_Edge_Value(iEBC, 6)
                end if
            end do
            read (unit_num, *)

            do iEBC = 1, self%Num_BC_Edge
                read (unit_num, *) self%Work_EBC_Edge(iEBC, 1:2), self%Work_EBC_Edge_Type(iEBC)
            end do
            close (unit_num)

#ifdef _MPI
        end if
            !!! FIXME: Bcastについては後で実装する
#endif
    end subroutine Inout_Input_BC

    subroutine Inout_Input_IC(self)
        implicit none
        class(Input) :: self
        integer(int32) :: status, unit_num
        character(256) :: c_dummy

#ifdef _MPI
        if (self%myrank == root) then
#endif

            open (newunit=unit_num, file=self%IC_FileName, status="old", action="read", iostat=status)
            if (status /= 0) call error_message(902, opt_file_name=self%IC_FileName)

            read (unit_num, *)
            read (unit_num, *) c_dummy, self%IC_Type
            read (unit_num, *)
            read (unit_num, *)

#ifdef _MPI
        end if
        call MPI_Bcast(self%IC_Type, 1, MPI_INTEGER, root, MPI_COMM_WORLD, ierr)
#endif

        if (self%IC_Type == 1) then
            call Allocate_Vector(self%Work_IC_Type, 3)
            call Allocate_Vector(self%Work_IC_Value, 3)
        end if

#ifdef _MPI
        if (self%myrank == root) then
#endif

            if (self%IC_Type == 1) then
                read (unit_num, *) self%Work_IC_Type(1), self%Work_IC_Value(1)
                read (unit_num, *) self%Work_IC_Type(2), self%Work_IC_Value(2)
                read (unit_num, *) self%Work_IC_Type(3), self%Work_IC_Value(3)
            end if

            close (unit_num)

#ifdef _MPI
        end if
            !!! FIXME: Bcastについては後で実装する
#endif

    end subroutine Inout_Input_IC

    subroutine Inout_Input_Observation(self)
        implicit none
        class(Input) :: self
        integer(int32) :: status, unit_num, iObs

#ifdef _MPI
        if (self%myrank == root) then
#endif

            open (newunit=unit_num, file=self%Obs_FileName, status="old", action="read", iostat=status)
            if (status /= 0) call error_message(902, opt_file_name=self%Obs_FileName)

            read (unit_num, *)
            read (unit_num, *) self%Observation_Type
            read (unit_num, *)
            read (unit_num, *) self%Num_Observation
            read (unit_num, *)

            if (self%Observation_Type == 1) then
                call Allocate_Vector(self%Work_Observation_Node, self%Num_Observation)
                do iObs = 1, self%Num_Observation
                    read (unit_num, *) self%Work_Observation_Node(iObs)
                end do
            else if (self%Observation_Type == 2) then
                call Allocate_Matrix(self%Work_Observation_Coordinate, self%Num_Observation, 3)
                if (self%COO_Dimension == 1) then
                    do iObs = 1, self%Num_Observation
                        read (unit_num, *) &
                            self%Work_Observation_Coordinate(iObs, 1), &
                            self%Work_Observation_Coordinate(iObs, 2)
                    end do
                else if (self%COO_Dimension == 2) then
                    do iObs = 1, self%Num_Observation
                        read (unit_num, *) &
                            self%Work_Observation_Coordinate(iObs, 1), &
                            self%Work_Observation_Coordinate(iObs, 3)
                    end do
                else if (self%COO_Dimension == 3) then
                    do iObs = 1, self%Num_Observation
                        read (unit_num, *) &
                            self%Work_Observation_Coordinate(iObs, 1), &
                            self%Work_Observation_Coordinate(iObs, 2), &
                            self%Work_Observation_Coordinate(iObs, 3)
                    end do
                end if
            end if
            close (unit_num)

#ifdef _MPI
        end if
            !!! FIXME: Bcastについては後で実装する
#endif

    end subroutine Inout_Input_Observation

    subroutine Inout_Input_Flags(self)
        implicit none
        class(Input) :: self
        integer(int32) :: unit_num, status
        integer(int32) :: iFlag
        character(256) :: c_dummy

#ifdef _MPI
        if (self%myrank == root) then
#endif

            open (newunit=unit_num, file=self%ObsFlag_FileName, status="old", action="read", iostat=status)
            if (status /= 0) call error_message(902, opt_file_name=self%ObsFlag_FileName)

            read (unit_num, *)
            read (unit_num, *) self%Num_Observation_Flag
            read (unit_num, *)

            call Allocate_Vector(self%Work_Observation_Flag, self%Num_Observation_Flag)

            do iFlag = 1, self%Num_Observation_Flag
                read (unit_num, *) c_dummy, self%Work_Observation_Flag(iFlag)
            end do

            close (unit_num)

#ifdef _MPI
        end if
            !!! FIXME: Bcastについては後で実装する
#endif

    end subroutine Inout_Input_Flags

    subroutine Inout_Input_Finalize(self)
        implicit none
        type(Input) :: self

        if (allocated(self%Work_Region_Basic_Infomatin)) deallocate (self%Work_Region_Basic_Infomatin)
        if (allocated(self%Work_Region_Paremeters_real64)) deallocate (self%Work_Region_Paremeters_real64)
        if (allocated(self%Work_Region_Parameters_int32)) deallocate (self%Work_Region_Parameters_int32)
        if (allocated(self%Work_Region_Parameters_Number)) deallocate (self%Work_Region_Parameters_Number)
        if (allocated(self%Work_Coordinates)) deallocate (self%Work_Coordinates)
        if (allocated(self%Work_Coordinates_Region)) deallocate (self%Work_Coordinates_Region)
        if (allocated(self%Work_Top)) deallocate (self%Work_Top)
        if (allocated(self%Work_NBC_Node)) deallocate (self%Work_NBC_Node)
        if (allocated(self%Work_NBC_Node_Type)) deallocate (self%Work_NBC_Node_Type)
        if (allocated(self%Work_NBC_Node_Value_Info)) deallocate (self%Work_NBC_Node_Value_Info)
        if (allocated(self%Work_NBC_Node_Value)) deallocate (self%Work_NBC_Node_Value)
        if (allocated(self%Work_EBC_Edge)) deallocate (self%Work_EBC_Edge)
        if (allocated(self%Work_EBC_Edge_Type)) deallocate (self%Work_EBC_Edge_Type)
        if (allocated(self%Work_EBC_Edge_Value_Info)) deallocate (self%Work_EBC_Edge_Value_Info)
        if (allocated(self%Work_EBC_Edge_Value)) deallocate (self%Work_EBC_Edge_Value)
        if (allocated(self%Work_IC_Type)) deallocate (self%Work_IC_Type)
        if (allocated(self%Work_IC_Value)) deallocate (self%Work_IC_Value)
        if (allocated(self%Work_Observation_Node)) deallocate (self%Work_Observation_Node)
        if (allocated(self%Work_Observation_Coordinate)) deallocate (self%Work_Observation_Coordinate)
        if (allocated(self%Work_Observation_Flag)) deallocate (self%Work_Observation_Flag)

    end subroutine Inout_Input_Finalize

    function Inout_Input_Get_Nodes(self) result(iNodes)
        implicit none
        class(Input) :: self
        integer(int32) :: iNodes

        iNodes = self%Nodes
    end function Inout_Input_Get_Nodes

    function Inout_Input_Get_Elements(self) result(iElements)
        implicit none
        class(Input) :: self
        integer(int32) :: iElements

        iElements = self%Elements
    end function Inout_Input_Get_Elements

    function Inout_Input_Get_Shape(self) result(iShape)
        implicit none
        class(Input) :: self
        integer(int32) :: iShape

        iShape = self%Shape
    end function Inout_Input_Get_Shape

    function Inout_Input_Get_Dimension(self) result(iDimension)
        implicit none
        class(Input) :: self
        integer(int32) :: iDimension

        iDimension = self%Dimemsion
    end function Inout_Input_Get_Dimension

    function Inout_Input_Get_Regions(self) result(iRegions)
        implicit none
        class(Input) :: self
        integer(int32) :: iRegions

        iRegions = self%Region
    end function Inout_Input_Get_Regions

    function Inout_Input_Get_Standard_Output(self) result(iStandard_Output)
        implicit none
        class(Input) :: self
        integer(int32) :: iStandard_Output

        iStandard_Output = self%StandardOutput
    end function Inout_Input_Get_Standard_Output

    function Inout_Input_Get_Output_File(self) result(iOutputFile)
        implicit none
        class(Input) :: self
        integer(int32) :: iOutputFile

        iOutputFile = self%OutputFile
    end function Inout_Input_Get_Output_File

    function Inout_Input_Get_Observation_Flag(self) result(arr_Observation_Flag)
        implicit none
        class(Input) :: self
        integer(int32), allocatable :: arr_Observation_Flag(:)

        allocate (arr_Observation_Flag(self%Num_Observation_Flag))
        arr_Observation_Flag = self%Work_Observation_Flag
    end function Inout_Input_Get_Observation_Flag

    function Inout_Input_Get_Top(self) result(arr_Top)
        implicit none
        class(Input) :: self
        integer(int32), allocatable :: arr_Top(:, :)

        ! print *, self%Work_Region_Basic_Infomatin(1, :)

        if (allocated(arr_Top)) then
            call error_message(953)
        else
#ifdef _MPI
            ! MPI用の処理，分割して割当てる
#else
            allocate (arr_Top, source=self%Work_Top)
#endif
        end if
    end function Inout_Input_Get_Top

    function Inout_Input_Get_Top_Region(self) result(arr_Top_Regions)
        implicit none
        class(Input) :: self
        integer(int32), allocatable :: arr_Top_Regions(:)

        if (allocated(arr_Top_Regions)) then
            call error_message(953)
        else
#ifdef _MPI
            ! MPI用の処理，分割して割当てる
#else
            allocate (arr_Top_Regions, source=self%Work_Top_Regions)
#endif
        end if
        ! print *, self%Work_Top_Regions
    end function Inout_Input_Get_Top_Region

    function Inout_Input_Get_Coordinates_DP2d(self) result(arr_Coordinates)
        implicit none
        class(Input) :: self
        type(DP2d) :: arr_Coordinates

        if (allocated(arr_Coordinates%x) .or. allocated(arr_Coordinates%y)) then
            call error_message(953)
        else
#ifdef _MPI
            ! MPI用の処理，分割して割当てる
#else
            allocate (arr_Coordinates%x, source=self%Work_Coordinates(:, 1))
            allocate (arr_Coordinates%y, source=self%Work_Coordinates(:, 2))
#endif
        end if
    end function Inout_Input_Get_Coordinates_DP2d

    function Inout_Input_Get_Coordinates(self) result(arr_Coordinates)
        implicit none
        class(Input) :: self
        type(DP2d) :: arr_Coordinates

        select case (self%COO_Dimension)
        case (1:2)
            arr_Coordinates = Inout_Input_Get_Coordinates_DP2d(self)
        case (3)
            ! 3次元の場合の処理
        end select
    end function Inout_Input_Get_Coordinates

    function Inout_Input_Get_Coordinates_Region(self) result(arr_Coordinates_Region)
        implicit none
        class(Input) :: self
        integer(int32), allocatable :: arr_Coordinates_Region(:)

        if (allocated(arr_Coordinates_Region)) then
            call error_message(953)
        else
#ifdef _MPI
            ! MPI用��処理，分���して割当てる
#else
            allocate (arr_Coordinates_Region, source=self%Work_Coordinates_Region)
#endif
        end if

    end function Inout_Input_Get_Coordinates_Region

    function Inout_Input_Get_BC_Node(self) result(arr_BC_Node)
        implicit none
        class(Input) :: self
        integer(int32), allocatable :: arr_BC_Node(:)

        if (allocated(arr_BC_Node)) then
            call error_message(953)
        else
#ifdef _MPI
            ! MPI用の処理，分割して割当てる
#else
            allocate (arr_BC_Node, source=self%Work_NBC_Node)
#endif
        end if
    end function Inout_Input_Get_BC_Node

    function Inout_Input_Get_BC_Node_Type(self) result(arr_BC_Node_Type)
        implicit none
        class(Input) :: self
        integer(int32), allocatable :: arr_BC_Node_Type(:)

        if (allocated(arr_BC_Node_Type)) then
            call error_message(953)
        else
#ifdef _MPI
            ! MPI用の処理，分割して割当てる
#else
            allocate (arr_BC_Node_Type, source=self%Work_NBC_Node_Type)
#endif
        end if
    end function Inout_Input_Get_BC_Node_Type

    function Inout_Input_Get_BC_Node_Value_Info(self) result(arr_BC_Node_Value_Info)
        implicit none
        class(Input) :: self
        integer(int32), allocatable :: arr_BC_Node_Value_Info(:, :)

        if (allocated(arr_BC_Node_Value_Info)) then
            call error_message(953)
        else
#ifdef _MPI
            ! MPI用の処理，分割して割当てる
#else
            allocate (arr_BC_Node_Value_Info, source=self%Work_NBC_Node_Value_Info(:, :))
#endif
        end if
    end function Inout_Input_Get_BC_Node_Value_Info

    function Inout_Input_Get_BC_Node_Value(self, Calc_Type) result(arr_BC_Node_Value)
        implicit none
        class(Input) :: self
        real(real64), allocatable :: arr_BC_Node_Value(:)
        integer(int32), intent(in) :: Calc_Type

        if (allocated(arr_BC_Node_Value)) then
            call error_message(953)
        else
#ifdef _MPI
            ! MPI用の処理，分割して割当てる
#else
            allocate (arr_BC_Node_Value, source=self%Work_NBC_Node_Value(:, Calc_Type))
#endif
        end if
    end function Inout_Input_Get_BC_Node_Value

    function Inout_Input_Get_BC_Edge(self) result(arr_BC_Edge)
        implicit none
        class(Input) :: self
        integer(int32), allocatable :: arr_BC_Edge(:, :)

        if (allocated(arr_BC_Edge)) then
            call error_message(953)
        else
#ifdef _MPI
            ! MPI用の処理，分割して割当てる
#else
            allocate (arr_BC_Edge, source=self%Work_EBC_Edge(:, :))
#endif
        end if
    end function Inout_Input_Get_BC_Edge

    function Inout_Input_Get_BC_Edge_Type(self) result(arr_BC_Edge_Type)
        implicit none
        class(Input) :: self
        integer(int32), allocatable :: arr_BC_Edge_Type(:)

        if (allocated(arr_BC_Edge_Type)) then
            call error_message(953)
        else
#ifdef _MPI
            ! MPI用の処理，分割して割当てる
#else
            allocate (arr_BC_Edge_Type, source=self%Work_EBC_Edge_Type)
#endif
        end if
    end function Inout_Input_Get_BC_Edge_Type

    function Inout_Input_Get_BC_Edge_Value_Info(self) result(arr_BC_Edge_Value_Info)
        implicit none
        class(Input) :: self
        integer(int32), allocatable :: arr_BC_Edge_Value_Info(:, :)

        if (allocated(arr_BC_Edge_Value_Info)) then
            call error_message(953)
        else
#ifdef _MPI
            ! MPI用の処理，分割して割当てる
#else
            allocate (arr_BC_Edge_Value_Info, source=self%Work_EBC_Edge_Value_Info(:, :))
#endif
        end if
    end function Inout_Input_Get_BC_Edge_Value_Info

    function Inout_Input_Get_BC_Edge_Value(self, Calc_Type) result(arr_BC_Edge_Value)
        implicit none
        class(Input) :: self
        real(real64), allocatable :: arr_BC_Edge_Value(:, :)
        integer(int32), intent(in) :: Calc_Type

        if (allocated(arr_BC_Edge_Value)) then
            call error_message(953)
        else
#ifdef _MPI
            ! MPI用の処理，分割して割���てる
#else
            allocate (arr_BC_Edge_Value, source=self%Work_EBC_Edge_Value(:, :))
#endif
        end if
    end function Inout_Input_Get_BC_Edge_Value

end module Inout_Input

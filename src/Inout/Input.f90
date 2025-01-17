module Inout_Input
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: Inout_SetProjectPath, only:GetProjectPath => Inout_SetProjectPath_GetProjectPath
    use :: error
    use :: allocate
    use :: Allocate_Structure, only:Allocate_Structure_Thermal_Type, Allocate_Structure_Ice_Type, Allocate_Structure_WRF_Type
    use :: Types
    use :: tomlf
    implicit none
    private

    integer(int32), parameter :: min_calculation_type = 1, max_calculation_type = 7
    integer(int32), parameter :: min_model_type = 11, max_model_type = 18
    integer(int32), parameter :: min_Coordinate_Dimesion_type = 1, max_Coordinate_Dimesion_type = 3
    character(*), parameter :: BasicName = "Basic"
    character(*), parameter :: ThemalName = "Thermal"
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
    character(*), parameter :: QiceTypeName = "QiceType"
    character(*), parameter :: TfName = "Tf"
    character(*), parameter :: ParametersName = "Parameters"
    character(*), parameter :: ModelName = "Model"
    character(*), parameter :: thetaSName = "thetaS"
    character(*), parameter :: thetaRName = "thetaR"
    character(*), parameter :: alpha1Name = "alpha1"
    character(*), parameter :: n1Name = "n1"
    character(*), parameter :: KTDynamicsName = "KTDynamics"
    character(*), parameter :: ImpedanceName = "Impedance"
    character(*), parameter :: KsName = "Ks"

    character(*), parameter :: AName = "a"

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

        procedure :: Input_Parameters => Inout_Input_Parameters_TOML
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
        character(256) :: dir_Path
        integer(int32) :: access, status

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

    subroutine Inout_Input_Parameters_TOML(self)
        implicit none
        class(Input) :: self
        type(toml_table), allocatable :: table
        type(toml_error), allocatable :: parse_error
        integer(int32) :: status, unit_num
        integer(int32) :: iRegion

        open (newunit=unit_num, file="/workspaces/FTDSS/Inout/new-toml/Basic.toml", status="old", action="read", iostat=status)
        if (status /= 0) call error_message(902, opt_file_name="/workspaces/FTDSS/Inout/new-toml/Basic.toml")
        call toml_parse(table, unit_num, parse_error)
        close (unit=unit_num)
        if (allocated(parse_error)) then
            print *, "Error parsing table:"//parse_error%message
        end if
        call Inout_Input_Parameters_TOML_Basic(self, table)
        if (.not. allocated(self%Regions)) allocate (self%Regions(self%Basic%Region))
        do iRegion = 1, self%Basic%Region
            call Inout_Input_Parameters_TOML_Reigion_Infomation(self, table, iRegion)
            if (self%Regions(iRegion)%Flags%isHeat) then
                call Inout_Input_Parameters_TOML_Thermal(self, table, iRegion)
            end if
        end do

        stop
    end subroutine Inout_Input_Parameters_TOML

    subroutine Inout_Input_Parameters_TOML_Basic(self, table)
        implicit none
        class(Input) :: self
        type(toml_table), intent(inout) :: table
        type(toml_table), pointer :: child => null(), grandchild => null()

        call get_value(table, BasicName, child)
        if (associated(child)) then
            call get_value(child, ElementName, self%Basic%Element)
            call get_value(child, NodeName, self%Basic%Node)
            call get_value(child, ShapeName, self%Basic%Shape)
            call get_value(child, DimensionName, self%Basic%Dim)
            call get_value(child, RegionName, self%Basic%Region)

            call get_value(child, CalculationName, grandchild)
            if (associated(grandchild)) then
                call get_value(grandchild, timeUnitName, self%Basic%Calculation_timeUnit)
                call get_value(grandchild, stepName, self%Basic%Calculation_step)
                nullify (grandchild)
            end if

            call get_value(child, InputName, grandchild)
            if (associated(grandchild)) then
                call get_value(grandchild, timeUnitName, self%Basic%Input_timeUnit)
                call get_value(grandchild, calculationPeriodName, self%Basic%CalculationPeriod)
                nullify (grandchild)
            end if

            call get_value(child, OutputName, grandchild)
            if (associated(grandchild)) then
                call get_value(grandchild, timeUnitName, self%Basic%Output_timeUnit)
                nullify (grandchild)
            end if

            call get_value(child, IntervalName, grandchild)
            if (associated(grandchild)) then
                call get_value(grandchild, timeUnitName, self%Basic%Interval_timeUnit)
                call get_value(grandchild, stepName, self%Basic%Interval)
                nullify (grandchild)
            end if

            call get_value(child, isDisplayPromptName, self%Basic%isDisplayPrompt)
            call get_value(child, FileOutputName, self%Basic%FileOutput)

            nullify (child)
        end if

    end subroutine Inout_Input_Parameters_TOML_Basic

    subroutine Inout_Input_Parameters_TOML_Reigion_Infomation(self, table, iRegion)
        implicit none
        class(Input) :: self
        integer(int32), intent(in) :: iRegion
        type(toml_table), intent(inout) :: table
        type(toml_table), pointer :: child => null()
        character(8) :: region_name

        write (region_name, '(a, i0)') RegionName, iRegion
        call get_value(table, trim(region_name), child)
        if (associated(child)) then
            call get_value(child, CalculationTypeName, self%Regions(iRegion)%CalculationType)
            call get_value(child, ModelnumberName, self%Regions(iRegion)%Modelnumber)
            nullify (child)
        end if

        select case (self%Regions(iRegion)%CalculationType)
        case (1)
            call Inout_Input_Parameters_TOML_SetCalculationTypes(self, iRegion, .false., .false., .true.)
        case (2)
            call Inout_Input_Parameters_TOML_SetCalculationTypes(self, iRegion, .false., .true., .false.)
        case (3)
            call Inout_Input_Parameters_TOML_SetCalculationTypes(self, iRegion, .false., .true., .true.)
        case (4)
            call Inout_Input_Parameters_TOML_SetCalculationTypes(self, iRegion, .true., .false., .false.)
        case (5)
            call Inout_Input_Parameters_TOML_SetCalculationTypes(self, iRegion, .true., .true., .false.)
        case (6)
            call Inout_Input_Parameters_TOML_SetCalculationTypes(self, iRegion, .true., .false., .true.)
        case (7)
            call Inout_Input_Parameters_TOML_SetCalculationTypes(self, iRegion, .true., .true., .true.)
        case default
            call error_message(903, copt1=CalculationTypeName)
        end select

        select case (self%Regions(iRegion)%Modelnumber)
        case (10)
            call Inout_Input_Parameters_TOML_SetFlags(self, iRegion, .true., .false., .false.)
        case (20)
            call Inout_Input_Parameters_TOML_SetFlags(self, iRegion, .false., .true., .false.)
        case (31)
            call Inout_Input_Parameters_TOML_SetFlags(self, iRegion, .false., .false., .true., .false., .false., .false.)
        case (32)
            call Inout_Input_Parameters_TOML_SetFlags(self, iRegion, .false., .false., .true., .false., .false., .true.)
        case (33)
            call Inout_Input_Parameters_TOML_SetFlags(self, iRegion, .false., .false., .true., .false., .true., .false.)
        case (34)
            call Inout_Input_Parameters_TOML_SetFlags(self, iRegion, .false., .false., .true., .false., .true., .true.)
        case (35)
            call Inout_Input_Parameters_TOML_SetFlags(self, iRegion, .false., .false., .true., .true., .false., .false.)
        case (36)
            call Inout_Input_Parameters_TOML_SetFlags(self, iRegion, .false., .false., .true., .true., .false., .true.)
        case (37)
            call Inout_Input_Parameters_TOML_SetFlags(self, iRegion, .false., .false., .true., .true., .true., .false.)
        case (38)
            call Inout_Input_Parameters_TOML_SetFlags(self, iRegion, .false., .false., .true., .true., .true., .true.)
        case default
            call error_message(903, copt1=ModelnumberName)
        end select

    end subroutine Inout_Input_Parameters_TOML_Reigion_Infomation

    subroutine Inout_Input_Parameters_TOML_SetCalculationTypes(self, iRegion, isHeat, isWater, isStress)
        implicit none
        class(Input) :: self
        integer(int32), intent(in) :: iRegion
        logical, intent(in) :: isHeat, isWater, isStress

        self%Regions(iRegion)%Flags%isHeat = isHeat
        self%Regions(iRegion)%Flags%isWater = isWater
        self%Regions(iRegion)%Flags%isStress = isStress

    end subroutine Inout_Input_Parameters_TOML_SetCalculationTypes

    subroutine Inout_Input_Parameters_TOML_SetFlags(self, iRegion, is1Phase, is2Phase, is3Phase, isCompression, isFrostHeavePressure, isDispersity)
        implicit none
        class(Input) :: self
        integer(int32), intent(in) :: iRegion
        logical, intent(in) :: is1Phase, is2Phase, is3Phase
        logical, intent(in), optional :: isCompression, isFrostHeavePressure, isDispersity

        self%Regions(iRegion)%Flags%is1Phase = is1Phase
        self%Regions(iRegion)%Flags%is2Phase = is2Phase
        self%Regions(iRegion)%Flags%is3Phase = is3Phase
        if (present(isCompression)) self%Regions(iRegion)%Flags%isCompression = isCompression
        if (present(isFrostHeavePressure)) self%Regions(iRegion)%Flags%isFrostHeavePressure = isFrostHeavePressure
        if (present(isDispersity)) self%Regions(iRegion)%Flags%isDispersity = isDispersity

    end subroutine Inout_Input_Parameters_TOML_SetFlags

    subroutine Inout_Input_Parameters_TOML_Thermal(self, table, iRegion)
        implicit none
        class(Input) :: self
        integer(int32), intent(in) :: iRegion
        type(toml_table), intent(inout) :: table
        type(toml_table), pointer :: child => null(), grandchild => null(), greatgrandchild => null(), greatgreatgrandchild => null()

        character(8) :: region_name
        integer(int32) :: QiceType

        write (region_name, '(a, i0)') RegionName, iRegion
        call get_value(table, trim(region_name), child)
        call Allocate_Structure_Thermal_Type(self%Regions(iRegion)%Thermal, self%Regions(iRegion)%Flags)
        if (associated(child)) then
            call get_value(child, ThemalName, grandchild)

            if (associated(grandchild)) then
                if (self%Regions(iRegion)%Flags%is3Phase) then
                    call get_value(grandchild, PorosityName, self%Regions(iRegion)%Thermal%Porosity)
                    call get_value(grandchild, LatentHeatName, self%Regions(iRegion)%Thermal%LatentHeat)
                end if

                ! Density input
                select type (Density => self%Regions(iRegion)%Thermal%Density)
                type is (Type_Density_1Phase)
                    call get_value(grandchild, DensityName, Density%Phase1)
                type is (Type_Density_2Phase)
                    call get_value(grandchild, DensityName, greatgrandchild)
                    if (associated(greatgrandchild)) then
                        call get_value(greatgrandchild, Phase1Name, Density%Phase1)
                        call get_value(greatgrandchild, Phase2Name, Density%Phase2)
                        nullify (greatgrandchild)
                    end if
                type is (Type_Density_3Phase)
                    call get_value(grandchild, DensityName, greatgrandchild)
                    if (associated(greatgrandchild)) then
                        call get_value(greatgrandchild, SoilName, Density%Soil)
                        call get_value(greatgrandchild, WaterName, Density%Water)
                        call get_value(greatgrandchild, IceName, Density%Ice)
                        nullify (greatgrandchild)
                    end if
                end select

                ! Specific Heat input
                select type (SpecificHeat => self%Regions(iRegion)%Thermal%SpecificHeat)
                type is (Type_SpecificHeat_1Phase)
                    call get_value(grandchild, SpecificHeatName, SpecificHeat%Phase1)
                type is (Type_SpecificHeat_2Phase)
                    call get_value(grandchild, SpecificHeatName, greatgrandchild)
                    if (associated(greatgrandchild)) then
                        call get_value(greatgrandchild, Phase1Name, SpecificHeat%Phase1)
                        call get_value(greatgrandchild, Phase2Name, SpecificHeat%Phase2)
                        nullify (greatgrandchild)
                    end if
                type is (Type_SpecificHeat_3Phase)
                    call get_value(grandchild, SpecificHeatName, greatgrandchild)
                    if (associated(greatgrandchild)) then
                        call get_value(greatgrandchild, SoilName, SpecificHeat%Soil)
                        call get_value(greatgrandchild, WaterName, SpecificHeat%Water)
                        call get_value(greatgrandchild, IceName, SpecificHeat%Ice)
                        nullify (greatgrandchild)
                    end if
                end select

                ! Thermal Conductivity input
                select type (ThermalConductivity => self%Regions(iRegion)%Thermal%ThermalConductivity)
                type is (Type_ThermalConductivity_1Phase)
                    call get_value(grandchild, ThermalConductivityName, ThermalConductivity%Phase1)
                type is (Type_ThermalConductivity_2Phase)
                    call get_value(grandchild, ThermalConductivityName, greatgrandchild)
                    if (associated(greatgrandchild)) then
                        call get_value(greatgrandchild, Phase1Name, ThermalConductivity%Phase1)
                        call get_value(greatgrandchild, Phase2Name, ThermalConductivity%Phase2)
                        nullify (greatgrandchild)
                    end if
                type is (Type_ThermalConductivity_3Phase)
                    call get_value(grandchild, ThermalConductivityName, greatgrandchild)
                    if (associated(greatgrandchild)) then
                        call get_value(greatgrandchild, SoilName, ThermalConductivity%Soil)
                        call get_value(greatgrandchild, WaterName, ThermalConductivity%Water)
                        call get_value(greatgrandchild, IceName, ThermalConductivity%Ice)
                        nullify (greatgrandchild)
                    end if
                type is (Type_ThermalConductivity_3Phase_Dispersity_2D)
                    call get_value(grandchild, ThermalConductivityName, greatgrandchild)
                    if (associated(greatgrandchild)) then
                        call get_value(greatgrandchild, SoilName, ThermalConductivity%Soil)
                        call get_value(greatgrandchild, WaterName, ThermalConductivity%Water)
                        call get_value(greatgrandchild, IceName, ThermalConductivity%Ice)
                        if (self%Regions(iRegion)%Flags%isDispersity) then
                            call get_value(greatgrandchild, dispersityName, greatgreatgrandchild)
                            if (associated(greatgreatgrandchild)) then
                                call get_value(greatgreatgrandchild, xName, ThermalConductivity%dispersity%x)
                                call get_value(greatgreatgrandchild, yName, ThermalConductivity%dispersity%y)
                                nullify (greatgreatgrandchild)
                            end if
                        end if
                        nullify (greatgrandchild)
                    end if
                end select

                ! Ice input
                call get_value(grandchild, IceName, greatgrandchild)
                if (associated(greatgrandchild)) then
                    ! print *, self%Regions(iRegion)%Thermal%Ice%QiceType
                    call get_value(greatgrandchild, QiceTypeName, QiceType)
                    call Allocate_Structure_Ice_Type(self%Regions(iRegion)%Thermal, QiceType)
                    self%Regions(iRegion)%Thermal%Ice%QiceType = QiceType

                    select type (Ice => self%Regions(iRegion)%Thermal%Ice)
                    type is (Type_Ice_TRM)
                        call get_value(greatgrandchild, TfName, Ice%Tf)
                        nullify (greatgrandchild)
                    type is (Type_Ice_GCC)
                        call get_value(greatgrandchild, TfName, Ice%Tf)
                        call get_value(greatgrandchild, ParametersName, greatgreatgrandchild)
                        if (associated(greatgreatgrandchild)) then
                            call get_value(greatgreatgrandchild, ModelName, Ice%ModelType)
                            call Allocate_Structure_WRF_Type(self%Regions(iRegion)%Thermal, Ice%ModelType)
                            call get_value(greatgreatgrandchild, thetaSName, Ice%WRF%thetaS)
                            call get_value(greatgreatgrandchild, thetaRName, Ice%WRF%thetaR)
                            select type (WRF => Ice%WRF)
                            type is (Type_WRF_BC)
                                call get_value(greatgreatgrandchild, alpha1Name, WRF%alpha1)
                                call get_value(greatgreatgrandchild, n1Name, WRF%n1)
                            type is (Type_WRF_VG)
                                call get_value(greatgreatgrandchild, alpha1Name, WRF%alpha1)
                                call get_value(greatgreatgrandchild, n1Name, WRF%n1)
                                WRF%m1 = 1.0 - 1.0 / WRF%n1

                            end select
                            nullify (greatgreatgrandchild)
                        end if

                    type is (Type_Ice_EXP)
                        call get_value(greatgrandchild, TfName, Ice%Tf)
                        call get_value(greatgrandchild, AName, Ice%a)
                        nullify (greatgrandchild)
                    end select
                end if
            end if
        end if

    end subroutine Inout_Input_Parameters_TOML_Thermal

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
            ! MPI用の処理，分割して割当てる
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
            ! MPI用の処理，分割して割当てる
#else
            allocate (arr_BC_Edge_Value, source=self%Work_EBC_Edge_Value(:, :))
#endif
        end if
    end function Inout_Input_Get_BC_Edge_Value

end module Inout_Input

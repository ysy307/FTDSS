module Inout_Output
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding, only: c_int64_t, c_ptr, c_f_pointer, c_char, c_null_char, c_associated
    use :: Inout_ProjectPath, only:GetProjectPath => Inout_ProjectPath_GetProjectPath
    use :: Core_BaseTypes
    use :: Core_Allocate
    use :: Core_Error
    use :: Core_C_Util
    use :: Inout_Input
    use :: Domain_Element, only:ElementHolder
    use :: Domain_Element_Factory, only:Create_Element
    use :: Properties_Model_Base, only:Proereties_Model_t
    use :: domain_module, only:Domain_t
    use :: Main_Thermal
    use :: Time_Time
    use :: stdlib_strings, only:to_string
#ifdef _OPENMP
    use :: omp_lib
#endif

    implicit none
    private

    ! 個々の観測変数を管理するクラス
    type :: ObservationVariable_t
        character(:), allocatable :: VariableName
        character(:), allocatable :: VariableUnitName
        character(:), allocatable :: FileName
        integer(int32) :: num_unit = -1
        logical(4) :: doOutput = .false.
        procedure(Abst_Calculate_obs_values), pointer, nopass :: get_values => null()
    contains
        procedure, pass(self) :: initialize => ObservationVariable_Initialize
    end type

! In a new or existing module

    interface
        module subroutine ObservationVariable_Initialize(self, dir_Output, VariableName, FileName, VariableUnitName, doOutput)
            implicit none
            class(ObservationVariable_t), intent(inout) :: self
            character(*), intent(in) :: dir_Output
            character(*), intent(in) :: VariableName
            character(*), intent(in) :: FileName
            character(*), intent(in) :: VariableUnitName
            logical(4), intent(in) :: doOutput
        end subroutine ObservationVariable_Initialize

    end interface

    type :: Output_Config
        logical(4) :: doOutput
        character(:), allocatable :: Filename
        character(:), allocatable :: VariableUnit
        integer(int32) :: numUnit
    end type

    type :: Output_Observation
        ! type(ObservationVariable_t) :: Temperature
        ! type(ObservationVariable_t) :: Si
        ! type(ObservationVariable_t) :: TC
        ! type(ObservationVariable_t) :: C
        ! type(ObservationVariable_t) :: Pressure
        ! type(ObservationVariable_t) :: wFlux
        ! type(ObservationVariable_t) :: K
        ! ]
        type(ObservationVariable_t), allocatable :: Variables(:)

        integer(int32) :: ObservationType
        integer(int32) :: NumObservation
        type(DP3d) :: Cood_Obs
        type(ElementHolder), allocatable :: Element(:)
        real(real64), allocatable :: obs_xi(:)
        real(real64), allocatable :: obs_eta(:)
        integer(int32), allocatable :: ObsNodeID(:)
    contains
        procedure, pass(self) :: initialize => Output_Observation_Initialize
        procedure, pass(self) :: Write_Header => Output_Observation_Write_Header
        ! procedure, pass(self) :: Interpolate => Interpolate_ObsValues
    end type

    interface
        module subroutine Output_Observation_Initialize(self, Input, Coordinate, Domain)
            implicit none
            class(Output_Observation), intent(inout) :: self
            type(Type_Input), intent(in) :: Input
            type(DP3d), intent(inout), pointer :: Coordinate
            type(Domain_t), intent(in), optional :: Domain

        end subroutine Output_Observation_Initialize

        module subroutine Output_Observation_Write_Header(self, ObsVar, TimeUnit)
            implicit none
            class(Output_Observation), intent(inout) :: self
            class(ObservationVariable_t), intent(inout) :: ObsVar
            character(*), intent(in) :: TimeUnit

        end subroutine Output_Observation_Write_Header

        module subroutine Interpolate_ObsValues_Temperature(obs_values, observation_data, nodal_temperature, &
                                                            nodal_porosity, nodal_Pw, Properties)
            implicit none
            real(real64), intent(out) :: obs_values(:)
            type(Output_Observation), intent(in) :: observation_data
            real(real64), intent(in), optional :: nodal_temperature(:)
            real(real64), intent(in), optional :: nodal_porosity(:)
            real(real64), intent(in), optional :: nodal_Pw(:)
            type(Proereties_Model_t), intent(in), optional :: Properties

        end subroutine Interpolate_ObsValues_Temperature

        module subroutine Interpolate_ObsValues_Si(obs_values, observation_data, nodal_temperature, &
                                                   nodal_porosity, nodal_Pw, Properties)
            implicit none
            real(real64), intent(out) :: obs_values(:)
            type(Output_Observation), intent(in) :: observation_data
            real(real64), intent(in), optional :: nodal_temperature(:)
            real(real64), intent(in), optional :: nodal_porosity(:)
            real(real64), intent(in), optional :: nodal_Pw(:)
            type(Proereties_Model_t), intent(in), optional :: Properties

        end subroutine Interpolate_ObsValues_Si

        module subroutine Interpolate_ObsValues_THC(obs_values, observation_data, nodal_temperature, &
                                                    nodal_porosity, nodal_Pw, Properties)
            implicit none
            real(real64), intent(out) :: obs_values(:)
            type(Output_Observation), intent(in) :: observation_data
            real(real64), intent(in), optional :: nodal_temperature(:)
            real(real64), intent(in), optional :: nodal_porosity(:)
            real(real64), intent(in), optional :: nodal_Pw(:)
            type(Proereties_Model_t), intent(in), optional :: Properties

        end subroutine Interpolate_ObsValues_THC

        module subroutine Interpolate_ObsValues_VHC(obs_values, observation_data, nodal_temperature, &
                                                    nodal_porosity, nodal_Pw, Properties)
            implicit none
            real(real64), intent(out) :: obs_values(:)
            type(Output_Observation), intent(in) :: observation_data
            real(real64), intent(in), optional :: nodal_temperature(:)
            real(real64), intent(in), optional :: nodal_porosity(:)
            real(real64), intent(in), optional :: nodal_Pw(:)
            type(Proereties_Model_t), intent(in), optional :: Properties

        end subroutine Interpolate_ObsValues_VHC

        module subroutine Interpolate_ObsValues_Pw(obs_values, observation_data, nodal_temperature, &
                                                   nodal_porosity, nodal_Pw, Properties)
            implicit none
            real(real64), intent(out) :: obs_values(:)
            type(Output_Observation), intent(in) :: observation_data
            real(real64), intent(in), optional :: nodal_temperature(:)
            real(real64), intent(in), optional :: nodal_porosity(:)
            real(real64), intent(in), optional :: nodal_Pw(:)
            type(Proereties_Model_t), intent(in), optional :: Properties

        end subroutine Interpolate_ObsValues_Pw

        ! module subroutine Interpolate_ObsValues_wFlux(obs_values, observation_data, nodal_temperature, &
        !                                               nodal_porosity, nodal_Pw, Properties)
        !     implicit none
        !     real(real64), intent(out) :: obs_values(:)
        !     type(Output_Observation), intent(in) :: observation_data
        !     real(real64), intent(in), optional :: nodal_temperature(:)
        !     real(real64), intent(in), optional :: nodal_porosity(:)
        !     real(real64), intent(in), optional :: nodal_Pw(:)
        !     type(Proereties_Model_t), intent(in), optional :: Properties

        ! end subroutine Interpolate_ObsValues_wFlux

        ! module subroutine Interpolate_ObsValues_K(obs_values, observation_data, nodal_temperature, &
        !                                           nodal_porosity, nodal_Pw, Properties)
        !     implicit none
        !     real(real64), intent(out) :: obs_values(:)
        !     type(Output_Observation), intent(in) :: observation_data
        !     real(real64), intent(in), optional :: nodal_temperature(:)
        !     real(real64), intent(in), optional :: nodal_porosity(:)
        !     real(real64), intent(in), optional :: nodal_Pw(:)
        !     type(Proereties_Model_t), intent(in), optional :: Properties

        ! end subroutine Interpolate_ObsValues_K
    end interface

! In a new or existing module
    abstract interface
        ! This is the "contract" for the procedure pointer.
        subroutine Abst_Calculate_obs_values(obs_values, observation_data, nodal_temperature, &
                                             nodal_porosity, nodal_Pw, Properties)
            import :: real64, Proereties_Model_t, Output_Observation
            implicit none
            real(real64), intent(out) :: obs_values(:)
            type(Output_Observation), intent(in) :: observation_data
            real(real64), intent(in), optional :: nodal_temperature(:)
            real(real64), intent(in), optional :: nodal_porosity(:)
            real(real64), intent(in), optional :: nodal_Pw(:)
            type(Proereties_Model_t), intent(in), optional :: Properties
        end subroutine Abst_Calculate_obs_values
    end interface

    type :: Output_VTK_Series
        integer(int32) :: nPoints
        integer(int32) :: nCell
        type(DP3d) :: Coordinates
        integer(int32), allocatable :: connectivity(:)
        integer(int32), allocatable :: offset(:)
        integer(int8), allocatable :: CellType(:)
    end type

    type :: Type_Output
        private
        character(:), allocatable :: fextend
        type(Output_Observation) :: Observation

        logical(4) :: doOutput_stdout

        character(:), allocatable :: dir_Output
        character(:), allocatable :: dir_FileOutput
        character(:), allocatable :: format_output

        type(Output_VTK_Series) :: VTKInfo
        character(:), allocatable :: Output_TimeUnit
        character(:), allocatable :: Interval_TimeUnit

        logical(4) :: doHeat
        logical(4) :: doPressure
        logical(4) :: doStress

        character(:), allocatable :: logFileName

    contains
        procedure, pass(self) :: Output_All_vtu => Inout_Output_All_vtu
        procedure, pass(self) :: Output_All_vtk => Inout_Output_All_vtk
        procedure, pass(self) :: Output_All_vtk_Scalar => Inout_Output_All_vtk_Scalar_Field
        procedure, pass(self) :: Output_All_vtk_Vector => Inout_Output_All_vtk_Vector_Field
        procedure, pass(self), public :: Output_All => Inout_Output_All

        ! procedure, pass(self) :: Write_Observation_Header
        ! procedure, pass(self) :: Initialize_Observation_Header
        ! procedure, pass(self) :: Interpolate_ObsValues
        procedure, pass(self), public :: Output_Observation => Output_Process_Observation

        procedure, pass(self), public :: Output_SystemLog
    end type Type_Output

    interface Type_Output
        module procedure Type_Output_Construct
    end interface

    public :: Type_Output

    !----------------------------------------------------------------------
    ! Base interface
    !-----------------------------------------------------------------------
    interface
        module subroutine Setup_Directory(dirPath, fileExtensions)
            implicit none
            character(*), intent(in) :: dirPath
            character(*), dimension(:), intent(in) :: fileExtensions
        end subroutine Setup_Directory

        module function Get_UserName() result(UserName)
            implicit none
            character(:), allocatable :: UserName

        end function Get_UserName

        module function Get_HostName() result(HostName)
            implicit none
            character(:), allocatable :: HostName

        end function Get_HostName

        module function Get_CompilerName() result(CompilerName)
            implicit none
            character(:), allocatable :: CompilerName

        end function Get_CompilerName

        module function Get_CompilerVersion() result(CompilerVersion)
            implicit none
            character(:), allocatable :: CompilerVersion

        end function Get_CompilerVersion

        module function Get_CPUArchitecture() result(architecture)
            implicit none
            character(:), allocatable :: architecture

        end function Get_CPUArchitecture

        module function Get_OS() result(os)
            implicit none
            character(:), allocatable :: os

        end function Get_OS

        module function Get_OpneMP_Version() result(OpenMPversion)
            implicit none
            character(:), allocatable :: OpenMPversion

        end function Get_OpneMP_Version

    end interface

    interface

        ! module subroutine Initialize_Observation_Header(self, data_name)
        !     implicit none
        !     class(Type_Output) :: self
        !     character(*), intent(in) :: data_name

        ! end subroutine Initialize_Observation_Header

        module subroutine Output_Process_Observation(self, time, Temp, Si, TC, C, Pres, wFlux, K, Thermal, phi, Propeties)
            implicit none
            class(Type_Output) :: self
            real(real64), intent(in) :: time
            real(real64), intent(in), optional :: Temp(:)
            real(real64), intent(in), optional :: Si(:)
            real(real64), intent(in), optional :: TC(:)
            real(real64), intent(in), optional :: C(:)
            real(real64), intent(in), optional :: Pres(:)
            real(real64), intent(in), optional :: wFlux(:)
            real(real64), intent(in), optional :: K(:)
            class(Abstract_Thermal), intent(inout), optional :: Thermal
            real(real64), intent(in), optional :: phi(:)
            type(Proereties_Model_t), intent(inout), optional :: Propeties

        end subroutine Output_Process_Observation
    end interface

    interface
        function get_rss_kb() bind(C, name="get_rss_kb")
            import :: c_int64_t
            integer(c_int64_t) :: get_rss_kb
        end function

        function C_Get_OS() bind(C, name="C_Get_OS")
            import :: c_ptr
            type(c_ptr) :: C_Get_OS
        end function

        function C_Get_Architecture() bind(C, name="C_Get_Architecture")
            import :: c_ptr
            type(c_ptr) :: C_Get_Architecture
        end function
    end interface

    interface
        module subroutine Output_SystemLog(self, time, Matrix)
            implicit none
            class(Type_Output) :: self
            type(Type_Time), intent(in) :: time
            type(Type_CRS), intent(in) :: Matrix
        end subroutine Output_SystemLog
    end interface

contains

    function Type_Output_Construct(Input, Domain, Coordinate) result(Structure)
        implicit none
        type(Type_Input), intent(in) :: Input
        class(Domain_t), intent(in), optional :: Domain
        ! class(Abstract_Thermal), intent(in), optional :: Thermal
        type(DP3d), intent(inout), pointer :: Coordinate
        type(Type_Output) :: Structure

        character(256) :: dir_Path
        logical(4) :: exists

        character(:), allocatable :: command
        integer(int32) :: i, j, idx
        integer(int32) :: total
        real(real64) :: tmp_xi, tmp_eta
        logical(4) :: is_inside

        integer(int32) :: iObs, iElem
        integer(int32) :: nElements
        integer(int32) :: local_id, local_type
        integer(int32) :: ierr

        character(len=256) :: OutputExtentions(2) = [".dat", ".csv"]
        character(len=256) :: OutputFileExtentions(5) = [".dat", ".csv", ".vtk", ".vtu", "log"]

        ! Path settings
        dir_Path = GetProjectPath()

        Structure%dir_Output = trim(adjustl(dir_Path))//"Output/"
        call Setup_Directory(Structure%dir_Output, OutputExtentions)
        Structure%dir_FileOutput = trim(adjustl(dir_Path))//"Output/Files/"
        call Setup_Directory(Structure%dir_FileOutput, OutputFileExtentions)

        Structure%logFileName = trim(adjustl(Structure%dir_Output))//"run.log"

        Structure%Output_TimeUnit = Input%OutputSettings%Output_TimeUnit
        Structure%Interval_TimeUnit = Input%OutputSettings%Interval_TimeUnit
        Structure%doHeat = any(Input%Regions(:)%Flag%isHeat)
        Structure%doPressure = any(Input%Regions(:)%Flag%isWater)
        Structure%doStress = any(Input%Regions(:)%Flag%isStress)
        Structure%fextend = "."//trim(adjustl(Input%OutputSettings%FileFormat))
        Structure%doOutput_stdout = Input%Basic%shouldDisplayPrompt

        call Structure%Observation%initialize(Input, Coordinate, Domain)
        allocate (Structure%Observation%Variables(7))

        call Structure%Observation%Variables(1)%initialize(Structure%dir_Output, "Temperature", "obsf_T.dat", "°C", Input%OutputSettings%outTemp)
        if (Structure%Observation%Variables(1)%doOutput) then
            Structure%Observation%Variables(1)%get_values => Interpolate_ObsValues_Temperature
        end if
        call Structure%Observation%Variables(2)%initialize(Structure%dir_Output, "Si", "obsf_Si.dat", "-", Input%OutputSettings%outSi)
        if (Structure%Observation%Variables(2)%doOutput) then
            Structure%Observation%Variables(2)%get_values => Interpolate_ObsValues_Si
        end if
        call Structure%Observation%Variables(3)%initialize(Structure%dir_Output, "Thermal Conductivity", "obsf_THC.dat", "W/m/K", Input%OutputSettings%outTC)
        if (Structure%Observation%Variables(3)%doOutput) then
            Structure%Observation%Variables(3)%get_values => Interpolate_ObsValues_THC
        end if
        call Structure%Observation%Variables(4)%initialize(Structure%dir_Output, "Volumetric Heat Capacity", "obsf_VHC.dat", "J/m^3/K", Input%OutputSettings%outC)
        if (Structure%Observation%Variables(4)%doOutput) then
            Structure%Observation%Variables(4)%get_values => Interpolate_ObsValues_VHC
        end if
        call Structure%Observation%Variables(5)%initialize(Structure%dir_Output, "Pressure", "obsf_P.dat", &
                                                           Input%Regions(1)%Ice%c_unit, Input%OutputSettings%outPres)
        if (Structure%Observation%Variables(5)%doOutput) then
            Structure%Observation%Variables(5)%get_values => Interpolate_ObsValues_Pw
        end if
        call Structure%Observation%Variables(6)%initialize(Structure%dir_Output, "Water Flux", "obsf_Flux.dat", "m/s", Input%OutputSettings%outFlux)
        if (Structure%Observation%Variables(6)%doOutput) then
            ! Structure%Observation%Variables(6)%get_values => Interpolate_ObsValues_wFlux
        end if
        call Structure%Observation%Variables(7)%initialize(Structure%dir_Output, "Hydraulic Conductivity", "obsf_K.dat", "m/s", Input%OutputSettings%outK)
        if (Structure%Observation%Variables(7)%doOutput) then
            ! Structure%Observation%Variables(7)%get_values => Interpolate_ObsValues_K
        end if

        if (Structure%doHeat) then
            call Structure%Observation%Write_Header(Structure%Observation%Variables(1), Structure%Output_TimeUnit)
            call Structure%Observation%Write_Header(Structure%Observation%Variables(2), Structure%Output_TimeUnit)
            call Structure%Observation%Write_Header(Structure%Observation%Variables(3), Structure%Output_TimeUnit)
            call Structure%Observation%Write_Header(Structure%Observation%Variables(4), Structure%Output_TimeUnit)
        end if
        if (Structure%doPressure) then
            call Structure%Observation%Write_Header(Structure%Observation%Variables(5), Structure%Output_TimeUnit)
            call Structure%Observation%Write_Header(Structure%Observation%Variables(6), Structure%Output_TimeUnit)
            call Structure%Observation%Write_Header(Structure%Observation%Variables(7), Structure%Output_TimeUnit)
        end if

        Structure%format_output = '(a,a,i5.5,a)'

        select case (Structure%fextend)
        case (".vtk")
            Structure%VTKInfo%nPoints = Input%VTK%numPoints
            Structure%VTKInfo%nCell = Input%VTK%numTotalCells
            call Structure%VTKInfo%Coordinates%allocate(Structure%VTKInfo%nPoints)
            Structure%VTKInfo%Coordinates = Input%VTK%POINTS

            call Allocate_Array(Structure%VTKInfo%offset, Structure%VTKInfo%nCell)
            call Allocate_Array(Structure%VTKInfo%CellType, Structure%VTKInfo%nCell)

            do i = 1, Structure%VTKInfo%nCell
                Structure%VTKInfo%offset(i) = Input%VTK%CELLS(i)%offset
                Structure%VTKInfo%CellType(i) = Input%VTK%CELLS(i)%CellType
            end do
            total = sum(Structure%VTKInfo%offset(:))

            call Allocate_Array(Structure%VTKInfo%connectivity, total)
            idx = 0
            do i = 1, Structure%VTKInfo%nCell
                do j = 1, Input%VTK%CELLS(i)%offset
                    idx = idx + 1
                    Structure%VTKInfo%connectivity(idx) = &
                        Input%VTK%CELLS(i)%connectivity(j) - 1
                end do
            end do
        case (".vtu")
            Structure%VTKInfo%nPoints = Input%VTK%numPoints
            Structure%VTKInfo%nCell = Input%VTK%numTotalCells
            call Structure%VTKInfo%Coordinates%allocate(Structure%VTKInfo%nPoints)
            Structure%VTKInfo%Coordinates = Input%VTK%POINTS

            call Allocate_Array(Structure%VTKInfo%offset, Structure%VTKInfo%nCell)
            call Allocate_Array(Structure%VTKInfo%CellType, Structure%VTKInfo%nCell)

            do i = 1, Structure%VTKInfo%nCell
                if (i == 1) then
                    Structure%VTKInfo%offset(i) = Input%VTK%CELLS(i)%offset
                else
                    Structure%VTKInfo%offset(i) = Structure%VTKInfo%offset(i - 1) + &
                                                  Input%VTK%CELLS(i)%offset
                end if
                Structure%VTKInfo%CellType(i) = Input%VTK%CELLS(i)%CellType
            end do
            total = Structure%VTKInfo%offset(Structure%VTKInfo%nCell)

            call Allocate_Array(Structure%VTKInfo%connectivity, total)
            do i = 1, Structure%VTKInfo%nCell
                if (i == 1) then
                    do j = 1, Input%VTK%CELLS(i)%offset
                        Structure%VTKInfo%connectivity(j) = Input%VTK%CELLS(i)%connectivity(j) - 1
                    end do
                else
                    do j = 1, Input%VTK%CELLS(i)%offset
                        Structure%VTKInfo%connectivity(Structure%VTKInfo%offset(i - 1) + j) = &
                            Input%VTK%CELLS(i)%connectivity(j) - 1
                    end do
                end if
            end do
        end select

    end function Type_Output_Construct

    subroutine Inout_Output_All(self, fc, Temp, Si, Pres, wFlux)
        implicit none
        class(Type_Output) :: self
        integer(int32), intent(in) :: fc

        real(real64), intent(in), optional :: Temp(:)
        real(real64), intent(in), optional :: Si(:)
        real(real64), intent(in), optional :: Pres(:)
        type(DP3d), intent(in), optional :: wFlux

        select case (trim(adjustl(self%fextend)))
        case (".vtk")
            call self%Output_All_vtk(fc=fc, Temp=Temp, Si=Si, Pres=Pres, wFlux=wFlux)
        case (".vtu")
            call self%Output_All_vtu(fc=fc, Temp=Temp, Si=Si, Pres=Pres, wFlux=wFlux)
        end select

    end subroutine Inout_Output_All

    subroutine Inout_Output_All_vtk(self, fc, Temp, Si, Pres, wFlux)
        implicit none
        class(Type_Output) :: self
        integer(int32), intent(in) :: fc

        real(real64), intent(in), optional :: Temp(:)
        real(real64), intent(in), optional :: Si(:)
        real(real64), intent(in), optional :: Pres(:)
        type(DP3d), intent(in), optional :: wFlux

        integer(int32) :: status
        integer(int32) :: unit_num
        integer(int32) :: iN, iE, idx

        character(256) :: outName

        ! Initialize VTK file
        write (outName, self%format_output) trim(self%dir_FileOutput), "Out_", fc, self%fextend
        open (newunit=unit_num, file=outName, status='replace', action='write', iostat=status)
        if (status /= 0) call error_message(931)

        write (unit_num, '(a)') "# vtk DataFile Version 2.0"
        write (unit_num, '(a)') "Analysis ASCII VTK file"
        write (unit_num, '(a)') "ASCII"
        write (unit_num, '(a)') "DATASET UNSTRUCTURED_GRID"
        write (unit_num, '(a,i0,a)') "POINTS ", self%VTKInfo%nPoints, " double"

        do iN = 1, self%VTKInfo%nPoints
            write (unit_num, '(3(es22.15,x))') self%VTKInfo%Coordinates%x(iN), self%VTKInfo%Coordinates%y(iN), self%VTKInfo%Coordinates%z(iN)
        end do
        write (unit_num, '(a)') ""

        write (unit_num, '(a,i0,x,i0,a)') "CELLS ", self%VTKInfo%nCell, sum(self%VTKInfo%offset(:)) + self%VTKInfo%nCell
        idx = 1
        do iE = 1, self%VTKInfo%nCell
            write (unit_num, '(i0,'//to_string(self%VTKInfo%offset(iE))//'(x,i0))') self%VTKInfo%offset(iE), self%VTKInfo%connectivity(idx:idx + self%VTKInfo%offset(iE) - 1)
            idx = idx + self%VTKInfo%offset(iE)
        end do
        write (unit_num, '(a)') ""

        write (unit_num, '(a,i0)') "CELL_TYPES ", self%VTKInfo%nCell
        do iE = 1, self%VTKInfo%nCell
            write (unit_num, '(i0)') self%VTKInfo%CellType(iE)
        end do
        write (unit_num, '(a)') ""

        write (unit_num, '(a, i0)') "POINT_DATA ", self%VTKInfo%nPoints
        if (present(Temp)) then
            call self%Output_All_vtk_Scalar(unit_num=unit_num, &
                                            data_name='Temperature', &
                                            x=Temp)
        end if
        if (present(Si)) then
            call self%Output_All_vtk_Scalar(unit_num=unit_num, &
                                            data_name='Si', &
                                            x=Si)
        end if
        if (present(Pres)) then
            call self%Output_All_vtk_Scalar(unit_num=unit_num, &
                                            data_name='Pressure', &
                                            x=Pres)
        end if
        if (present(wFlux)) then
            call self%Output_All_vtk_Vector(unit_num=unit_num, &
                                            data_name='waterFlux', &
                                            x=wFlux%x, &
                                            y=wFlux%y, &
                                            z=wFlux%z)
        end if

    end subroutine Inout_Output_All_vtk

    subroutine Inout_Output_All_vtk_Scalar_Field(self, unit_num, data_name, x)
        implicit none
        class(Type_Output) :: self
        integer(int32), intent(in) :: unit_num
        character(*), intent(in) :: data_name
        real(real64), intent(in) :: x(:)

        write (unit_num, '(3a)') "SCALARS ", trim(adjustl(data_name)), " double 1"
        write (unit_num, '(a)') "LOOKUP_TABLE default"
        write (unit_num, '(es22.15)') x(:)
        write (unit_num, '(a)') ""

    end subroutine Inout_Output_All_vtk_Scalar_Field

    subroutine Inout_Output_All_vtk_Vector_Field(self, unit_num, data_name, x, y, z)
        implicit none
        class(Type_Output) :: self
        integer(int32), intent(in) :: unit_num
        character(*), intent(in) :: data_name
        real(real64), intent(in) :: x(:), y(:), z(:)
        integer(int32) :: i

        write (unit_num, '(3a)') "VECTORS ", trim(adjustl(data_name)), " double"
        do i = 1, size(x)
            write (unit_num, '(3(es22.15,x))') x(i), y(i), z(i)
        end do
        write (unit_num, '(a)') ""
    end subroutine

    subroutine Inout_Output_All_vtu(self, fc, Temp, Si, Pres, wFlux)
        use :: vtk_fortran, only:vtk_file
        implicit none
        class(Type_Output) :: self
        integer(int32), intent(in) :: fc

        real(real64), intent(in), optional :: Temp(:)
        real(real64), intent(in), optional :: Si(:)
        real(real64), intent(in), optional :: Pres(:)
        type(DP3d), intent(in), optional :: wFlux

        type(vtk_file) :: vtu
        integer(int32) :: status

        character(256) :: outName

        ! Initialize VTK file
        write (outName, self%format_output) trim(self%dir_FileOutput), "Out_", fc, self%fextend
        status = vtu%initialize(format='ascii', filename=trim(outName), mesh_topology='UnstructuredGrid')

        ! Write data
        status = vtu%xml_writer%write_piece(np=self%VTKInfo%nPoints, &
                                            nc=self%VTKInfo%nCell)
        status = vtu%xml_writer%write_geo(np=self%VTKInfo%nPoints, &
                                          nc=self%VTKInfo%nCell, &
                                          x=self%VTKInfo%Coordinates%x, &
                                          y=self%VTKInfo%Coordinates%y, &
                                          z=self%VTKInfo%Coordinates%z)
        status = vtu%xml_writer%write_connectivity(nc=self%VTKInfo%nCell, &
                                                   connectivity=self%VTKInfo%connectivity, &
                                                   offset=self%VTKInfo%offset, &
                                                   cell_type=self%VTKInfo%CellType)
        status = vtu%xml_writer%write_dataarray(location='node', action='open')
        if (present(Temp)) then
            status = vtu%xml_writer%write_dataarray(data_name='Temperature', &
                                                    x=Temp)
        end if
        if (present(Si)) then
            status = vtu%xml_writer%write_dataarray(data_name='Si', &
                                                    x=Si)
        end if
        if (present(Pres)) then
            status = vtu%xml_writer%write_dataarray(data_name='Pressure', &
                                                    x=Pres)
        end if
        if (present(wFlux)) then
            status = vtu%xml_writer%write_dataarray(data_name='waterFlux', &
                                                    x=wFlux%x, &
                                                    y=wFlux%y, &
                                                    z=wFlux%z)
        end if
        status = vtu%xml_writer%write_dataarray(location='node', action='close')
        status = vtu%xml_writer%write_piece()

        ! Finalize VTK file
        status = vtu%finalize()

    end subroutine Inout_Output_All_vtu

end module Inout_Output

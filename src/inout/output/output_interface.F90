module input_output
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding, only: c_int64_t, c_ptr, c_f_pointer, c_char, c_null_char, c_associated
!$  use :: omp_lib
    use :: stdlib_strings, only:to_string

    use :: inout_project_settings, only:get_project_path

    use :: module_core, only:allocate_array, deallocate_array, type_variable, type_dp_3d, type_gauss_point_state, & !&
                             get_username, get_hostname, get_compiler_name, get_compiler_version, & !&
                             get_cpu_architecture, get_os, get_openmp_version, get_memory_usage !&

    use :: Inout_Input
    use :: module_domain, only:holder_elements, create_element, type_domain, type_rcm
    use :: module_control, only:type_time, type_iteration
    use :: module_properties, only:type_proereties_manager
    use :: Main_Thermal
    use :: Matrix_CRS

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

    ! type :: Output_Config
    !     logical(4) :: doOutput
    !     character(:), allocatable :: Filename
    !     character(:), allocatable :: VariableUnit
    !     integer(int32) :: numUnit
    ! end type

    type :: Output_Observation
        type(ObservationVariable_t), allocatable :: Variables(:)

        integer(int32) :: ObservationType
        integer(int32) :: NumObservation
        type(type_dp_3d) :: Cood_Obs
        type(holder_elements), allocatable :: Element(:)
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
            type(type_dp_3d), intent(inout), pointer :: Coordinate
            type(type_domain), intent(inout), optional :: Domain

        end subroutine Output_Observation_Initialize

        module subroutine Output_Observation_Write_Header(self, ObsVar, TimeUnit)
            implicit none
            class(Output_Observation), intent(inout) :: self
            class(ObservationVariable_t), intent(inout) :: ObsVar
            character(*), intent(in) :: TimeUnit

        end subroutine Output_Observation_Write_Header

        module subroutine Interpolate_ObsValues_Temperature(obs_values, observation_data, nodal_temperature, &
                                                            nodal_porosity, nodal_Pw, Properties, Domain)
            implicit none
            real(real64), intent(out) :: obs_values(:)
            type(Output_Observation), intent(in) :: observation_data
            real(real64), intent(in), optional :: nodal_temperature(:)
            real(real64), intent(in), optional :: nodal_porosity(:)
            real(real64), intent(in), optional :: nodal_Pw(:)
            type(type_proereties_manager), intent(inout), optional :: Properties
            type(type_domain), intent(inout), optional :: Domain

        end subroutine Interpolate_ObsValues_Temperature

        module subroutine Interpolate_ObsValues_Si(obs_values, observation_data, nodal_temperature, &
                                                   nodal_porosity, nodal_Pw, Properties, Domain)
            implicit none
            real(real64), intent(out) :: obs_values(:)
            type(Output_Observation), intent(in) :: observation_data
            real(real64), intent(in), optional :: nodal_temperature(:)
            real(real64), intent(in), optional :: nodal_porosity(:)
            real(real64), intent(in), optional :: nodal_Pw(:)
            type(type_proereties_manager), intent(inout), optional :: Properties
            type(type_domain), intent(inout), optional :: Domain

        end subroutine Interpolate_ObsValues_Si

        module subroutine Interpolate_ObsValues_THC(obs_values, observation_data, nodal_temperature, &
                                                    nodal_porosity, nodal_Pw, Properties, Domain)
            implicit none
            real(real64), intent(out) :: obs_values(:)
            type(Output_Observation), intent(in) :: observation_data
            real(real64), intent(in), optional :: nodal_temperature(:)
            real(real64), intent(in), optional :: nodal_porosity(:)
            real(real64), intent(in), optional :: nodal_Pw(:)
            type(type_proereties_manager), intent(inout), optional :: Properties
            type(type_domain), intent(inout), optional :: Domain

        end subroutine Interpolate_ObsValues_THC

        module subroutine Interpolate_ObsValues_VHC(obs_values, observation_data, nodal_temperature, &
                                                    nodal_porosity, nodal_Pw, Properties, Domain)
            implicit none
            real(real64), intent(out) :: obs_values(:)
            type(Output_Observation), intent(in) :: observation_data
            real(real64), intent(in), optional :: nodal_temperature(:)
            real(real64), intent(in), optional :: nodal_porosity(:)
            real(real64), intent(in), optional :: nodal_Pw(:)
            type(type_proereties_manager), intent(inout), optional :: Properties
            type(type_domain), intent(inout), optional :: Domain

        end subroutine Interpolate_ObsValues_VHC

        module subroutine Interpolate_ObsValues_Pw(obs_values, observation_data, nodal_temperature, &
                                                   nodal_porosity, nodal_Pw, Properties, Domain)
            implicit none
            real(real64), intent(out) :: obs_values(:)
            type(Output_Observation), intent(in) :: observation_data
            real(real64), intent(in), optional :: nodal_temperature(:)
            real(real64), intent(in), optional :: nodal_porosity(:)
            real(real64), intent(in), optional :: nodal_Pw(:)
            type(type_proereties_manager), intent(inout), optional :: Properties
            type(type_domain), intent(inout), optional :: Domain

        end subroutine Interpolate_ObsValues_Pw

        ! module subroutine Interpolate_ObsValues_wFlux(obs_values, observation_data, nodal_temperature, &
        !                                               nodal_porosity, nodal_Pw, Properties)
        !     implicit none
        !     real(real64), intent(out) :: obs_values(:)
        !     type(Output_Observation), intent(in) :: observation_data
        !     real(real64), intent(in), optional :: nodal_temperature(:)
        !     real(real64), intent(in), optional :: nodal_porosity(:)
        !     real(real64), intent(in), optional :: nodal_Pw(:)
        !     type(type_proereties_manager), intent(inout), optional :: Properties

        ! end subroutine Interpolate_ObsValues_wFlux

        ! module subroutine Interpolate_ObsValues_K(obs_values, observation_data, nodal_temperature, &
        !                                           nodal_porosity, nodal_Pw, Properties)
        !     implicit none
        !     real(real64), intent(out) :: obs_values(:)
        !     type(Output_Observation), intent(in) :: observation_data
        !     real(real64), intent(in), optional :: nodal_temperature(:)
        !     real(real64), intent(in), optional :: nodal_porosity(:)
        !     real(real64), intent(in), optional :: nodal_Pw(:)
        !     type(type_proereties_manager), intent(inout), optional :: Properties

        ! end subroutine Interpolate_ObsValues_K
    end interface

! In a new or existing module
    abstract interface
        ! This is the "contract" for the procedure pointer.
        subroutine Abst_Calculate_obs_values(obs_values, observation_data, nodal_temperature, &
                                             nodal_porosity, nodal_Pw, Properties, Domain)
            import :: real64, type_proereties_manager, Output_Observation, type_domain
            implicit none
            real(real64), intent(out) :: obs_values(:)
            type(Output_Observation), intent(in) :: observation_data
            real(real64), intent(in), optional :: nodal_temperature(:)
            real(real64), intent(in), optional :: nodal_porosity(:)
            real(real64), intent(in), optional :: nodal_Pw(:)
            type(type_proereties_manager), intent(inout), optional :: Properties
            type(type_domain), intent(inout), optional :: Domain
        end subroutine Abst_Calculate_obs_values
    end interface

    type :: Output_VTK_Series
        integer(int32) :: nPoints
        integer(int32) :: nCell
        type(type_dp_3d) :: Coordinates
        integer(int32), allocatable :: connectivity(:)
        integer(int32), allocatable :: offset(:)
        integer(int8), allocatable :: CellType(:)
    end type

    type :: Output_Overall
        private
        ! Output format
        character(:), allocatable :: dir_FileOutput
        character(:), allocatable :: format_output
        character(:), allocatable :: fextend
        ! DATA
        type(Output_VTK_Series) :: VTK
    contains
        procedure, pass(self), public :: initialize => input_output_Overall_initialize
        procedure, pass(self) :: initialize_vtk => input_output_Overall_initialize_vtk
        procedure, pass(self) :: initialize_vtu => input_output_Overall_initialize_vtu
        procedure, pass(self), public :: Output => input_output_Overall_Output
        procedure, pass(self) :: Output_vtu => input_output_Overall_Output_vtu
        procedure, pass(self) :: Output_vtk => input_output_Overall_Output_vtk
        procedure, pass(self) :: Output_vtk_scalar_int32 => input_output_Overall_Output_vtk_scalar_int32
        procedure, pass(self) :: Output_vtk_scalar_real64 => input_output_Overall_Output_vtk_scalar_real64
        generic :: Output_vtk_scalar => Output_vtk_scalar_int32, & !&
                                        Output_vtk_scalar_real64 !&
        procedure, pass(self) :: Output_vtk_vector => input_output_Overall_Output_vtk_vector

    end type

    interface
        module subroutine input_output_Overall_initialize(self, Input, Coordinate, Domain)
            implicit none
            class(Output_Overall), intent(inout) :: self
            type(Type_Input), intent(in) :: Input
            type(type_dp_3d), intent(in) :: Coordinate
            type(type_domain), intent(inout) :: Domain

        end subroutine input_output_Overall_initialize

        module subroutine input_output_Overall_initialize_vtk(self, Input, Coordinate, Domain)
            implicit none
            class(Output_Overall), intent(inout) :: self
            type(Type_Input), intent(in) :: Input
            type(type_dp_3d), intent(in) :: Coordinate
            type(type_domain), intent(inout) :: Domain

        end subroutine input_output_Overall_initialize_vtk

        module subroutine input_output_Overall_initialize_vtu(self, Input, Coordinate, Domain)
            implicit none
            class(Output_Overall), intent(inout) :: self
            type(Type_Input), intent(in) :: Input
            type(type_dp_3d), intent(in) :: Coordinate
            type(type_domain), intent(inout) :: Domain

        end subroutine input_output_Overall_initialize_vtu

        module subroutine input_output_Overall_Output(self, fc, rcm, Temp, Si, Pres, wFlux, Colors)
            implicit none
            class(Output_Overall) :: self
            integer(int32), intent(in) :: fc
            type(type_rcm), intent(in), optional :: rcm
            real(real64), intent(in), optional :: Temp(:)
            real(real64), intent(in), optional :: Si(:)
            real(real64), intent(in), optional :: Pres(:)
            type(type_dp_3d), intent(in), optional :: wFlux
            integer(int32), intent(in), optional :: Colors(:)

        end subroutine input_output_Overall_Output

        module subroutine input_output_Overall_Output_vtk(self, fc, iperm, Temp, Si, Pres, wFlux, Colors)
            implicit none
            class(Output_Overall), intent(inout) :: self
            integer(int32), intent(in) :: fc
            integer(int32), intent(in), optional :: iperm(:)
            real(real64), intent(in), optional :: Temp(:)
            real(real64), intent(in), optional :: Si(:)
            real(real64), intent(in), optional :: Pres(:)
            type(type_dp_3d), intent(in), optional :: wFlux
            integer(int32), intent(in), optional :: Colors(:)

        end subroutine input_output_Overall_Output_vtk

        module subroutine input_output_Overall_Output_vtk_scalar_real64(self, iperm, unit_num, data_name, x)
            implicit none
            class(Output_Overall) :: self
            integer(int32), intent(in), optional :: iperm(:)
            integer(int32), intent(in) :: unit_num
            character(*), intent(in) :: data_name
            real(real64), intent(in) :: x(:)

        end subroutine input_output_Overall_Output_vtk_scalar_real64

        module subroutine input_output_Overall_Output_vtk_scalar_int32(self, iperm, unit_num, data_name, x)
            implicit none
            class(Output_Overall) :: self
            integer(int32), intent(in), optional :: iperm(:)
            integer(int32), intent(in) :: unit_num
            character(*), intent(in) :: data_name
            integer(int32), intent(in) :: x(:)

        end subroutine input_output_Overall_Output_vtk_scalar_int32

        module subroutine input_output_Overall_Output_vtk_vector(self, iperm, unit_num, data_name, x, y, z)
            implicit none
            class(Output_Overall) :: self
            integer(int32), intent(in), optional :: iperm(:)
            integer(int32), intent(in) :: unit_num
            character(*), intent(in) :: data_name
            real(real64), intent(in) :: x(:), y(:), z(:)

        end subroutine input_output_Overall_Output_vtk_vector

        module subroutine input_output_Overall_Output_vtu(self, fc, rcm, Temp, Si, Pres, wFlux, Colors)
            implicit none
            class(Output_Overall), intent(inout) :: self
            integer(int32), intent(in) :: fc
            type(type_rcm), intent(in), optional :: rcm
            real(real64), intent(in), optional :: Temp(:)
            real(real64), intent(in), optional :: Si(:)
            real(real64), intent(in), optional :: Pres(:)
            type(type_dp_3d), intent(in), optional :: wFlux
            integer(int32), intent(in), optional :: Colors(:)

        end subroutine input_output_Overall_Output_vtu

    end interface

    type :: Type_Output
        ! private
        ! character(:), allocatable :: fextend
        type(Output_Observation) :: Observation

        type(Output_Overall) :: Overall

        logical(4) :: doOutput_stdout

        character(:), allocatable :: dir_Output
        ! character(:), allocatable :: dir_FileOutput
        ! character(:), allocatable :: format_output

        type(Output_VTK_Series) :: VTKInfo
        character(:), allocatable :: Output_TimeUnit
        character(:), allocatable :: Interval_TimeUnit

        logical(4) :: doHeat
        logical(4) :: doPressure
        logical(4) :: doStress

        character(:), allocatable :: logFileName

    contains
        ! procedure, pass(self) :: Output_All_vtu => input_output_All_vtu
        ! procedure, pass(self) :: Output_All_vtk => input_output_All_vtk
        ! procedure, pass(self) :: Output_All_vtk_Scalar => input_output_All_vtk_Scalar_Field
        ! procedure, pass(self) :: Output_All_vtk_Vector => input_output_All_vtk_Vector_Field
        ! procedure, pass(self), public :: Output_All => input_output_All

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
    end interface

    interface
        module subroutine Output_Process_Observation(self, time, Temp, Si, TC, C, Pres, wFlux, K, Thermal, phi, Propeties, Domain)
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
            type(type_proereties_manager), intent(inout), optional :: Propeties
            type(type_domain), intent(inout), optional :: Domain

        end subroutine Output_Process_Observation
    end interface

    interface
        module subroutine Output_SystemLog(self, time, Matrix, Domain)
            implicit none
            class(Type_Output) :: self
            type(type_time), intent(in) :: time
            type(Type_CRS), intent(in) :: Matrix
            type(type_domain), intent(inout) :: Domain
        end subroutine Output_SystemLog
    end interface

contains

    function Type_Output_Construct(Input, Domain, Coordinate) result(Structure)
        implicit none
        type(Type_Input), intent(in) :: Input
        class(type_domain), intent(inout), optional :: Domain
        ! class(Abstract_Thermal), intent(in), optional :: Thermal
        type(type_dp_3d), intent(inout), pointer :: Coordinate
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

        character(len=256) :: OutputExtentions(3) = [".dat", ".csv", ".log"]
        character(len=256) :: OutputFileExtentions(5) = [".dat", ".csv", ".vtk", ".vtu", ".log"]

        ! Path settings
        dir_Path = get_project_path()

        Structure%dir_Output = trim(adjustl(dir_Path))//"Output/"
        call Setup_Directory(Structure%dir_Output, OutputExtentions)
        Structure%Overall%dir_FileOutput = trim(adjustl(dir_Path))//"Output/Files/"
        call Setup_Directory(Structure%Overall%dir_FileOutput, OutputFileExtentions)

        Structure%logFileName = trim(adjustl(Structure%dir_Output))//"run.log"

        Structure%Output_TimeUnit = Input%OutputSettings%Output_TimeUnit
        Structure%Interval_TimeUnit = Input%OutputSettings%Interval_TimeUnit
        Structure%doHeat = any(Input%Regions(:)%Flag%isHeat)
        Structure%doPressure = any(Input%Regions(:)%Flag%isWater)
        Structure%doStress = any(Input%Regions(:)%Flag%isStress)
        Structure%Overall%fextend = "."//trim(adjustl(Input%OutputSettings%FileFormat))
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

        Structure%Overall%format_output = '(a,a,i5.5,a)'

        call Structure%Overall%initialize(Input, Coordinate, Domain)

    end function Type_Output_Construct

end module input_output

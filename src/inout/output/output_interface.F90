module input_output
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding, only: c_int64_t, c_ptr, c_f_pointer, c_char, c_null_char, c_associated
!$  use :: omp_lib
    use :: stdlib_strings, only:to_string
    use :: inout_project_settings, only:get_project_path
    use :: module_core, only:allocate_array, deallocate_array, type_variable, type_dp_3d, type_gauss_point_state, & !&
                             get_username, get_hostname, get_compiler_name, get_compiler_version, & !&
                             get_cpu_architecture, get_os, get_openmp_version, get_memory_usage !&

    use :: module_input
    use :: module_domain, only:holder_elements, create_element, type_domain, type_reordering
    use :: module_control, only:type_time, type_iteration
    use :: module_properties, only:type_proereties_manager
    use :: module_matrix
    use :: module_thermal

    implicit none
    private

    ! 個々の観測変数を管理するクラス
    type :: type_oservations
        character(:), allocatable :: name
        character(:), allocatable :: unit
        character(:), allocatable :: file_name
        integer(int32) :: num_unit = -1
        procedure(abst_calculate_obs_values), pointer, nopass :: get_values => null()
    contains
        procedure, pass(self) :: initialize => initialize_type_oservations
    end type type_oservations

    interface
        module subroutine initialize_type_oservations(self, dir_output, variable_name, variable_unit, file_name)
            implicit none
            class(type_oservations), intent(inout) :: self
            character(*), intent(in) :: dir_output
            character(*), intent(in) :: variable_name
            character(*), intent(in) :: variable_unit
            character(*), intent(in) :: file_name

        end subroutine initialize_type_oservations

    end interface

    type :: type_output_observation
        type(type_oservations), allocatable :: variables(:)

        character(:), allocatable :: type
        integer(int32) :: num_observations
        type(type_dp_3d) :: coordinate
        !!
        type(holder_elements), allocatable :: elements(:)
        real(real64), allocatable :: xi(:)
        real(real64), allocatable :: eta(:)
        !!
        integer(int32), allocatable :: node_ids(:)
    contains
        procedure, pass(self) :: initialize => initialize_type_output_observation
        procedure, pass(self) :: Write_Header => type_output_observation_write_header
        ! procedure, pass(self) :: Interpolate => interpolate_observations
    end type type_output_observation

    interface
        module subroutine initialize_type_output_observation(self, input, coordinate, domain)
            implicit none
            class(type_output_observation), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_dp_3d), intent(inout), pointer :: coordinate
            type(type_domain), intent(inout) :: domain

        end subroutine initialize_type_output_observation

        module subroutine type_output_observation_write_header(self, variable, time_unit)
            implicit none
            class(type_output_observation), intent(inout) :: self
            class(type_oservations), intent(inout) :: variable
            character(*), intent(in) :: time_unit

        end subroutine type_output_observation_write_header

        module subroutine interpolate_observations_temperature(obs_values, observation_data, nodal_temperature, &
                                                               nodal_porosity, nodal_Pw, properties, domain)
            implicit none
            real(real64), intent(out) :: obs_values(:)
            type(type_output_observation), intent(in) :: observation_data
            real(real64), intent(in), optional :: nodal_temperature(:)
            real(real64), intent(in), optional :: nodal_porosity(:)
            real(real64), intent(in), optional :: nodal_Pw(:)
            type(type_proereties_manager), intent(inout), optional :: properties
            type(type_domain), intent(inout), optional :: domain

        end subroutine interpolate_observations_temperature

        module subroutine interpolate_observations_si(obs_values, observation_data, nodal_temperature, &
                                                      nodal_porosity, nodal_Pw, properties, domain)
            implicit none
            real(real64), intent(out) :: obs_values(:)
            type(type_output_observation), intent(in) :: observation_data
            real(real64), intent(in), optional :: nodal_temperature(:)
            real(real64), intent(in), optional :: nodal_porosity(:)
            real(real64), intent(in), optional :: nodal_Pw(:)
            type(type_proereties_manager), intent(inout), optional :: properties
            type(type_domain), intent(inout), optional :: domain

        end subroutine interpolate_observations_si

        module subroutine interpolate_observations_thc(obs_values, observation_data, nodal_temperature, &
                                                       nodal_porosity, nodal_Pw, properties, domain)
            implicit none
            real(real64), intent(out) :: obs_values(:)
            type(type_output_observation), intent(in) :: observation_data
            real(real64), intent(in), optional :: nodal_temperature(:)
            real(real64), intent(in), optional :: nodal_porosity(:)
            real(real64), intent(in), optional :: nodal_Pw(:)
            type(type_proereties_manager), intent(inout), optional :: properties
            type(type_domain), intent(inout), optional :: domain

        end subroutine interpolate_observations_thc

        module subroutine interpolate_observations_VHC(obs_values, observation_data, nodal_temperature, &
                                                       nodal_porosity, nodal_Pw, properties, domain)
            implicit none
            real(real64), intent(out) :: obs_values(:)
            type(type_output_observation), intent(in) :: observation_data
            real(real64), intent(in), optional :: nodal_temperature(:)
            real(real64), intent(in), optional :: nodal_porosity(:)
            real(real64), intent(in), optional :: nodal_Pw(:)
            type(type_proereties_manager), intent(inout), optional :: properties
            type(type_domain), intent(inout), optional :: domain

        end subroutine interpolate_observations_VHC

        module subroutine interpolate_observations_Pw(obs_values, observation_data, nodal_temperature, &
                                                      nodal_porosity, nodal_Pw, properties, domain)
            implicit none
            real(real64), intent(out) :: obs_values(:)
            type(type_output_observation), intent(in) :: observation_data
            real(real64), intent(in), optional :: nodal_temperature(:)
            real(real64), intent(in), optional :: nodal_porosity(:)
            real(real64), intent(in), optional :: nodal_Pw(:)
            type(type_proereties_manager), intent(inout), optional :: properties
            type(type_domain), intent(inout), optional :: domain

        end subroutine interpolate_observations_Pw

        ! module subroutine interpolate_observations_wFlux(obs_values, observation_data, nodal_temperature, &
        !                                               nodal_porosity, nodal_Pw, properties)
        !     implicit none
        !     real(real64), intent(out) :: obs_values(:)
        !     type(type_output_observation), intent(in) :: observation_data
        !     real(real64), intent(in), optional :: nodal_temperature(:)
        !     real(real64), intent(in), optional :: nodal_porosity(:)
        !     real(real64), intent(in), optional :: nodal_Pw(:)
        !     type(type_proereties_manager), intent(inout), optional :: properties

        ! end subroutine interpolate_observations_wFlux

        ! module subroutine interpolate_observations_K(obs_values, observation_data, nodal_temperature, &
        !                                           nodal_porosity, nodal_Pw, properties)
        !     implicit none
        !     real(real64), intent(out) :: obs_values(:)
        !     type(type_output_observation), intent(in) :: observation_data
        !     real(real64), intent(in), optional :: nodal_temperature(:)
        !     real(real64), intent(in), optional :: nodal_porosity(:)
        !     real(real64), intent(in), optional :: nodal_Pw(:)
        !     type(type_proereties_manager), intent(inout), optional :: properties

        ! end subroutine interpolate_observations_K
    end interface

! In a new or existing module
    abstract interface
        ! This is the "contract" for the procedure pointer.
        subroutine abst_calculate_obs_values(obs_values, observation_data, nodal_temperature, &
                                             nodal_porosity, nodal_Pw, properties, domain)
            import :: real64, type_proereties_manager, type_output_observation, type_domain
            implicit none
            real(real64), intent(out) :: obs_values(:)
            type(type_output_observation), intent(in) :: observation_data
            real(real64), intent(in), optional :: nodal_temperature(:)
            real(real64), intent(in), optional :: nodal_porosity(:)
            real(real64), intent(in), optional :: nodal_Pw(:)
            type(type_proereties_manager), intent(inout), optional :: properties
            type(type_domain), intent(inout), optional :: domain
        end subroutine abst_calculate_obs_values
    end interface

    type :: Output_VTK_Series
        integer(int32) :: nPoints
        integer(int32) :: nCell
        type(type_dp_3d) :: coordinates
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
        module subroutine input_output_Overall_initialize(self, input, coordinate, domain)
            implicit none
            class(Output_Overall), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_dp_3d), intent(in) :: coordinate
            type(type_domain), intent(inout) :: domain

        end subroutine input_output_Overall_initialize

        module subroutine input_output_Overall_initialize_vtk(self, input, coordinate, domain)
            implicit none
            class(Output_Overall), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_dp_3d), intent(in) :: coordinate
            type(type_domain), intent(inout) :: domain

        end subroutine input_output_Overall_initialize_vtk

        module subroutine input_output_Overall_initialize_vtu(self, input, coordinate, domain)
            implicit none
            class(Output_Overall), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_dp_3d), intent(in) :: coordinate
            type(type_domain), intent(inout) :: domain

        end subroutine input_output_Overall_initialize_vtu

        module subroutine input_output_Overall_Output(self, fc, reordering, Temp, Si, Pres, wFlux, Colors)
            implicit none
            class(Output_Overall) :: self
            integer(int32), intent(in) :: fc
            type(type_reordering), intent(in) :: reordering
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

        module subroutine input_output_Overall_Output_vtu(self, fc, reordering, Temp, Si, Pres, wFlux, Colors)
            implicit none
            class(Output_Overall), intent(inout) :: self
            integer(int32), intent(in) :: fc
            type(type_reordering), intent(in) :: reordering
            real(real64), intent(in), optional :: Temp(:)
            real(real64), intent(in), optional :: Si(:)
            real(real64), intent(in), optional :: Pres(:)
            type(type_dp_3d), intent(in), optional :: wFlux
            integer(int32), intent(in), optional :: Colors(:)

        end subroutine input_output_Overall_Output_vtu

    end interface

    type :: type_output
        ! private
        ! character(:), allocatable :: fextend
        type(type_output_observation) :: Observation

        type(Output_Overall) :: Overall

        logical(4) :: doOutput_stdout

        character(:), allocatable :: dir_output
        ! character(:), allocatable :: dir_FileOutput
        ! character(:), allocatable :: format_output

        type(Output_VTK_Series) :: VTKInfo
        character(:), allocatable :: Output_TimeUnit
        character(:), allocatable :: Interval_TimeUnit

        logical(4) :: doHeat
        logical(4) :: doPressure
        logical(4) :: doStress

        character(:), allocatable :: logfile_name

    contains
        procedure, pass(self), public :: initialize => initialize_type_output
        ! procedure, pass(self) :: Output_All_vtu => input_output_All_vtu
        ! procedure, pass(self) :: Output_All_vtk => input_output_All_vtk
        ! procedure, pass(self) :: Output_All_vtk_Scalar => input_output_All_vtk_Scalar_Field
        ! procedure, pass(self) :: Output_All_vtk_Vector => input_output_All_vtk_Vector_Field
        ! procedure, pass(self), public :: Output_All => input_output_All

        ! procedure, pass(self) :: Write_Observation_Header
        ! procedure, pass(self) :: Initialize_Observation_Header
        ! procedure, pass(self) :: interpolate_observations
        procedure, pass(self), public :: type_output_observation => Output_Process_Observation

        procedure, pass(self), public :: Output_SystemLog
    end type type_output

    ! interface type_output
    !     module procedure initialize_type_output
    ! end interface

    public :: type_output

    !----------------------------------------------------------------------
    ! Base interface
    !-----------------------------------------------------------------------
    interface
        module subroutine setup_directory(dir_path, file_extension)
            implicit none
            character(*), intent(in) :: dir_path
            character(*), intent(in) :: file_extension(:)
        end subroutine setup_directory
    end interface

    interface
        module subroutine Output_Process_Observation(self, time, Temp, Si, TC, C, Pres, wFlux, K, Thermal, phi, Propeties, domain)
            implicit none
            class(type_output) :: self
            real(real64), intent(in) :: time
            real(real64), intent(in), optional :: Temp(:)
            real(real64), intent(in), optional :: Si(:)
            real(real64), intent(in), optional :: TC(:)
            real(real64), intent(in), optional :: C(:)
            real(real64), intent(in), optional :: Pres(:)
            real(real64), intent(in), optional :: wFlux(:)
            real(real64), intent(in), optional :: K(:)
            class(abst_thermal), intent(inout), optional :: Thermal
            real(real64), intent(in), optional :: phi(:)
            type(type_proereties_manager), intent(inout), optional :: Propeties
            type(type_domain), intent(inout), optional :: domain

        end subroutine Output_Process_Observation
    end interface

    interface
        module subroutine Output_SystemLog(self, time, Matrix, domain)
            implicit none
            class(type_output), intent(inout) :: self
            type(type_time), intent(in) :: time
            type(Type_CRS), intent(in) :: Matrix
            type(type_domain), intent(inout) :: domain
        end subroutine Output_SystemLog
    end interface

contains

    subroutine initialize_type_output(self, input, domain, coordinate)
        implicit none
        class(type_output), intent(inout) :: self
        type(type_input), intent(in) :: input
        class(type_domain), intent(inout), optional :: domain
        type(type_dp_3d), intent(inout), pointer :: coordinate

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

        character(8) :: output_extentions(3) = [".dat", ".csv", ".log"]
        character(8) :: output_file_extentions(5) = [".dat", ".csv", ".vtk", ".vtu", ".log"]

        ! Path settings
        dir_Path = get_project_path()

        self%dir_output = trim(adjustl(dir_Path))//"Output/"
        call setup_directory(self%dir_output, output_extentions)
        self%Overall%dir_FileOutput = trim(adjustl(dir_Path))//"Output/Files/"
        call setup_directory(self%Overall%dir_FileOutput, output_file_extentions)

        self%logfile_name = trim(adjustl(self%dir_output))//"run.log"

        ! self%Output_TimeUnit = input%OutputSettings%Output_TimeUnit
        ! self%Interval_TimeUnit = input%OutputSettings%Interval_TimeUnit
        ! self%doHeat = any(input%Regions(:)%Flag%isHeat)
        ! self%doPressure = any(input%Regions(:)%Flag%isWater)
        ! self%doStress = any(input%Regions(:)%Flag%isStress)
        ! self%Overall%fextend = "."//trim(adjustl(input%OutputSettings%FileFormat))
        ! self%doOutput_stdout = input%Basic%shouldDisplayPrompt

        ! call self%Observation%initialize(input, coordinate, domain)
        ! allocate (self%Observation%Variables(7))

        ! call self%Observation%Variables(1)%initialize(self%dir_output, "Temperature", "obsf_T.dat", "°C", input%OutputSettings%outTemp)
        ! if (self%Observation%Variables(1)%doOutput) then
        !     self%Observation%Variables(1)%get_values => interpolate_observations_Temperature
        ! end if
        ! call self%Observation%Variables(2)%initialize(self%dir_output, "Si", "obsf_Si.dat", "-", input%OutputSettings%outSi)
        ! if (self%Observation%Variables(2)%doOutput) then
        !     self%Observation%Variables(2)%get_values => interpolate_observations_Si
        ! end if
        ! call self%Observation%Variables(3)%initialize(self%dir_output, "Thermal Conductivity", "obsf_THC.dat", "W/m/K", input%OutputSettings%outTC)
        ! if (self%Observation%Variables(3)%doOutput) then
        !     self%Observation%Variables(3)%get_values => interpolate_observations_thc
        ! end if
        ! call self%Observation%Variables(4)%initialize(self%dir_output, "Volumetric Heat Capacity", "obsf_VHC.dat", "J/m^3/K", input%OutputSettings%outC)
        ! if (self%Observation%Variables(4)%doOutput) then
        !     self%Observation%Variables(4)%get_values => interpolate_observations_VHC
        ! end if
        ! call self%Observation%Variables(5)%initialize(self%dir_output, "Pressure", "obsf_P.dat", &
        !                                               input%Regions(1)%Ice%c_unit, input%OutputSettings%outPres)
        ! if (self%Observation%Variables(5)%doOutput) then
        !     self%Observation%Variables(5)%get_values => interpolate_observations_Pw
        ! end if
        ! call self%Observation%Variables(6)%initialize(self%dir_output, "Water Flux", "obsf_Flux.dat", "m/s", input%OutputSettings%outFlux)
        ! if (self%Observation%Variables(6)%doOutput) then
        !     ! self%Observation%Variables(6)%get_values => interpolate_observations_wFlux
        ! end if
        ! call self%Observation%Variables(7)%initialize(self%dir_output, "Hydraulic Conductivity", "obsf_K.dat", "m/s", input%OutputSettings%outK)
        ! if (self%Observation%Variables(7)%doOutput) then
        !     ! self%Observation%Variables(7)%get_values => interpolate_observations_K
        ! end if

        ! if (self%doHeat) then
        !     call self%Observation%Write_Header(self%Observation%Variables(1), self%Output_TimeUnit)
        !     call self%Observation%Write_Header(self%Observation%Variables(2), self%Output_TimeUnit)
        !     call self%Observation%Write_Header(self%Observation%Variables(3), self%Output_TimeUnit)
        !     call self%Observation%Write_Header(self%Observation%Variables(4), self%Output_TimeUnit)
        ! end if
        ! if (self%doPressure) then
        !     call self%Observation%Write_Header(self%Observation%Variables(5), self%Output_TimeUnit)
        !     call self%Observation%Write_Header(self%Observation%Variables(6), self%Output_TimeUnit)
        !     call self%Observation%Write_Header(self%Observation%Variables(7), self%Output_TimeUnit)
        ! end if

        ! self%Overall%format_output = '(a,a,i5.5,a)'

        ! call self%Overall%initialize(input, coordinate, domain)

    end subroutine initialize_type_output

end module input_output

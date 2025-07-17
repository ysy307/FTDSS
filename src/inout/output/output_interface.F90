module input_output
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding, only: c_int64_t, c_ptr, c_f_pointer, c_char, c_null_char, c_associated
!$  use :: omp_lib
    use :: stdlib_strings, only:to_string
    use :: inout_project_settings, only:get_project_path
    use :: module_core, only:allocate_array, deallocate_array, type_variable, type_dp_3d, type_gauss_point_state, & !&
                             get_username, get_hostname, get_compiler_name, get_compiler_version, & !&
                             get_cpu_architecture, get_os, get_openmp_version, get_memory_usage, & !&
                             filter, type_dp_vector_3d

    use :: module_input
    use :: module_domain, only:holder_elements, create_element, type_domain, type_reordering, abst_element
    use :: module_control, only:type_time, type_iteration
    use :: module_properties, only:type_proereties_manager
    use :: module_matrix

    implicit none
    private

    type :: type_output_observation
        character(:), allocatable :: name
        character(:), allocatable :: unit
        character(:), allocatable :: file_name
        integer(int32) :: num_unit

        character(:), allocatable :: type
        integer(int32) :: num_observations
        type(type_dp_3d) :: coordinate
        !!
        type(holder_elements), allocatable :: elements(:)
        real(real64), allocatable :: xi(:)
        real(real64), allocatable :: eta(:)
        !!
        integer(int32), allocatable :: node_ids(:)
        procedure(abst_write_line), pointer, pass(self) :: write_line => null()
        procedure(abst_write_obeservation_header), pointer, pass(self) :: write_header => null()
        procedure(abst_get_values), pointer, pass(self) :: get_values => null()
    contains
        procedure, pass(self) :: initialize => initialize_type_output_observation
        ! procedure, pass(self) :: write_header => write_obeservation_header
    end type type_output_observation

    abstract interface
        subroutine abst_write_line(self, unit, time, values)
            import :: type_output_observation, real64, int32
            implicit none
            class(type_output_observation), intent(in) :: self
            integer(int32), intent(in) :: unit
            real(real64), intent(in) :: time
            real(real64), intent(in) :: values(:)

        end subroutine abst_write_line

        subroutine abst_get_values(self, obs_values, domain, properties, &
                                   nodal_temperature, nodal_porosity, nodal_pw)
            import :: type_output_observation, type_domain, type_proereties_manager, real64, int32
            implicit none
            class(type_output_observation), intent(inout) :: self
            real(real64), intent(out) :: obs_values(:)
            type(type_domain), intent(inout), optional :: domain
            type(type_proereties_manager), intent(inout), optional :: properties
            real(real64), intent(in), optional :: nodal_temperature(:)
            real(real64), intent(in), optional :: nodal_porosity(:)
            real(real64), intent(in), optional :: nodal_pw(:)
        end subroutine abst_get_values

        subroutine abst_write_obeservation_header(self, time_unit)
            import :: type_output_observation
            implicit none
            class(type_output_observation), intent(inout) :: self
            character(*), intent(in) :: time_unit

        end subroutine abst_write_obeservation_header
    end interface

    interface
        module subroutine initialize_type_output_observation(self, input, coordinate, domain, dir_output, variable_name)
            implicit none
            class(type_output_observation), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_dp_3d), intent(inout), pointer :: coordinate
            type(type_domain), intent(inout) :: domain
            character(*), intent(in) :: dir_output
            character(*), intent(in) :: variable_name

        end subroutine initialize_type_output_observation

        ! module subroutine write_obeservation_header(self, time_unit)
        !     implicit none
        !     class(type_output_observation), intent(inout) :: self
        !     character(*), intent(in) :: time_unit

        ! end subroutine write_obeservation_header
    end interface

! In a new or existing module

    type :: Output_VTK_Series
        integer(int32) :: nPoints
        integer(int32) :: nCell
        type(type_dp_3d) :: coordinates
        integer(int32), allocatable :: connectivity(:)
        integer(int32), allocatable :: offset(:)
        integer(int8), allocatable :: CellType(:)
    end type

    type :: type_output_overall
        private
        ! Output format
        character(:), allocatable :: dir_FileOutput
        character(:), allocatable :: format_output
        character(:), allocatable :: fextend
        ! DATA
        type(Output_VTK_Series) :: VTK
    contains
        procedure, pass(self), public :: initialize => initialize_input_type_output_overall
        procedure, pass(self) :: initialize_vtk => initialize_output_overall_vtk
        procedure, pass(self) :: initialize_vtu => initialize_output_overall_vtu
        procedure, pass(self), public :: Output => input_type_output_overall_Output
        procedure, pass(self) :: Output_vtu => input_type_output_overall_Output_vtu
        procedure, pass(self) :: Output_vtk => input_type_output_overall_Output_vtk
        procedure, pass(self) :: Output_vtk_scalar_int32 => input_type_output_overall_Output_vtk_scalar_int32
        procedure, pass(self) :: Output_vtk_scalar_real64 => input_type_output_overall_Output_vtk_scalar_real64
        generic :: Output_vtk_scalar => Output_vtk_scalar_int32, & !&
                                        Output_vtk_scalar_real64 !&
        procedure, pass(self) :: Output_vtk_vector => input_type_output_overall_Output_vtk_vector

    end type

    interface
        module subroutine initialize_input_type_output_overall(self, input, coordinate, domain)
            implicit none
            class(type_output_overall), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_dp_3d), intent(in) :: coordinate
            type(type_domain), intent(inout) :: domain

        end subroutine initialize_input_type_output_overall

        module subroutine initialize_output_overall_vtk(self, input, coordinate, domain)
            implicit none
            class(type_output_overall), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_dp_3d), intent(in) :: coordinate
            type(type_domain), intent(inout) :: domain

        end subroutine initialize_output_overall_vtk

        module subroutine initialize_output_overall_vtu(self, input, coordinate, domain)
            implicit none
            class(type_output_overall), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_dp_3d), intent(in) :: coordinate
            type(type_domain), intent(inout) :: domain

        end subroutine initialize_output_overall_vtu

        module subroutine input_type_output_overall_Output(self, fc, reordering, Temp, Si, Pres, wFlux, Colors)
            implicit none
            class(type_output_overall) :: self
            integer(int32), intent(in) :: fc
            type(type_reordering), intent(in) :: reordering
            real(real64), intent(in), optional :: Temp(:)
            real(real64), intent(in), optional :: Si(:)
            real(real64), intent(in), optional :: Pres(:)
            type(type_dp_3d), intent(in), optional :: wFlux
            integer(int32), intent(in), optional :: Colors(:)

        end subroutine input_type_output_overall_Output

        module subroutine input_type_output_overall_Output_vtk(self, fc, iperm, Temp, Si, Pres, wFlux, Colors)
            implicit none
            class(type_output_overall), intent(inout) :: self
            integer(int32), intent(in) :: fc
            integer(int32), intent(in), optional :: iperm(:)
            real(real64), intent(in), optional :: Temp(:)
            real(real64), intent(in), optional :: Si(:)
            real(real64), intent(in), optional :: Pres(:)
            type(type_dp_3d), intent(in), optional :: wFlux
            integer(int32), intent(in), optional :: Colors(:)

        end subroutine input_type_output_overall_Output_vtk

        module subroutine input_type_output_overall_Output_vtk_scalar_real64(self, iperm, unit_num, data_name, x)
            implicit none
            class(type_output_overall) :: self
            integer(int32), intent(in), optional :: iperm(:)
            integer(int32), intent(in) :: unit_num
            character(*), intent(in) :: data_name
            real(real64), intent(in) :: x(:)

        end subroutine input_type_output_overall_Output_vtk_scalar_real64

        module subroutine input_type_output_overall_Output_vtk_scalar_int32(self, iperm, unit_num, data_name, x)
            implicit none
            class(type_output_overall) :: self
            integer(int32), intent(in), optional :: iperm(:)
            integer(int32), intent(in) :: unit_num
            character(*), intent(in) :: data_name
            integer(int32), intent(in) :: x(:)

        end subroutine input_type_output_overall_Output_vtk_scalar_int32

        module subroutine input_type_output_overall_Output_vtk_vector(self, iperm, unit_num, data_name, x, y, z)
            implicit none
            class(type_output_overall) :: self
            integer(int32), intent(in), optional :: iperm(:)
            integer(int32), intent(in) :: unit_num
            character(*), intent(in) :: data_name
            real(real64), intent(in) :: x(:), y(:), z(:)

        end subroutine input_type_output_overall_Output_vtk_vector

        module subroutine input_type_output_overall_Output_vtu(self, fc, reordering, Temp, Si, Pres, wFlux, Colors)
            implicit none
            class(type_output_overall), intent(inout) :: self
            integer(int32), intent(in) :: fc
            type(type_reordering), intent(in) :: reordering
            real(real64), intent(in), optional :: Temp(:)
            real(real64), intent(in), optional :: Si(:)
            real(real64), intent(in), optional :: Pres(:)
            type(type_dp_3d), intent(in), optional :: wFlux
            integer(int32), intent(in), optional :: Colors(:)

        end subroutine input_type_output_overall_Output_vtu

    end interface

    type :: type_output
        ! private
        ! character(:), allocatable :: fextend
        type(type_output_observation), allocatable :: observations(:)

        type(type_output_overall) :: Overall

        character(:), allocatable :: dir_output
        ! character(:), allocatable :: dir_FileOutput
        ! character(:), allocatable :: format_output

        type(Output_VTK_Series) :: VTKInfo
        character(:), allocatable :: Output_TimeUnit
        character(:), allocatable :: Interval_TimeUnit

        character(:), allocatable :: log_file_name

    contains
        procedure, pass(self), public :: initialize => initialize_type_output
        ! procedure, pass(self) :: Output_All_vtu => input_output_All_vtu
        ! procedure, pass(self) :: Output_All_vtk => input_output_All_vtk
        ! procedure, pass(self) :: Output_All_vtk_Scalar => input_output_All_vtk_Scalar_Field
        ! procedure, pass(self) :: Output_All_vtk_Vector => input_output_All_vtk_Vector_Field
        ! procedure, pass(self), public :: Output_All => input_output_All

        procedure, pass(self), public :: output_history
        procedure, pass(self), public :: output_system_log
    end type type_output

    ! interface type_output
    !     module procedure initialize_type_output
    ! end interface

    public :: type_output

    !----------------------------------------------------------------------
    ! Base interface
    !-----------------------------------------------------------------------
    interface
        module subroutine setup_directory(dir_path, file_extensions)
            implicit none
            character(*), intent(in) :: dir_path
            character(*), intent(in) :: file_extensions(:)
        end subroutine setup_directory
    end interface

    interface
        module subroutine output_history(self, time, domain, propeties, porosity, temperature, pressure)
            implicit none
            class(Type_Output) :: self
            real(real64), intent(in) :: time
            type(type_domain), intent(inout), optional :: domain
            type(type_proereties_manager), intent(inout), optional :: propeties
            real(real64), intent(in), optional :: porosity(:)
            real(real64), intent(in), optional :: temperature(:)
            real(real64), intent(in), optional :: pressure(:)

        end subroutine output_history
    end interface

    interface
        module subroutine output_system_log(self, time, Matrix, domain)
            implicit none
            class(type_output), intent(inout) :: self
            type(type_time), intent(in) :: time
            type(Type_CRS), intent(in) :: Matrix
            type(type_domain), intent(inout) :: domain
        end subroutine output_system_log
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

        self%log_file_name = trim(adjustl(self%dir_output))//"run.log"

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
        !     call self%Observation%write_header(self%Observation%Variables(1), self%Output_TimeUnit)
        !     call self%Observation%write_header(self%Observation%Variables(2), self%Output_TimeUnit)
        !     call self%Observation%write_header(self%Observation%Variables(3), self%Output_TimeUnit)
        !     call self%Observation%write_header(self%Observation%Variables(4), self%Output_TimeUnit)
        ! end if
        ! if (self%doPressure) then
        !     call self%Observation%write_header(self%Observation%Variables(5), self%Output_TimeUnit)
        !     call self%Observation%write_header(self%Observation%Variables(6), self%Output_TimeUnit)
        !     call self%Observation%write_header(self%Observation%Variables(7), self%Output_TimeUnit)
        ! end if

        ! self%Overall%format_output = '(a,a,i5.5,a)'

        ! call self%Overall%initialize(input, coordinate, domain)

    end subroutine initialize_type_output

end module input_output

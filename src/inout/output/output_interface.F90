module input_output
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding, only: c_int64_t, c_ptr, c_f_pointer, c_char, c_null_char, c_associated
!$  use :: omp_lib
    use :: stdlib_strings, only:to_string
    use :: vtk_fortran, only:vtk_file
    use :: module_core, only:allocate_array, deallocate_array, type_variable, type_coordinate_array_dp, type_state, & !&
                             get_username, get_hostname, get_compiler_name, get_compiler_version, & !&
                             get_cpu_architecture, get_os, get_openmp_version, get_memory_usage, & !&
                             filter, type_coordinate_dp, assignment(=), type_crs

    use :: module_input
    use :: inout_project_settings, only:get_project_path
    use :: module_domain, only:holder_elements, create_element, type_domain, type_reordering, abst_element
    use :: module_control, only:type_time, type_iteration
    use :: module_properties, only:type_properties_manager

    implicit none
    private

    public :: type_output

    type :: type_output_observation
        character(:), allocatable :: name
        character(:), allocatable :: unit
        character(:), allocatable :: file_name
        integer(int32) :: num_unit

        character(:), allocatable :: type
        logical :: do_output
        integer(int32) :: num_observations
        type(type_coordinate_array_dp) :: coordinate
        !!
        type(holder_elements), allocatable :: elements(:)
        type(type_coordinate_dp), allocatable :: coordinate_normalized(:)
        !!
        integer(int32), allocatable :: node_ids(:)
        procedure(abst_write_line), pointer, pass(self) :: write_line => null()
        procedure(abst_write_obeservation_header), pointer, pass(self) :: write_header => null()
        procedure(abst_get_values), pointer, pass(self) :: get_values => null()
    contains
        procedure, pass(self) :: initialize => initialize_type_output_observation
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
            import :: type_output_observation, type_domain, type_properties_manager, real64, int32
            implicit none
            class(type_output_observation), intent(inout) :: self
            real(real64), intent(out) :: obs_values(:)
            type(type_domain), intent(inout), optional :: domain
            type(type_properties_manager), intent(inout), optional :: properties
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
            type(type_coordinate_array_dp), intent(inout), pointer :: coordinate
            type(type_domain), intent(inout) :: domain
            character(*), intent(in) :: dir_output
            character(*), intent(in) :: variable_name

        end subroutine initialize_type_output_observation
    end interface

    type :: type_output_vtk
        integer(int32) :: num_points
        integer(int32) :: num_cells
        type(type_coordinate_array_dp) :: coordinate
        integer(int32), allocatable :: connectivities(:)
        integer(int32), allocatable :: offsets(:)
        integer(int8), allocatable :: cell_types(:)
    end type

    type :: type_output_overall
        private
        ! Output format
        character(:), allocatable :: dir_output_field
        character(:), allocatable :: format_output
        character(:), allocatable :: file_extension
        character(:), allocatable :: variable_names(:)
        logical :: do_output
        ! DATA
        type(type_output_vtk) :: vtk
        procedure(abst_output_overall_fields), pointer, pass(self) :: write_fields => null()
        procedure(abst_output_overall_cell), pointer, pass(self) :: write_cell => null()
    contains
        procedure, pass(self), public :: initialize => initialize_input_type_output_overall
        procedure, pass(self) :: initialize_vtk => initialize_output_overall_vtk
        procedure, pass(self) :: initialize_vtu => initialize_output_overall_vtu
    end type

    abstract interface
        subroutine abst_output_overall_fields(self, file_counts, domain, porosity, temperature, si, pressure, water_flux)
            import :: type_output_overall, type_domain, type_coordinate_array_dp, real64, int32
            implicit none
            class(type_output_overall), intent(inout) :: self
            integer(int32), intent(in) :: file_counts
            type(type_domain), intent(in) :: domain
            real(real64), intent(in), optional :: porosity(:)
            real(real64), intent(in), optional :: temperature(:)
            real(real64), intent(in), optional :: si(:)
            real(real64), intent(in), optional :: pressure(:)
            type(type_coordinate_array_dp), intent(in), optional :: water_flux

        end subroutine abst_output_overall_fields

        subroutine abst_output_overall_cell(self, file_name, variable_name, variable)
            import :: type_output_overall, int32
            implicit none
            class(type_output_overall), intent(inout) :: self
            character(*), intent(in) :: file_name
            character(*), intent(in) :: variable_name
            integer(int32), intent(in) :: variable(:)

        end subroutine abst_output_overall_cell
    end interface

    interface
        module subroutine initialize_input_type_output_overall(self, input, coordinate, domain, dir_output)
            implicit none
            class(type_output_overall), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_coordinate_array_dp), intent(in) :: coordinate
            type(type_domain), intent(inout) :: domain
            character(*), intent(in) :: dir_output

        end subroutine initialize_input_type_output_overall

        module subroutine initialize_output_overall_vtk(self, input, coordinate, domain)
            implicit none
            class(type_output_overall), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_coordinate_array_dp), intent(in) :: coordinate
            type(type_domain), intent(inout) :: domain

        end subroutine initialize_output_overall_vtk

        module subroutine initialize_output_overall_vtu(self, input, coordinate, domain)
            implicit none
            class(type_output_overall), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_coordinate_array_dp), intent(in) :: coordinate
            type(type_domain), intent(inout) :: domain

        end subroutine initialize_output_overall_vtu

    end interface

    type :: type_output
        private
        character(:), allocatable :: dir_output
        character(:), allocatable :: dir_output_field
        character(:), allocatable :: log_file_name

        logical :: is_thermal
        logical :: is_hydraulic

        type(type_output_observation), allocatable :: observations(:)
        type(type_output_overall) :: overall

    contains
        procedure, pass(self), public :: initialize => initialize_type_output
        procedure, pass(self), public :: output_fields
        procedure, pass(self), public :: output_coloring
        procedure, pass(self), public :: output_history
        procedure, pass(self), public :: output_system_log
    end type type_output

    ! interface type_output
    !     module procedure initialize_type_output
    ! end interface

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
        type(type_coordinate_array_dp), intent(inout), pointer :: coordinate

        character(256) :: dir_path
        logical :: exists

        character(:), allocatable :: command
        integer(int32) :: i, j, idx
        integer(int32) :: total
        real(real64) :: tmp_xi, tmp_eta
        logical :: is_inside

        integer(int32) :: iObs, iElem
        integer(int32) :: nElements
        integer(int32) :: local_id, local_type
        integer(int32) :: ierr

        character(8) :: output_extentions(3) = [".dat", ".csv", ".log"]
        character(8) :: output_file_extentions(5) = [".dat", ".csv", ".vtk", ".vtu", ".log"]

        ! Path settings
        dir_path = get_project_path()

        self%dir_output = trim(adjustl(dir_path))//"Output/"
        call setup_directory(self%dir_output, output_extentions)
        self%dir_output_field = trim(adjustl(dir_path))//"Output/Files/"
        call setup_directory(self%dir_output_field, output_file_extentions)

        self%log_file_name = trim(adjustl(self%dir_output))//"run.log"

        self%is_thermal = input%basic%analysis_controls%calculate_thermal
        self%is_hydraulic = input%basic%analysis_controls%calculate_hydraulic

        if (allocated(self%observations)) deallocate (self%observations)
        allocate (self%observations(size(input%output_settings%history_output%variable_names)))
        do i = 1, size(input%output_settings%history_output%variable_names)
            call self%observations(i)%initialize(input, coordinate, domain, self%dir_output, &
                                                 input%output_settings%history_output%variable_names(i))
            call self%observations(i)%write_header(input%output_settings%history_output%output_interval_unit)
        end do

        call self%overall%initialize(input, coordinate, domain, self%dir_output_field)

    end subroutine initialize_type_output

    subroutine output_fields(self, file_counts, domain, porosity, temperature, si, pressure, water_flux)
        implicit none
        class(type_output), intent(inout) :: self
        integer(int32), intent(in) :: file_counts
        type(type_domain), intent(in) :: domain
        real(real64), intent(in), optional :: porosity(:)
        real(real64), intent(in), optional :: temperature(:)
        real(real64), intent(in), optional :: si(:)
        real(real64), intent(in), optional :: pressure(:)
        type(type_coordinate_array_dp), intent(in), optional :: water_flux

        if (.not. self%overall%do_output) return

        if (self%is_thermal .and. self%is_hydraulic) then
            call self%overall%write_fields(file_counts=file_counts, &
                                           domain=domain, &
                                           porosity=porosity, &
                                           temperature=temperature, &
                                           si=si, &
                                           pressure=pressure, &
                                           water_flux=water_flux)
        else if (self%is_thermal) then
            call self%overall%write_fields(file_counts=file_counts, &
                                           domain=domain, &
                                           porosity=porosity, &
                                           temperature=temperature, &
                                           si=si)
        else if (self%is_hydraulic) then
            call self%overall%write_fields(file_counts=file_counts, &
                                           domain=domain, &
                                           porosity=porosity, &
                                           pressure=pressure, &
                                           water_flux=water_flux)
        end if

    end subroutine output_fields

    subroutine output_coloring(self, domain)
        implicit none
        class(type_output), intent(inout) :: self
        type(type_domain), intent(in) :: domain

        integer(int32), allocatable :: coloring(:)
        integer(int32) :: num_sides, num_elements

        select case (domain%get_computation_dimension())
        case (2)
            num_sides = domain%get_num_sides()
            num_elements = domain%get_num_elements()
            if (allocated(coloring)) deallocate (coloring)
            allocate (coloring(num_sides + num_elements))
            coloring(1:num_sides) = 0
            coloring(num_sides + 1:num_sides + num_elements) = domain%colors%color(1:num_elements)
        end select

        call self%overall%write_cell(file_name="coloring", variable_name="Coloring", variable=coloring)

    end subroutine output_coloring

    !----------------------------------------------------------------------!
    ! output_history:
    !----------------------------------------------------------------------!
    ! This subroutine handles the processing and output of observation
    ! data at a given time step. It supports both nodal and interpolated
    ! observation types and multiple physical variables.
    !
    ! Subroutine Details:
    !   - For each enabled observation type and available input, performs:
    !       * Initialization of observation header (if needed)
    !       * Selection between direct node ID or interpolated values
    !       * Optional post-processing (e.g., ice content calculations via Thermal model)
    !   - Writes the results to the corresponding output files with time-stamped lines.
    !   - Supports extensibility by checking optional arguments and types (e.g., GCC, EXP models).
    !
    !----------------------------------------------------------------------!
    subroutine output_history(self, time, domain, propeties, porosity, temperature, pressure)
        implicit none
        class(type_output) :: self
        real(real64), intent(in) :: time
        type(type_domain), intent(inout), optional :: domain
        type(type_properties_manager), intent(inout), optional :: propeties
        real(real64), intent(in), optional :: porosity(:)
        real(real64), intent(in), optional :: temperature(:)
        real(real64), intent(in), optional :: pressure(:)

        real(real64) :: obsValues(3 * size(self%observations))

        integer(int32) :: iObs

        do iObs = 1, size(self%observations)
            if (.not. self%observations(iObs)%do_output) cycle
            if (self%is_thermal .and. self%is_hydraulic) then
                call self%observations(iObs)%get_values(obs_values=obsValues, &
                                                        nodal_temperature=temperature, &
                                                        nodal_porosity=porosity, &
                                                        nodal_pw=pressure, &
                                                        properties=propeties, &
                                                        domain=domain)
            else if (self%is_thermal) then
                call self%observations(iObs)%get_values(obs_values=obsValues, &
                                                        nodal_temperature=temperature, &
                                                        nodal_porosity=porosity, &
                                                        properties=propeties, &
                                                        domain=domain)
            else if (self%is_hydraulic) then
                call self%observations(iObs)%get_values(obs_values=obsValues, &
                                                        nodal_pw=pressure, &
                                                        nodal_porosity=porosity, &
                                                        properties=propeties, &
                                                        domain=domain)
            end if
            call self%observations(iObs)%write_line( &
                unit=self%observations(iObs)%num_unit, &
                time=time, &
                values=obsValues)
        end do

    end subroutine output_history

end module input_output

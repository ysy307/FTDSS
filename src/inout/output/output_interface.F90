module inout_output
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding, only: c_int64_t, c_ptr, c_f_pointer, c_char, c_null_char, c_associated
    use :: omp_lib
    use :: stdlib_strings, only:to_string, strip
    use :: vtk_fortran, only:vtk_file
    use :: module_core
    use :: module_input
    use :: module_domain
    use :: module_control
    use :: module_physics

    implicit none
    private

    public :: type_output

    !---------------------------------------------------------------------------
    ! type_output_observation
    !---------------------------------------------------------------------------
    type :: type_output_observation
        character(:), allocatable :: name
        character(:), allocatable :: unit
        character(:), allocatable :: file_name
        integer(int32) :: num_unit

        character(:), allocatable :: type
        logical :: do_output
        integer(int32) :: num_observations
        type(type_coordinate_array_dp) :: coordinate

        integer(int32), allocatable :: element_ids(:)
        type(type_coordinate_dp), allocatable :: coordinate_normalized(:)
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

        subroutine abst_get_values(self, obs_values, domain, &
                                   nodal_temperature, nodal_porosity, nodal_pw)
            import :: type_output_observation, type_domain, type_physics_manager, real64, int32
            implicit none
            class(type_output_observation), intent(inout) :: self
            real(real64), intent(inout) :: obs_values(:)
            type(type_domain), intent(inout), optional :: domain
            real(real64), intent(in), optional :: nodal_temperature(:)
            real(real64), intent(in), optional :: nodal_porosity(:)
            real(real64), intent(in), optional :: nodal_pw(:)
        end subroutine abst_get_values

        subroutine abst_write_obeservation_header(self, time_unit)
            import :: type_output_observation, int32
            implicit none
            class(type_output_observation), intent(inout) :: self
            integer(int32), intent(in) :: time_unit
        end subroutine abst_write_obeservation_header
    end interface

    interface
        ! [修正] coordinate を削除
        module subroutine initialize_type_output_observation(self, input, domain, dir_output, variable_name)
            implicit none
            class(type_output_observation), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_domain), intent(inout) :: domain
            character(*), intent(in) :: dir_output
            character(*), intent(in) :: variable_name
        end subroutine initialize_type_output_observation
    end interface

    !---------------------------------------------------------------------------
    ! type_output_vtk / type_output_overall
    !---------------------------------------------------------------------------
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
        ! [修正] control 追加, coordinate 削除
        module subroutine initialize_input_type_output_overall(self, input, control, domain, dir_output)
            implicit none
            class(type_output_overall), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_controls), intent(in) :: control
            type(type_domain), intent(inout) :: domain
            character(*), intent(in) :: dir_output
        end subroutine initialize_input_type_output_overall

        ! [修正] coordinate 削除
        module subroutine initialize_output_overall_vtk(self, input, domain)
            implicit none
            class(type_output_overall), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_domain), intent(inout) :: domain
        end subroutine initialize_output_overall_vtk

        ! [修正] coordinate 削除
        module subroutine initialize_output_overall_vtu(self, input, domain)
            implicit none
            class(type_output_overall), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_domain), intent(inout) :: domain
        end subroutine initialize_output_overall_vtu
    end interface

    !---------------------------------------------------------------------------
    ! type_output
    !---------------------------------------------------------------------------
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
        procedure, pass(self), public :: output_history
        procedure, pass(self), public :: output_system_log
    end type type_output

    interface
        module subroutine setup_directory(dir_path, file_extensions)
            implicit none
            character(*), intent(in) :: dir_path
            character(*), intent(in) :: file_extensions(:)
        end subroutine setup_directory
    end interface

    interface
        module subroutine output_system_log(self, control, matrix, domain)
            implicit none
            class(type_output), intent(inout) :: self
            type(type_controls), intent(in) :: control
            class(abst_matrix), intent(in) :: matrix
            type(type_domain), intent(inout) :: domain
        end subroutine output_system_log
    end interface

contains

    subroutine initialize_type_output(self, input, control, domain)
        implicit none
        class(type_output), intent(inout) :: self
        type(type_input), intent(in) :: input
        type(type_controls), intent(in) :: control
        type(type_domain), intent(inout) :: domain

        integer(int32) :: i
        character(:), allocatable :: project_path_env
        character(8) :: output_extentions(3) = [".dat", ".csv", ".log"]
        character(8) :: output_file_extentions(5) = [".dat", ".csv", ".vtk", ".vtu", ".log"]
        character(*), parameter :: PROJECT_ENV = "FTDSS_PROJECT_PATH"

        call get_env_string(PROJECT_ENV, project_path_env)
        call modify_path_format(project_path_env)
        self%dir_output = trim(adjustl(project_path_env))//"Output/"
        call setup_directory(self%dir_output, output_extentions)
        self%dir_output_field = trim(adjustl(project_path_env))//"Output/Files/"
        call setup_directory(self%dir_output_field, output_file_extentions)

        self%log_file_name = trim(adjustl(self%dir_output))//"run.log"

        self%is_thermal = input%basic%analysis_controls%is_active(PHYSICS_TYPE_THERMAL)
        self%is_hydraulic = input%basic%analysis_controls%is_active(PHYSICS_TYPE_HYDRAULIC)

        if (allocated(self%observations)) deallocate (self%observations)
        allocate (self%observations(size(input%output_settings%history_output%variable_names)))
        do i = 1, size(input%output_settings%history_output%variable_names)
            ! [修正] coordinate 引数を削除, domain を渡す
            call self%observations(i)%initialize(input, domain, self%dir_output, &
                                                 input%output_settings%history_output%variable_names(i))
            call self%observations(i)%write_header(input%output_settings%history_output%output_time_unit)
        end do

        ! [修正] control を渡し, coordinate を削除
        call self%overall%initialize(input, control, domain, self%dir_output_field)
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

        call self%overall%write_fields(file_counts=file_counts, &
                                       domain=domain, &
                                       porosity=porosity, &
                                       temperature=temperature, &
                                       si=si, &
                                       pressure=pressure, &
                                       water_flux=water_flux)
    end subroutine output_fields

    subroutine output_history(self, time, domain, porosity, temperature, pressure)
        implicit none
        class(type_output), intent(inout) :: self
        real(real64), intent(in) :: time
        type(type_domain), intent(inout), optional :: domain
        real(real64), intent(in), optional :: porosity(:)
        real(real64), intent(in), optional :: temperature(:)
        real(real64), intent(in), optional :: pressure(:)

        real(real64) :: obsValues(3 * size(self%observations))
        integer(int32) :: iObs

        do iObs = 1, size(self%observations)
            if (.not. self%observations(iObs)%do_output) cycle
            call self%observations(iObs)%get_values(obs_values=obsValues, &
                                                    nodal_temperature=temperature, &
                                                    nodal_porosity=porosity, &
                                                    nodal_pw=pressure, &
                                                    domain=domain)
            call self%observations(iObs)%write_line( &
                unit=self%observations(iObs)%num_unit, &
                time=time, &
                values=obsValues)
        end do
    end subroutine output_history

end module inout_output

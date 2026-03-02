module io_output_manager
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding, only: c_int64_t, c_ptr, c_f_pointer, c_char, c_null_char, c_associated
    use :: omp_lib
    use :: stdlib_strings, only:to_string, strip
    use :: stdlib_io, only:open
    use :: vtk_fortran, only:vtk_file
    use :: module_core
    use :: module_input
    use :: module_domain
    use :: module_control

    use :: io_output_overall, only:type_output_overall
    use :: io_output_observation, only:type_output_observation
    use :: io_output_logging, only:type_output_log

    implicit none
    private

    public :: type_output

    ! !---------------------------------------------------------------------------
    ! ! type_output
    ! !---------------------------------------------------------------------------
    type :: type_output
        logical, private, allocatable :: active(:)

        type(type_output_observation), private, allocatable :: observations(:)
        type(type_output_overall), private :: overall
        type(type_output_log), private :: log

    contains
        procedure, pass(self), public :: initialize => initialize_type_output
        procedure, pass(self), public :: output_fields
        procedure, pass(self), public :: output_history
        procedure, pass(self), public :: get_log_io_unit
        procedure, pass(self), public :: output_system_log
    end type type_output

contains

    subroutine initialize_type_output(self, config_output)
        implicit none
        class(type_output), intent(inout) :: self
        type(type_config_output), intent(in) :: config_output

        integer(int32) :: i
        character(:), allocatable :: project_path_env
        character(8) :: output_extentions(3) = [".dat", ".csv", ".log"]
        character(8) :: output_file_extentions(5) = [".dat", ".csv", ".vtk", ".vtu", ".log"]
        character(*), parameter :: PROJECT_ENV = "FTDSS_PROJECT_PATH"

        character(:), allocatable :: dir_output
        character(:), allocatable :: dir_output_field

        call get_env_string(PROJECT_ENV, project_path_env)
        call modify_path_format(project_path_env)
        dir_output = trim(adjustl(project_path_env))//"Output/"
        call setup_directory(dir_output, output_extentions)
        dir_output_field = trim(adjustl(project_path_env))//"Output/Files/"
        call setup_directory(dir_output_field, output_file_extentions)

        call allocate_array(self%active, source=config_output%is_output_enabled)

        ! if (allocated(self%observations)) deallocate (self%observations)
        ! allocate (self%observations(size(input%output_settings%history_output%variable_names)))
        ! do i = 1, size(input%output_settings%history_output%variable_names)
        !     ! [修正] coordinate 引数を削除, domain を渡す
        !     call self%observations(i)%initialize(input, dir_output, &
        !                                          input%output_settings%history_output%variable_names(i))
        !     call self%observations(i)%write_header(input%output_settings%history_output%output_time_unit)
        ! end do

        ! ! [修正] control を渡し, coordinate を削除
        ! call self%overall%initialize(input, control, dir_output_field)
    end subroutine initialize_type_output

    subroutine output_fields(self, file_counts, porosity, temperature, si, pressure, water_flux)
        implicit none
        class(type_output), intent(inout) :: self
        integer(int32), intent(in) :: file_counts
        real(real64), intent(in), optional :: porosity(:)
        real(real64), intent(in), optional :: temperature(:)
        real(real64), intent(in), optional :: si(:)
        real(real64), intent(in), optional :: pressure(:)
        type(type_coordinate_array_dp), intent(in), optional :: water_flux

        if (.not. self%overall%should_output()) return

        call self%overall%write_fields(file_counts=file_counts, &
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
            if (.not. self%observations(iObs)%should_output()) cycle
            call self%observations(iObs)%get_values(obs_values=obsValues, &
                                                    nodal_temperature=temperature, &
                                                    nodal_porosity=porosity, &
                                                    nodal_pw=pressure, &
                                                    domain=domain)
            call self%observations(iObs)%write_line(time=time, values=obsValues)
        end do
    end subroutine output_history

    subroutine get_log_io_unit(self, io_unit)
        implicit none
        class(type_output), intent(in) :: self
        integer(int32), intent(inout) :: io_unit

        call self%log%get_io_unit(io_unit)
    end subroutine get_log_io_unit

    subroutine output_system_log(self)
        implicit none
        class(type_output), intent(inout) :: self

        call self%log%output_system_log()
    end subroutine output_system_log

end module io_output_manager

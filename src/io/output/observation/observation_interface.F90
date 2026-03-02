module io_output_observation
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

    implicit none
    private

    public :: type_output_observation

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
            import :: type_output_observation, type_domain, real64, int32
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
        module subroutine initialize_type_output_observation(self, input, domain, dir_output, variable_name)
            implicit none
            class(type_output_observation), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_domain), intent(inout) :: domain
            character(*), intent(in) :: dir_output
            character(*), intent(in) :: variable_name
        end subroutine initialize_type_output_observation
    end interface

end module io_output_observation

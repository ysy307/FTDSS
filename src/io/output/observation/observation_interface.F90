module io_output_observation
    use, intrinsic :: iso_fortran_env
    use :: stdlib_strings, only:to_string, strip
    use :: stdlib_io, only:open
    use :: module_core
    use :: module_input
    use :: module_domain
    use :: module_control

    implicit none
    private

    public :: type_output_observation

    !> Abstract base representing a single observation location
    type, abstract :: abst_observation_point
    contains
        procedure(abst_extract_value), public, pass(self), deferred :: extract_value
    end type abst_observation_point

    abstract interface
        !> Evaluates the field data at this specific observation point
        subroutine abst_extract_value(self, nodal_values, value)
            import :: abst_observation_point, real64
            implicit none
            class(abst_observation_point), intent(in) :: self
            real(real64), intent(in) :: nodal_values(:)
            real(real64), intent(inout) :: value
        end subroutine abst_extract_value
    end interface

    !> Observation point defined by spatial coordinates
    type, extends(abst_observation_point) :: type_observation_point_coordinate
        type(type_coordinate_dp), private :: coordinate
        integer(int32), private :: element_id
        type(type_coordinate_dp), private :: coordinate_normalized
        integer(int32), allocatable :: connectivity(:)
        class(abst_fe), pointer :: fe => null()
    contains
        procedure, pass(self) :: extract_value => extract_value_coordinate
    end type type_observation_point_coordinate

    interface
        module subroutine extract_value_coordinate(self, nodal_values, value)
            implicit none
            class(type_observation_point_coordinate), intent(in) :: self
            real(real64), intent(in) :: nodal_values(:)
            real(real64), intent(inout) :: value
        end subroutine extract_value_coordinate
    end interface

    !> Observation point defined by a specific mesh node ID
    type, extends(abst_observation_point) :: type_observation_point_node
        integer(int32) :: node_id
    contains
        procedure, pass(self) :: extract_value => extract_value_node
    end type type_observation_point_node

    interface
        module subroutine extract_value_node(self, nodal_values, value)
            implicit none
            class(type_observation_point_node), intent(in) :: self
            real(real64), intent(in) :: nodal_values(:)
            real(real64), intent(inout) :: value
        end subroutine extract_value_node
    end interface

    !> Wrapper to hold an array of polymorphic observation points
    type :: holder_observation_point
        class(abst_observation_point), allocatable :: point
    end type holder_observation_point

    !> Manager for file output and iterating over observation points
    type :: type_output_observation
        logical :: do_output
        type(type_constant_id), private :: variable_type
        character(:), private, allocatable :: variable_unit
        character(:), private, allocatable :: file_name
        character(:), private, allocatable :: delimiter
        character(:), private, allocatable :: fmt_line
        integer(int32), private :: io_unit

        type(type_constant_id), private :: observation_type = OUTPUT_OBSERVATION_TYPES%NONE
        integer(int32), private :: num_observations

        ! Renamed from output_methods to observation_points
        type(holder_observation_point), private, allocatable :: observation_points(:)

    contains
        procedure, public, pass(self) :: initialize => initialize_type_output_observation
        procedure, public, pass(self) :: should_output => should_output_overall

        procedure, public, pass(self) :: write_header => write_observation_header
        procedure, public, pass(self) :: write_line => write_observation_line
    end type type_output_observation

    interface
        module subroutine initialize_type_output_observation(self, input, domain, dir_output, variable_type)
            implicit none
            class(type_output_observation), intent(inout) :: self
            type(type_input), intent(in) :: input
            type(type_domain), intent(inout) :: domain
            character(*), intent(in) :: dir_output
            type(type_constant_id), intent(in) :: variable_type
        end subroutine initialize_type_output_observation

        module pure function should_output_overall(self) result(should_output)
            implicit none
            class(type_output_observation), intent(in) :: self
            logical :: should_output
        end function should_output_overall

        module subroutine write_observation_header(self, output_time_unit)
            implicit none
            class(type_output_observation), intent(inout) :: self
            type(type_constant_id), intent(in) :: output_time_unit
        end subroutine write_observation_header

        module subroutine write_observation_line(self, time, values)
            implicit none
            class(type_output_observation), intent(in) :: self
            real(real64), intent(in) :: time
            real(real64), intent(in) :: values(:)
        end subroutine write_observation_line
    end interface

end module io_output_observation

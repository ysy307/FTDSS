module inout_input_conditions
    use, intrinsic :: iso_fortran_env
!$  use :: omp_lib
    use :: mpi_f08
    use :: stdlib_strings, only:to_string, strip, ends_with
    use :: stdlib_logger
    use :: json_module, only:json_file
    use :: module_core, only:join, error_message, allocate_array
    use :: inout_input_base, only:get_json_value, abst_input
    implicit none
    private

    public :: type_conditions

    type :: type_time_controls_simulation_period
        character(:), allocatable :: unit
        real(real64) :: start
        real(real64) :: end
    end type type_time_controls_simulation_period

    type :: type_time_controls_time_stepping
        character(:), allocatable :: unit
        real(real64) :: initial_step
        real(real64) :: min_step
        real(real64) :: max_step
    end type type_time_controls_time_stepping

    type :: type_time_controls
        type(type_time_controls_simulation_period) :: simulation_period
        type(type_time_controls_time_stepping) :: time_stepping
        real(real64), allocatable :: boundary_time_points(:)
    contains
        procedure, pass(self) :: display => display_time_controls
    end type type_time_controls

    interface
        module subroutine display_time_controls(self)
            implicit none
            class(type_time_controls), intent(in) :: self
        end subroutine display_time_controls
    end interface

    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_boundary_local
        character(:), allocatable :: type
        real(real64), allocatable :: values(:)
    end type type_boundary_local

    type :: type_boundary_conditions
        integer(int32) :: id
        logical :: calculate_thermal
        logical :: calculate_hydraulic
        logical :: calculate_mechanical
        type(type_boundary_local) :: thermal
        type(type_boundary_local) :: hydraulic
    contains
        procedure, pass(self) :: display => display_boundary_conditions
    end type type_boundary_conditions

    interface
        module subroutine display_boundary_conditions(self)
            implicit none
            class(type_boundary_conditions), intent(in) :: self
        end subroutine display_boundary_conditions
    end interface
    !!------------------------------------------------------------------------------------------------------------------------------
    type, extends(type_boundary_local) :: type_boundary_local_initial
        integer(int32) :: id
    end type

    type :: type_initial_local
        character(:), allocatable :: type
        real(real64) :: value
        type(type_boundary_local_initial), allocatable :: boundary(:)
        character(:), allocatable :: field_name
    end type type_initial_local

    type :: type_initial_conditions
        type(type_initial_local) :: thermal
        type(type_initial_local) :: hydraulic
        type(type_initial_local) :: porosity
    contains
        procedure, pass(self) :: display => display_initial_conditions
    end type type_initial_conditions

    interface
        module subroutine display_initial_conditions(self)
            implicit none
            class(type_initial_conditions), intent(in) :: self
        end subroutine display_initial_conditions
    end interface
    !!------------------------------------------------------------------------------------------------------------------------------
    type :: type_conditions
        class(abst_input), pointer :: parent => null()
        character(:), allocatable :: file_name
        type(type_time_controls) :: time_control
        type(type_boundary_conditions), allocatable :: boundary_conditions(:)
        integer(int32) :: num_boundaries
        type(type_initial_conditions) :: initial_conditions
    contains
        procedure, pass(self) :: initialize => initialize_type_conditions
    end type type_conditions

    interface
        module subroutine read_conditions_time_controls(self, json)
            implicit none
            class(type_conditions), intent(inout) :: self
            type(json_file), intent(inout) :: json
        end subroutine read_conditions_time_controls

        module subroutine read_conditions_boundary_conditions(self, json)
            implicit none
            class(type_conditions), intent(inout) :: self
            type(json_file), intent(inout) :: json
        end subroutine read_conditions_boundary_conditions

        module subroutine read_conditions_initial_conditions(self, json)
            implicit none
            class(type_conditions), intent(inout) :: self
            type(json_file), intent(inout) :: json
        end subroutine read_conditions_initial_conditions
    end interface

contains
    subroutine initialize_type_conditions(self)
!         !> Load the boundary/initial conditions from the JSON file
        implicit none
        class(type_conditions), intent(inout) :: self
        type(json_file) :: json

        integer(int32) :: my_rank, ierr, i

        call json%initialize()
        call json%load(filename=self%file_name)
        call json%print_error_message(output_unit)

        call read_conditions_time_controls(self, json)
        call read_conditions_boundary_conditions(self, json)
        call read_conditions_initial_conditions(self, json)

        call json%destroy()
        call json%print_error_message(output_unit)

    end subroutine initialize_type_conditions

end module inout_input_conditions

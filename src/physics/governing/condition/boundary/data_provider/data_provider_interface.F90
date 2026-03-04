!> Abstract layer for boundary condition data provision.
!! Defines the interface for retrieving physical values from various sources.
module condition_boundary_data_provider
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core
    use :: condition_boundary_dto
    implicit none
    private

    ! --- Public Types ---
    public :: abst_bc_data
    public :: type_bc_data_constant
    public :: type_bc_data_table
    public :: type_bc_data_dynamic

    ! ==========================================================================
    ! Abstract Base Class
    ! ==========================================================================

    !> Abstract base for data providers that supply values to boundary strategies.
    type, abstract :: abst_bc_data
        !> Number of physical variables provided by this instance.
        integer(int32), private :: num_variables = 0
    contains
        ! ---- Lifecycle ----
        !> Initializes the data provider using configuration object.
        procedure(abst_initialize_bc_data), public, pass(self), deferred :: initialize
        !> Finalizes the provider and releases resources.
        procedure(abst_destroy_bc_data), public, pass(self), deferred :: destroy
        ! ---- Algorithm / Operation ----
        !> Retrieves numerical values for a given time and state.
        procedure(abst_get_data), public, pass(self), deferred :: get_data
    end type abst_bc_data

    ! ==========================================================================
    ! Abstract Interfaces
    ! ==========================================================================

    abstract interface
        !> Interface for initialization.
        subroutine abst_initialize_bc_data(self, config_bc)
            import :: abst_bc_data, type_config_bc
            implicit none
            class(abst_bc_data), intent(inout) :: self
            !> Boundary condition configuration data.
            type(type_config_bc), intent(in) :: config_bc

        end subroutine abst_initialize_bc_data

        !> Interface for resource cleanup.
        subroutine abst_destroy_bc_data(self)
            import :: abst_bc_data
            implicit none
            class(abst_bc_data), intent(inout) :: self

        end subroutine abst_destroy_bc_data

        !> Interface for data retrieval.
        !! \[ \mathbf{v} = \mathbf{f}(t, \mathbf{u}) \]
        subroutine abst_get_data(self, current_time, output_value)
            import :: abst_bc_data, abst_bc_dto, real64
            implicit none
            class(abst_bc_data), intent(inout) :: self
            !> Current simulation time [s].
            real(real64), intent(in) :: current_time
            !> Calculated boundary values.
            class(abst_bc_dto), intent(inout) :: output_value

        end subroutine abst_get_data
    end interface

    ! ==========================================================================
    ! Concrete Derived Types
    ! ==========================================================================

    !> Provider for constant values.
    type, extends(abst_bc_data) :: type_bc_data_constant
        !> Internal storage for constant parameters.
        real(real64), allocatable, private :: constant_values(:)
    contains
        procedure, public, pass(self) :: initialize => initialize_type_bc_data_constant
        procedure, public, pass(self) :: destroy => destroy_type_bc_data_constant
        procedure, public, pass(self) :: get_data => get_data_bc_data_constant
    end type type_bc_data_constant

    interface
        module subroutine initialize_type_bc_data_constant(self, config_bc)
            implicit none
            class(type_bc_data_constant), intent(inout) :: self
            type(type_config_bc), intent(in) :: config_bc

        end subroutine initialize_type_bc_data_constant

        module subroutine destroy_type_bc_data_constant(self)
            implicit none
            class(type_bc_data_constant), intent(inout) :: self

        end subroutine destroy_type_bc_data_constant

        module subroutine get_data_bc_data_constant(self, current_time, output_value)
            implicit none
            class(type_bc_data_constant), intent(inout) :: self
            real(real64), intent(in) :: current_time
            class(abst_bc_dto), intent(inout) :: output_value

        end subroutine get_data_bc_data_constant
    end interface

    !> Provider for time-series data using linear interpolation.
    type, extends(abst_bc_data) :: type_bc_data_table
        !> Array of discrete time points.
        real(real64), allocatable, private :: time_points(:)
        !> Matrix of values corresponding to time points.
        real(real64), allocatable, private :: table_values(:, :)
        !> Cached index of the last accessed time interval.
        integer(int32), private :: current_idx = 1
    contains
        procedure, public, pass(self) :: initialize => initialize_type_bc_data_table
        procedure, public, pass(self) :: destroy => destroy_type_bc_data_table
        procedure, public, pass(self) :: get_data => get_data_bc_data_table
        procedure, private, pass(self) :: calc_time_coefficient => calc_time_coefficient_bc_data_table
    end type type_bc_data_table

    interface
        module subroutine initialize_type_bc_data_table(self, config_bc)
            implicit none
            class(type_bc_data_table), intent(inout) :: self
            type(type_config_bc), intent(in) :: config_bc

        end subroutine initialize_type_bc_data_table

        module subroutine destroy_type_bc_data_table(self)
            implicit none
            class(type_bc_data_table), intent(inout) :: self

        end subroutine destroy_type_bc_data_table

        module subroutine get_data_bc_data_table(self, current_time, output_value)
            implicit none
            class(type_bc_data_table), intent(inout) :: self
            real(real64), intent(in) :: current_time
            class(abst_bc_dto), intent(inout) :: output_value

        end subroutine get_data_bc_data_table

        module subroutine calc_time_coefficient_bc_data_table(self, current_time, coef, idx)
            implicit none
            class(type_bc_data_table), intent(inout) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(inout) :: coef
            integer(int32), intent(inout) :: idx
        end subroutine calc_time_coefficient_bc_data_table

    end interface

    !> Provider for data updated externally (e.g., from an atmosphere model).
    type, extends(abst_bc_data) :: type_bc_data_dynamic
        !> Buffer for the most recently pushed values.
        real(real64), allocatable, private :: current_buffer(:)
    contains
        procedure, public, pass(self) :: initialize => initialize_type_bc_data_dynamic
        procedure, public, pass(self) :: destroy => destroy_type_bc_data_dynamic
        procedure, public, pass(self) :: get_data => get_data_bc_data_dynamic
        ! ---- Mutator ----
        !> Updates the internal buffer with new data.
        procedure, public, pass(self) :: update_buffer => update_buffer_bc_data_dynamic
    end type type_bc_data_dynamic

    interface
        module subroutine initialize_type_bc_data_dynamic(self, config_bc)
            implicit none
            class(type_bc_data_dynamic), intent(inout) :: self
            type(type_config_bc), intent(in) :: config_bc

        end subroutine initialize_type_bc_data_dynamic

        module subroutine get_data_bc_data_dynamic(self, current_time, output_value)
            implicit none
            class(type_bc_data_dynamic), intent(inout) :: self
            real(real64), intent(in) :: current_time
            class(abst_bc_dto), intent(inout) :: output_value

        end subroutine get_data_bc_data_dynamic

        module subroutine update_buffer_bc_data_dynamic(self, new_values)
            implicit none
            class(type_bc_data_dynamic), intent(inout) :: self
            real(real64), intent(in) :: new_values(:)

        end subroutine update_buffer_bc_data_dynamic

        module subroutine destroy_type_bc_data_dynamic(self)
            implicit none
            class(type_bc_data_dynamic), intent(inout) :: self

        end subroutine destroy_type_bc_data_dynamic
    end interface

end module condition_boundary_data_provider

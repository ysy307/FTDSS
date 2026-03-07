module condition_boundary_data_provider
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core
    implicit none
    private

    public :: abst_bc_data
    public :: type_bc_data_constant
    public :: type_bc_data_dynamic
    public :: type_bc_data_table

    type, abstract :: abst_bc_data
        type(type_constant_id) :: data_kind = type_constant_id("", "", -1)
    contains
        procedure(abst_initialize_bc_data), public, pass(self), deferred :: initialize
        procedure(abst_destroy_bc_data), public, pass(self), deferred :: destroy
        procedure(abst_get_data), public, pass(self), deferred :: get_data
        procedure, public, pass(self) :: get_data_kind => get_data_kind_abst_bc_data
    end type abst_bc_data

    abstract interface
        subroutine abst_initialize_bc_data(self, config)
            import :: abst_bc_data, type_config_bc
            implicit none
            class(abst_bc_data), intent(inout) :: self
            type(type_config_bc), intent(in) :: config
        end subroutine abst_initialize_bc_data

        subroutine abst_destroy_bc_data(self)
            import :: abst_bc_data
            implicit none
            class(abst_bc_data), intent(inout) :: self
        end subroutine abst_destroy_bc_data

        subroutine abst_get_data(self, current_time, values)
            import :: abst_bc_data, real64
            implicit none
            class(abst_bc_data), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), allocatable, intent(inout) :: values(:)
        end subroutine abst_get_data
    end interface

    type, extends(abst_bc_data) :: type_bc_data_constant
        real(real64), allocatable, private :: constant_values(:)
    contains
        procedure, public, pass(self) :: initialize => initialize_type_bc_data_constant
        procedure, public, pass(self) :: destroy => destroy_type_bc_data_constant
        procedure, public, pass(self) :: get_data => get_data_bc_data_constant
    end type type_bc_data_constant

    type, extends(abst_bc_data) :: type_bc_data_dynamic
        real(real64), allocatable, private :: current_buffer(:)
    contains
        procedure, public, pass(self) :: initialize => initialize_type_bc_data_dynamic
        procedure, public, pass(self) :: destroy => destroy_type_bc_data_dynamic
        procedure, public, pass(self) :: get_data => get_data_bc_data_dynamic
        procedure, public, pass(self) :: update_buffer => update_buffer_bc_data_dynamic
    end type type_bc_data_dynamic

    type, extends(abst_bc_data) :: type_bc_data_table
        real(real64), allocatable, private :: time_points(:)
        real(real64), allocatable, private :: table_values(:, :)
    contains
        procedure, public, pass(self) :: initialize => initialize_type_bc_data_table
        procedure, public, pass(self) :: destroy => destroy_type_bc_data_table
        procedure, public, pass(self) :: get_data => get_data_bc_data_table
        procedure, private, pass(self) :: calc_time_coefficient => calc_time_coefficient_bc_data_table
    end type type_bc_data_table

    interface
        module subroutine initialize_type_bc_data_constant(self, config)
            implicit none
            class(type_bc_data_constant), intent(inout) :: self
            type(type_config_bc), intent(in) :: config
        end subroutine initialize_type_bc_data_constant

        module subroutine destroy_type_bc_data_constant(self)
            implicit none
            class(type_bc_data_constant), intent(inout) :: self
        end subroutine destroy_type_bc_data_constant

        module subroutine get_data_bc_data_constant(self, current_time, values)
            implicit none
            class(type_bc_data_constant), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(inout) , allocatable:: values(:)
        end subroutine get_data_bc_data_constant

        module subroutine initialize_type_bc_data_dynamic(self, config)
            implicit none
            class(type_bc_data_dynamic), intent(inout) :: self
            type(type_config_bc), intent(in) :: config
        end subroutine initialize_type_bc_data_dynamic

        module subroutine destroy_type_bc_data_dynamic(self)
            implicit none
            class(type_bc_data_dynamic), intent(inout) :: self
        end subroutine destroy_type_bc_data_dynamic

        module subroutine get_data_bc_data_dynamic(self, current_time, values)
            implicit none
            class(type_bc_data_dynamic), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), allocatable, intent(inout) :: values(:)
        end subroutine get_data_bc_data_dynamic

        module subroutine update_buffer_bc_data_dynamic(self, new_values)
            implicit none
            class(type_bc_data_dynamic), intent(inout) :: self
            real(real64), intent(in) :: new_values(:)
        end subroutine update_buffer_bc_data_dynamic

        module subroutine initialize_type_bc_data_table(self, config)
            implicit none
            class(type_bc_data_table), intent(inout) :: self
            type(type_config_bc), intent(in) :: config
        end subroutine initialize_type_bc_data_table

        module subroutine destroy_type_bc_data_table(self)
            implicit none
            class(type_bc_data_table), intent(inout) :: self
        end subroutine destroy_type_bc_data_table

        module subroutine get_data_bc_data_table(self, current_time, values)
            implicit none
            class(type_bc_data_table), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), allocatable, intent(inout) :: values(:)
        end subroutine get_data_bc_data_table

        module subroutine calc_time_coefficient_bc_data_table(self, current_time, coef, idx)
            implicit none
            class(type_bc_data_table), intent(in) :: self
            real(real64), intent(in) :: current_time
            real(real64), intent(inout) :: coef
            integer(int32), intent(inout) :: idx
        end subroutine calc_time_coefficient_bc_data_table
    end interface

contains
    subroutine get_data_kind_abst_bc_data(self, data_kind)
        implicit none
        class(abst_bc_data), intent(in), target :: self
        type(type_constant_id), intent(inout), pointer :: data_kind

        data_kind => self%data_kind
    end subroutine get_data_kind_abst_bc_data
end module condition_boundary_data_provider

module core_types_vector
    use, intrinsic :: iso_fortran_env
!$  use :: omp_lib
    use :: core_constants
    use :: core_check_length, only:check_match_length
    use :: core_check_range, only:value_in_range
    use :: core_allocate, only:allocate_array
    use :: core_deallocate, only:deallocate_array
    implicit none

    public :: type_vector_dp
    public :: type_vector_int

    ! ==========================================================
    ! Double Precision Vector
    ! ==========================================================
    type :: type_vector_dp
        private
        real(real64), allocatable :: val(:)
        integer(int32) :: num_nodes = 0
        integer(int32) :: num_blocks = 0
        logical :: is_initialized_vector = .false.
        integer(int32) :: status = VECTOR_STATUS_SUCCESS
    contains
        procedure, public, pass(self) :: initialize => initialize_vector_dp
        procedure, public, pass(self) :: destroy => destroy_vector_dp
        procedure, public, pass(self) :: is_initialized => is_initialized_vector_dp
        procedure, public, pass(self) :: get_size => get_size_vector_dp
        procedure, public, pass(self) :: get_data => get_data_vector_dp
        procedure, public, pass(self) :: get_status => get_status_vector_dp

        procedure, private, pass(self) :: set_scalar => set_scalar_vector_dp
        procedure, private, pass(self) :: set_array => set_array_vector_dp
        procedure, private, pass(self) :: set_value_at_index => set_value_at_index_vector_dp
        procedure, private, pass(self) :: set_values_at_indices => set_values_at_indices_vector_dp
        generic, public :: set => set_scalar, set_array, set_value_at_index, set_values_at_indices

        procedure, public, pass(self) :: scale => scale_vector_dp
        procedure, public, pass(self) :: copy => copy_vector_dp
        procedure, public, pass(self) :: zero => zero_vector_dp
        procedure, public, pass(self) :: display => display_vector_dp
        procedure, public, pass(self) :: check => check_vector_dp
    end type type_vector_dp

    ! ==========================================================
    ! Integer Vector
    ! ==========================================================
    type :: type_vector_int
        private
        integer(int32), allocatable :: val(:)
        integer(int32) :: num_nodes = 0
        integer(int32) :: num_blocks = 0
        logical :: is_initialized_vector = .false.
        integer(int32) :: status = VECTOR_STATUS_SUCCESS
    contains
        procedure, public, pass(self) :: initialize => initialize_vector_int
        procedure, public, pass(self) :: destroy => destroy_vector_int
        procedure, public, pass(self) :: is_initialized => is_initialized_vector_int
        procedure, public, pass(self) :: get_size => get_size_vector_int
        procedure, public, pass(self) :: get_data => get_data_vector_int
        procedure, public, pass(self) :: get_status => get_status_vector_int

        procedure, private, pass(self) :: set_array => set_array_vector_int
        procedure, private, pass(self) :: set_value_at_index => set_value_at_index_vector_int
        procedure, private, pass(self) :: set_values_at_indices => set_values_at_indices_vector_int
        generic, public :: set => set_array, set_value_at_index, set_values_at_indices

        procedure, public, pass(self) :: scale => scale_vector_int
        procedure, public, pass(self) :: copy => copy_vector_int
        procedure, public, pass(self) :: zero => zero_vector_int
        procedure, public, pass(self) :: display => display_vector_int
        procedure, public, pass(self) :: check => check_vector_int
    end type type_vector_int

    ! ==========================================================
    ! Interfaces
    ! ==========================================================
    interface
        ! --- Double Precision Interfaces ---
        module subroutine initialize_vector_dp(self, num_nodes, num_blocks)
            implicit none
            class(type_vector_dp), intent(inout) :: self
            integer(int32), intent(in) :: num_nodes
            integer(int32), intent(in), optional :: num_blocks
        end subroutine initialize_vector_dp

        module subroutine destroy_vector_dp(self)
            implicit none
            class(type_vector_dp), intent(inout) :: self
        end subroutine destroy_vector_dp

        module pure function is_initialized_vector_dp(self) result(initialized)
            implicit none
            class(type_vector_dp), intent(in) :: self
            logical :: initialized
        end function is_initialized_vector_dp

        module pure function get_size_vector_dp(self) result(vector_size)
            implicit none
            class(type_vector_dp), intent(in) :: self
            integer(int32) :: vector_size
        end function get_size_vector_dp

        module function get_data_vector_dp(self) result(data)
            implicit none
            class(type_vector_dp), intent(in), target :: self
            real(real64), pointer, dimension(:) :: data
        end function get_data_vector_dp

        module pure function get_status_vector_dp(self) result(status)
            implicit none
            class(type_vector_dp), intent(in) :: self
            integer(int32) :: status
        end function get_status_vector_dp

        module subroutine set_scalar_vector_dp(self, op, scalar_value, row_block)
            implicit none
            class(type_vector_dp), intent(inout) :: self
            integer(int32), intent(in) :: op
            real(real64), intent(in) :: scalar_value
            integer(int32), intent(in), optional :: row_block
        end subroutine set_scalar_vector_dp

        module subroutine set_array_vector_dp(self, op, array_value, row_block)
            implicit none
            class(type_vector_dp), intent(inout) :: self
            integer(int32), intent(in) :: op
            real(real64), intent(in) :: array_value(:)
            integer(int32), intent(in), optional :: row_block
        end subroutine set_array_vector_dp

        module subroutine set_value_at_index_vector_dp(self, op, global_index, value, row_block)
            implicit none
            class(type_vector_dp), intent(inout) :: self
            integer(int32), intent(in) :: op
            integer(int32), intent(in) :: global_index
            real(real64), intent(in) :: value
            integer(int32), intent(in), optional :: row_block
        end subroutine set_value_at_index_vector_dp

        module subroutine set_values_at_indices_vector_dp(self, op, global_indices, new_values, row_block)
            implicit none
            class(type_vector_dp), intent(inout) :: self
            integer(int32), intent(in) :: op
            integer(int32), intent(in) :: global_indices(:)
            real(real64), intent(in) :: new_values(:)
            integer(int32), intent(in), optional :: row_block
        end subroutine set_values_at_indices_vector_dp

        module subroutine scale_vector_dp(self, op, alpha)
            implicit none
            class(type_vector_dp), intent(inout) :: self
            integer(int32), intent(in) :: op
            class(type_vector_dp), intent(in) :: alpha
        end subroutine scale_vector_dp

        module subroutine copy_vector_dp(self, source_vector)
            implicit none
            class(type_vector_dp), intent(inout) :: self
            class(type_vector_dp), intent(in) :: source_vector
        end subroutine copy_vector_dp

        module subroutine zero_vector_dp(self)
            implicit none
            class(type_vector_dp), intent(inout) :: self
        end subroutine zero_vector_dp

        module subroutine display_vector_dp(self)
            implicit none
            class(type_vector_dp), intent(in) :: self
        end subroutine display_vector_dp

        module subroutine check_vector_dp(self)
            implicit none
            class(type_vector_dp), intent(in) :: self
        end subroutine check_vector_dp

        ! --- Integer Interfaces ---
        module subroutine initialize_vector_int(self, num_nodes, num_blocks)
            implicit none
            class(type_vector_int), intent(inout) :: self
            integer(int32), intent(in) :: num_nodes
            integer(int32), intent(in), optional :: num_blocks
        end subroutine initialize_vector_int

        module subroutine destroy_vector_int(self)
            implicit none
            class(type_vector_int), intent(inout) :: self
        end subroutine destroy_vector_int

        module pure function is_initialized_vector_int(self) result(initialized)
            implicit none
            class(type_vector_int), intent(in) :: self
            logical :: initialized
        end function is_initialized_vector_int

        module pure function get_size_vector_int(self) result(vector_size)
            implicit none
            class(type_vector_int), intent(in) :: self
            integer(int32) :: vector_size
        end function get_size_vector_int

        module function get_data_vector_int(self) result(data)
            implicit none
            class(type_vector_int), intent(in), target :: self
            integer(int32), pointer, dimension(:) :: data
        end function get_data_vector_int

        module pure function get_status_vector_int(self) result(status)
            implicit none
            class(type_vector_int), intent(in) :: self
            integer(int32) :: status
        end function get_status_vector_int

        module subroutine set_array_vector_int(self, op, array_value, row_block)
            implicit none
            class(type_vector_int), intent(inout) :: self
            integer(int32), intent(in) :: op
            integer(int32), intent(in) :: array_value(:)
            integer(int32), intent(in), optional :: row_block
        end subroutine set_array_vector_int

        module subroutine set_value_at_index_vector_int(self, op, global_index, value, row_block)
            implicit none
            class(type_vector_int), intent(inout) :: self
            integer(int32), intent(in) :: op
            integer(int32), intent(in) :: global_index
            integer(int32), intent(in) :: value
            integer(int32), intent(in), optional :: row_block
        end subroutine set_value_at_index_vector_int

        module subroutine set_values_at_indices_vector_int(self, op, global_indices, new_values, row_block)
            implicit none
            class(type_vector_int), intent(inout) :: self
            integer(int32), intent(in) :: op
            integer(int32), intent(in) :: global_indices(:)
            integer(int32), intent(in) :: new_values(:)
            integer(int32), intent(in), optional :: row_block
        end subroutine set_values_at_indices_vector_int

        module subroutine scale_vector_int(self, op, alpha)
            implicit none
            class(type_vector_int), intent(inout) :: self
            integer(int32), intent(in) :: op
            class(type_vector_int), intent(in) :: alpha
        end subroutine scale_vector_int

        module subroutine copy_vector_int(self, source_vector)
            implicit none
            class(type_vector_int), intent(inout) :: self
            class(type_vector_int), intent(in) :: source_vector
        end subroutine copy_vector_int

        module subroutine zero_vector_int(self)
            implicit none
            class(type_vector_int), intent(inout) :: self
        end subroutine zero_vector_int

        module subroutine display_vector_int(self)
            implicit none
            class(type_vector_int), intent(in) :: self
        end subroutine display_vector_int

        module subroutine check_vector_int(self)
            implicit none
            class(type_vector_int), intent(in) :: self
        end subroutine check_vector_int

    end interface

end module core_types_vector

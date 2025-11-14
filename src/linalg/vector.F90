!>
!> Defines classes for handling mathematical vectors (1D arrays) used in
!> linear algebra operations.
!> This module is intended for abstract mathematical vectors such as residual or
!> solution vectors in a solver. For physical 3D coordinates, use the
!> `core_types_coordinate` module instead.
!>
module linalg_vector
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core
    implicit none
    private

    public :: type_vector_dp
    public :: type_vector_int

    ! ==========================================================
    ! Double Precision Vector
    ! ==========================================================
    !>
    !> Encapsulates a 1D double precision mathematical vector.
    !>
    type :: type_vector_dp
        private
        !> The internal allocatable array holding the vector data.
        real(real64), allocatable :: val(:)
        !> The number of nodes (size) of the vector.
        integer(int32) :: num_nodes = 0
        !> A flag to track the allocation status.
        logical :: is_allocated = .false.
    contains
        procedure, public, pass(self) :: initialize => initialize_vector_dp
        procedure, public, pass(self) :: destroy => destroy_vector_dp
        procedure, public, pass(self) :: is_initialized => is_initialized_vector_dp
        procedure, public, pass(self) :: get_size => get_size_vector_dp
        procedure, public, pass(self) :: get_data => get_data_vector_dp

        procedure, private, pass(self) :: set_scalar => set_scalar_vector_dp
        procedure, private, pass(self) :: set_array => set_array_vector_dp
        procedure, private, pass(self) :: set_value_at_index => set_value_at_index_vector_dp
        procedure, private, pass(self) :: set_values_at_indices => set_values_at_indices_vector_dp
        !> Generic interface for setting vector values.
        generic, public :: set => set_scalar, set_array, set_value_at_index, set_values_at_indices

        procedure, private, pass(self) :: add_scalar => add_scalar_vector_dp
        procedure, private, pass(self) :: add_array => add_array_vector_dp
        procedure, private, pass(self) :: add_value_at_index => add_value_at_index_vector_dp
        procedure, private, pass(self) :: add_values_at_indices => add_values_at_indices_vector_dp
        !> Generic interface for adding values to the vector.
        generic, public :: add => add_scalar, add_array, add_value_at_index, add_values_at_indices
        procedure, public, pass(self) :: scale => scale_vector_dp

        procedure, public, pass(self) :: zero => zero_vector_dp
        procedure, public, pass(self) :: display => display_vector_dp
    end type type_vector_dp

    ! ==========================================================
    ! Integer Vector
    ! ==========================================================
    !>
    !> Encapsulates a 1D integer mathematical vector.
    !>
    type :: type_vector_int
        private
        !> The internal allocatable array holding the vector data.
        integer(int32), allocatable :: val(:)
        !> The number of nodes (size) of the vector.
        integer(int32) :: num_nodes = 0
        !> A flag to track the allocation status.
        logical :: is_allocated = .false.
    contains
        procedure, public, pass(self) :: initialize => initialize_vector_int
        procedure, public, pass(self) :: destroy => destroy_vector_int
        procedure, public, pass(self) :: is_initialized => is_initialized_vector_int
        procedure, public, pass(self) :: get_size => get_size_vector_int
        procedure, public, pass(self) :: get_data => get_data_vector_int

        procedure, private, pass(self) :: set_scalar => set_scalar_vector_int
        procedure, private, pass(self) :: set_array => set_array_vector_int
        procedure, private, pass(self) :: set_value_at_index => set_value_at_index_vector_int
        procedure, private, pass(self) :: set_values_at_indices => set_values_at_indices_vector_int
        !> Generic interface for setting vector values.
        generic, public :: set => set_scalar, set_array, set_value_at_index, set_values_at_indices

        procedure, private, pass(self) :: add_scalar => add_scalar_vector_int
        procedure, private, pass(self) :: add_array => add_array_vector_int
        procedure, private, pass(self) :: add_value_at_index => add_value_at_index_vector_int
        procedure, private, pass(self) :: add_values_at_indices => add_values_at_indices_vector_int
        !> Generic interface for adding values to the vector.
        generic, public :: add => add_scalar, add_array, add_value_at_index, add_values_at_indices
        procedure, public, pass(self) :: scale => scale_vector_int

        procedure, public, pass(self) :: zero => zero_vector_int
        procedure, public, pass(self) :: display => display_vector_int
    end type type_vector_int

contains

    ! ==========================================================
    ! Double Precision Vector Procedures
    ! ==========================================================

    !>
    !> Initializes the vector by allocating it with a specified size.
    !> The indices will range from 1 to num_nodes.
    !>
    subroutine initialize_vector_dp(self, num_nodes)
        implicit none
        !> The vector object to initialize.
        class(type_vector_dp), intent(inout) :: self
        !> The number of nodes (size) for the vector.
        integer(int32), intent(in) :: num_nodes

        call allocate_array(self%val, num_nodes)
        self%val(:) = 0.0d0
        self%num_nodes = num_nodes
        self%is_allocated = .true.
    end subroutine initialize_vector_dp

    !>
    !> Deallocates the vector's internal data array.
    !>
    subroutine destroy_vector_dp(self)
        implicit none
        !> The vector object to destroy.
        class(type_vector_dp), intent(inout) :: self

        call deallocate_array(self%val)
        self%num_nodes = 0
        self%is_allocated = .false.
    end subroutine destroy_vector_dp

    !>
    !> Checks if the vector has been allocated.
    !>
    pure function is_initialized_vector_dp(self) result(initialized)
        implicit none
        !> The vector object to check.
        class(type_vector_dp), intent(in) :: self
        !> Returns .true. if the vector is allocated, .false. otherwise.
        logical :: initialized

        initialized = self%is_allocated
    end function is_initialized_vector_dp

    !>
    !> Returns the size (number of elements) of the vector.
    !>
    pure function get_size_vector_dp(self) result(vector_size)
        implicit none
        !> The vector object.
        class(type_vector_dp), intent(in) :: self
        !> The size of the vector.
        integer(int32) :: vector_size

        vector_size = self%num_nodes
    end function get_size_vector_dp

    !>
    !> Returns a pointer to the internal data array of the vector.
    !>
    function get_data_vector_dp(self) result(data_pointer)
        implicit none
        !> The vector object.
        class(type_vector_dp), intent(in), target :: self
        !> A pointer to the internal data array.
        real(real64), pointer :: data_pointer(:)

        data_pointer => self%val
    end function get_data_vector_dp

    !>
    !> Sets all elements of the vector to a single scalar value.
    !>
    subroutine set_scalar_vector_dp(self, scalar_value)
        implicit none
        !> The vector object to modify.
        class(type_vector_dp), intent(inout) :: self
        !> The scalar value to assign to all elements.
        real(real64), intent(in) :: scalar_value

        self%val(:) = scalar_value
    end subroutine set_scalar_vector_dp

    !>
    !> Sets the vector's elements from a source array of the same size.
    !>
    subroutine set_array_vector_dp(self, array_value)
        implicit none
        !> The vector object to modify.
        class(type_vector_dp), intent(inout) :: self
        !> The source array containing the new values.
        real(real64), intent(in) :: array_value(:)

        if (self%num_nodes /= size(array_value)) stop "Error: size mismatch in set_array_vector_dp"
        self%val(:) = array_value
    end subroutine set_array_vector_dp

    !>
    !> Sets the value of a single element at a specified index.
    !>
    subroutine set_value_at_index_vector_dp(self, global_index, value)
        implicit none
        !> The vector object to modify.
        class(type_vector_dp), intent(inout) :: self
        !> The index of the element to set.
        integer(int32), intent(in) :: global_index
        !> The new value for the element.
        real(real64), intent(in) :: value

        if (global_index >= 1 .and. global_index <= self%num_nodes) then
            self%val(global_index) = value
        end if
    end subroutine set_value_at_index_vector_dp

    !>
    !> Sets the values of multiple elements at specified indices (scatter operation).
    !>
    subroutine set_values_at_indices_vector_dp(self, global_indices, new_values)
        implicit none
        !> The vector object to modify.
        class(type_vector_dp), intent(inout) :: self
        !> An array of indices to set.
        integer(int32), intent(in) :: global_indices(:)
        !> An array of new values corresponding to the indices.
        real(real64), intent(in) :: new_values(:)
        integer(int32) :: i

        do i = 1, size(global_indices)
            if (global_indices(i) >= 1 .and. global_indices(i) <= self%num_nodes) then
                self%val(global_indices(i)) = new_values(i)
            end if
        end do
    end subroutine set_values_at_indices_vector_dp

    !>
    !> Adds a scalar value to all elements of the vector.
    !>
    subroutine add_scalar_vector_dp(self, scalar_value)
        implicit none
        !> The vector object to modify.
        class(type_vector_dp), intent(inout) :: self
        !> The scalar value to add to all elements.
        real(real64), intent(in) :: scalar_value

        self%val(:) = self%val(:) + scalar_value
    end subroutine add_scalar_vector_dp

    !>
    !> Adds the elements of a source array to this vector's elements.
    !>
    subroutine add_array_vector_dp(self, array_value)
        implicit none
        !> The vector object to modify.
        class(type_vector_dp), intent(inout) :: self
        !> The source array of values to add.
        real(real64), intent(in) :: array_value(:)

        if (self%num_nodes /= size(array_value)) stop "Error: size mismatch in add_array_vector_dp"
        self%val(:) = self%val(:) + array_value
    end subroutine add_array_vector_dp

    !>
    !> Adds a value to a single element at a specified index.
    !>
    subroutine add_value_at_index_vector_dp(self, global_index, value)
        implicit none
        !> The vector object to modify.
        class(type_vector_dp), intent(inout) :: self
        !> The index of the element to modify.
        integer(int32), intent(in) :: global_index
        !> The value to add to the element.
        real(real64), intent(in) :: value

        if (global_index >= 1 .and. global_index <= self%num_nodes) then
            self%val(global_index) = self%val(global_index) + value
        end if
    end subroutine add_value_at_index_vector_dp

    !>
    !> Adds values to multiple elements at specified indices (scatter-add operation).
    !>
    subroutine add_values_at_indices_vector_dp(self, global_indices, new_values)
        implicit none
        !> The vector object to modify.
        class(type_vector_dp), intent(inout) :: self
        !> An array of indices to modify.
        integer(int32), intent(in) :: global_indices(:)
        !> An array of values to add, corresponding to the indices.
        real(real64), intent(in) :: new_values(:)
        integer(int32) :: i

        do i = 1, size(global_indices)
            if (global_indices(i) >= 1 .and. global_indices(i) <= self%num_nodes) then
                self%val(global_indices(i)) = self%val(global_indices(i)) + new_values(i)
            end if
        end do
    end subroutine add_values_at_indices_vector_dp

    !>
    !> Scales all elements of the vector by a given scalar factor.
    !>
    subroutine scale_vector_dp(self, factor)
        implicit none
        !> The vector object to modify.
        class(type_vector_dp), intent(inout) :: self
        !> The scalar factor to scale by.
        real(real64), intent(in) :: factor

        self%val(:) = self%val(:) * factor
    end subroutine scale_vector_dp

    !>
    !> Sets all elements of the vector to zero.
    !>
    subroutine zero_vector_dp(self)
        implicit none
        !> The vector object to zero out.
        class(type_vector_dp), intent(inout) :: self

        self%val(:) = 0.0d0
    end subroutine zero_vector_dp

    !>
    !> Displays the contents of the vector to standard output.
    !>
    subroutine display_vector_dp(self)
        implicit none
        !> The vector object to display.
        class(type_vector_dp), intent(in) :: self
        integer(int32) :: i

        do i = 1, self%num_nodes
            write (*, '(A,I0,A,F12.6)') "Index ", i, ": ", self%val(i)
        end do
    end subroutine display_vector_dp

    ! ==========================================================
    ! Integer Vector Procedures
    ! ==========================================================

    !>
    !> Initializes the vector by allocating it with a specified size.
    !> The indices will range from 1 to num_nodes.
    !>
    subroutine initialize_vector_int(self, num_nodes)
        implicit none
        !> The vector object to initialize.
        class(type_vector_int), intent(inout) :: self
        !> The number of nodes (size) for the vector.
        integer(int32), intent(in) :: num_nodes

        call allocate_array(self%val, num_nodes)
        self%val(:) = 0
        self%num_nodes = num_nodes
        self%is_allocated = .true.
    end subroutine initialize_vector_int

    !>
    !> Deallocates the vector's internal data array.
    !>
    subroutine destroy_vector_int(self)
        implicit none
        !> The vector object to destroy.
        class(type_vector_int), intent(inout) :: self

        call deallocate_array(self%val)
        self%num_nodes = 0
        self%is_allocated = .false.
    end subroutine destroy_vector_int

    !>
    !> Checks if the vector has been allocated.
    !>
    pure function is_initialized_vector_int(self) result(initialized)
        implicit none
        !> The vector object to check.
        class(type_vector_int), intent(in) :: self
        !> Returns .true. if the vector is allocated, .false. otherwise.
        logical :: initialized

        initialized = self%is_allocated
    end function is_initialized_vector_int

    !>
    !> Returns the size (number of elements) of the vector.
    !>
    pure function get_size_vector_int(self) result(vector_size)
        implicit none
        !> The vector object.
        class(type_vector_int), intent(in) :: self
        !> The size of the vector.
        integer(int32) :: vector_size

        vector_size = self%num_nodes
    end function get_size_vector_int

    !>
    !> Returns a pointer to the internal data array of the vector.
    !>
    function get_data_vector_int(self) result(data_pointer)
        implicit none
        !> The vector object.
        class(type_vector_int), intent(in), target :: self
        !> A pointer to the internal data array.
        integer(int32), pointer :: data_pointer(:)

        data_pointer => self%val
    end function get_data_vector_int

    !>
    !> Sets all elements of the vector to a single scalar value.
    !>
    subroutine set_scalar_vector_int(self, scalar_value)
        implicit none
        !> The vector object to modify.
        class(type_vector_int), intent(inout) :: self
        !> The scalar value to assign to all elements.
        integer(int32), intent(in) :: scalar_value

        self%val(:) = scalar_value
    end subroutine set_scalar_vector_int

    !>
    !> Sets the vector's elements from a source array of the same size.
    !>
    subroutine set_array_vector_int(self, array_value)
        implicit none
        !> The vector object to modify.
        class(type_vector_int), intent(inout) :: self
        !> The source array containing the new values.
        integer(int32), intent(in) :: array_value(:)

        if (self%num_nodes /= size(array_value)) stop "Error: size mismatch in set_array_vector_int"
        self%val(:) = array_value
    end subroutine set_array_vector_int

    !>
    !> Sets the value of a single element at a specified index.
    !>
    subroutine set_value_at_index_vector_int(self, global_index, value)
        implicit none
        !> The vector object to modify.
        class(type_vector_int), intent(inout) :: self
        !> The index of the element to set.
        integer(int32), intent(in) :: global_index
        !> The new value for the element.
        integer(int32), intent(in) :: value

        if (global_index >= 1 .and. global_index <= self%num_nodes) then
            self%val(global_index) = value
        end if
    end subroutine set_value_at_index_vector_int

    !>
    !> Sets the values of multiple elements at specified indices (scatter operation).
    !>
    subroutine set_values_at_indices_vector_int(self, global_indices, new_values)
        implicit none
        !> The vector object to modify.
        class(type_vector_int), intent(inout) :: self
        !> An array of indices to set.
        integer(int32), intent(in) :: global_indices(:)
        !> An array of new values corresponding to the indices.
        integer(int32), intent(in) :: new_values(:)
        integer(int32) :: i

        do i = 1, size(global_indices)
            if (global_indices(i) >= 1 .and. global_indices(i) <= self%num_nodes) then
                self%val(global_indices(i)) = new_values(i)
            end if
        end do
    end subroutine set_values_at_indices_vector_int

    !>
    !> Adds a scalar value to all elements of the vector.
    !>
    subroutine add_scalar_vector_int(self, scalar_value)
        implicit none
        !> The vector object to modify.
        class(type_vector_int), intent(inout) :: self
        !> The scalar value to add to all elements.
        integer(int32), intent(in) :: scalar_value

        self%val(:) = self%val(:) + scalar_value
    end subroutine add_scalar_vector_int

    !>
    !> Adds the elements of a source array to this vector's elements.
    !>
    subroutine add_array_vector_int(self, array_value)
        implicit none
        !> The vector object to modify.
        class(type_vector_int), intent(inout) :: self
        !> The source array of values to add.
        integer(int32), intent(in) :: array_value(:)

        if (self%num_nodes /= size(array_value)) stop "Error: size mismatch in add_array_vector_int"
        self%val(:) = self%val(:) + array_value
    end subroutine add_array_vector_int

    !>
    !> Adds a value to a single element at a specified index.
    !>
    subroutine add_value_at_index_vector_int(self, global_index, value)
        implicit none
        !> The vector object to modify.
        class(type_vector_int), intent(inout) :: self
        !> The index of the element to modify.
        integer(int32), intent(in) :: global_index
        !> The value to add to the element.
        integer(int32), intent(in) :: value

        if (global_index >= 1 .and. global_index <= self%num_nodes) then
            self%val(global_index) = self%val(global_index) + value
        end if
    end subroutine add_value_at_index_vector_int

    !>
    !> Adds values to multiple elements at specified indices (scatter-add operation).
    !>
    subroutine add_values_at_indices_vector_int(self, global_indices, new_values)
        implicit none
        !> The vector object to modify.
        class(type_vector_int), intent(inout) :: self
        !> An array of indices to modify.
        integer(int32), intent(in) :: global_indices(:)
        !> An array of values to add, corresponding to the indices.
        integer(int32), intent(in) :: new_values(:)
        integer(int32) :: i

        do i = 1, size(global_indices)
            if (global_indices(i) >= 1 .and. global_indices(i) <= self%num_nodes) then
                self%val(global_indices(i)) = self%val(global_indices(i)) + new_values(i)
            end if
        end do
    end subroutine add_values_at_indices_vector_int

    !>
    !> Scales all elements of the vector by a given scalar factor.
    !>
    subroutine scale_vector_int(self, factor)
        implicit none
        !> The vector object to modify.
        class(type_vector_int), intent(inout) :: self
        !> The scalar factor to scale by.
        integer(int32), intent(in) :: factor

        self%val(:) = self%val(:) * factor
    end subroutine scale_vector_int

    !>
    !> Sets all elements of the vector to zero.
    !>
    subroutine zero_vector_int(self)
        implicit none
        !> The vector object to zero out.
        class(type_vector_int), intent(inout) :: self

        self%val(:) = 0
    end subroutine zero_vector_int

    !>
    !> Displays the contents of the vector to standard output.
    !>
    subroutine display_vector_int(self)
        implicit none
        !> The vector object to display.
        class(type_vector_int), intent(in) :: self
        integer(int32) :: i

        do i = 1, self%num_nodes
            write (*, '(A,I0,A,I0)') "Index ", i, ": ", self%val(i)
        end do
    end subroutine display_vector_int

end module linalg_vector

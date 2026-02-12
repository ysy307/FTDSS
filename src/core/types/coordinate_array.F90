!>
!> Defines derived types for handling arrays of 3D coordinates, supporting
!> both double precision real and integer components.
!>
module core_types_coordinate_array
    use, intrinsic :: iso_fortran_env, only: real64, int32
    use :: core_memory
    use :: core_types_coordinate, only:type_coordinate_dp, type_coordinate_int
    implicit none
    private

    public :: type_coordinate_array_dp
    public :: type_coordinate_array_int

    !>
    !> Represents an array of 3D coordinates using double precision values.
    !>
    type :: type_coordinate_array_dp
        !> The number of coordinate points in the array.
        integer(int32), private :: length = 0
        !> Array of x-coordinates.
        real(real64), allocatable :: x(:)
        !> Array of y-coordinates.
        real(real64), allocatable :: y(:)
        !> Array of z-coordinates.
        real(real64), allocatable :: z(:)
        !> Flag indicating whether the coordinate array is allocated.
        logical, private :: is_allocated = .false.
    contains
        procedure, pass(self) :: initialize => initialize_type_coordinate_array_dp
        procedure, pass(self) :: destroy => destroy_type_coordinate_array_dp
        procedure, pass(self) :: is_initialized => is_initialized_type_coordinate_array_dp
        procedure, pass(self) :: get_length => get_length_type_coordinate_array_dp
        procedure, pass(self) :: zero => zero_type_coordinate_array_dp
        procedure, pass(self) :: get_coordinate => get_coordinate_type_coordinate_array_dp
    end type

    !>
    !> Represents an array of 3D coordinates using integer values.
    !>
    type :: type_coordinate_array_int
        !> The number of coordinate points in the array.
        integer(int32), private :: length = 0
        !> Array of x-coordinates.
        integer(int32), allocatable :: x(:)
        !> Array of y-coordinates.
        integer(int32), allocatable :: y(:)
        !> Array of z-coordinates.
        integer(int32), allocatable :: z(:)
        !> Flag indicating whether the coordinate array is allocated.
        logical, private :: is_allocated = .false.
    contains
        procedure, pass(self) :: initialize => initialize_type_coordinate_array_int
        procedure, pass(self) :: destroy => destroy_type_coordinate_array_int
        procedure, pass(self) :: is_initialized => is_initialized_type_coordinate_array_int
        procedure, pass(self) :: get_length => get_length_type_coordinate_array_int
        procedure, pass(self) :: zero => zero_type_coordinate_array_int
        procedure, pass(self) :: get_coordinate => get_coordinate_type_coordinate_array_int
    end type

contains

    ! ==========================================================
    ! Double Precision Coordinate Array Procedures
    ! ==========================================================

    !>
    !> Allocates and initializes the coordinate component arrays.
    !>
    subroutine initialize_type_coordinate_array_dp(self, length, initialize_value)
        implicit none
        !> The coordinate array object to initialize.
        class(type_coordinate_array_dp), intent(inout) :: self
        !> The number of coordinate points to allocate.
        integer(int32), intent(in) :: length
        !> An optional value to assign to all components. Defaults to zero.
        real(real64), intent(in), optional :: initialize_value

        call allocate_array(self%x, length)
        call allocate_array(self%y, length)
        call allocate_array(self%z, length)

        if (present(initialize_value)) then
            self%x(:) = initialize_value
            self%y(:) = initialize_value
            self%z(:) = initialize_value
        else
            self%x(:) = 0.0d0
            self%y(:) = 0.0d0
            self%z(:) = 0.0d0
        end if

        self%length = length
        self%is_allocated = .true.

    end subroutine initialize_type_coordinate_array_dp

    !>
    !> Deallocates the coordinate component arrays.
    !>
    subroutine destroy_type_coordinate_array_dp(self)
        implicit none
        !> The coordinate array object to destroy.
        class(type_coordinate_array_dp), intent(inout) :: self

        call deallocate_array(self%x)
        call deallocate_array(self%y)
        call deallocate_array(self%z)

        self%length = 0
        self%is_allocated = .false.

    end subroutine destroy_type_coordinate_array_dp

    !>
    !> Checks if the coordinate arrays are allocated.
    !>
    pure function is_initialized_type_coordinate_array_dp(self) result(initialized)
        implicit none
        !> The coordinate array object to check.
        class(type_coordinate_array_dp), intent(in) :: self
        !> Logical result indicating if the arrays are allocated.
        logical :: initialized

        initialized = self%is_allocated

    end function is_initialized_type_coordinate_array_dp

    !>
    !> Returns the number of coordinate points in the array.
    !>
    pure function get_length_type_coordinate_array_dp(self) result(length)
        implicit none
        !> The coordinate array object.
        class(type_coordinate_array_dp), intent(in) :: self
        !> The number of coordinate points.
        integer(int32) :: length

        if (.not. self%is_allocated) then
            length = 0
        else
            length = self%length
        end if
    end function get_length_type_coordinate_array_dp

    !>
    !> Sets all coordinate components to zero.
    !>
    subroutine zero_type_coordinate_array_dp(self)
        implicit none
        !> The coordinate array object to zero.
        class(type_coordinate_array_dp), intent(inout) :: self

        if (self%is_allocated) then
            self%x(:) = 0.0d0
            self%y(:) = 0.0d0
            self%z(:) = 0.0d0
        end if
    end subroutine zero_type_coordinate_array_dp

    !>
    !> Returns the coordinate components as three separate arrays.
    !>
    subroutine get_coordinate_type_coordinate_array_dp(self, index, coordinate)
        implicit none
        !> The coordinate array object.
        class(type_coordinate_array_dp), intent(in) :: self
        !> The index of the coordinate point to retrieve.
        integer(int32), intent(in) :: index
        !> The output coordinate object to populate.
        type(type_coordinate_dp), intent(inout) :: coordinate

        if (self%is_allocated .and. index >= 1 .and. index <= self%length) then
            call coordinate%set(self%x(index), self%y(index), self%z(index))
        else
            call coordinate%set(0.0d0, 0.0d0, 0.0d0)
        end if
    end subroutine get_coordinate_type_coordinate_array_dp

    ! ==========================================================
    ! Integer Coordinate Array Procedures
    ! ==========================================================

    !>
    !> Allocates and initializes the coordinate component arrays.
    !>
    subroutine initialize_type_coordinate_array_int(self, length, initialize_value)
        implicit none
        !> The coordinate array object to initialize.
        class(type_coordinate_array_int), intent(inout) :: self
        !> The number of coordinate points to allocate.
        integer(int32), intent(in) :: length
        !> An optional value to assign to all components. Defaults to zero.
        integer(int32), intent(in), optional :: initialize_value

        call allocate_array(self%x, length)
        call allocate_array(self%y, length)
        call allocate_array(self%z, length)

        if (present(initialize_value)) then
            self%x(:) = initialize_value
            self%y(:) = initialize_value
            self%z(:) = initialize_value
        else
            self%x(:) = 0
            self%y(:) = 0
            self%z(:) = 0
        end if

        self%length = length
        self%is_allocated = .true.

    end subroutine initialize_type_coordinate_array_int

    !>
    !> Deallocates the coordinate component arrays.
    !>
    subroutine destroy_type_coordinate_array_int(self)
        implicit none
        !> The coordinate array object to destroy.
        class(type_coordinate_array_int), intent(inout) :: self

        call deallocate_array(self%x)
        call deallocate_array(self%y)
        call deallocate_array(self%z)

        self%length = 0
        self%is_allocated = .false.

    end subroutine destroy_type_coordinate_array_int

    !>
    !> Checks if the coordinate arrays are allocated.
    !>
    pure function is_initialized_type_coordinate_array_int(self) result(initialized)
        implicit none
        !> The coordinate array object to check.
        class(type_coordinate_array_int), intent(in) :: self
        !> Logical result indicating if the arrays are allocated.
        logical :: initialized

        initialized = self%is_allocated

    end function is_initialized_type_coordinate_array_int

    !>
    !> Returns the number of coordinate points in the array.
    !>
    pure function get_length_type_coordinate_array_int(self) result(length)
        implicit none
        !> The coordinate array object.
        class(type_coordinate_array_int), intent(in) :: self
        !> The number of coordinate points.
        integer(int32) :: length

        if (.not. self%is_allocated) then
            length = 0
        else
            length = self%length
        end if
    end function get_length_type_coordinate_array_int

    !>
    !> Sets all coordinate components to zero.
    !>
    subroutine zero_type_coordinate_array_int(self)
        implicit none
        !> The coordinate array object to zero.
        class(type_coordinate_array_int), intent(inout) :: self

        if (self%is_allocated) then
            self%x(:) = 0
            self%y(:) = 0
            self%z(:) = 0
        end if
    end subroutine zero_type_coordinate_array_int

    !>
    !> Returns the coordinate components as three separate arrays.
    !>
    subroutine get_coordinate_type_coordinate_array_int(self, index, coordinate)
        implicit none
        !> The coordinate array object.
        class(type_coordinate_array_int), intent(in) :: self
        !> The index of the coordinate point to retrieve.
        integer(int32), intent(in) :: index
        !> The output coordinate object to populate.
        type(type_coordinate_int), intent(inout) :: coordinate

        if (self%is_allocated .and. index >= 1 .and. index <= self%length) then
            call coordinate%set(self%x(index), self%y(index), self%z(index))
        else
            call coordinate%set(0, 0, 0)
        end if
    end subroutine get_coordinate_type_coordinate_array_int
end module core_types_coordinate_array

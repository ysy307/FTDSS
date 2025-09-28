!>
!> Defines derived types for handling arrays of 2D and 3D vectors,
!> supporting both double precision real and integer components.
!>
module core_types_coordinate_array
    use, intrinsic :: iso_fortran_env, only: real64, int32
    use :: core_allocate, only:allocate_array
    use :: core_deallocate, only:deallocate_array
    implicit none
    private

    public :: type_coordinate_array_dp
    public :: type_coordinate_array_int

    type :: type_coordinate_array_dp
        real(real64), allocatable :: x(:)
        real(real64), allocatable :: y(:)
        real(real64), allocatable :: z(:)
    contains
        procedure, pass(self) :: initialize => initialize_type_coordinate_array_dp
        procedure, pass(self) :: destroy => destroy_type_coordinate_array_dp
    end type

    type :: type_coordinate_array_int
        integer(int32), allocatable :: x(:)
        integer(int32), allocatable :: y(:)
        integer(int32), allocatable :: z(:)
    contains
        procedure, pass(self) :: initialize => initialize_type_coordinate_array_int
        procedure, pass(self) :: destroy => destroy_type_coordinate_array_int
    end type

contains
    subroutine initialize_type_coordinate_array_dp(self, length, initialize_value)
        implicit none
        class(type_coordinate_array_dp), intent(inout) :: self
        integer(int32), intent(in) :: length
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

    end subroutine initialize_type_coordinate_array_dp

    subroutine destroy_type_coordinate_array_dp(self)
        implicit none
        class(type_coordinate_array_dp), intent(inout) :: self

        call deallocate_array(self%x)
        call deallocate_array(self%y)
        call deallocate_array(self%z)

    end subroutine destroy_type_coordinate_array_dp

    subroutine initialize_type_coordinate_array_int(self, length, initialize_value)
        implicit none
        class(type_coordinate_array_int), intent(inout) :: self
        integer(int32), intent(in) :: length
        integer(int32), intent(in), optional :: initialize_value

        call allocate_array(self%x, length)
        call allocate_array(self%y, length)
        call allocate_array(self%z, length)

        if (present(initialize_value)) then
            self%x(:) = initialize_value
            self%y(:) = initialize_value
            self%z(:) = initialize_value
        else
            self%x(:) = 0_int32
            self%y(:) = 0_int32
            self%z(:) = 0_int32
        end if

    end subroutine initialize_type_coordinate_array_int

    subroutine destroy_type_coordinate_array_int(self)
        implicit none
        class(type_coordinate_array_int), intent(inout) :: self

        call deallocate_array(self%x)
        call deallocate_array(self%y)
        call deallocate_array(self%z)

    end subroutine destroy_type_coordinate_array_int
!     ! Public types
!     public :: type_dp_2d
!     public :: type_coordinate_array_dp
!     public :: type_int_2d
!     public :: type_int_3d

!     ! Public generic assignment
!     public :: assignment(=)

!     interface assignment(=)
!         module procedure :: assignment_type_dp_2d
!         module procedure :: assignment_type_coordinate_array_dp
!         module procedure :: assignment_type_int_2d
!         module procedure :: assignment_type_int_3d
!     end interface

!     !>
!     !> Represents an array of 2D double precision vectors.
!     !>
!     type :: type_dp_2d
!         !> Array of x-components.
!         real(real64), allocatable :: x(:)
!         !> Array of y-components.
!         real(real64), allocatable :: y(:)
!     contains
!         procedure, pass(self) :: initialize => initialize_type_dp_2d
!         procedure, pass(self) :: destroy => destroy_type_dp_2d
!     end type type_dp_2d

!     !>
!     !> Represents an array of 3D double precision vectors.
!     !>
!     type :: type_coordinate_array_dp
!         !> Array of x-components.
!         real(real64), allocatable :: x(:)
!         !> Array of y-components.
!         real(real64), allocatable :: y(:)
!         !> Array of z-components.
!         real(real64), allocatable :: z(:)
!     contains
!         procedure, pass(self) :: initialize => initialize_type_coordinate_array_dp
!         procedure, pass(self) :: destroy => destroy_type_coordinate_array_dp
!     end type type_coordinate_array_dp

!     !>
!     !> Represents an array of 2D integer vectors.
!     !>
!     type :: type_int_2d
!         !> Array of x-components.
!         integer(int32), allocatable :: x(:)
!         !> Array of y-components.
!         integer(int32), allocatable :: y(:)
!     contains
!         procedure, pass(self) :: initialize => initialize_type_int_2d
!         procedure, pass(self) :: destroy => destroy_type_int_2d
!     end type type_int_2d

!     !>
!     !> Represents an array of 3D integer vectors.
!     !>
!     type :: type_int_3d
!         !> Array of x-components.
!         integer(int32), allocatable :: x(:)
!         !> Array of y-components.
!         integer(int32), allocatable :: y(:)
!         !> Array of z-components.
!         integer(int32), allocatable :: z(:)
!     contains
!         procedure, pass(self) :: initialize => initialize_type_int_3d
!         procedure, pass(self) :: destroy => destroy_type_int_3d
!     end type type_int_3d

! contains

!     !>
!     !> Allocates and initializes the 2D double precision vector component arrays.
!     !>
!     subroutine initialize_type_dp_2d(self, length, initialize_value)
!         implicit none
!         !> The vector array object to initialize.
!         class(type_dp_2d), intent(inout) :: self
!         !> The number of vectors to allocate.
!         integer(int32), intent(in) :: length
!         !> An optional value to assign to all components. Defaults to zero.
!         real(real64), intent(in), optional :: initialize_value

!         call allocate_array(self%x, length)
!         call allocate_array(self%y, length)

!         if (present(initialize_value)) then
!             self%x(:) = initialize_value
!             self%y(:) = initialize_value
!         else
!             self%x(:) = 0.0d0
!             self%y(:) = 0.0d0
!         end if

!     end subroutine initialize_type_dp_2d

!     !>
!     !> Overloads the assignment operator (=) to perform a deep copy.
!     !>
!     subroutine assignment_type_dp_2d(a, b)
!         implicit none
!         !> The destination object (left-hand side).
!         class(type_dp_2d), intent(inout) :: a
!         !> The source object (right-hand side).
!         class(type_dp_2d), intent(in) :: b

!         a%x(:) = b%x(:)
!         a%y(:) = b%y(:)

!     end subroutine assignment_type_dp_2d

!     !>
!     !> Deallocates the component arrays of the 2D double precision vector type.
!     !>
!     subroutine destroy_type_dp_2d(self)
!         implicit none
!         !> The vector array object to destroy.
!         class(type_dp_2d), intent(inout) :: self

!         call deallocate_array(self%x)
!         call deallocate_array(self%y)

!     end subroutine destroy_type_dp_2d

!     !>
!     !> Allocates and initializes the 3D double precision vector component arrays.
!     !>
!     subroutine initialize_type_coordinate_array_dp(self, length, initialize_value)
!         implicit none
!         !> The vector array object to initialize.
!         class(type_coordinate_array_dp), intent(inout) :: self
!         !> The number of vectors to allocate.
!         integer(int32), intent(in) :: length
!         !> An optional value to assign to all components. Defaults to zero.
!         real(real64), intent(in), optional :: initialize_value

!         call allocate_array(self%x, length)
!         call allocate_array(self%y, length)
!         call allocate_array(self%z, length)

!         if (present(initialize_value)) then
!             self%x(:) = initialize_value
!             self%y(:) = initialize_value
!             self%z(:) = initialize_value
!         else
!             self%x(:) = 0.0d0
!             self%y(:) = 0.0d0
!             self%z(:) = 0.0d0
!         end if

!     end subroutine initialize_type_coordinate_array_dp

!     !>
!     !> Overloads the assignment operator (=) to perform a deep copy.
!     !>
!     subroutine assignment_type_coordinate_array_dp(a, b)
!         implicit none
!         !> The destination object (left-hand side).
!         class(type_coordinate_array_dp), intent(inout) :: a
!         !> The source object (right-hand side).
!         class(type_coordinate_array_dp), intent(in) :: b

!         a%x(:) = b%x(:)
!         a%y(:) = b%y(:)
!         a%z(:) = b%z(:)

!     end subroutine assignment_type_coordinate_array_dp

!     !>
!     !> Deallocates the component arrays of the 3D double precision vector type.
!     !>
!     subroutine destroy_type_coordinate_array_dp(self)
!         implicit none
!         !> The vector array object to destroy.
!         class(type_coordinate_array_dp), intent(inout) :: self

!         call deallocate_array(self%x)
!         call deallocate_array(self%y)
!         call deallocate_array(self%z)

!     end subroutine destroy_type_coordinate_array_dp

!     !>
!     !> Allocates and initializes the 2D integer vector component arrays.
!     !>
!     subroutine initialize_type_int_2d(self, length, initialize_value)
!         implicit none
!         !> The vector array object to initialize.
!         class(type_int_2d), intent(inout) :: self
!         !> The number of vectors to allocate.
!         integer(int32), intent(in) :: length
!         !> An optional value to assign to all components. Defaults to zero.
!         integer(int32), intent(in), optional :: initialize_value

!         call allocate_array(self%x, length)
!         call allocate_array(self%y, length)

!         if (present(initialize_value)) then
!             self%x(:) = initialize_value
!             self%y(:) = initialize_value
!         else
!             self%x(:) = 0_int32
!             self%y(:) = 0_int32
!         end if

!     end subroutine initialize_type_int_2d

!     !>
!     !> Overloads the assignment operator (=) to perform a deep copy.
!     !>
!     subroutine assignment_type_int_2d(a, b)
!         implicit none
!         !> The destination object (left-hand side).
!         class(type_int_2d), intent(inout) :: a
!         !> The source object (right-hand side).
!         class(type_int_2d), intent(in) :: b

!         a%x(:) = b%x(:)
!         a%y(:) = b%y(:)

!     end subroutine assignment_type_int_2d

!     !>
!     !> Deallocates the component arrays of the 2D integer vector type.
!     !>
!     subroutine destroy_type_int_2d(self)
!         implicit none
!         !> The vector array object to destroy.
!         class(type_int_2d), intent(inout) :: self

!         call deallocate_array(self%x)
!         call deallocate_array(self%y)

!     end subroutine destroy_type_int_2d

!     !>
!     !> Allocates and initializes the 3D integer vector component arrays.
!     !>
!     subroutine initialize_type_int_3d(self, length, initialize_value)
!         implicit none
!         !> The vector array object to initialize.
!         class(type_int_3d), intent(inout) :: self
!         !> The number of vectors to allocate.
!         integer(int32), intent(in) :: length
!         !> An optional value to assign to all components. Defaults to zero.
!         integer(int32), intent(in), optional :: initialize_value

!         call allocate_array(self%x, length)
!         call allocate_array(self%y, length)
!         call allocate_array(self%z, length)

!         if (present(initialize_value)) then
!             self%x(:) = initialize_value
!             self%y(:) = initialize_value
!             self%z(:) = initialize_value
!         else
!             self%x(:) = 0_int32
!             self%y(:) = 0_int32
!             self%z(:) = 0_int32
!         end if

!     end subroutine initialize_type_int_3d

!     !>
!     !> Overloads the assignment operator (=) to perform a deep copy.
!     !>
!     subroutine assignment_type_int_3d(a, b)
!         implicit none
!         !> The destination object (left-hand side).
!         class(type_int_3d), intent(inout) :: a
!         !> The source object (right-hand side).
!         class(type_int_3d), intent(in) :: b

!         a%x(:) = b%x(:)
!         a%y(:) = b%y(:)
!         a%z(:) = b%z(:)

!     end subroutine assignment_type_int_3d

!     !>
!     !> Deallocates the component arrays of the 3D integer vector type.
!     !>
!     subroutine destroy_type_int_3d(self)
!         implicit none
!         !> The vector array object to destroy.
!         class(type_int_3d), intent(inout) :: self

!         call deallocate_array(self%x)
!         call deallocate_array(self%y)
!         call deallocate_array(self%z)

!     end subroutine destroy_type_int_3d

end module core_types_coordinate_array

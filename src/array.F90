module core_types_array
    use, intrinsic :: iso_fortran_env, only: real64, int32
    use :: core_allocate, only:allocate_array
    use :: core_deallocate, only:deallocate_array
    implicit none
    private

    ! 2D and 3D arrays of double precision
    public :: type_dp_2d
    public :: type_dp_3d
    ! 2D and 3D arrays of integers
    public :: type_int_2d
    public :: type_int_3d

    public :: assignment(=)

    interface assignment(=)
        module procedure :: assignment_type_dp_2d
        module procedure :: assignment_type_dp_3d
        module procedure :: assignment_type_int_2d
        module procedure :: assignment_type_int_3d
    end interface

    type :: type_dp_2d
        real(real64), allocatable :: x(:)
        real(real64), allocatable :: y(:)
    contains
        procedure, pass(self) :: initialize => initialize_type_dp_2d
        procedure, pass(self) :: destroy => destroy_type_dp_2d
    end type type_dp_2d

    type :: type_dp_3d
        real(real64), allocatable :: x(:)
        real(real64), allocatable :: y(:)
        real(real64), allocatable :: z(:)
    contains
        procedure, pass(self) :: initialize => initialize_type_dp_3d
        procedure, pass(self) :: destroy => destroy_type_dp_3d
    end type type_dp_3d

    type :: type_int_2d
        integer(int32), allocatable :: x(:)
        integer(int32), allocatable :: y(:)
    contains
        procedure, pass(self) :: initialize => initialize_type_int_2d
        procedure, pass(self) :: destroy => destroy_type_int_2d
    end type type_int_2d

    type :: type_int_3d
        integer(int32), allocatable :: x(:)
        integer(int32), allocatable :: y(:)
        integer(int32), allocatable :: z(:)
    contains
        procedure, pass(self) :: initialize => initialize_type_int_3d
        procedure, pass(self) :: destroy => destroy_type_int_3d
    end type type_int_3d

contains

    subroutine initialize_type_dp_2d(self, length, initialize_value)
        implicit none
        class(type_dp_2d), intent(inout) :: self
        integer(int32), intent(in) :: length
        real(real64), intent(in), optional :: initialize_value

        call allocate_array(self%x, length)
        call allocate_array(self%y, length)

        if (present(initialize_value)) then
            self%x(:) = initialize_value
            self%y(:) = initialize_value
        else
            self%x(:) = 0.0d0
            self%y(:) = 0.0d0
        end if

    end subroutine initialize_type_dp_2d

    subroutine assignment_type_dp_2d(a, b)
        implicit none
        class(type_dp_2d), intent(inout) :: a
        class(type_dp_2d), intent(in) :: b

        a%x(:) = b%x(:)
        a%y(:) = b%y(:)

    end subroutine assignment_type_dp_2d

    subroutine destroy_type_dp_2d(self)
        implicit none
        class(type_dp_2d), intent(inout) :: self

        call deallocate_array(self%x)
        call deallocate_array(self%y)

    end subroutine destroy_type_dp_2d

    subroutine initialize_type_dp_3d(self, length, initialize_value)
        implicit none
        class(type_dp_3d), intent(inout) :: self
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

    end subroutine initialize_type_dp_3d

    subroutine assignment_type_dp_3d(a, b)
        implicit none
        class(type_dp_3d), intent(inout) :: a
        class(type_dp_3d), intent(in) :: b

        a%x(:) = b%x(:)
        a%y(:) = b%y(:)
        a%z(:) = b%z(:)

    end subroutine assignment_type_dp_3d

    subroutine destroy_type_dp_3d(self)
        implicit none
        class(type_dp_3d), intent(inout) :: self

        call deallocate_array(self%x)
        call deallocate_array(self%y)
        call deallocate_array(self%z)

    end subroutine destroy_type_dp_3d

    subroutine initialize_type_int_2d(self, length, initialize_value)
        implicit none
        class(type_int_2d), intent(inout) :: self
        integer(int32), intent(in) :: length
        integer(int32), intent(in), optional :: initialize_value

        call allocate_array(self%x, length)
        call allocate_array(self%y, length)

        if (present(initialize_value)) then
            self%x(:) = initialize_value
            self%y(:) = initialize_value
        else
            self%x(:) = 0_int32
            self%y(:) = 0_int32
        end if

    end subroutine initialize_type_int_2d

    subroutine assignment_type_int_2d(a, b)
        implicit none
        class(type_int_2d), intent(inout) :: a
        class(type_int_2d), intent(in) :: b

        a%x(:) = b%x(:)
        a%y(:) = b%y(:)

    end subroutine assignment_type_int_2d

    subroutine destroy_type_int_2d(self)
        implicit none
        class(type_int_2d), intent(inout) :: self

        call deallocate_array(self%x)
        call deallocate_array(self%y)

    end subroutine destroy_type_int_2d

    subroutine initialize_type_int_3d(self, length, initialize_value)
        implicit none
        class(type_int_3d), intent(inout) :: self
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

    end subroutine initialize_type_int_3d

    subroutine assignment_type_int_3d(a, b)
        implicit none
        class(type_int_3d), intent(inout) :: a
        class(type_int_3d), intent(in) :: b

        a%x(:) = b%x(:)
        a%y(:) = b%y(:)
        a%z(:) = b%z(:)

    end subroutine assignment_type_int_3d

    subroutine destroy_type_int_3d(self)
        implicit none
        class(type_int_3d), intent(inout) :: self

        call deallocate_array(self%x)
        call deallocate_array(self%y)
        call deallocate_array(self%z)

    end subroutine destroy_type_int_3d

end module core_types_array

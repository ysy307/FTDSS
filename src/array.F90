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
        module procedure :: type_dp_2d_assignment
        module procedure :: type_dp_3d_assignment
        module procedure :: type_int_2d_assignment
        module procedure :: type_int_3d_assignment
    end interface

    type :: type_dp_2d
        real(real64), allocatable :: x(:)
        real(real64), allocatable :: y(:)
    contains
        procedure, pass(self) :: initialize => type_dp_2d_initialize
        procedure, pass(self) :: destory => type_dp_2d_destroy
    end type type_dp_2d

    type :: type_dp_3d
        real(real64), allocatable :: x(:)
        real(real64), allocatable :: y(:)
        real(real64), allocatable :: z(:)
    contains
        procedure, pass(self) :: initialize => type_dp_3d_initialize
    end type type_dp_3d

    type :: type_int_2d
        integer(int32), allocatable :: x(:)
        integer(int32), allocatable :: y(:)
    end type type_int_2d

    type :: type_int_3d
        integer(int32), allocatable :: x(:)
        integer(int32), allocatable :: y(:)
        integer(int32), allocatable :: z(:)
    end type type_int_3d

contains

    subroutine type_dp_2d_initialize(self, length, initialize_value)
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

    end subroutine type_dp_2d_initialize

    subroutine type_dp_2d_assignment(a, b)
        implicit none
        class(type_dp_2d), intent(inout) :: a
        class(type_dp_2d), intent(in) :: b

        a%x(:) = b%x(:)
        a%y(:) = b%y(:)

    end subroutine type_dp_2d_assignment

    subroutine type_dp_2d_destroy(self)
        implicit none
        class(type_dp_2d), intent(inout) :: self

        call deallocate_array(self%x)
        call deallocate_array(self%y)

    end subroutine type_dp_2d_destroy

    subroutine type_dp_3d_initialize(self, length, initialize_value)
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

    end subroutine type_dp_3d_initialize

    subroutine type_dp_3d_assignment(a, b)
        implicit none
        class(type_dp_3d), intent(inout) :: a
        class(type_dp_3d), intent(in) :: b

        a%x(:) = b%x(:)
        a%y(:) = b%y(:)
        a%z(:) = b%z(:)

    end subroutine type_dp_3d_assignment

    subroutine type_dp_3d_destroy(self)
        implicit none
        class(type_dp_3d), intent(inout) :: self

        call deallocate_array(self%x)
        call deallocate_array(self%y)
        call deallocate_array(self%z)

    end subroutine type_dp_3d_destroy

    subroutine type_int_2d_initialize(self, length, initialize_value)
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

    end subroutine type_int_2d_initialize

    subroutine type_int_2d_assignment(a, b)
        implicit none
        class(type_int_2d), intent(inout) :: a
        class(type_int_2d), intent(in) :: b

        a%x(:) = b%x(:)
        a%y(:) = b%y(:)

    end subroutine type_int_2d_assignment

    subroutine type_int_2d_destroy(self)
        implicit none
        class(type_int_2d), intent(inout) :: self

        call deallocate_array(self%x)
        call deallocate_array(self%y)

    end subroutine type_int_2d_destroy

    subroutine type_int_3d_initialize(self, length, initialize_value)
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

    end subroutine type_int_3d_initialize

    subroutine type_int_3d_assignment(a, b)
        implicit none
        class(type_int_3d), intent(inout) :: a
        class(type_int_3d), intent(in) :: b

        a%x(:) = b%x(:)
        a%y(:) = b%y(:)
        a%z(:) = b%z(:)

    end subroutine type_int_3d_assignment

    subroutine type_int_3d_destroy(self)
        implicit none
        class(type_int_3d), intent(inout) :: self

        call deallocate_array(self%x)
        call deallocate_array(self%y)
        call deallocate_array(self%z)

    end subroutine type_int_3d_destroy

end module core_types_array

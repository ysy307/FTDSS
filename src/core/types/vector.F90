module core_types_vector
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    public :: type_dp_vector_2d
    public :: type_dp_vector_3d

    public :: type_int_vector_2d
    public :: type_int_vector_3d

    public :: assignment(=)
    interface assignment(=)
        module procedure :: type_dp_vector_2d_assignment
        module procedure :: type_dp_vector_3d_assignment
        module procedure :: type_int_vector_2d_assignment
        module procedure :: type_int_vector_3d_assignment
    end interface assignment(=)

    public :: operator(+)
    interface operator(+)
        module procedure :: type_dp_vector_2d_add
        module procedure :: type_dp_vector_3d_add
        module procedure :: type_int_vector_2d_add
        module procedure :: type_int_vector_3d_add
    end interface operator(+)

    public :: operator(-)
    interface operator(-)
        module procedure :: type_dp_vector_2d_sub
        module procedure :: type_dp_vector_3d_sub
        module procedure :: type_int_vector_2d_sub
        module procedure :: type_int_vector_3d_sub
    end interface operator(-)

    type :: type_dp_vector_2d
        real(real64) :: x
        real(real64) :: y
    contains
        procedure, private, pass(self) :: set_dp_vector_2d
        procedure, private, pass(self) :: set_dp_vector_2d_array
        generic, public :: set => set_dp_vector_2d, set_dp_vector_2d_array
    end type type_dp_vector_2d

    type :: type_dp_vector_3d
        real(real64) :: x
        real(real64) :: y
        real(real64) :: z
    contains
        procedure, private, pass(self) :: set_dp_vector_3d
        procedure, private, pass(self) :: set_dp_vector_3d_array
        generic, public :: set => set_dp_vector_3d, set_dp_vector_3d_array
    end type type_dp_vector_3d

    type :: type_int_vector_2d
        integer(int32) :: x
        integer(int32) :: y
    contains
        procedure, private, pass(self) :: set_int_vector_2d
        procedure, private, pass(self) :: set_int_vector_2d_array
        generic, public :: set => set_int_vector_2d, set_int_vector_2d_array
    end type type_int_vector_2d

    type :: type_int_vector_3d
        integer(int32) :: x
        integer(int32) :: y
        integer(int32) :: z
    contains
        procedure, private, pass(self) :: set_int_vector_3d
        procedure, private, pass(self) :: set_int_vector_3d_array
        generic, public :: set => set_int_vector_3d, set_int_vector_3d_array
    end type type_int_vector_3d

contains

    ! Assignment for 2D double precision vector
    pure elemental subroutine type_dp_vector_2d_assignment(a, b)
        implicit none
        type(type_dp_vector_2d), intent(inout) :: a
        type(type_dp_vector_2d), intent(in) :: b

        a%x = b%x
        a%y = b%y
    end subroutine type_dp_vector_2d_assignment

    pure elemental function type_dp_vector_2d_add(a, b) result(c)
        implicit none
        type(type_dp_vector_2d), intent(in) :: a, b
        type(type_dp_vector_2d) :: c

        c%x = a%x + b%x
        c%y = a%y + b%y
    end function type_dp_vector_2d_add

    pure elemental function type_dp_vector_2d_sub(a, b) result(c)
        implicit none
        type(type_dp_vector_2d), intent(in) :: a, b
        type(type_dp_vector_2d) :: c

        c%x = a%x - b%x
        c%y = a%y - b%y
    end function type_dp_vector_2d_sub

    subroutine set_dp_vector_2d(self, x_val, y_val)
        implicit none
        class(type_dp_vector_2d), intent(inout) :: self
        real(real64), intent(in) :: x_val, y_val

        self%x = x_val
        self%y = y_val
    end subroutine set_dp_vector_2d

    subroutine set_dp_vector_2d_array(self, value)
        implicit none
        class(type_dp_vector_2d), intent(inout) :: self
        real(real64), intent(in) :: value(2)

        self%x = value(1)
        self%y = value(2)
    end subroutine set_dp_vector_2d_array

    ! Assignment for 3D double precision vector
    pure elemental subroutine type_dp_vector_3d_assignment(a, b)
        implicit none
        type(type_dp_vector_3d), intent(inout) :: a
        type(type_dp_vector_3d), intent(in) :: b

        a%x = b%x
        a%y = b%y
        a%z = b%z
    end subroutine type_dp_vector_3d_assignment

    pure elemental function type_dp_vector_3d_add(a, b) result(c)
        implicit none
        type(type_dp_vector_3d), intent(in) :: a, b
        type(type_dp_vector_3d) :: c

        c%x = a%x + b%x
        c%y = a%y + b%y
        c%z = a%z + b%z
    end function type_dp_vector_3d_add

    pure elemental function type_dp_vector_3d_sub(a, b) result(c)
        implicit none
        type(type_dp_vector_3d), intent(in) :: a, b
        type(type_dp_vector_3d) :: c

        c%x = a%x - b%x
        c%y = a%y - b%y
        c%z = a%z - b%z
    end function type_dp_vector_3d_sub

    subroutine set_dp_vector_3d(self, x_val, y_val, z_val)
        implicit none
        class(type_dp_vector_3d), intent(inout) :: self
        real(real64), intent(in) :: x_val, y_val, z_val

        self%x = x_val
        self%y = y_val
        self%z = z_val
    end subroutine set_dp_vector_3d

    subroutine set_dp_vector_3d_array(self, value)
        implicit none
        class(type_dp_vector_3d), intent(inout) :: self
        real(real64), intent(in) :: value(3)

        self%x = value(1)
        self%y = value(2)
        self%z = value(3)
    end subroutine set_dp_vector_3d_array

    ! Assignment for 2D integer vector
    pure elemental subroutine type_int_vector_2d_assignment(a, b)
        implicit none
        type(type_int_vector_2d), intent(inout) :: a
        type(type_int_vector_2d), intent(in) :: b

        a%x = b%x
        a%y = b%y
    end subroutine type_int_vector_2d_assignment

    pure elemental function type_int_vector_2d_add(a, b) result(c)
        implicit none
        type(type_int_vector_2d), intent(in) :: a, b
        type(type_int_vector_2d) :: c

        c%x = a%x + b%x
        c%y = a%y + b%y
    end function type_int_vector_2d_add

    pure elemental function type_int_vector_2d_sub(a, b) result(c)
        implicit none
        type(type_int_vector_2d), intent(in) :: a, b
        type(type_int_vector_2d) :: c

        c%x = a%x - b%x
        c%y = a%y - b%y
    end function type_int_vector_2d_sub

    subroutine set_int_vector_2d(self, x_val, y_val)
        implicit none
        class(type_int_vector_2d), intent(inout) :: self
        integer(int32), intent(in) :: x_val, y_val

        self%x = x_val
        self%y = y_val
    end subroutine set_int_vector_2d

    subroutine set_int_vector_2d_array(self, value)
        implicit none
        class(type_int_vector_2d), intent(inout) :: self
        integer(int32), intent(in) :: value(2)

        self%x = value(1)
        self%y = value(2)
    end subroutine set_int_vector_2d_array

    ! Assignment for 3D integer vector
    pure elemental subroutine type_int_vector_3d_assignment(a, b)
        implicit none
        type(type_int_vector_3d), intent(inout) :: a
        type(type_int_vector_3d), intent(in) :: b

        a%x = b%x
        a%y = b%y
        a%z = b%z
    end subroutine type_int_vector_3d_assignment

    pure elemental function type_int_vector_3d_add(a, b) result(c)
        implicit none
        type(type_int_vector_3d), intent(in) :: a, b
        type(type_int_vector_3d) :: c

        c%x = a%x + b%x
        c%y = a%y + b%y
        c%z = a%z + b%z
    end function type_int_vector_3d_add

    pure elemental function type_int_vector_3d_sub(a, b) result(c)
        implicit none
        type(type_int_vector_3d), intent(in) :: a, b
        type(type_int_vector_3d) :: c

        c%x = a%x - b%x
        c%y = a%y - b%y
        c%z = a%z - b%z
    end function type_int_vector_3d_sub

    subroutine set_int_vector_3d(self, x_val, y_val, z_val)
        implicit none
        class(type_int_vector_3d), intent(inout) :: self
        integer(int32), intent(in) :: x_val, y_val, z_val

        self%x = x_val
        self%y = y_val
        self%z = z_val
    end subroutine set_int_vector_3d

    subroutine set_int_vector_3d_array(self, value)
        implicit none
        class(type_int_vector_3d), intent(inout) :: self
        integer(int32), intent(in) :: value(3)

        self%x = value(1)
        self%y = value(2)
        self%z = value(3)
    end subroutine set_int_vector_3d_array

end module core_types_vector

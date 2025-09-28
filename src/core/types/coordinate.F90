!>
!> Defines derived types for handling 2D and 3D vectors.
!> This module provides types for both double precision and integer vectors,
!> along with overloaded operators for common vector arithmetic.
!>
module core_types_coordinate
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    public :: type_coordinate_dp
    public :: type_coordinate_int

    !> 倍精度 3D座標クラス
    type :: type_coordinate_dp
        real(real64) :: x = 0.0d0
        real(real64) :: y = 0.0d0
        real(real64) :: z = 0.0d0
    contains
        procedure, private, pass(self) :: set_coordinate_dp
        procedure, private, pass(self) :: set_coordinate_dp_array
        generic, public :: set => set_coordinate_dp, set_coordinate_dp_array
    end type type_coordinate_dp

    !> 整数 3D座標クラス
    type :: type_coordinate_int
        integer(int32) :: x = 0
        integer(int32) :: y = 0
        integer(int32) :: z = 0
    contains
        procedure, private, pass(self) :: set_coordinate_int
        procedure, private, pass(self) :: set_coordinate_int_array
        generic, public :: set => set_coordinate_int, set_coordinate_int_array
    end type type_coordinate_int

contains

    subroutine set_coordinate_dp(self, x_val, y_val, z_val)
        implicit none
        class(type_coordinate_dp), intent(inout) :: self
        real(real64), intent(in) :: x_val
        real(real64), intent(in) :: y_val
        real(real64), intent(in) :: z_val

        self%x = x_val
        self%y = y_val
        self%z = z_val
    end subroutine set_coordinate_dp

    subroutine set_coordinate_dp_array(self, value)
        implicit none
        class(type_coordinate_dp), intent(inout) :: self
        real(real64), intent(in) :: value(3)

        self%x = value(1)
        self%y = value(2)
        self%z = value(3)
    end subroutine set_coordinate_dp_array

    subroutine set_coordinate_int(self, x_val, y_val, z_val)
        implicit none
        class(type_coordinate_int), intent(inout) :: self
        integer(int32), intent(in) :: x_val
        integer(int32), intent(in) :: y_val
        integer(int32), intent(in) :: z_val

        self%x = x_val
        self%y = y_val
        self%z = z_val
    end subroutine set_coordinate_int

    subroutine set_coordinate_int_array(self, value)
        implicit none
        class(type_coordinate_int), intent(inout) :: self
        integer(int32), intent(in) :: value(3)

        self%x = value(1)
        self%y = value(2)
        self%z = value(3)
    end subroutine set_coordinate_int_array

!     ! Public Types
!     public :: type_dp_vector_2d
!     public :: type_coordinate_dp
!     public :: type_int_vector_2d
!     public :: type_int_vector_3d

!     ! Public Generic Operators
!     public :: assignment(=)
!     public :: operator(+)
!     public :: operator(-)

!     interface assignment(=)
!         module procedure :: type_dp_vector_2d_assignment
!         module procedure :: type_coordinate_dp_assignment
!         module procedure :: type_int_vector_2d_assignment
!         module procedure :: type_int_vector_3d_assignment
!     end interface assignment(=)

!     interface operator(+)
!         module procedure :: type_dp_vector_2d_add
!         module procedure :: type_coordinate_dp_add
!         module procedure :: type_int_vector_2d_add
!         module procedure :: type_int_vector_3d_add
!     end interface operator(+)

!     interface operator(-)
!         module procedure :: type_dp_vector_2d_sub
!         module procedure :: type_coordinate_dp_sub
!         module procedure :: type_int_vector_2d_sub
!         module procedure :: type_int_vector_3d_sub
!     end interface operator(-)

!     !>
!     !> Represents a 1D vector with double precision components.
!     !>
!     type :: type_dp_vector_1d
!         !> The x-component of the vector.
!         real(real64) :: x
!     contains
!         procedure, private, pass(self) :: set_dp_vector_1d
!         procedure, private, pass(self) :: set_dp_vector_1d_array
!         !> Generic interface to set the vector's components.
!         generic, public :: set => set_dp_vector_1d, set_dp_vector_1d_array
!     end type type_dp_vector_1d

!     !>
!     !> Represents a 2D vector with double precision components.
!     !>
!     type :: type_dp_vector_2d
!         !> The x-component of the vector.
!         real(real64) :: x
!         !> The y-component of the vector.
!         real(real64) :: y
!     contains
!         procedure, private, pass(self) :: set_dp_vector_2d
!         procedure, private, pass(self) :: set_dp_vector_2d_array
!         !> Generic interface to set the vector's components.
!         generic, public :: set => set_dp_vector_2d, set_dp_vector_2d_array
!     end type type_dp_vector_2d

!     !>
!     !> Represents a 3D vector with double precision components.
!     !>
!     type :: type_coordinate_dp
!         !> The x-component of the vector.
!         real(real64) :: x
!         !> The y-component of the vector.
!         real(real64) :: y
!         !> The z-component of the vector.
!         real(real64) :: z
!     contains
!         procedure, private, pass(self) :: set_dp_vector_3d
!         procedure, private, pass(self) :: set_dp_vector_3d_array
!         !> Generic interface to set the vector's components.
!         generic, public :: set => set_dp_vector_3d, set_dp_vector_3d_array
!     end type type_coordinate_dp

!     !>
!     !> Represents a 1D vector with integer components.
!     !>
!     type :: type_int_vector_1d
!         !> The x-component of the vector.
!         integer(int32) :: x
!         !> The y-component of the vector.
!         integer(int32) :: y
!     contains
!         procedure, private, pass(self) :: set_int_vector_2d
!         procedure, private, pass(self) :: set_int_vector_2d_array
!         !> Generic interface to set the vector's components.
!         generic, public :: set => set_int_vector_2d, set_int_vector_2d_array
!     end type type_int_vector_1d

!     !>
!     !> Represents a 2D vector with integer components.
!     !>
!     type :: type_int_vector_2d
!         !> The x-component of the vector.
!         integer(int32) :: x
!         !> The y-component of the vector.
!         integer(int32) :: y
!     contains
!         procedure, private, pass(self) :: set_int_vector_2d
!         procedure, private, pass(self) :: set_int_vector_2d_array
!         !> Generic interface to set the vector's components.
!         generic, public :: set => set_int_vector_2d, set_int_vector_2d_array
!     end type type_int_vector_2d

!     !>
!     !> Represents a 3D vector with integer components.
!     !>
!     type :: type_int_vector_3d
!         !> The x-component of the vector.
!         integer(int32) :: x
!         !> The y-component of the vector.
!         integer(int32) :: y
!         !> The z-component of the vector.
!         integer(int32) :: z
!     contains
!         procedure, private, pass(self) :: set_int_vector_3d
!         procedure, private, pass(self) :: set_int_vector_3d_array
!         !> Generic interface to set the vector's components.
!         generic, public :: set => set_int_vector_3d, set_int_vector_3d_array
!     end type type_int_vector_3d

! contains

!     ! ==========================================================
!     ! 2D Double Precision Vector Procedures
!     ! ==========================================================

!     !>
!     !> Overloads the assignment operator (=) for the 2D double precision vector type.
!     !>
!     pure elemental subroutine type_dp_vector_2d_assignment(a, b)
!         implicit none
!         !> The destination vector (left-hand side).
!         type(type_dp_vector_2d), intent(inout) :: a
!         !> The source vector (right-hand side).
!         type(type_dp_vector_2d), intent(in) :: b

!         a%x = b%x
!         a%y = b%y
!     end subroutine type_dp_vector_2d_assignment

!     !>
!     !> Overloads the addition operator (+) for component-wise vector addition.
!     !>
!     pure elemental function type_dp_vector_2d_add(a, b) result(c)
!         implicit none
!         !> The input vectors.
!         type(type_dp_vector_2d), intent(in) :: a, b
!         !> The resulting vector from the addition.
!         type(type_dp_vector_2d) :: c

!         c%x = a%x + b%x
!         c%y = a%y + b%y
!     end function type_dp_vector_2d_add

!     !>
!     !> Overloads the subtraction operator (-) for component-wise vector subtraction.
!     !>
!     pure elemental function type_dp_vector_2d_sub(a, b) result(c)
!         implicit none
!         !> The input vectors.
!         type(type_dp_vector_2d), intent(in) :: a, b
!         !> The resulting vector from the subtraction.
!         type(type_dp_vector_2d) :: c

!         c%x = a%x - b%x
!         c%y = a%y - b%y
!     end function type_dp_vector_2d_sub

!     !>
!     !> Sets the vector components from individual scalar values.
!     !>
!     subroutine set_dp_vector_2d(self, x_val, y_val)
!         implicit none
!         !> The vector object to modify.
!         class(type_dp_vector_2d), intent(inout) :: self
!         !> The new x-component value.
!         real(real64), intent(in) :: x_val
!         !> The new y-component value.
!         real(real64), intent(in) :: y_val

!         self%x = x_val
!         self%y = y_val
!     end subroutine set_dp_vector_2d

!     !>
!     !> Sets the vector components from a 2-element array.
!     !>
!     subroutine set_dp_vector_2d_array(self, value)
!         implicit none
!         !> The vector object to modify.
!         class(type_dp_vector_2d), intent(inout) :: self
!         !> A 2-element array containing the new x and y values.
!         real(real64), intent(in) :: value(2)

!         self%x = value(1)
!         self%y = value(2)
!     end subroutine set_dp_vector_2d_array

!     ! ==========================================================
!     ! 3D Double Precision Vector Procedures
!     ! ==========================================================

!     !>
!     !> Overloads the assignment operator (=) for the 3D double precision vector type.
!     !>
!     pure elemental subroutine type_coordinate_dp_assignment(a, b)
!         implicit none
!         !> The destination vector (left-hand side).
!         type(type_coordinate_dp), intent(inout) :: a
!         !> The source vector (right-hand side).
!         type(type_coordinate_dp), intent(in) :: b

!         a%x = b%x
!         a%y = b%y
!         a%z = b%z
!     end subroutine type_coordinate_dp_assignment

!     !>
!     !> Overloads the addition operator (+) for component-wise vector addition.
!     !>
!     pure elemental function type_coordinate_dp_add(a, b) result(c)
!         implicit none
!         !> The input vectors.
!         type(type_coordinate_dp), intent(in) :: a, b
!         !> The resulting vector from the addition.
!         type(type_coordinate_dp) :: c

!         c%x = a%x + b%x
!         c%y = a%y + b%y
!         c%z = a%z + b%z
!     end function type_coordinate_dp_add

!     !>
!     !> Overloads the subtraction operator (-) for component-wise vector subtraction.
!     !>
!     pure elemental function type_coordinate_dp_sub(a, b) result(c)
!         implicit none
!         !> The input vectors.
!         type(type_coordinate_dp), intent(in) :: a, b
!         !> The resulting vector from the subtraction.
!         type(type_coordinate_dp) :: c

!         c%x = a%x - b%x
!         c%y = a%y - b%y
!         c%z = a%z - b%z
!     end function type_coordinate_dp_sub

!     !>
!     !> Sets the vector components from individual scalar values.
!     !>
!     subroutine set_dp_vector_3d(self, x_val, y_val, z_val)
!         implicit none
!         !> The vector object to modify.
!         class(type_coordinate_dp), intent(inout) :: self
!         !> The new x-component value.
!         real(real64), intent(in) :: x_val
!         !> The new y-component value.
!         real(real64), intent(in) :: y_val
!         !> The new z-component value.
!         real(real64), intent(in) :: z_val

!         self%x = x_val
!         self%y = y_val
!         self%z = z_val
!     end subroutine set_dp_vector_3d

!     !>
!     !> Sets the vector components from a 3-element array.
!     !>
!     subroutine set_dp_vector_3d_array(self, value)
!         implicit none
!         !> The vector object to modify.
!         class(type_coordinate_dp), intent(inout) :: self
!         !> A 3-element array containing the new x, y, and z values.
!         real(real64), intent(in) :: value(3)

!         self%x = value(1)
!         self%y = value(2)
!         self%z = value(3)
!     end subroutine set_dp_vector_3d_array

!     ! ==========================================================
!     ! 2D Integer Vector Procedures
!     ! ==========================================================

!     !>
!     !> Overloads the assignment operator (=) for the 2D integer vector type.
!     !>
!     pure elemental subroutine type_int_vector_2d_assignment(a, b)
!         implicit none
!         !> The destination vector (left-hand side).
!         type(type_int_vector_2d), intent(inout) :: a
!         !> The source vector (right-hand side).
!         type(type_int_vector_2d), intent(in) :: b

!         a%x = b%x
!         a%y = b%y
!     end subroutine type_int_vector_2d_assignment

!     !>
!     !> Overloads the addition operator (+) for component-wise vector addition.
!     !>
!     pure elemental function type_int_vector_2d_add(a, b) result(c)
!         implicit none
!         !> The input vectors.
!         type(type_int_vector_2d), intent(in) :: a, b
!         !> The resulting vector from the addition.
!         type(type_int_vector_2d) :: c

!         c%x = a%x + b%x
!         c%y = a%y + b%y
!     end function type_int_vector_2d_add

!     !>
!     !> Overloads the subtraction operator (-) for component-wise vector subtraction.
!     !>
!     pure elemental function type_int_vector_2d_sub(a, b) result(c)
!         implicit none
!         !> The input vectors.
!         type(type_int_vector_2d), intent(in) :: a, b
!         !> The resulting vector from the subtraction.
!         type(type_int_vector_2d) :: c

!         c%x = a%x - b%x
!         c%y = a%y - b%y
!     end function type_int_vector_2d_sub

!     !>
!     !> Sets the vector components from individual scalar values.
!     !>
!     subroutine set_int_vector_2d(self, x_val, y_val)
!         implicit none
!         !> The vector object to modify.
!         class(type_int_vector_2d), intent(inout) :: self
!         !> The new x-component value.
!         integer(int32), intent(in) :: x_val
!         !> The new y-component value.
!         integer(int32), intent(in) :: y_val

!         self%x = x_val
!         self%y = y_val
!     end subroutine set_int_vector_2d

!     !>
!     !> Sets the vector components from a 2-element array.
!     !>
!     subroutine set_int_vector_2d_array(self, value)
!         implicit none
!         !> The vector object to modify.
!         class(type_int_vector_2d), intent(inout) :: self
!         !> A 2-element array containing the new x and y values.
!         integer(int32), intent(in) :: value(2)

!         self%x = value(1)
!         self%y = value(2)
!     end subroutine set_int_vector_2d_array

!     ! ==========================================================
!     ! 3D Integer Vector Procedures
!     ! ==========================================================

!     !>
!     !> Overloads the assignment operator (=) for the 3D integer vector type.
!     !>
!     pure elemental subroutine type_int_vector_3d_assignment(a, b)
!         implicit none
!         !> The destination vector (left-hand side).
!         type(type_int_vector_3d), intent(inout) :: a
!         !> The source vector (right-hand side).
!         type(type_int_vector_3d), intent(in) :: b

!         a%x = b%x
!         a%y = b%y
!         a%z = b%z
!     end subroutine type_int_vector_3d_assignment

!     !>
!     !> Overloads the addition operator (+) for component-wise vector addition.
!     !>
!     pure elemental function type_int_vector_3d_add(a, b) result(c)
!         implicit none
!         !> The input vectors.
!         type(type_int_vector_3d), intent(in) :: a, b
!         !> The resulting vector from the addition.
!         type(type_int_vector_3d) :: c

!         c%x = a%x + b%x
!         c%y = a%y + b%y
!         c%z = a%z + b%z
!     end function type_int_vector_3d_add

!     !>
!     !> Overloads the subtraction operator (-) for component-wise vector subtraction.
!     !>
!     pure elemental function type_int_vector_3d_sub(a, b) result(c)
!         implicit none
!         !> The input vectors.
!         type(type_int_vector_3d), intent(in) :: a, b
!         !> The resulting vector from the subtraction.
!         type(type_int_vector_3d) :: c

!         c%x = a%x - b%x
!         c%y = a%y - b%y
!         c%z = a%z - b%z
!     end function type_int_vector_3d_sub

!     !>
!     !> Sets the vector components from individual scalar values.
!     !>
!     subroutine set_int_vector_3d(self, x_val, y_val, z_val)
!         implicit none
!         !> The vector object to modify.
!         class(type_int_vector_3d), intent(inout) :: self
!         !> The new x-component value.
!         integer(int32), intent(in) :: x_val
!         !> The new y-component value.
!         integer(int32), intent(in) :: y_val
!         !> The new z-component value.
!         integer(int32), intent(in) :: z_val

!         self%x = x_val
!         self%y = y_val
!         self%z = z_val
!     end subroutine set_int_vector_3d

!     !>
!     !> Sets the vector components from a 3-element array.
!     !>
!     subroutine set_int_vector_3d_array(self, value)
!         implicit none
!         !> The vector object to modify.
!         class(type_int_vector_3d), intent(inout) :: self
!         !> A 3-element array containing the new x, y, and z values.
!         integer(int32), intent(in) :: value(3)

!         self%x = value(1)
!         self%y = value(2)
!         self%z = value(3)
!     end subroutine set_int_vector_3d_array

end module core_types_coordinate

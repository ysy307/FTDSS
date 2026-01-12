!>
!> Defines derived types for handling single 3D coordinates, supporting both
!> double precision real and integer components.
!>
module core_types_coordinate
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    public :: type_coordinate_dp
    public :: type_coordinate_int

    !>
    !> Represents a 3D coordinate with double precision components.
    !>
    type :: type_coordinate_dp
        !> The x-component of the coordinate.
        real(real64) :: x = 0.0d0
        !> The y-component of the coordinate.
        real(real64) :: y = 0.0d0
        !> The z-component of the coordinate.
        real(real64) :: z = 0.0d0
    contains
        procedure, private, pass(self) :: set_coordinate_dp
        procedure, private, pass(self) :: set_coordinate_dp_array
        !> Generic interface to set the coordinate's components.
        generic, public :: set => set_coordinate_dp, set_coordinate_dp_array
        procedure, public, pass(self) :: reset => reset_coordinate_dp
    end type type_coordinate_dp

    !>
    !> Represents a 3D coordinate with integer components.
    !>
    type :: type_coordinate_int
        !> The x-component of the coordinate.
        integer(int32) :: x = 0
        !> The y-component of the coordinate.
        integer(int32) :: y = 0
        !> The z-component of the coordinate.
        integer(int32) :: z = 0
    contains
        procedure, private, pass(self) :: set_coordinate_int
        procedure, private, pass(self) :: set_coordinate_int_array
        !> Generic interface to set the coordinate's components.
        generic, public :: set => set_coordinate_int, set_coordinate_int_array
        procedure, public, pass(self) :: reset => reset_coordinate_int
    end type type_coordinate_int

contains

    ! ==========================================================
    ! Double Precision Coordinate Procedures
    ! ==========================================================

    !>
    !> Sets the coordinate components from individual scalar values.
    !>
    subroutine set_coordinate_dp(self, x_val, y_val, z_val)
        implicit none
        !> The coordinate object to modify.
        class(type_coordinate_dp), intent(inout) :: self
        !> The new x-component value.
        real(real64), intent(in) :: x_val
        !> The new y-component value.
        real(real64), intent(in) :: y_val
        !> The new z-component value.
        real(real64), intent(in) :: z_val

        self%x = x_val
        self%y = y_val
        self%z = z_val
    end subroutine set_coordinate_dp

    !>
    !> Sets the coordinate components from a 3-element array.
    !>
    subroutine set_coordinate_dp_array(self, value)
        implicit none
        !> The coordinate object to modify.
        class(type_coordinate_dp), intent(inout) :: self
        !> A 3-element array containing the new x, y, and z values.
        real(real64), intent(in) :: value(3)

        self%x = value(1)
        self%y = value(2)
        self%z = value(3)
    end subroutine set_coordinate_dp_array

    subroutine reset_coordinate_dp(self)
        implicit none
        class(type_coordinate_dp), intent(inout) :: self

        self%x = 0.0d0
        self%y = 0.0d0
        self%z = 0.0d0
    end subroutine reset_coordinate_dp

    ! ==========================================================
    ! Integer Coordinate Procedures
    ! ==========================================================

    !>
    !> Sets the coordinate components from individual scalar values.
    !>
    subroutine set_coordinate_int(self, x_val, y_val, z_val)
        implicit none
        !> The coordinate object to modify.
        class(type_coordinate_int), intent(inout) :: self
        !> The new x-component value.
        integer(int32), intent(in) :: x_val
        !> The new y-component value.
        integer(int32), intent(in) :: y_val
        !> The new z-component value.
        integer(int32), intent(in) :: z_val

        self%x = x_val
        self%y = y_val
        self%z = z_val
    end subroutine set_coordinate_int

    !>
    !> Sets the coordinate components from a 3-element array.
    !>
    subroutine set_coordinate_int_array(self, value)
        implicit none
        !> The coordinate object to modify.
        class(type_coordinate_int), intent(inout) :: self
        !> A 3-element array containing the new x, y, and z values.
        integer(int32), intent(in) :: value(3)

        self%x = value(1)
        self%y = value(2)
        self%z = value(3)
    end subroutine set_coordinate_int_array

    subroutine reset_coordinate_int(self)
        implicit none
        class(type_coordinate_int), intent(inout) :: self

        self%x = 0
        self%y = 0
        self%z = 0
    end subroutine reset_coordinate_int

end module core_types_coordinate

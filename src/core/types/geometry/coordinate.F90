!>
!> Defines derived types for handling single 3D coordinates, supporting both
!> double precision real and integer components.
!>
module core_types_geometry_coordinate
    use, intrinsic :: iso_fortran_env
    use :: stdlib_optval, only:optval
    use :: stdlib_strings, only:strip
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
        procedure, public, pass(self) :: to_array => to_array_coordinate_dp
        procedure, public, pass(self) :: display => display_coordinate_dp
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
        procedure, public, pass(self) :: to_array => to_array_coordinate_int
        procedure, public, pass(self) :: display => display_coordinate_int
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

    subroutine to_array_coordinate_dp(self, arr)
        implicit none
        class(type_coordinate_dp), intent(in) :: self
        real(real64), intent(inout) :: arr(3)

        arr(1) = self%x
        arr(2) = self%y
        arr(3) = self%z
    end subroutine to_array_coordinate_dp

    subroutine display_coordinate_dp(self, unit_in, label_in)
        implicit none
        class(type_coordinate_dp), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in
        character(len=*), intent(in), optional :: label_in

        integer(int32) :: unit
        character(len=128) :: label

        unit = optval(unit_in, output_unit)
        label = optval(label_in, "Values")

        write (unit, '(2A,F12.6,A,F12.6,A,F12.6,A)') &
            strip(label), " :(", self%x, ", ", self%y, ", ", self%z, ")"

    end subroutine display_coordinate_dp

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

    subroutine to_array_coordinate_int(self, arr)
        implicit none
        class(type_coordinate_int), intent(in) :: self
        integer(int32), intent(inout) :: arr(3)

        arr(1) = self%x
        arr(2) = self%y
        arr(3) = self%z
    end subroutine to_array_coordinate_int

    subroutine display_coordinate_int(self, unit_in, label_in)
        implicit none
        class(type_coordinate_int), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in
        character(len=*), intent(in), optional :: label_in

        integer(int32) :: unit
        character(len=128) :: label

        unit = optval(unit_in, output_unit)
        label = optval(label_in, "Values")

        write (unit, '(2A,I12,A,I12,A,I12,A)') &
            strip(label), " :(", self%x, ", ", self%y, ", ", self%z, ") "
    end subroutine display_coordinate_int

end module core_types_geometry_coordinate

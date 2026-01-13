module core_constants_base
    use, intrinsic :: iso_fortran_env
    use :: stdlib_optval, only:optval
    implicit none
    private

    public :: type_constant_int
    public :: type_constant_dp

    type :: type_constant_int
        character(len=64) :: name
        integer(int32) :: value
        character(len=16) :: unit
    contains
        procedure, public, pass(self) :: set => set_constant_int
        procedure, public, pass(self) :: display => display_constant_int
    end type type_constant_int

    type :: type_constant_dp
        character(len=64) :: name
        real(real64) :: value
        character(len=16) :: unit
    contains
        procedure, public, pass(self) :: set => set_constant_dp
        procedure, public, pass(self) :: display => display_constant_dp
    end type type_constant_dp

contains
    subroutine set_constant_int(self, name_in, value_in, unit_in)
        implicit none
        class(type_constant_int), intent(inout) :: self
        character(*), intent(in), optional :: name_in
        integer(int32), intent(in), optional :: value_in
        character(*), intent(in), optional :: unit_in

        self%name = name_in
        self%value = value_in
        self%unit = unit_in
    end subroutine set_constant_int

    subroutine display_constant_int(self, unit_in)
        implicit none
        class(type_constant_int), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: unit
        unit = optval(unit_in, output_unit)

        write (unit, '(A, ": ", I12, " ", A)') trim(self%name), self%value, trim(self%unit)
    end subroutine display_constant_int

    subroutine set_constant_dp(self, name_in, value_in, unit_in)
        implicit none
        class(type_constant_dp), intent(inout) :: self
        character(*), intent(in), optional :: name_in
        real(real64), intent(in), optional :: value_in
        character(*), intent(in), optional :: unit_in

        self%name = name_in
        self%value = value_in
        self%unit = unit_in
    end subroutine set_constant_dp

    subroutine display_constant_dp(self, unit_in)
        implicit none
        class(type_constant_dp), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: unit
        unit = optval(unit_in, output_unit)

        write (unit, '(A, ": ", ES24.16, " ", A)') trim(self%name), self%value, trim(self%unit)
    end subroutine display_constant_dp

end module core_constants_base

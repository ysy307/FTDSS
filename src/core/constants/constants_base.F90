module core_constants_base
    use, intrinsic :: iso_fortran_env
    use :: stdlib_optval, only:optval
    use :: stdlib_strings, only:strip
    implicit none
    private

    public :: type_constant_id
    public :: type_constant_value

    type :: type_constant_id
        character(len=64) :: group
        character(len=64) :: name
        integer(int32) :: id
    contains
        procedure, public, pass(self) :: display => display_constant_id
        
        procedure, private, pass(self) :: eq_const => eq_const_const_id
        procedure, private, pass(self) :: eq_id => eq_id_const_id
        procedure, private, pass(self) :: eq_name => eq_name_const_id
        generic, public :: operator(==) => eq_const, eq_id, eq_name

        procedure, private, pass(self) :: neq_const => neq_const_const_id
        procedure, private, pass(self) :: neq_id => neq_id_const_id
        procedure, private, pass(self) :: neq_name => neq_name_const_id
        generic, public :: operator(/=) => neq_const, neq_id, neq_name
    end type type_constant_id

    type :: type_constant_value
        character(len=64) :: group
        character(len=64) :: name
        integer(int32) :: id
        character(len=16) :: unit
        real(real64) :: value
    contains
        procedure, public, pass(self) :: display => display_constant

        procedure, private, pass(self) :: eq_const => eq_const_const_value
        procedure, private, pass(self) :: eq_id => eq_id_const_value
        procedure, private, pass(self) :: eq_name => eq_name_const_value
        generic, public :: operator(==) => eq_const, eq_id, eq_name

        procedure, private, pass(self) :: neq_const => neq_const_const_value
        procedure, private, pass(self) :: neq_id => neq_id_const_value
        procedure, private, pass(self) :: neq_name => neq_name_const_value
        generic, public :: operator(/=) => neq_const, neq_id, neq_name
    end type type_constant_value

contains
    subroutine display_constant_id(self, unit_in)
        implicit none
        class(type_constant_id), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: unit
        unit = optval(unit_in, output_unit)

        write (unit, '("[Group: ", a, "]", a, ": ", i6)') strip(self%group), strip(self%name), self%id
    end subroutine display_constant_id

    pure elemental function eq_const_const_id(self, other) result(is_equal)
        implicit none
        class(type_constant_id), intent(in) :: self
        class(type_constant_id), intent(in) :: other
        logical :: is_equal

        is_equal = ((self%id == other%id) .and. &
                    (strip(self%name) == strip(other%name)) .and. &
                    (strip(self%group) == strip(other%group)))
    end function eq_const_const_id

    pure elemental function eq_id_const_id(self, other) result(is_equal)
        implicit none
        class(type_constant_id), intent(in) :: self
        integer(int32), intent(in) :: other
        logical :: is_equal

        is_equal = (self%id == other)
    end function eq_id_const_id

    pure elemental function eq_name_const_id(self, other) result(is_equal)
        implicit none
        class(type_constant_id), intent(in) :: self
        character(len=*), intent(in) :: other
        logical :: is_equal

        is_equal = (strip(self%name) == strip(other))
    end function eq_name_const_id

    pure elemental function neq_const_const_id(self, other) result(is_not_equal)
        implicit none
        class(type_constant_id), intent(in) :: self
        class(type_constant_id), intent(in) :: other
        logical :: is_not_equal

        is_not_equal = .not. self%eq_const(other)
    end function neq_const_const_id

    pure elemental function neq_id_const_id(self, other) result(is_not_equal)
        implicit none
        class(type_constant_id), intent(in) :: self
        integer(int32), intent(in) :: other
        logical :: is_not_equal

        is_not_equal = .not. (self%eq_id(other))
    end function neq_id_const_id

    pure elemental function neq_name_const_id(self, other) result(is_not_equal)
        implicit none
        class(type_constant_id), intent(in) :: self
        character(len=*), intent(in) :: other
        logical :: is_not_equal

        is_not_equal = .not. (self%eq_name(other))
    end function neq_name_const_id

    subroutine display_constant(self, unit_in)
        implicit none
        class(type_constant_value), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: unit
        unit = optval(unit_in, output_unit)

        write (unit, '("[Group: ", a, "]", a, ": ", i6, " ", a, " ", es24.16)') &
            strip(self%group), strip(self%name), self%id, strip(self%unit), self%value
    end subroutine display_constant

    pure elemental function eq_const_const_value(self, other) result(is_equal)
        implicit none
        class(type_constant_value), intent(in) :: self
        class(type_constant_value), intent(in) :: other
        logical :: is_equal

        is_equal = ((self%id == other%id) .and. &
                    (self%value == other%value) .and. &
                    (strip(self%name) == strip(other%name)) .and. &
                    (strip(self%unit) == strip(other%unit)) .and. &
                    (strip(self%group) == strip(other%group)))
    end function eq_const_const_value

    pure elemental function eq_id_const_value(self, other) result(is_equal)
        implicit none
        class(type_constant_value), intent(in) :: self
        integer(int32), intent(in) :: other
        logical :: is_equal

        is_equal = (self%id == other)
    end function eq_id_const_value

    pure elemental function eq_name_const_value(self, other) result(is_equal)
        implicit none
        class(type_constant_value), intent(in) :: self
        character(len=*), intent(in) :: other
        logical :: is_equal

        is_equal = (strip(self%name) == strip(other))
    end function eq_name_const_value

    pure elemental function neq_const_const_value(self, other) result(is_not_equal)
        implicit none
        class(type_constant_value), intent(in) :: self
        class(type_constant_value), intent(in) :: other
        logical :: is_not_equal

        is_not_equal = .not. self%eq_const(other)
    end function neq_const_const_value

    pure elemental function neq_id_const_value(self, other) result(is_not_equal)
        implicit none
        class(type_constant_value), intent(in) :: self
        integer(int32), intent(in) :: other
        logical :: is_not_equal

        is_not_equal = .not. (self%eq_id(other))
    end function neq_id_const_value

    pure elemental function neq_name_const_value(self, other) result(is_not_equal)
        implicit none
        class(type_constant_value), intent(in) :: self
        character(len=*), intent(in) :: other
        logical :: is_not_equal

        is_not_equal = .not. (self%eq_name(other))
    end function neq_name_const_value

end module core_constants_base

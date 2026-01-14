module core_constants_base
    use, intrinsic :: iso_fortran_env
    use :: stdlib_optval, only:optval
    use :: stdlib_strings, only:strip
    implicit none
    private

    public :: type_constant_id
    public :: type_constant_value

    type :: type_constant_id
        character(len=64) :: name
        integer(int32) :: id
    contains
        procedure, public, pass(self) :: display => display_constant_id
        procedure, private, pass(self) :: eq_const_const_id
        procedure, private, pass(self) :: eq_id_const_id
        procedure, private, pass(self) :: eq_name_const_id
        generic, public :: operator(==) => &
            eq_const_const_id, eq_id_const_id, eq_name_const_id
        procedure, private, pass(self) :: ne_const_const_id
        procedure, private, pass(self) :: ne_id_const_id
        procedure, private, pass(self) :: ne_name_const_id
        generic, public :: operator(/=) => &
            ne_const_const_id, ne_id_const_id, ne_name_const_id
    end type type_constant_id

    type :: type_constant_value
        character(len=64) :: name
        integer(int32) :: id
        character(len=16) :: unit
        real(real64) :: value
    contains
        procedure, public, pass(self) :: display => display_constant
        procedure, private, pass(self) :: eq_const_const_value
        procedure, private, pass(self) :: eq_id_const_value
        procedure, private, pass(self) :: eq_name_const_value
        generic, public :: operator(==) => &
            eq_const_const_value, eq_id_const_value, eq_name_const_value

        procedure, private, pass(self) :: ne_const_const_value
        procedure, private, pass(self) :: ne_id_const_value
        procedure, private, pass(self) :: ne_name_const_value
        generic, public :: operator(/=) => &
            ne_const_const_value, ne_id_const_value, ne_name_const_value

    end type type_constant_value

contains
    subroutine display_constant_id(self, unit_in)
        implicit none
        class(type_constant_id), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: unit
        unit = optval(unit_in, output_unit)

        write (unit, '(A, ": ", I12)') strip(self%name), self%id
    end subroutine display_constant_id

    pure elemental function eq_const_const_id(self, other) result(is_equal)
        implicit none
        class(type_constant_id), intent(in) :: self
        class(type_constant_id), intent(in) :: other
        logical :: is_equal

        is_equal = ((self%id == other%id) .and. &
                    (strip(self%name) == strip(other%name)))
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

    pure elemental function ne_const_const_id(self, other) result(is_not_equal)
        implicit none
        class(type_constant_id), intent(in) :: self
        class(type_constant_id), intent(in) :: other
        logical :: is_not_equal

        is_not_equal = .not. ((self%id == other%id) .and. &
                              (strip(self%name) == strip(other%name)))
    end function ne_const_const_id

    pure elemental function ne_id_const_id(self, other) result(is_not_equal)
        implicit none
        class(type_constant_id), intent(in) :: self
        integer(int32), intent(in) :: other
        logical :: is_not_equal

        is_not_equal = .not. (self%id == other)
    end function ne_id_const_id

    pure elemental function ne_name_const_id(self, other) result(is_not_equal)
        implicit none
        class(type_constant_id), intent(in) :: self
        character(len=*), intent(in) :: other
        logical :: is_not_equal

        is_not_equal = .not. (strip(self%name) == strip(other))
    end function ne_name_const_id

    subroutine display_constant(self, unit_in)
        implicit none
        class(type_constant_value), intent(in) :: self
        integer(int32), intent(in), optional :: unit_in

        integer(int32) :: unit
        unit = optval(unit_in, output_unit)

        write (unit, '(A, ": ", I12, " ", A, " ", ES24.16)') trim(self%name), self%id, trim(self%unit), self%value
    end subroutine display_constant

    pure elemental function eq_const_const_value(self, other) result(is_equal)
        implicit none
        class(type_constant_value), intent(in) :: self
        class(type_constant_value), intent(in) :: other
        logical :: is_equal

        is_equal = ((self%id == other%id) .and. &
                    (self%value == other%value) .and. &
                    (strip(self%name) == strip(other%name)) .and. &
                    (strip(self%unit) == strip(other%unit)))
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

    pure elemental function ne_const_const_value(self, other) result(is_not_equal)
        implicit none
        class(type_constant_value), intent(in) :: self
        class(type_constant_value), intent(in) :: other
        logical :: is_not_equal

        is_not_equal = .not. ((self%id == other%id) .and. &
                              (self%value == other%value) .and. &
                              (strip(self%name) == strip(other%name)) .and. &
                              (strip(self%unit) == strip(other%unit)))
    end function ne_const_const_value

    pure elemental function ne_id_const_value(self, other) result(is_not_equal)
        implicit none
        class(type_constant_value), intent(in) :: self
        integer(int32), intent(in) :: other
        logical :: is_not_equal

        is_not_equal = .not. (self%id == other)
    end function ne_id_const_value

    pure elemental function ne_name_const_value(self, other) result(is_not_equal)
        implicit none
        class(type_constant_value), intent(in) :: self
        character(len=*), intent(in) :: other
        logical :: is_not_equal

        is_not_equal = .not. (strip(self%name) == strip(other))
    end function ne_name_const_value

end module core_constants_base

!> Base module for JSON input operations
!>
!> Provides a unified interface for extracting and validating
!> various data types from a parsed JSON structure.
module io_input_base
    use, intrinsic :: iso_fortran_env
    use :: stdlib_strings, only:strip
    use :: stdlib_optval, only:optval
    use :: json_module, only:json_file
    use :: module_core, only:raise_error, ERROR_CODES
    implicit none
    private

    public :: abst_input
    public :: get_json_value

    !> Abstract base type for input handlers
    type, abstract :: abst_input
    end type abst_input

    !> Generic interface to retrieve values from a JSON object
    interface get_json_value
        module procedure :: get_json_integer32
        module procedure :: get_json_real64
        module procedure :: get_json_logical
        module procedure :: get_json_string
        module procedure :: get_json_integer32_array
        module procedure :: get_json_real64_array
        module procedure :: get_json_string_array
    end interface

contains

    ! ==========================================================================
    ! Integer Scalar
    ! ==========================================================================

    !> Retrieve an integer value from a JSON object
    !>
    !> This routine extracts a scalar integer associated with the specified key.
    !> - If the key is missing and `default_value` is provided, the target is set to the default.
    !> - If the key is missing and `is_required` is true, it raises an error.
    !> - If the value is found, it is validated against `valid_range` and `valid_list` (if provided).
    subroutine get_json_integer32(json, key, target_var, found, is_required, &
                                  default_value, valid_range, valid_list)
        implicit none
        !> JSON file object
        class(json_file), intent(inout) :: json
        !> Target key string
        character(len=*), intent(in) :: key
        !> Extracted value
        integer(int32), intent(inout) :: target_var
        !> True if variable was found in JSON
        logical, intent(inout), optional :: found
        !> True if the key is mandatory
        logical, intent(in), optional :: is_required
        !> Fallback value if the key is missing
        integer(int32), intent(in), optional :: default_value
        !> Array of size 2 defining [min, max] allowed values
        integer(int32), intent(in), optional :: valid_range(:)
        !> Array of explicitly allowed values
        integer(int32), intent(in), optional :: valid_list(:)

        logical :: var_found
        logical :: required
        integer(int32) :: i

        required = optval(is_required, .false.)

        call json%get(key, target_var, var_found)
        call json%print_error_message(output_unit)

        if (.not. var_found) then
            if (present(found)) found = var_found
            if (present(default_value)) then
                target_var = default_value
            else if (required) then
                call raise_error(ERROR_CODES%MISSING_REQ_ARG, opt=strip(key))
            end if
        else
            if (present(valid_range)) then
                if (target_var < valid_range(1) .or. target_var > valid_range(2)) then
                    call raise_error(ERROR_CODES%PARAM_RANGE, opt=strip(key))
                end if
            end if
            if (present(valid_list)) then
                if (.not. any(valid_list == target_var)) then
                    call raise_error(ERROR_CODES%PARAM_RANGE, opt=strip(key))
                end if
            end if
        end if
    end subroutine get_json_integer32

    ! ==========================================================================
    ! Real Scalar
    ! ==========================================================================

    !> Retrieve a real value from a JSON object
    !>
    !> This routine extracts a scalar floating-point number associated with the specified key.
    !> - If the key is missing and `default_value` is provided, the target is set to the default.
    !> - If the key is missing and `is_required` is true, it raises an error.
    !> - If the value is found, it is validated against `valid_range` (if provided).
    subroutine get_json_real64(json, key, target_var, found, is_required, &
                               default_value, valid_range)
        implicit none
        !> JSON file object
        class(json_file), intent(inout) :: json
        !> Target key string
        character(len=*), intent(in) :: key
        !> Extracted value
        real(real64), intent(inout) :: target_var
        !> True if variable was found in JSON
        logical, intent(inout), optional :: found
        !> True if the key is mandatory
        logical, intent(in), optional :: is_required
        !> Fallback value if the key is missing
        real(real64), intent(in), optional :: default_value
        !> Array of size 2 defining [min, max] allowed values
        real(real64), intent(in), optional :: valid_range(:)

        logical :: var_found
        logical :: required

        required = optval(is_required, .false.)

        call json%get(key, target_var, var_found)
        call json%print_error_message(output_unit)

        if (.not. var_found) then
            if (present(found)) found = var_found
            if (present(default_value)) then
                target_var = default_value
            else if (required) then
                call raise_error(ERROR_CODES%MISSING_REQ_ARG, opt=strip(key))
            end if
        else
            if (present(valid_range)) then
                if (target_var < valid_range(1) .or. target_var > valid_range(2)) then
                    call raise_error(ERROR_CODES%PARAM_RANGE, opt=strip(key))
                end if
            end if
        end if
    end subroutine get_json_real64

    ! ==========================================================================
    ! Logical Scalar
    ! ==========================================================================

    !> Retrieve a boolean value from a JSON object
    !>
    !> This routine extracts a logical value associated with the specified key.
    !> - If the key is missing and `default_value` is provided, the target is set to the default.
    !> - If the key is missing and `is_required` is true, it raises an error.
    subroutine get_json_logical(json, key, target_var, found, is_required, &
                                default_value)
        implicit none
        !> JSON file object
        class(json_file), intent(inout) :: json
        !> Target key string
        character(len=*), intent(in) :: key
        !> Extracted value
        logical, intent(inout) :: target_var
        !> True if variable was found in JSON
        logical, intent(inout), optional :: found
        !> True if the key is mandatory
        logical, intent(in), optional :: is_required
        !> Fallback value if the key is missing
        logical, intent(in), optional :: default_value

        logical :: var_found
        logical :: required

        required = optval(is_required, .false.)

        call json%get(key, target_var, var_found)
        call json%print_error_message(output_unit)

        if (.not. var_found) then
            if (present(found)) found = var_found
            if (present(default_value)) then
                target_var = default_value
            end if
        end if
    end subroutine get_json_logical

    ! ==========================================================================
    ! String Scalar
    ! ==========================================================================

    !> Retrieve a string value from a JSON object
    !>
    !> This routine extracts a character sequence associated with the specified key.
    !> - If the key is missing and `default_value` is provided, the target is set to the default.
    !> - If the key is missing and `is_required` is true, it raises an error.
    !> - If the value is found, it is validated against `valid_list` (if provided).
    subroutine get_json_string(json, key, target_var, found, is_required, &
                               default_value, valid_list)
        implicit none
        !> JSON file object
        class(json_file), intent(inout) :: json
        !> Target key string
        character(len=*), intent(in) :: key
        !> Extracted string value
        character(len=:), allocatable, intent(inout) :: target_var
        !> True if variable was found in JSON
        logical, intent(inout), optional :: found
        !> True if the key is mandatory
        logical, intent(in), optional :: is_required
        !> Fallback string if the key is missing
        character(len=*), intent(in), optional :: default_value
        !> Array of explicitly allowed strings
        character(len=*), intent(in), optional :: valid_list(:)

        logical :: var_found
        logical :: required
        integer(int32) :: i
        logical :: is_in_list

        required = optval(is_required, .false.)

        call json%get(key, target_var, var_found)
        call json%print_error_message(output_unit)

        if (.not. var_found) then
            if (present(found)) found = var_found
            if (present(default_value)) then
                target_var = default_value
            else if (required) then
                call raise_error(ERROR_CODES%MISSING_REQ_ARG, opt=strip(key))
            end if
        else
            if (present(valid_list)) then
                is_in_list = .false.
                do i = 1, size(valid_list)
                    if (strip(valid_list(i)) == strip(target_var)) then
                        is_in_list = .true.
                        exit
                    end if
                end do
                if (.not. is_in_list) then
                    call raise_error(ERROR_CODES%PARAM_RANGE, opt=strip(key))
                end if
            end if
        end if
    end subroutine get_json_string

    ! ==========================================================================
    ! Integer Array
    ! ==========================================================================

    !> Retrieve an array of integers from a JSON object
    !>
    !> This routine extracts a vector of integers associated with the specified key.
    !> - If the key is missing, it falls back to `default_value` or allocates a zero-sized array.
    !> - Raises an error if `is_required` is true and the key is missing.
    !> - Validates the array size against `array_size` if provided.
    !> - Validates all elements against `valid_range` and `valid_list` if provided.
    subroutine get_json_integer32_array(json, key, target_var, found, is_required, &
                                        default_value, valid_range, valid_list, array_size)
        implicit none
        !> JSON file object
        class(json_file), intent(inout) :: json
        !> Target key string
        character(len=*), intent(in) :: key
        !> Extracted array of values
        integer(int32), allocatable, dimension(:), intent(inout) :: target_var
        !> True if variable was found in JSON
        logical, intent(inout), optional :: found
        !> True if the key is mandatory
        logical, intent(in), optional :: is_required
        !> Fallback array if the key is missing
        integer(int32), intent(in), optional :: default_value(:)
        !> Array of size 2 defining [min, max] allowed values for all elements
        integer(int32), intent(in), optional :: valid_range(:)
        !> Array of explicitly allowed values for all elements
        integer(int32), intent(in), optional :: valid_list(:)
        !> Expected number of elements in the array
        integer(int32), intent(in), optional :: array_size

        logical :: var_found
        logical :: required
        integer(int32) :: i

        required = optval(is_required, .false.)

        call json%get(key, target_var, var_found)
        call json%print_error_message(output_unit)

        if (.not. var_found) then
            if (present(found)) found = var_found
            if (present(default_value)) then
                target_var = default_value
            else if (required) then
                call raise_error(ERROR_CODES%MISSING_REQ_ARG, opt=strip(key))
            else
                if (allocated(target_var)) deallocate (target_var)
                allocate (target_var(0))
            end if
        else
            if (present(array_size)) then
                if (size(target_var) /= array_size) then
                    call raise_error(ERROR_CODES%PARAM_RANGE, opt=strip(key))
                end if
            end if
            if (present(valid_range)) then
                if (any(target_var < valid_range(1)) .or. any(target_var > valid_range(2))) then
                    call raise_error(ERROR_CODES%PARAM_RANGE, opt=strip(key))
                end if
            end if
            if (present(valid_list)) then
                if (.not. all(merge(.true., .false., [(any(valid_list == target_var(i)), i=1, size(target_var))]))) then
                    call raise_error(ERROR_CODES%PARAM_RANGE, opt=strip(key))
                end if
            end if
        end if

    end subroutine get_json_integer32_array

    ! ==========================================================================
    ! Real Array
    ! ==========================================================================

    !> Retrieve an array of real values from a JSON object
    !>
    !> This routine extracts a vector of floating-point numbers associated with the specified key.
    !> - If the key is missing, it falls back to `default_value` or allocates a zero-sized array.
    !> - Raises an error if `is_required` is true and the key is missing.
    !> - Validates the array size against `array_size` if provided.
    !> - Validates all elements against `valid_range` and `valid_list` if provided.
    subroutine get_json_real64_array(json, key, target_var, found, is_required, &
                                     default_value, valid_range, valid_list, array_size)
        implicit none
        !> JSON file object
        class(json_file), intent(inout) :: json
        !> Target key string
        character(len=*), intent(in) :: key
        !> Extracted array of values
        real(real64), allocatable, dimension(:), intent(inout) :: target_var
        !> True if variable was found in JSON
        logical, intent(inout), optional :: found
        !> True if the key is mandatory
        logical, intent(in), optional :: is_required
        !> Fallback array if the key is missing
        real(real64), intent(in), optional :: default_value(:)
        !> Array of size 2 defining [min, max] allowed values for all elements
        real(real64), intent(in), optional :: valid_range(:)
        !> Array of explicitly allowed values for all elements
        real(real64), intent(in), optional :: valid_list(:)
        !> Expected number of elements in the array
        integer(int32), intent(in), optional :: array_size

        logical :: var_found
        logical :: required
        integer(int32) :: i

        required = optval(is_required, .false.)

        call json%get(key, target_var, var_found)
        call json%print_error_message(output_unit)

        if (.not. var_found) then
            if (present(found)) found = var_found
            if (present(default_value)) then
                target_var = default_value
            else if (required) then
                call raise_error(ERROR_CODES%MISSING_REQ_ARG, opt=strip(key))
            else
                if (allocated(target_var)) deallocate (target_var)
                allocate (target_var(0))
            end if
        else
            if (present(array_size)) then
                if (size(target_var) /= array_size) then
                    call raise_error(ERROR_CODES%PARAM_RANGE, opt=strip(key))
                end if
            end if
            if (present(valid_range)) then
                if (any(target_var < valid_range(1)) .or. any(target_var > valid_range(2))) then
                    call raise_error(ERROR_CODES%PARAM_RANGE, opt=strip(key))
                end if
            end if
            if (present(valid_list)) then
                if (.not. all(merge(.true., .false., [(any(valid_list == target_var(i)), i=1, size(target_var))]))) then
                    call raise_error(ERROR_CODES%PARAM_RANGE, opt=strip(key))
                end if
            end if
        end if

    end subroutine get_json_real64_array

    ! ==========================================================================
    ! String Array
    ! ==========================================================================

    !> Retrieve an array of strings from a JSON object
    !>
    !> This routine extracts a vector of character sequences associated with the specified key.
    !> - If the key is missing, it falls back to `default_value` or allocates a zero-sized array.
    !> - Raises an error if `is_required` is true and the key is missing.
    !> - Validates the array size against `array_size` if provided.
    !> - Validates all elements against `valid_list` if provided.
    subroutine get_json_string_array(json, key, target_var, found, is_required, &
                                     default_value, valid_list, array_size)
        implicit none
        !> JSON file object
        class(json_file), intent(inout) :: json
        !> Target key string
        character(len=*), intent(in) :: key
        !> Extracted array of string values
        character(len=:), allocatable, intent(inout) :: target_var(:)
        !> True if variable was found in JSON
        logical, intent(inout), optional :: found
        !> True if the key is mandatory
        logical, intent(in), optional :: is_required
        !> Fallback array of strings if the key is missing
        character(len=*), intent(in), optional :: default_value(:)
        !> Array of explicitly allowed strings for all elements
        character(len=*), intent(in), optional :: valid_list(:)
        !> Expected number of elements in the array
        integer(int32), intent(in), optional :: array_size

        character(len=256), allocatable :: tmp(:)

        logical :: var_found
        logical :: required
        integer(int32) :: i

        required = optval(is_required, .false.)

        call json%get(key, tmp, var_found)
        call json%print_error_message(output_unit)

        if (.not. var_found) then
            if (present(found)) found = var_found
            if (present(default_value)) then
                if (allocated(target_var)) deallocate (target_var)
                allocate (character(len=len(default_value(1))) :: target_var(size(default_value)))
                target_var = default_value
            else if (required) then
                call raise_error(ERROR_CODES%MISSING_REQ_ARG, opt=strip(key))
            else
                if (allocated(target_var)) deallocate (target_var)
                allocate (character(len=0) :: target_var(0))
            end if
        else
            if (allocated(target_var)) deallocate (target_var)
            if (size(tmp) > 0) then
                allocate (character(len=len(tmp(1))) :: target_var(size(tmp)))
                do i = 1, size(tmp)
                    target_var(i) = strip(tmp(i))
                end do
            else
                allocate (character(len=0) :: target_var(0))
            end if

            if (present(array_size)) then
                if (size(target_var) /= array_size) then
                    call raise_error(ERROR_CODES%PARAM_RANGE, opt=strip(key))
                end if
            end if

            if (present(valid_list)) then
                do i = 1, size(target_var)
                    if (.not. any(valid_list == strip(target_var(i)))) then
                        call raise_error(ERROR_CODES%PARAM_RANGE, opt=strip(key))
                    end if
                end do
            end if
        end if

    end subroutine get_json_string_array

end module io_input_base

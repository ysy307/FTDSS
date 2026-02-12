!>
!> Provides utility functions for string manipulation, such as joining, filtering,
!> and converting strings to specific types.
!>
module core_string_utils
    use, intrinsic :: iso_fortran_env, only: int32
    use :: stdlib_strings, only:strip
    use :: core_constants
    use :: core_allocate, only:allocate_array
    implicit none
    private

    public :: join
    public :: filter
    public :: modify_path_format
    public :: get_bc_type_from_string

    interface filter
        module procedure :: filter_character_array
    end interface

contains

    !>
    !> Joins an array of strings into a single string using a delimiter.
    !> Any empty or blank strings in the input array are ignored and excluded
    !> from the result.
    !>
    function join(strings, delimiter) result(key)
        implicit none
        !> An array of strings to join.
        character(*), intent(in) :: strings(:)
        !> The character(s) to place between elements. If not provided or blank, defaults to ".".
        character(*), intent(in), optional :: delimiter
        !> The resulting joined string.
        character(:), allocatable :: key

        integer(int32) :: i, n, total_len, current_pos
        integer(int32) :: length_strings
        character(:), allocatable :: effective_delimiter
        integer(int32) :: length_delimiter
        logical :: is_first_element

        ! Determine the delimiter to use
        if (present(delimiter)) then
            effective_delimiter = strip(delimiter)
            ! Use default "." if the provided delimiter is blank
            if (len(effective_delimiter) == 0) then
                effective_delimiter = "."
            end if
        else
            effective_delimiter = "."
        end if
        length_delimiter = len(effective_delimiter)

        n = size(strings)
        if (n == 0) then
            key = ""
            return
        end if

        ! ==========================================================
        ! Pass 1: Calculate the total length of the final string
        ! ==========================================================
        total_len = 0
        is_first_element = .true.
        do i = 1, n
            length_strings = len_trim(strings(i))
            ! Only process non-empty strings
            if (length_strings > 0) then
                if (is_first_element) then
                    ! For the first valid element, add its length
                    total_len = length_strings
                    is_first_element = .false.
                else
                    ! For subsequent elements, add delimiter length and string length
                    total_len = total_len + length_delimiter + length_strings
                end if
            end if
        end do

        ! Handle case where all input strings were empty
        if (total_len == 0) then
            key = ""
            return
        end if

        ! ==========================================================
        ! Pass 2: Allocate and build the final string
        ! ==========================================================
        allocate (character(len=total_len) :: key)

        current_pos = 1
        is_first_element = .true.
        do i = 1, n
            length_strings = len_trim(strings(i))
            if (length_strings > 0) then
                if (is_first_element) then
                    is_first_element = .false.
                else
                    ! Add the delimiter before the next valid element
                    key(current_pos:current_pos + length_delimiter - 1) = effective_delimiter
                    current_pos = current_pos + length_delimiter
                end if

                ! Write the string content
                key(current_pos:current_pos + length_strings - 1) = strip(strings(i))
                current_pos = current_pos + length_strings
            end if
        end do

    end function join

    !>
    !> Filters a string array, keeping only the elements present in a list of valid strings.
    !>
    subroutine filter_character_array(input_array, valid_list, filtered_array)
        implicit none
        !> The array of strings to be filtered.
        character(*), intent(in) :: input_array(:)
        !> An array containing the valid strings to keep.
        character(*), intent(in) :: valid_list(:)
        !> The output array containing only the valid strings from the input array.
        character(:), allocatable, intent(inout) :: filtered_array(:)

        integer(int32) :: i
        character(64), allocatable :: packed_array(:)
        logical, allocatable :: mask(:)

        if (size(input_array) == 0) then
            if (allocated(filtered_array)) deallocate (filtered_array)
            allocate (character(len=0) :: filtered_array(0))
            return
        end if

        allocate (mask(size(input_array)))

        ! Create a mask by checking if each element of input_array is in valid_list
        mask = .false.
        do i = 1, size(input_array)
            mask(i) = any(valid_list(:) == strip(input_array(i)))
        end do

        ! Use the mask to extract valid elements
        packed_array = pack(input_array, mask)

        ! Copy the result to the output argument, matching its character length
        if (allocated(filtered_array)) deallocate (filtered_array)
        allocate (filtered_array, source=packed_array)

        deallocate (mask, packed_array)

    end subroutine filter_character_array

    !>
    !> Normalizes a file path string.
    !> It replaces all backslashes with forward slashes and ensures the path
    !> ends with a single forward slash if it is not empty.
    !>
    subroutine modify_path_format(path)
        implicit none
        !> The file path string to modify in place.
        character(len=:), allocatable, intent(inout) :: path
        integer(int32) :: i

        ! Replace backslashes with forward slashes
        do i = 1, len(path)
            if (path(i:i) == '\') then
                path(i:i) = '/'
            end if
        end do

        ! Append a slash if the path is not empty and does not already end with one
        if (len_trim(path) > 0 .and. path(len_trim(path):len_trim(path)) /= "/") then
            path = trim(path)//"/"
        end if
    end subroutine modify_path_format

    !>
    !> Converts a boundary condition type string to its corresponding integer ID.
    !>
    pure function get_bc_type_from_string(str, physics_type_id) result(bc_type)
        implicit none
        !> The string representing the BC type (e.g., "dirichlet", "neumann").
        character(*), intent(in) :: str
        !> The integer ID for the physics context (e.g., thermal, hydraulic).
        integer(int32), intent(in) :: physics_type_id
        !> The integer constant for the BC type, or -1 if the string is not recognized.
        integer(int32) :: bc_type

        select case (physics_type_id)
        case (PHYSICS_TYPE_THERMAL)
            select case (strip(str))
            case ("dirichlet")
                bc_type = THERMAL_BC_DIRICHLET
            case ("neumann")
                bc_type = THERMAL_BC_NEUMANN
            case ("flux")
                bc_type = THERMAL_BC_FLUX
            case ("robin")
                bc_type = THERMAL_BC_ROBIN
            case ("convective")
                bc_type = THERMAL_BC_CONVECTIVE
            case ("radiation")
                bc_type = THERMAL_BC_RADIATION
            case ("adiabatic")
                bc_type = THERMAL_BC_ADIABATIC
            case ("free")
                bc_type = THERMAL_BC_FREE
            case default
                bc_type = -1
            end select
        case (PHYSICS_TYPE_HYDRAULIC)
            select case (strip(str))
            case ("dirichlet")
                bc_type = HYDRAULIC_BC_DIRICHLET
            case ("neumann")
                bc_type = HYDRAULIC_BC_NEUMANN
            case ("flux")
                bc_type = HYDRAULIC_BC_FLUX
            case ("impermeable")
                bc_type = HYDRAULIC_BC_IMPERMEABLE
            case ("seepage")
                bc_type = HYDRAULIC_BC_SEEPAGE
            case default
                bc_type = -1
            end select
        end select
    end function get_bc_type_from_string

end module core_string_utils

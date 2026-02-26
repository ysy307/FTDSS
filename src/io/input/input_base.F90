module io_input_base
    use, intrinsic :: iso_fortran_env
    use :: stdlib_strings, only:to_string, strip, ends_with
    use :: json_module, only:json_file
    use :: module_core, only:raise_error, ERROR_CODES
    implicit none
    private

    public :: abst_input
    public :: get_json_value

    type, abstract :: abst_input
    end type abst_input

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

    !----------------------------------------------------------------!
    ! INTEGER版
    !----------------------------------------------------------------!
    subroutine get_json_integer32(json, key, target_var, is_required, default_value, valid_range, valid_list)
        implicit none
        class(json_file), intent(inout) :: json
        character(len=*), intent(in) :: key
        integer(int32), intent(inout) :: target_var

        logical, intent(in), optional :: is_required
        integer(int32), intent(in), optional :: default_value
        ! [Fix] (2) -> (:) に変更して一時配列作成を回避
        integer(int32), intent(in), optional :: valid_range(:)
        integer(int32), intent(in), optional :: valid_list(:)

        logical :: found
        logical :: required = .false.
        integer(int32) :: i

        if (present(is_required)) required = is_required

        call json%get(key, target_var, found)
        call json%print_error_message(output_unit)

        if (.not. found) then
            if (present(default_value)) then
                target_var = default_value
            else if (required) then
                call raise_error(ERROR_CODES%MISSING_REQ_ARG, opt=strip(key))
            end if
        else
            ! 値が見つかった場合のバリデーション
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

    !----------------------------------------------------------------!
    ! REAL版
    !----------------------------------------------------------------!
    subroutine get_json_real64(json, key, target_var, is_required, default_value, valid_range)
        implicit none
        class(json_file), intent(inout) :: json
        character(len=*), intent(in) :: key
        real(real64), intent(inout) :: target_var

        logical, intent(in), optional :: is_required
        real(real64), intent(in), optional :: default_value
        ! [Fix] (2) -> (:) に変更
        real(real64), intent(in), optional :: valid_range(:)

        logical :: found
        logical :: required = .false.

        if (present(is_required)) required = is_required

        call json%get(key, target_var, found)
        call json%print_error_message(output_unit)

        if (.not. found) then
            if (present(default_value)) then
                target_var = default_value
            else if (required) then
                call raise_error(ERROR_CODES%MISSING_REQ_ARG, opt=trim(key))
            end if
        else
            if (present(valid_range)) then
                if (target_var < valid_range(1) .or. target_var > valid_range(2)) then
                    call raise_error(ERROR_CODES%PARAM_RANGE, opt=strip(key))
                end if
            end if
        end if
    end subroutine get_json_real64

    !----------------------------------------------------------------!
    ! LOGICAL版
    !----------------------------------------------------------------!
    subroutine get_json_logical(json, key, target_var, is_required, default_value)
        implicit none
        class(json_file), intent(inout) :: json
        character(len=*), intent(in) :: key
        logical, intent(inout) :: target_var

        logical, intent(in), optional :: is_required
        logical, intent(in), optional :: default_value

        logical :: found
        logical :: required = .false.

        if (present(is_required)) required = is_required

        call json%get(key, target_var, found)
        call json%print_error_message(output_unit)

        if (.not. found .and. present(default_value)) then
            target_var = default_value
        end if
    end subroutine get_json_logical

    !----------------------------------------------------------------!
    ! STRING版
    !----------------------------------------------------------------!
    subroutine get_json_string(json, key, target_var, is_required, default_value, valid_list)
        class(json_file), intent(inout) :: json
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(inout) :: target_var

        logical, intent(in), optional :: is_required
        character(len=*), intent(in), optional :: default_value
        character(len=*), intent(in), optional :: valid_list(:)

        logical :: found
        logical :: required = .false.
        integer(int32) :: i
        logical :: is_in_list

        if (present(is_required)) required = is_required

        call json%get(key, target_var, found)
        call json%print_error_message(output_unit)

        if (.not. found) then
            if (present(default_value)) then
                target_var = default_value
            else if (required) then
                call raise_error(ERROR_CODES%MISSING_REQ_ARG, opt=trim(key))
            end if
        else
            ! 値が見つかった場合のバリデーション
            if (present(valid_list)) then
                is_in_list = .false.
                do i = 1, size(valid_list)
                    if (trim(valid_list(i)) == trim(target_var)) then
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

    !----------------------------------------------------------------!
    ! INTEGER ARRAY版
    !----------------------------------------------------------------!
    subroutine get_json_integer32_array(json, key, target_var, is_required, default_value, valid_range, valid_list, array_size)
        implicit none
        class(json_file), intent(inout) :: json
        character(len=*), intent(in) :: key
        integer(int32), allocatable, dimension(:), intent(inout) :: target_var

        logical, intent(in), optional :: is_required
        integer(int32), intent(in), optional :: default_value(:)
        ! [Fix] (2) -> (:) に変更
        integer(int32), intent(in), optional :: valid_range(:)
        integer(int32), intent(in), optional :: valid_list(:)
        integer(int32), intent(in), optional :: array_size

        logical :: found
        logical :: required = .false.
        integer(int32) :: i

        if (present(is_required)) required = is_required

        call json%get(key, target_var, found)
        call json%print_error_message(output_unit)

        if (.not. found) then
            if (present(default_value)) then
                target_var = default_value
            else if (required) then
                call raise_error(ERROR_CODES%MISSING_REQ_ARG, opt=trim(key))
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

    !----------------------------------------------------------------!
    ! REAL ARRAY版
    !----------------------------------------------------------------!
    subroutine get_json_real64_array(json, key, target_var, is_required, default_value, valid_range, valid_list, array_size)
        implicit none
        class(json_file), intent(inout) :: json
        character(len=*), intent(in) :: key
        real(real64), allocatable, dimension(:), intent(inout) :: target_var

        logical, intent(in), optional :: is_required
        real(real64), intent(in), optional :: default_value(:)
        ! [Fix] (2) -> (:) に変更
        real(real64), intent(in), optional :: valid_range(:)
        real(real64), intent(in), optional :: valid_list(:)
        integer(int32), intent(in), optional :: array_size

        logical :: found
        logical :: required = .false.
        integer(int32) :: i

        if (present(is_required)) required = is_required

        call json%get(key, target_var, found)
        call json%print_error_message(output_unit)

        if (.not. found) then
            if (present(default_value)) then
                target_var = default_value
            else if (required) then
                call raise_error(ERROR_CODES%MISSING_REQ_ARG, opt=trim(key))
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

    !----------------------------------------------------------------!
    ! STRING ARRAY版
    !----------------------------------------------------------------!
    subroutine get_json_string_array(json, key, target_var, is_required, default_value, valid_list, array_size)
        implicit none
        class(json_file), intent(inout) :: json
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(inout) :: target_var(:)

        logical, intent(in), optional :: is_required
        character(len=*), intent(in), optional :: default_value(:)
        character(len=*), intent(in), optional :: valid_list(:)
        integer(int32), intent(in), optional :: array_size

        character(len=256), allocatable :: tmp(:)

        logical :: found
        logical :: required = .false.
        integer(int32) :: i

        if (present(is_required)) required = is_required

        call json%get(key, tmp, found)
        call json%print_error_message(output_unit)

        if (.not. found) then
            if (present(default_value)) then
                if (allocated(target_var)) deallocate (target_var)
                allocate (character(len=len(default_value(1))) :: target_var(size(default_value)))
                target_var = default_value
            else if (required) then
                call raise_error(ERROR_CODES%MISSING_REQ_ARG, opt=trim(key))
            else
                if (allocated(target_var)) deallocate (target_var)
                allocate (character(len=0) :: target_var(0))
            end if
        else
            if (allocated(target_var)) deallocate (target_var)
            if (size(tmp) > 0) then
                allocate (character(len=len(tmp(1))) :: target_var(size(tmp)))
                do i = 1, size(tmp)
                    target_var(i) = trim(tmp(i))
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
                    if (.not. any(valid_list == trim(target_var(i)))) then
                        call raise_error(ERROR_CODES%PARAM_RANGE, opt=strip(key))
                    end if
                end do
            end if
        end if

    end subroutine get_json_string_array

end module io_input_base

!>
!> Module for error handling implementation
!> Uses error constants defined in core_constants_error
!>
module core_validation_error
    use, intrinsic :: iso_fortran_env, only: int32
    use :: stdlib_strings, only:to_string, strip
    use :: core_constants, only:ERROR_CODES, type_constant_error
    implicit none
    private

    public :: raise_error

contains

    !> Raise an error, format the message, and stop execution.
    !>
    !> Usage:
    !>   call raise_error(ERROR_CODES%FILE_MISSING, opt="data.txt", scope="mod:sub", line=__LINE__)
    !>
    pure subroutine raise_error(err, opt, scope, line)
        implicit none
        !> The error constant object (e.g. ERROR_CODES%FILE_MISSING)
        type(type_constant_error), intent(in) :: err
        !> String to replace '{}' in the message template
        character(*), optional, intent(in) :: opt
        !> Scope name (e.g. 'module_name:procedure_name')
        character(*), optional, intent(in) :: scope
        !> Line number where the error occurred (usually __LINE__)
        integer(int32), optional, intent(in) :: line

        character(:), allocatable :: msg_body
        character(2048) :: full_msg
        character(20) :: line_str

        ! 1. Base Message Construction
        if (present(opt)) then
            ! replace_placeholder now handles stripming correctly
            msg_body = replace_placeholder(err%message, opt)
        else
            msg_body = strip(err%message)
        end if

        ! 2. Start constructing the full message
        ! Format: "# 901(INPUT_DIR_MISSING): Message body"
        full_msg = "# "//strip(to_string(err%ID))// &
                   "("//strip(strip(err%name))//"): "//msg_body

        ! 3. Add Scope and Line info
        ! Format: " [scope:line]" or " [scope]" or " [Line:line]"
        if (present(scope) .or. present(line)) then
            full_msg = strip(full_msg)//" ["

            if (present(scope)) then
                full_msg = strip(full_msg)//strip(scope)
                if (present(line)) then
                    full_msg = strip(full_msg)//":"
                end if
            end if

            if (present(line)) then
                if (.not. present(scope)) then
                    full_msg = strip(full_msg)//"Line:"
                end if
                line_str = to_string(line)
                full_msg = strip(full_msg)//strip(line_str)
            end if

            full_msg = strip(full_msg)//"]"
        end if

        ! 4. STOP Execution
        error stop err%ID
        ! error stop strip(full_msg)

    end subroutine raise_error

    ! --------------------------------------------------------------------------
    ! Internal Helpers
    ! --------------------------------------------------------------------------

    !> Simple Pure placeholder replacer for '{}'
    !> Returns an allocated string of the exact required length.
    pure function replace_placeholder(tmpl, val) result(res)
        character(*), intent(in) :: tmpl, val
        character(:), allocatable :: res
        integer(int32) :: idx
        character(*), parameter :: PH = '{}'

        idx = index(tmpl, PH)
        if (idx > 0) then
            ! Replace first occurrence of {}
            ! Critical: strip(tmpl(idx+2:)) prevents including huge trailing spaces from fixed-length strings
            res = tmpl(1:idx - 1)//strip(val)//strip(tmpl(idx + 2:))
        else
            res = strip(tmpl)
        end if
    end function replace_placeholder

end module core_validation_error

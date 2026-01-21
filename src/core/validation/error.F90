!>
!> Module for error handling implementation
!> Uses error constants defined in core_constants_error
!>
module core_error
    use, intrinsic :: iso_fortran_env, only: int32
    use :: stdlib_strings, only: to_string, strip
    use :: core_constants, only: ERROR_CODES, type_constant_error
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

        character(1024) :: msg
        character(256) :: val
        character(20)  :: line_str

        ! 1. Base Message Construction (Handle '{}' replacement)
        ! The message template is stored in err%message
        if (present(opt)) then
            val = strip(opt)
            msg = replace_placeholder(err%message, val)
        else
            msg = strip(err%message)
        end if

        ! 2. Add Scope info if present: "Message [mod:sub]"
        if (present(scope)) then
            msg = trim(msg) // " [" // strip(scope)
            
            if (present(line)) then
                line_str = to_string(line)
                msg = trim(msg) // ":" // trim(line_str)
            end if
            msg = trim(msg) // "]"
        else if (present(line)) then
            ! Line number without scope
            line_str = to_string(line)
            msg = trim(msg) // " [Line:" // trim(line_str) // "]"
        end if

        ! 3. Prepend Error ID and Name:
        ! Format: "# 901(INPUT_DIR_MISSING): Message..."
        msg = "# " // trim(to_string(err%id)) // &
              "(" // trim(strip(err%name)) // "): " // trim(msg)

        ! 4. STOP Execution (Pure safe in F2018+)
        error stop trim(msg)

    end subroutine raise_error

    ! --------------------------------------------------------------------------
    ! Internal Helpers
    ! --------------------------------------------------------------------------
    
    !> Simple Pure placeholder replacer for '{}'
    !> Replaces only the first occurrence.
    pure function replace_placeholder(tmpl, val) result(res)
        character(*), intent(in) :: tmpl, val
        character(len(tmpl)+len(val)) :: res
        integer :: idx

        idx = index(tmpl, '{}')
        if (idx > 0) then
            ! Replace first occurrence of {}
            res = tmpl(1:idx-1) // trim(val) // tmpl(idx+2:)
        else
            res = tmpl
        end if
    end function replace_placeholder

end module core_error
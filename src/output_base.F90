module io_output_base
    use, intrinsic :: iso_fortran_env, only: int32
    use :: stdlib_strings, only:strip
    implicit none
    private

    public :: setup_directory

contains
    subroutine setup_directory(dir_path, file_extensions)
        implicit none
        character(*), intent(in) :: dir_path
        character(*), intent(in) :: file_extensions(:)

        character(512) :: command
        integer(int32) :: i

#ifdef _WIN32
            command = "mkdir "//'"'//strip(dir_path)//'"'
#else
            command = "mkdir -p "//'"'//strip(dir_path)//'"'
#endif
            call execute_safely(strip(command))

            do i = 1, size(file_extensions)
#ifdef _WIN32
                command = "del /Q "//'"'//strip(dir_path)//"*"//strip(file_extensions(i))//'"'
#else
                command = "rm -f "//strip(dir_path)//"*"//strip(file_extensions(i))
#endif
                call execute_safely(strip(command))
            end do
    end subroutine setup_directory

    subroutine execute_safely(command)
        implicit none
        character(*), intent(in) :: command
        integer(int32) :: exit_stat, cmd_stat
        character(256) :: cmd_msg

        exit_stat = 0
        cmd_stat = 0
        cmd_msg = ""

        call execute_command_line(command, wait=.true., exitstat=exit_stat, cmdstat=cmd_stat, cmdmsg=cmd_msg)

        if (cmd_stat /= 0) then
            print *, "OS command execution failed."
            print *, "Status: ", cmd_stat
            print *, "Message: ", strip(cmd_msg)
            print *, "Command: ", strip(command)
            stop 1
        end if

        ! Note: exit_stat represents the return code of the command itself.
        ! Non-zero exit_stat is ignored here because commands like 'del'
        ! may return non-zero when no files are found.
    end subroutine execute_safely

end module io_output_base

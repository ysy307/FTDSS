submodule(Inout_Output) Inout_Output_Base
    implicit none
contains
    !----------------------------------------------------------------------!
    ! Setup_Directory:
    !----------------------------------------------------------------------!
    ! This subroutine ensures that a specified directory exists and is
    ! properly set up for use. If the directory does not exist, it will be
    ! created. If it already exists, all files matching the given file
    ! extensions will be deleted.
    !
    ! Arguments:
    !   dirPath        : Input string specifying the directory path to check
    !                    or create.
    !   fileExtensions : Input array of file extension strings (e.g. ".txt",
    !                    ".dat") used to identify which files to delete if
    !                    the directory exists.
    !
    ! Subroutine Details:
    !   - Uses the `inquire` statement to check if the directory exists.
    !   - On Windows, uses `mkdir` and `del` commands.
    !   - On Linux, uses `mkdir -p` and `rm -f` commands.
    !   - Platform-specific code is selected using preprocessor directives.
    !   - File deletion only occurs if the directory already exists.
    !
    !----------------------------------------------------------------------!
    module subroutine Setup_Directory(dirPath, fileExtensions)
        implicit none
        character(*), intent(in) :: dirPath
        character(*), dimension(:), intent(in) :: fileExtensions

        character(len=512) :: command
        logical :: exists
        integer :: i

        inquire (DIRECTORY=trim(adjustl(dirPath)), exist=exists)

        if (.not. exists) then
#ifdef _WIN32
            command = "mkdir "//'"'//trim(adjustl(dirPath))//'"'
            call system(command)
#endif
#ifdef __linux__
            command = "mkdir -p "//'"'//trim(adjustl(dirPath))//'"'
            call system(command)
#endif
        else
            do i = 1, size(fileExtensions)
#ifdef _WIN32
                command = "del /Q "//'"'//trim(adjustl(dirPath))//"*"//trim(fileExtensions(i))//'"'
                call system(command)
#endif
#ifdef __linux__
                command = "rm -f "//trim(adjustl(dirPath))//"*"//trim(fileExtensions(i))
                call system(command)
#endif
            end do
        end if
    end subroutine Setup_Directory

end submodule Inout_Output_Base

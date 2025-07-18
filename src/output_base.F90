submodule(input_output) input_output_base
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
    !   dir_path        : Input string specifying the directory path to check
    !                    or create.
    !   file_extensions : Input array of file extension strings (e.g. ".txt",
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
    module subroutine setup_directory(dir_path, file_extensions)
        implicit none
        character(*), intent(in) :: dir_path
        character(*), intent(in) :: file_extensions(:)

        character(512) :: command
        logical :: exists
        integer :: i

        inquire (DIRECTORY=trim(adjustl(dir_path)), exist=exists)

        if (.not. exists) then
#ifdef _WIN32
            command = "mkdir "//'"'//trim(adjustl(dir_path))//'"'
            call system(command)
#endif
#ifdef __linux__
            command = "mkdir -p "//'"'//trim(adjustl(dir_path))//'"'
            call system(command)
#endif
        else
            do i = 1, size(file_extensions)
#ifdef _WIN32
                command = "del /Q "//'"'//trim(adjustl(dir_path))//"*"//trim(file_extensions(i))//'"'
                call system(command)
#endif
#ifdef __linux__
                command = "rm -f "//trim(adjustl(dir_path))//"*"//trim(file_extensions(i))
                call system(command)
#endif
            end do
        end if
    end subroutine setup_directory

end submodule input_output_base

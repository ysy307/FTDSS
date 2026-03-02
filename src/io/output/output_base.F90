module io_output_base
    use :: iso_fortran_env, only:int32
    implicit none
    private
    public :: setup_directory
contains
    subroutine setup_directory(dir_path, file_extensions)
        implicit none
        character(*), intent(in) :: dir_path
        character(*), intent(in) :: file_extensions(:)

        character(512) :: command
        logical :: exists
        integer(int32) :: i

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

end module io_output_base

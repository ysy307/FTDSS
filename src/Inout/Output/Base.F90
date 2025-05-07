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

    !----------------------------------------------------------------------!
    ! Get_UserName:
    !----------------------------------------------------------------------!
    ! This function retrieves the username of the user currently running
    ! the program. It checks a list of commonly used environment variables
    ! that may store the username.
    !
    ! Return Value:
    !   UserName : Allocatable character string containing the username of
    !              the current user. Returns "Unknown" if the username
    !              cannot be determined.
    !
    ! Function Details:
    !   - Attempts to retrieve the username from environment variables:
    !     "USER", "LOGNAME", "LNAME", and "USERNAME".
    !   - Uses the intrinsic `get_environment_variable` procedure to query
    !     each variable in order.
    !   - If a valid value is found, it is trimmed and returned.
    !   - If none of the variables are set, the function returns "Unknown".
    !   - Dynamically allocates and deallocates an array of environment
    !     variable names.
    !
    !----------------------------------------------------------------------!
    module function Get_UserName() result(UserName)
        implicit none
        character(:), allocatable :: UserName

        character(64) :: tmpUserName
        integer(int32) :: len, status
        integer(int32) :: i

        character(:), allocatable :: UserNameLists(:)
        integer(int32), parameter :: UserNameListsLength = 4

        allocate (character(len=16) :: UserNameLists(UserNameListsLength))

        UserNameLists(1) = "USER"
        UserNameLists(2) = "LOGNAME"
        UserNameLists(3) = "LNAME"
        UserNameLists(4) = "USERNAME"

        do i = 1, UserNameListsLength
            call get_environment_variable(UserNameLists(i), &
                                          tmpUserName, &
                                          len, &
                                          status)
            if (status == 0 .and. len > 0) then
                UserName = trim(adjustl(tmpUserName))
                deallocate (UserNameLists)
                return
            end if
        end do

        UserName = "Unknown"

        deallocate (UserNameLists)

    end function Get_UserName

    !----------------------------------------------------------------------!
    ! Get_HostName:
    !----------------------------------------------------------------------!
    ! This function retrieves the hostname of the system where the program
    ! is currently running. It attempts to obtain the hostname by querying
    ! environment variables commonly used for this purpose.
    !
    ! Return Value:
    !   HostName : Allocatable character string containing the name of the
    !              host computer. Returns "Unknown" if the hostname cannot
    !              be determined.
    !
    ! Function Details:
    !   - Attempts to retrieve the hostname by checking common environment
    !     variables: "HOSTNAME" and "COMPUTERNAME".
    !   - Uses the Fortran intrinsic procedure `get_environment_variable`
    !     to query the environment.
    !   - If a valid hostname is found, it is returned; otherwise, the
    !     function returns "Unknown".
    !   - Dynamically allocates and deallocates an array used to hold
    !     environment variable names.
    !
    !----------------------------------------------------------------------!
    module function Get_HostName() result(HostName)
        implicit none
        character(:), allocatable :: HostName

        character(64) :: tmpHostName
        integer(int32) :: len, status
        integer(int32) :: i

        character(:), allocatable :: HostNameLists(:)
        integer(int32), parameter :: HostNameListsLength = 2

        allocate (character(len=16) :: HostNameLists(HostNameListsLength))
        HostNameLists(1) = "HOSTNAME"
        HostNameLists(2) = "COMPUTERNAME"

        do i = 1, HostNameListsLength
            call get_environment_variable(HostNameLists(i), &
                                          tmpHostName, &
                                          len, &
                                          status)
            if (status == 0 .and. len > 0) then
                HostName = trim(adjustl(tmpHostName))
                deallocate (HostNameLists)
                return
            end if
        end do

        HostName = "Unknown"

        deallocate (HostNameLists)

    end function Get_HostName

end submodule Inout_Output_Base

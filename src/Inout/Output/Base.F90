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
    ! module function Get_UserName() result(UserName)
    !     implicit none
    !     character(:), allocatable :: UserName

    !     character(64) :: tmpUserName
    !     integer(int32) :: len, status
    !     integer(int32) :: i

    !     character(:), allocatable :: UserNameLists(:)
    !     integer(int32), parameter :: UserNameListsLength = 4

    !     allocate (character(len=16) :: UserNameLists(UserNameListsLength))

    !     UserNameLists(1) = "USER"
    !     UserNameLists(2) = "LOGNAME"
    !     UserNameLists(3) = "LNAME"
    !     UserNameLists(4) = "USERNAME"

    !     do i = 1, UserNameListsLength
    !         call get_environment_variable(UserNameLists(i), &
    !                                       tmpUserName, &
    !                                       len, &
    !                                       status)
    !         if (status == 0 .and. len > 0) then
    !             UserName = trim(adjustl(tmpUserName))
    !             deallocate (UserNameLists)
    !             return
    !         end if
    !     end do

    !     UserName = "Unknown"

    !     deallocate (UserNameLists)

    ! end function Get_UserName

    ! !----------------------------------------------------------------------!
    ! ! Get_HostName:
    ! !----------------------------------------------------------------------!
    ! ! This function retrieves the hostname of the system where the program
    ! ! is currently running. It attempts to obtain the hostname by querying
    ! ! environment variables commonly used for this purpose.
    ! !
    ! ! Return Value:
    ! !   HostName : Allocatable character string containing the name of the
    ! !              host computer. Returns "Unknown" if the hostname cannot
    ! !              be determined.
    ! !
    ! ! Function Details:
    ! !   - Attempts to retrieve the hostname by checking common environment
    ! !     variables: "HOSTNAME" and "COMPUTERNAME".
    ! !   - Uses the Fortran intrinsic procedure `get_environment_variable`
    ! !     to query the environment.
    ! !   - If a valid hostname is found, it is returned; otherwise, the
    ! !     function returns "Unknown".
    ! !   - Dynamically allocates and deallocates an array used to hold
    ! !     environment variable names.
    ! !
    ! !----------------------------------------------------------------------!
    ! module function Get_HostName() result(HostName)
    !     implicit none
    !     character(:), allocatable :: HostName

    !     character(64) :: tmpHostName
    !     integer(int32) :: len, status
    !     integer(int32) :: i

    !     character(:), allocatable :: HostNameLists(:)
    !     integer(int32), parameter :: HostNameListsLength = 2

    !     allocate (character(len=16) :: HostNameLists(HostNameListsLength))
    !     HostNameLists(1) = "HOSTNAME"
    !     HostNameLists(2) = "COMPUTERNAME"

    !     do i = 1, HostNameListsLength
    !         call get_environment_variable(HostNameLists(i), &
    !                                       tmpHostName, &
    !                                       len, &
    !                                       status)
    !         if (status == 0 .and. len > 0) then
    !             HostName = trim(adjustl(tmpHostName))
    !             deallocate (HostNameLists)
    !             return
    !         end if
    !     end do

    !     HostName = "Unknown"

    !     deallocate (HostNameLists)

    ! end function Get_HostName

!     module function Get_CompilerName() result(CompilerName)
!         implicit none
!         character(:), allocatable :: CompilerName

! #ifdef __GFORTRAN__
!         CompilerName = "GNU Fortran Compiler"
! #elif defined(__INTEL_COMPILER)
!         CompilerName = "Intel Fortran Compiler"
! #elif defined(__PGI) || defined(__NVCOMPILER)
!         CompilerName = "NVIDIA (PGI) Fortran Compiler"
! #else
!         CompilerName = "Unknown Compiler"
! #endif

!     end function Get_CompilerName

!     module function Get_CompilerVersion() result(CompilerVersion)
!         use :: stdlib_strings, only:to_string
!         implicit none
!         character(:), allocatable :: CompilerVersion
!         integer(int32) :: year, major, minor

! #ifdef __GFORTRAN__
! #ifdef __GNUC__
!         CompilerVersion = to_string(__GNUC__)//"."//to_string(__GNUC_MINOR__)//"."//to_string(__GNUC_PATCHLEVEL__)
! #else
!         CompilerVersion = "Unknown Compiler Version"
! #endif
! #elif defined(__INTEL_COMPILER)
!         year = __INTEL_COMPILER / 10000
!         major = mod(__INTEL_COMPILER / 100, 100)
!         minor = mod(__INTEL_COMPILER, 100)

!         CompilerVersion = to_string(year)//"."//to_string(major)//"."//to_string(minor)
! #elif defined(__PGI) || defined(__NVCOMPILER)
!         CompilerVersion = to_string(__NVCOMPILER_MAJOR__)//"."//to_string(__NVCOMPILER_MINOR__)//"."//to_string(__NVCOMPILER_PATCHLEVEL__)
! #else
!         CompilerVersion = "Unknown Compiler Version"
! #endif

!     end function Get_CompilerVersion

!     module function Get_CPUArchitecture() result(architecture)
!         implicit none
!         character(:), allocatable :: architecture
!         type(c_ptr) :: ptr

!         ! C 側 get_architecture() を呼び出し
!         ptr = C_Get_Architecture()

!         ! NULL ポインタなら "Unknown"、そうでなければ変換関数を使う
!         if (c_associated(ptr)) then
!             architecture = c_ptr_to_string(ptr)
!         else
!             allocate (character(len=24) :: architecture)
!             architecture = "Unknown CPU Architecture"
!         end if
!     end function Get_CPUArchitecture

!     module function Get_OS() result(os)
!         implicit none
!         character(:), allocatable :: os
!         type(c_ptr) :: ptr

!         ! C 側 get_os() を呼び出し
!         ptr = C_Get_OS()

!         ! NULL ポインタなら "Unknown"、そうでなければ変換関数を使う
!         if (c_associated(ptr)) then
!             os = c_ptr_to_string(ptr)
!         else
!             allocate (character(len=10) :: os)
!             os = "Unknown OS"
!         end if
!     end function Get_OS

!     module function Get_OpneMP_Version() result(OpenMPversion)
!         implicit none
!         character(:), allocatable :: OpenMPversion

! #ifdef _OPENMP
!         select case (_OPENMP)
!         case (199911)
!             OpenMPversion = '1.0'
!         case (200203)
!             OpenMPversion = '2.0'
!         case (200505)
!             OpenMPversion = '2.5'
!         case (200805)
!             OpenMPversion = '3.0'
!         case (201107)
!             OpenMPversion = '3.1'
!         case (201307)
!             OpenMPversion = '4.0'
!         case (201511)
!             OpenMPversion = '4.5'
!         case (201811)
!             OpenMPversion = '5.0'
!         case (202011)
!             OpenMPversion = '5.1'
!         case (202111)
!             OpenMPversion = '5.2'
!         case default
!             OpenMPversion = 'unknown'
!         end select
! #else
!         OpenMPversion = 'not defined'
! #endif

!     end function Get_OpneMP_Version

end submodule Inout_Output_Base

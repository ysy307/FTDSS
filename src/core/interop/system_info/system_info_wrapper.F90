module core_fortran_utils_system_info_wrapper
    use, intrinsic :: iso_fortran_env, only: int32
    use, intrinsic :: iso_c_binding, only: c_ptr, c_associated
    use :: stdlib_strings, only:to_string, strip
    use :: core_c_utils_system_info, only:c_get_os, c_get_cpu_architecture
    use :: core_c_utils, only:c_ptr_to_string
    use :: core_system_env, only:get_env_string
    implicit none

    private
    !----------------------------------------------------------------------!
    ! Public Interface:
    !----------------------------------------------------------------------!
    public :: get_username
    public :: get_hostname
    public :: get_compiler_name
    public :: get_compiler_version
    public :: get_os
    public :: get_cpu_architecture
    public :: get_openmp_version
    !----------------------------------------------------------------------!

contains
    !----------------------------------------------------------------------!
    ! get_username:
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
    function get_username() result(user_name)
        implicit none
        character(:), allocatable :: user_name

        character(:), allocatable :: tmp_user_name
        integer(int32) :: len, status
        integer(int32) :: i

        character(:), allocatable :: user_name_lists(:)
        integer(int32), parameter :: user_name_lists_length = 4

        allocate (character(len=16) :: user_name_lists(user_name_lists_length))

        user_name_lists(1) = "USER"
        user_name_lists(2) = "LOGNAME"
        user_name_lists(3) = "LNAME"
        user_name_lists(4) = "USERNAME"

        do i = 1, user_name_lists_length
            call get_env_string(user_name_lists(i), &
                                tmp_user_name)
            if (allocated(tmp_user_name)) then
                if (len_trim(tmp_user_name) > 0) then
                    user_name = strip(tmp_user_name)
                    deallocate (user_name_lists)
                    return
                end if
            end if
        end do

        user_name = "Unknown"

        deallocate (user_name_lists)

    end function get_username

    !----------------------------------------------------------------------!
    ! get_hostname:
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
    function get_hostname() result(host_name)
        implicit none
        character(:), allocatable :: host_name

        character(:), allocatable :: tmp_host_name
        integer(int32) :: len, status
        integer(int32) :: i

        character(:), allocatable :: host_name_lists(:)
        integer(int32), parameter :: host_name_lists_length = 2

        allocate (character(len=16) :: host_name_lists(host_name_lists_length))
        host_name_lists(1) = "HOSTNAME"
        host_name_lists(2) = "COMPUTERNAME"

        do i = 1, host_name_lists_length
            call get_env_string(host_name_lists(i), &
                                tmp_host_name)
            if (allocated(tmp_host_name)) then
                if (len_trim(tmp_host_name) > 0) then
                    host_name = strip(tmp_host_name)
                    deallocate (host_name_lists)
                    return
                end if
            end if
        end do

        host_name = "Unknown"

        deallocate (host_name_lists)

    end function get_hostname

    function get_compiler_name() result(compiler_name)
        implicit none
        character(:), allocatable :: compiler_name

#ifdef __GFORTRAN__
        compiler_name = "GNU Fortran Compiler"
#elif defined(__INTEL_COMPILER)
        compiler_name = "Intel Fortran Compiler"
#elif defined(__PGI) || defined(__NVCOMPILER)
        compiler_name = "NVIDIA (PGI) Fortran Compiler"
#else
        compiler_name = "Unknown Compiler"
#endif

    end function get_compiler_name

    function get_compiler_version() result(compiler_version)
        implicit none
        character(:), allocatable :: compiler_version
        integer(int32) :: year, major, minor

#ifdef __GFORTRAN__
#ifdef __GNUC__
        compiler_version = to_string(__GNUC__)//"."//to_string(__GNUC_MINOR__)//"."//to_string(__GNUC_PATCHLEVEL__)
#else
        compiler_version = "Unknown Compiler Version"
#endif
#elif defined(__INTEL_COMPILER)
        year = __INTEL_COMPILER / 10000
        major = mod(__INTEL_COMPILER / 100, 100)
        minor = mod(__INTEL_COMPILER, 100)

        compiler_version = to_string(year)//"."//to_string(major)//"."//to_string(minor)
#elif defined(__PGI) || defined(__NVCOMPILER)
        compiler_version = to_string(__NVCOMPILER_MAJOR__)//"."//to_string(__NVCOMPILER_MINOR__)//"."//to_string(__NVCOMPILER_PATCHLEVEL__)
#else
        compiler_version = "Unknown Compiler Version"
#endif

    end function get_compiler_version

    function get_os() result(os)
        implicit none
        character(:), allocatable :: os
        type(c_ptr) :: ptr

        ! C 側 c_get_os() を呼び出し
        ptr = c_get_os()

        ! NULL ポインタなら "Unknown"、そうでなければ変換関数を使う
        if (c_associated(ptr)) then
            os = c_ptr_to_string(ptr)
        else
            allocate (character(len=10) :: os)
            os = "Unknown OS"
        end if
    end function get_os

    function get_cpu_architecture() result(architecture)
        implicit none
        character(:), allocatable :: architecture
        type(c_ptr) :: ptr

        ! C 側 c_get_cpu_architecture() を呼び出し
        ptr = c_get_cpu_architecture()

        ! NULL ポインタなら "Unknown"、そうでなければ変換関数を使う
        if (c_associated(ptr)) then
            architecture = c_ptr_to_string(ptr)
        else
            allocate (character(len=24) :: architecture)
            architecture = "Unknown CPU Architecture"
        end if
    end function get_cpu_architecture

    function get_openmp_version() result(openmp_version)
        implicit none
        character(:), allocatable :: openmp_version

#ifdef _OPENMP
        select case (_OPENMP)
        case (199911)
            openmp_version = '1.0'
        case (200203)
            openmp_version = '2.0'
        case (200505)
            openmp_version = '2.5'
        case (200805)
            openmp_version = '3.0'
        case (201107)
            openmp_version = '3.1'
        case (201307)
            openmp_version = '4.0'
        case (201511)
            openmp_version = '4.5'
        case (201811)
            openmp_version = '5.0'
        case (202011)
            openmp_version = '5.1'
        case (202111)
            openmp_version = '5.2'
        case default
            openmp_version = 'unknown'
        end select
#else
        openmp_version = 'not defined'
#endif

    end function get_openmp_version

end module core_fortran_utils_system_info_wrapper

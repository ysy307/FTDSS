module inout_project_settings
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: module_core, only:error_message
    implicit none
    private

    character(256) :: ProjectPath
    logical :: is_initialize_project_path = .false.

    public :: get_project_path

contains
    subroutine inout_project_path_initialize()
        implicit none
        character(64), parameter :: dName = "ProjectPath.dir"
        integer(int32) :: status, len_path, unit_num
        integer(int32) :: i
        logical :: found

        inquire (file=dName, exist=found)
        if (.not. found) call error_message(900, c_opt=dName)

        open (newunit=unit_num, file=dName, iostat=status, status="old")
        if (status /= 0) call error_message(902, c_opt=dName)

        read (unit_num, '(a)') ProjectPath
        close (unit_num)
        len_path = len_trim(ProjectPath)
        ProjectPath = trim(adjustl(ProjectPath))

        ! For windows, replace "\\" with "/"
        i = index(ProjectPath, "\\")
        do while (i > 0)
            ProjectPath(i:i + 1) = "/"
            if (i + 2 <= len_path) then
                ProjectPath(i + 1:) = ProjectPath(i + 2:)//" "
            end if
            len_path = len_path - 1
            i = index(ProjectPath, "\\")
        end do

        ! For UNIX, replace "\" with "/"
        i = index(ProjectPath, "\")
        do while (i > 0)
            ProjectPath(i:i) = "/"
            len_path = len_trim(ProjectPath)
            i = index(ProjectPath, "\")
        end do

        ! Add "/" to end to path
        if (len_path > 0 .and. ProjectPath(len_path:len_path) /= "/") then
            ProjectPath = trim(adjustl(ProjectPath))//"/"
        end if

        is_initialize_project_path = .true.

    end subroutine inout_project_path_initialize

    function get_project_path() result(project_path)
        implicit none
        character(256) :: project_path

        if (.not. is_initialize_project_path) call inout_project_path_initialize

        project_path = ProjectPath

    end function get_project_path

end module inout_project_settings

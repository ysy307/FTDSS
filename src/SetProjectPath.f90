module Inout_SetProjectPath
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: error
    implicit none
    private
    character(256) :: ProjectPath
    logical :: isSetProjectPath = .false.

    public :: Inout_SetProjectPath_GetProjectPath

contains

    subroutine Inout_SetProjectPath_SetProjectPath
        implicit none
        character(64), parameter :: dName = "ProjectPath.dir"
        integer(int32) :: access, status, len_path, unit_num
        integer :: i

        status = access(dName, "r")
        if (status /= 0) call error_message(901, opt_file_name=dName)

        open (newunit=unit_num, file=dName, iostat=status, status="old")
        if (status /= 0) call error_message(902, opt_file_name=dName)

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

        isSetProjectPath = .true.

    end subroutine Inout_SetProjectPath_SetProjectPath

    character(256) function Inout_SetProjectPath_GetProjectPath()
        implicit none

        if (.not. isSetProjectPath) call Inout_SetProjectPath_SetProjectPath

        Inout_SetProjectPath_GetProjectPath = ProjectPath

    end function Inout_SetProjectPath_GetProjectPath

end module Inout_SetProjectPath

submodule(Inout_Output) Inout_Output_SystemLogger
    use, intrinsic :: iso_fortran_env
    implicit none
contains
    module subroutine Output_SystemLog(self, time)
        implicit none
        class(Type_Output) :: self
        type(Type_Time) :: time
        character(:), allocatable :: username
        character(:), allocatable :: hostname
        character(:), allocatable :: compiler
        character(:), allocatable :: compiler_version
        character(:), allocatable :: architecture
        integer(int32) :: num_unit, ios
        integer(int64) :: rss_kb
        real(real64) :: rss_mb

        username = Get_Username()
        hostname = Get_Hostname()
        compiler = Get_CompilerName()
        compiler_version = Get_CompilerVersion()
        architecture = Get_CPUArchitecture()

        rss_kb = get_rss_kb()
        rss_mb = dble(rss_kb) / 1024.0d0

        open (newunit=num_unit, file=self%logFileName, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write (*, *) "Error opening log file: ", self%logFileName
            stop
        end if
        write (num_unit, '(a)') "----------------------------------------------"
        write (num_unit, '(a)') "FTDSS System Log"
        write (num_unit, '(a)') "----------------------------------------------"
        write (num_unit, '(a)') "Username        : "//trim(username)
        write (num_unit, '(a)') "Hostname        : "//trim(hostname)
        write (num_unit, '(a)') "Compiler        : "//trim(compiler)
        write (num_unit, '(a)') "Compiler Version: "//trim(compiler_version)
        write (num_unit, '(a)') "Architecture    : "//trim(architecture)
        write (num_unit, '(a,f10.4,a)') "RSS Memory Usage: ", rss_mb, " MB"
        write (num_unit, '(a)') "----------------------------------------------"
        write (num_unit, '(a)') "Time Information"
        write (num_unit, '(a)') "----------------------------------------------"
        write (num_unit, '(a)') trim(time%start%label)//" Time: "//time%start%date(1:4)//"-"//time%start%date(5:6)//"-"//time%start%date(7:8)//"T"//time%start%time(1:2)//":"//time%start%time(3:4)//":"//time%start%time(5:6)//trim(time%start%zone)
        write (num_unit, '(a)') trim(time%end%label)//" Time: "//time%end%date(1:4)//"-"//time%end%date(5:6)//"-"//time%end%date(7:8)//"T"//time%end%time(1:2)//":"//time%end%time(3:4)//":"//time%end%time(5:6)//trim(time%end%zone)
        write (num_unit, '(a)') "----------------------------------------------"

        close (num_unit)
    end subroutine Output_SystemLog

end submodule Inout_Output_SystemLogger

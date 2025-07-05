submodule(Inout_Output) Inout_Output_SystemLogger
    use, intrinsic :: iso_fortran_env
    implicit none
contains
    module subroutine Output_SystemLog(self, time, Matrix)
        use :: stdlib_strings, only:to_string
        implicit none
        class(Type_Output) :: self
        type(Type_Time), intent(in) :: time
        type(Type_CRS), intent(in) :: Matrix
        character(:), allocatable :: username
        character(:), allocatable :: hostname
        character(:), allocatable :: compiler
        character(:), allocatable :: compiler_version
        character(:), allocatable :: architecture
        character(:), allocatable :: os
        integer(int32) :: num_unit, ios
        integer(int64) :: rss_kb
        real(real64) :: rss_mb

        integer(int32) :: i
        real(real64) :: total_measured_time

        username = Get_Username()
        hostname = Get_Hostname()
        compiler = Get_CompilerName()
        compiler_version = Get_CompilerVersion()
        architecture = Get_CPUArchitecture()
        os = Get_OS()

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
        write (num_unit, '(a)') "Username           : "//trim(username)
        write (num_unit, '(a)') "Hostname           : "//trim(hostname)
        write (num_unit, '(a)') "OS                 : "//trim(os)
        write (num_unit, '(a)') "Architecture       : "//trim(architecture)
        write (num_unit, '(a)') "Compiler           : "//trim(compiler)
        write (num_unit, '(a)') "Compiler Version   : "//trim(compiler_version)
        write (num_unit, '(a,f'//to_string(int(log10(rss_mb) + 6))//'.4,a)') "RSS Memory Usage   : ", rss_mb, " MB"
#ifdef _OPENMP
        write (num_unit, '(2a)') "OpenMP Version     : ", Get_OpneMP_Version()
        write (num_unit, '(a,i0)') "OpenMP Max Threads : ", omp_get_num_procs()
        write (num_unit, '(a,i0)') "OpenMP Threads     : ", omp_get_max_threads()
#endif
        write (num_unit, '(a)') "----------------------------------------------"
        write (num_unit, '(a)') "Time Information"
        write (num_unit, '(a)') "----------------------------------------------"
        write (num_unit, '(a)') trim(time%start%label)//" Time : "//time%start%date(1:4)//"-"//time%start%date(5:6)//"-"//time%start%date(7:8)//"T"//time%start%time(1:2)//":"//time%start%time(3:4)//":"//time%start%time(5:6)//trim(time%start%zone)
        write (num_unit, '(a)') trim(time%end%label)//" Time   : "//time%end%date(1:4)//"-"//time%end%date(5:6)//"-"//time%end%date(7:8)//"T"//time%end%time(1:2)//":"//time%end%time(3:4)//":"//time%end%time(5:6)//trim(time%end%zone)

        total_measured_time = 0.0d0
        do i = 1, size(time%sections)
            total_measured_time = total_measured_time + time%sections(i)%total_time
        end do

        write (num_unit, *)
        write (num_unit, '(A)') "----------------------------------------------"
        write (num_unit, '(A)') "Performance Profiling Report"
        write (num_unit, '(A)') "----------------------------------------------"
        write (num_unit, '(A20, A15, A12)') "Section", "Time (sec)", "Percentage"
        write (num_unit, '(A47)') repeat('-', 47)

        do i = 1, size(time%sections)
            if (total_measured_time > 0.0d0) then
                write (num_unit, '(A20, F15.4, F11.2, A)') trim(time%sections(i)%label), &
                    time%sections(i)%total_time, &
                    (time%sections(i)%total_time / total_measured_time) * 100.0d0, " %"
            else
                write (num_unit, '(A20, F15.4, A)') trim(time%sections(i)%label), time%sections(i)%total_time, " (N/A %)"
            end if
        end do
        write (num_unit, '(a)') repeat('-', 47)
        write (num_unit, '(a, f15.4, a)') "Total Measured", total_measured_time, ""
        write (num_unit, '(a)') "----------------------------------------------"
        write (num_unit, '(a)') "Matrix Information"
        write (num_unit, '(a)') "----------------------------------------------"
        write (num_unit, '(a)') "Matrix type : CRS"
        write (num_unit, '(a,i0)') "Matrix size : ", Matrix%nrow
        write (num_unit, '(a,i0)') "Matrix nnz  : ", Matrix%nnz

        close (num_unit)
    end subroutine Output_SystemLog

end submodule Inout_Output_SystemLogger

submodule(input_output) input_output_system_logger
    implicit none
contains
    module subroutine output_system_log(self, time, Matrix, domain)
        implicit none
        class(type_output), intent(inout) :: self
        type(type_time), intent(in) :: time
        type(Type_CRS), intent(in) :: Matrix
        type(type_domain), intent(inout) :: domain

        character(:), allocatable :: username
        character(:), allocatable :: hostname
        character(:), allocatable :: compiler
        character(:), allocatable :: compiler_version
        character(:), allocatable :: architecture
        character(:), allocatable :: os
!$      character(:), allocatable :: openmp_version
!$      integer(int32) :: max_threads
!$      integer(int32) :: num_threads
        integer(int32) :: num_unit, ios
        integer(int64) :: rss_kb
        real(real64) :: rss_mb

        integer(int32) :: i
        real(real64) :: component_total_time ! 「Total」を除いた合計時間
        real(real64) :: total_section_time ! 「Total」セクションの時間
        integer(int32), parameter :: nRepeat = 50

        character(len=32) :: fmt
        integer(int32) :: width

        ! 保険として初期化
        fmt = ''

        username = get_username()
        hostname = get_hostname()
        compiler = get_compiler_name()
        compiler_version = get_compiler_version()
        architecture = get_cpu_architecture()
        os = get_os()
!$      openmp_version = get_openmp_version()
!$      max_threads = omp_get_num_procs()
!$      num_threads = omp_get_max_threads()

        rss_mb = get_memory_usage()
        ! 幅の計算。log10(0) の回避と最小幅保証
        width = max(6, int(log10(max(1.0d0, rss_mb))) + 6)
        ! フォーマット文字列の構築
        write (fmt, '(a,i0,a)') '(a,f', width, '.4,a)'

        open (newunit=num_unit, file=self%log_file_name, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write (*, *) "Error opening log file: ", self%log_file_name
            stop
        end if
        write (num_unit, '(a)') repeat('=', nRepeat)
        write (num_unit, '(a)') "FTDSS System Log" !&
        write (num_unit, '(a)') repeat('=', nRepeat)
        write (num_unit, '(a)') "Username           : "//trim(username) !&
        write (num_unit, '(a)') "Hostname           : "//trim(hostname) !&
        write (num_unit, '(a)') "OS                 : "//trim(os) !&
        write (num_unit, '(a)') "Architecture       : "//trim(architecture) !&
        write (num_unit, '(a)') "Compiler           : "//trim(compiler) !&
        write (num_unit, '(a)') "Compiler Version   : "//trim(compiler_version) !&
        write (num_unit, fmt)   "RSS Memory Usage   : ", rss_mb, " MB" !&
!$      write (num_unit, '(a)') "OpenMP Version     : "//trim(openmp_version) !&
!$      write (num_unit, '(a)') "OpenMP Max Threads : "//trim(to_string(max_threads)) !&
!$      write (num_unit, '(a)') "OpenMP Threads     : "//trim(to_string(num_threads)) !&
        write (num_unit, '(a)') repeat('=', nRepeat)
        write (num_unit, '(a)') "Time Information" !&
        write (num_unit, '(a)') repeat('=', nRepeat)
        write (num_unit, '(a)') trim(time%start%label)//" Time : "//time%start%date(1:4)//"-"//time%start%date(5:6)//"-"//time%start%date(7:8)//"T"//time%start%time(1:2)//":"//time%start%time(3:4)//":"//time%start%time(5:6)//trim(time%start%zone)
        write (num_unit, '(a)') trim(time%end%label)//" Time   : "//time%end%date(1:4)//"-"//time%end%date(5:6)//"-"//time%end%date(7:8)//"T"//time%end%time(1:2)//":"//time%end%time(3:4)//":"//time%end%time(5:6)//trim(time%end%zone)

        component_total_time = 0.0d0
        total_section_time = 0.0d0

        ! --- 先に合計を計算 ---
        do i = 1, size(time%sections)
            ! 'Total'セクションは別で保持し、部品の合計からは除外する
            if (trim(adjustl(time%sections(i)%label)) == "Total") then
                total_section_time = time%sections(i)%total_time
            else
                component_total_time = component_total_time + time%sections(i)%total_time
            end if
        end do

        write (num_unit, '(a)') repeat('=', nRepeat)
        write (num_unit, '(a)') "Performance Profiling Report"
        write (num_unit, '(a)') repeat('=', nRepeat)
        write (num_unit, '(a10, a15, a15)') "Section", "Time (sec)", "Percentage"
        write (num_unit, '(a)') repeat('-', nRepeat)

        ! --- 各セクションの結果を出力 ---
        do i = 1, size(time%sections)
            if (trim(adjustl(time%sections(i)%label)) == "Total") cycle

            if (component_total_time > 0.0d0) then
                write (num_unit, '(a10, f15.4, f14.4, a)') trim(time%sections(i)%label), &
                    time%sections(i)%total_time, &
                    (time%sections(i)%total_time / component_total_time) * 100.0d0, " %"
            else
                write (num_unit, '(a10, f15.4, a)') trim(time%sections(i)%label), time%sections(i)%total_time, " (N/A %)"
            end if
        end do

        write (num_unit, '(a)') repeat('-', nRepeat)

        ! 'Total'セクションが計測されていれば、それも表示
        if (total_section_time > 0.0d0) then
            write (num_unit, '(a10, f15.4, a)') "Total", total_section_time, ""
        else
            write (num_unit, '(a10, f15.4, a)') "Total", component_total_time, ""
        end if

        write (num_unit, '(a)') repeat('=', nRepeat)
        write (num_unit, '(a)') "Matrix Information"
        write (num_unit, '(a)') repeat('-', nRepeat)
        write (num_unit, '(a)') "Matrix type : CRS"
        write (num_unit, '(a,i0)') "Matrix size : ", Matrix%num_row
        write (num_unit, '(a,i0)') "Matrix nnz  : ", Matrix%nnz
        write (num_unit, '(a)') repeat('-', nRepeat)
        write (num_unit, '(a,i0)') "Coloring Count : ", Domain%Colors%num_colors
        write (num_unit, '(a)') repeat('=', nRepeat)

        close (num_unit)
    end subroutine output_system_log

end submodule input_output_system_logger

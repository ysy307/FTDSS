submodule(inout_output) inout_output_system_logger
    implicit none

contains

    module subroutine output_system_log(self, control, matrix, domain)
        implicit none
        class(type_output), intent(inout) :: self
        type(type_controls), intent(in) :: control
        class(abst_matrix), intent(in) :: matrix
        type(type_domain), intent(inout) :: domain

        ! システム情報用
        character(:), allocatable :: username
        character(:), allocatable :: hostname
        character(:), allocatable :: compiler
        character(:), allocatable :: compiler_version
        character(:), allocatable :: architecture
        character(:), allocatable :: os_name

        integer(int32) :: num_unit, ios
        integer(int32) :: width
        real(real64) :: rss_mb
        character(len=32) :: fmt
        integer(int32), parameter :: n_repeat = 50

        ! 既存メソッドから受け取るための文字列バッファ
        character(:), allocatable :: time_record_str

        ! --- 初期化 ---
        fmt = ''

        ! --- システム情報の取得 ---
        username = get_username()
        hostname = get_hostname()
        compiler = get_compiler_name()
        compiler_version = get_compiler_version()
        architecture = get_cpu_architecture()
        os_name = get_os()
        rss_mb = get_memory_usage()

        ! --- フォーマットの動的生成 ---
        if (rss_mb > 0.0d0) then
            width = max(6, int(log10(rss_mb)) + 6)
        else
            width = 6
        end if
        write (fmt, '(a,i0,a)') '(a,f', width, '.4,a)'

        ! --- ログファイルのオープン ---
        open (newunit=num_unit, file=self%log_file_name, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            print *, "Error: Cannot open log file: ", self%log_file_name
            return
        end if

        ! --- ヘッダー出力 ---
        write (num_unit, '(a)') repeat('=', n_repeat)
        write (num_unit, '(a)') "FTDSS System Log"
        write (num_unit, '(a)') repeat('=', n_repeat)
        write (num_unit, '(a)') "Username           : "//trim(username)
        write (num_unit, '(a)') "Hostname           : "//trim(hostname)
        write (num_unit, '(a)') "OS                 : "//trim(os_name)
        write (num_unit, '(a)') "Architecture       : "//trim(architecture)
        write (num_unit, '(a)') "Compiler           : "//trim(compiler)
        write (num_unit, '(a)') "Compiler Version   : "//trim(compiler_version)
#ifdef _OPENMP
        write (num_unit, '(a, i0)') "OpenMP Threads     : ", omp_get_max_threads()
#else
        write (num_unit, '(a)') "OpenMP Threads     : 1 (Serial)"
#endif
        write (num_unit, fmt) "RSS Memory Usage   : ", rss_mb, " MB"
        write (num_unit, '(a)') repeat('=', n_repeat)

        ! --- 時間情報出力 ---
        write (num_unit, '(a)') "Time Information"
        write (num_unit, '(a)') repeat('=', n_repeat)

        ! Start Time
        call control%profiler%get_record(TIME_RECORD_START, time_record_str)
        if (allocated(time_record_str)) then
            write (num_unit, '(a)') time_record_str
        end if

        ! End Time
        call control%profiler%get_record(TIME_RECORD_END, time_record_str)
        if (allocated(time_record_str)) then
            write (num_unit, '(a)') time_record_str
        end if

        ! --- プロファイリング集計出力 ---
        write (num_unit, '(a)') repeat('=', n_repeat)
        write (num_unit, '(a)') "Performance Profiling Report"
        write (num_unit, '(a)') repeat('=', n_repeat)

        call control%profiler%display(unit=num_unit)

        ! 行列・ドメイン情報の出力は指示により削除

        close (num_unit)

    end subroutine output_system_log

end submodule inout_output_system_logger

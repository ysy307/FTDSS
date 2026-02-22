submodule(inout_output) inout_output_system_logger
    implicit none

contains

    module subroutine output_system_log(self, start_time_str, end_time_str, &
                                        sec_labels, sec_total_times, sec_call_counts, sec_percentages)
        implicit none
        class(type_output), intent(inout) :: self
        ! Receive primitive data instead of control object
        character(*), intent(in) :: start_time_str
        character(*), intent(in) :: end_time_str
        character(*), intent(in) :: sec_labels(:)
        real(real64), intent(in) :: sec_total_times(:)
        integer(int32), intent(in) :: sec_call_counts(:)
        real(real64), intent(in) :: sec_percentages(:)

        ! For system information
        character(:), allocatable :: username
        character(:), allocatable :: hostname
        character(:), allocatable :: compiler
        character(:), allocatable :: compiler_version
        character(:), allocatable :: architecture
        character(:), allocatable :: os_name

        integer(int32) :: num_unit, ios, i
        integer(int32) :: width
        real(real64) :: rss_mb
        character(len=32) :: fmt

        ! --- Initialization ---
        fmt = ''

        ! --- Get system information ---
        username = get_username()
        hostname = get_hostname()
        compiler = get_compiler_name()
        compiler_version = get_compiler_version()
        architecture = get_cpu_architecture()
        os_name = get_os()
        rss_mb = get_memory_usage()

        ! --- Dynamic generation of format ---
        if (rss_mb > 0.0d0) then
            width = max(6, int(log10(rss_mb)) + 6)
        else
            width = 6
        end if
        ! Generate format string like: (a,f10.4,a) for the memory usage line
        write (fmt, '(a,i0,a)') '(a,f', width, '.4,a)'

        ! --- Open log file ---
        open (newunit=num_unit, file=self%log_file_name, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            call raise_error(ERROR_CODES%OPEN_FILE_FAILED, opt=self%log_file_name)
        end if

        ! --- Output Header (Markdown) ---
        write (num_unit, '(a)') "# FTDSS System Log"
        write (num_unit, '(a)') ""

        ! --- System Information (Markdown List) ---
        write (num_unit, '(a)') "## System Information"
        write (num_unit, '(a)') ""
        write (num_unit, '(a)') "- **Username**: "//strip(username)
        write (num_unit, '(a)') "- **Hostname**: "//strip(hostname)
        write (num_unit, '(a)') "- **OS**: "//strip(os_name)
        write (num_unit, '(a)') "- **Architecture**: "//strip(architecture)
        write (num_unit, '(a)') "- **Compiler**: "//strip(compiler)
        write (num_unit, '(a)') "- **Compiler Version**: "//strip(compiler_version)
#ifdef _OPENMP
        write (num_unit, '(a, i0)') "- **Number of Processors**: ", omp_get_num_procs()
        write (num_unit, '(a, i0)') "- **OpenMP Threads**: ", omp_get_max_threads()
#else
        write (num_unit, '(a)') "- **OpenMP Threads**: 1 (Serial)"
#endif
        ! Output RSS Memory Usage using the dynamically generated format
        write (num_unit, fmt) "- **RSS Memory Usage**: ", rss_mb, " MB"
        write (num_unit, '(a)') ""

        ! --- Time Information (Markdown List) ---
        write (num_unit, '(a)') "## Time Information"
        write (num_unit, '(a)') ""

        if (len_trim(start_time_str) > 0) then
            write (num_unit, '(a)') "- **Start Time**: "//strip(start_time_str)
        else
            write (num_unit, '(a)') "- **Start Time**: (Not recorded)"
        end if

        if (len_trim(end_time_str) > 0) then
            write (num_unit, '(a)') "- **End Time**: "//strip(end_time_str)
        else
            write (num_unit, '(a)') "- **End Time**: (Not recorded)"
        end if
        write (num_unit, '(a)') ""

        ! --- Profiling Report (Markdown Table) ---
        write (num_unit, '(a)') "## Performance Profiling Report"
        write (num_unit, '(a)') ""

        if (size(sec_labels) > 0) then
            ! Table Header
            write (num_unit, '(a)') "| Section            | Time (s)   | Calls | Percentage |"
            write (num_unit, '(a)') "|:-------------------|:----------:|:-----:|:----------:|"

            ! Table Body
            do i = 1, size(sec_labels)
                write (num_unit, '("|", a20, "| ", es10.3, " | ", i5, " |   ", f6.1, " % |")') &
                    sec_labels(i), sec_total_times(i), sec_call_counts(i), sec_percentages(i)
            end do
        else
            write (num_unit, '(a)') "(No sections recorded)"
        end if

        write (num_unit, '(a)') ""

        close (num_unit)

    end subroutine output_system_log

end submodule inout_output_system_logger

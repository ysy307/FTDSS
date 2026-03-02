submodule(io_output_logging) output_logging_system_logger
    implicit none

contains

    module subroutine initialize_type_output_log(self, dir_output)
        implicit none
        class(type_output_log), intent(inout) :: self
        character(*), intent(in) :: dir_output

        integer(int32) :: iostat

        self%log_file_name = strip(dir_output)//"run.log"
        self%io_unit = open (self%log_file_name, "wt", iostat=iostat)
        if (iostat /= 0) then
            call raise_error(ERROR_CODES%OPEN_FILE_FAILED, opt=self%log_file_name)
        end if

        self%initialized = .true.
    end subroutine initialize_type_output_log

    module subroutine destroy_type_output_log(self, dir_output)
        implicit none
        class(type_output_log), intent(inout) :: self
        character(*), intent(in) :: dir_output

        integer(int32) :: iostat

        self%log_file_name = ""
        if (self%io_unit > 0) then
            close (self%io_unit, iostat=iostat)
            if (iostat /= 0) then
                call raise_error(ERROR_CODES%CLOSE_FILE_FAILED, opt=to_string(self%io_unit))
            end if
            self%io_unit = -1
        end if

        self%initialized = .false.
    end subroutine destroy_type_output_log

    module subroutine output_system_log_type_output_log(self)
        implicit none
        class(type_output_log), intent(in) :: self

        ! For system information
        character(:), allocatable :: username
        character(:), allocatable :: hostname
        character(:), allocatable :: compiler
        character(:), allocatable :: compiler_version
        character(:), allocatable :: architecture
        character(:), allocatable :: os_name

        integer(int32) :: width
        real(real64) :: rss_mb
        character(len=32) :: fmt

        ! if (.not. self%initialized) then
        !     call raise_error(ERROR_CODES%OUTPUT_NOT_INITIALIZED)
        ! end if

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

        ! --- Output Header (Markdown) ---
        write (self%io_unit, '(a)') "# FTDSS System Log"
        write (self%io_unit, '(a)') ""

        ! --- System Information (Markdown List) ---
        write (self%io_unit, '(a)') "## System Information"
        write (self%io_unit, '(a)') ""
        write (self%io_unit, '(a)') "- **Username**: "//strip(username)
        write (self%io_unit, '(a)') "- **Hostname**: "//strip(hostname)
        write (self%io_unit, '(a)') "- **OS**: "//strip(os_name)
        write (self%io_unit, '(a)') "- **Architecture**: "//strip(architecture)
        write (self%io_unit, '(a)') "- **Compiler**: "//strip(compiler)
        write (self%io_unit, '(a)') "- **Compiler Version**: "//strip(compiler_version)
#ifdef _OPENMP
        write (self%io_unit, '(a, i0)') "- **Number of Processors**: ", omp_get_num_procs()
        write (self%io_unit, '(a, i0)') "- **OpenMP Threads**: ", omp_get_max_threads()
#else
        write (self%io_unit, '(a)') "- **OpenMP Threads**: 1 (Serial)"
#endif
        ! Output RSS Memory Usage using the dynamically generated format
        write (self%io_unit, fmt) "- **RSS Memory Usage**: ", rss_mb, " MB"
        write (self%io_unit, '(a)') ""

    end subroutine output_system_log_type_output_log

    module subroutine get_log_io_unit_type_output_log(self, io_unit)
        implicit none
        class(type_output_log), intent(in) :: self
        integer(int32), intent(inout) :: io_unit

        ! if (.not. self%initialized) then
        !     call raise_error(ERROR_CODES%OUTPUT_NOT_INITIALIZED)
        ! end if

        io_unit = self%io_unit

    end subroutine get_log_io_unit_type_output_log

end submodule output_logging_system_logger

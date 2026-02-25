submodule(inout_output) inout_output_system_logger
    implicit none

contains

    module subroutine initialize_type_output_log(self, dir_output)
        implicit none
        class(type_output_log), intent(inout) :: self
        character(*), intent(in) :: dir_output

        integer(int32) :: iostat

        self%log_file_name = strip(self%dir_output)//"run.log"
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

        if (.not. self%initialized) then
            call raise_error(ERROR_CODES%OUTPUT_NOT_INITIALIZED)
        end if

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
        write (self%io_unit,'(a)') "# FTDSS System Log"
        write (self%io_unit,'(a)') ""

        ! --- System Information (Markdown List) ---
        write (self%io_unit,'(a)') "## System Information"
        write (self%io_unit,'(a)') ""
        write (self%io_unit,'(a)') "- **Username**: "//strip(username)
        write (self%io_unit,'(a)') "- **Hostname**: "//strip(hostname)
        write (self%io_unit,'(a)') "- **OS**: "//strip(os_name)
        write (self%io_unit,'(a)') "- **Architecture**: "//strip(architecture)
        write (self%io_unit,'(a)') "- **Compiler**: "//strip(compiler)
        write (self%io_unit,'(a)') "- **Compiler Version**: "//strip(compiler_version)
#ifdef _OPENMP
        write (self%io_unit,'(a, i0)') "- **Number of Processors**: ", omp_get_num_procs()
        write (self%io_unit,'(a, i0)') "- **OpenMP Threads**: ", omp_get_max_threads()
#else
        write (self%io_unit,'(a)') "- **OpenMP Threads**: 1 (Serial)"
#endif
        ! Output RSS Memory Usage using the dynamically generated format
        write (self%io_unit,fmt) "- **RSS Memory Usage**: ", rss_mb, " MB"
        write (self%io_unit,'(a)') ""

    end subroutine output_system_log_type_output_log

!     module subroutine output_system_log(self, start_time_str, end_time_str, &
!                                         sec_labels, sec_total_times, sec_call_counts, sec_percentages)
!         implicit none
!         class(type_output), intent(inout) :: self
!         ! ! Receive primitive data instead of control object
!         ! character(*), intent(in) :: start_time_str
!         ! character(*), intent(in) :: end_time_str
!         ! character(*), intent(in) :: sec_labels(:)
!         ! real(real64), intent(in) :: sec_total_times(:)
!         ! integer(int32), intent(in) :: sec_call_counts(:)
!         ! real(real64), intent(in) :: sec_percentages(:)

!         ! For system information
!         character(:), allocatable :: username
!         character(:), allocatable :: hostname
!         character(:), allocatable :: compiler
!         character(:), allocatable :: compiler_version
!         character(:), allocatable :: architecture
!         character(:), allocatable :: os_name

!         integer(int32) :: ios, i
!         integer(int32) :: width
!         real(real64) :: rss_mb
!         character(len=32) :: fmt

!         ! --- Initialization ---
!         fmt = ''

!         ! --- Get system information ---
!         username = get_username()
!         hostname = get_hostname()
!         compiler = get_compiler_name()
!         compiler_version = get_compiler_version()
!         architecture = get_cpu_architecture()
!         os_name = get_os()
!         rss_mb = get_memory_usage()

!         ! --- Dynamic generation of format ---
!         if (rss_mb > 0.0d0) then
!             width = max(6, int(log10(rss_mb)) + 6)
!         else
!             width = 6
!         end if
!         ! Generate format string like: (a,f10.4,a) for the memory usage line
!         write (fmt, '(a,i0,a)') '(a,f', width, '.4,a)'

!         ! --- Output Header (Markdown) ---
!         write (self%io_unit,'(a)') "# FTDSS System Log"
!         write (self%io_unit,'(a)') ""

!         ! --- System Information (Markdown List) ---
!         write (self%io_unit,'(a)') "## System Information"
!         write (self%io_unit,'(a)') ""
!         write (self%io_unit,'(a)') "- **Username**: "//strip(username)
!         write (self%io_unit,'(a)') "- **Hostname**: "//strip(hostname)
!         write (self%io_unit,'(a)') "- **OS**: "//strip(os_name)
!         write (self%io_unit,'(a)') "- **Architecture**: "//strip(architecture)
!         write (self%io_unit,'(a)') "- **Compiler**: "//strip(compiler)
!         write (self%io_unit,'(a)') "- **Compiler Version**: "//strip(compiler_version)
! #ifdef _OPENMP
!         write (self%io_unit,'(a, i0)') "- **Number of Processors**: ", omp_get_num_procs()
!         write (self%io_unit,'(a, i0)') "- **OpenMP Threads**: ", omp_get_max_threads()
! #else
!         write (self%io_unit,'(a)') "- **OpenMP Threads**: 1 (Serial)"
! #endif
!         ! Output RSS Memory Usage using the dynamically generated format
!         write (self%io_unit,fmt) "- **RSS Memory Usage**: ", rss_mb, " MB"
!         write (self%io_unit,'(a)') ""

!         ! --- Time Information (Markdown List) ---
!         write (self%io_unit,'(a)') "## Time Information"
!         write (self%io_unit,'(a)') ""

!         if (len_trim(start_time_str) > 0) then
!             write (self%io_unit,'(a)') "- **Start Time**: "//strip(start_time_str)
!         else
!             write (self%io_unit,'(a)') "- **Start Time**: (Not recorded)"
!         end if

!         if (len_trim(end_time_str) > 0) then
!             write (self%io_unit,'(a)') "- **End Time**: "//strip(end_time_str)
!         else
!             write (self%io_unit,'(a)') "- **End Time**: (Not recorded)"
!         end if
!         write (self%io_unit,'(a)') ""

!         ! --- Profiling Report (Markdown Table) ---
!         write (self%io_unit,'(a)') "## Performance Profiling Report"
!         write (self%io_unit,'(a)') ""

!         if (size(sec_labels) > 0) then
!             ! Table Header
!             write (self%io_unit,'(a)') "| Section            | Time (s)   | Calls | Percentage |"
!             write (self%io_unit,'(a)') "|:-------------------|:----------:|:-----:|:----------:|"

!             ! Table Body
!             do i = 1, size(sec_labels)
!                 write (self%io_unit,'("|", a20, "| ", es10.3, " | ", i5, " |   ", f6.1, " % |")') &
!                     sec_labels(i), sec_total_times(i), sec_call_counts(i), sec_percentages(i)
!             end do
!         else
!             write (self%io_unit,'(a)') "(No sections recorded)"
!         end if

!         write (self%io_unit,'(a)') ""

!         ! close (num_unit)

!     end subroutine output_system_log

end submodule inout_output_system_logger

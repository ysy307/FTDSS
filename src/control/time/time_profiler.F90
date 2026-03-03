module control_time_profiler
    use, intrinsic :: iso_fortran_env, only: int32, real64, output_unit
#ifdef _OPENMP
    use :: omp_lib
#endif
    use :: stdlib_strings, only:strip, to_string
    use :: module_core
    implicit none
    private

    public :: type_time_profiler

    !> Abstract base type for profiling and time records
    type, abstract :: abst_record
        !> Identifier for the record
        type(type_constant_id) :: label = type_constant_id("", "", -1)
    contains
        procedure(abst_get_log_entry), public, pass(self), deferred :: get_log_entry
    end type abst_record

    abstract interface
        !> Retrieve a formatted log entry string
        !>
        !> Mathematical definition:
        !> - Generates a string representation of the record
        !>
        !> Assumptions:
        !> - None
        !>
        !> Numerical guarantee:
        !> - No theoretical error bound available
        !>
        !> Computational complexity:
        !> - Memory: \(O(1)\)
        !> - Arithmetic: \(O(1)\)
        !>
        !> Failure behavior:
        !> - Returns without error
        subroutine abst_get_log_entry(self, log_entry)
            import :: abst_record
            implicit none
            !> Record object

            class(abst_record), intent(in) :: self
            !> String to hold the formatted log entry
            !> Overwritten on exit
            character(:), intent(inout), allocatable :: log_entry
        end subroutine abst_get_log_entry
    end interface

    !> Record type for tracking execution time intervals
    type, extends(abst_record) :: type_record_profiler
        !> Flag indicating if the profiler is currently running
        logical :: running = .false.
        !> Accumulated total execution time in seconds
        real(real64) :: total_time = 0.0d0
        !> Start time of the current execution lap in seconds
        real(real64) :: start_time = 0.0d0
        !> Number of times the profiler has been called
        integer(int32) :: num_calls = 0
    contains
        procedure, public, pass(self) :: get_log_entry => get_log_entry_profiler
    end type type_record_profiler

    !> Record type for tracking absolute timestamps
    type, extends(abst_record) :: type_record_time
        !> Date string representation
        character(10) :: date = ""
        !> Time string representation
        character(10) :: time = ""
        !> Time zone representation
        character(10) :: zone = ""
    contains
        procedure, public, pass(self) :: get_timestamp => get_timestamp_time_record
        procedure, public, pass(self) :: get_log_entry => get_log_entry_time_record
    end type type_record_time

    !> Profiler management class
    type :: type_time_profiler
        !> Array of execution time profilers
        type(type_record_profiler), private, allocatable :: timers(:)
        !> Array of absolute timestamp records
        type(type_record_time), private, allocatable :: timestamps(:)
    contains
        procedure, public, pass(self) :: initialize => initialize_type_time_profiler
        procedure, public, pass(self) :: destroy => destroy_type_time_profiler
        procedure, public, pass(self) :: record => record_time
        procedure, public, pass(self) :: start => start_profile
        procedure, public, pass(self) :: stop => stop_profile
        procedure, private, pass(self) :: get_current_wall_time
        procedure, public, pass(self) :: display => display_profiler
    end type type_time_profiler

contains

    !> Format the timestamp record into a standardized string
    !>
    !> Mathematical definition:
    !> - Concatenates date, time, and zone into ISO 8601-like format
    !>
    !> Assumptions:
    !> - Date, time, and zone fields are populated
    !>
    !> Numerical guarantee:
    !> - No theoretical error bound available
    !>
    !> Computational complexity:
    !> - Memory: \(O(1)\)
    !> - Arithmetic: \(O(1)\)
    !>
    !> Failure behavior:
    !> - Returns without error
    subroutine get_timestamp_time_record(self, timestamp_str)
        implicit none
        !> Timestamp record object
        class(type_record_time), intent(in) :: self
        !> Formatted timestamp string
        !> Overwritten on exit
        character(:), intent(inout), allocatable :: timestamp_str

        timestamp_str = &
            self%date(1:4)//"-"//self%date(5:6)//"-"//self%date(7:8)//"T"// &
            self%time(1:2)//":"//self%time(3:4)//":"//self%time(5:6)//strip(self%zone)
    end subroutine get_timestamp_time_record

    !> Retrieve a formatted log entry for the timestamp record
    !>
    !> Mathematical definition:
    !> - Appends the label name to the formatted timestamp string
    !>
    !> Assumptions:
    !> - None
    !>
    !> Numerical guarantee:
    !> - No theoretical error bound available
    !>
    !> Computational complexity:
    !> - Memory: \(O(1)\)
    !> - Arithmetic: \(O(1)\)
    !>
    !> Failure behavior:
    !> - Returns without error
    subroutine get_log_entry_time_record(self, log_entry)
        implicit none
        !> Timestamp record object
        class(type_record_time), intent(in) :: self
        !> String to hold the formatted log entry
        !> Overwritten on exit
        character(:), intent(inout), allocatable :: log_entry

        character(:), allocatable :: time_stamp

        call self%get_timestamp(time_stamp)
        log_entry = strip(self%label%NAME)//" Time : "//time_stamp
    end subroutine get_log_entry_time_record

    !> Retrieve a formatted log entry for the profiler record
    !>
    !> Mathematical definition:
    !> - Converts accumulated time and call counts to a string
    !>
    !> Assumptions:
    !> - None
    !>
    !> Numerical guarantee:
    !> - No theoretical error bound available
    !>
    !> Computational complexity:
    !> - Memory: \(O(1)\)
    !> - Arithmetic: \(O(1)\)
    !>
    !> Failure behavior:
    !> - Returns without error
    subroutine get_log_entry_profiler(self, log_entry)
        implicit none
        !> Profiler record object
        class(type_record_profiler), intent(in) :: self
        !> String to hold the formatted log entry
        !> Overwritten on exit
        character(:), intent(inout), allocatable :: log_entry

        log_entry = strip(self%label%NAME)//" - Total Time: "// &
                    to_string(self%total_time)//" s, Calls: "// &
                    to_string(self%num_calls)
    end subroutine get_log_entry_profiler

    !> Initialize the time profiler
    !>
    !> Mathematical definition:
    !> - Allocates arrays for timers and timestamp records
    !>
    !> Assumptions:
    !> - None
    !>
    !> Numerical guarantee:
    !> - No theoretical error bound available
    !>
    !> Computational complexity:
    !> - Memory: \(O(n)\)
    !> - Arithmetic: \(O(n)\)
    !>
    !> Failure behavior:
    !> - Returns without error
    subroutine initialize_type_time_profiler(self)
        implicit none
        !> Profiler manager object
        !> Overwritten on exit
        class(type_time_profiler), intent(inout) :: self

        integer(int32) :: i
        integer(int32) :: stat

        allocate (self%timers(PROFILER_TYPES%NUM_ID), stat=stat)
        if (stat == 0) then
            do i = 1, PROFILER_TYPES%NUM_ID
                self%timers(i)%label = PROFILER_TYPES%to_object(i)
                self%timers(i)%total_time = 0.0d0
                self%timers(i)%start_time = 0.0d0
                self%timers(i)%num_calls = 0
            end do
        end if

        allocate (self%timestamps(TIME_RECORDS%NUM_ID), stat=stat)
        if (stat == 0) then
            do i = 1, TIME_RECORDS%NUM_ID
                self%timestamps(i)%label = TIME_RECORDS%to_object(i)
                self%timestamps(i)%date = ""
                self%timestamps(i)%time = ""
                self%timestamps(i)%zone = ""
            end do
        end if
    end subroutine initialize_type_time_profiler

    !> Destroy the time profiler
    !>
    !> Mathematical definition:
    !> - Deallocates arrays for timers and timestamp records
    !>
    !> Assumptions:
    !> - None
    !>
    !> Numerical guarantee:
    !> - No theoretical error bound available
    !>
    !> Computational complexity:
    !> - Memory: \(O(1)\)
    !> - Arithmetic: \(O(1)\)
    !>
    !> Failure behavior:
    !> - Returns without error
    subroutine destroy_type_time_profiler(self)
        implicit none
        !> Profiler manager object
        !> Overwritten on exit
        class(type_time_profiler), intent(inout) :: self

        if (allocated(self%timers)) deallocate (self%timers)
        if (allocated(self%timestamps)) deallocate (self%timestamps)
    end subroutine destroy_type_time_profiler

    !> Record an absolute timestamp
    !>
    !> Mathematical definition:
    !> - Calls system date and time routines to log current state
    !>
    !> Assumptions:
    !> - Label is a valid timestamp identifier
    !>
    !> Numerical guarantee:
    !> - No theoretical error bound available
    !>
    !> Computational complexity:
    !> - Memory: \(O(1)\)
    !> - Arithmetic: \(O(1)\)
    !>
    !> Failure behavior:
    !> - Returns silently if label is invalid
    subroutine record_time(self, label)
        implicit none
        !> Profiler manager object
        !> Overwritten on exit
        class(type_time_profiler), intent(inout) :: self
        !> Identifier for the timestamp record
        type(type_constant_id), intent(in) :: label

        if (.not. TIME_RECORDS%is_valid(label)) return

        associate (record => self%timestamps(label%ID))
            call date_and_time(date=record%date, time=record%time, zone=record%zone)
        end associate
    end subroutine record_time

    !> Start the timer for a specified profiling section
    !>
    !> Mathematical definition:
    !> - Records the current system wall time
    !>
    !> Assumptions:
    !> - Label is a valid profiler identifier
    !> - Timer is not already running
    !>
    !> Numerical guarantee:
    !> - No theoretical error bound available
    !>
    !> Computational complexity:
    !> - Memory: \(O(1)\)
    !> - Arithmetic: \(O(1)\)
    !>
    !> Failure behavior:
    !> - Stops execution if label is invalid or timer is already running
    subroutine start_profile(self, label)
        implicit none
        !> Profiler manager object
        !> Overwritten on exit
        class(type_time_profiler), intent(inout) :: self
        !> Identifier for the profiler record
        type(type_constant_id), intent(in) :: label

        if (.not. PROFILER_TYPES%is_valid(label)) then
            call raise_error(ERROR_CODES%INVALID_PROFILER_LABEL, opt=strip(label%NAME))
        end if

        if (self%timers(label%ID)%running) then
            error stop "Profiler '"//strip(label%NAME)//"' is already running."
        end if

        call self%get_current_wall_time(self%timers(label%ID)%start_time)
        self%timers(label%ID)%running = .true.
        self%timers(label%ID)%num_calls = self%timers(label%ID)%num_calls + 1
    end subroutine start_profile

    !> Stop the timer for a specified profiling section
    !>
    !> Mathematical definition:
    !> - Calculates elapsed time and accumulates it
    !>
    !> Assumptions:
    !> - Label is a valid profiler identifier
    !> - Timer is currently running
    !>
    !> Numerical guarantee:
    !> - No theoretical error bound available
    !>
    !> Computational complexity:
    !> - Memory: \(O(1)\)
    !> - Arithmetic: \(O(1)\)
    !>
    !> Failure behavior:
    !> - Stops execution if label is invalid or timer is not running
    subroutine stop_profile(self, label)
        implicit none
        !> Profiler manager object
        !> Overwritten on exit
        class(type_time_profiler), intent(inout) :: self
        !> Identifier for the profiler record
        type(type_constant_id), intent(in) :: label

        real(real64) :: end_time

        if (.not. PROFILER_TYPES%is_valid(label)) then
            call raise_error(ERROR_CODES%INVALID_PROFILER_LABEL, opt=strip(label%NAME))
        end if

        if (.not. self%timers(label%ID)%running) then
            error stop "Profiler '"//strip(label%NAME)//"' is not running."
        end if

        call self%get_current_wall_time(end_time)
        self%timers(label%ID)%total_time = self%timers(label%ID)%total_time &
                                           + (end_time - self%timers(label%ID)%start_time)
        self%timers(label%ID)%start_time = 0.0d0
        self%timers(label%ID)%running = .false.
    end subroutine stop_profile

    !> Get the current system wall time
    !>
    !> Mathematical definition:
    !> - Retrieves the wall clock time in seconds
    !>
    !> Assumptions:
    !> - OpenMP or system clock is available
    !>
    !> Numerical guarantee:
    !> - No theoretical error bound available
    !>
    !> Computational complexity:
    !> - Memory: \(O(1)\)
    !> - Arithmetic: \(O(1)\)
    !>
    !> Failure behavior:
    !> - Returns without error
    subroutine get_current_wall_time(self, current_time)
        implicit none
        !> Profiler manager object
        class(type_time_profiler), intent(in) :: self
        !> Current wall time in seconds
        !> Overwritten on exit
        real(real64), intent(inout) :: current_time

        integer(int32) :: count, rate

#ifdef _OPENMP
        current_time = omp_get_wtime()
#else
        call system_clock(count=count, count_rate=rate)
        current_time = real(count, kind=real64) / real(rate, kind=real64)
#endif
    end subroutine get_current_wall_time

    !> Display the accumulated profiling results
    !>
    !> Mathematical definition:
    !> - Formats and writes timing summaries to the specified output unit
    !>
    !> Assumptions:
    !> - None
    !>
    !> Numerical guarantee:
    !> - No theoretical error bound available
    !>
    !> Computational complexity:
    !> - Memory: \(O(1)\)
    !> - Arithmetic: \(O(n)\)
    !>
    !> Failure behavior:
    !> - Returns without error
    subroutine display_profiler(self, unit)
        implicit none
        !> Profiler manager object
        class(type_time_profiler), intent(in) :: self
        !> Optional output unit number
        integer(int32), intent(in), optional :: unit

        integer(int32) :: i, out_unit
        character(:), allocatable :: str_start, str_end
        real(real64) :: sum_total_time, percentage
        logical :: is_opened
        character(20) :: write_action

        out_unit = output_unit
        if (present(unit)) then
            if (unit /= output_unit) then
                inquire (unit=unit, opened=is_opened, write=write_action)
                if (is_opened .and. strip(write_action) == "YES") out_unit = unit
            else
                out_unit = unit
            end if
        end if

        call self%timestamps(TIME_RECORDS%START%ID)%get_log_entry(str_start)
        call self%timestamps(TIME_RECORDS%END%ID)%get_log_entry(str_end)

        write (out_unit, '(a)') "## Time Profiler Results"
        write (out_unit, '(a)') ""

        if (allocated(str_start)) then
            write (out_unit, '(a, a)') "- **Start:** ", str_start
        else
            write (out_unit, '(a)') "- **Start:** (Not recorded)"
        end if

        if (allocated(str_end)) then
            write (out_unit, '(a, a)') "- **End:** ", str_end
        else
            write (out_unit, '(a)') "- **End:** (Not recorded)"
        end if

        write (out_unit, '(a)') ""

        if (allocated(self%timers)) then
            if (size(self%timers) > 0) then
                sum_total_time = sum(self%timers%total_time)

                write (out_unit, '(a)') "| Section            | Time (s)   | Calls | Percentage |"
                write (out_unit, '(a)') "|:-------------------|:----------:|:-----:|:----------:|"

                do i = 1, size(self%timers)
                    percentage = 0.0d0
                    if (sum_total_time > 1.0d-12) then
                        percentage = (self%timers(i)%total_time / sum_total_time) * 100.0d0
                    end if

                    write (out_unit, '("| ", a20, " | ", es10.3, " | ", i5, " | ", f6.1, "%    |")') &
                        self%timers(i)%label%NAME, &
                        self%timers(i)%total_time, &
                        self%timers(i)%num_calls, &
                        percentage
                end do
            else
                write (out_unit, '(a)') "(No sections recorded)"
            end if
        end if

        write (out_unit, '(a)') ""
    end subroutine display_profiler

end module control_time_profiler

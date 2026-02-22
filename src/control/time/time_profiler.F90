module control_time_profiler
    use, intrinsic :: iso_fortran_env, only: int32, real64, output_unit
    use :: omp_lib
    use :: stdlib_strings, only:strip
    use :: module_core
    implicit none
    private

    public :: type_profiler

    type :: type_profiler_section
        character(20) :: label = ''
        real(real64) :: total_time = 0.0d0
        real(real64) :: start_time = 0.0d0
        integer(int32) :: call_count = 0
    contains
        procedure, pass(self), public :: match_label => match_profiler_section_label
    end type type_profiler_section

    type :: type_time_record
        character(20) :: label = ''
        character(10) :: date = ''
        character(10) :: time = ''
        character(10) :: zone = ''
    contains
        procedure, pass(self), public :: format => format_profiler_section
        procedure, pass(self), public :: get_log => get_log_formatted
    end type type_time_record

    type :: type_profiler
        private
        type(type_time_record) :: record_start
        type(type_time_record) :: record_end
        type(type_profiler_section), allocatable :: sections(:)
    contains
        procedure, pass(self), public :: initialize => initialize_profiler
        procedure, pass(self), private :: start_profile_by_name
        procedure, pass(self), private :: start_profile_by_id
        generic, public :: start => start_profile_by_name, start_profile_by_id
        procedure, pass(self), private :: stop_profile_by_name
        procedure, pass(self), private :: stop_profile_by_id
        generic, public :: stop => stop_profile_by_name, stop_profile_by_id
        procedure, pass(self), private :: get_current_wall_time
        procedure, pass(self), private :: get_profiler_id

        procedure, pass(self), public :: record => record_profiler
        procedure, pass(self), public :: get_record => get_record_profiler

        ! > Getter for exporting data to logger (Updated with percentage)
        procedure, pass(self), public :: get_data => get_profiling_data

        procedure, pass(self), public :: display => display_profiler

    end type type_profiler

contains

    subroutine initialize_profiler(self, labels)
        implicit none
        class(type_profiler), intent(inout) :: self
        character(len=10), intent(in) :: labels(:)

        integer(int32) :: i

        ! --- Profiler Sections Initialization ---
        if (allocated(self%sections)) deallocate (self%sections)
        if (size(labels) > 0) then
            allocate (self%sections(size(labels)))

            do i = 1, size(labels)
                self%sections(i)%label = trim(labels(i))
                self%sections(i)%total_time = 0.0d0
                self%sections(i)%start_time = 0.0d0
                self%sections(i)%call_count = 0
            end do
        end if

    end subroutine initialize_profiler

    subroutine format_profiler_section(self, formatted_string)
        implicit none
        class(type_time_record), intent(in) :: self
        character(:), allocatable, intent(inout) :: formatted_string

        formatted_string = &
            self%date(1:4)//"-"//self%date(5:6)//"-"//self%date(7:8)//"T"// &
            self%time(1:2)//":"//self%time(3:4)//":"//self%time(5:6)//strip(self%zone)

    end subroutine format_profiler_section

    subroutine get_log_formatted(self, log_string)
        implicit none
        class(type_time_record), intent(in) :: self
        character(:), allocatable, intent(inout) :: log_string

        character(:), allocatable :: time_stamp

        ! First, create date-time string
        call self%format(time_stamp)

        ! Combine with label
        log_string = strip(self%label)//" Time : "//time_stamp
    end subroutine get_log_formatted

    function match_profiler_section_label(self, label) result(is_match)
        implicit none
        class(type_profiler_section), intent(in) :: self
        character(*), intent(in) :: label
        logical :: is_match

        is_match = (strip(self%label) == strip(label))
    end function match_profiler_section_label

    subroutine get_profiler_id(self, label, id)
        implicit none
        class(type_profiler), intent(in) :: self
        character(*), intent(in) :: label
        integer(int32), intent(inout) :: id
        integer(int32) :: i

        id = -1
        if (allocated(self%sections)) then
            do i = 1, size(self%sections)
                if (self%sections(i)%match_label(label)) then
                    id = i
                    return
                end if
            end do
        end if
    end subroutine get_profiler_id

    subroutine start_profile_by_name(self, label)
        implicit none
        class(type_profiler), intent(inout) :: self
        character(*), intent(in) :: label
        integer(int32) :: id

        call self%get_profiler_id(label, id)
        if (id > 0) then
            call self%start_profile_by_id(id)
        else
            call raise_error(ERROR_CODES%INVALID_PROFILER_LABEL, opt=strip(label))
        end if
    end subroutine start_profile_by_name

    subroutine start_profile_by_id(self, id)
        implicit none
        class(type_profiler), intent(inout) :: self
        integer(int32), intent(in) :: id

        if (allocated(self%sections)) then
            if (id >= 1 .and. id <= size(self%sections)) then
                call self%get_current_wall_time(self%sections(id)%start_time)
                self%sections(id)%call_count = self%sections(id)%call_count + 1
            end if
        end if
    end subroutine start_profile_by_id

    subroutine stop_profile_by_name(self, label)
        implicit none
        class(type_profiler), intent(inout) :: self
        character(len=*), intent(in) :: label
        integer(int32) :: id

        call self%get_profiler_id(label, id)
        if (id > 0) then
            call self%stop_profile_by_id(id)
        else
            call raise_error(ERROR_CODES%INVALID_PROFILER_LABEL, opt=strip(label))
        end if
    end subroutine stop_profile_by_name

    subroutine stop_profile_by_id(self, id)
        implicit none
        class(type_profiler), intent(inout) :: self
        integer(int32), intent(in) :: id
        real(real64) :: end_time

        if (allocated(self%sections)) then
            if (id >= 1 .and. id <= size(self%sections)) then
                call self%get_current_wall_time(end_time)
                self%sections(id)%total_time = self%sections(id)%total_time &
                                               + (end_time - self%sections(id)%start_time)
                self%sections(id)%start_time = 0.0d0
            end if
        end if
    end subroutine stop_profile_by_id

    subroutine get_current_wall_time(self, current_time)
        implicit none
        class(type_profiler), intent(in) :: self
        real(real64), intent(inout) :: current_time
        integer(int32) :: count, rate

#ifdef _OPENMP
        current_time = omp_get_wtime()
#else
        call system_clock(count=count, count_rate=rate)
        current_time = real(count, kind=real64) / real(rate, kind=real64)
#endif
    end subroutine get_current_wall_time

    subroutine record_profiler(self, label)
        implicit none
        class(type_profiler), intent(inout) :: self
        integer(int32), intent(in) :: label

        select case (label)
        case (TIME_RECORD_START)
            call date_and_time(date=self%record_start%date, time=self%record_start%time, &
                               zone=self%record_start%zone)
            self%record_start%label = get_time_record_string(label)
        case (TIME_RECORD_END)
            call date_and_time(date=self%record_end%date, time=self%record_end%time, &
                               zone=self%record_end%zone)
            self%record_end%label = get_time_record_string(label)
        end select
    end subroutine record_profiler

    subroutine get_record_profiler(self, label, record)
        implicit none
        class(type_profiler), intent(in) :: self
        integer(int32), intent(in) :: label
        character(:), allocatable, intent(inout) :: record

        select case (label)
        case (TIME_RECORD_START)
            call self%record_start%get_log(record)
        case (TIME_RECORD_END)
            call self%record_end%get_log(record)
        end select
    end subroutine get_record_profiler

!> Extracts all profiling data including percentage
    !> Logic: If "Total" section exists, use its time as 100%. Otherwise, sum of all sections is 100%.
    subroutine get_profiling_data(self, str_start, str_end, sec_labels, sec_total_times, sec_call_counts, sec_percentages)
        implicit none
        class(type_profiler), intent(in) :: self
        character(:), allocatable, intent(inout) :: str_start
        character(:), allocatable, intent(inout) :: str_end
        character(20), allocatable, intent(inout) :: sec_labels(:)
        real(real64), allocatable, intent(inout) :: sec_total_times(:)
        integer(int32), allocatable, intent(inout) :: sec_call_counts(:)
        real(real64), allocatable, intent(inout) :: sec_percentages(:)

        integer(int32) :: n, i, idx_total
        real(real64) :: total_basis

        ! --- Export Time Records ---
        call self%record_start%format(str_start)
        call self%record_end%format(str_end)

        ! --- Export Section Arrays ---
        if (allocated(self%sections)) then
            n = size(self%sections)

            ! Reallocate output arrays
            if (allocated(sec_labels)) deallocate (sec_labels)
            if (allocated(sec_total_times)) deallocate (sec_total_times)
            if (allocated(sec_call_counts)) deallocate (sec_call_counts)
            if (allocated(sec_percentages)) deallocate (sec_percentages)

            allocate (sec_labels(n))
            allocate (sec_total_times(n))
            allocate (sec_call_counts(n))
            allocate (sec_percentages(n))

            ! Copy Data
            sec_labels = self%sections%label
            sec_total_times = self%sections%total_time
            sec_call_counts = self%sections%call_count

            ! --- Calculate Percentages ---
            total_basis = 0.0d0
            idx_total = -1

            ! 1. Search for "Total" section
            do i = 1, n
                if (strip(self%sections(i)%label) == 'Total') then
                    idx_total = i
                    exit
                end if
            end do

            ! 2. Determine the basis (denominator)
            if (idx_total > 0) then
                ! If "Total" exists, use its time as the basis
                total_basis = self%sections(idx_total)%total_time
            else
                ! If not, use the sum of all sections
                total_basis = sum(self%sections%total_time)
            end if

            ! 3. Compute percentage
            if (total_basis > 1.0d-12) then
                sec_percentages = (self%sections%total_time / total_basis) * 100.0d0
            else
                sec_percentages = 0.0d0
            end if

        else
            ! Deallocate if no sections
            if (allocated(sec_labels)) deallocate (sec_labels)
            if (allocated(sec_total_times)) deallocate (sec_total_times)
            if (allocated(sec_call_counts)) deallocate (sec_call_counts)
            if (allocated(sec_percentages)) deallocate (sec_percentages)
        end if

    end subroutine get_profiling_data

    subroutine display_profiler(self, unit)
        implicit none
        class(type_profiler), intent(in) :: self
        integer(int32), intent(in), optional :: unit

        integer(int32) :: i, out_unit
        character(:), allocatable :: str_start, str_end
        real(real64) :: sum_total_time, percentage

        logical :: is_opened
        character(20) :: write_action

        ! --- Output destination setting and validation ---
        out_unit = output_unit
        if (present(unit)) then
            if (unit /= output_unit) then
                inquire (unit=unit, opened=is_opened, write=write_action)
                if (is_opened .and. strip(write_action) == 'YES') then
                    out_unit = unit
                else
                    out_unit = output_unit
                end if
            else
                out_unit = unit
            end if
        end if

        call self%record_start%format(str_start)
        call self%record_end%format(str_end)

        ! --- Output in Markdown format ---
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

        ! --- Display section table with Percentage ---
        if (allocated(self%sections)) then
            if (size(self%sections) > 0) then
                sum_total_time = sum(self%sections%total_time)

                ! Header adjusted for Percentage column
                write (out_unit, '(a)') "| Section            | Time (s)   | Calls | Percentage |"
                write (out_unit, '(a)') "|:-------------------|:----------:|:-----:|:----------:|"

                do i = 1, size(self%sections)
                    ! Calculate percentage for display
                    percentage = 0.0d0
                    if (sum_total_time > 1.0d-12) then
                        percentage = (self%sections(i)%total_time / sum_total_time) * 100.0d0
                    end if

                    ! f6.1 format for percentage (e.g. 100.0 or 12.5)
                    write (out_unit, '("| ", a20, " | ", es10.3, " | ", i5, " | ", f6.1, "%    |")') &
                        self%sections(i)%label, &
                        self%sections(i)%total_time, &
                        self%sections(i)%call_count, &
                        percentage
                end do
            else
                write (out_unit, '(a)') "(No sections recorded)"
            end if
        end if

        write (out_unit, '(a)') ""

    end subroutine display_profiler

end module control_time_profiler

module control_time_profiler
    use, intrinsic :: iso_fortran_env, only: int32, real64, output_unit
    use :: omp_lib
    use :: stdlib_strings, only:strip
    use :: module_core
    implicit none
    private

    public :: type_profiler

    integer(int32), parameter :: ERR_PROFILER = 982

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

    subroutine format_profiler_section(self, formated_string)
        implicit none
        class(type_time_record), intent(in) :: self
        character(:), allocatable, intent(inout) :: formated_string

        formated_string = &
            self%date(1:4)//"-"//self%date(5:6)//"-"//self%date(7:8)//"T"// &
            self%time(1:2)//":"//self%time(3:4)//":"//self%time(5:6)//strip(self%zone)

    end subroutine format_profiler_section

    subroutine get_log_formatted(self, log_string)
        implicit none
        class(type_time_record), intent(in) :: self
        character(:), allocatable, intent(inout) :: log_string

        character(:), allocatable :: time_stamp

        ! まず日時文字列を作成
        call self%format(time_stamp)

        ! ラベルと結合
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
            call error_message(ERR_PROFILER, c_opt="Unknown label: "//strip(label))
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
            call error_message(ERR_PROFILER, c_opt="Unknown label: "//strip(label))
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
        real(real64) :: current_time
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
        character(:), allocatable :: record

        select case (label)
        case (TIME_RECORD_START)
            call self%record_start%get_log(record)
        case (TIME_RECORD_END)
            call self%record_end%get_log(record)
        end select
    end subroutine get_record_profiler

    subroutine display_profiler(self, unit)
        implicit none
        class(type_profiler), intent(in) :: self
        integer(int32), intent(in), optional :: unit

        integer(int32) :: i, out_unit
        character(:), allocatable :: str_start, str_end

        logical :: is_opened
        character(20) :: write_action

        ! --- 出力先の設定と検証 ---
        out_unit = output_unit ! デフォルト設定
        if (present(unit)) then
            if (unit /= output_unit) then
                ! 1. ユニットが開かれているか (opened)
                ! 2. 書き込み可能か (write) -> 'YES', 'NO', 'UNKNOWN'
                inquire (unit=unit, opened=is_opened, write=write_action)

                if (is_opened .and. strip(write_action) == 'YES') then
                    out_unit = unit
                else
                    ! 書き込めない場合は警告を出して標準出力に戻す
                    out_unit = output_unit
                end if
            else
                ! 指定が output_unit そのものだった場合
                out_unit = unit
            end if
        end if
        ! --- 日時文字列の取得 ---
        ! type_time_record の format 手続き (intent(inout) allocatable) を使用
        call self%record_start%format(str_start)
        call self%record_end%format(str_end)

        ! --- Markdown形式で出力 ---
        write (out_unit, '(a)') "## Time Profiler Results"
        write (out_unit, '(a)') ""
        ! Start/End 時間の表示 (未割り付け時のガード付き)
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

        ! --- セクションテーブルの表示 ---
        if (allocated(self%sections)) then
            if (size(self%sections) > 0) then
                write (out_unit, '(a)') "| Section            | Time (s)    | Calls |"
                write (out_unit, '(a)') "|:-------------------|:-----------:|:-----:|"

                do i = 1, size(self%sections)
                    write (out_unit, '("| ", a18, " | ", es10.3, " | ", i5, " |")') &
                        self%sections(i)%label, &
                        self%sections(i)%total_time, &
                        self%sections(i)%call_count
                end do
            else
                write (out_unit, '(a)') "(No sections recorded)"
            end if
        end if

        write (out_unit, '(a)') ""

    end subroutine display_profiler

end module control_time_profiler

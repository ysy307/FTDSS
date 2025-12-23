module control_time
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: omp_lib
    use :: module_core
    use :: module_input, only:type_input

    implicit none
    private

    public :: type_time

    ! --- Constants ---
    integer(int32), parameter :: MAX_BDF_ORDER = 6
    integer(int32), parameter :: ERR_TIME_INIT = 981
    integer(int32), parameter :: ERR_PROFILER = 982

    ! --- Types ---
    type :: type_time_record
        character(20) :: label = ''
        character(10) :: date = ''
        character(10) :: time = ''
        character(10) :: zone = ''
    end type type_time_record

    type :: type_profiler_section
        character(20) :: label = ''
        real(real64) :: total_time = 0.0d0
        real(real64) :: start_time = 0.0d0
        integer(int32) :: call_count = 0 ! 呼び出し回数も記録すると便利
    end type type_profiler_section

    type :: type_time
        private
        ! --- Time Stepping State ---
        real(real64) :: start_time = 0.0d0
        real(real64) :: end_time = 0.0d0
        real(real64) :: current_time = 0.0d0 ! 変数名を time -> current_time に明確化
        real(real64) :: time_old = 0.0d0

        real(real64) :: dt = 0.0d0
        real(real64), allocatable :: dt_old(:) ! History of dt
        real(real64) :: dt_min = 0.0d0
        real(real64) :: dt_max = 0.0d0

        real(real64) :: time_conversion = 1.0d0 ! Unit conversion factor

        ! --- BDF Coefficients ---
        real(real64) :: alpha(0:MAX_BDF_ORDER) = 0.0d0
        real(real64) :: beta = 0.0d0
        integer(int32) :: bdf_order = 1

        ! --- Records ---
        type(type_time_record) :: record_start
        type(type_time_record) :: record_end

        ! --- Profiler ---
        type(type_profiler_section), allocatable :: sections(:)

    contains
        ! --- Public Interfaces ---
        procedure, public, pass(self) :: initialize => initialize_type_time
        procedure, public, pass(self) :: record => record_timestamp

        ! Profiler (Overloaded for String and ID)
        procedure, public, pass(self) :: get_profiler_id
        procedure, public, pass(self) :: profile_start_by_name
        procedure, public, pass(self) :: profile_start_by_id
        generic, public :: profile_start => profile_start_by_name, profile_start_by_id

        procedure, public, pass(self) :: profile_stop_by_name
        procedure, public, pass(self) :: profile_stop_by_id
        generic, public :: profile_stop => profile_stop_by_name, profile_stop_by_id

        ! Time Management
        procedure, public, pass(self) :: update_bdf_coefficients => update_bdf_coefficients_wrapper
        procedure, public, pass(self) :: get_record
        procedure, public, pass(self) :: get_time
        procedure, public, pass(self) :: get_dt
        procedure, public, pass(self) :: get_bdf_order
        procedure, public, pass(self) :: get_bdf_alpha
        procedure, public, pass(self) :: get_bdf_beta
        procedure, public, pass(self) :: shift => shift_time
        procedure, public, pass(self) :: display => display_status

        ! --- Private Procedures ---
        procedure, private, pass(self) :: compute_bdf_coefficients
        procedure, public, nopass :: convert_time_unit
    end type type_time

contains

    ! ==========================================================================
    ! Initialization
    ! ==========================================================================
    subroutine initialize_type_time(self, input, profiler_sections)
        implicit none
        class(type_time), intent(inout) :: self
        type(type_input), intent(in), optional :: input
        character(*), intent(in), optional :: profiler_sections(:)

        integer(int32) :: i, istat
        real(real64) :: time_conv_coeff

        if (present(input)) then
            ! --- BDF設定 ---
            self%bdf_order = input%basic%solver_settings%bdf_order
            if (self%bdf_order > MAX_BDF_ORDER) then
                self%bdf_order = MAX_BDF_ORDER
                ! 警告を出しても良い箇所
            end if

            ! --- 時間単位変換係数の取得 ---
            time_conv_coeff = convert_time_unit(input%conditions%time_control%time_stepping%unit, &
                                                TIME_UNIT_SECONDS)

            ! --- dt 設定 ---
            self%dt = input%conditions%time_control%time_stepping%initial_step * time_conv_coeff
            self%dt_max = input%conditions%time_control%time_stepping%max_step * time_conv_coeff
            self%dt_min = input%conditions%time_control%time_stepping%min_step * time_conv_coeff

            ! --- 配列確保 ---
            if (allocated(self%dt_old)) deallocate (self%dt_old)
            allocate (self%dt_old(MAX_BDF_ORDER), stat=istat)
            if (istat /= 0) call error_message(ERR_TIME_INIT, c_opt="Failed allocating dt_old")

            self%dt_old = 0.0_real64
            self%alpha = 0.0_real64
            self%beta = 0.0_real64

            ! --- 初期係数計算 (1次精度) ---
            call self%compute_bdf_coefficients(1)

            ! --- シミュレーション期間 ---
            if (input%output_settings%field_output%file_format /= "none") then
                time_conv_coeff = convert_time_unit(input%conditions%time_control%simulation_period%unit, &
                                                    TIME_UNIT_SECONDS)
                self%start_time = input%conditions%time_control%simulation_period%start * time_conv_coeff
                self%end_time = input%conditions%time_control%simulation_period%end * time_conv_coeff

                self%time_conversion = convert_time_unit(input%output_settings%field_output%output_interval_unit, &
                                                         input%conditions%time_control%simulation_period%unit)
            end if

            ! 初期時間をセット
            self%current_time = self%start_time
        end if

        ! --- Profiler Sections Initialization ---
        if (present(profiler_sections)) then
            if (allocated(self%sections)) deallocate (self%sections)
            if (size(profiler_sections) > 0) then
                allocate (self%sections(size(profiler_sections)), stat=istat)
                if (istat /= 0) call error_message(ERR_TIME_INIT, c_opt="Failed allocating sections")

                do i = 1, size(profiler_sections)
                    self%sections(i)%label = trim(profiler_sections(i))
                    self%sections(i)%total_time = 0.0_real64
                    self%sections(i)%start_time = 0.0_real64
                    self%sections(i)%call_count = 0
                end do
            end if
        end if

    end subroutine initialize_type_time

    ! ==========================================================================
    ! Time Stepping & BDF
    ! ==========================================================================
    subroutine shift_time(self, reverse)
        implicit none
        class(type_time), intent(inout) :: self
        logical, intent(in), optional :: reverse

        integer(int32) :: n
        logical :: do_reverse

        do_reverse = .false.
        if (present(reverse)) do_reverse = reverse

        if (.not. allocated(self%dt_old)) return
        n = size(self%dt_old)

        if (do_reverse) then
            self%current_time = self%time_old
            ! 履歴の復元（完全には不可能だが，逆操作として近似）
            if (n > 0) self%dt = self%dt_old(1)
            if (n > 1) self%dt_old(1:n - 1) = self%dt_old(2:n)
            if (n > 0) self%dt_old(n) = 0.0d0
        else
            self%time_old = self%current_time
            self%current_time = self%current_time + self%dt

            ! 履歴の更新 (シフト)
            if (n > 1) self%dt_old(2:n) = self%dt_old(1:n - 1)
            if (n > 0) self%dt_old(1) = self%dt
        end if
    end subroutine shift_time

    subroutine update_bdf_coefficients_wrapper(self, order)
        implicit none
        class(type_time), intent(inout) :: self
        integer(int32), intent(in) :: order

        integer(int32) :: i, effective_order, order_to_use
        real(real64), parameter :: DT_TOL = 1.0d-15

        if (order < 1) call error_message(ERR_TIME_INIT, c_opt="Invalid BDF order requested")
        if (.not. allocated(self%dt_old)) call error_message(ERR_TIME_INIT, c_opt="dt_old not allocated")

        ! --- 利用可能な履歴の長さを確認 ---
        effective_order = 0
        do i = 1, min(size(self%dt_old), MAX_BDF_ORDER)
            if (self%dt_old(i) <= DT_TOL) exit
            effective_order = i
        end do

        order_to_use = min(order, effective_order)
        if (order_to_use < 1) order_to_use = 1 ! 最低でも1次（Backward Euler）

        call self%compute_bdf_coefficients(order_to_use)
    end subroutine update_bdf_coefficients_wrapper

    subroutine compute_bdf_coefficients(self, order)
        implicit none
        class(type_time), intent(inout) :: self
        integer(int32), intent(in) :: order

        integer(int32) :: j, m, n
        ! 自動配列を使用 (allocate オーバーヘッドを回避)
        real(real64) :: tau(0:MAX_BDF_ORDER)
        real(real64) :: num, denom, dLj, Lj
        real(real64), parameter :: eps = 1.0d-15

        if (self%dt <= 0.0d0) call error_message(ERR_TIME_INIT, c_opt="Current dt is nonpositive")
        if (order > MAX_BDF_ORDER) call error_message(ERR_TIME_INIT, c_opt="Order exceeds MAX_BDF_ORDER")

        ! --- BDF1 (Backward Euler) Optimization ---
        if (order == 1) then
            self%alpha(0) = 1.0d0
            self%alpha(1) = -1.0d0
            self%alpha(2:MAX_BDF_ORDER) = 0.0d0
            self%beta = 1.0d0
            self%bdf_order = 1
            return
        end if

        ! --- Initialize Tau ---
        ! tau_j = (t_{n-j} - t_n) / dt
        tau(0) = 0.0d0
        do j = 1, order
            tau(j) = tau(j - 1) - (self%dt_old(j) / self%dt)
        end do

        ! --- Compute Coefficients (alpha) ---
        do j = 0, order
            dLj = 0.0d0
            do m = 0, order
                if (m == j) cycle
                num = 1.0d0
                denom = 1.0d0
                do n = 0, order
                    if (n == j .or. n == m) cycle
                    num = num * (-tau(n))
                    denom = denom * (tau(j) - tau(n))
                end do

                if (abs(denom) < eps) then
                    ! 分母が極端に小さい場合は安全策としてBDF1にフォールバックなどを検討すべきだが，
                    ! ここではエラーとして停止させる
                    call error_message(ERR_TIME_INIT, c_opt="Small denominator in BDF calc")
                end if

                dLj = dLj + (num / denom) / (tau(j) - tau(m))
            end do
            self%alpha(j) = dLj
        end do

        ! unused alpha should be zero
        if (order < MAX_BDF_ORDER) self%alpha(order + 1:) = 0.0d0

        ! --- Compute Beta ---
        self%beta = 0.0d0
        do j = 0, order
            Lj = 1.0d0
            do m = 0, order
                if (m == j) cycle
                Lj = Lj * (-tau(m)) / (tau(j) - tau(m))
            end do
            self%beta = self%beta + Lj
        end do

        self%bdf_order = order
    end subroutine compute_bdf_coefficients

    ! ==========================================================================
    ! Profiler Logic
    ! ==========================================================================
    ! IDを取得する関数（文字列比較をここだけに集約）
    function get_profiler_id(self, label) result(id)
        implicit none
        class(type_time), intent(in) :: self
        character(len=*), intent(in) :: label
        integer(int32) :: id, i

        id = -1
        if (allocated(self%sections)) then
            do i = 1, size(self%sections)
                if (trim(self%sections(i)%label) == trim(label)) then
                    id = i
                    return
                end if
            end do
        end if
    end function get_profiler_id

    ! 文字列でスタート（従来の互換性）
    subroutine profile_start_by_name(self, label)
        implicit none
        class(type_time), intent(inout) :: self
        character(len=*), intent(in) :: label
        integer(int32) :: id

        id = self%get_profiler_id(label)
        if (id > 0) then
            call self%profile_start_by_id(id)
        else
            call error_message(ERR_PROFILER, c_opt="Unknown label: "//trim(label))
        end if
    end subroutine profile_start_by_name

    ! IDでスタート（高速版）
    subroutine profile_start_by_id(self, id)
        implicit none
        class(type_time), intent(inout) :: self
        integer(int32), intent(in) :: id

        if (allocated(self%sections)) then
            if (id >= 1 .and. id <= size(self%sections)) then
                self%sections(id)%start_time = get_current_wall_time()
                self%sections(id)%call_count = self%sections(id)%call_count + 1
            end if
        end if
    end subroutine profile_start_by_id

    subroutine profile_stop_by_name(self, label)
        implicit none
        class(type_time), intent(inout) :: self
        character(len=*), intent(in) :: label
        integer(int32) :: id

        id = self%get_profiler_id(label)
        if (id > 0) then
            call self%profile_stop_by_id(id)
        else
            call error_message(ERR_PROFILER, c_opt="Unknown label: "//trim(label))
        end if
    end subroutine profile_stop_by_name

    subroutine profile_stop_by_id(self, id)
        implicit none
        class(type_time), intent(inout) :: self
        integer(int32), intent(in) :: id
        real(real64) :: end_time

        if (allocated(self%sections)) then
            if (id >= 1 .and. id <= size(self%sections)) then
                end_time = get_current_wall_time()
                self%sections(id)%total_time = self%sections(id)%total_time + &
                                               (end_time - self%sections(id)%start_time)
                self%sections(id)%start_time = 0.0d0
            end if
        end if
    end subroutine profile_stop_by_id

    function get_current_wall_time() result(current_time)
        implicit none
        real(real64) :: current_time
        integer(int32) :: count, rate
#ifdef _OPENMP
        current_time = omp_get_wtime()
#else
        call system_clock(count=count, count_rate=rate)
        current_time = real(count, kind=real64) / real(rate, kind=real64)
#endif
    end function get_current_wall_time

    ! ==========================================================================
    ! Utility & Display
    ! ==========================================================================
    subroutine record_timestamp(self, label)
        implicit none
        class(type_time), intent(inout) :: self
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
    end subroutine record_timestamp

    pure function get_record(self, label) result(record)
        implicit none
        class(type_time), intent(in) :: self
        integer(int32), intent(in) :: label
        character(:), allocatable :: record

        select case (label)
        case (TIME_RECORD_START)
            record = trim(self%record_start%label)//" Time : "// &
                     self%record_start%date(1:4)//"-"//self%record_start%date(5:6)//"-"// &
                     self%record_start%date(7:8)//"T"// &
                     self%record_start%time(1:2)//":"//self%record_start%time(3:4)//":"// &
                     self%record_start%time(5:6)//trim(self%record_start%zone)
        case (TIME_RECORD_END)
            record = trim(self%record_end%label)//" Time : "// &
                     self%record_end%date(1:4)//"-"//self%record_end%date(5:6)//"-"// &
                     self%record_end%date(7:8)//"T"// &
                     self%record_end%time(1:2)//":"//self%record_end%time(3:4)//":"// &
                     self%record_end%time(5:6)//trim(self%record_end%zone)
        case default
            record = "Unknown Record"
        end select
    end function get_record

    ! Getters for private components
    pure function get_time(self) result(t)
        class(type_time), intent(in) :: self
        real(real64) :: t
        t = self%current_time * self%time_conversion
    end function get_time

    pure function get_dt(self) result(val)
        class(type_time), intent(in) :: self
        real(real64) :: val
        val = self%dt
    end function get_dt

    pure function get_bdf_order(self) result(val)
        class(type_time), intent(in) :: self
        integer(int32) :: val
        val = self%bdf_order
    end function get_bdf_order

    pure function get_bdf_alpha(self) result(val)
        class(type_time), intent(in) :: self
        real(real64), allocatable :: val(:)
        val = self%alpha(0:self%bdf_order)
    end function get_bdf_alpha

    pure function get_bdf_beta(self) result(val)
        class(type_time), intent(in) :: self
        real(real64) :: val
        val = self%beta
    end function get_bdf_beta

    pure function convert_time_unit(source_unit, target_unit) result(coefficient)
        implicit none
        integer(int32), intent(in) :: source_unit, target_unit
        real(real64) :: coefficient
        real(real64) :: to_seconds_factor(5)

        ! Table based conversion might be cleaner but select case is fine.
        ! Simply defined relative to seconds.
        ! 1:sec, 2:min, 3:hour, 4:day, 5:year
        to_seconds_factor = [1.0d0, 60.0d0, 3600.0d0, 86400.0d0, 31557600.0d0]

        if (source_unit < 1 .or. source_unit > 5 .or. target_unit < 1 .or. target_unit > 5) then
            coefficient = 1.0d0
        else
            coefficient = to_seconds_factor(source_unit) / to_seconds_factor(target_unit)
        end if
    end function convert_time_unit

    subroutine display_status(self)
        implicit none
        class(type_time), intent(in) :: self
        integer :: i

        write (*, '(a)') "## Time Status"
        write (*, '(a)') "---"
        write (*, *)

        write (*, '(a)') "### Simulation Period"
        write (*, '(" - Start Time      : ", ES12.5)') self%start_time
        write (*, '(" - End Time        : ", ES12.5)') self%end_time
        write (*, *)

        write (*, '(a)') "### Current Time Step"
        write (*, '(" - Current Time    : ", ES12.5)') self%current_time
        write (*, '(" - Current dt      : ", ES12.5)') self%dt
        write (*, '(" - BDF Order       : ", I0)') self%bdf_order
        write (*, *)

        if (allocated(self%sections)) then
            if (size(self%sections) > 0) then
                write (*, '(a)') "### Profiler Results"
                write (*, '(a)') "| Section            | Time (s)    | Calls |"
                write (*, '(a)') "|:-------------------|:-----------:|:-----:|"
                do i = 1, size(self%sections)
                    write (*, '("| ", A18, " | ", ES10.3, " | ", I5, " |")') &
                        trim(self%sections(i)%label), self%sections(i)%total_time, self%sections(i)%call_count
                end do
                write (*, *)
            end if
        end if
    end subroutine display_status

end module control_time

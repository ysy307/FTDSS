module control_time
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: omp_lib
    use :: module_core
    use :: module_input, only:type_input

    implicit none
    private

    public :: type_time

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
    end type type_profiler_section

    type :: type_time
        real(real64) :: start_time = 0.0d0
        real(real64) :: end_time = 0.0d0
        real(real64) :: time = 0.0d0
        real(real64) :: time_old = 0.0d0
        real(real64) :: dt = 0.0d0
        real(real64), allocatable :: dt_old(:)
        real(real64) :: dt_min = 0.0d0
        real(real64) :: dt_max = 0.0d0
        real(real64) :: time_conversion = 1.0d0
        real(real64), allocatable :: alpha(:)
        real(real64) :: beta = 0.0d0
        integer(int32) :: bdf_order = 1
        type(type_time_record) :: start
        type(type_time_record) :: end
        type(type_profiler_section), allocatable :: sections(:)
    contains
        procedure, public, pass(self) :: initialize => initialize_type_time
        procedure, public, pass(self) :: record => record_timestamp
        procedure, public, pass(self) :: profile_start => profile_start_timer
        procedure, public, pass(self) :: profile_stop => profile_stop_timer
        procedure, private, pass(self) :: compute_bdf_coefficients
        procedure, public, pass(self) :: update_bdf_coefficients => update_bdf_coefficients_wrapper
        procedure, public, pass(self) :: get_record
        procedure, public, pass(self) :: get_time
        procedure, public, nopass :: convert_time_unit
        procedure, public, pass(self) :: shift => shift_time
        procedure, public, pass(self) :: get_dt
        procedure, public, pass(self) :: display => display_status
    end type type_time

contains

    subroutine initialize_type_time(self, input, profiler_sections)
        implicit none
        class(type_time), intent(inout) :: self
        type(type_input), intent(in), optional :: input
        character(*), intent(in), optional :: profiler_sections(:)

        integer(int32) :: i, istat
        real(real64) :: time_conv_coeff

        if (present(input)) then
            ! --- 最大BDFオーダーを取得 ---
            self%bdf_order = input%basic%solver_settings%bdf_order

            ! --- dt 初期値/max/min ---
            time_conv_coeff = convert_time_unit(input%conditions%time_control%time_stepping%unit, &
                                                TIME_UNIT_SECONDS)
            self%dt = input%conditions%time_control%time_stepping%initial_step * time_conv_coeff
            self%dt_max = input%conditions%time_control%time_stepping%max_step * time_conv_coeff
            self%dt_min = input%conditions%time_control%time_stepping%min_step * time_conv_coeff

            ! --- dt_old 配列確保（ゼロ初期化） ---
            if (.not. allocated(self%dt_old)) then
                allocate (self%dt_old(self%bdf_order), stat=istat)
                if (istat /= 0) call error_message(981, c_opt="[initialize_type_time] failed allocating dt_old")
            end if
            self%dt_old = 0.0_real64

            ! --- alpha 配列確保（ゼロ初期化） ---
            if (.not. allocated(self%alpha)) then
                allocate (self%alpha(0:self%bdf_order), stat=istat)
                if (istat /= 0) call error_message(981, c_opt="[initialize_type_time] failed allocating alpha")
            end if
            self%alpha = 0.0_real64
            self%beta = 0.0_real64

            ! --- 初期 BDF 計算 (order=1) ---
            ! 初期化時には dt_old の履歴が存在しないため，汎用計算ルーチンはエラーとなる．
            ! そのため，安全な1次精度（後退オイラー法）の係数を直接計算・設定する．
            call self%compute_bdf_coefficients(1)

            ! --- シミュレーション開始/終了時間 ---
            if (.not. input%output_settings%field_output%file_format == "none") then
                time_conv_coeff = convert_time_unit(input%conditions%time_control%simulation_period%unit, &
                                                    TIME_UNIT_SECONDS)
                self%start_time = input%conditions%time_control%simulation_period%start * time_conv_coeff
                self%end_time = input%conditions%time_control%simulation_period%end * time_conv_coeff

                self%time_conversion = convert_time_unit(input%output_settings%field_output%output_interval_unit, &
                                                         input%conditions%time_control%simulation_period%unit)
            end if
        end if

        ! --- Profiler Sections ---
        if (present(profiler_sections)) then
            if (size(profiler_sections) > 0) then
                if (allocated(self%sections)) deallocate (self%sections)
                allocate (self%sections(size(profiler_sections)), stat=istat)
                if (istat /= 0) call error_message(981, c_opt="[initialize_type_time] failed allocating sections")
                do i = 1, size(profiler_sections)
                    self%sections(i)%label = trim(profiler_sections(i))
                    self%sections(i)%total_time = 0.0_real64
                    self%sections(i)%start_time = 0.0_real64
                end do
            end if
        end if

    end subroutine initialize_type_time

    subroutine record_timestamp(self, label)
        implicit none
        class(type_time), intent(inout) :: self
        integer(int32), intent(in) :: label

        select case (label)
        case (TIME_RECORD_START)
            call date_and_time(date=self%start%date, time=self%start%time, zone=self%start%zone)
            self%start%label = get_time_record_string(label)
        case (TIME_RECORD_END)
            call date_and_time(date=self%end%date, time=self%end%time, zone=self%end%zone)
            self%end%label = get_time_record_string(label)
        end select
    end subroutine record_timestamp

    subroutine profile_start_timer(self, label)
        implicit none
        class(type_time), intent(inout) :: self
        character(len=*), intent(in) :: label
        integer(int32) :: i
        do i = 1, size(self%sections)
            if (trim(self%sections(i)%label) == trim(label)) then
                self%sections(i)%start_time = get_current_time()
                return
            end if
        end do
        call error_message(982, c_opt="[profile_start_timer] unknown label: "//trim(label))
    end subroutine profile_start_timer

    subroutine profile_stop_timer(self, label)
        implicit none
        class(type_time), intent(inout) :: self
        character(len=*), intent(in) :: label
        integer(int32) :: i
        real(real64) :: end_time

        if (.not. allocated(self%sections)) then
            call error_message(982, c_opt="[profile_stop_timer] no profiler sections allocated")
        end if

        end_time = get_current_time()
        do i = 1, size(self%sections)
            if (trim(self%sections(i)%label) == trim(label)) then
                self%sections(i)%total_time = self%sections(i)%total_time + (end_time - self%sections(i)%start_time)
                self%sections(i)%start_time = 0.0d0
                return
            end if
        end do
        call error_message(982, c_opt="[profile_stop_timer] unknown label: "//trim(label))
    end subroutine profile_stop_timer

    function get_current_time() result(current_time)
        implicit none
        real(real64) :: current_time
#ifdef _OPENMP
        current_time = omp_get_wtime()
#else
        integer(int32) :: count, rate
        call system_clock(count=count, count_rate=rate)
        current_time = real(count, kind=real64) / real(rate, kind=real64)
#endif
    end function get_current_time

    pure function get_record(self, label) result(record)
        implicit none
        class(type_time), intent(in) :: self
        integer(int32), intent(in) :: label
        character(:), allocatable :: record

        select case (label)
        case (TIME_RECORD_START)
            record = trim(self%start%label)//" Time : "// &
                     self%start%date(1:4)//"-"//self%start%date(5:6)//"-"//self%start%date(7:8)//"T"// &
                     self%start%time(1:2)//":"//self%start%time(3:4)//":"//self%start%time(5:6)//trim(self%start%zone)
        case (TIME_RECORD_END)
            record = trim(self%end%label)//" Time : "// &
                     self%end%date(1:4)//"-"//self%end%date(5:6)//"-"//self%end%date(7:8)//"T"// &
                     self%end%time(1:2)//":"//self%end%time(3:4)//":"//self%end%time(5:6)//trim(self%end%zone)
        end select
    end function get_record

    function get_time(self) result(t)
        implicit none
        class(type_time), intent(in) :: self
        real(real64) :: t

        t = self%time * self%time_conversion
    end function get_time

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
            self%time = self%time_old
            if (n > 0) self%dt = self%dt_old(1)
            if (n > 1) self%dt_old(1:n - 1) = self%dt_old(2:n)
            if (n > 0) self%dt_old(n) = 0.0d0
        else
            self%time_old = self%time
            self%time = self%time + self%dt
            if (n > 1) self%dt_old(2:n) = self%dt_old(1:n - 1)
            if (n > 0) self%dt_old(1) = self%dt
        end if
    end subroutine shift_time

    subroutine update_bdf_coefficients_wrapper(self, order)
        implicit none
        class(type_time), intent(inout) :: self
        integer(int32), intent(in) :: order

        integer(int32) :: i, effective_order, order_to_use

        if (order < 1) then
            call error_message(981, c_opt="[update_bdf_coefficients] invalid BDF order")
        end if
        if (.not. allocated(self%dt_old)) call error_message(981, c_opt="[update_bdf_coefficients] dt_old not allocated")
        if (size(self%dt_old) < order) call error_message(981, c_opt="[update_bdf_coefficients] dt_old shorter than requested BDF order")

        ! --- alpha 配列が要求される次数に対して適切か確認し，必要であれば再確保 ---
        if (.not. allocated(self%alpha) .or. ubound(self%alpha, 1) < self%bdf_order) then
            if (allocated(self%alpha)) deallocate (self%alpha)
            allocate (self%alpha(0:self%bdf_order))
        end if

        ! --- 堅牢性の向上 ---
        ! dt_old の履歴に基づいて，現在計算可能な最大のBDF次数を決定する．
        ! BDF次数kには，k個の有効な（ゼロではない）過去のタイムステップが必要．
        effective_order = 0
        do i = 1, size(self%dt_old)
            if (self%dt_old(i) <= 1.0d-15) then
                ! 履歴がここで途切れている
                exit
            end if
            effective_order = i
        end do

        ! 要求された次数と，履歴から可能な次数のうち，小さい方を使用する．
        order_to_use = min(order, effective_order)
        ! 履歴がない場合(effective_order=0)でも，最低1次精度は保証する．
        if (order_to_use < 1) order_to_use = 1

        call self%compute_bdf_coefficients(order_to_use)
    end subroutine update_bdf_coefficients_wrapper

    subroutine compute_bdf_coefficients(self, order)
        implicit none
        class(type_time), intent(inout) :: self
        integer(int32), intent(in) :: order

        integer :: j, m, n
        real(real64), allocatable :: tau(:)
        real(real64) :: num, denom, dLj, Lj
        real(real64), parameter :: eps = 1.0d-15

        if (abs(self%dt) <= 0.0d0) call error_message(981, c_opt="[compute_bdf_coefficients] current dt is nonpositive or zero")

        ! --- BDF1（後退オイラー法）の場合 ---
        ! BDF1はdtの履歴を必要としないため，係数は常に一定．
        ! 履歴がない初期状態でも安全に計算できるよう，このケースを特別に処理する．
        if (order == 1) then
            self%alpha(0) = 1.0d0
            self%alpha(1) = -1.0d0
            if (ubound(self%alpha, 1) >= 2) self%alpha(2:) = 0.0d0 ! 高次の係数をクリア
            self%beta = 1.0d0
            self%bdf_order = 1
            return
        end if

        ! --- BDF2以上（可変ステップサイズ）の場合 ---
        if (order < 1) call error_message(981, c_opt="[compute_bdf_coefficients] invalid order")
        if (.not. allocated(self%dt_old)) call error_message(981, c_opt="[compute_bdf_coefficients] dt_old not allocated")
        if (size(self%dt_old) < order) call error_message(981, c_opt="[compute_bdf_coefficients] dt_old too short for order")

        ! `tau` は現在の時間 t_n を基準(0)とし，過去の時間点 t_{n-j} を現在のステップ幅 dt で正規化したもの．
        ! tau_j = (t_{n-j} - t_n) / dt
        allocate (tau(0:order))
        tau(0) = 0.0d0
        do j = 1, order
            tau(j) = tau(j - 1) - (self%dt_old(j) / self%dt)
        end do

        ! 念のためtauの重複をチェック
        do j = 0, order
            do m = j + 1, order
                if (abs(tau(j) - tau(m)) < eps) then
                    call error_message(981, c_opt="[compute_bdf_coefficients] duplicate tau values detected. Insufficient dt history.")
                end if
            end do
        end do

        ! ラグランジュ補間多項式の微分からBDF係数（alpha）を計算する．
        ! これは可変ステップサイズに対応した一般的な数値微分公式．
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
                if (abs(denom) < eps) call error_message(981, c_opt="[compute_bdf_coefficients] small denominator")
                dLj = dLj + num / denom / (tau(j) - tau(m))
            end do
            self%alpha(j) = dLj
        end do

        self%beta = 0.0d0
        do j = 0, order
            Lj = 1.0d0
            do m = 0, order
                if (m == j) cycle
                Lj = Lj * (-tau(m)) / (tau(j) - tau(m))
            end do
            self%beta = self%beta + Lj
        end do

        deallocate (tau)
        self%bdf_order = order
    end subroutine compute_bdf_coefficients

    pure function get_dt(self) result(dt)
        class(type_time), intent(in) :: self
        real(real64) :: dt
        dt = self%dt
    end function get_dt

    pure function convert_time_unit(source_unit, target_unit) result(coefficient)
        implicit none
        integer(int32), intent(in) :: source_unit
        integer(int32), intent(in) :: target_unit
        real(real64) :: coefficient
        real(real64) :: source_factor, target_factor

        select case (source_unit)
        case (TIME_UNIT_SECONDS)
            source_factor = 1.0d0
        case (TIME_UNIT_MINUTES)
            source_factor = 60.0d0
        case (TIME_UNIT_HOURS)
            source_factor = 3600.0d0
        case (TIME_UNIT_DAYS)
            source_factor = 86400.0d0
        case (TIME_UNIT_YEARS)
            source_factor = 31557600.0d0
        case default
            source_factor = 1.0d0
            ! Or error
        end select

        select case (target_unit)
        case (TIME_UNIT_SECONDS)
            target_factor = 1.0d0
        case (TIME_UNIT_MINUTES)
            target_factor = 60.0d0
        case (TIME_UNIT_HOURS)
            target_factor = 3600.0d0
        case (TIME_UNIT_DAYS)
            target_factor = 86400.0d0
        case (TIME_UNIT_YEARS)
            target_factor = 31557600.0d0
        case default
            target_factor = 1.0d0
            ! Or error
        end select

        coefficient = source_factor / target_factor
    end function convert_time_unit

    subroutine display_status(self)
        implicit none
        class(type_time), intent(in) :: self
        integer :: i, lb, ub

        ! --- Header ---
        write (*, '(a)') "## Time Status"
        write (*, '(a)') "---"
        write (*, *)

        ! --- Simulation Period ---
        write (*, '(a)') "### Simulation Period (seconds)"
        write (*, '(a)') "------------------------------------"
        write (*, '(" - Start Time      : ", ES12.5)') self%start_time
        write (*, '(" - End Time        : ", ES12.5)') self%end_time
        if (trim(self%start%label) /= "") write (*, '(" - Start Timestamp : ", A)') self%get_record(TIME_RECORD_START)
        if (trim(self%end%label) /= "") write (*, '(" - End Timestamp   : ", A)') self%get_record(TIME_RECORD_END)
        write (*, *)

        ! --- Current Time Step ---
        write (*, '(a)') "### Current Time Step (seconds)"
        write (*, '(a)') "------------------------------------"
        write (*, '(" - Current Time    : ", ES12.5)') self%time
        write (*, '(" - Current dt      : ", ES12.5)') self%dt
        write (*, '(" - Min dt          : ", ES12.5)') self%dt_min
        write (*, '(" - Max dt          : ", ES12.5)') self%dt_max

        if (allocated(self%dt_old)) then
            write (*, '(a)') "- dt History (newest first):"
            lb = lbound(self%dt_old, 1)
            ub = ubound(self%dt_old, 1)
            write (*, '(100(ES12.5,1X))', advance='no') (self%dt_old(i), i=lb, ub)
            write (*, *)
        end if
        write (*, *)

        ! --- BDF Coefficients ---
        if (allocated(self%alpha)) then
            write (*, '(a)') "### BDF Coefficients"
            write (*, '(" - Current Order   : ", I0)') self%bdf_order
            do i = lbound(self%alpha, 1), ubound(self%alpha, 1)
                write (*, '(" - alpha(",I0,") = ", ES12.5)') i, self%alpha(i)
            end do
            write (*, '(" - beta = ", ES12.5)') self%beta
            write (*, *)
        end if

        ! --- Profiler Results ---
        if (allocated(self%sections)) then
            if (size(self%sections) > 0) then
                write (*, '(a)') "### Profiler Results (seconds)"
                write (*, '(a)') "| Section | Time        |"
                write (*, '(a)') "|:-------:|:-----------:|"
                do i = 1, size(self%sections)
                    write (*, '("|", A20, "|", ES12.5, "|")') trim(self%sections(i)%label), self%sections(i)%total_time
                end do
                write (*, *)
            end if
        end if
    end subroutine display_status

end module control_time

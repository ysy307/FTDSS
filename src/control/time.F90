module control_time
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: omp_lib
    use :: stdlib_strings, only:strip
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
        integer(int32) :: call_count = 0
    end type type_profiler_section

    type :: type_time
        private
        ! --- Time Stepping State ---
        real(real64) :: start_time = 0.0d0
        real(real64) :: end_time = 0.0d0
        real(real64) :: current_time = 0.0d0
        real(real64) :: time_old = 0.0d0

        real(real64) :: dt = 0.0d0
        real(real64), allocatable :: dt_history(:) ! dtの履歴 (dt_{n}, dt_{n-1}, ...)
        real(real64) :: dt_min = 0.0d0
        real(real64) :: dt_max = 0.0d0

        real(real64) :: time_conversion = 1.0d0 ! 表示用の単位変換係数

        ! --- BDF Coefficients ---
        ! 方程式: dy/dt = sum( coeffs(j) * y_{n-j} )
        ! coeffsは 1/dt の次元を持つ (dtによる除算を含む)
        real(real64) :: coeffs(0:MAX_BDF_ORDER) = 0.0d0
        integer(int32) :: target_bdf_order = 1 ! 設定された目標次数
        integer(int32) :: current_bdf_order = 1 ! 現在利用可能な次数（起動直後など）

        ! --- Records ---
        type(type_time_record) :: record_start
        type(type_time_record) :: record_end

        ! --- Profiler ---
        type(type_profiler_section), allocatable :: sections(:)

    contains
        ! --- Public Interfaces ---
        procedure, public, pass(self) :: initialize => initialize_type_time
        procedure, public, pass(self) :: record => record_timestamp

        ! Profiler
        procedure, public, pass(self) :: get_profiler_id
        procedure, public, pass(self) :: profile_start_by_name
        procedure, public, pass(self) :: profile_start_by_id
        generic, public :: profile_start => profile_start_by_name, profile_start_by_id

        procedure, public, pass(self) :: profile_stop_by_name
        procedure, public, pass(self) :: profile_stop_by_id
        generic, public :: profile_stop => profile_stop_by_name, profile_stop_by_id

        ! Time Management
        procedure, public, pass(self) :: update_bdf_coefficients
        procedure, public, pass(self) :: get_record
        procedure, public, pass(self) :: get_time
        procedure, public, pass(self) :: get_dt
        procedure, public, pass(self) :: get_bdf_order
        procedure, public, pass(self) :: get_bdf_coeffs
        procedure, public, pass(self) :: shift => shift_time
        procedure, public, pass(self) :: display => display_status

        ! --- Private Procedures ---
        procedure, private, pass(self) :: compute_variable_step_coeffs
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
            self%target_bdf_order = input%basic%solver_settings%bdf_order
            if (self%target_bdf_order > MAX_BDF_ORDER) then
                self%target_bdf_order = MAX_BDF_ORDER
            end if
            ! 初期状態では履歴がないため1次からスタート
            self%current_bdf_order = 1

            ! --- 時間単位変換係数の取得 ---
            time_conv_coeff = convert_time_unit(input%conditions%time_control%time_stepping%unit, &
                                                TIME_UNIT_SECONDS)

            ! --- dt 設定 ---
            self%dt = input%conditions%time_control%time_stepping%initial_step * time_conv_coeff
            self%dt_max = input%conditions%time_control%time_stepping%max_step * time_conv_coeff
            self%dt_min = input%conditions%time_control%time_stepping%min_step * time_conv_coeff

            ! --- 履歴配列確保 ---
            if (allocated(self%dt_history)) deallocate (self%dt_history)
            allocate (self%dt_history(MAX_BDF_ORDER), stat=istat)
            if (istat /= 0) call error_message(ERR_TIME_INIT, c_opt="Failed allocating dt_history")

            self%dt_history = 0.0_real64
            self%dt_history(1) = self%dt ! 現在のdtを入れておく

            ! --- 初期係数計算 (1次精度) ---
            call self%compute_variable_step_coeffs()

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
    subroutine shift_time(self)
        implicit none
        class(type_time), intent(inout) :: self
        integer(int32) :: n

        if (.not. allocated(self%dt_history)) return
        n = size(self%dt_history)

        ! 時間更新
        self%time_old = self%current_time
        self%current_time = self%current_time + self%dt

        ! 履歴のシフト (dt_history(1) が最新のステップ幅になるようにする)
        ! dt_history(1) = dt_{n}
        ! dt_history(2) = dt_{n-1} ...
        if (n > 1) self%dt_history(2:n) = self%dt_history(1:n - 1)
        self%dt_history(1) = self%dt

        ! 利用可能な次数の更新（履歴が溜まるまでは次数を下げる）
        if (self%current_bdf_order < self%target_bdf_order) then
            self%current_bdf_order = self%current_bdf_order + 1
        end if

        ! 係数の再計算
        call self%compute_variable_step_coeffs()

    end subroutine shift_time

    subroutine update_bdf_coefficients(self)
        implicit none
        class(type_time), intent(inout) :: self
        ! 外部からdtなどを変更した後に手動で呼び出す場合
        call self%compute_variable_step_coeffs()
    end subroutine update_bdf_coefficients

    ! --------------------------------------------------------------------------
    ! 可変刻み幅BDF係数の計算
    ! 定義: dy/dt|_{t_n} approx sum_{j=0}^{k} coeffs(j) * y_{n-j}
    ! coeffsには 1/dt のスケーリングが含まれていることに注意．
    ! --------------------------------------------------------------------------
    subroutine compute_variable_step_coeffs(self)
        implicit none
        class(type_time), intent(inout) :: self

        integer(int32) :: k, j, m
        real(real64) :: tau(0:MAX_BDF_ORDER) ! 現在時刻 t_n からの相対時間差
        real(real64) :: prod_term, denom_term

        k = self%current_bdf_order

        ! 0. dtが不正でないかチェック
        if (self%dt <= 1.0d-16) then
            ! dtが極小の場合は警告あるいはエラーだが，ここでは安全のためBackward Euler係数をセットして戻る
            self%coeffs = 0.0d0
            if (self%dt > 0.0d0) then
                self%coeffs(0) = 1.0d0 / self%dt
                self%coeffs(1) = -1.0d0 / self%dt
            end if
            return
        end if

        ! 1. 相対時間 tau の計算
        ! tau(j) = t_n - t_{n-j}
        ! tau(0) = 0
        ! tau(1) = dt_n
        ! tau(2) = dt_n + dt_{n-1} ...
        tau(0) = 0.0d0
        do j = 1, k
            tau(j) = tau(j - 1) + self%dt_history(j)
        end do

        self%coeffs = 0.0d0

        ! 2. ラグランジュ補間多項式の微分値 (t=t_n) を計算
        ! L_j(t) = prod_{m!=j} (t - t_{n-m}) / (t_{n-j} - t_{n-m})
        ! dL_j/dt (t_n) を求める．

        ! (A) j = 0 の場合 (現在のステップ y_n に対する係数)
        ! L_0(t) = prod_{m=1}^k (t - t_{n-m}) / (t_n - t_{n-m})
        ! L_0'(t_n) = sum_{m=1}^k [ 1 / (t_n - t_{n-m}) * prod_{p!=m, p!=0} ... ]
        ! t=t_n を代入すると，(t_n - t_{n-m}) が約分されるため，
        ! L_0'(t_n) = sum_{m=1}^k (1 / tau(m)) となる．
        do m = 1, k
            self%coeffs(0) = self%coeffs(0) + (1.0d0 / tau(m))
        end do

        ! (B) j > 0 の場合 (過去のステップ y_{n-j} に対する係数)
        ! L_j(t) = (t - t_n)/(t_{n-j} - t_n) * prod_{m!=0, j} ...
        ! t=t_n で微分すると，(t - t_n) の微分の項（=1）だけが残り，他は (t-t_n) が掛かって消える．
        ! Coeff_j = (1 / (t_{n-j} - t_n)) * prod_{m!=0, j} (t_n - t_{n-m}) / (t_{n-j} - t_{n-m})
        !         = (1 / -tau(j)) * prod_{m!=0, j} (tau(m) / (tau(m) - tau(j)))
        do j = 1, k
            prod_term = 1.0d0
            do m = 1, k
                if (m == j) cycle
                prod_term = prod_term * (tau(m) / (tau(m) - tau(j)))
            end do
            self%coeffs(j) = (-1.0d0 / tau(j)) * prod_term
        end do

    end subroutine compute_variable_step_coeffs

    ! ==========================================================================
    ! Profiler Logic
    ! ==========================================================================
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

    ! Getters
    pure function get_time(self) result(t)
        implicit none
        class(type_time), intent(in) :: self
        real(real64) :: t
        t = self%current_time * self%time_conversion
    end function get_time

    pure function get_dt(self) result(val)
        implicit none
        class(type_time), intent(in) :: self
        real(real64) :: val
        val = self%dt
    end function get_dt

    pure function get_bdf_order(self) result(val)
        implicit none
        class(type_time), intent(in) :: self
        integer(int32) :: val
        val = self%current_bdf_order
    end function get_bdf_order

    pure function get_bdf_coeffs(self) result(val)
        implicit none
        class(type_time), intent(in) :: self
        real(real64), allocatable :: val(:)
        ! 呼び出し側には現在有効な次数分だけ渡す
        val = self%coeffs(0:self%current_bdf_order)
    end function get_bdf_coeffs

    pure function convert_time_unit(source_unit, target_unit) result(coefficient)
        implicit none
        integer(int32), intent(in) :: source_unit, target_unit
        real(real64) :: coefficient
        real(real64) :: to_seconds_factor(5)

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
        write (*, '(" - Start Time       : ", ES12.5)') self%start_time
        write (*, '(" - End Time         : ", ES12.5)') self%end_time
        write (*, *)

        write (*, '(a)') "### Current Time Step"
        write (*, '(" - Current Time     : ", ES12.5)') self%current_time
        write (*, '(" - Current dt       : ", ES12.5)') self%dt
        write (*, '(" - BDF Order        : ", I0)') self%current_bdf_order
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

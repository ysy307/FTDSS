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
    contains
        ! --- Public Interfaces ---
        procedure, public, pass(self) :: initialize => initialize_type_time
        procedure, public, pass(self) :: update_bdf_coefficients
        procedure, public, pass(self) :: get_time
        procedure, public, pass(self) :: get_dt
        procedure, public, pass(self) :: get_bdf_order
        procedure, public, pass(self) :: get_bdf_coeffs
        procedure, public, pass(self) :: shift => shift_time
        procedure, public, pass(self) :: display => display_status

        ! --- Private Procedures ---
        procedure, private, pass(self) :: compute_bdf_coefficients
        procedure, public, pass(self) :: convert_time_unit
    end type type_time

contains

    ! ==========================================================================
    ! Initialization
    ! ==========================================================================
    subroutine initialize_type_time(self, input)
        implicit none
        class(type_time), intent(inout) :: self
        type(type_input), intent(in) :: input

        integer(int32) :: i, istat
        real(real64) :: time_conv_coeff

        ! --- BDF設定 ---
        self%target_bdf_order = input%basic%solver_settings%bdf_order
        if (self%target_bdf_order > MAX_BDF_ORDER) then
            self%target_bdf_order = MAX_BDF_ORDER
        end if
        ! 初期状態では履歴がないため1次からスタート
        self%current_bdf_order = 1

        ! --- 時間単位変換係数の取得 ---
        associate (time_control => input%conditions%time_control)
            call self%convert_time_unit(time_control%time_stepping%unit, TIME_UNIT_SECONDS, time_conv_coeff)

            ! --- dt 設定 ---
            self%dt = time_control%time_stepping%initial_step * time_conv_coeff
            self%dt_max = time_control%time_stepping%max_step * time_conv_coeff
            self%dt_min = time_control%time_stepping%min_step * time_conv_coeff

            ! --- 履歴配列確保 ---
            call deallocate_array(self%dt_history)
            call allocate_array(self%dt_history, self%target_bdf_order)

            self%dt_history(:) = 0.0d0
            self%dt_history(1) = self%dt

            ! --- 初期係数計算 (1次精度) ---
            call self%compute_bdf_coefficients()

            ! --- シミュレーション期間 ---
            if (input%output_settings%field_output%file_format /= "none") then
                call self%convert_time_unit(time_control%simulation_period%unit, TIME_UNIT_SECONDS, time_conv_coeff)
                self%start_time = time_control%simulation_period%start * time_conv_coeff
                self%end_time = time_control%simulation_period%end * time_conv_coeff

                call self%convert_time_unit(input%output_settings%field_output%output_interval_unit, &
                                            time_control%simulation_period%unit, &
                                            self%time_conversion)
            end if

            ! 初期時間をセット
            self%current_time = self%start_time

        end associate

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
        call self%compute_bdf_coefficients()

    end subroutine shift_time

    subroutine update_bdf_coefficients(self)
        implicit none
        class(type_time), intent(inout) :: self
        ! 外部からdtなどを変更した後に手動で呼び出す場合
        call self%compute_bdf_coefficients()
    end subroutine update_bdf_coefficients

    ! --------------------------------------------------------------------------
    ! 可変刻み幅BDF係数の計算
    ! 定義: dy/dt|_{t_n} approx sum_{j=0}^{k} coeffs(j) * y_{n-j}
    ! coeffsには 1/dt のスケーリングが含まれていることに注意．
    ! --------------------------------------------------------------------------
    subroutine compute_bdf_coefficients(self)
        implicit none
        class(type_time), intent(inout) :: self

        integer(int32) :: k, j, m
        real(real64) :: tau(0:self%current_bdf_order)
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

    end subroutine compute_bdf_coefficients

    ! Getters
    subroutine get_time(self, current_time)
        implicit none
        class(type_time), intent(in) :: self
        real(real64), intent(inout) :: current_time

        current_time = self%current_time
    end subroutine get_time

    subroutine get_dt(self, dt)
        implicit none
        class(type_time), intent(in) :: self
        real(real64), intent(inout) :: dt

        dt = self%dt
    end subroutine get_dt

    subroutine get_bdf_order(self, bdf_order)
        implicit none
        class(type_time), intent(in) :: self
        integer(int32), intent(inout) :: bdf_order

        bdf_order = self%current_bdf_order
    end subroutine get_bdf_order

    subroutine get_bdf_coeffs(self, coeffs)
        implicit none
        class(type_time), intent(in), target :: self
        real(real64), intent(inout), pointer, dimension(:) :: coeffs

        coeffs => self%coeffs(0:self%current_bdf_order)
    end subroutine get_bdf_coeffs

    pure subroutine convert_time_unit(self, source_unit, target_unit, coefficient)
        implicit none
        class(type_time), intent(in) :: self
        integer(int32), intent(in) :: source_unit, target_unit
        real(real64), intent(inout) :: coefficient
        real(real64) :: to_seconds_factor(5)

        ! 1:sec, 2:min, 3:hour, 4:day, 5:year
        to_seconds_factor = [1.0d0, 60.0d0, 3600.0d0, 86400.0d0, 31557600.0d0]

        if (source_unit < 1 .or. source_unit > 5 .or. target_unit < 1 .or. target_unit > 5) then
            coefficient = 1.0d0
        else
            coefficient = to_seconds_factor(source_unit) / to_seconds_factor(target_unit)
        end if
    end subroutine convert_time_unit

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

    end subroutine display_status

end module control_time

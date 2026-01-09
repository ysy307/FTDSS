!>
!> 時間依存シミュレーションにおける物理変数の状態と履歴を管理するクラス
!>
module core_types_variable
    use, intrinsic :: iso_fortran_env, only: real64, int32
    use :: core_allocate, only:allocate_array
    use :: core_deallocate, only:deallocate_array
    use :: core_types_coordinate_array, only:type_coordinate_array_dp
    use :: core_types_coordinate, only:type_coordinate_dp
    implicit none
    private

    public :: type_variable

    !>
    !> 変数の時間発展状態をカプセル化する構造体．
    !> Newton-Raphson法などの非線形反復計算や，BDF法による時間積分をサポートする．
    !>
    type :: type_variable
        !> 初期化済みフラグ
        logical, private :: is_initialized = .false.

        !> 履歴として保持する過去のステップ数（BDFの次数に対応）
        integer(int32), private :: num_history_steps

        !> 変数の自由度（配列サイズ）
        integer(int32), private :: num_dof

        !> [t_{n+1}] 計算用現在値．
        !> Newton-Raphson法などの反復計算中は，収束前の「未確定値」が格納される．
        real(real64), allocatable :: current(:)

        !> [t_n] 確定した直前のタイムステップでの値
        real(real64), allocatable :: previous(:)

        !> [t_{n-1}, t_{n-2}, ...] さらに過去の履歴値
        !> 第2次元が古い順に並ぶ (:, 1) -> t_{n-1}, (:, 2) -> t_{n-2}
        real(real64), allocatable :: history(:, :)

        !> [du/dt] 時間微分値（currentの変化に伴い更新される）
        real(real64), allocatable :: diff(:)

        !> [grad u] 空間勾配
        type(type_coordinate_array_dp) :: grad

    contains
        !> 初期化・破棄
        procedure, public, pass(self) :: initialize => initialize_type_variable
        procedure, public, pass(self) :: destroy => destroy_type_variable

        !> 状態操作
        procedure, public, pass(self) :: advance => advance_time_step_variable
        procedure, public, pass(self) :: restore => restore_previous_step_variable
        procedure, public, pass(self) :: reset => reset_all_states_variable

        !> 値の設定（Setter）
        procedure, private, pass(self) :: set_current_array_variable
        procedure, private, pass(self) :: set_current_scalar_variable
        procedure, private, pass(self) :: set_current_scalar_all_variable
        generic, public :: set_current => set_current_array_variable, set_current_scalar_variable, set_current_scalar_all_variable
        procedure, private, pass(self) :: set_previous_array_variable
        procedure, private, pass(self) :: set_previous_scalar_variable
        procedure, private, pass(self) :: set_previous_scalar_all_variable
        generic, public :: set_previous => set_previous_array_variable, set_previous_scalar_variable, set_previous_scalar_all_variable

        !> 値の取得（Getter）
        procedure, private, pass(self) :: get_current_array_variable
        procedure, private, pass(self) :: get_current_scalar_variable
        procedure, private, pass(self) :: get_current_gradient_variable
        generic, public :: get_current => get_current_array_variable, get_current_scalar_variable, get_current_gradient_variable

        procedure, private, pass(self) :: get_previous_array
        procedure, private, pass(self) :: get_previous_scalar
        generic, public :: get_previous => get_previous_array, get_previous_scalar

        procedure, public, pass(self) :: get_history => get_history_values_variable

        !> 計算処理
        procedure, public, pass(self) :: compute_time_derivative => compute_time_derivative_variable
    end type type_variable

contains

    !>
    !> 変数管理配列のメモリ確保と初期化を行う．
    !>
    subroutine initialize_type_variable(self, num_dof, num_history_steps)
        implicit none
        class(type_variable), intent(inout) :: self
        integer(int32), intent(in) :: num_dof
        integer(int32), intent(in) :: num_history_steps

        self%num_dof = num_dof
        self%num_history_steps = num_history_steps

        call allocate_array(self%current, num_dof)
        call allocate_array(self%previous, num_dof)
        call allocate_array(self%history, num_dof, self%num_history_steps)
        call allocate_array(self%diff, num_dof)

        call self%grad%initialize(num_dof, 0.0d0)

        ! ゼロクリア
        self%current(:) = 0.0d0
        self%previous(:) = 0.0d0
        self%history(:, :) = 0.0d0
        self%diff(:) = 0.0d0

        self%is_initialized = .true.
    end subroutine initialize_type_variable

    !>
    !> 変数に関連付けられたメモリを解放する．
    !>
    subroutine destroy_type_variable(self)
        implicit none
        class(type_variable), intent(inout) :: self

        if (self%is_initialized) then
            call deallocate_array(self%current)
            call deallocate_array(self%previous)
            call deallocate_array(self%history)
            call deallocate_array(self%diff)
            call self%grad%destroy()
            self%is_initialized = .false.
        end if
    end subroutine destroy_type_variable

    !>
    !> 時間ステップを進める（Update）．
    !> 計算が収束し，現在の反復値(current)を次のステップの確定値として保存する場合に呼ぶ．
    !>
    subroutine advance_time_step_variable(self)
        implicit none
        class(type_variable), intent(inout) :: self

        if (.not. self%is_initialized) return

        ! 履歴のシフト (古い方へ順送り)
        if (self%num_history_steps > 1) then
            self%history(:, 2:self%num_history_steps) = self%history(:, 1:self%num_history_steps - 1)
        end if

        ! 直前の値を履歴の先頭(t_{n-1})へ
        if (self%num_history_steps > 0) then
            self%history(:, 1) = self%previous(:)
        end if

        ! 現在の反復値を直前の値(t_n)として確定
        self%previous(:) = self%current(:)

    end subroutine advance_time_step_variable

    !>
    !> 時間ステップを巻き戻す．
    !>
    subroutine restore_previous_step_variable(self)
        implicit none
        class(type_variable), intent(inout) :: self

        if (.not. self%is_initialized) return

        if (self%num_history_steps > 0) then
            ! 履歴の先頭から直前値を復元
            self%previous(:) = self%history(:, 1)

            ! 履歴を逆シフト
            if (self%num_history_steps > 1) then
                self%history(:, 1:self%num_history_steps - 1) = self%history(:, 2:self%num_history_steps)
            end if

            self%history(:, self%num_history_steps) = 0.0d0
        end if

    end subroutine restore_previous_step_variable

    !>
    !> 全ての時刻の状態を指定した値でリセットする（初期条件設定用）．
    !>
    subroutine reset_all_states_variable(self, initial_value)
        implicit none
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: initial_value(:)
        integer(int32) :: i

        if (.not. self%is_initialized) then
            error stop "Error: Variable not initialized in reset_all_states_variable."
        end if

        self%current(:) = initial_value(:)
        self%previous(:) = initial_value(:)

        if (self%num_history_steps > 0) then
            do i = 1, self%num_history_steps
                self%history(:, i) = initial_value(:)
            end do
        end if

        self%diff(:) = 0.0d0

    end subroutine reset_all_states_variable

    !>
    !> 現在値(current)を配列全体で設定する．
    !>
    subroutine set_current_array_variable(self, values)
        implicit none
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: values(:)

        if (.not. self%is_initialized) return

        if (size(values) /= self%num_dof) then
            error stop "Error: Dimension mismatch in set_current (array)."
        end if

        self%current(:) = values(:)
    end subroutine set_current_array_variable

    !>
    !> 特定の節点における現在値(current)を設定する．
    !>
    subroutine set_current_scalar_variable(self, node_id, value)
        implicit none
        class(type_variable), intent(inout) :: self
        integer(int32), intent(in) :: node_id
        real(real64), intent(in) :: value

        if (.not. self%is_initialized) return

        if (node_id < 1 .or. node_id > self%num_dof) then
            error stop "Error: Index out of bounds in set_current (scalar)."
        end if

        self%current(node_id) = value
    end subroutine set_current_scalar_variable

    !> 全節点における現在値(current)を設定する．
    !>
    subroutine set_current_scalar_all_variable(self, value)
        implicit none
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: value

        if (.not. self%is_initialized) return

        self%current(:) = value
    end subroutine set_current_scalar_all_variable

    !>
    !> 確定値(previous)を配列全体で設定する．
    !>
    subroutine set_previous_array_variable(self, values)
        implicit none
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: values(:)

        if (.not. self%is_initialized) return

        if (size(values) /= self%num_dof) then
            error stop "Error: Dimension mismatch in set_current (array)."
        end if

        self%previous(:) = values(:)
    end subroutine set_previous_array_variable

    !>
    !> 特定の節点における確定値(previous)を設定する．
    !>
    subroutine set_previous_scalar_variable(self, node_id, value)
        implicit none
        class(type_variable), intent(inout) :: self
        integer(int32), intent(in) :: node_id
        real(real64), intent(in) :: value

        if (.not. self%is_initialized) return

        if (node_id < 1 .or. node_id > self%num_dof) then
            error stop "Error: Index out of bounds in set_current (scalar)."
        end if

        self%previous(node_id) = value
    end subroutine set_previous_scalar_variable

    !>
    !> 全節点における確定値(previous)を設定する．
    !>
    subroutine set_previous_scalar_all_variable(self, value)
        implicit none
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: value

        if (.not. self%is_initialized) return

        self%previous(:) = value
    end subroutine set_previous_scalar_all_variable

    !>
    !> 計算中の現在値(配列全体)へのポインタを取得する．
    !> Newton-Raphsonの更新などで使用．
    !>
    subroutine get_current_array_variable(self, ptr_values)
        implicit none
        class(type_variable), intent(in), target :: self
        !> ポインタ引数の宣言
        real(real64), intent(inout), pointer, contiguous, dimension(:) :: ptr_values

        if (self%is_initialized) then
            ptr_values => self%current
        else
            ptr_values => null()
        end if
    end subroutine get_current_array_variable

    !>
    !> 特定の節点における現在値を取得する．
    !>
    pure subroutine get_current_scalar_variable(self, node_id, scalar_value)
        implicit none
        class(type_variable), intent(in) :: self
        integer(int32), intent(in) :: node_id
        real(real64), intent(inout) :: scalar_value

        if (self%is_initialized) then
            scalar_value = self%current(node_id)
        else
            scalar_value = 0.0d0
        end if
    end subroutine get_current_scalar_variable

    !>
    !> 特定の節点における現在の勾配を取得する．
    !>
    pure subroutine get_current_gradient_variable(self, node_id, gradient_value)
        implicit none
        class(type_variable), intent(in) :: self
        integer(int32), intent(in) :: node_id
        type(type_coordinate_dp), intent(inout) :: gradient_value

        if (self%is_initialized) then
            gradient_value%x = self%grad%x(node_id)
            gradient_value%y = self%grad%y(node_id)
            gradient_value%z = self%grad%z(node_id)
        else
            gradient_value%x = 0.0d0
            gradient_value%y = 0.0d0
            gradient_value%z = 0.0d0
        end if
    end subroutine get_current_gradient_variable

    !>
    !> 特定の節点における履歴値を取得する．
    !>
    pure subroutine get_history_values_variable(self, node_id, output_history)
        implicit none
        class(type_variable), intent(in) :: self
        integer(int32), intent(in) :: node_id
        real(real64), intent(inout) :: output_history(:)

        integer(int32) :: i, num_out, max_avail

        output_history(:) = 0.0d0
        if (.not. self%is_initialized) return

        max_avail = 2 + self%num_history_steps
        num_out = min(size(output_history), max_avail)

        if (num_out >= 1) output_history(1) = self%current(node_id)
        if (num_out >= 2) output_history(2) = self%previous(node_id)

        if (num_out > 2) then
            do i = 1, num_out - 2
                output_history(i + 2) = self%history(node_id, i)
            end do
        end if

    end subroutine get_history_values_variable

    !>
    !> 直前の確定値(配列全体)へのポインタを取得する．
    !> ファイル出力や可視化などで使用．
    !>
    subroutine get_previous_array(self, ptr_values)
        implicit none
        class(type_variable), intent(in), target :: self
        !> ポインタ引数の宣言
        real(real64), intent(inout), pointer, contiguous, dimension(:) :: ptr_values

        if (self%is_initialized) then
            ptr_values => self%previous
        else
            ptr_values => null()
        end if
    end subroutine get_previous_array

    !>
    !> 特定の節点における直前の確定値を取得する．
    !>
    pure subroutine get_previous_scalar(self, node_id, scalar_value)
        implicit none
        class(type_variable), intent(in) :: self
        integer(int32), intent(in) :: node_id
        real(real64), intent(inout) :: scalar_value

        if (self%is_initialized) then
            scalar_value = self%previous(node_id)
        else
            scalar_value = 0.0d0
        end if
    end subroutine get_previous_scalar

    !>
    !> BDF係数を用いて時間微分(du/dt)を計算する．
    !> 非線形反復中にcurrentが更新されるたびに再計算が必要となる．
    !>
    subroutine compute_time_derivative_variable(self, bdf_coeffs)
        implicit none
        class(type_variable), intent(inout) :: self
        real(real64), intent(in) :: bdf_coeffs(:)

        integer(int32) :: i, hist_idx
        integer(int32) :: n_coeffs

        if (.not. self%is_initialized) return

        n_coeffs = size(bdf_coeffs)
        self%diff(:) = 0.0d0

        ! 1. t_{n+1} (Current/Iterating) の項
        if (n_coeffs >= 1) then
            self%diff(:) = self%diff(:) + bdf_coeffs(1) * self%current(:)
        end if

        ! 2. t_{n} (Previous/Fixed) の項
        if (n_coeffs >= 2) then
            self%diff(:) = self%diff(:) + bdf_coeffs(2) * self%previous(:)
        end if

        ! 3. t_{n-1}... (History/Fixed) の項
        if (n_coeffs >= 3) then
            do i = 3, n_coeffs
                hist_idx = i - 2
                if (hist_idx > self%num_history_steps) exit

                self%diff(:) = self%diff(:) + bdf_coeffs(i) * self%history(:, hist_idx)
            end do
        end if

    end subroutine compute_time_derivative_variable

end module core_types_variable

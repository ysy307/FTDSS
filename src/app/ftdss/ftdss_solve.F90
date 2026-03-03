submodule(app_ftdss) ftdss_solve
    implicit none

contains
    module subroutine solve_time_step_initial_setup_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        real(real64), pointer, contiguous, dimension(:) :: u

        nullify (u)

        ! 1. 反復管理のリセット
        !    reset() は設定が NONE の場合に計算用も NONE にする可能性があります．
        call self%control%iteration%reset()

        ! [重要] 計算用ソルバーは常に PICARD か NEWTON でなければなりません．
        ! 設定が NONE (線形) の場合でも，離散化定式化としては Picard を使用するため，
        ! ここで明示的に PICARD をセットして reset の状態を上書きします．
        call self%control%iteration%set_nonlinear_solver(NONLINEAR_SOLVER%PICARD)

        call self%control%iteration%increment_total()
        call self%control%aitken%reset()

        ! 2. 前ステップの値の保存 (Previous <- Current)
        call self%porosity%get_previous(u)
        if (associated(u)) then
            call self%porosity%set_current(u)
            nullify (u)
        end if

        if (self%control%is_physics_active(PHYSICS_TYPES%THERMAL)) then
            call self%temperature%get_previous(u)
            if (associated(u)) then
                call self%temperature%set_current(u)
                nullify (u)
            end if
        end if

        if (self%control%is_physics_active(PHYSICS_TYPES%HYDRAULIC)) then
            call self%pressure%get_previous(u)
            if (associated(u)) then
                call self%pressure%set_current(u)
                nullify (u)
            end if
        end if

    end subroutine solve_time_step_initial_setup_ftdss

    module subroutine solve_time_step_setup_ftdss(self, prescribe_bc)
        implicit none
        class(type_ftdss), intent(inout) :: self
        logical, intent(inout) :: prescribe_bc

        integer(int32) :: iter

        call self%control%iteration%increment_nonlinear()
        call self%control%iteration%get_nonlinear_iter(iter)

        if (iter == 1) then
            prescribe_bc = .true.
        else
            prescribe_bc = .false.
        end if

        call self%calc_gradient_temperature()
        call self%calc_gradient_pressure()

    end subroutine solve_time_step_setup_ftdss

    module subroutine solve_time_step_check_convergence_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout), target :: self

        integer(int32) :: iter

        real(real64), pointer, contiguous, dimension(:) :: current_value

        real(real64), allocatable :: residual(:)
        real(real64), allocatable :: increment(:)
        real(real64) :: current_norm
        real(real64) :: switch_norm(PHYSICS_TYPES%NUM_ID) = [1.0d-2, 1.0d-4, 1.0d-4] ! [温度, 圧力] 切り替え閾値
        logical :: should_switch = .true.
        logical, parameter :: diverged = .true.

        logical :: is_compute_newton, is_compute_picard, is_config_none

        nullify (current_value)

        ! 計算用(Dynamic)の状態を取得
        is_compute_newton = self%control%is_compute_newton()
        is_compute_picard = self%control%is_compute_picard()
        ! 設定(Static)がNONEかどうかも取得しておく
        is_config_none = self%control%is_none()

        ! ----------------------------------------------------------------------
        ! Thermal Convergence Check
        ! ----------------------------------------------------------------------
        if (self%control%is_physics_active(PHYSICS_TYPES%THERMAL)) then
            call self%get_variable_residual(PHYSICS_TYPES%THERMAL, residual)
            call self%get_variable_increment(PHYSICS_TYPES%THERMAL, increment)

            if (has_nan(residual) .or. has_nan(increment)) then
                write (*, *) "Error: NaN detected in thermal variables during convergence check."
                call self%control%iteration%set_diverged(PHYSICS_TYPES%THERMAL, diverged)
            else
                ! 設定がNONEの場合は iteration モジュール内で即時 True が返されるため問題なし
                call self%control%iteration%check_convergence(PHYSICS_TYPES%THERMAL, residual, increment)
            end if

            ! Aitken緩和チェック
            ! 計算用がPicard であっても，設定が NONE の場合は緩和計算(reflect側)が行われない(omega=1.0)ため，
            ! ここでのチェックは不要(かつ誤検知の元)なのでスキップする．
            if (is_compute_picard .and. .not. is_config_none) then
                if (self%control%aitken%reach_min_relaxation(PHYSICS_TYPES%THERMAL)) then
                    write (*, *) "Warning: Relaxation factor too small. Stagnation detected."
                    call self%control%iteration%set_diverged(PHYSICS_TYPES%THERMAL, diverged)
                end if
            end if
        end if

        ! ----------------------------------------------------------------------
        ! Hydraulic Convergence Check
        ! ----------------------------------------------------------------------
        if (self%control%is_physics_active(PHYSICS_TYPES%HYDRAULIC)) then
            call self%get_variable_residual(PHYSICS_TYPES%HYDRAULIC, residual)
            call self%get_variable_increment(PHYSICS_TYPES%HYDRAULIC, increment)

            if (has_nan(residual) .or. has_nan(increment)) then
                write (*, *) "Error: NaN detected in hydraulic variables during convergence check."
                call self%control%iteration%set_diverged(PHYSICS_TYPES%HYDRAULIC, diverged)
            else
                call self%control%iteration%check_convergence(PHYSICS_TYPES%HYDRAULIC, residual, increment)
            end if

            if (is_compute_picard .and. .not. is_config_none) then
                if (self%control%aitken%reach_min_relaxation(PHYSICS_TYPES%HYDRAULIC)) then
                    write (*, *) "Warning: Relaxation factor too small. Stagnation detected."
                    call self%control%iteration%set_diverged(PHYSICS_TYPES%HYDRAULIC, diverged)
                end if
            end if
        end if

        ! ----------------------------------------------------------------------
        ! 2. Hybrid法 切り替え判定
        !    設定が NONE (線形) の場合は切り替えを行わない
        ! ----------------------------------------------------------------------
        call self%control%iteration%get_nonlinear_iter(iter)

        if (iter > 1 .and. .not. self%control%iteration%has_diverged()) then
            if (is_compute_picard .and. .not. is_config_none) then
                should_switch = .true.

                ! Thermal Residual Check
                if (self%control%is_physics_active(PHYSICS_TYPES%THERMAL)) then
                    current_norm = 0.0d0
                    call self%control%iteration%get_current_residual_norm(PHYSICS_TYPES%THERMAL, NORM_TYPES%LINF, current_norm)
                    ! [Debug output skipped]
                    if (current_norm > switch_norm(PHYSICS_TYPES%THERMAL%ID)) then
                        should_switch = .false.
                    end if
                end if

                ! Hydraulic Residual Check
                if (self%control%is_physics_active(PHYSICS_TYPES%HYDRAULIC)) then
                    current_norm = 0.0d0
                    call self%control%iteration%get_current_residual_norm(PHYSICS_TYPES%HYDRAULIC, NORM_TYPES%LINF, current_norm)
                    if (current_norm > switch_norm(PHYSICS_TYPES%HYDRAULIC%ID)) then
                        should_switch = .false.
                    end if
                end if

                if (should_switch) then
                    write (*, '("   -> Residual small enough. Switching to Newton-Raphson.")')
                    call self%control%iteration%set_nonlinear_solver(NONLINEAR_SOLVER%NEWTON)
                end if
            end if
        end if

        if (allocated(increment)) call deallocate_array(increment)
        if (allocated(residual)) call deallocate_array(residual)

    end subroutine solve_time_step_check_convergence_ftdss

    module subroutine solve_time_step_ftdss(self, is_step_converged)
        implicit none
        class(type_ftdss), intent(inout) :: self
        logical, intent(inout) :: is_step_converged
        logical :: prescribe_bc

        is_step_converged = .false.

        ! 1. 初期化セットアップ (ここで計算用ソルバーは必ず PICARD に設定される)
        call self%solve_time_step_initial_setup()

        ! 2. 非線形反復ループ
        nonlinear: do while (self%control%iteration%should_continue())

            ! 2.1 セットアップ (iter更新)
            call self%solve_time_step_setup(prescribe_bc)

            ! 2.2 行列・残差のアセンブル (compute_type=PICARD なので正しく動作する)
            call self%assemble()

            ! 2.3 境界条件の適用
            call self%apply_bc(prescribe_bc)

            ! 2.4 線形ソルバー (K * u = F)
            call self%solve()

            ! 2.5 収束判定
            !     設定が NONE なら常に is_converged = .true. となる
            call self%solve_time_step_check_convergence()

            ! 2.6 解の更新
            !     設定が NONE なら reflect 内で omega=1.0 となる(Aitken無効化)
            call self%reflect_variables()

            ! [追加] 設定(Static)が NONE の場合は，1回の計算でループを強制終了する
            ! 計算用変数が PICARD であっても，ここで抜けることで線形計算を実現する
            if (self%control%iteration%is_none()) exit nonlinear

        end do nonlinear

        is_step_converged = self%control%iteration%has_converged()
    end subroutine solve_time_step_ftdss

    module subroutine run_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        logical :: is_step_converged

        ! 終了時刻までループ
        time_loop: do while (.not. self%control%is_end_time())
            ! 1. 計算実行 (t -> t+dt)
            call self%solve_time_step(is_step_converged)

            ! 2. 先に時刻とATSを更新 (時刻が t+dt になる)
            call self%control%update(is_step_converged)

            if (is_step_converged) then
                ! [成功時]
                ! 3. 物理量の履歴のみをシフトする
                call self%shift()

                ! 4. 更新後の時刻で出力判定
                call self%update_variables()
                call self%output_fields()
                call self%output_history()
            else
                ! [失敗] やり直し処理
                write (*, '("   [WARNING] Step Failed. Retrying with smaller dt...")')
                call self%control%update(is_step_converged)
                cycle time_loop
            end if

        end do time_loop

    end subroutine run_ftdss
end submodule ftdss_solve

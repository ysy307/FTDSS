submodule(main_ftdss) ftdss_solve
    implicit none

contains
    module subroutine solve_time_step_initial_setup_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        real(real64), pointer, contiguous, dimension(:) :: u => null()

        call self%controls%iteration%reset()

        call self%controls%iteration%increment_total()
        call self%controls%aitken%reset()

        call self%porosity%get_previous(u)
        if (associated(u)) then
            call self%porosity%set_current(u)
            nullify (u)
        end if

        if (self%controls%is_physics_active(PHYSICS_TYPES%THERMAL)) then
            call self%temperature%get_previous(u)
            if (associated(u)) then
                call self%temperature%set_current(u)
                nullify (u)
            end if
        end if

        if (self%controls%is_physics_active(PHYSICS_TYPES%HYDRAULIC)) then
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

        call self%controls%iteration%increment_nonlinear()
        call self%controls%iteration%get_nonlinear_iter(iter)

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

        real(real64), pointer, contiguous, dimension(:) :: current_value => null()

        real(real64), allocatable :: residual(:)
        real(real64), allocatable :: increment(:)
        real(real64) :: current_norm
        real(real64) :: switch_norm(PHYSICS_TYPES%NUM_ID) = [1.0d-2, 1.0d-4, 1.0d-4] ! [温度, 圧力] 切り替え閾値
        logical :: should_switch = .true.
        logical, parameter :: diverged = .true.

        if (self%controls%is_physics_active(PHYSICS_TYPES%THERMAL)) then
            call self%get_variable_residual(PHYSICS_TYPES%THERMAL, residual)
            call self%get_variable_increment(PHYSICS_TYPES%THERMAL, increment)
            if (has_nan(residual) .or. has_nan(increment)) then
                write (*, *) "Error: NaN detected in thermal variables during convergence check."
                call self%controls%iteration%set_diverged(PHYSICS_TYPES%THERMAL, .true.)
            else
                call self%controls%iteration%check_convergence(PHYSICS_TYPES%THERMAL, residual, increment)
            end if
            if (self%controls%iteration%is_picard()) then
                if (self%controls%aitken%reach_min_relaxation(PHYSICS_TYPES%THERMAL)) then
                    write (*, *) "Warning: Relaxation factor too small. Stagnation detected."
                    call self%controls%iteration%set_diverged(PHYSICS_TYPES%THERMAL, .true.) ! 即時撤退させる
                end if
            end if
        end if

        if (self%controls%is_physics_active(PHYSICS_TYPES%HYDRAULIC)) then
            call self%get_variable_residual(PHYSICS_TYPES%HYDRAULIC, residual)
            call self%get_variable_increment(PHYSICS_TYPES%HYDRAULIC, increment)
            if (has_nan(residual) .or. has_nan(increment)) then
                write (*, *) "Error: NaN detected in hydraulic variables during convergence check."
                call self%controls%iteration%set_diverged(PHYSICS_TYPES%HYDRAULIC, .true.)
            else
                call self%controls%iteration%check_convergence(PHYSICS_TYPES%HYDRAULIC, residual, increment)
            end if

            if (self%controls%iteration%is_picard()) then
                if (self%controls%aitken%reach_min_relaxation(PHYSICS_TYPES%HYDRAULIC)) then
                    write (*, *) "Warning: Relaxation factor too small. Stagnation detected."
                    call self%controls%iteration%set_diverged(PHYSICS_TYPES%HYDRAULIC, .true.) ! 即時撤退させる
                end if
            end if
        end if

        ! ----------------------------------------------------------------------
        ! 2. [追加] Hybrid法 切り替え判定 (Residual Check)
        !    Picardモードで，残差が十分小さくなったらNewtonへ切り替える
        ! ----------------------------------------------------------------------
        call self%controls%iteration%get_nonlinear_iter(iter)

        if (iter > 1 .and. .not. self%controls%iteration%has_diverged()) then
            if (self%controls%iteration%is_picard()) then
                should_switch = .true.
                if (self%controls%is_physics_active(PHYSICS_TYPES%THERMAL)) then
                    current_norm = 0.0d0
                    call self%controls%iteration%get_current_residual_norm(PHYSICS_TYPES%THERMAL, NORM_TYPES%LINF, current_norm)
                    ! [推奨] デバッグ出力: 熱の残差状況を表示
                    write (*, '("   [Picard Check] Thermal |R|_inf: ", ES10.3, " / Threshold: ", ES10.3)') &
                        current_norm, switch_norm(PHYSICS_TYPES%THERMAL%id)
                    if (current_norm > switch_norm(PHYSICS_TYPES%THERMAL%id)) then
                        should_switch = .false.
                    end if
                end if
                if (self%controls%is_physics_active(PHYSICS_TYPES%HYDRAULIC)) then
                    call self%controls%iteration%get_current_residual_norm(PHYSICS_TYPES%HYDRAULIC, NORM_TYPES%LINF, current_norm)
                    if (current_norm > switch_norm(PHYSICS_TYPES%HYDRAULIC%id)) then
                        should_switch = .false.
                    end if
                end if

                if (should_switch) then
                    write (*, '("   -> Residual small enough. Switching to Newton-Raphson.")')
                    call self%controls%iteration%set_nonlinear_solver(NONLINEAR_SOLVER%NEWTON)
                end if

            end if
        end if

        call deallocate_array(increment)
        call deallocate_array(residual)

    end subroutine solve_time_step_check_convergence_ftdss

    module subroutine solve_time_step_ftdss(self, is_step_converged)
        implicit none
        class(type_ftdss), intent(inout) :: self
        logical, intent(inout) :: is_step_converged
        logical :: prescribe_bc
        is_step_converged = .false.

        ! 1. 初期化セットアップ
        call self%solve_time_step_initial_setup()

        ! 2. 非線形反復ループ（Newtonループ）
        !    収束判定は check_convergence で状態を更新し、ここ(should_continue)で抜ける
        nonlinear: do while (self%controls%iteration%should_continue())

            ! 2.1 セットアップ (iter更新, BCフラグ設定, 勾配計算など)
            call self%solve_time_step_setup(prescribe_bc)

            ! 2.2 行列・残差のアセンブル
            call self%assemble()

            ! 2.3 境界条件の適用
            call self%apply_bc(prescribe_bc)

            ! 2.4 線形ソルバー (K * u = F)
            call self%solve()

            ! 2.5 収束判定
            call self%solve_time_step_check_convergence()

            ! 2.6 解の更新 (U <= U + delta)
            call self%reflect_variables()

        end do nonlinear

        is_step_converged = self%controls%iteration%has_converged()
    end subroutine solve_time_step_ftdss

    module subroutine run_ftdss(self)
        implicit none
        class(type_ftdss), intent(inout) :: self

        logical :: is_step_converged
        real(real64) :: current_dt, next_dt

        ! 終了時刻までループ
        time_loop: do while (.not. self%controls%time%is_end_time())

            ! ------------------------------------------------------------------
            ! 2. 1ステップ計算
            ! ------------------------------------------------------------------
            ! ここで reset や 非線形反復ループ が回る
            call self%solve_time_step(is_step_converged)

            ! ------------------------------------------------------------------
            ! 3. 判定分岐 (ATSの核心)
            ! ------------------------------------------------------------------
            if (is_step_converged) then
                ! ! ==============================================================
                ! ! [成功] 次のステップへ進む処理
                ! ! ==============================================================
                ! write(*, '("   [INFO] Step Converged at t=", ES12.4)') self%controls%time%current_time

                ! ! A. 要素等へのマッピング (平滑化)
                ! call self%update_variables()

                ! ! B. ファイル出力 (現在のステップの結果を出力)
                ! call self%output_fields()
                ! call self%output_history()

                ! ! C. 時間を進める (New -> Old へ値をシフト)
                ! call self%shift()

                ! ! D. 時間管理変数の更新 (t = t + dt)
                ! call self%controls%time%update()

                ! E. 次の dt を少し増やす (回復運転: dt = dt * 1.1 等)
                !    (もし余裕があれば実装)
                ! call self%controls%time%increase_dt()

            else
                ! ! ==============================================================
                ! ! [失敗] やり直し処理 (リトライ)
                ! ! ==============================================================
                ! write(*, '("   [WARNING] Step Failed. Retrying with smaller dt...")')

                ! ! A. 状態を巻き戻す (温度などを解く前の値に戻す)
                ! call self%restore_state()

                ! ! B. 時間刻みを半分にする (dt = dt * 0.5)
                ! !    ※ ここで dt が小さくなりすぎたら停止するエラー処理も必要
                ! call self%controls%time%cut_dt(0.5d0)

                ! ! C. ループの先頭に戻って再計算 (cycle)
                ! cycle time_loop

            end if

        end do time_loop

    end subroutine run_ftdss
end submodule ftdss_solve

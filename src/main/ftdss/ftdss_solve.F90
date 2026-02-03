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

        call self%controls%iteration%set_nonlinear_solver(NONLINEAR_SOLVER%PICARD)

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

        logical :: is_newton, is_picard

        ! 現在の計算用ソルバー設定を取得 (Active Solver)
        is_newton = self%controls%iteration%is_newton()
        is_picard = self%controls%iteration%is_picard()

        ! ----------------------------------------------------------------------
        ! Thermal Convergence Check
        ! ----------------------------------------------------------------------
        if (self%controls%is_physics_active(PHYSICS_TYPES%THERMAL)) then
            call self%get_variable_residual(PHYSICS_TYPES%THERMAL, residual)
            call self%get_variable_increment(PHYSICS_TYPES%THERMAL, increment)

            if (has_nan(residual) .or. has_nan(increment)) then
                write (*, *) "Error: NaN detected in thermal variables during convergence check."
                call self%controls%iteration%set_diverged(PHYSICS_TYPES%THERMAL, diverged)
            else
                call self%controls%iteration%check_convergence(PHYSICS_TYPES%THERMAL, residual, increment)
            end if

            ! note: 元のコードの if (is_picard .or. is_newton) ... else if (is_picard) は
            !       Picard時に最初のブロック(空)に入りAitkenが呼ばれないため修正しました．
            if (is_picard) then
                if (self%controls%aitken%reach_min_relaxation(PHYSICS_TYPES%THERMAL)) then
                    write (*, *) "Warning: Relaxation factor too small. Stagnation detected."
                    call self%controls%iteration%set_diverged(PHYSICS_TYPES%THERMAL, diverged) ! 即時撤退させる
                end if
            end if
        end if

        ! ----------------------------------------------------------------------
        ! Hydraulic Convergence Check
        ! ----------------------------------------------------------------------
        if (self%controls%is_physics_active(PHYSICS_TYPES%HYDRAULIC)) then
            call self%get_variable_residual(PHYSICS_TYPES%HYDRAULIC, residual)
            call self%get_variable_increment(PHYSICS_TYPES%HYDRAULIC, increment)

            if (has_nan(residual) .or. has_nan(increment)) then
                write (*, *) "Error: NaN detected in hydraulic variables during convergence check."
                call self%controls%iteration%set_diverged(PHYSICS_TYPES%HYDRAULIC, diverged)
            else
                call self%controls%iteration%check_convergence(PHYSICS_TYPES%HYDRAULIC, residual, increment)
            end if

            if (is_picard) then
                if (self%controls%aitken%reach_min_relaxation(PHYSICS_TYPES%HYDRAULIC)) then
                    write (*, *) "Warning: Relaxation factor too small. Stagnation detected."
                    call self%controls%iteration%set_diverged(PHYSICS_TYPES%HYDRAULIC, diverged) ! 即時撤退させる
                end if
            end if
        end if

        ! ----------------------------------------------------------------------
        ! 2. [追加] Hybrid法 切り替え判定 (Residual Check)
        !    Picardモードで，残差が十分小さくなったらNewtonへ切り替える
        !    reset_nonlinearで初期状態がPicardになっているため，ここで条件を満たせばActiveをNewtonに変更する
        ! ----------------------------------------------------------------------
        call self%controls%iteration%get_nonlinear_iter(iter)

        ! まだ発散しておらず，かつ現在の計算モードがPicardの場合のみチェック
        if (iter > 1 .and. .not. self%controls%iteration%has_diverged()) then
            if (self%controls%iteration%is_picard()) then
                should_switch = .true.

                ! Thermal Residual Check
                if (self%controls%is_physics_active(PHYSICS_TYPES%THERMAL)) then
                    current_norm = 0.0d0
                    ! 直前の check_convergence で計算された最新の残差ノルムを取得
                    call self%controls%iteration%get_current_residual_norm(PHYSICS_TYPES%THERMAL, NORM_TYPES%LINF, current_norm)

                    ! [推奨] デバッグ出力: 熱の残差状況を表示
                    write (*, '("    [Picard Check] Thermal |R|_inf: ", ES10.3, " / Threshold: ", ES10.3)') &
                        current_norm, switch_norm(PHYSICS_TYPES%THERMAL%id)

                    if (current_norm > switch_norm(PHYSICS_TYPES%THERMAL%id)) then
                        should_switch = .false.
                    end if
                end if

                ! Hydraulic Residual Check
                if (self%controls%is_physics_active(PHYSICS_TYPES%HYDRAULIC)) then
                    current_norm = 0.0d0
                    call self%controls%iteration%get_current_residual_norm(PHYSICS_TYPES%HYDRAULIC, NORM_TYPES%LINF, current_norm)

                    if (current_norm > switch_norm(PHYSICS_TYPES%HYDRAULIC%id)) then
                        should_switch = .false.
                    end if
                end if

                ! Switch Logic
                if (should_switch) then
                    write (*, '("   -> Residual small enough. Switching to Newton-Raphson.")')
                    ! ここで計算用ソルバータイプをNewtonに変更する．
                    ! 次の反復(solve_nonlinear_step等)からは is_newton() がTrueになる．
                    call self%controls%iteration%set_nonlinear_solver(NONLINEAR_SOLVER%NEWTON)
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
        time_loop: do while (.not. self%controls%is_end_time())
            ! 1. 計算実行 (t -> t+dt)
            call self%solve_time_step(is_step_converged)

            ! 2. 先に時刻とATSを更新 (時刻が t+dt になる)
            call self%controls%update(is_step_converged)

            if (is_step_converged) then
                ! [成功時]
                ! 3. 物理量の履歴のみをシフトする
                call self%shift()

                ! 4. 更新後の時刻で出力判定
                call self%update_variables()
                call self%output_fields()
                call self%output_history()
            else
                ! ! ==============================================================
                ! ! [失敗] やり直し処理 (リトライ)
                ! ! ==============================================================
                write (*, '("   [WARNING] Step Failed. Retrying with smaller dt...")')

                call self%controls%update(is_step_converged)
                ! ! C. ループの先頭に戻って再計算 (cycle)
                cycle time_loop

            end if

        end do time_loop

    end subroutine run_ftdss
end submodule ftdss_solve

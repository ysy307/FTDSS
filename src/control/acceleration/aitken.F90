submodule(control_acceleration) acceleration_aitken
    implicit none
contains
    module subroutine initialize_acceleration_aitken(self, config)
        implicit none
        class(type_acceleration_aitken), intent(inout) :: self
        type(type_config_acceleration), intent(in) :: config

        call self%config%copy(config)
        if (allocated(self%du_raw)) deallocate (self%du_raw)
        call allocate_array(self%du_raw, self%config%num_dofs, PHYSICS_TYPES%NUM_ID)

        call self%reset()

    end subroutine initialize_acceleration_aitken

    module subroutine destory_acceleration_aitken(self)
        implicit none
        class(type_acceleration_aitken), intent(inout) :: self

        call self%config%reset()

        call deallocate_array(self%du_raw)

        self%relaxation_factor(:) = 0.0d0
        self%previous_relaxation_factor(:) = 0.0d0
    end subroutine destory_acceleration_aitken

    module subroutine compute_acceleration_aitken(self, physics_type, iter, du, vec)
        implicit none
        class(type_acceleration_aitken), intent(inout) :: self
        type(type_constant_id), intent(in) :: physics_type
        integer(int32), intent(in) :: iter
        real(real64), intent(in) :: du(:) ! 増分 Δu
        real(real64), intent(inout) :: vec(:) ! 更新されるベクトル u (u = u + ω*Δu)

        integer(int32) :: pid
        real(real64) :: numerator
        real(real64) :: denominator
        real(real64) :: omega

        pid = physics_type%ID

        ! 1. リラクゼーション係数 (omega) の計算
        if (iter > 1) then
            ! 前回の du (self%du_raw) と今回の du を用いて omega を計算
            ! 分子: (Δu_n - Δu_{n-1}) · Δu_{n-1}
            numerator = vector_dot((du - self%du_raw(:, pid)), self%du_raw(:, pid))
            ! 分母: |Δu_n - Δu_{n-1}|^2
            denominator = vector_dot(du - self%du_raw(:, pid), du - self%du_raw(:, pid))

            if (denominator > epsilon(1.0d0)) then
                ! ω_n = -ω_{n-1} * (numerator / denominator)
                omega = -self%previous_relaxation_factor(pid) * (numerator / denominator)

                ! リミッターの適用
                if (omega < self%config%min_relaxation) then
                    omega = self%config%min_relaxation
                else if (omega > self%config%max_relaxation) then
                    omega = self%config%max_relaxation
                end if
                self%relaxation_factor(pid) = omega
                self%previous_relaxation_factor(pid) = omega
            else
                ! 分母が小さすぎる場合は前回の値を維持
                omega = self%previous_relaxation_factor(pid)
            end if
        else
            ! 初回反復はリラクゼーションなし (1.0)
            omega = 1.0d0
            self%relaxation_factor(pid) = omega
            ! 次回計算用に保存はしない（または初期値 0.5 等を維持）
        end if

        ! 2. ベクトルの更新 (u = u + ω*Δu)
        vec(:) = vec(:) + omega * du(:)

        ! 3. 次回ステップのための状態保存 (今回の du を保存)
        self%du_raw(:, pid) = du(:)

    end subroutine compute_acceleration_aitken

    module subroutine reset_acceleration_aitken(self)
        implicit none
        class(type_acceleration_aitken), intent(inout) :: self

        self%du_raw(:, :) = 0.0d0
        self%relaxation_factor(:) = 1.0d0
        self%previous_relaxation_factor(:) = 0.0d0
    end subroutine reset_acceleration_aitken
end submodule acceleration_aitken

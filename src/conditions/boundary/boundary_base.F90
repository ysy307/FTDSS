submodule(conditions_boundary) conditions_boundary_base
    implicit none

contains

    ! --------------------------------------------------------------------------
    ! 初期化ルーチン
    ! ※ Manager側で allocate された後に呼ばれる前提
    ! --------------------------------------------------------------------------
    module subroutine initialize_bc(self, cell_id, target_bc, input, controls)
        implicit none
        class(abst_bc), intent(inout) :: self
        integer(int32), intent(in) :: cell_id
        integer(int32), intent(in) :: target_bc
        type(type_input), intent(in) :: input
        type(type_controls), intent(in) :: controls

        integer(int32) :: i, target_physics
        real(real64) :: time_conv
        real(real64), allocatable :: raw_values(:)
        logical :: found

        if (self%initialized) call self%destroy()
        self%boundary_id = cell_id
        self%bc_kind = target_bc

        ! --- BCの種類から、物理タイプ(Thermal/Hydraulic)と必要変数を決定 ---
        select case (target_bc)
            ! [Thermal]
        case (THERMAL_BC_DIRICHLET, THERMAL_BC_ADIABATIC, THERMAL_BC_FREE)
            target_physics = PHYSICS_TYPE_THERMAL
            self%num_variables = 1

        case (THERMAL_BC_NEUMANN, THERMAL_BC_FLUX)
            target_physics = PHYSICS_TYPE_THERMAL
            self%num_variables = 1

        case (THERMAL_BC_ROBIN, THERMAL_BC_CONVECTIVE)
            target_physics = PHYSICS_TYPE_THERMAL
            self%num_variables = 2 ! [1]:Ref, [2]:h

        case (THERMAL_BC_RADIATION)
            target_physics = PHYSICS_TYPE_THERMAL
            self%num_variables = 2

            ! [Hydraulic]
        case (HYDRAULIC_BC_DIRICHLET, HYDRAULIC_BC_IMPERMEABLE, HYDRAULIC_BC_SEEPAGE)
            target_physics = PHYSICS_TYPE_HYDRAULIC
            self%num_variables = 1

        case (HYDRAULIC_BC_NEUMANN, HYDRAULIC_BC_FLUX)
            target_physics = PHYSICS_TYPE_HYDRAULIC
            self%num_variables = 1

        case default
            call error_message(ERR_BC_INIT, c_opt="Unknown BC Type ID: "//trim(to_string(target_bc)))
            return
        end select
        self%physics_type = target_physics

        ! --- 時間データの処理 ---
        associate (time_ctl => input%conditions%time_control)
            if (allocated(time_ctl%boundary_time_points)) then
                allocate (self%time_points, source=time_ctl%boundary_time_points)
                call controls%time%convert_time_unit( &
                    time_ctl%simulation_period%unit, TIME_UNIT_SECONDS, time_conv)
                self%time_points = self%time_points * time_conv
            else
                allocate (self%time_points(1))
                self%time_points(1) = 0.0d0
            end if
        end associate
        self%num_time_points = size(self%time_points)

        ! --- 値データの確保と取得 ---
        found = .false.
        associate (bcs => input%conditions%boundary_conditions)
            do i = 1, input%conditions%num_boundaries
                if (bcs(i)%id /= cell_id) cycle

                if (allocated(bcs(i)%physics(target_physics)%values)) then
                    allocate (self%values(self%num_variables, self%num_time_points))
                    self%values = 0.0d0

                    raw_values = bcs(i)%physics(target_physics)%values

                    ! Inputデータ(1次元)を self%values(変数, 時間) にマッピング
                    ! ※ ここのロジックは入力データの並び順仕様に合わせて調整してください
                    if (size(raw_values) >= self%num_variables * self%num_time_points) then
                        self%values = reshape(raw_values, [self%num_variables, self%num_time_points])
                    else
                        ! データ不足時のフォールバック (1成分だけある場合など)
                        self%values(1, :) = raw_values(1:min(size(raw_values), self%num_time_points))
                    end if

                    found = .true.
                end if
                exit
            end do
        end associate

        ! 値がない場合でも断熱などはOKとする処理
        if (.not. found) then
            select case (target_bc)
            case (THERMAL_BC_ADIABATIC, HYDRAULIC_BC_IMPERMEABLE)
                if (.not. allocated(self%values)) allocate (self%values(self%num_variables, self%num_time_points))
                self%values = 0.0d0
                found = .true.
            case default
                call error_message(ERR_BC_INIT, c_opt="BC data missing for ID: "//trim(to_string(cell_id)))
                return
            end select
        end if

        self%initialized = .true.

    end subroutine initialize_bc

    ! --------------------------------------------------------------------------
    ! 時間係数計算 (Binary Search)
    ! --------------------------------------------------------------------------
    module subroutine calc_time_coefficient_bc(self, current_time, coef, idx)
        implicit none
        class(abst_bc), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(inout) :: coef
        integer(int32), intent(inout) :: idx

        integer(int32) :: n, low, high, mid

        if (.not. allocated(self%time_points)) then
            coef = 0.0d0
            idx = 1
            return
        end if

        n = size(self%time_points)

        if (n < 2 .or. current_time <= self%time_points(1)) then
            coef = 0.0d0
            idx = 1
            return
        end if
        if (current_time >= self%time_points(n)) then
            coef = 0.0d0
            idx = n
            return
        end if

        low = 1
        high = n
        do while (high - low > 1)
            mid = (low + high) / 2
            if (current_time >= self%time_points(mid)) then
                low = mid
            else
                high = mid
            end if
        end do
        idx = low

        if (self%time_points(idx + 1) - self%time_points(idx) > epsilon(1.0d0)) then
            coef = (current_time - self%time_points(idx)) / &
                   (self%time_points(idx + 1) - self%time_points(idx))
        else
            coef = 0.0d0
        end if
    end subroutine calc_time_coefficient_bc

    ! --------------------------------------------------------------------------
    ! 生の値を取得
    ! --------------------------------------------------------------------------
    module subroutine calc_values_raw_bc(self, current_time, out_values)
        implicit none
        class(abst_bc), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(inout) :: out_values(:)

        real(real64) :: coef
        integer(int32) :: idx

        if (.not. self%initialized) then
            out_values = 0.0d0
            return
        end if

        call self%calc_time_coefficient(current_time, coef, idx)

        if (idx < self%num_time_points) then
            out_values(1:self%num_variables) = self%values(:, idx) + &
                                               coef * (self%values(:, idx + 1) - self%values(:, idx))
        else
            out_values(1:self%num_variables) = self%values(:, self%num_time_points)
        end if
    end subroutine calc_values_raw_bc

    ! --------------------------------------------------------------------------
    ! アクセサ (Flux / Dirichlet)
    ! --------------------------------------------------------------------------
    module subroutine calc_flux_and_derivative_bc(self, current_time, u_curr, q_flux, dq_du)
        implicit none
        class(abst_bc), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        real(real64), intent(out) :: q_flux, dq_du

        real(real64) :: vals(2)

        q_flux = 0.0d0
        dq_du = 0.0d0

        select type (self)
        type is (type_bc_neumann)
            call self%calc_values_raw(current_time, vals)
            q_flux = vals(IDX_BC_VAL)
            dq_du = 0.0d0

        type is (type_bc_robin)
            call self%calc_values_raw(current_time, vals)
            dq_du = vals(IDX_BC_COEFF) ! h
            q_flux = vals(IDX_BC_COEFF) * (u_curr - vals(IDX_BC_VAL)) ! h(u - u_ref)

        type is (type_bc_adiabatic)
            ! Flux=0
        end select
    end subroutine calc_flux_and_derivative_bc

    module subroutine calc_dirichlet_value_bc(self, current_time, val_fixed, is_active)
        implicit none
        class(abst_bc), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(out) :: val_fixed
        logical, intent(out) :: is_active

        real(real64) :: vals(1)

        select type (self)
        type is (type_bc_dirichlet)
            is_active = .true.
            call self%calc_values_raw(current_time, vals)
            val_fixed = vals(IDX_BC_VAL)
        class default
            is_active = .false.
            val_fixed = 0.0d0
        end select
    end subroutine calc_dirichlet_value_bc

    module subroutine destroy_bc(self)
        implicit none
        class(abst_bc), intent(inout) :: self

        self%bc_kind = -1
        self%physics_type = -1
        self%num_variables = 0
        self%boundary_id = -1
        call deallocate_array(self%values)
        call deallocate_array(self%time_points)
        self%initialized = .false.
    end subroutine destroy_bc

end submodule conditions_boundary_base

submodule(conditions_boundary) conditions_boundary_base
    implicit none

contains

    ! --------------------------------------------------------------------------
    ! 初期化ルーチン
    ! ※ Manager側で allocate された後に呼ばれる前提
    ! --------------------------------------------------------------------------
    module subroutine initialize_bc(self, cell_id, state_bc)
        implicit none
        class(abst_bc), intent(inout) :: self
        integer(int32), intent(in) :: cell_id
        type(type_state_bc), intent(in) :: state_bc

        self%boundary_id = cell_id
        call self%state%copy(state_bc)

        self%current_idx = 0

        self%initialized = .true.

    end subroutine initialize_bc

    ! --------------------------------------------------------------------------
    ! 時間係数計算 (Binary Search)
    ! --------------------------------------------------------------------------
    module subroutine calc_time_coefficient_bc(self, current_time, coef, idx)
        implicit none
        class(abst_bc), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(inout) :: coef
        integer(int32), intent(inout) :: idx

        integer(int32) :: n, low, high, mid, hint, i
        integer(int32), parameter :: MAX_LINEAR_STEPS = 4
        logical :: found

        if (.not. allocated(self%state%time_points)) then
            coef = 0.0d0
            idx = 1
            return
        end if

        n = size(self%state%time_points)

        if (n < 2 .or. current_time <= self%state%time_points(1)) then
            coef = 0.0d0
            idx = 1
            self%current_idx = 1
            return
        end if
        if (current_time >= self%state%time_points(n)) then
            coef = 0.0d0
            idx = n
            self%current_idx = n
            return
        end if

        ! ----- Hint-based local search (O(1) in typical forward-stepping) -----
        found = .false.
        hint = self%current_idx

        if (hint >= 1 .and. hint < n) then
            ! Check current interval [hint, hint+1)
            if (current_time >= self%state%time_points(hint) .and. &
                current_time < self%state%time_points(hint + 1)) then
                idx = hint
                found = .true.
            end if

            ! Forward linear search (normal time-stepping case)
            if (.not. found) then
                do i = 1, MAX_LINEAR_STEPS
                    if (hint + i >= n) exit
                    if (current_time >= self%state%time_points(hint + i) .and. &
                        current_time < self%state%time_points(hint + i + 1)) then
                        idx = hint + i
                        found = .true.
                        exit
                    end if
                end do
            end if

            ! Backward linear search (rollback case)
            if (.not. found) then
                do i = 1, MAX_LINEAR_STEPS
                    if (hint - i < 1) exit
                    if (current_time >= self%state%time_points(hint - i) .and. &
                        current_time < self%state%time_points(hint - i + 1)) then
                        idx = hint - i
                        found = .true.
                        exit
                    end if
                end do
            end if
        end if

        ! ----- Fallback: Binary Search (O(log n)) -----
        if (.not. found) then
            low = 1
            high = n
            do while (high - low > 1)
                mid = (low + high) / 2
                if (current_time >= self%state%time_points(mid)) then
                    low = mid
                else
                    high = mid
                end if
            end do
            idx = low
        end if

        ! Cache the found index for next call
        self%current_idx = idx

        ! Compute interpolation coefficient
        if (self%state%time_points(idx + 1) - self%state%time_points(idx) > epsilon(1.0d0)) then
            coef = (current_time - self%state%time_points(idx)) / &
                   (self%state%time_points(idx + 1) - self%state%time_points(idx))
        else
            coef = 0.0d0
        end if
    end subroutine calc_time_coefficient_bc

    ! --------------------------------------------------------------------------
    ! 生の値を取得
    ! --------------------------------------------------------------------------
    module subroutine calc_values_raw_bc(self, current_time, out_values)
        implicit none
        class(abst_bc), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(inout) :: out_values(:)

        real(real64) :: coef
        integer(int32) :: idx

        if (.not. self%initialized) then
            out_values = 0.0d0
            return
        end if

        call self%calc_time_coefficient(current_time, coef, idx)

        if (idx < self%state%num_time_points) then
            out_values(1:self%state%num_variables) = self%state%values(:, idx) + &
                                                     coef * (self%state%values(:, idx + 1) - self%state%values(:, idx))
        else
            out_values(1:self%state%num_variables) = self%state%values(:, self%state%num_time_points)
        end if
    end subroutine calc_values_raw_bc

    ! --------------------------------------------------------------------------
    ! アクセサ (Flux / Dirichlet)
    ! --------------------------------------------------------------------------
    module subroutine calc_flux_and_derivative_bc(self, current_time, u_curr, q_flux, dq_du)
        implicit none
        class(abst_bc), intent(inout) :: self
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

        type is (type_bc_zero_flux)
            ! Flux=0
        end select
    end subroutine calc_flux_and_derivative_bc

    module subroutine calc_dirichlet_value_bc(self, current_time, val_fixed, is_active)
        implicit none
        class(abst_bc), intent(inout) :: self
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

        ! self%bc_kind = -1
        ! self%physics_type = -1
        self%state%num_variables = 0
        self%boundary_id = -1
        call deallocate_array(self%state%values)
        call deallocate_array(self%state%time_points)
        self%initialized = .false.
    end subroutine destroy_bc

end submodule conditions_boundary_base

submodule(condition_boundary_data_provider) data_provider_table
    implicit none
contains

    module subroutine initialize_type_bc_data_table(self, config_bc)
        implicit none
        class(type_bc_data_table), intent(inout) :: self
        type(type_config_bc), intent(in) :: config_bc
    end subroutine initialize_type_bc_data_table

    module subroutine destroy_type_bc_data_table(self)
        implicit none
        class(type_bc_data_table), intent(inout) :: self
    end subroutine destroy_type_bc_data_table

    module subroutine get_data_bc_data_table(self, current_time, output_value)
        implicit none
        class(type_bc_data_table), intent(in) :: self
        real(real64), intent(in) :: current_time
        class(abst_bc_dto), intent(inout) :: output_value
    end subroutine get_data_bc_data_table

    module subroutine calc_time_coefficient_bc_data_table(self, current_time, coef, idx)
        implicit none
        class(type_bc_data_table), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(inout) :: coef
        integer(int32), intent(inout) :: idx

        integer(int32) :: n, low, high, mid, hint, i
        integer(int32), parameter :: MAX_LINEAR_STEPS = 4
        logical :: found

        if (.not. allocated(self%time_points)) then
            coef = 0.0d0
            idx = 1
            return
        end if

        n = size(self%time_points)

        if (n < 2 .or. current_time <= self%time_points(1)) then
            coef = 0.0d0
            idx = 1
            self%current_idx = 1
            return
        end if
        if (current_time >= self%time_points(n)) then
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
            if (current_time >= self%time_points(hint) .and. &
                current_time < self%time_points(hint + 1)) then
                idx = hint
                found = .true.
            end if

            ! Forward linear search (normal time-stepping case)
            if (.not. found) then
                do i = 1, MAX_LINEAR_STEPS
                    if (hint + i >= n) exit
                    if (current_time >= self%time_points(hint + i) .and. &
                        current_time < self%time_points(hint + i + 1)) then
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
                    if (current_time >= self%time_points(hint - i) .and. &
                        current_time < self%time_points(hint - i + 1)) then
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
                if (current_time >= self%time_points(mid)) then
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
        if (self%time_points(idx + 1) - self%time_points(idx) > epsilon(1.0d0)) then
            coef = (current_time - self%time_points(idx)) / &
                   (self%time_points(idx + 1) - self%time_points(idx))
        else
            coef = 0.0d0
        end if
    end subroutine calc_time_coefficient_bc_data_table

    ! !     module subroutine calc_values_raw_bc(self, current_time, out_values)
    ! !         implicit none
    ! !         class(abst_bc), intent(inout) :: self
    ! !         real(real64), intent(in) :: current_time
    ! !         real(real64), intent(inout) :: out_values(:)

    ! !         real(real64) :: coef
    ! !         integer(int32) :: idx

    ! !         if (.not. self%initialized) then
    ! !             out_values = 0.0d0
    ! !             return
    ! !         end if

    ! !         call self%calc_time_coefficient(current_time, coef, idx)

    ! !         if (idx < self%num_time_points) then
    ! !             out_values(1:self%num_variables) = self%values(:, idx) + &
    ! !                                                       coef * (self%values(:, idx + 1) - self%values(:, idx))
    ! !         else
    ! !             out_values(1:self%num_variables) = self%values(:, self%num_time_points)
    ! !         end if
    ! !     end subroutine calc_values_raw_bc

end submodule data_provider_table

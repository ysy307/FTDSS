submodule(condition_boundary_data_provider) data_provider_table
    implicit none
contains

    module subroutine initialize_type_bc_data_table(self, config)
        implicit none
        class(type_bc_data_table), intent(inout) :: self
        type(type_config_bc), intent(in) :: config
        ! User implements initialization

        call allocate_array(self%time_points, source=config%time_points)
        call allocate_array(self%table_values, source=config%values)

        self%data_kind = BC_DATA_PROVIDERS%TABLE
    end subroutine initialize_type_bc_data_table

    module subroutine destroy_type_bc_data_table(self)
        implicit none
        class(type_bc_data_table), intent(inout) :: self

        call deallocate_array(self%time_points)
        call deallocate_array(self%table_values)
        self%data_kind = type_constant_id("", "", -1)
    end subroutine destroy_type_bc_data_table

    module subroutine get_data_bc_data_table(self, current_time, values)
        implicit none
        class(type_bc_data_table), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), allocatable, intent(inout) :: values(:)

        real(real64) :: coef
        integer(int32) :: idx, num_vars

        if (.not. allocated(self%time_points) .or. .not. allocated(self%table_values)) return

        call self%calc_time_coefficient(current_time, coef, idx)

        num_vars = size(self%table_values, 1)
        if (allocated(values)) deallocate (values)
        allocate (values(num_vars))

        if (idx < size(self%time_points)) then
            values(1:num_vars) = self%table_values(:, idx) + &
                                 coef * (self%table_values(:, idx + 1) - self%table_values(:, idx))
        else
            values(1:num_vars) = self%table_values(:, size(self%time_points))
        end if
    end subroutine get_data_bc_data_table

    module subroutine calc_time_coefficient_bc_data_table(self, current_time, coef, idx)
        implicit none
        class(type_bc_data_table), intent(in) :: self
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
    end subroutine calc_time_coefficient_bc_data_table

end submodule data_provider_table

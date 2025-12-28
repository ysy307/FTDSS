submodule(conditions_boundary) conditions_boundary_base
    implicit none
contains

    module subroutine calc_time_coefficient_bc(self, current_time, coef, idx)
        implicit none
        class(abst_bc), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(inout) :: coef
        integer(int32), intent(inout) :: idx

        integer(int32) :: i, n

        if (.not. allocated(self%time_points)) then
            coef = 0.0d0
            idx = -1
            return
        end if

        n = size(self%time_points)
        if (n < 2) then
            coef = 0.0d0
            idx = 1
            return
        end if

        if (current_time < self%time_points(1)) then
            coef = 0.0d0
            idx = 1
            return
        end if

        if (current_time >= self%time_points(n)) then
            coef = 0.0d0
            idx = n
            return
        end if

        do i = 1, n - 1
            if (current_time >= self%time_points(i) .and. current_time < self%time_points(i + 1)) then
                if (abs(self%time_points(i + 1) - self%time_points(i)) > epsilon(1.0d0)) then
                    coef = (current_time - self%time_points(i)) / (self%time_points(i + 1) - self%time_points(i))
                else
                    coef = 0.0d0
                end if
                idx = i
                return
            end if
        end do

        coef = 0.0d0
        idx = n
    end subroutine calc_time_coefficient_bc

    module subroutine calc_value_at_time_bc(self, current_time, values)
        implicit none
        class(abst_bc), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(inout) :: values(:)

        real(real64) :: coef
        integer(int32) :: idx

        if (.not. self%is_allocated) then
            values = 0.0d0
            return
        end if
        call self%calc_time_coefficient(current_time, coef, idx)

        if (idx < size(self%values, 1)) then
            values = self%values(:, idx) + coef * (self%values(:, idx + 1) - self%values(:, idx))
        else
            values = self%values(:, size(self%values, 2))
        end if

    end subroutine calc_value_at_time_bc

    module subroutine destroy_bc(self)
        implicit none
        class(abst_bc), intent(inout) :: self

        call deallocate_array(self%time_points)
        call deallocate_array(self%values)
        self%is_allocated = .false.

    end subroutine destroy_bc

end submodule conditions_boundary_base

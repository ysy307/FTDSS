submodule(conditions_boundary) conditions_boundary_base
    implicit none
contains

    module subroutine calculate_time_coefficient(current_time, time_points, coef, idx)
        implicit none
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: time_points(:)
        real(real64), intent(inout) :: coef
        integer(int32), intent(inout) :: idx

        integer(int32) :: i, n

        n = size(time_points)
        if (n < 2) then
            coef = 0.0d0
            idx = 1
            return
        end if

        if (current_time < time_points(1)) then
            coef = 0.0d0
            idx = 1
            return
        end if

        if (current_time >= time_points(n)) then
            coef = 0.0d0
            idx = n
            return
        end if

        do i = 1, n - 1
            if (current_time >= time_points(i) .and. current_time < time_points(i + 1)) then
                if (abs(time_points(i + 1) - time_points(i)) > epsilon(1.0d0)) then
                    coef = (current_time - time_points(i)) / (time_points(i + 1) - time_points(i))
                else
                    coef = 0.0d0
                end if
                idx = i
                return
            end if
        end do

        coef = 0.0d0
        idx = n
    end subroutine calculate_time_coefficient

end submodule conditions_boundary_base

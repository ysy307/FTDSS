submodule(condition_boundary_strategy) strategy_cauchy
    implicit none
contains

    module subroutine evaluate_cauchy_bc(self, current_time, u_curr, result)
        implicit none
        class(type_bc_cauchy), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        type(type_bc_result), intent(inout) :: result

        real(real64), allocatable :: values(:)

        call self%provider%get_data(current_time, values)

        result%is_dirichlet = .true.

        if (allocated(values)) then
            if (size(values) >= 3) then
                result%prescribed_value = values(1)
                result%flux_value = values(2)
                result%flux_derivative = values(3)
            end if
            deallocate(values)
        end if
    end subroutine evaluate_cauchy_bc

end submodule strategy_cauchy
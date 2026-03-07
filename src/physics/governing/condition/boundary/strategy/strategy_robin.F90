submodule(condition_boundary_strategy) strategy_robin
    implicit none
contains

    module subroutine evaluate_robin_bc(self, current_time, u_curr, result)
        implicit none
        class(type_bc_robin), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        type(type_bc_result), intent(inout) :: result

        real(real64), allocatable :: values(:)
        real(real64) :: transfer_coeff, env_value

        call self%provider%get_data(current_time, values)

        result%is_dirichlet = .false.
        result%prescribed_value = 0.0d0

        if (allocated(values)) then
            if (size(values) >= 2) then
                transfer_coeff = values(1)
                env_value = values(2)
                result%flux_value = transfer_coeff * (u_curr - env_value)
                result%flux_derivative = transfer_coeff
            end if
            deallocate(values)
        end if
    end subroutine evaluate_robin_bc

end submodule strategy_robin
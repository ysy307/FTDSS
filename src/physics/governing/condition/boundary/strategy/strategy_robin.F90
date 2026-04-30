submodule(boundary_strategy) strategy_robin
    implicit none
contains

    module subroutine evaluate_robin_bc(self, current_time, u_curr, result)
        implicit none
        class(type_bc_robin), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        type(type_bc_result), intent(inout) :: result

        real(real64) :: values(3)
        real(real64) :: transfer_coeff, env_value

        call self%provider%get_data(current_time, values)

        call result%initialize()

        transfer_coeff = values(1)
        env_value = values(2)

        result%flux_value = -transfer_coeff * (u_curr - env_value)
        result%flux_derivative = transfer_coeff

    end subroutine evaluate_robin_bc

end submodule strategy_robin

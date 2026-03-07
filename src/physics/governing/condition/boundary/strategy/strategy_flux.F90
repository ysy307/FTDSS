submodule(boundary_strategy) strategy_flux
    implicit none
contains

    module pure subroutine evaluate_flux_bc(self, current_time, u_curr, result)
        implicit none
        class(type_bc_flux), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        type(type_bc_result), intent(inout) :: result

        real(real64) :: values(3)

        call self%provider%get_data(current_time, values)
        call result%initialize()

        result%flux_value = values(1)

    end subroutine evaluate_flux_bc

end submodule strategy_flux

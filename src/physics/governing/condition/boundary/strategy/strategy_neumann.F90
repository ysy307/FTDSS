submodule(condition_boundary_strategy) strategy_neumann
    implicit none
contains

    module subroutine evaluate_neumann_bc(self, current_time, u_curr, result)
        implicit none
        class(type_bc_neumann), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        type(type_bc_result), intent(inout) :: result

        real(real64), allocatable :: values(:)

        call self%provider%get_data(current_time, values)

        result%is_dirichlet = .false.
        result%prescribed_value = 0.0d0
        result%flux_derivative = 0.0d0

        if (allocated(values)) then
            if (size(values) >= 1) result%flux_value = values(1)
            deallocate(values)
        end if
    end subroutine evaluate_neumann_bc

end submodule strategy_neumann
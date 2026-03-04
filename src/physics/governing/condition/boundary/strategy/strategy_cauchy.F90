submodule(condition_boundary_strategy) strategy_cauchy
    implicit none
contains

    !> Calculates the residual flux and Jacobian derivative for Cauchy boundaries.
    !! Mathematical definition:
    !! \[ q = q_{prescribed}(t), \quad \frac{\partial q}{\partial u} = dq\_du \]
    !! Computational complexity: O(1)
    module subroutine calc_flux_cauchy_bc(self, current_time, u_curr, flux_value, flux_derivative)
        implicit none
        class(type_bc_cauchy), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        real(real64), intent(inout) :: flux_value
        real(real64), intent(inout) :: flux_derivative

        type(type_bc_data_cauchy) :: bc_data

        ! プロバイダからCauchy専用のデータを引き出す
        call self%data_provider%get_data(current_time, bc_data)

        flux_value = bc_data%flux_value
        flux_derivative = bc_data%flux_derivative
    end subroutine calc_flux_cauchy_bc

    !> Determines the fixed value and activates Dirichlet treatment for Cauchy boundaries.
    !! Computational complexity: O(1)
    module subroutine calc_dirichlet_cauchy_bc(self, current_time, u_curr, prescribed_value, is_active)
        implicit none
        class(type_bc_cauchy), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        real(real64), intent(inout) :: prescribed_value
        logical, intent(inout) :: is_active

        type(type_bc_data_cauchy) :: bc_data

        ! プロバイダからCauchy専用のデータを引き出す
        call self%data_provider%get_data(current_time, bc_data)

        prescribed_value = bc_data%prescribed_value
        is_active = .true.
    end subroutine calc_dirichlet_cauchy_bc

end submodule strategy_cauchy

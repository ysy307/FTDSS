!> Implementation of Robin (convective/linearized) boundary strategy.
submodule(condition_boundary_strategy) strategy_robin
    implicit none
contains

    !> Calculates the residual flux and Jacobian derivative for Robin boundaries.
    !! Mathematical definition:
    !! \[ q = h \cdot (u - u_{env}) \]
    !! \[ \frac{\partial q}{\partial u} = h \]
    !! Computational complexity: O(1)
    module subroutine calc_flux_robin_bc(self, current_time, u_curr, flux_value, flux_derivative)
        implicit none
        class(type_bc_robin), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        real(real64), intent(inout) :: flux_value
        real(real64), intent(inout) :: flux_derivative

        type(type_bc_data_robin) :: bc_data

        ! プロバイダから Robin 専用のデータ（伝達係数と環境温度など）を引き出す
        call self%data_provider%get_data(current_time, bc_data)

        flux_value = bc_data%transfer_coeff * (u_curr - bc_data%environment_value)
        flux_derivative = bc_data%transfer_coeff
    end subroutine calc_flux_robin_bc

    !> Determines if the node should be treated as Dirichlet.
    !! Always returns false for pure Robin conditions.
    !! Computational complexity: O(1)
    module subroutine calc_dirichlet_robin_bc(self, current_time, u_curr, prescribed_value, is_active)
        implicit none
        class(type_bc_robin), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        real(real64), intent(inout) :: prescribed_value
        logical, intent(inout) :: is_active

        prescribed_value = 0.0d0
        is_active = .false.
    end subroutine calc_dirichlet_robin_bc

end submodule strategy_robin

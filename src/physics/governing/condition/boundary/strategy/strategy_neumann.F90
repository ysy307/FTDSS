!> Implementation of Neumann (fixed flux) boundary strategy.
submodule(condition_boundary_strategy) strategy_neumann
    implicit none
contains

    !> Calculates the residual flux and Jacobian derivative for Neumann boundaries.
    !! Mathematical definition:
    !! \[ q = q_{prescribed}(t), \quad \frac{\partial q}{\partial u} = 0 \]
    !! Computational complexity: O(1)
    module subroutine calc_flux_neumann_bc(self, current_time, u_curr, flux_value, flux_derivative)
        implicit none
        class(type_bc_neumann), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        real(real64), intent(inout) :: flux_value
        real(real64), intent(inout) :: flux_derivative

        type(type_bc_data_scalar) :: bc_data

        ! プロバイダから Scalar 用のデータを引き出す
        call self%data_provider%get_data(current_time, bc_data)

        flux_value = bc_data%prescribed_value
        flux_derivative = 0.0d0
    end subroutine calc_flux_neumann_bc

    !> Determines if the node should be treated as Dirichlet.
    !! Always returns false for pure Neumann conditions.
    !! Computational complexity: O(1)
    module subroutine calc_dirichlet_neumann_bc(self, current_time, u_curr, prescribed_value, is_active)
        implicit none
        class(type_bc_neumann), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        real(real64), intent(inout) :: prescribed_value
        logical, intent(inout) :: is_active

        prescribed_value = 0.0d0
        is_active = .false.
    end subroutine calc_dirichlet_neumann_bc

end submodule strategy_neumann

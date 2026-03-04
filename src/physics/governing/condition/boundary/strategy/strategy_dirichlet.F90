!> Implementation of Dirichlet (fixed value) boundary strategy.
submodule(condition_boundary_strategy) strategy_dirichlet
    implicit none
contains

    !> Calculates the residual flux and Jacobian derivative for Dirichlet boundaries.
    !! Mathematical definition:
    !! \[ q = 0, \quad \frac{\partial q}{\partial u} = 0 \]
    !! (Since Dirichlet conditions overwrite the matrix equation directly, residual is zero.)
    !! Computational complexity: O(1)
    module subroutine calc_flux_dirichlet_bc(self, current_time, u_curr, flux_value, flux_derivative)
        implicit none
        class(type_bc_dirichlet), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        real(real64), intent(inout) :: flux_value
        real(real64), intent(inout) :: flux_derivative

        flux_value = 0.0d0
        flux_derivative = 0.0d0
    end subroutine calc_flux_dirichlet_bc

    !> Calculates the prescribed value for Dirichlet boundaries.
    !! Mathematical definition:
    !! \[ u_{fixed} = u_{prescribed}(t) \]
    !! Computational complexity: O(1)
    module subroutine calc_dirichlet_dirichlet_bc(self, current_time, u_curr, prescribed_value, is_active)
        implicit none
        class(type_bc_dirichlet), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        real(real64), intent(inout) :: prescribed_value
        logical, intent(inout) :: is_active

        type(type_bc_data_scalar) :: bc_data

        ! プロバイダから Scalar 用のデータを引き出す
        call self%data_provider%get_data(current_time, bc_data)

        prescribed_value = bc_data%prescribed_value
        is_active = .true.
    end subroutine calc_dirichlet_dirichlet_bc

end submodule strategy_dirichlet

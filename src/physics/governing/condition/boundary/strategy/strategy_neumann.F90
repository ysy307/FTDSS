submodule(condition_boundary_strategy) strategy_neumann
    implicit none
contains

    module subroutine calc_flux_neumann_bc(self, current_time, u_curr, q_flux, dq_du)
        implicit none
        class(type_bc_neumann), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        real(real64), intent(inout) :: q_flux
        real(real64), intent(inout) :: dq_du
    end subroutine calc_flux_neumann_bc

    module subroutine calc_dirichlet_neumann_bc(self, current_time, u_curr, val_fixed, is_active)
        implicit none
        class(type_bc_neumann), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        real(real64), intent(inout) :: val_fixed
        logical, intent(inout) :: is_active
    end subroutine calc_dirichlet_neumann_bc

end submodule strategy_neumann

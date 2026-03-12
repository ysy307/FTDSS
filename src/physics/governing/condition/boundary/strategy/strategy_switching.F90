submodule(boundary_strategy) strategy_switching
    implicit none
contains

    module subroutine evaluate_atmospheric_bc(self, current_time, u_curr, result)
        implicit none
        class(type_bc_atmospheric), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        type(type_bc_result), intent(inout) :: result

        call result%initialize()
    end subroutine evaluate_atmospheric_bc

    module subroutine evaluate_radiation_bc(self, current_time, u_curr, result)
        implicit none
        class(type_bc_radiation), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        type(type_bc_result), intent(inout) :: result

        call result%initialize()
    end subroutine evaluate_radiation_bc

    module subroutine evaluate_convective_bc(self, current_time, u_curr, result)
        implicit none
        class(type_bc_convective), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        type(type_bc_result), intent(inout) :: result

        call result%initialize()
    end subroutine evaluate_convective_bc

    module subroutine evaluate_seepage_bc(self, current_time, u_curr, result)
        implicit none
        class(type_bc_seepage), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        type(type_bc_result), intent(inout) :: result

        call result%initialize()
    end subroutine evaluate_seepage_bc

end submodule strategy_switching

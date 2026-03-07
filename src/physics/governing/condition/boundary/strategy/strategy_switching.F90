submodule(boundary_strategy) strategy_switching
    implicit none
contains

    module pure subroutine evaluate_atmospheric_bc(self, current_time, u_curr, result)
        implicit none
        class(type_bc_atmospheric), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        type(type_bc_result), intent(inout) :: result

        call result%initialize()
    end subroutine evaluate_atmospheric_bc

    module pure subroutine evaluate_radiation_bc(self, current_time, u_curr, result)
        implicit none
        class(type_bc_radiation), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        type(type_bc_result), intent(inout) :: result

        call result%initialize()
    end subroutine evaluate_radiation_bc

    module pure subroutine evaluate_convective_bc(self, current_time, u_curr, result)
        implicit none
        class(type_bc_convective), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        type(type_bc_result), intent(inout) :: result

        call result%initialize()
    end subroutine evaluate_convective_bc

    module pure subroutine evaluate_seepage_bc(self, current_time, u_curr, result)
        implicit none
        class(type_bc_seepage), intent(in) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        type(type_bc_result), intent(inout) :: result

        call result%initialize()
    end subroutine evaluate_seepage_bc

end submodule strategy_switching

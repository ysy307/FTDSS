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

        real(real64) :: values(3)
        real(real64) :: transfer_coeff, env_value

        call self%provider%get_data(current_time, values)
        call result%initialize()

        transfer_coeff = values(1)
        env_value = values(2)

        result%flux_value = transfer_coeff * (u_curr - env_value)
        result%flux_derivative = transfer_coeff
    end subroutine evaluate_convective_bc

    module subroutine evaluate_seepage_bc(self, current_time, u_curr, result)
        implicit none
        class(type_bc_seepage), intent(inout) :: self
        real(real64), intent(in) :: current_time
        real(real64), intent(in) :: u_curr
        type(type_bc_result), intent(inout) :: result

        ! values(1) = q_potential [m/s]: positive=evaporation, negative=infiltration
        ! values(2) = Pmin [m]: drying pressure limit
        ! values(3) = Pmax [m]: saturation pressure limit
        real(real64) :: values(3)
        real(real64) :: q_pot, Pmin_v, Pmax_v

        call result%initialize()
        call self%provider%get_data(current_time, values)

        q_pot  = values(1)
        Pmin_v = values(2)
        Pmax_v = values(3)

        if (q_pot < 0.0d0) then
            if (u_curr >= Pmax_v) then
                result%is_dirichlet     = .true.
                result%prescribed_value = Pmax_v
            else
                result%flux_value = q_pot
            end if
        else
            if (u_curr <= Pmin_v) then
                result%is_dirichlet     = .true.
                result%prescribed_value = Pmin_v
            else
                result%flux_value = q_pot
            end if
        end if
    end subroutine evaluate_seepage_bc

end submodule strategy_switching

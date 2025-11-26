submodule(physics_material_iapws) iapws_maxwell_criterion
    implicit none
contains
! ==========================================================
    ! Phase-equilibrium condition (Maxwell criterion)
    ! Region 3 saturation condition check
    ! ==========================================================

    !> Maxwell criterion Eq 1 (Liquid phase pressure consistency)
    !> Residual = p_s / (rho' * R * T) - delta' * phi_delta'
    !> Should be zero at equilibrium.
    module pure elemental function calc_maxwell_residual_1(T_in, rho_liq, p_sat) result(res)
        implicit none
        real(real64), intent(in) :: T_in ! Temperature [K]
        real(real64), intent(in) :: rho_liq ! Saturated Liquid Density [kg/m^3] (rho')
        real(real64), intent(in) :: p_sat ! Saturation Pressure [Pa] (p_s)
        real(real64) :: res

        real(real64) :: delta_liq, tau
        real(real64) :: phi_d_liq

        ! Dimensionless variables
        delta_liq = rho_liq / rho_star3
        tau = T_star3 / T_in

        ! Derivative
        phi_d_liq = calc_phi_delta_region3(delta_liq, tau)

        ! Calculate Residual 1
        ! LHS = p_s / (R * T * rho')
        ! RHS = delta' * phi_delta'
        res = p_sat / (specific_gas_constant_water * T_in * rho_liq) - delta_liq * phi_d_liq

    end function calc_maxwell_residual_1

    !> Maxwell criterion Eq 2 (Vapor phase pressure consistency)
    !> Residual = p_s / (rho'' * R * T) - delta'' * phi_delta''
    !> Should be zero at equilibrium.
    module pure elemental function calc_maxwell_residual_2(T_in, rho_vap, p_sat) result(res)
        implicit none
        real(real64), intent(in) :: T_in ! Temperature [K]
        real(real64), intent(in) :: rho_vap ! Saturated Vapor Density [kg/m^3] (rho'')
        real(real64), intent(in) :: p_sat ! Saturation Pressure [Pa] (p_s)
        real(real64) :: res

        real(real64) :: delta_vap, tau
        real(real64) :: phi_d_vap

        ! Dimensionless variables
        delta_vap = rho_vap / rho_star3
        tau = T_star3 / T_in

        ! Derivative
        phi_d_vap = calc_phi_delta_region3(delta_vap, tau)

        ! Calculate Residual 2
        ! LHS = p_s / (R * T * rho'')
        ! RHS = delta'' * phi_delta''
        res = p_sat / (specific_gas_constant_water * T_in * rho_vap) - delta_vap * phi_d_vap

    end function calc_maxwell_residual_2

    !> Maxwell criterion Eq 3 (Gibbs energy consistency / Equal Area Rule)
    !> Residual = (p_s / (R*T)) * (1/rho'' - 1/rho') - (phi(delta') - phi(delta''))
    !> Should be zero at equilibrium.
    module pure elemental function calc_maxwell_residual_3(T_in, rho_liq, rho_vap, p_sat) result(res)
        implicit none
        real(real64), intent(in) :: T_in ! Temperature [K]
        real(real64), intent(in) :: rho_liq ! Saturated Liquid Density [kg/m^3] (rho')
        real(real64), intent(in) :: rho_vap ! Saturated Vapor Density [kg/m^3] (rho'')
        real(real64), intent(in) :: p_sat ! Saturation Pressure [Pa] (p_s)
        real(real64) :: res

        real(real64) :: delta_liq, delta_vap, tau
        real(real64) :: phi_liq, phi_vap
        real(real64) :: lhs, rhs

        ! Dimensionless variables
        delta_liq = rho_liq / rho_star3
        delta_vap = rho_vap / rho_star3
        tau = T_star3 / T_in

        ! Helmholtz free energies
        phi_liq = calc_phi_region3(delta_liq, tau)
        phi_vap = calc_phi_region3(delta_vap, tau)

        ! Calculate Residual 3
        ! LHS = (p_s / (R * T)) * (1/rho'' - 1/rho')
        lhs = (p_sat / (specific_gas_constant_water * T_in)) * (1.0d0 / rho_vap - 1.0d0 / rho_liq)

        ! RHS = phi(delta') - phi(delta'')
        rhs = phi_liq - phi_vap

        res = lhs - rhs

    end function calc_maxwell_residual_3
end submodule iapws_maxwell_criterion

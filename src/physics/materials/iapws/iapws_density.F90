submodule(physics_material_iapws) iapws_density
    implicit none
contains

    !> Calculate the density of liquid water (Region 1).
    !> Valid range: \( 273.15 \text{ K} \le T \le 623.15 \text{ K} \), \( P_s(T) \le P \le 100 \text{ MPa} \).
    module pure elemental function get_density_iapws_region1(T_in, P_in) result(rho)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Density [kg/m^3]
        real(real64) :: rho

        real(real64) :: pi, tau, gamma_pi

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in * 1.0d-6 / p_star1 ! Pa -> MPa
        tau = T_star1 / T_in

        gamma_pi = get_gamma_pi_region1(pi, tau)

        ! ==========================================================
        ! Calculate Density
        ! ==========================================================
        rho = p_star1 * 1.0d6 / (specific_gas_constant_water * 1000.0d0 * T_in * gamma_pi)
    end function get_density_iapws_region1

    !> Calculate the derivative of density with respect to temperature at constant pressure.
    !> Computes \( \left(\frac{\partial \rho}{\partial T}\right)_P \).
    module pure elemental function get_drho_dt_iapws_region1(T_in, P_in) result(drho_dt)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Derivative of density w.r.t temperature [kg/m^3/K]
        real(real64) :: drho_dt

        real(real64) :: pi, tau, gamma_pi, gamma_pitau, rho

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = (P_in * 1.0d-6) / p_star1
        tau = T_star1 / T_in

        gamma_pi = get_gamma_pi_region1(pi, tau)
        gamma_pitau = get_gamma_pitau_region1(pi, tau)

        ! ==========================================================
        ! Calculate Derivative
        ! ==========================================================
        rho = (p_star1 * 1.0d6) / (specific_gas_constant_water * 1000.0d0 * T_in * gamma_pi)
        drho_dt = (rho / T_in) * ((tau * gamma_pitau / gamma_pi) - 1.0d0)
    end function get_drho_dt_iapws_region1

    !> Calculate the derivative of density with respect to pressure at constant temperature.
    !> Computes \( \left(\frac{\partial \rho}{\partial P}\right)_T \).
    module pure elemental function get_drho_dp_iapws_region1(T_in, P_in) result(drho_dp)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Derivative of density w.r.t pressure [kg/m^3/Pa]
        real(real64) :: drho_dp

        real(real64) :: pi, tau, gamma_pi, gamma_pipi, rho

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = (P_in * 1.0d-6) / p_star1
        tau = T_star1 / T_in

        gamma_pi = get_gamma_pi_region1(pi, tau)
        gamma_pipi = get_gamma_pipi_region1(pi, tau)

        ! ==========================================================
        ! Calculate Derivative
        ! ==========================================================
        rho = (p_star1 * 1.0d6) / (specific_gas_constant_water * 1000.0d0 * T_in * gamma_pi)
        drho_dp = -rho * gamma_pipi / (gamma_pi * p_star1 * 1.0d6)
    end function get_drho_dp_iapws_region1

    module pure elemental function get_density_iapws_region2(T_in, P_in) result(rho)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Density [kg/m^3]
        real(real64) :: rho

        real(real64) :: pi, tau, gamma_pi

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in * 1.0d-6 / p_star2 ! Pa -> MPa
        tau = T_star2 / T_in

        ! ==========================================================
        ! Calculate Density
        ! ==========================================================
        gamma_pi = get_gamma0_pi_region2(pi, tau) + get_gammar_pi_region2(pi, tau)
        rho = p_star2 * 1.0d6 / (specific_gas_constant_water * 1000.0d0 * T_in * gamma_pi)

    end function get_density_iapws_region2

end submodule iapws_density

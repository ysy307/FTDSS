submodule(physics_material_iapws) iapws_density
    implicit none
contains
    !-----------------------------------------------------------------------------
    ! Region 1 (Liquid Water)
    ! Valid: 273.15 K <= T <= 623.15 K (Extrapolates well to < 273.15 K)
    !        p_s(T) <= P <= 100 MPa
    ! Input:
    !   T_in : Temperature [K]
    !   P_in : Pressure [Pa]
    ! Output:
    !   rho     : Density [kg/m^3]
    !-----------------------------------------------------------------------------
    module pure elemental function get_density_iapws_region1(T_in, P_in) result(rho)
        implicit none
        real(real64), intent(in) :: T_in ! Temperature [K]
        real(real64), intent(in) :: P_in ! Pressure [Pa]
        real(real64) :: rho ! Density [kg/m^3]

        real(real64) :: pi, tau, gamma_pi

        ! dimensionless variables
        pi = P_in * 1.0d-6 / p_star1 ! Pa -> MPa
        tau = T_star1 / T_in

        gamma_pi = get_gamma_pi_region1(pi, tau)

        rho = p_star1 * 1.0d6 / (specific_gas_constant_water * 1000.0d0 * T_in * gamma_pi)
    end function get_density_iapws_region1

    !> Density derivative w.r.t Temperature (drho/dT)_P [kg/m^3/K]
    module pure elemental function get_drho_dt_iapws_region1(T_in, P_in) result(drho_dt)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        real(real64) :: drho_dt

        real(real64) :: pi, tau, gamma_pi, gamma_pitau, rho

        ! dimensionless variables
        pi = (P_in * 1.0d-6) / p_star1
        tau = T_star1 / T_in

        gamma_pi = get_gamma_pi_region1(pi, tau)
        gamma_pitau = get_gamma_pitau_region1(pi, tau)

        rho = (p_star1 * 1.0d6) / (specific_gas_constant_water * 1000.0d0 * T_in * gamma_pi)
        drho_dt = (rho / T_in) * ((tau * gamma_pitau / gamma_pi) - 1.0d0)
    end function get_drho_dt_iapws_region1

    !> Density derivative w.r.t Pressure (drho/dP)_T [kg/m^3/Pa]
    module pure elemental function get_drho_dp_iapws_region1(T_in, P_in) result(drho_dp)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        real(real64) :: drho_dp

        real(real64) :: pi, tau, gamma_pi, gamma_pipi, rho

        pi = (P_in * 1.0d-6) / p_star1
        tau = T_star1 / T_in

        gamma_pi = get_gamma_pi_region1(pi, tau)
        gamma_pipi = get_gamma_pipi_region1(pi, tau) ! 親モジュールの関数を使用

        rho = (p_star1 * 1.0d6) / (specific_gas_constant_water * 1000.0d0 * T_in * gamma_pi)
        drho_dp = -rho * gamma_pipi / (gamma_pi * p_star1 * 1.0d6)
    end function get_drho_dp_iapws_region1

end submodule iapws_density

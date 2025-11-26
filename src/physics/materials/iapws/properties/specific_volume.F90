submodule(physics_material_iapws) iapws_specific_volume
    implicit none
contains

    module pure elemental function get_nu_iapws97_region1(T_in, P_in) result(nu)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific volume [m^3/kg]
        real(real64) :: nu

        real(real64) :: pi, tau, gamma_pi

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in * 1.0d-6 / p_star1 ! Pa -> MPa
        tau = T_star1 / T_in

        gamma_pi = get_gamma_pi_region1(pi, tau)

        ! ==========================================================
        ! Calculate specific volume [m^3/kg]
        ! ==========================================================
        nu = 1.0d-3 * specific_gas_constant_water * T_in * gamma_pi / p_star1
    end function get_nu_iapws97_region1

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

end submodule iapws_specific_volume

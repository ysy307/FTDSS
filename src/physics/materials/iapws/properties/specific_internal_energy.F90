submodule(physics_material_iapws) iapws_specific_internal_energy
    implicit none
contains

    module pure elemental function get_u_iapws97_region1(T_in, P_in) result(u)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific internal energy [J/kg]
        real(real64) :: u

        real(real64) :: pi, tau
        real(real64) :: gamma_t, gamma_p

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_star1
        tau = T_star1 / T_in

        gamma_t = get_gamma_tau_region1(pi, tau)
        gamma_p = get_gamma_pi_region1(pi, tau)

        ! ==========================================================
        ! Calculate Specific internal energy [J/kg]
        ! ==========================================================
        u = specific_gas_constant_water * T_in * (tau * gamma_t - pi * gamma_p)

    end function get_u_iapws97_region1

    module pure elemental function get_u_iapws97_region2(T_in, P_in) result(u)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific internal energy [J/kg]
        real(real64) :: u

        real(real64) :: pi, tau
        real(real64) :: gamma_tau, gamma_pi

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_star2
        tau = T_star2 / T_in

        gamma_tau = get_gamma0_tau_region2(pi, tau) + get_gammar_tau_region2(pi, tau)
        gamma_pi = get_gamma0_pi_region2(pi, tau) + get_gammar_pi_region2(pi, tau)

        ! ==========================================================
        ! Calculate Specific internal energy [J/kg]
        ! ==========================================================
        u = specific_gas_constant_water * T_in * (tau * gamma_tau - pi * gamma_pi)

    end function get_u_iapws97_region2

    module pure elemental function get_u_iapws97_region3(T_in, rho_in) result(u)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Density [kg/m^3]
        real(real64), intent(in) :: rho_in
        !> Specific internal energy [J/kg]
        real(real64) :: u

        real(real64) :: delta, tau
        real(real64) :: phi_t

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        delta = rho_in / rho_star3
        tau = T_star3 / T_in

        phi_t = get_phi_tau_region3(delta, tau)

        ! ==========================================================
        ! Calculate Specific internal energy [J/kg]
        ! ==========================================================
        u = specific_gas_constant_water * T_in * tau * phi_t

    end function get_u_iapws97_region3

end submodule iapws_specific_internal_energy

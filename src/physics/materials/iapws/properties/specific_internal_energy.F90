submodule(physics_material_iapws) iapws_specific_internal_energy
    implicit none
contains

    module pure elemental function calc_u_iapws97_region1(T_in, P_in) result(u)
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

        gamma_t = calc_gamma_t_region1(pi, tau)
        gamma_p = calc_gamma_p_region1(pi, tau)

        ! ==========================================================
        ! Calculate Specific internal energy [J/kg]
        ! ==========================================================
        u = R_w * T_in * (tau * gamma_t - pi * gamma_p)

    end function calc_u_iapws97_region1

    module pure elemental function calc_u_iapws97_region2(T_in, P_in) result(u)
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

        gamma_tau = calc_gamma0_t_region2(pi, tau) + calc_gammar_t_region2(pi, tau)
        gamma_pi = calc_gamma0_p_region2(pi, tau) + calc_gammar_p_region2(pi, tau)

        ! ==========================================================
        ! Calculate Specific internal energy [J/kg]
        ! ==========================================================
        u = R_w * T_in * (tau * gamma_tau - pi * gamma_pi)

    end function calc_u_iapws97_region2

    module pure elemental function calc_u_iapws97_region3(T_in, rho_in) result(u)
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

        phi_t = calc_phi_t_region3(delta, tau)

        ! ==========================================================
        ! Calculate Specific internal energy [J/kg]
        ! ==========================================================
        u = R_w * T_in * tau * phi_t

    end function calc_u_iapws97_region3

    !> Specific Internal Energy [J/kg]
    !> Formula: u = R*T * (tau*gamma_tau - pi*gamma_pi)
    module pure elemental function calc_u_iapws97_region5(T_in, P_in) result(u)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        real(real64) :: u

        real(real64) :: pi, tau
        real(real64) :: gamma_p, gamma_t

        pi = P_in / p_star5
        tau = T_star5 / T_in

        ! Sum of ideal and residual derivatives
        gamma_p = calc_gamma0_p_region5(pi, tau) + calc_gammar_p_region5(pi, tau)
        gamma_t = calc_gamma0_t_region5(pi, tau) + calc_gammar_t_region5(pi, tau)

        u = R_w * T_in * (tau * gamma_t - pi * gamma_p)
    end function calc_u_iapws97_region5

    module pure elemental function calc_u_iapws06_Ih(T_in, P_in) result(u)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific internal energy [J/kg]
        real(real64) :: u

        real(real64) :: pi, tau
        real(real64) :: gamma
        real(real64) :: gamma_t, gamma_p

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_starIh
        tau = T_in / T_starIh

        gamma = calc_gamma_iapws06_Ih(pi, tau)
        gamma_t = calc_gamma_t_iapws06_Ih(pi, tau)
        gamma_p = calc_gamma_p_iapws06_Ih(pi, tau)
        ! ==========================================================
        ! Calculate Specific internal energy [J/kg]
        ! ==========================================================
        u = gamma * T_in * gamma_t - P_in * gamma_p

    end function calc_u_iapws06_Ih

end submodule iapws_specific_internal_energy

submodule(physics_material_iapws) iapws_specific_volume
    implicit none
contains

    module pure elemental function calc_nu_iapws97_region1(T_in, P_in) result(nu)
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
        pi = P_in / p_star1
        tau = T_star1 / T_in

        gamma_pi = calc_gamma_p_region1(pi, tau)

        ! ==========================================================
        ! Calculate specific volume [m^3/kg]
        ! ==========================================================
        nu = R_w * T_in * gamma_pi / p_star1
    end function calc_nu_iapws97_region1

    module pure elemental function calc_nu_iapws97_region2(T_in, P_in) result(nu)
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
        pi = P_in / p_star2
        tau = T_star2 / T_in

        ! ==========================================================
        ! Calculate Density
        ! ==========================================================
        gamma_pi = calc_gamma0_p_region2(pi, tau) + calc_gammar_p_region2(pi, tau)
        nu = R_w * T_in * gamma_pi / p_star2

    end function calc_nu_iapws97_region2

    !> Specific Volume [m^3/kg]
    !> Formula: v = (R*T/P) * pi * gamma_pi
    module pure elemental function calc_nu_iapws97_region5(T_in, P_in) result(nu)
        implicit none
        real(real64), intent(in) :: T_in ! Temperature [K]
        real(real64), intent(in) :: P_in ! Pressure [Pa] (Modified from rho_in)
        real(real64) :: nu

        real(real64) :: pi, tau
        real(real64) :: gamma0_p, gammar_p, gamma_p

        ! Dimensionless variables
        pi = P_in / p_star5
        tau = T_star5 / T_in

        ! Derivatives
        gamma0_p = calc_gamma0_p_region5(pi, tau)
        gammar_p = calc_gammar_p_region5(pi, tau)
        gamma_p = gamma0_p + gammar_p

        ! v = (R * T / P) * pi * gamma_pi
        ! Note: pi = P / P_star -> P = pi * P_star
        ! v = (R * T / (pi * P_star)) * pi * gamma_pi
        ! v = (R * T / P_star) * gamma_pi
        ! P_star5 is 10 MPa = 10^7 Pa. R is J/kgK.

        nu = (R_w * T_in / p_star5) * gamma_p
        ! Or simply using input P:
        ! nu = (R_w * T_in / P_in) * pi * gamma_p
    end function calc_nu_iapws97_region5

    module pure elemental function calc_nu_iapws06_Ih(T_in, P_in) result(nu)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        real(real64) :: nu

        real(real64) :: pi, tau

        ! Dimensionless variables
        pi = P_in / p_starIh
        tau = T_in / T_starIh

        ! Specific Volume [m^3/kg]
        nu = calc_gamma_p_iapws06_Ih(pi, tau)

    end function calc_nu_iapws06_Ih
end submodule iapws_specific_volume

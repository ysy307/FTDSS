submodule(physics_material_iapws) iapws_specific_isochoric_heat_capacity
    implicit none
contains

    module pure elemental function get_cv_iapws97_region1(T_in, P_in) result(cv)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific heat capacity at constant volume [J/(kg K)]
        real(real64) :: cv

        real(real64) :: pi, tau
        real(real64) :: gamma_p
        real(real64) :: gamma_tt, gamma_pp, gamma_pt
        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_star1
        tau = T_star1 / T_in

        ! ==========================================================
        ! Get derivatives from parent module
        ! ==========================================================
        gamma_p = get_gamma_pi_region1(pi, tau)
        gamma_tt = get_gamma_tautau_region1(pi, tau)
        gamma_pp = get_gamma_pipi_region1(pi, tau)
        gamma_pt = get_gamma_pitau_region1(pi, tau)

        cv = (-tau**2.0d0 * gamma_tt + (gamma_p - tau * gamma_pt)**2.0d0 / gamma_pp) * specific_gas_constant_water

    end function get_cv_iapws97_region1

    module pure elemental function get_cv_iapws97_region2(T_in, P_in) result(cv)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Pressure [Pa]
        real(real64), intent(in) :: P_in
        !> Specific heat capacity at constant volume [J/(kg K)]
        real(real64) :: cv

        real(real64) :: pi, tau
        real(real64) :: gammar_p
        real(real64) :: gamma0_tt, gammar_tt
        real(real64) :: gamma0_pp, gammar_pp
        real(real64) :: gamma0_pt, gammar_pt

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        pi = P_in / p_star2
        tau = T_star2 / T_in
        ! ==========================================================
        ! Get derivatives from parent module
        ! ==========================================================
        gammar_p = get_gammar_pi_region2(pi, tau)
        gamma0_tt = get_gamma0_tautau_region2(pi, tau)
        gammar_tt = get_gammar_tautau_region2(pi, tau)
        gamma0_pp = get_gamma0_pipi_region2(pi, tau)
        gammar_pp = get_gammar_pipi_region2(pi, tau)
        gamma0_pt = get_gamma0_pitau_region2(pi, tau)
        gammar_pt = get_gammar_pitau_region2(pi, tau)

        cv = (-tau**2.0d0 * (gamma0_tt + gammar_tt) - (1.0d0 + pi * gammar_p - tau * pi * gammar_pt)**2.0d0 / (1 - pi**2.0d0 * gamma0_pp)) * specific_gas_constant_water

    end function get_cv_iapws97_region2

    module pure elemental function get_cv_iapws97_region3(T_in, rho_in) result(cv)
        implicit none
        !> Temperature [K]
        real(real64), intent(in) :: T_in
        !> Density [kg/m^3]
        real(real64), intent(in) :: rho_in
        !> Specific heat capacity at constant volume [J/kg-K]
        real(real64) :: cv

        real(real64) :: delta, tau
        real(real64) :: phi_tt

        ! ==========================================================
        ! Dimensionless variables
        ! ==========================================================
        delta = rho_in / rho_star3
        tau = T_star3 / T_in

        ! ==========================================================
        ! Get derivatives from parent module
        ! ==========================================================
        phi_tt = get_phi_tautau_region3(delta, tau)

        cv = -tau**2.0d0 * phi_tt * specific_gas_constant_water

    end function get_cv_iapws97_region3

end submodule iapws_specific_isochoric_heat_capacity

submodule(physics_material_iapws) iapws_specific_isochoric_heat_capacity
    implicit none
contains

    module pure elemental function calc_cv_iapws97_region1(T_in, P_in) result(cv)
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
        gamma_p = calc_gamma_pi_region1(pi, tau)
        gamma_tt = calc_gamma_tautau_region1(pi, tau)
        gamma_pp = calc_gamma_pipi_region1(pi, tau)
        gamma_pt = calc_gamma_pitau_region1(pi, tau)

        cv = (-tau**2.0d0 * gamma_tt + (gamma_p - tau * gamma_pt)**2.0d0 / gamma_pp) * specific_gas_constant_water

    end function calc_cv_iapws97_region1

    module pure elemental function calc_cv_iapws97_region2(T_in, P_in) result(cv)
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
        gammar_p = calc_gammar_pi_region2(pi, tau)
        gamma0_tt = calc_gamma0_tautau_region2(pi, tau)
        gammar_tt = calc_gammar_tautau_region2(pi, tau)
        gamma0_pp = calc_gamma0_pipi_region2(pi, tau)
        gammar_pp = calc_gammar_pipi_region2(pi, tau)
        gamma0_pt = calc_gamma0_pitau_region2(pi, tau)
        gammar_pt = calc_gammar_pitau_region2(pi, tau)

        cv = (-tau**2.0d0 * (gamma0_tt + gammar_tt) - (1.0d0 + pi * gammar_p - tau * pi * gammar_pt)**2.0d0 / (1 - pi**2.0d0 * gamma0_pp)) * specific_gas_constant_water

    end function calc_cv_iapws97_region2

    module pure elemental function calc_cv_iapws97_region3(T_in, rho_in) result(cv)
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
        phi_tt = calc_phi_tautau_region3(delta, tau)

        cv = -tau**2.0d0 * phi_tt * specific_gas_constant_water

    end function calc_cv_iapws97_region3

    !> Specific Isochoric Heat Capacity [J/(kg K)]
    !> Formula: cv = cp - R * (gamma_pi - tau*gamma_pitau)^2 / (-gamma_pipi)
    module pure elemental function calc_cv_iapws97_region5(T_in, P_in) result(cv)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        real(real64) :: cv

        real(real64) :: pi, tau
        real(real64) :: gamma_tt, gamma_p, gamma_pp, gamma_pt
        real(real64) :: cp_val, numerator, denominator

        pi = P_in / p_star5
        tau = T_star5 / T_in

        ! Get all total derivatives (ideal + residual)
        gamma_tt = calc_gamma0_tautau_region5(pi, tau) + calc_gammar_tautau_region5(pi, tau)
        gamma_p = calc_gamma0_pi_region5(pi, tau) + calc_gammar_pi_region5(pi, tau)
        gamma_pp = calc_gamma0_pipi_region5(pi, tau) + calc_gammar_pipi_region5(pi, tau)
        gamma_pt = calc_gamma0_pitau_region5(pi, tau) + calc_gammar_pitau_region5(pi, tau)

        ! Calculate Cp first
        cp_val = specific_gas_constant_water * (-tau**2 * gamma_tt)

        numerator = (gamma_p - tau * gamma_pt)**2
        denominator = -gamma_pp ! Note: gamma_pp is typically negative

        cv = cp_val - specific_gas_constant_water * (numerator / denominator)
    end function calc_cv_iapws97_region5

end submodule iapws_specific_isochoric_heat_capacity

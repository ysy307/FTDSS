submodule(physics_material_iapws97_region5) iapws97_region5_properties
    implicit none
contains

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

    module pure elemental function calc_rho_iapws97_region5(T_in, P_in) result(rho)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        real(real64) :: rho

        rho = 1.0d0 / calc_nu_iapws97_region5(T_in, P_in)

    end function calc_rho_iapws97_region5

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

    !> Specific Entropy [J/(kg K)]
    !> Formula: s = R * (tau*gamma_tau - gamma)
    module pure elemental function calc_s_iapws97_region5(T_in, P_in) result(s)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        real(real64) :: s

        real(real64) :: pi, tau
        real(real64) :: gamma, gamma_t

        pi = P_in / p_star5
        tau = T_star5 / T_in

        gamma = calc_gamma0_region5(pi, tau) + calc_gammar_region5(pi, tau)
        gamma_t = calc_gamma0_t_region5(pi, tau) + calc_gammar_t_region5(pi, tau)

        s = R_w * (tau * gamma_t - gamma)
    end function calc_s_iapws97_region5

    !> Specific Enthalpy [J/kg]
    !> Formula: h = R*T * tau * gamma_tau
    module pure elemental function calc_h_iapws97_region5(T_in, P_in) result(h)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        real(real64) :: h

        real(real64) :: pi, tau
        real(real64) :: gamma_t

        pi = P_in / p_star5
        tau = T_star5 / T_in

        gamma_t = calc_gamma0_t_region5(pi, tau) + calc_gammar_t_region5(pi, tau)

        h = R_w * T_in * tau * gamma_t
    end function calc_h_iapws97_region5

    !> Specific Isobaric Heat Capacity [J/(kg K)]
    !> Formula: cp = -R * tau^2 * gamma_tautau
    module pure elemental function calc_cp_iapws97_region5(T_in, P_in) result(cp)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        real(real64) :: cp

        real(real64) :: pi, tau
        real(real64) :: gamma_tt

        pi = P_in / p_star5
        tau = T_star5 / T_in

        gamma_tt = calc_gamma0_tt_region5(pi, tau) + calc_gammar_tt_region5(pi, tau)

        cp = R_w * (-tau**2 * gamma_tt)
    end function calc_cp_iapws97_region5

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
        gamma_tt = calc_gamma0_tt_region5(pi, tau) + calc_gammar_tt_region5(pi, tau)
        gamma_p = calc_gamma0_p_region5(pi, tau) + calc_gammar_p_region5(pi, tau)
        gamma_pp = calc_gamma0_pp_region5(pi, tau) + calc_gammar_pp_region5(pi, tau)
        gamma_pt = calc_gamma0_pt_region5(pi, tau) + calc_gammar_pt_region5(pi, tau)

        ! Calculate Cp first
        cp_val = R_w * (-tau**2 * gamma_tt)

        numerator = (gamma_p - tau * gamma_pt)**2
        denominator = -gamma_pp ! Note: gamma_pp is typically negative

        cv = cp_val - R_w * (numerator / denominator)
    end function calc_cv_iapws97_region5

    module pure elemental function calc_w_iapws97_region5(T_in, P_in) result(w)
        implicit none
        real(real64), intent(in) :: T_in
        real(real64), intent(in) :: P_in
        real(real64) :: w

        real(real64) :: pi, tau
        real(real64) :: gammar_p, gammar_pp, gammar_pt, gammar_tt
        real(real64) :: gamma0_tt
        real(real64) :: numerator, denom_term1, denom_term2, w2_dimless

        ! Region 5 Reference Pressure is 1 MPa
        real(real64), parameter :: p_ref_r5 = 1.0d6

        ! Dimensionless variables
        pi = P_in / p_ref_r5
        tau = T_star5 / T_in

        ! ==========================================================
        ! Calculate derivatives needed for the explicit formula
        ! Use Residual parts (gammar) and Ideal part (gamma0_tt only)
        ! ==========================================================

        ! Residual part derivatives
        gammar_p = calc_gammar_p_region5(pi, tau)
        gammar_pp = calc_gammar_pp_region5(pi, tau)
        gammar_pt = calc_gammar_pt_region5(pi, tau)
        gammar_tt = calc_gammar_tt_region5(pi, tau)

        ! Ideal gas part derivative (only gamma0_tautau is needed)
        gamma0_tt = calc_gamma0_tt_region5(pi, tau)

        ! ==========================================================
        ! Calculate Speed of Sound using explicit formula (Eq. 16 style)
        ! w^2/RT = Numerator / Denominator
        ! ==========================================================

        ! Numerator = 1 + 2*pi*gammar_pi + (pi*gammar_pi)^2
        numerator = 1.0d0 + 2.0d0 * pi * gammar_p + (pi * gammar_p)**2

        ! Denominator Term 1 = 1 - pi^2 * gammar_pipi
        denom_term1 = 1.0d0 - pi**2 * gammar_pp

        ! Denominator Term 2 = (1 + pi*gammar_pi - tau*pi*gammar_pitau)^2 / (tau^2 * (gamma0_tautau + gammar_tautau))
        ! Note: (gamma0_tt + gammar_tt) corresponds to -Cp*tau^2/R and is negative.
        ! So denom_term2 will be a negative value subtraction.
        denom_term2 = ((1.0d0 + pi * gammar_p - tau * pi * gammar_pt)**2) / &
                      (tau**2 * (gamma0_tt + gammar_tt))

        ! Total Denominator = Term1 + Term2 (as per the image formula)
        w2_dimless = numerator / (denom_term1 + denom_term2)

        ! Convert to dimensions [m/s]
        w = sqrt(max(R_w * T_in * w2_dimless, 0.0d0))

    end function calc_w_iapws97_region5

end submodule iapws97_region5_properties
